;;; et-checkers.el --- Non-internal helpers of et.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

(require 'et-check)


;;; ============================================================
;;; Identifiers - `et:identify'
;;;; et-declare

(et-set-identifier #'et-declare #'et:identify-et-declare)

(et-defun et:identify-et-declare (&rest forms: Sexps) EtIdentifyPlist
  (let* ((plists (et-at-offset 1 (et-identify-exprs forms)))
         (perform-phase
          (lambda (phase)
            (cl-loop for plist in plists
                     for pos upfrom 1
                     for func = (plist-get plist phase)
                     when func do (et-at pos (funcall func))))))
    (list
     :constrain (lambda () (funcall perform-phase :constrain))
     :populate (lambda () (funcall perform-phase :populate))
     :declare (lambda () (funcall perform-phase :declare)))))


;;;; @alias

(et-set-identifier '@alias #'et:identify-@alias)

(et-defun et:identify-@alias (&rest args) EtIdentifyPlist
  "Identify an alias definition, returning a processing plist.

During identification, declares the alias name and generics.
Returns a plist with :constrain and :populate functions."
  (let* ((orig-args args)
         (name (pop args))
         (_ (or (symbolp name)
                (et-fatal 1 "Alias name must be a symbol")))
         (genvec (if (vectorp (car args)) (pop args) []))
         (pb (condition-case nil (et-props-and-body args)
               (error (et-fatal nil "Expected format (@alias NAME [GENERICS] [PROPS...] TYPE)"))))

         ;; Identification phase: declare the alias name, generics, and spec
         ;; (but don't parse repr or constraints yet)
         (_ (apply #'et:type-declare-alias name genvec (cdr pb) (car pb)))

         (spec-idx (length orig-args))
         (genvec-idx (when (vectorp (nth 1 orig-args)) 2)))

    (list
     :constrain
     (lambda ()
       ;; Parse the generic constraints (which reference other types)
       (when genvec-idx
         (et-at genvec-idx
           (let* ((props (get name 'et-alias)))
             (plist-put props :constraints
                        (et-genvec-constraints (plist-get props :genvec)))))))

     :populate
     (lambda ()
       ;; Parse the spec into a repr
       (et-at spec-idx
         (let* ((props (or (get name 'et-alias)
                           (et-fatal nil "Alias `%s' not declared" name)))
                (spec (plist-get props :spec))
                (generics (plist-get props :generics)))
           (plist-put props :repr
                      (et-parse-repr spec generics))))))))


;;;; Variables

(et-set-identifier #'et-defvar #'et:identify-et-defvar)
(et-set-identifier '@variable #'et:identify-et-defvar)

(et-defun et:identify-et-defvar (name: Var spec: EtSpec &rest _) EtIdentifyPlist
  (list :declare (lambda () (et-set-global-var-type name (et-parse-type spec)))))


;;;; Callables

(et-set-identifier #'defun #'et:identify-defun)
(et-set-identifier #'cl-defun #'et:identify-defun)
(et-set-identifier #'defmacro #'et:identify-defun)

(et-defun et:identify-defun (name: Var arglist: ListR<Any> &rest rest: Sexps) EtIdentifyPlist
  (list
   :declare
   (lambda ()
     (let* ((params (et-at 2 (et-parse-arglist arglist))))
       (when-let* ((decls (et-at-offset 3 (et-find-and-parse-func-decls params rest))))
         (et-symbol-set-func-decls name decls))))))

(et-set-identifier '@def #'et:identify-@def)

(et-defun et:identify-@def (name: Var arglist: ListR<Any> &rest declares: Sexps) EtIdentifyPlist
  (list
   :declare
   (lambda ()
     (let* ((defun-expr (macroexpand-1 `(et-defun ,name ,arglist (declare (et ,@declares))))))
       (apply #'et:identify-defun (cdr defun-expr))))))

(et-set-identifier '@check #'et:identify-@check)

(et-defun et:identify-@check (name: Var chk: Sexp) EtIdentifyPlist
  (list
   :declare
   (lambda ()
     (et-set-checker name (eval `(lambda () (et-check ,chk)))))))


;;;; Macroexpand

(et-set-identifier #'et-defun #'et:identify-macroexpand)

(defun et:identify-macroexpand (&rest _body)
  "Identify an expression by identifying its macro expansion."
  (let* ((expanded (macroexpand-1 (et-cur-expr))))
    (et-at 0 (et-with-sticky-path (et-identify-expr expanded)))))


;;;; Struct

(et-set-identifier #'defun #'et:identify-cl-defstruct)

(et-defun et:identify-cl-defstruct (&rest body) EtIdentifyPlist
  "Identify a `cl-defstruct' expression, returning a processing plist."
  (let* ((name-or-opts (car body))
         (name (if (consp name-or-opts) (car name-or-opts) name-or-opts))
         (opts (when (consp name-or-opts) (cdr name-or-opts)))
         (conc-name (if-let* ((entry (assq :conc-name opts))) (cadr entry)
                      (intern (format "%s-" name))))
         (constructor (if-let* ((entry (assq :constructor opts))) (cadr entry)
                        (intern (format "make-%s" name))))
         (copier (if-let* ((entry (assq :copier opts))) (cadr entry)
                   (intern (format "copy-%s" name))))
         (predicate (if-let* ((entry (assq :predicate opts))) (cadr entry)
                      (intern (format "%s-p" name))))
         (slots-start (if (stringp (cadr body)) 2 1))
         (slot-forms (nthcdr slots-start body))
         slots genvec genvec-rel generics)

    ;; Parse slots
    (dotimes (slot-idx (length slot-forms))
      (let ((rel (+ 1 slots-start slot-idx)))
        (et-error-boundary rel
          (pcase (nth slot-idx slot-forms)
            ((and s (pred symbolp))
             (push (list rel s nil nil) slots))
            (`(,(and s (pred symbolp)) ,default . ,plist)
             (push (list rel s default
                         (when-let* ((pos (cl-position :et plist))
                                     ((= 0 (mod pos 2))))
                           (cons (+ 3 pos) (nth (1+ pos) plist))))
                   slots)
             (when-let* ((pos (cl-position :et-generics plist))
                         ((= 0 (mod pos 2))))
               (if (> slot-idx 0) (et-err rel "Generics must be set in the first slot")
                 (setq genvec (nth (1+ pos) plist)
                       genvec-rel (list rel (+ 3 pos))
                       generics (et-genvec-generics genvec)))))
            (_ (et-err rel "Invalid slot format"))))))

    (setq slots (nreverse slots))
    (put name 'et-struct (list :generics generics))

    (list
     :constrain
     (lambda ()
       (when genvec
         (et-at genvec-rel
           (plist-put (get name 'et-struct) :constraints
                      (et-genvec-constraints genvec)))))

     :populate
     (lambda ()
       (let* ((plist (or (get name 'et-struct)
                         (et-fatal 0 "Struct `%s' not defined" name)))
              (constraints (plist-get plist :constraints))
              ;; Repr helpers shared across all generated functions
              ;; STRUCT-REPR is the struct's own type, e.g. *Name<T1 T2>
              (struct-repr (et-parse-repr (et-q (Struct ,name ,@generics)) generics))
              ;; ARGLIST-REPR is the single-argument arglist (STRUCT), used by
              ;; accessors and the copier
              (arglist-repr (et-parse-repr (et-q (ConsR ,struct-repr Nil)) generics)))

         ;; --- Predicate ---
         (when predicate
           (let* ((never-args (make-list (length generics) 'Never))
                  (output-repr
                   (et-parse-repr
                    (et-q (or (and True (bindsof (and T (Struct ,name ,@never-args))))
                              (and Nil (bindsof (subtract T (Struct ,name ,@never-args))))))
                    '(T))))
             (put predicate 'et-symbol-func-type
                  (et-dt 'DynFunction (et-parse-matcher '(Args T) [T]) output-repr))))

         ;; --- Accessors ---
         (dolist (slot slots)
           (pcase-let* ((`(,rel ,slot-name ,_default ,type-info) slot)
                        (accessor (if conc-name (intern (format "%s%s" conc-name slot-name))
                                    slot-name)))
             (et-error-boundary rel
               (let* ((slot-repr (if type-info
                                     (et-at (car type-info)
                                       (et-parse-repr (cdr type-info) generics))
                                   (et-parse-repr 'Any generics))))
                 (put accessor 'et-symbol-func-type
                      (et-create-function-type generics constraints
                                               arglist-repr slot-repr))))))

         ;; --- Constructor ---
         (when constructor
           (let* ((input-repr
                   (cl-loop for (rel slot-name _default type-info) in slots
                            for slot-repr = (if type-info
                                                (et-error-boundary rel
                                                  (et-at (car type-info)
                                                    (et-parse-repr (cdr type-info) generics)))
                                              (et-parse-repr 'Any nil))
                            nconc (list (intern (format ":%s" slot-name)) slot-repr) into args
                            finally return (et-parse-repr (if args `(PList ,@args) 'Nil) nil))))
             (put constructor 'et-symbol-func-type
                  (et-create-function-type generics constraints
                                           input-repr struct-repr))))

         ;; --- Copier ---
         (when copier
           (put copier 'et-symbol-func-type
                (et-create-function-type generics constraints
                                         arglist-repr struct-repr))))))))



(et-set-identifier '@get #'et:identify-@get)

(et-defun et:identify-@get (&rest body) EtIdentifyPlist
  "Identify a symbol property declaration, returning a processing plist.

During identification, just validates the format.
Returns a plist with :declare to set the symbol type."
  (pcase body
    (`(,(and symbol (pred symbolp)) ,spec)
     (list
      :declare
      (lambda ()
        (et-at 2
          (put symbol 'et-symbol-property-type (et-parse-type spec))))))

    (_ (et-fatal nil "Expected format (@symbol-property SYMBOL TYPE)"))))


;;;; require

(et-set-identifier #'require #'et:identify-require)

(et-defvar et:identify--require-loaded-libs List<Symbol> nil
  "List of libraries which have been processed due to a `require'.")

(et-defun et:identify-require (name: Symbol) EtIdentifyPlist
  (let* ((dir (when buffer-file-name (file-name-parent-directory buffer-file-name)))
         (load-path (cons dir load-path))
         (library (or (locate-library (symbol-name (eval name)))
                      (error "Library `%s' not found" name))))
    (unless (member library et:identify--require-loaded-libs)
      (push library et:identify--require-loaded-libs)
      ;; Process the buffer without propagating diagnostics
      (et-process-exprs (et-process-file-exprs library)))))


;;; ============================================================
;;; Useful helpers
;;;; Print narrows

(defcustom et-display-narrows nil
  "Whether to display narrowed types on if/when/etc blocks."
  :group 'et
  :type 'boolean)

(defun et-pp-narrows (narrows &optional sep)
  (cl-loop for (var . type) in narrows
           collect (format "%s: %s" (et:type-var->name var) (et-pp type)) into strs
           finally return (string-join strs (or sep "\\n"))))

(defun et-checker-hint-narrows (path &rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et-type-binds type) ; TODO: display just binds instead of whole type
             when binds do (et-hint path fmt (et-pp-narrows binds)))))


;;;; Resolve

(defun et-checker-resolve (type &rest path)
  "Type check an expression at PATH, ensuring that it satisfied TYPE.

TYPE is a type or an expression parseable to a type.

PATH is the path to the subexpression."
  (unless (et:type-p type) (setq type (et-parse-type type)))
  (let* ((expr-type (et-check-at type path)))
    (unless (et-subtype? expr-type type)
      (et-err path "Expected %s, found %s" type expr-type))
    type))


;;;; Infer

(defmacro et-checker-infer (type genvec matcher-spec output-spec)
  (let* ((gens (et-genvec-generics genvec))
         (constraints (et-genvec-constraints genvec)))
    `(et--checker-infer ,type ',gens ',constraints ,(list '\` matcher-spec) ,(list '\` output-spec))))

(defun et--checker-infer (type gens constraints matcher-spec output-spec)
  (let* ((result
          (et-infer (et:match-matcher-new
                     :generics gens
                     :constraints constraints
                     :repr (et-parse-repr matcher-spec gens))
                    type
                    (et-parse-repr output-spec gens))))
    (when (et:match-result->success result)
      (et:match-result->value result))))


;;;; Nicer funcall

(defun et-checker-funcall (func-type arglist-type)
  (declare (et (func-type *et:type) (arglist-type *et:type)
               (@return *et:type)))
  (let* ((result (et-funcall func-type arglist-type)))
    (when (et:match-result->success result)
      (et:match-result->value result))))


;;; ============================================================
;;; Handy checkers - `et:checker'
;;;; Macroexpand

(defun et-macroexpand-checker ()
  "Type checker which expands a macro and type-checks the expansion."
  (unless (macrop (car (et-cur-expr)))
    (et-fatal 0 "Macro not defined: %s" (car (et-cur-expr))))
  (et-check-expansion (macroexpand-1 (et-cur-expr))))

(et-define-checker-abbrev '@expand #'et-macroexpand-checker)


;;;; Function checkers

(et-defvar et:checker--checking-defun Nil|Var nil)

(defun et-check-function-body (params func-type body-path)
  "The path should point to the function expr.

Returns the type of the last expression in the body."
  (et-declare (func-type *et:type) (params EtFuncParameters<Var>) (body-path TreeR<Integer>)
              (@return *et:type))
  (let* ((returns
          (et-in-function-body func-type params
            (et-check-deferred
              (let* ((actual-ret (et-check-tail body-path)))
                (or (et-subtype? actual-ret expected-ret)
                    (et-err 0 "Expected %s, found %s" expected-ret actual-ret))
                (et-remove-type-binds-and-polys actual-ret (mapcar #'car polys)))))))
    (apply #'et-union returns)))

(et-defvar et-checking-defun Var|Nil nil
  "The defun currently being processed.")

(et-set-checker #'defun #'et:checker--defun)
(et-set-checker #'cl-defun #'et:checker--defun)
(et-defun et:checker--defun () *et:type
  (let* ((name (cadr (et-cur-expr))))
    (when-let* ((func-type (et-symbol-func-type name))
                ((not (plist-get (et-symbol-func-props name) :skip)))
                (et-checking-defun name))
      (et-check-function-body (et-symbol-func-params name) func-type 3))
    (et-literal name)))


(et-set-checker #'et-defun #'et:checker--et-defun)
(et-defun et:checker--et-defun () *et:type
  (let* ((name (cadr (et-cur-expr))))
    (when-let* ((func-type (et-symbol-func-type name))
                ((not (plist-get (et-symbol-func-props name) :skip)))
                (et-checking-defun name))
      (et-check-function-body (et-symbol-func-params name) func-type 4))
    (et-literal name)))


(et-set-checker #'lambda #'et:checker--lambda)
(et-defun et:checker--lambda () *et:type
  (let* ((arglist (cadr (et-cur-expr)))
         (body (cddr (et-cur-expr)))
         (params (et-at 1 (et-parse-arglist arglist)))
         (untyped-input nil)
         (func-type
          (or (when-let* ((decls (et-at-offset 2 (et-find-and-parse-func-decls params body)))
                          (func-type (plist-get decls :definition)))
                (when (et:type-p func-type) func-type))
              ;; From recommendation
              (et-cur-recommendation)
              ;; All params are Any
              (progn (setq untyped-input (et-untyped-func-input params))
                     (et-dt 'Function untyped-input (et Any)))))
         (actual-ret (et-check-function-body params func-type 2)))
    ;; If the function is untyped, then we should use the actual return type
    ;; as the function's return type
    (if untyped-input (et-dt 'Function untyped-input actual-ret) func-type)))


;;; ============================================================
;;; Provide

(provide 'et-checkers)


;;; et-checkers.el ends here
