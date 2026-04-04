;;; types.el --- Typesystem for emacs lisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou;; -*- lexical-binding: t; -*- <benzanol@nixos>
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

;; EXPR-TYPE ::= SYMBOL (function name)
;;            |  :number
;;            |  :string
;;            |  :symbol
;;
;; TYPE ::= (BASE-TYPE TYPE-ARGS...)
;; BASE-TYPE ::= keyword (capitalized)
;; TYPE-ARGS ::= plist

;; A resolver is (EXPR TYPE) -> ERROR?
;; A checker is (EXPR) -> ERROR?
;; A matcher is (CHILD-TYPE PARENT-TYPE) -> ERROR?


;;; Code:
;;; ============================================================
;;; Core
;;;; Flycheck

(defvar types--flycheck-path nil)
(defvar types--flycheck-offset 0)

(defmacro types--error (msg &rest args)
  `(error ,(format "%s\0;;flycheck-path:%%s" msg)
          ,@args types--flycheck-path))


(defun types--flycheck-reposition-error (err)
  "If ERR has a ;;flycheck-path: sentinel, reposition it."
  (with-demoted-errors "Error in reposition: %s"
    (when-let* ((msg (flycheck-error-message err))
                (match (string-match "\0;;flycheck-path:\\((.*)\\)" msg))
                (path (car (read-from-string (match-string 1 msg))))
                (prev-start t))

      ;; Strip the sentinel from the displayed message
      ;; (setf (flycheck-error-message err)
      ;;       (substring msg 0 match))

      ;; Find the macro call in the buffer and walk the path
      (with-current-buffer (flycheck-error-buffer err)
        (save-excursion
          ;; (debug)
          (goto-char (flycheck-error-pos err)) ; start near the error
          (beginning-of-defun)

          (dolist (idx path)
            (cl-assert (looking-at-p "[[(]"))
            (forward-char 1)
            (dotimes (_ idx) (forward-sexp))
            (forward-sexp) (backward-sexp))

          (setf (flycheck-error-line err) (line-number-at-pos))
          (setf (flycheck-error-column err) (1+ (current-column)))
          (forward-sexp)
          (setf (flycheck-error-end-line err) (line-number-at-pos))
          (setf (flycheck-error-end-column err) (1+ (current-column))))))
    ;; return nil so other handlers still run
    nil))

(add-hook 'flycheck-process-error-functions #'types--flycheck-reposition-error)


;;;; Define resolver

;; The resolvers list is a list of
;; (BASE-TYPE . (PATTERN FUNC))

(defmacro types-define-resolver (expr-pattern type-pattern &rest body)
  "Define a type resolver.

EXPR-PATTERN can be (:primitive var) or (func args...).

TYPE-PATTERN can be (:Type args...) or `var'."
  (declare (indent 2))

  (let ((make-fn
         (lambda (name)
           `(let ((sym (make-symbol ,name)))
              (fset sym (cons #'lambda
                              (cl--transform-lambda
                               ',(cons (list (if (keywordp (car expr-pattern))
                                                 (cadr expr-pattern)
                                               (cons '_ (cdr expr-pattern)))
                                             (if (consp type-pattern)
                                                 (cons '_ (cdr type-pattern))
                                               type-pattern))
                                       body)
                               ,name)))
              sym))))

    (pcase expr-pattern
      (`(,(and (pred symbolp) expr-type) . ,_args)
       (cl-assert (or (not (keywordp expr-type))
                      (memq expr-type '(:any :number :string :symbol))))

       (pcase type-pattern
         ;; If it is for a specific base type
         (`(,base-type . ,_type-args)
          (cl-assert (keywordp base-type))
          (cl-assert (string-match-p "^:[A-Z]" (symbol-name base-type)))

          `(let ((func ,(funcall make-fn (format "types--%s-is-%s" expr-type base-type))))
             (setf (alist-get ',base-type (get ',expr-type 'types-resolvers)) func)))

         ;; If it is for a generic type
         (_
          `(let ((func ,(funcall make-fn (format "types--%s-is-type" expr-type))))
             (setf (get ',expr-type 'types-default-resolver) func)))))
      (_ (error "Invalid expr pattern")))))


;;;; Define checker

(defmacro types-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(setf (get ',expr-type 'types-checker)
         (lambda ,arglist . ,body)))


;;;; Check

(defvar types--binds nil)

(defun types--get-expr-type (expr)
  (pcase expr
    (`(,(and (pred symbolp) sym) . ,_args) sym)
    ((pred numberp) :number)
    ((pred stringp) :string)
    ((pred symbolp) :symbol)
    (_ (types--error "Invalid expr %s" expr))))

(defmacro types--resolve (indices type expr)
  (cl-assert (listp indices))
  `(progn
     (cl-assert (eq types--flycheck-offset 0))
     (let ((types--flycheck-path (append types--flycheck-path ',indices)))
       (types-resolve ,type ,expr))))

(defun types-resolve (type expr)
  ;; Right now, this will result in types-check being called many
  ;; times for the same deeply-nested expression
  (when (keywordp type) (setq type (types-parse type)))

  (or
   ;; If the checker confirms it immediately, its fine
   (equal type (types-check expr))

   (if (and (symbolp expr) expr (not (eq expr t)))
       (let ((bind-type (alist-get expr types--binds)))
         (or bind-type (types--error "Free variable %s" expr))
         (or (types-a-is-b bind-type type)
             (types--error "Variable %s has type %s, expected %s" expr bind-type type)))

     (let* ((base-type (car type))
            (expr-type (types--get-expr-type expr))

            (resolver (or (alist-get base-type (get expr-type 'types-resolvers))
                          (alist-get base-type (get :any 'types-resolvers))
                          (get expr-type 'types-default-resolver))))

       (message "%s %s %s" type expr resolver)
       (if resolver (funcall resolver expr type)
         (types--error "Expression %s cannot be assigned to type %s" expr base-type))))))

(defun types-check (expr)
  (let* ((expr-type (types--get-expr-type expr))
         (checker (get expr-type 'types-checker)))
    (when checker (apply checker (cdr expr)))))


;;;; Parse a type

(defun types-parse-split (s)
  "Split S on ~ but only at depth 0 (not inside angle brackets)."
  (let ((depth 0)
        (start 0)
        (result '()))
    (cl-loop for i from 0 below (length s)
             for c = (aref s i)
             do (cond ((eq c ?<) (cl-incf depth))
                      ((eq c ?>) (cl-decf depth))
                      ((and (eq c ?~) (= depth 0))
                       (push (substring s start i) result)
                       (setq start (1+ i)))))
    (push (substring s start) result)
    (nreverse result)))

(defun types-parse (spec)
  (if (not (keywordp spec)) spec
    (let ((name (symbol-name spec)))
      (if (string-match "^:\\([A-Z][A-Za-z]*\\)<\\(.*\\)>$" name)
          (let* ((base  (intern (format ":%s" (match-string 1 name))))
                 (inner (match-string 2 name))
                 (parts (types-parse-split inner)))
            (cons base
                  (mapcar (lambda (s)
                            (types-parse (intern (format ":%s" s))))
                          parts)))
        (list spec)))))


;;;; Is subtype

(defun types-a-is-b (a b)
  "Can an object of type A be assigned to a variable of type B?"
  (pcase (list a b)
    ((guard (equal a b)) t)
    (`(,_ (:Any)) t)
    ('((:Integer) (:Number)) t)
    (`((:List ,ae) (:List ,ab)) (types-a-is-b ae ab))))


;;; ============================================================
;;; Macros
;;;; Assert at compile type

(defmacro types-assert-error (expr)
  (condition-case _err (eval expr)
    (error nil)
    (:success (error "Expected error"))))

(defmacro types-assert-success (expr)
  (eval expr))


;;;; Errored "or"

(defmacro types-or (&rest forms)
  `(or ,@(cl-loop for tail on forms
                  if (eq (length tail) 1)
                  collect (car tail)
                  else
                  collect `(ignore-errors (or ,(car tail) t)))))


;;;; Process a block

(defun types-block-func (&rest body)
  (let* ((new-body-rev nil))

    (dolist (expr body)
      (pcase expr

        ;; let
        (`(let* ,forms . ,let-body)
         (let ((let-binds-rev nil)
               (new-forms-rev nil))
           ;; Process let forms
           (dolist (form forms)
             (pcase form
               (`(,var ,type ,val)
                (setq type (types-parse type))
                (let ((types--binds (append let-binds-rev types--binds))
                      (types--flycheck-path
                       (list (+ types--flycheck-offset (length new-body-rev)) 1
                             (length new-forms-rev) 1)))
                  (types-resolve type val))
                (push (list var val) new-forms-rev)
                (push (cons var type) let-binds-rev))
               (_ (push form new-forms-rev))))
           ;; Process let body
           (let ((types--flycheck-offset 2)
                 (types--flycheck-path
                  (append types--flycheck-path
                          (list (+ types--flycheck-offset (length new-body-rev)))))
                 (types--binds (append let-binds-rev types--binds)))
             (push `(let (,(reverse new-forms-rev))
                      ,@(apply #'types-block-func let-body))
                   new-body-rev))))

        ;; setq
        (`(setq ,var ,val)
         (let ((type (or (alist-get var types--binds)
                         (types--error "Free variable %s" var))))
           (types-resolve type val))
         (push expr new-body-rev))

        ;; dolist
        (`(dolist (,var ,(and (pred keywordp) etype) ,lst) . ,dolist-body)
         (setq etype (types-parse etype))
         (types-or (types-resolve `(:List ,etype) lst)
                   (types--error "Invalid list element type"))
         (let ((types--binds (cons (cons var etype) types--binds)))
           (push `(dolist (,var ,lst)
                    ,@(apply #'types-block-func dolist-body))
                 new-body-rev)))

        ;; Process other expr
        (_ (types-check expr)
           (push expr new-body-rev))))))

(defmacro types-block (&rest body)
  ;; Alist of variables to types
  (let ((types--flycheck-offset 1))
    (apply #'types-block-func body)))


;;; ============================================================
;;; Types
;;;; Constants

(types-define-resolver (:any _) (:Any))

(types-define-resolver (:number n) (:Integer)
  (or (eq (mod n 1) 0) (types--error "Not an integer")))

(types-define-resolver (:number _n) (:Number))
(types-define-resolver (:string _str) (:String))

(types-define-resolver (:symbol sym) (:Symbol)
  (or (null sym) (eq sym t)
      (types--error "Symbol %s is not self-evaluating" sym)))

(types-define-resolver (:symbol sym) (:List _elem)
  (when sym (types--error "Symbol %s cannot be assigned to type :List" sym)))

(types-define-resolver (:symbol sym) (:Boolean)
  (unless (or (null sym) (eq sym t))
    (types--error "Symbol %s cannot be assigned to type :Boolean" sym)))


;;;; Quoted

(types-define-resolver (quote expr) (:Number)
  (unless (numberp expr) (types--error "Expression %s cannot be assigned to type :Number")))

(types-define-resolver (quote expr) (:Integer)
  (unless (and (numberp expr) (eq (mod expr 1) 0))
    (types--error "Expression %s cannot be assigned to type :Integer")))

(types-define-resolver (quote expr) (:String)
  (unless (stringp expr) (types--error "Expression %s cannot be assigned to type :String")))

(types-define-resolver (quote expr) (:Symbol)
  (unless (symbolp expr) (types--error "Expression %s cannot be assigned to type :Symbol")))

(types-define-resolver (quote expr) (:List elem)
  (unless (listp expr) (types--error "Expression %s cannot be assigned to type :List"))
  (dolist (subexpr expr) (types-resolve elem (list 'quote subexpr))))

(types-define-resolver (quote expr) (:Cons ltype rtype)
  (unless (consp expr) (types--error "Expression %s cannot be assigned to type :Cons"))
  (type--resolve ltype (list 'quote (car expr)))
  (types--resolve rtype (list 'quote (cdr expr))))


;;;; Logic

(types-define-resolver (:any expr) (:And &rest types)
  (dolist (type types)
    (types-resolve type expr)))

(types-define-resolver (:any expr) (:Or &rest types)
  (or (cl-loop for type in types
               thereis (ignore-errors (types-resolve type expr) t))
      (types--error "No :Or clauses matched")))


;;;; Arithmetic

(defmacro types--repeat-with-replacement (key values &rest exprs)
  (declare (indent 2))
  `(progn ,@(cl-loop for repl in values
                     append (cl-subst repl key exprs))))

(types--repeat-with-replacement :NumType (:Number :Integer)

  (types-define-resolver (1+ expr) (:NumType) (types-resolve '(:NumType) expr))
  (types-define-resolver (1- expr) (:NumType) (types-resolve '(:NumType) expr))

  (types-define-resolver (+ &rest exprs) (:NumType)
    (dolist (expr exprs) (types-resolve '(:NumType) expr)))

  (types-define-resolver (- &rest exprs) (:NumType)
    (dolist (expr exprs) (types-resolve '(:NumType) expr)))

  (types-define-resolver (* &rest exprs) (:NumType)
    (dolist (expr exprs) (types-resolve '(:NumType) expr)))

  (types-define-resolver (/ &rest exprs) (:NumType)
    (dolist (expr exprs) (types-resolve '(:NumType) expr))))


;;;; cons

(types-define-resolver (cons lval rval) (:Cons ltype rtype)
  (types-resolve ltype lval)
  (types-resolve rtype rval))

(types-define-resolver (cons lval rval) (:List elem)
  (types-resolve elem lval)
  (types-resolve `(:List ,elem) rval))


;;;; list

(types-define-resolver (list &rest elems) (:Cons left right)
  (or elems (types--error "Nonempty list is not a cons cell"))
  (types-resolve left (car elems))
  (types-resolve right (cons 'list (cdr elems))))

(types-define-resolver (list &rest elems) (:List elem)
  (dolist (sub-expr elems)
    (types-resolve elem sub-expr)))

(types-define-resolver (list &rest elems) (:Symbol)
  (when elems (types--error "Nonempty list cannot be symbol")))

(types-define-resolver (list &rest elems) (:Boolean)
  (when elems (types--error "Nonempty list cannot be boolean")))


;;;; car

(types-define-resolver (car expr) type
  (types-resolve `(:Cons ,type (:Any)) expr))


;;;; cdr

(types-define-resolver (cdr expr) type
  (types-resolve `(:Cons (:Any) ,type) expr))


;;; ============================================================
;;; Provide

(provide 'types)
