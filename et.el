;;; et.el --- Typesystem for emacs lisp           -*- lexical-binding: t; -*-

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
;;; Code:

(require 'flycheck)
(require 'seq)


;;; ============================================================
;;; Type definitions
;;;; Structs

(cl-defstruct et-type
  "Struct representing a root-level et type.

CASES is a list of `et-case' instances being unioned."
  cases)

(cl-defstruct et-case
  "Struct representing a case of an et type.

FACTORS is a list of datatypes which being intersected. Each DATATYPE
has the form (KEYWORD ARGS...).

BINDS is an alist of (VARSPEC . TYPE). VARSPEC is a reference to one of
the entries in `et--binds', so it is a cons cell mapping the variable
symbol its original type. This makes it easier to discern whether the
variable being narrowed is being shadowed by another variable of the
same name."
  factors
  binds)

(cl-defmethod cl-print-object ((type et-type) stream)
  (princ (et-pp type) stream))

(defun et-datatype (dt) (make-et-type :cases (list (make-et-case :factors (list dt)))))
(defun et-dt (&rest dt) (make-et-type :cases (list (make-et-case :factors (list dt)))))
(defun et-literal (value) (et-dt :literal value))
(defun et-alias (name &rest args)
  ;; (or (get name 'et-alias) (error "Alias does not exist: %s" name))
  (et-datatype (cons :alias (cons name args))))

(defun et-any () (make-et-type :cases (list (make-et-case :factors nil))))
(defun et-never () (make-et-type :cases nil))
(defun et-nil () (et-datatype `(:literal nil)))
(defun et-t () (et-datatype `(:literal t)))

(defun et-type-factors (type)
  "Expand out the factors as a list of lists."
  (cl-loop for case in (et-type-cases type)
           collect (cl-loop for factor in (et-case-factors case)
                            collect factor)))

(defun et-never? (type)
  "Return non-nil if TYPE is not the never type."
  (null (et-type-cases type)))


;;;; Data types

;; A datatype keyword has the following symbol properties:
;;
;; et-datatype - The keyword of the property
;;
;; et-datatype-check-args: (ARGS...) -> NIL (errors if args are invalid)
;;
;; et-datatype-read-only - Prevent future definitions from overwriting
;;
;;
;; The following optional properties correspond to major type
;; operations:
;;
;; :subtype?: (ARGS OTHER-DATATYPE) -> BOOLEAN - Check if I am a
;;   subtype of OTHER-DATATYPE.
;;
;; :supertype?: (ARGS OTHER-DATATYPE) -> BOOLEAN - Check if I am a
;;   supertype of OTHER-DATATYPE. Favor using `subtype?' instead when
;;   possible.
;;
;; :type-subtype?: (ARGS OTHER-TYPE) -> BOOLEAN - Check if I am a
;;   subtype of OTHER-TYPE.
;;
;; :non-disjoint?: (ARGS OTHER-DATATYPE) -> BOOLEAN - By default, it
;;   is assumed that any two datatypes which are not subtypes of each
;;   other are disjoint. Return t if I am not necessarily disjoint
;;   from OTHER-DATATYPE. t does not mean I am definitely disjoint,
;;   but nil means I am definitely disjoint.
;;
;; :subtract: (ARGS OTHER-DATATYPE) -> TYPE or NIL - Subtract
;;   OTHER-DATATYPE from myself, or return nil if the subtraction is
;;   undefined.
;;
;; :subtract-from: (ARGS OTHER-DATATYPE) -> TYPE or NIL - Subtract
;;   myself from OTHER-DATATYPE, or return nil if the subtraction is
;;   undefined.
;;
;; :merge: (ARGS OTHER-DATATYPE) -> DATATYPE or NIL - Create a new
;;   datatype which satisfies both myself and OTHER-DATATYPE.
;;

(defmacro et-define-datatype (keyword &rest props)
  "Alias KEYWORD types to return a specific type.

\(fn keyword &key check-args subtype? supertype? non-disjoint? subtract subtract-from read-only)"
  (declare (indent 1))
  (cl-assert (keywordp keyword))

  (defconst et--datatype-properties
    '(check-args subtype? supertype? type-subtype? non-disjoint? subtract subtract-from merge read-only))

  `(ignore
    (when (get ,keyword 'et-datatype-read-only)
      (error "Cannot overwrite read-only datatype"))
    (put ,keyword 'et-datatype ,keyword)
    ,@(cl-loop for key in et--datatype-properties
               for value = (plist-get props (intern (format ":%s" key)))
               collect `(put ,keyword ',(intern (format "et-datatype-%s" key)) ,value))))

(defmacro et-datatype-funcall (method datatype &rest extra-args)
  `(let ((dt ,datatype))
     (funcall (or (et-get-datatype-property ,method dt) #'ignore)
              (cdr dt) ,@extra-args)))

(defmacro et-get-datatype-property (prop datatype)
  `(get (car ,datatype) ',(intern (format "et-datatype-%s" prop))))

(defun et--assert-no-args (&rest args)
  (when args (error "No arguments allowed")))

(defmacro et-pcase-lambda (&rest patterns)
  `(lambda (&rest args) (pcase args ,@patterns)))


;;;; Basic datatypes

(et-define-datatype :literal
  ;; :read-only t
  :check-args (lambda (_)))

(et-define-datatype :number
  ;; :read-only t
  :check-args #'et--assert-no-args
  :supertype? (et-pcase-lambda (`(() (:literal ,(pred numberp))) t)))

(et-define-datatype :integer
  ;; :read-only t
  :check-args #'et--assert-no-args
  :supertype? (et-pcase-lambda (`(() (:literal ,(pred integerp))) t))
  :subtype? (et-pcase-lambda (`(() (:number)) t)))

(et-define-datatype :string
  ;; :read-only t
  :check-args #'et--assert-no-args
  :supertype? (et-pcase-lambda (`(() (:literal ,(pred stringp))) t)))

(et-define-datatype :symbol
  ;; :read-only t
  :check-args #'et--assert-no-args
  :supertype? (et-pcase-lambda (`(() (:literal ,(pred symbolp))) t)))

(et-define-datatype :cons
  ;; :read-only t
  :check-args (et-pcase-lambda
               (`(,(pred et-type-p) ,(pred et-type-p)) nil)
               (_ (error "Expected two type arguments")))
  :subtype? (et-pcase-lambda
             (`((,l ,r) (:cons ,l2 ,r2)) (and (et-subtype? l l2) (et-subtype? r r2))))
  :supertype? (et-pcase-lambda
               (`((,l ,r) (:literal ,val))
                (and (consp val)
                     (et-subtype? (et-literal (car val)) l)
                     (et-subtype? (et-literal (cdr val)) r))))
  :non-disjoint
  (et-pcase-lambda
   (`((,l ,r) (:cons ,l2 ,r2)) (or (not (et-disjoint? l l2)) (not (et-disjoint? r r2)))))
  :subtract
  (et-pcase-lambda
   (`((,l ,r) (:cons ,l2 ,r2)) (et-dt :cons (et-subtract l l2) (et-subtract r r2))))
  :merge
  (et-pcase-lambda
   (`((,l ,r) (:cons ,l2 ,r2)) (et-dt :cons (et-and l l2) (et-and r r2)))))


;;;; Infer

(defvar et--inferring-types nil
  "Alist of symbol -> type.")

(defun et--infer (vars subtype supertype is-subtype?)
  (let* ((et--inferring-types
          (cl-loop for var in vars
                   collect (cons var (if is-subtype? (et-any) (et-never)))))
         (datatype (if is-subtype? :infer-subtype :infer-supertype))
         (replacements
          (cl-loop for var in vars
                   collect (cons (list :alias var) (list datatype var)))))
    (setq subtype (if is-subtype? (et--replace-datatype subtype replacements) subtype))
    (setq supertype (if is-subtype? supertype (et--replace-datatype supertype replacements)))
    (when (et-subtype? subtype supertype)
      (cl-loop for (_ . type) in et--inferring-types
               always (or (not is-subtype?) (not (et-never? type)))
               collect type))))

(et-define-datatype :infer-subtype
  ;; :read-only t
  :check-args (lambda (arg) (or (symbolp arg) (error "Argument must be a symbol")))
  :type-subtype? (lambda (args other-type)
                   (let ((entry (assoc (car args) et--inferring-types)))
                     (unless entry (error "Not inferring a type %s" (car args)))
                     (setcdr entry (et-and (cdr entry) other-type))))
  :supertype? (lambda (_args _other)
                (error "Cannot check if an infer type is a supertype")))

(defmacro et-infer-subtype (vars subtype supertype)
  "Infer all variables in SUBTYPE so that it matches SUPERTYPE."
  (declare (indent 1))
  (cl-assert (vectorp vars))
  `(et--infer ',(append vars nil) ,subtype ,supertype t))

(et-define-datatype :infer-supertype
  ;; :read-only t
  :check-args (lambda (arg &optional req)
                (or (symbolp arg) (error "Argument must be a symbol"))
                (when req (or (et-type-p req) (error "Requirement must be a type"))))
  :subtype? (lambda (_args _other)
              (error "Cannot check if an infer type is a supertype"))
  :supertype? (lambda (args other)
                (let ((entry (assoc (car args) et--inferring-types)))
                  (unless entry (error "Not inferring a type %s" (car args)))
                  (if (and (cadr args) (not (et-subtype? (et-datatype other) (cadr args))))
                      nil ; Does not fit the constraint
                    (setcdr entry (et-or (cdr entry) (et-datatype other)))
                    t))))

(defmacro et-infer-supertype (vars supertype subtype)
  "Infer all variables in SUPERTYPE so that it matches SUBTYPE."
  (declare (indent 1))
  (cl-assert (vectorp vars))
  `(et--infer ',(append vars nil) ,subtype ,supertype nil))


;;;; Define aliases

(defmacro et-define-alias (keyword arglist &rest body)
  "Alias KEYWORD types to return a specific type."
  (declare (indent 2))
  (cl-assert (keywordp keyword))
  (cl-assert (listp arglist))

  (let ((plist nil))
    (while (keywordp (car body))
      (setq plist (nconc plist (list (pop body) (pop body)))))

    `(et--define-alias
      ,keyword
      ,`(lambda . ,(cl--transform-lambda
                    (cons arglist body)
                    (format "et-alias%s" keyword)))
      (list ,@plist))))

(defun et--define-alias (keyword function props)
  (let ((kind (get keyword 'et-datatype-kind)))
    (unless (or (null kind) (eq kind 'alias))
      (error "Type %s is already defined as a %s, canot redefine it as an alias"
             keyword kind)))

  (when (get keyword 'et-alias-read-only)
    (error "Type %s is defined as read only" keyword))

  (when (plist-get props :read-only)
    (put keyword 'et-alias-read-only t))

  (put keyword 'et-alias function))

(defun et-expand-alias (datatype-args)
  (apply (or (get (car datatype-args) 'et-alias)
             (error "Alias %s is not defined" (car datatype-args)))
         (cdr datatype-args)))


;;;; Alias datatype

(defmacro et--alias-fn (body)
  `(lambda (args other)
     (let ((self (et-expand-alias args))
           (other (if (eq (car other) :alias) (et-expand-alias (cdr other))
                    (et-datatype other))))
       ,body)))

(et-define-datatype :alias
  :check-args (et-pcase-lambda
               (`(,(pred keywordp) . ,args) nil)
               (_ (error "Expected first argument to be a keyword")))
  :type-subtype? (lambda (args other-type) (et-subtype? (et-expand-alias args) other-type))
  :subtype? (et--alias-fn (et-subtype? self other))
  :supertype? (et--alias-fn (et-subtype? other self))
  :non-disjoint? (et--alias-fn (not (et-disjoint? self other)))
  :subtract (et--alias-fn (et-subtract self other))
  :subtract-from (et--alias-fn (et-subtract other self))
  :merge (et--alias-fn
          (pcase (et-type-factors (et-and self other))
            (`((,only)) only))))


;;;; Built-in aliases

(et-define-alias :Boolean ()
  (et-or (et-literal t) (et-nil)))

(et-define-alias :List (elem)
  (et-or (et-nil)
         (et-dt :cons elem (et-dt :alias :List elem))))

(et-define-alias :Tree (elem)
  (et-or elem
         (et-dt :List (et-dt :alias :Tree elem))))


;;; ============================================================
;;; Core type operations
;;;; Subtype

(defun et--datatype-subtype? (a b)
  (or (equal a b)
      (et-datatype-funcall subtype? a b)
      (et-datatype-funcall supertype? b a)))

(defvar et--subtype-pending nil
  "List of cons cells (A-TYPE . B-TYPE).")

(defun et-subtype? (a-type b-type)
  (or (equal a-type b-type)
      (member (cons a-type b-type) et--subtype-pending)
      (let ((et--subtype-pending (cons (cons a-type b-type) et--subtype-pending)))
        ;; For a<b, we must have a-case<b FOR ALL a cases
        (cl-loop for a-case in (et-type-cases a-type)
                 always
                 ;; For a-case<b, we must have a-factor<b FOR ANY a factor
                 (cl-loop for a-factor in (et-case-factors a-case)
                          thereis
                          ;; Some datatypes allow checking if all of b-type is a supertype at once
                          (if (et-get-datatype-property type-subtype? a-factor)
                              (et-datatype-funcall type-subtype? a-factor b-type)
                            ;; For a-factor<b, we must have a-factor<b-case FOR ANY b case
                            (cl-loop for b-case in (et-type-cases b-type)
                                     thereis
                                     ;; For a-factor<b-case, we must have a-factor<b-factor FOR ALL b factors
                                     (cl-loop for b-factor in (et-case-factors b-case)
                                              always (et--datatype-subtype? a-factor b-factor)))))))))


;;;; Disjoint

(defun et--datatype-disjoint? (a b)
  (not (or (equal a b)

           (et-datatype-funcall non-disjoint? a b)
           (et-datatype-funcall non-disjoint? b a)

           (et-datatype-funcall subtype? a b)
           (et-datatype-funcall supertype? a b)
           (et-datatype-funcall subtype? b a)
           (et-datatype-funcall supertype? b a))))

(defun et-disjoint? (a-type b-type)
  ;; For a/b, we must have a-case/b-case FOR ALL a cases and b cases
  (cl-loop for a-case in (et-type-cases a-type)
           always
           (cl-loop for b-case in (et-type-cases b-type)
                    always
                    ;; For a-case/b-case, we must have a-factor/b-factor FOR ANY a factor and b factor
                    (cl-loop for a-factor in (et-case-factors a-case)
                             thereis
                             (cl-loop for b-factor in (et-case-factors b-case)
                                      thereis (et--datatype-disjoint? a-factor b-factor))))))


;;;; Subtract

(defun et--datatype-subtract (a b)
  "Subtract A from B, like `et-subtract', but where A and B are datatypes."
  (or (et-datatype-funcall subtract a b)
      (et-datatype-funcall subtract-from b a)
      (and (et--datatype-subtype? a b) (et-never))
      (et-datatype a)))

(defun et-subtract (type remove)
  "Create a subtype of TYPE with some elements of REMOVE removed.

Nothing will be removed which is not in REMOVE. Aka, any element in TYPE
but not in the subtraction is guaranteed to be in REMOVE. However, some
types in the subtraction may also be in REMOVE."

  ;; (A ∪ B) - C = (A - B) ∪ (A - C)
  (cl-loop for a-case in (et-type-cases type)
           collect
           ;; (A ∩ B) - C = (A - C) ∩ (B - C)
           (cl-loop for a-factor in (et-case-factors a-case)
                    collect
                    ;; A - (B ∪ C) = (A - B) ∩ (A - C)
                    (cl-loop for b-case in (et-type-cases remove)
                             ;; A - (B ∩ C) = (A - B) ∪ (A - C)
                             collect
                             (cl-loop for b-factor in (et-case-factors b-case)
                                      collect (et--replace-type-binds
                                               (et--datatype-subtract a-factor b-factor)
                                               (et-case-binds a-case))
                                      into b-factor-results
                                      ;; union of factor subtractions = list of cases with one factor each
                                      finally return (apply #'et-or b-factor-results))
                             into b-case-results
                             ;; intersection of case subtractions
                             finally return (apply #'et-and b-case-results))
                    ;; intersectin
                    into a-factor-results
                    finally return (apply #'et-and a-factor-results))
           ;; union
           into a-case-results
           finally return (apply #'et-or a-case-results)))


;;;; Simplify

(defun et--datatype-intersection (a b)
  "Attempt to merge two datatypes into a single equivalent datatype.

Returns a single datatype, or nil if it is impossible."
  (or (et-datatype-funcall merge a b)
      (and (et--datatype-subtype? a b) a)
      (and (et--datatype-subtype? b a) b)))

(defun et--simplify-factors (factors)
  "Given FACTORS, return an equivalent but simplified list of factors."

  ;; Search through every possible pair of factors to check if
  ;; `et--datatype-intersection' can merge them. If it can, then
  ;; replace both factors with the merged factor.
  (cl-loop for (next . tail) on factors
           unless (cl-loop for new-tail on new-factors
                           for simple = (et--datatype-intersection next (car new-tail))
                           when simple do (setcar new-tail simple)
                           thereis simple)
           collect next into new-factors
           finally return new-factors))

(defun et-simplify (type)
  "Simplify TYPE.

This function will fail to simplify certain aliases whose expansions
could be simplified (specifically, aliases where neither are a subtype
of the other). It would be possible to first expand out all aliases
before performing simplification, but this would result in a simplified
type with all aliases removed, and worse would cause an infinite loop
for recursive aliases."
  (let ((simple-cases
         (cl-loop for (case . rest) on (et-type-cases type)
                  ;; If any two factors are disjoint, then the case is empty
                  when (cl-loop for (a . rest) on (et-case-factors case)
                                always (cl-loop for b in rest
                                                always (not (et--datatype-disjoint? a b))))
                  collect (make-et-case :factors (et--simplify-factors (et-case-factors case))
                                        :binds (et-case-binds case)))))

    (cl-loop for (case . rest) on simple-cases
             ;; Check if this case is redundant
             unless (cl-loop for c in (append new-cases rest)
                             ;; case is a subtype of c, so case is redundant
                             thereis (and (et-subtype? (make-et-type :cases (list case))
                                                       (make-et-type :cases (list c)))
                                          (equal (et-case-binds c) (et-case-binds case))))
             collect case into new-cases
             finally return (make-et-type :cases new-cases))))


;;; ============================================================
;;; Typesystem helpers
;;;; Bindings

(defun et--intersect-binds (a-binds b-binds)
  "Return a set of binds which satisfy both A-BINDS and B-BINDS."
  (cl-loop for varlist in (seq-uniq (mapcar #'car (append a-binds b-binds)))
           collect (cons varlist
                         (et-and
                          (or (alist-get varlist a-binds) (et-any))
                          (or (alist-get varlist b-binds) (et-any))))))

(defun et--type-binds (type)
  "Return the bindings implied by TYPE.

Since any one of the cases may be valid, the binding for a particular
variable is the union of the possible types for that variable in each
case.

Returns a list of (VARSPEC . TYPE)."
  (when-let ((alists (mapcar #'et-case-binds (et-type-cases type))))
    (cl-loop for (varspec . type) in (car alists)
             for types = (cl-loop for alist in alists
                                  for type = (alist-get varspec alist)
                                  always type collect type)
             for merged = (when types (apply #'et-or types))
             when (and merged (not (equal merged (et-any))))
             collect (cons varspec merged))))

(defun et--map-type-binds (type function)
  "Return a copy of TYPE with the bindings modified.

The new bindings for each case is determined is determined by calling
FUNCTION with the old bindings for that case."
  (cl-loop for case in (et-type-cases type)
           for copy = (copy-et-case case)
           do (cl-loop for (varspec . bind-type) in (funcall function (et-case-binds copy))
                       for and-type = (et--replace-type-binds
                                       (et-and (et--get-var-bind (car varspec))
                                               bind-type)
                                       nil)
                       ;; If the bind is never then remove this entire case
                       when (et-never? and-type)
                       do (progn (setq copy nil) (cl-return nil))
                       ;; Remove useless (any) new binds
                       unless (et-subtype? (cdr varspec) bind-type)
                       collect (cons varspec and-type) into new-binds
                       finally do (setf (et-case-binds copy) new-binds))
           when copy
           collect copy into case-copies

           finally return (make-et-type :cases case-copies)))

(defun et--replace-type-binds (type binds)
  "Return a copy of TYPE with all binds set to BINDS."
  (et--map-type-binds type (lambda (_) binds)))

(defun et--intersect-type-binds (type binds)
  "Return a copy of TYPE with BINDS intersected into its bindings."
  (et--map-type-binds type (lambda (bs) (et--intersect-binds bs binds))))


;;;; And/or

(defun et-and (&rest types) (et-simplify (apply #'et-raw-and types)))
(defun et-or (&rest types) (et-simplify (apply #'et-raw-or types)))

(defun et-raw-or (&rest types)
  (cl-loop for type in types
           append (et-type-cases type) into cases
           finally return (make-et-type :cases cases)))

(defun et-raw-and (&rest types)
  (pcase types
    (`() (et-any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et-and a (apply #'et-and b c rest)))
    (`(,a ,b)
     (cl-loop for ac in (et-type-cases a)
              append
              (cl-loop for bc in (et-type-cases b)
                       for factors = (append (et-case-factors ac) (et-case-factors bc))
                       collect (make-et-case
                                :factors factors
                                :binds (et--intersect-binds
                                        (et-case-binds ac) (et-case-binds bc))))
              into all-cases
              finally return (make-et-type :cases all-cases)))))


;;;; Parsing

(defun et-parse (spec)
  "Parse a type keyword SPEC into an `et-type'.

Syntax (within the keyword name, after the leading colon):
  foo            → (:Foo)
  foo<A~B>       → (:foo (:A) (:B))
  Foo            → (:alias :Foo)
  Foo<A~B>       → (:alias :Foo (:A) (:B))
  A|B            → union of A, B
  A&B            → intersection of A, B
  A|B&C          → union of A with intersection of B,C

Operator precedence: & binds tighter than |.
Type names must match [A-Z][a-zA-Z0-9]*."
  (let ((case-fold-search nil))
    (pcase spec
      ((pred et-type-p) spec)
      ((pred keywordp) (et--parse-string (substring (symbol-name spec) 1)))
      ((pred stringp) (et--parse-string spec))

      (`(:or . ,args) (apply #'et-raw-or (mapcar #'et-parse args)))
      (`(:and . ,args) (apply #'et-raw-and (mapcar #'et-parse args)))
      (`(:any) (et-any))
      (`(:never) (et-never))
      (`(:nil) (et-nil))
      (`(:t) (et-t))

      (`(:sym ,str) (et-literal (if (symbolp str) str (intern str))))
      (`(:str ,str) (et-literal str))
      (`(:num ,str) (et-literal (if (numberp str) str (string-to-number str))))

      (`(,(and name (or :infer :infer-supertype :infer-subtype)) ,var . ,rest)
       (cl-assert (keywordp var))
       (when (eq name :infer) (setq name :infer-supertype))
       (et-dt name var (when (car rest) (et-parse (car rest)))))

      (`(,(and name (pred keywordp)) . ,args)
       (if (string-match-p "^:[A-Z]" (symbol-name name))
           (apply #'et-alias name (mapcar #'et-parse args))
         (apply #'et-dt name (mapcar #'et-parse args))))

      (_ (error "Invalid type spec: %s" spec)))))

(defun et--parse-string (s)
  "Parse type string S (no leading colon) into an `et-type'.
Splits on | at depth 0, then & at depth 0, then parses atoms."
  (when (string-empty-p s)
    (error "Empty type expression"))
  (cl-loop for or-seg in (et--split-at-depth s ?|)
           when (string-empty-p or-seg)
           do (error "Empty segment in union type: %s" s)
           collect
           (cl-loop for and-seg in (et--split-at-depth or-seg ?&)
                    when (string-empty-p and-seg)
                    do (error "Empty segment in intersection type: %s" s)
                    collect (et--parse-atom and-seg) into and-parts
                    finally return (apply #'et-raw-and and-parts))
           into or-parts
           finally return (apply #'et-raw-or or-parts)))

(defun et--parse-atom (s)
  "Parse a single type atom into an `et-type'."
  (if (eq (aref s 0) ?{)
      ;; {expr} — grouped/parenthesized compound expression
      (if (eq (aref s (1- (length s))) ?})
          (et--parse-string (substring s 1 -1))
        (error "Unclosed brace in: %s" s))

    (unless (string-match "^\\([A-Za-z][a-zA-Z0-9]*\\)\\(?:<\\(.*\\)>\\)?$" s)
      (error "Invalid type syntax: %s" s))

    ;; Name or Name<...>
    (pcase (list (intern (concat ":" (match-string 1 s))) (match-string 2 s))
      (`(,name nil) (et-parse (list name)))
      (`(,(and name (or :sym :str :num)) ,str) (et-parse (list name str)))
      (`(,(and name (or :infer :infer-subtype)) ,inner)
       (or (string-match "^\\([A-Z][a-zA-Z0-9]*\\)\\(?:=\\(.*\\)\\)?$" inner)
           (error "Invalid infer format: %s" inner))
       (et-parse (list name (intern (concat ":" (match-string 1 inner))) (match-string 2 inner))))
      (`(,name ,inner)
       (et-parse (cons name (et--split-at-depth inner ?~)))))))

(defun et--split-at-depth (s delim)
  "Split string S on character DELIM at depth 0 only.
Depth tracks < > and { } nesting."
  (let ((depth 0) (start 0) (result '()))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (cond ((memq c '(?< ?{)) (cl-incf depth))
              ((memq c '(?> ?})) (cl-decf depth))
              ((and (eq c delim) (= depth 0))
               (push (substring s start i) result)
               (setq start (1+ i))))))
    (push (substring s start) result)
    (nreverse result)))


;;;; Printing

(defvar et-pp-show-binds t
  "Whether to display binds in `et-pp'.")

(defun et-pp (type)
  "Format an `et-type' into a human-readable string."
  (let ((cases (et-type-cases type)))
    (if (null cases)
        "nothing"
      (mapconcat #'et--format-case cases " | "))))

(defun et--format-case (case)
  "Format an `et-case' into a human-readable string."
  (let* ((factors (et-case-factors case))
         (parts (if (null factors) (list "anything")
                  (mapcar #'et--format-datatype factors)))
         (and-str (mapconcat #'identity parts " & "))
         (binds (et-case-binds case)))
    (if (not (and binds et-pp-show-binds)) and-str
      (cl-loop for ((var . _base) . type) in binds
               collect (format "%s: %s" var (et-pp type)) into strs
               finally return
               (format "%s {%s}" and-str (string-join strs " "))))))

(defun et--format-datatype (dt)
  "Format a single datatype factor into a human-readable string."
  (pcase dt
    (`(:alias . ,rest) (et--format-datatype rest))
    (`(:literal ,val)
     (if (and (symbolp val) val (not (eq val t)))
         (format "`%s'" val)
       (prin1-to-string val)))
    (`(:cons ,left ,right)
     (let* ((elems (list (et-pp left))))
       (while (pcase right
                ((and (pred et-type-p) r)
                 (let ((rcases (et-type-cases r)))
                   (when (and (= (length rcases) 1)
                              (= (length (et-case-factors (car rcases))) 1)
                              (null (et-case-binds (car rcases))))
                     (let ((inner (car (et-case-factors (car rcases)))))
                       (pcase inner
                         (`(:cons ,car-type ,cdr-type)
                          (nconc elems (list (et-pp car-type)))
                          (setq right cdr-type)
                          t)
                         (_ nil))))))))
       ;; Check if the tail is (:literal nil)
       (let ((tail-nil-p
              (and (et-type-p right)
                   (let ((rcases (et-type-cases right)))
                     (and (= (length rcases) 1)
                          (let ((f (et-case-factors (car rcases))))
                            (and (= (length f) 1)
                                 (null (et-case-binds (car rcases)))
                                 (equal (car f) '(:literal nil)))))))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et-pp right))))))
    (`(,(and kw (guard (keywordp kw))) . ,args)
     (let ((name (substring (symbol-name kw) 1)))
       (cl-loop for arg in args
                collect (if (et-type-p arg) (et-pp arg) (format "%s" arg)) into strs
                finally return
                (if strs (format "%s<%s>" name (string-join strs ", ")) name))))
    (_ (error "Invalid datatype: %S" dt))))


;;;; Replacing

(defun et--map-factors (type function)
  (cl-loop with type-copy = (copy-et-type type)
           for case in (et-type-cases type)
           collect
           (cl-loop with case-copy = (copy-et-case case)
                    for factor in (et-case-factors case)
                    collect (funcall function factor) into new-factors
                    finally do (setf (et-case-factors case-copy) new-factors)
                    finally return case-copy)
           into new-cases
           finally do (setf (et-type-cases type-copy) new-cases)
           finally return type-copy))

(defun et--replace-datatype (type alist)
  "Return a copy of TYPE with all mappings in ALIST replaced.

ALIST is a list of (TARGET-DATATYPE . REPLACEMENT-DATATYPE)."
  (et--map-factors
   type
   (lambda (factor)
     (or (alist-get factor alist nil nil #'equal)
         (cl-loop for arg in (cdr factor)
                  collect (if (not (et-type-p arg)) arg
                            (et--replace-datatype arg alist))
                  into new-args
                  finally return (cons (car factor) new-args))))))

(defun et--map-datatype-to-type (type func)
  "Replace each factor with a custom type returned by FUNC."
  (cl-loop for case in (et-type-cases type)
           collect
           (cl-loop for factor in (et-case-factors case)
                    collect
                    (or (funcall func factor)
                        (cl-loop for arg in (cdr factor)
                                 collect (if (not (et-type-p arg)) arg
                                           (et--map-datatype-to-type arg func))
                                 into new-args
                                 finally return (et-datatype (cons (car factor) new-args))))
                    into new-factors
                    finally return (apply #'et-raw-and new-factors))
           into new-cases
           finally return (apply #'et-raw-or new-cases)))


;;; ============================================================
;;; Checking
;;;; Path

(defvar et--current-expr nil)
(defvar et--current-path nil)

(defmacro et-with-path (path &rest body)
  (declare (indent 1))
  (let ((path-var (make-symbol "path"))
        (parent-var (make-symbol "parent")))
    `(let* ((,path-var ,path)
            (et--current-path (append et--current-path ,path-var))
            (,parent-var (when ,path-var (et--traverse-tree (butlast ,path-var) et--current-expr)))
            (et--current-expr (if ,path-var (nth (car (last ,path-var)) ,parent-var)
                                et--current-expr)))
       (unwind-protect (progn ,@body)
         (when ,path-var
           (setf (nth (car (last ,path-var)) ,parent-var)
                 et--current-expr))))))

(defmacro et--label-errors (expr)
  `(condition-case err ,expr
     (error
      (let ((str (error-message-string err)))
        (if (string-match-p "\0;;flycheck-path:([0-9 ]*)\\'" str)
            (error str)
          (error (format "%s\0;;flycheck-path:%s" str et--current-path)))))))

(defun et--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (cdr path) (nth (car path) tree))))


;;;; Binds

(defvar et--binds nil)
(defvar et--narrow-binds nil)

(defun et--get-var-bind (var)
  (when-let ((base-bind (assoc var et--binds)))
    (or (alist-get base-bind et--narrow-binds)
        (cdr base-bind))))

(defmacro et-with-binds (binds &rest body)
  (declare (indent 1))
  `(let ((et--binds (append ,binds et--binds)))
     ,@body))

(defmacro et-with-narrow-binds (binds &rest body)
  (declare (indent 1))
  `(let ((et--narrow-binds (append ,binds et--narrow-binds)))
     ,@body))

(defun et-pp-binds (binds &optional sep)
  (cl-loop for (var . type) in binds
           collect (format "%s: %s" (if (symbolp var) var (car var)) (et-pp type)) into strs
           finally return (string-join strs (or sep "\\n"))))


;;;; Error/warn

(defun et--error-advice (error string &rest args)
  (if et--current-path
      (apply error (format "%s\0;;flycheck-path:%s" string et--current-path)
             args)
    (apply error string args)))

(advice-add #'error :around #'et--error-advice)

(defun et-warn (path msg &rest args)
  (setq msg (format "%s\0;;flycheck-path:%s" msg (append et--current-path path)))
  (apply #'byte-compile-warn msg args))

(defvar et-display-narrows nil
  "Whether to display narrowed types on if/when/etc blocks.")

(defun et-warn-narrows (&rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et--type-binds type)
             when binds
             collect (format fmt (et-pp-binds binds)) into strs
             finally do
             (when strs
               (et-warn '(0) (string-join strs "\\n"))))))


;;;; Root

(defmacro et--root (expr &rest body)
  (declare (indent 1))
  `(progn
     (cl-assert (null et--current-expr))
     (cl-assert (null et--current-path))
     (cl-assert (null et--binds))
     (let ((et--current-expr ,expr))
       ,@body)))


;;;; Define checker

(defmacro et-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(prog1 ',expr-type
     (setf (get ',expr-type 'et-checker)
           (lambda . ,(cl--transform-lambda (cons arglist body) (format "et--checker:%s" expr-type))))))

(defmacro et-define-type-checker (func &rest arguments)
  "Define a checker using argument and return types.

FUNC is the function to define the checker for.

GENERICS is a vector of symbols, representing generic variables. Each
generic variable should be uppercase.

\(fn FUNC [GENERICS] ARGS... RETURN)"
  (declare (indent 2))
  (cl-assert (symbolp func))

  (let ((generics (when (vectorp (car arguments))
                    ;; Make sure the generics have the correct format
                    (cl-loop for var across (car arguments)
                             do (or (keywordp var) (error "Generics vector should contain symbols"))
                             do (or (let ((case-fold-search nil))
                                      (string-match-p "^:[A-Z]" (format "%s" var)))
                                    (error "Var should start with an uppercase letter")))
                    (pop arguments))))
    (unless arguments (error "No return type specified"))

    `(et-define-checker ,func (&rest exprs)
       (let ((arg-types (list ,@(mapcar #'et-parse (butlast arguments))))
             (return-type ,(et-parse (car (last arguments)))))

         (or (eq (length arg-types) (length exprs))
             (error "Wrong number of arguments. Expected %s, got %s" (length arg-types) (length exprs)))

         (let ((generic-types (list ,@(make-list (length generics) `(et-never))))
               type infer generics-alist)
           (dotimes (i (length arg-types))
             (setq type (et-check-path (1+ i)))
             (setq infer (or (et-infer-supertype ,generics (nth i arg-types) type)
                             (error "Invalid argument %s, expected %s" (nth i exprs)
                                    (et-pp (nth i arg-types)))))
             (cl-loop for tail on generic-types
                      for result in infer
                      unless (et-never? result)
                      do (cl-callf et-or (car tail) result)))
           (setq generics-alist
                 (cl-loop for g across ,generics
                          for type in generic-types
                          collect (cons (list :alias g) type)))
           (et--map-datatype-to-type
            return-type
            (lambda (dt) (alist-get dt generics-alist nil nil #'equal))))))))


;;;; Check

(defun et-check ()
  "Returns the type of the current expr, if typechecking did not error."
  (pcase et--current-expr
    (`(,func . ,args)
     (or (apply (or (get func 'et-checker)
                    (error "No checker for function: %s" func))
                args)
         (error "Checker for %s returned nil" func)))
    ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

     ;; Allow for type narrowing on variable names.
     ;; For example, if a: Number | nil,
     ;; then return the type Number&{a: Number} | nil
     (let* ((var-type (or (et--get-var-bind sym) (error "Free variable: %s" sym)))
            (varspec (assoc sym et--binds)))
       (cl-loop for case in (et-type-cases var-type)
                for case-type = (make-et-type :cases (list case))
                collect
                (et--intersect-type-binds case-type (list (cons varspec case-type)))
                into case-types
                finally return (apply #'et-or case-types))))

    (expr (et-literal expr))))


;;;; Check position helpers

(defsubst et-check-path (&rest path)
  (et-with-path path (et-check)))

(defun et-check-tail (start)
  (cl-loop for idx upfrom start below (length et--current-expr)
           for type = (et-with-path (list idx) (et-check))
           finally return (or type (et-nil))))


;;;; Root level functions

(defmacro et-root-block (&rest body)
  (et--root (cons #'progn body)
    (et-check-tail 1)
    et--current-expr))

(defun et-root-check (expr)
  (et--root expr (et-check)))

(defun et-resolve (type)
  (setq type (et-parse type))

  (let ((expr-type (et-check)))
    (unless (et-subtype? expr-type type)
      (error "Type %s is not assignable to type %s"
             (et-pp expr-type) (et-pp type)))))

(defun et-root-resolve (type expr)
  (et--root expr (et-resolve type)))


;;; ============================================================
;;; Utils
;;;; Assert at compile type

(defmacro et-assert-error (expr)
  (condition-case _err (eval expr)
    (error nil)
    (:success (error "Expected error"))))

(defmacro et-assert-success (expr)
  (ignore (eval expr)))

(defmacro et-assert-equal (expr1 expr2)
  (declare (indent 1))
  (let ((v1 (eval expr1))
        (v2 (eval expr2)))
    (or (equal v1 v2) (error "Expressions not equal: \"%s\" \"%s\"" v1 v2))))

(defmacro et-assert-true (expr)
  (or (eval expr) (error "Returned nil")))

(defmacro et-assert-nil (expr)
  (when (eval expr) (error "Returned non-nil")))


;;;; Flycheck move error

(defun et--flycheck-reposition-error (err)
  "If ERR has a ;;flycheck-path: sentinel, reposition it."
  (with-demoted-errors "Error in reposition: %s"
    (when-let* ((msg (flycheck-error-message err))
                (match (string-match "\0;;flycheck-path:\\((.*)\\)" msg))
                (path (car (read-from-string (match-string 1 msg))))
                (prev-start t))

      ;; Strip the sentinel from the displayed message
      (setf (flycheck-error-message err)
            (replace-regexp-in-string "\\\\n" "\n" (substring msg 0 match)))
      (if (eq (flycheck-error-level err) 'warning)
          (setf (flycheck-error-level err) 'info))

      ;; Find the macro call in the buffer and walk the path
      (with-current-buffer (flycheck-error-buffer err)
        (save-excursion
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

(add-hook 'flycheck-process-error-functions #'et--flycheck-reposition-error)


;;;; Testing checkers

(et-define-checker :assert-subtype (_expr type)
  (let ((expr-type (et-check-path 1)))
    (or (et-subtype? expr-type (eval type))
        (error "Not subtype: %s" (et-pp expr-type)))
    (setq et--current-expr "dummy")
    (et-nil)))

(et-define-checker :assert-error (_expr)
  (condition-case _err (et-check-path 1)
    (error (setq et--current-expr nil) (et-nil))
    (:success (error "Didn't error"))))

(et-define-checker :typeof (_expr)
  (et-warn '(0) "%s" (et-pp (et-check-path 1)))
  (setq et--current-expr nil)
  (et-nil))

(et-define-checker :narrows ()
  (cl-loop for ((var . _) . type) in (reverse et--narrow-binds)
           collect (format "%s: %s" var (et-pp type)) into strs
           finally do
           (et-warn '(0) "%s" (string-join strs "\\n")))
  (setq et--current-expr nil)
  (et-nil))


;;; ============================================================
;;; Provide

(provide 'et)


;;; et.el ends here
