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

(require 'seq)


;;; Types
;;;; Datatypes

(cl-defstruct et-datatype
  "A datatype factor of an `et-type'."
  name args)

(defvar et-scoped-datatypes nil
  "Locally scoped datatypes.

An list of (NAME PLIST), where PLIST has the following properties:
  :sub TYPE - means that this scoped type is a subtype of TYPE.
  :super TYPE - means that this scoped type is a supertype of TYPE.")

(defmacro et-with-scoped-datatype (entry &rest body)
  `(condition-case _err (et--datatype-arg-roles (car entry))
     (:success (error "Cannot shadow an existing type: %s" (car entry)))
     (error (let ((et-scoped-datatypes (cons ,entry et-scoped-datatypes)))
              ,@body))))


(defun et--datatype-map-args (dt-name dt-args func)
  "Apply FUNC to each argument, returning the resulting list.

FUNC is called with two arguments, ARG and ROLE, where role is one of
`CONST', `CO', `CONTRA', or `ISO'."
  (cl-loop for arg in dt-args
           for role in (et--datatype-arg-roles dt-name dt-args)
           collect (funcall func arg role)))

(defun et--datatype-map-type-args (dt-name dt-args func)
  "Like `et--datatype-map-args', but the identify for CONST args.

FUNC is called with one argument, the current argument"
  (et--datatype-map-args
   dt-name dt-args
   (lambda (arg role) (if (eq role 'CONST) arg (funcall func arg)))))

(defun et--datatype-arg-roles (dt-name dt-args)
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'."
  (pcase (cons dt-name dt-args)
    (`(,(guard (alist-get dt-name et-scoped-datatypes))) nil)
    (`(:literal ,_arg) `(CONST))
    (`(:cons ,_car ,_cdr) `(ISO ISO))
    (`(:cons:ro ,_car ,_cdr) `(CO CO))
    (`(:cons:wo ,_car ,_cdr) `(CONTRA CONTRA))
    (`(:vector ,_elem) `(ISO))
    (`(,(or :integer :number :string :symbol :any)) nil)
    (_ (error "Invalid datatype: %s %s" dt-name dt-args))))

(defun et-datatype-subtype? (sub super)
  (pcase (list (cons (et-datatype-name sub) (et-datatype-args sub))
               (cons (et-datatype-name super) (et-datatype-args super)))
    (`(,a ,a) t)
    (`(,_ (:any)) t)
    (`((:integer) (:number)) t)
    (`((:vector ,sub-e) (:vector ,super-e)) (et-subtype? sub-e super-e))
    (`((:cons ,l1 ,r1) (:vector ,l2 ,r2))
     (and (et-subtype? l1 l2) (et-subtype? r1 r2)))

    (`((:literal ,val) (:integer)) (integerp val))
    (`((:literal ,val) (:number)) (numberp val))
    (`((:literal ,val) (:string)) (stringp val))
    (`((:literal ,val) (:cons ,l ,r))
     (and (consp val)
          (et-subtype? (et-dt :literal (car val)) l)
          (et-subtype? (et-dt :literal (cdr val)) r)))
    (`((:literal ,val) (:vector ,elem))
     (and (vectorp val)
          (cl-loop for e across val
                   always (et-subtype? (et-dt :literal e) elem))))))


;;;; Aliases

(cl-defstruct et-alias "A type alias factor of an `et-type'." name args)

(defvar et-aliases nil
  "An alist where each entry is (NAME-KEYWORD TYPE-FN PROPS...).

TYPE-FN is a function which takes the alias arguments and returns the
type to use in place of that alias.")

(defmacro et-defalias (keyword arglist &rest body)
  "Alias KEYWORD types to return a specific type."
  (declare (indent 2))
  (cl-assert (keywordp keyword))
  (cl-assert (string-match-p "^:[A-Z]" (symbol-name keyword)))
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
  (when (plist-get (cdr (alist-get keyword et-aliases)) :read-only)
    (error "Alias %s is already defined, and is read-only" keyword))
  (setf (alist-get keyword et-aliases) (cons function props)))

(defun et-alias-expand (alias)
  "Return the type associated with an alias."
  (let ((type-fn (or (alist-get (et-alias-name alias) et-aliases)
                     (error "Alias %s is not defined" (et-alias-name alias)))))
    (et--verify-type (apply type-fn (et-alias-args alias)))))

(defun et-expand-all-aliases (type)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           nconc (if (et-alias-p val)
                     (et-expand-all-aliases (et-alias-expand val))
                   (list case))
           into new-cases
           finally return (make-et-type :cases new-cases)))


;;;; Signals

(cl-defstruct et-signal
  "A type representing that something threw a signal."
  symbol)


;;;; Built-in aliases

(et-defalias :Boolean ()
  (et-or (et-literal t) (et-nil)))

(et-defalias :List (elem)
  (et-or (et-nil)
         (et-dt :cons elem (et-alias :List elem))))

(et-defalias :Tree (elem)
  (et-or elem
         (et-dt :List (et-alias :Tree elem))))


;;;; Binds

(cl-defstruct et-bind "A variable bind factor of an `et-type'." name args)


;;;; Type struct

(cl-defstruct et-type-case
  "Struct representing a case of an `et-type'.

BINDS is a list of `et-bind's.

VALUE is an instance of either `et-datatype', `et-alias', or
`et-signal'."
  value binds)

(cl-defstruct et-type
  "Struct representing a root-level et type.

  CASES is a list of `et-type-case' instances being unioned."
  cases)

(cl-defmethod cl-print-object ((type et-type) stream)
  (princ (et-pp type) stream))

(defun et--verify-type (type)
  "Check that a matcher is valid."
  (dolist (case (et-type-cases type))
    (let ((val (et-type-case-value case)))
      (if (et-datatype-p val)
          ;; Check that all of the arguments have the correct role
          (let ((roles (et--datatype-arg-roles (et-datatype-name val) (et-datatype-args val))))
            (or (eq (length (et-datatype-args val)) (length roles))
                (error "Wrong number of arguments for datatype %s" (et-datatype-name val)))
            (cl-loop for role in roles
                     for arg in (et-datatype-args val)
                     do (pcase role
                          ('CONST nil)
                          ((or 'CO 'CONTRA 'ISO) (et--verify-type arg))
                          (_ (error "Unknown role type: %s" role)))))
        ;; Otherwise, it must be an alias or signal
        (or (et-alias-p val)
            (et-signal-p val)
            (error "Expected datatype, alias, or signal. Found %s" val))))

    (dolist (b (et-type-case-binds case))
      (or (et-bind-p b) (error "Expected bind, found %s" b))))
  type)

(advice-add #'make-et-type-case :filter-return #'et--verify-type)


;;; Utils
;;;; Check subtype

(defun et-subtype? (super sub)
  (et--verify-type super)
  (et--verify-type sub)

  (setq sub (et-expand-all-aliases sub))
  (setq super (et-expand-all-aliases super))

  (cl-loop for sub-case in (et-type-cases sub)
           for sub-val = (et-type-case-value sub-case)
           unless (et-datatype-p sub-val) do (error "Invalid case type")
           always
           (cl-loop for super-case in (et-type-cases super)
                    for super-val = (et-type-case-value super-case)
                    unless (et-datatype-p super-val) do (error "Invalid case type")
                    thereis
                    (et-datatype-subtype? sub-val super-val))))


;;;; Simplify

(defun et--datatype-disjoint? (_a _b)
  nil)

(defun et-simplify-type (type)
  (et--verify-type type)

  (cl-loop for (case . rest) on (et-type-cases type)
           ;; Check if this case is redundant
           unless (cl-loop for c in (append new-cases rest)
                           ;; case is a subtype of c, so case is redundant
                           thereis (et-subtype? (make-et-type :cases (list case))
                                                (make-et-type :cases (list c))))
           collect case into new-cases
           finally return (make-et-type :cases new-cases)))


;;;; And/Or

(defun et-or (&rest types)
  (et-simplify-type (apply #'et--or types)))

(defun et-and (&rest types)
  (et-simplify-type (apply #'et--and types)))

(defun et--or (&rest types)
  (mapc #'et--verify-type types)

  (cl-loop for type in types
           append (et-type-cases type) into cases
           finally return (make-et-type :cases cases)))

(defun et-never-p (type)
  (et--verify-type type)
  (null (et-type-cases type)))

(defun et--and (&rest types)
  "Return the type intersection of TYPES."
  (mapc #'et--verify-type types)

  (pcase types
    ('nil (et-any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et--and a (apply #'et--and b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in (et-type-cases a)
              nconc
              (cl-loop for b-case in (et-type-cases b)
                       nconc (et--intersect-cases a-case b-case))
              into all-cases
              finally return (et-simplify-type (make-et-type :cases all-cases))))))

(defun et--intersect-cases (a b)
  "Return a list of type cases resulting from intersecting cases A and B."

  ;; TODO: binds

  (setq a (et-type-case-value a))
  (setq b (et-type-case-value b))
  (cond
   ((equal a b) (list a))
   ((et-alias-p a) (cl-loop for a-case in (et-alias-expand a)
                            nconc (et--intersect-cases a-case b)))
   ((et-alias-p b) (cl-loop for b-case in (et-alias-expand b)
                            nconc (et--intersect-cases a b-case)))
   ((and (et-datatype-p a) (et-datatype-p b))
    (let ((dt (et--intersect-datatypes a b)))
      (if (eq dt 'NEVER) nil
        (list (make-et-type-case :value dt)))))

   (t (error "Signals not yet supported"))))

(defun et--intersect-datatypes (a b)
  "Returns the datatype resulting from intersecting A and B, or `NEVER'."
  (cl-assert (et-datatype-p a))
  (cl-assert (et-datatype-p b))

  (let ((a-name (et-datatype-name a))
        (b-name (et-datatype-name a))
        (a-args (et-datatype-args a))
        (b-args (et-datatype-args b)))

    (pcase (list a-name b-name)
      ((guard (et-datatype-subtype? a b)) b)
      ((guard (et-datatype-subtype? b a)) a)

      ((guard (not (eq a-name b-name))) 'NEVER)
      ((guard (not (eq (length a-args) (length b-args)))) 'NEVER)

      (_
       ;; This is currently the strategy for all datatypes.
       ;; Later, there might be different strategies
       (cl-loop for a-arg in a-args
                for b-arg in b-args
                for arg-intersection = (et-and a-arg b-arg)
                when (et-never-p arg-intersection) return :never
                collect arg-intersection into new-args
                finally return (make-et-datatype :name a-name :args new-args))))))


;;; Match
;;;; Matcher struct

(cl-defstruct et-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the possible match factors in disjunctive-nominal form. It is a
list of cases, each of which is a list of match factors.

Each match factor is one of:
  (m:datatype DT-NAME ARG-MATCHER-DNFS...)
  (m:match VAR)
  (m:set VAR `et-type')"
  generics dnf)

(cl-defmethod cl-print-object ((matcher et-matcher) stream)
  (princ (et-pp-matcher matcher) stream))

(defun et--verify-matcher (matcher)
  "Check that a matcher is valid."
  (let ((generics (et-matcher-generics matcher)))
    (dolist (generic generics)
      (or (symbolp generic) (error "Generics must be a list of symbols")))

    (cl-flet ((genericp (var) (or (and (symbolp var) (memq var generics))
                                  (error "Not a generic: %s" var))))
      (dolist (case (et-matcher-dnf matcher))
        (dolist (factor case)
          (pcase factor
            (`(m:datatype ,(and name (pred keywordp)) . ,args)
             (let ((roles (et--datatype-arg-roles name args)))
               ;; Make sure that there are the correct number of arguments
               (or (eq (length roles) (length args))
                   (error "Wrong number of arguments for datatype %s" name))
               ;; Make sure that all arguments have the correct role
               (cl-loop for role in roles for arg in args
                        do (pcase role
                             ('CONST nil)
                             ((or 'CO 'CONTRA 'ISO) (make-et-matcher :generics generics :dnf arg))
                             (_ (error "Unknown role type: %s" role))))))

            (`(m:match ,(pred genericp)))
            (`(m:set ,(pred genericp) ,(pred et-type-p)))
            (_ (error "Invalid match factor: %s" factor)))))
      matcher)))

(advice-add #'make-et-matcher :filter-return #'et--verify-matcher)


;;;; Type to matcher

(defun et-type-to-matcher (type &optional generics)
  (et--verify-type type)

  (cl-loop
   for case in (et-type-cases type)
   for value = (et-type-case-value case)
   nconc
   (cl-typecase value
     (et-datatype
      `(((m:datatype
          ,(et-datatype-name value)
          ,(et--datatype-map-type-args (et-datatype-name value) (et-datatype-args value)
                                       (lambda (arg) (et-type-to-matcher arg generics)))))))

     (et-alias (et-matcher-dnf (et-type-to-matcher (et-alias-expand value) generics)))
     (t (error "Unsupported type case to convert to matcher: %s" value)))
   into dnf
   finally return (make-et-matcher :dnf dnf :generics generics)))

(defun et--matcher-to-type (matcher)
  (cl-loop for case in (et-matcher-dnf matcher)
           collect
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(m:datatype ,name ,args)
                       (et-type (et--datatype-map-type-args name args #'et--matcher-to-type)))
                      (_ (error "Invalid matcher to convert to type")))
                    into new-factors
                    finally return (apply #'et--and new-factors))
           into new-cases
           finally return (apply #'et--or new-cases)))


;;;; Iso match

(defun et-iso-match (matcher type)
  (delete-dups (nconc (et--sub-constraints matcher type)
                      (et--super-constraints matcher type))))


;;;; Sub match

(defun et--sub-constraints (matcher type)
  (et--verify-matcher matcher)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           nconc (et--sub-constraints-2 matcher case) into result
           finally return (if (member `(q:never) result) `((q:never)) result)))

(defun et--sub-constraints-2 (matcher case)
  (et--verify-matcher matcher)
  (cl-assert (et-type-case-p case))

  (cl-loop for match-case in (et-matcher-dnf matcher)
           for result =
           (cl-loop for match-factor in match-case
                    for gens = (et-matcher-generics matcher)
                    nconc (et--sub-or-super-constraints-3 match-factor case gens))
           unless (member '(q:never) result)
           return result
           ;; If all cases failed, fallback to 2.2 or 2.3
           finally return
           (let ((val (et-type-case-value case)))
             (if (et-alias-p val)
                 (et--sub-constraints matcher (et-alias-expand val))
               (list '(q:never))))))

(defun et--sub-or-super-constraints-3 (match-factor case generics &optional is-super)
  (et--verify-matcher (make-et-matcher :generics generics :dnf (list (list match-factor))))
  (et--verify-type (make-et-type :cases (list case)))

  (pcase match-factor
    (`(m:match ,var) `((,(if is-super 'q:leq 'q:geq) ,var ,(et-type case))))
    (`(m:set ,var ,type) `((,(if is-super 'q:leq 'q:geq) ,var ,type)))
    (`(m:datatype ,mdt-name . ,mdt-args)
     (pcase (et-type-case-value case)
       ((pred et-alias-p) `((q:never)))
       ((and dt (pred et-datatype-p))
        (et--sub-or-super-constraints-4 mdt-name mdt-args dt generics is-super))
       (_ (error "Unsupported matching datatype"))))
    (_ (error "Invalid match factor"))))

(defun et--sub-or-super-constraints-4 (mdt-name mdt-args dt generics &optional is-super)
  (cl-assert (keywordp mdt-name))
  (cl-assert (listp mdt-args))
  (cl-assert (et-datatype-p dt))
  (cl-assert (listp generics))

  (if (not (eq mdt-name (et-datatype-name dt)))
      ;; Different datatypes cannot match
      `((q:never))

    ;; Datatypes of the same type should have the same number of arguments
    (cl-assert (eq (length mdt-args) (length (et-datatype-args dt))))
    (cl-loop for m-arg in mdt-args
             for t-arg in (et-datatype-args dt)
             for role in (et--datatype-arg-roles mdt-name mdt-args)
             for matcher = (make-et-matcher :generics generics :dnf m-arg)
             nconc (pcase role
                     ;; Const args must be equal to match
                     ('CONST (if (equal m-arg t-arg) nil `((q:never))))
                     ((or 'CO 'CONTRA)
                      (if (xor (eq role 'CO) is-super)
                          (et--sub-constraints matcher t-arg)
                        (et--super-constraints matcher t-arg)))
                     ('ISO (et-iso-match matcher t-arg))
                     (_ (error "Unknown argument role: %s" role))))))


;;;; Super match

(defun et--super-constraints (matcher type)
  (et--verify-matcher matcher)
  (et--verify-type type)

  (cl-loop for m-case in (et-matcher-dnf matcher)
           nconc (et--super-constraints-2 m-case type (et-matcher-generics matcher))
           into result
           finally return (if (member `(q:never) result) `((q:never)) result)))

(defun et--super-constraints-2 (match-case type generics)
  (make-et-matcher :dnf (list match-case) :generics generics)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for result =
           (cl-loop for match-factor in match-case
                    nconc (et--sub-or-super-constraints-3 match-factor case generics 'SUPER))
           unless (member '(q:never) result)
           return result
           ;; If all cases failed, return never
           finally return (list '(q:never))))


;;;; Satisfy constraints

(defun et--match-satisfy-constraints-biggest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns the symbol NEVER if invalid."
  (cl-loop
   for gen in generics
   for gen-result =
   (let ((guess
          (cl-loop for (fact g type) in constraints
                   when (and (eq g gen) (memq fact '(q:eq q:leq)))
                   collect type into types
                   finally return (apply #'et--and types))))
     (if (cl-loop for (fact g type) in constraints
                  always
                  (or (not (eq g gen))
                      (not (memq fact '(q:eq q:geq)))
                      (et-subtype? guess type)))
         guess (et-type)))
   when (equal gen-result (et-type))
   do (cl-return 'NEVER)
   collect gen-result))

(defun et--match-satisfy-constraints-smallest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns the symbol NEVER if invalid.

However, unlike `et--match-satisfy-constraints-biggest', this allows
values to be the never type."
  (cl-loop
   for gen in generics
   for gen-result =
   (let ((guess
          (cl-loop for (fact g type) in constraints
                   when (and (eq g gen) (memq fact '(q:eq q:geq)))
                   collect type into types
                   finally return (apply #'et-or types))))
     (if (cl-loop for (fact g type) in constraints
                  always
                  (or (not (eq g gen))
                      (not (memq fact '(q:eq q:leq)))
                      (et-subtype? type guess)))
         guess 'NEVER))
   when (equal gen-result 'NEVER)
   do (cl-return 'NEVER)
   collect gen-result))


;;; Helpers
;;;; Constructors

(defun et-type (&rest cases)
  "Construct a new `et-type' out of CASES.

Each of CASES should be an instance of `et-type-case', or alternatively
a valid `et-type-case-value'."
  (cl-loop for c in cases
           collect (if (et-type-case-p c) c
                     ;; Checking is done inside of `make-et-type'
                     (make-et-type-case :value c))
           into cases
           finally return (make-et-type :cases cases)))

(defun et-dt (name &rest args)
  (cl-assert (keywordp name))
  (cl-assert (string-match-p "^:[a-z]" (symbol-name name)))
  (et-type (make-et-datatype :name name :args args)))

(defun et-literal (value) (et-dt :literal value))
(defun et-nil () (et-literal nil))
(defun et-t () (et-literal t))
(defun et-any () (et-dt :any))
(defun et-never () (make-et-type :cases nil))

(defun et-alias (name &rest args)
  (cl-assert (keywordp name))
  (cl-assert (string-match-p "^:[A-Z]" (symbol-name name)))
  (et-type (make-et-alias :name name :args args)))


;;;; Parsing structure

(defun et--dnf-and (a b)
  (cl-loop for a-case in a
           nconc
           (cl-loop for b-case in b
                    collect (append a-case b-case))))

(defun et--parse-structure (spec generics)
  "Returns a list of lists of FACTOR.

Each FACTOR is one of:
  \(DT NAME ARGS...)
  \(ALIAS NAME ARGS...)
  \(GENERIC VAR)
  \(SET VAR DNF)

The list of lists is in dnf form."
  (let ((case-fold-search nil)
        (parse (lambda (arg) (et--parse-structure arg generics))))

    (pcase spec
      ((pred stringp) (et--parse-string spec generics))
      ((pred keywordp) (et--parse-string (substring (symbol-name spec) 1) generics))

      (`(:or . ,args) (mapcan parse args))
      (`(:and . ,args)
       (or args (error "`and' cannot be empty"))
       (cl-reduce #'et--dnf-and (mapcar parse args)))

      (`(:never) nil)
      (`(:any) `(((DT :any))))
      (`(:nil) `(((DT :literal nil))))
      (`(:t) `(((DT :literal t))))

      (`(:sym ,val)
       (when (stringp val) (setq val (intern val)))
       (or (symbolp val) (error "Not a symbol: %s" val))
       `(((DT :literal ,val))))

      (`(:num ,val)
       (and (stringp val) (string-match-p "^[0-9][0-9_]*\\.?[0-9_]*$" val)
            (setq val (string-to-number val)))
       (or (numberp val) (error "Not a number: %s" val))
       `(((DT :literal ,val))))

      (`(:str ,str)
       (or (stringp str) (error "Not a string: %s" str))
       `(((DT :literal ,str))))

      (`(:set ,var ,type)
       (or (memq var generics) (error "Not a generic: %s" var))
       `(((SET ,var ,(funcall parse type)))))

      (`(,(and name (pred keywordp)) . ,args)
       (cond
        ((memq name generics)
         (or (null args) (error "Generic type cannot have arguments"))
         `(((GENERIC ,name))))

        ((string-match-p "^:[A-Z]" (symbol-name name))
         `(((ALIAS ,name ,@args))))

        (t `(((DT ,name ,@args))))))

      (_ (error "Invalid type spec: %s" spec)))))

(defun et--parse-string (s generics)
  (when (string-empty-p s) (error "Empty type expression"))

  (cl-loop for or-seg in (et--split-at-depth s ?|)
           when (string-empty-p or-seg)
           do (error "Empty segment in union type: %s" s)
           collect
           (cl-loop for and-seg in (et--split-at-depth or-seg ?&)
                    when (string-empty-p and-seg)
                    do (error "Empty segment in intersection type: %s" s)
                    collect (et--parse-atom and-seg generics) into and-parts
                    finally return (cl-reduce #'et--dnf-and and-parts))
           into or-parts
           finally return (apply #'nconc or-parts)))

(defun et--parse-atom (s generics)
  "Parse a single type atom into an `et-type'."
  (cond
   ((string-match "{\\(.*\\)}" s) (et--parse-string (substring s 1 -1) generics))

   ;; Var=Type
   ((string-match "^\\([a-zA-Z0-9]*\\)=\\(.*\\)$" s)
    (let ((var (intern (concat ":" (match-string 1 s))))
          (expr (match-string 2 s)))
      (et--parse-structure (list :set var expr) generics)))

   ;; Name or Name<...>
   ((string-match "^\\([A-Za-z][a-zA-Z0-9]*\\)\\(?:<\\(.*\\)>\\)?$" s)
    (let* ((kwd (intern (concat ":" (match-string 1 s))))
           (inner (match-string 2 s))
           (arg-strs (when inner (et--split-at-depth inner ?~))))
      (et--parse-structure (cons kwd arg-strs) generics)))

   (t (error "Invalid parse syntax: %s" s))))


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


;;;; Parsing types

(defun et--type-dnf-to-type (dnf)
  "Convert structured DNF (output of `et--parse-structure') to an `et-type'."
  (cl-loop for case in dnf
           when (cdr case) do (error "Type cannot represent AND: %s" case)
           for value =
           (pcase (car case)
             (`(DT ,name . ,args)
              (make-et-datatype
               :name name
               :args (et--datatype-map-type-args name args #'et-parse-type)))
             (`(ALIAS ,name . ,args)
              (make-et-alias :name name :args (mapcar #'et-parse-type args)))
             (f (error "Invalid type factor: %s" f)))
           collect (make-et-type-case :value value) into cases
           finally return (make-et-type :cases cases)))

(defun et-parse-type (spec)
  "Parse SPEC as an `et-type'."
  (et--type-dnf-to-type (et--parse-structure spec nil)))

(defmacro et (&rest args)
  `(et-parse-type ,(if (eq (length args) 1) (car args) (cons #'list args))))


;;;; Parsing matchers

(defun et-parse-matcher (spec generics)
  "Parse SPEC as an `et-matcher' with GENERICS."
  (cl-loop for case in (et--parse-structure spec generics)
           collect
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(DT ,name . ,args)
                       `(m:datatype
                         ,name
                         ,(et--datatype-map-type-args
                           name args
                           (lambda (arg) (et-matcher-dnf (et-parse-matcher arg generics))))))
                      (`(GENERIC ,var) `(m:match ,var))
                      (`(SET ,var ,inner-dnf)
                       `(m:set ,var ,(et--type-dnf-to-type inner-dnf)))
                      (`(ALIAS . ,_) (error "Matchers cannot contain aliases: %s" factor))
                      (_ (error "Invalid match factor: %s" factor))))
           into dnf
           finally return (make-et-matcher :dnf dnf :generics generics)))


;;;; Printing

(defun et-pp (type)
  (et-pp-matcher (et-type-to-matcher type)))

(defun et-pp-matcher (matcher)
  "Format an `et-matcher' into a human-readable string."
  (let* ((generics (et-matcher-generics matcher))
         (body (et--format-matcher-dnf (et-matcher-dnf matcher))))
    (if generics
        (format "<%s> => %s" (mapconcat #'symbol-name generics ", ") body)
      body)))

(defun et--format-matcher-dnf (dnf)
  (if (null dnf) "never"
    (mapconcat #'et--format-matcher-case dnf " | ")))

(defun et--format-matcher-case (case)
  (if (null case) "any"
    (mapconcat #'et--format-matcher-factor case " & ")))

(defun et--format-matcher-factor (factor)
  (pcase factor
    (`(m:match ,var) (format "?%s" var))
    (`(m:set ,var ,type) (format "%s=%s" var (et-pp type)))
    (`(m:datatype ,name . ,args) (et--format-matcher-dt name args))
    (_ (error "Invalid match factor: %s" factor))))

(defun et--format-matcher-dt (name args)
  (pcase (cons name args)
    (`(:literal ,val)
     (if (and (symbolp val) val (not (eq val t)))
         (format "`%s'" val)
       (prin1-to-string val)))

    (`(:cons ,left-dnf ,right-dnf)
     (let ((elems (list (et--format-matcher-dnf left-dnf))))
       (while (pcase right-dnf
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     (`(m:datatype :cons ,car-dnf ,cdr-dnf)
                      (nconc elems (list (et--format-matcher-dnf car-dnf)))
                      (setq right-dnf cdr-dnf)
                      t))))))
       (let ((tail-nil-p
              (and (= (length right-dnf) 1)
                   (= (length (car right-dnf)) 1)
                   (equal (car (car right-dnf)) '(m:datatype :literal nil)))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et--format-matcher-dnf right-dnf))))))

    (_
     (let* ((name-str (substring (symbol-name name) 1))
            (roles (et--datatype-arg-roles name args))
            (strs (cl-loop for arg in args
                           for role in roles
                           collect (if (eq role 'CONST)
                                       (format "%s" arg)
                                     (et--format-matcher-dnf arg)))))
       (if strs
           (format "%s<%s>" name-str (string-join strs ", "))
         name-str)))))


;;; Test

(et--sub-constraints
 (make-et-matcher
  :generics '(a b)
  :dnf `(( (m:datatype :cons:ro (( (m:datatype :string) )) (( (m:datatype :number) )))
           (m:match a) )
         ( (m:datatype :literal nil)
           (m:set a ,(et-literal nil)) )
         ))
 (et-or (et-dt :cons:ro (et-dt :string) (et-dt :number)) (et-dt :literal nil))
 )

(et-parse-matcher :sym<abc> nil)


(defun et-subtype? (a b) t)
(et--match-satisfy-constraints-smallest '(a b) $29)
