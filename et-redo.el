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


(defun et--datatype-arg-roles (dt-name)
  "Returns a list of `const' | `sub' | `super'."
  (pcase dt-name
    ((guard (alist-get dt-name et-scoped-datatypes)) nil)
    (:literal `(const))
    (:cons `(sub sub))
    (:vector `(sub))
    ((or :integer :number :string :symbol :any) nil)
    (_ (error "Unknown datatype: %s" dt-name))))

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


;;;; Signals

(cl-defstruct et-signal
  "A type representing that something threw a signal."
  symbol)


;;;; Built-in aliases

(et-defalias :Boolean ()
  (et-or (et-literal t) (et-nil)))

(et-defalias :List (elem)
  (et-or (et-nil)
         (et-dt :cons elem (et-dt :alias :List elem))))

(et-defalias :Tree (elem)
  (et-or elem
         (et-dt :List (et-dt :alias :Tree elem))))


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

(defun et--verify-type (type)
  "Check that a matcher is valid."
  (dolist (case (et-type-cases type))
    (let ((val (et-type-case-value case)))
      (if (et-datatype-p val)
          ;; Check that all of the arguments have the correct role
          (let ((roles (et--datatype-arg-roles (et-datatype-name val))))
            (or (eq (length (et-datatype-args val)) (length roles))
                (error "Wrong number of arguments for datatype %s" (et-datatype-name val)))
            (cl-loop for role in roles
                     for arg in (et-datatype-args val)
                     do (pcase role
                          ('const nil)
                          ((or 'super 'sub) (et--verify-type arg))
                          (_ (error "Unknown role type: %s" role)))))
        ;; Otherwise, it must be an alias or signal
        (or (et-alias-p val)
            (et-signal-p val)
            (error "Expected datatype, alias, or signal. Found %s" val))))

    (dolist (b (et-type-case-binds case))
      (or (et-bind-p b) (error "Expected bind, found %s" b))))
  type)

(advice-add #'make-et-type-case :filter-return #'et--verify-type)


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

(defun et-alias (name &rest args)
  (cl-assert (keywordp name))
  (cl-assert (string-match-p "^:[A-Z]" (symbol-name name)))
  (et-type (make-et-alias :name name :args args)))


;;; Match
;;;; And/Or

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
    ('nil (et-dt :any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et--and a (apply #'et--and b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in (et-type-cases a)
              nconc
              (cl-loop for b-case in (et-type-cases b)
                       nconc (et--intersect-cases a-case b-case))
              into all-cases
              finally return (delete-dups all-cases)))))

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
      (`(:any ,_) b)
      (`(,_ :any) a)
      (`(:number :integer) b)
      (`(:integer :number) a)

      ((guard (not (eq a-name b-name))) 'NEVER)
      ((guard (not (eq (length a-args) (length b-args)))) 'NEVER)

      (_
       ;; This is currently the strategy for all datatypes.
       ;; Later, there might be different strategies
       (cl-loop for a-arg in a-args
                for b-arg in b-args
                for arg-intersection = (et--and a-arg b-arg)
                when (et-never-p arg-intersection) return :never
                collect arg-intersection into new-args
                finally return (make-et-datatype :name a-name :args new-args))))))


;;;; Matcher struct

(cl-defstruct et-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the possible match factors in disjunctive-nominal form. It is a
list of cases, each of which is a list of match factors.

Each match factor is one of:
  (m:datatype DT-NAME ARG-MATCHER-DNFS...)
  (m:eq VAR)
  (m:leq VAR)
  (m:geq VAR)
  (m:set-eq/leq/geq VAR `et-type')"
  generics dnf)

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
             (let ((roles (et--datatype-arg-roles name)))
               ;; Make sure that there are the correct number of arguments
               (or (eq (length roles) (length args))
                   (error "Wrong number of arguments for datatype %s" name))
               ;; Make sure that all arguments have the correct role
               (cl-loop for role in roles for arg in args
                        do (pcase role
                             ('const nil)
                             ((or 'sub 'super) (make-et-matcher :generics generics :dnf arg))
                             (_ (error "Unknown role type: %s" role))))))

            (`(m:eq ,(pred genericp)))
            (`(m:leq ,(pred genericp)))
            (`(m:geq ,(pred genericp)))
            (`(,(or 'm:set-eq 'm:set-leq 'm:set-geq) ,(pred genericp) ,(pred et-type-p)))
            (_ (error "Invalid match factor: %s" factor)))))
      matcher)))

(advice-add #'make-et-matcher :filter-return #'et--verify-matcher)


;;;; Type to matcher

(defun et-type-to-matcher (type)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for value = (et-type-case-value case)
           nconc
           (cl-typecase value
             (et-datatype )
             (et-alias (et-matcher-dnf (et-type-to-matcher (et-alias-expand value)))))
           into dnf
           )
  )


;;;; Constraints

(defun et-match-constraints (matcher type)
  (et--verify-matcher matcher)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           nconc (et--match-constraints-2 matcher case) into result
           finally return (if (member `(q:never) result) `((q:never)) result)))

(defun et--match-constraints-2 (matcher case)
  (et--verify-matcher matcher)
  (cl-assert (et-type-case-p case))

  (cl-loop for match-case in (et-matcher-dnf matcher)
           for result =
           (cl-loop for match-factor in match-case
                    nconc (et--match-constraints-3 match-factor case
                                                   (et-matcher-generics matcher)))
           unless (member '(q:never) result)
           return result
           ;; If all cases failed, fallback to 2.2 or 2.3
           finally return
           (let ((val (et-type-case-value case)))
             (if (et-alias-p val)
                 (et-match-constraints matcher (et-alias-expand val))
               (list '(q:never))))))

(defun et--match-constraints-3 (match-factor case generics)
  (et--verify-matcher (make-et-matcher :generics generics :dnf (list (list match-factor))))
  (et--verify-type (make-et-type :cases (list case)))

  (pcase match-factor
    (`(m:eq ,var) `((q:eq ,var ,(et-type case))))
    (`(m:leq ,var) `((q:leq ,var ,(et-type case))))
    (`(m:geq ,var) `((q:geq ,var ,(et-type case))))
    (`(m:set-eq ,var ,type) `((q:eq ,var ,type)))
    (`(m:set-leq ,var ,type) `((q:leq ,var ,type)))
    (`(m:set-geq ,var ,type) `((q:geq ,var ,type)))
    (`(m:datatype ,mdt-name . ,mdt-args)
     (let ((val (et-type-case-value case)))
       (cl-typecase val
         (et-alias `((q:never))) ;; Fall back to 2.2
         (et-datatype
          ;; Match the datatypes together
          (if (not (eq mdt-name (et-datatype-name val)))
              ;; Different datatype cannot match
              `((q:never))
            ;; Datatypes of the same type should have the same number of arguments
            (cl-assert (eq (length mdt-args) (length (et-datatype-args val))))
            (cl-loop for m-arg in mdt-args
                     for t-arg in (et-datatype-args val)
                     for role in (et--datatype-arg-roles mdt-name)
                     for matcher = (make-et-matcher :generics generics :dnf m-arg)
                     nconc (pcase role
                             ;; Const args must be equal to match
                             ('const (if (equal m-arg t-arg) nil `((q:never))))
                             ((or 'sub 'super) (et-match-constraints matcher t-arg))
                             (_ (error "Unknown argument role: %s" role))))))
         (t (error "Signals not yet supported")))))
    (_ (error "Invalid match factor"))))


;;;; Satisfy constraints

(defun et--match-satisfy-constraints (generics constraints)
  (cl-loop
   for gen in generics
   for gen-result =
   (let ((gle (cl-loop for (fact g type) in constraints
                       when (and (eq g gen) (memq fact '(q:eq q:leq)))
                       collect type into types
                       finally return (apply #'et--and types))))
     (if (cl-loop for (fact g type) in constraints
                  always
                  (or (not (eq g gen))
                      (not (memq fact '(q:eq q:geq)))
                      (et-subtype? type gle)))
         gle (et-type)))
   when (equal gen-result (et-type))
   do (cl-return 'NEVER)
   collect gen-result))


;;;; Test

(et-match-constraints
 (make-et-matcher
  :generics '(a b)
  :dnf `(( (m:datatype :cons (( (m:datatype :string) )) (( (m:datatype :number) )))
           (m:geq a) )
         ( (m:datatype :literal nil)
           (m:set-geq a ,(et-dt :literal nil)) )
         ))
 (et--or (et-dt :cons (et-dt :string) (et-dt :number)) (et-dt :literal nil))
 )

(defun et-subtype? (&rest _) nil)

(et--match-satisfy-constraints
 '(a) $60
 )

(et-alias :List (et-dt :number))
