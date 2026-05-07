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


(defvar et-debug nil
  "Perform extra debug checks.")


;;; ============================================================
;;; Helpers
;;;; Quote macro

(eval-and-compile
  (defun et--copy-quotes (expr)
    (cond ((eq (car-safe expr) #'quote) (list #'copy-tree expr))
          ((consp expr) (cons (et--copy-quotes (car expr)) (et--copy-quotes (cdr expr))))
          (t expr))))

(defmacro et-q (expr)
  "Like `backquote', but return copies of all list literals.

This avoids the undefined behavior caused by mutating list literals by
applying `copy-tree' to all list ltierals before they are returned. This
could be improved in the future by replacing all list literals with
instances of `list' and `cons', but this is not currently a high
priority."
  (et--copy-quotes (cdr (backquote-process expr))))

(defmacro et-ql (&rest exprs)
  (et--copy-quotes (cdr (backquote-process exprs))))


;;;; Dnf And

(defun et--dnf-and (&rest args)
  (pcase args
    ('() (list (list)))
    (`(,a) a)
    (`(,a ,b ,c . ,rest) (et--dnf-and a (apply #'et--dnf-and b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in a
              nconc
              (cl-loop for b-case in b
                       collect (append a-case b-case))))))


;;; ============================================================
;;; Types
;;;; Scoped datatypes

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


;;;; Datatypes

(cl-defstruct et-datatype
  "A datatype factor of an `et-type'."
  name args)

(defun et--datatype-name? (name)
  (not (not (memq name '(Any Literal Number Integer String Symbol Vector
                             Cons Cons:RR Cons:WW Cons:RW Cons:WR)))))

(defun et--datatype-parents (dt-name)
  "Return a list of datatype names which are parents of `DT-NAME'.

If B is a parent of A, then swapping the name A for B in any datatype
creates a strictly larger type.

For example, (Cons CAR CDR) <= (Cons:RR CAR CDR) no matter what CAR
and CDR are, so Cons:RR is a parent type of Cons."
  (pcase dt-name
    ('Cons (et-ql Cons:RR Cons:WW Cons:WR Cons:RW))
    ('Integer (et-ql Number))))

(defun et--datatype-arg-roles (dt-name dt-args)
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'.

The resulting list must be the exact length of DT-ARGS, and each element
corresponds to the role of each argument in `dt-args'. `CONST' indicates
an argument which is a literal Lisp value. `CO'/`CONTRA'/`ISO' indicate
that the argument is a type argument, and whether the type argument is
covariant, contravariant, or isovariant."

  (pcase (cons dt-name dt-args)
    (`(,(guard (alist-get dt-name et-scoped-datatypes))) nil)
    (`(Literal ,_arg) (et-ql CONST))
    (`(Cons ,_car ,_cdr) (et-ql ISO ISO))
    (`(Cons:RR ,_car ,_cdr) (et-ql CO CO))
    (`(Cons:WW ,_car ,_cdr) (et-ql CONTRA CONTRA))
    (`(Vector ,_elem) (et-ql ISO))
    (`(PList . ,args)
     (cl-loop for (prop _val) on args by #'cddr
              do (or (keywordp prop) (error "Expected keyword, found %s" prop))
              nconc (et-ql CONST CO)))
    (`(,(or Integer Number String Symbol Any)) nil)
    (_ (error "Invalid datatype: %s %s" dt-name dt-args))))

(defun et--datatype-intersect-args (name args1 args2 intersect union)
  "Return a list of arguments intersecting ARGS1 and ARGS2.

The goal of this funciton is to determine a list of arguments
INTERSECTION-ARGS such that (NAME INTERSECTION-ARGS) is a subtype of
both (NAME ARGS1) and (NAME ARGS2).

If no such list is found, then return the symbol `INVALID'.

This function assumes that neither datatype is already a subset of the
other, in which case the subset args would be a trivial solution to this
function. This is so that this function can focus on the non-trivial
cases where neither is a subset of the other.

INTERSECT and UNION are functions which each take 2 elements from
ARGS1/ARGS2 and return a new arg, either the intersection or union of
the two args respectively."

  (cond
   ;; Handling for normal datatypes, where arguments have a fixed order
   ((memq name '(Vector Cons Cons:RR Cons:WW Cons:WR Cons:RW))
    (cl-assert (eq (length args1) (length args2)))
    (cl-loop for role in (et--datatype-arg-roles name args1)
             for arg1 in args1
             for arg2 in args2
             for new-arg = (pcase role
                             ('CO (funcall intersect arg1 arg2))
                             ('CONTRA (funcall union arg1 arg2))
                             ('ISO (if (equal arg1 arg2) arg1 'INVALID))
                             (_ (error "Unexpected arg role: %s" role)))
             when (or (eq new-arg 'INVALID) (et-never-p new-arg)) return 'INVALID
             collect new-arg))

   ((eq name 'PList)
    (let ((all-props (cl-loop for (p) on (append args1 args2) by #'cddr collect p)))
      (cl-loop for prop in (delete-dups all-props)
               for val1 = (plist-get args1 prop)
               for val2 = (plist-get args2 prop)
               for intersection = (if val1 (if val2 (funcall intersect val1 val2) val1) val2)
               when (et-never-p intersection) return 'INVALID
               nconc (list prop intersection))))

   (t 'INVALID)))

(defun et--datatype-constraints (sub-name sub-args super-name super-args co contra iso co-literal)
  (cl-flet ((valid-if (valid) (if valid nil (et-ql (Q:NEVER)))))

    (pcase (list sub-name super-name)
      (`(,_ Any) nil)

      ('(Plist Plist)
       (cl-loop for (prop super-val) on super-args by #'cddr
                for sub-val = (plist-get sub-args prop)
                unless sub-val return (et-ql (Q:NEVER))
                nconc (funcall co sub-val super-val)))

      (`(Literal ,_)
       (let ((val (car sub-args)))
         (pcase super-name
           ('Literal (valid-if (eq val (car super-args))))
           ('Integer (valid-if (integerp val)))
           ('Number (valid-if (numberp val)))
           ('String (valid-if (stringp val)))
           ('Symbol (valid-if (symbolp val)))
           ((or 'Cons 'Cons:RR 'Cons:WW 'Cons:RW 'Cons:WR)
            (if (not (consp val)) (valid-if nil)
              (nconc (funcall co-literal (car val) (car super-args))
                     (funcall co-literal (cdr val) (cadr super-args)))))
           (_ (valid-if nil)))))

      ((guard (or (eq sub-name super-name)
                  (memq super-name (et--datatype-parents sub-name))))
       ;; Datatypes of the same type should have the same number of arguments
       (cl-assert (eq (length sub-args) (length super-args)))
       (cl-loop for sub-arg in sub-args
                for super-arg in super-args
                for role in (et--datatype-arg-roles super-name super-args)
                nconc (pcase role
                        ;; Const args must be equal to match
                        ('CONST (valid-if (equal sub-arg super-arg)))
                        ('CO (funcall co sub-arg super-arg))
                        ('CONTRA (funcall contra sub-arg super-arg))
                        ('ISO (funcall iso sub-arg super-arg))
                        (_ (error "Unknown argument role: %s" role)))))

      (_ (valid-if nil)))))


;;;; Datatype mappers

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
  (cl-loop for arg in dt-args
           for role in (et--datatype-arg-roles dt-name dt-args)
           if (eq role 'CONST) collect arg
           else collect (funcall func arg)))


;;;; Aliases

(cl-defstruct et-alias "A type alias factor of an `et-type'." name args)

(defvar et-aliases nil
  "An alist where each entry is (NAME-SYMBOL TYPE-FN PROPS...).

TYPE-FN is a function which takes the alias arguments and returns a
structure which can be parsed by `et-parse-type'.")

(defmacro et-defalias (symbol arglist &rest body)
  "Alias SYMBOL types to return a specific type."
  (declare (indent 2))
  (cl-assert (symbolp symbol))
  (cl-assert (string-match-p "^[A-Z]" (symbol-name symbol)))
  (cl-assert (listp arglist))

  (let ((plist nil))
    (while (keywordp (car body))
      (setq plist (nconc plist (list (pop body) (pop body)))))

    (cl-assert (eq (length body) 1))

    `(et--define-alias
      ',symbol
      ,`(lambda . ,(cl--transform-lambda
                    (list arglist (list #'et-q (car body)))
                    (format "et-alias%s" symbol)))
      (list ,@plist))))

(defun et--define-alias (symbol function props)
  (when (plist-get (cdr (alist-get symbol et-aliases)) :read-only)
    (error "Alias %s is already defined, and is read-only" symbol))
  (setf (alist-get symbol et-aliases) (cons function props)))

(defun et--alias-call (name args)
  "Call the alias expansion function for alias NAME with args ARGS."
  (let ((type-fn (or (car (alist-get name et-aliases)) (error "Alias %s is not defined" name))))
    (apply type-fn args)))

(defun et-alias-expand (alias)
  "Expand an alias to a type."
  (let ((s-args (cl-loop for type in (et-alias-args alias)
                         collect (et-q (:structure (((S:TYPE ,type))))))))
    (et-parse-type (et--alias-call (et-alias-name alias) s-args))))

(defun et-expand-all-aliases (type)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           nconc (if (et-alias-p val)
                     (apply #'list (et-type-cases (et-expand-all-aliases (et-alias-expand val))))
                   (list case))
           into new-cases
           finally return (make-et-type :cases new-cases)))


;;;; Type struct

(cl-defstruct et-var
  "A variable currently in scope."
  name type)

(cl-defstruct et-type-case
  "Struct representing a case of an `et-type'.

BINDS is a list of (`et-var' . `et-type').

TYPEOFS is a list of `et-var'.

VALUE is an instance of either `et-datatype' or `et-alias'."
  value binds typeofs)

(cl-defstruct et-type
  "Struct representing a root-level et type.

  CASES is a list of `et-type-case' instances being unioned."
  cases)

(defun et--verify-type (type)
  "Check that a matcher is valid."
  (unless (et-type-p type)
    (error "Not a type: %s" type))

  (when et-debug
    (dolist (case (et-type-cases type))
      (let ((val (et-type-case-value case)))
        (cond
         ((et-datatype-p val)
          ;; Check that all of the arguments have the correct role
          (et--datatype-map-type-args (et-datatype-name val) (et-datatype-args val) #'et--verify-type))
         ((et-alias-p val) (mapc #'et--verify-type (et-alias-args val)))
         (t (error "Expected datatype or alias, found %s" val))))

      (dolist (x (et-type-case-binds case))
        (or (and (consp x) (et-var-p (car x)) (et--verify-type (cdr x)))
            (error "Expected bind, found %s" x)))
      (dolist (x (et-type-case-typeofs case))
        (or (et-var-p x) (error "Expected typeof var, found %s" x)))))

  type)

(advice-add #'make-et-type :filter-return #'et--verify-type)


;;;; Constructors

(defun et-never-p (type)
  (et--verify-type type)
  (null (et-type-cases type)))

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
  (cl-assert (symbolp name))
  (cl-assert (string-match-p "^[A-Z]" (symbol-name name)))
  (et-type (make-et-datatype :name name :args args)))

(defun et-alias (name &rest args)
  (cl-assert (symbolp name))
  (cl-assert (string-match-p "^[A-Z]" (symbol-name name)))
  (et-type (make-et-alias :name name :args args)))

(defun et-any () (et-dt 'Any))
(defun et-never () (make-et-type :cases nil))
(defun et-literal (val) (et-dt 'Literal val))


;;; ============================================================
;;; Matching
;;;; Matcher struct

(cl-defstruct et-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the possible match factors in disjunctive-nominal form. It is a
list of cases, each of which is a list of match factors.

Each match factor is one of:
  (M:DATATYPE DT-NAME ARGS...)
  (M:MATCH VAR)
  (M:SET VAR `et-type')

ARGS is a mix of constant args (where the corresponding arg role is
'CONST,) and DNFs for the other role types."
  generics dnf)

(defun et--verify-matcher (matcher)
  "Check that a matcher is valid."
  (or (et-matcher-p matcher)
      (error "Not a matcher: %s" matcher))

  (when et-debug
    (let ((generics (et-matcher-generics matcher)))
      (dolist (generic generics)
        (or (symbolp generic) (error "Generics must be a list of symbols")))

      (cl-flet ((genericp (var) (or (and (symbolp var) (memq var generics))
                                    (error "Not a generic: %s" var))))
        (dolist (case (et-matcher-dnf matcher))
          (dolist (factor case)
            (pcase factor
              (`(M:DATATYPE ,(and name (pred symbolp)) . ,args)
               (et--datatype-map-args
                name args
                (lambda (arg role)
                  (pcase role
                    ('CONST nil)
                    ((or 'CO 'CONTRA 'ISO) (make-et-matcher :generics generics :dnf arg))
                    (_ (error "Unknown role type: %s" role))))))
              (`(M:ALIAS ,(pred symbolp) . ,args)
               (dolist (arg args) (make-et-matcher :generics generics :dnf arg)))
              (`(M:MATCH ,(pred genericp)))
              (`(M:SET ,(pred genericp) ,(pred et-type-p)))
              (_ (error "Invalid match factor: %s" factor))))))))

  matcher)

(advice-add #'make-et-matcher :filter-return #'et--verify-matcher)


;;;; Expand matcher aliaes

(defun et--expand-alias-as-matcher-dnf (name args generics)
  "Expand an alias within a matcher."
  (let* ((s-args (cl-loop for arg in args collect (et-q (:structure (((S:MATCHER-DNF ,arg)))))))
         (structure (et-parse-structure (et--alias-call name s-args) generics)))
    (et-structure-to-matcher-dnf structure generics)))

(defun et--matcher-expand-aliases (matcher)
  (let ((generics (et-matcher-generics matcher))
        (dnf (et-matcher-dnf matcher)))
    (make-et-matcher :dnf (et--matcher-dnf-expand-aliases dnf generics)
                     :generics generics)))

(defun et--matcher-dnf-expand-aliases (dnf generics)
  (cl-loop for case in dnf
           nconc
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(M:ALIAS ,name . ,args)
                       (et--matcher-dnf-expand-aliases
                        (et--expand-alias-as-matcher-dnf name args generics)
                        generics))
                      (other (list (list other))))
                    into and-terms
                    finally return (apply #'et--dnf-and and-terms))))


;;;; Iso match

(defun et-iso-match (matcher type)
  (delete-dups (nconc (et--sub-constraints matcher type)
                      (et--super-constraints matcher type))))


;;;; Sub match

(defvar et--sub-constraints-stack nil
  "Stack of calls to `et--sub-constraints' with the form (MATCHER . TYPE).")

(defmacro et--stop-recursion (var elem default &rest body)
  (declare (indent 3))
  `(let ((elem ,elem))
     (if (member elem et--sub-constraints-stack)
         ,default
       (let ((,var (cons elem ,var)))
         ,@body))))

(defun et--sub-constraints (matcher type)
  (et--stop-recursion et--sub-constraints-stack (cons matcher type) nil
    (et--verify-matcher matcher)
    (et--verify-type type)

    (setq matcher (et--matcher-expand-aliases matcher))

    (cl-loop for case in (et-type-cases type)
             nconc (et--sub-constraints-2 matcher case) into result
             finally return (if (member '(Q:NEVER) result) (et-q ((Q:NEVER)))
                              (delete-dups result)))))

(defun et--sub-constraints-2 (matcher case)
  (et--verify-matcher matcher)
  (cl-assert (et-type-case-p case))

  (cl-loop for match-case in (et-matcher-dnf matcher)
           for result =
           (cl-loop for match-factor in match-case
                    for gens = (et-matcher-generics matcher)
                    nconc (et--sub-or-super-constraints-3 match-factor case gens))
           unless (member '(Q:NEVER) result)
           return result
           ;; If all cases failed, fallback to 2.2 or 2.3
           finally return
           (let ((val (et-type-case-value case)))
             (if (et-alias-p val)
                 (et--sub-constraints matcher (et-alias-expand val))
               (et-q ((Q:NEVER)))))))

(defun et--sub-or-super-constraints-3 (match-factor case generics &optional is-super)
  (et--verify-matcher (make-et-matcher :generics generics :dnf (list (list match-factor))))
  (et--verify-type (make-et-type :cases (list case)))

  (pcase match-factor
    (`(M:MATCH ,var) (et-q ((,(if is-super 'Q:LEQ 'Q:GEQ) ,var ,(et-type case)))))
    (`(M:SET ,var ,type) (et-q ((,(if is-super 'Q:LEQ 'Q:GEQ) ,var ,type))))
    (`(M:DATATYPE ,mdt-name . ,mdt-args)
     (pcase (et-type-case-value case)
       ((pred et-alias-p) (et-q ((Q:NEVER))))
       ((and dt (pred et-datatype-p))
        (et--sub-or-super-constraints-4
         mdt-name mdt-args (et-datatype-name dt) (et-datatype-args dt)
         generics is-super))
       (_ (error "Unsupported matching datatype"))))
    (_ (error "Invalid match factor"))))

(defun et--sub-or-super-constraints-4 (m-name m-args t-name t-args generics &optional is-super)
  (cl-flet ((make-matcher (dnf) (make-et-matcher :dnf dnf :generics generics)))
    (if (not is-super)
        ;; subtype matching (super=MATCHER > sub=TYPE)
        (et--datatype-constraints
         t-name t-args m-name m-args
         (lambda (type dnf) (et--sub-constraints (make-matcher dnf) type))
         (lambda (type dnf) (et--super-constraints (make-matcher dnf) type))
         (lambda (type dnf) (et-iso-match (make-matcher dnf) type))
         (lambda (literal dnf) (et--sub-constraints (make-matcher dnf) (et-literal literal))))
      ;; supertype matching (sub=MATCHER < super=TYPE)
      (et--datatype-constraints
       m-name m-args t-name t-args
       (lambda (dnf type) (et--super-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et--sub-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et-iso-match (make-matcher dnf) type))
       (lambda (literal type)
         (let ((literal-m (make-matcher (et-q (((M:DATATYPE Literal ,literal)))))))
           (et--super-constraints literal-m type)))))))


;;;; Super match

(defvar et--super-constraints-stack nil
  "Stack of calls to `et--super-constraints' with form (MATCHER . TYPE).")

(defun et--super-constraints (matcher type)
  (et--stop-recursion et--super-constraints-stack (cons matcher type) nil
    (et--verify-matcher matcher)
    (et--verify-type type)

    (setq matcher (et--matcher-expand-aliases matcher))

    (cl-loop for m-case in (et-matcher-dnf matcher)
             nconc (et--super-constraints-2 m-case type (et-matcher-generics matcher))
             into result
             finally return (if (member '(Q:NEVER) result) (et-q ((Q:NEVER)))
                              (delete-dups result)))))

(defun et--super-constraints-2 (match-case type generics)
  (make-et-matcher :dnf (list match-case) :generics generics)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for result =
           (cl-loop for match-factor in match-case
                    nconc (et--sub-or-super-constraints-3 match-factor case generics 'SUPER))
           unless (member '(Q:NEVER) result)
           return result
           ;; If all cases failed, return never
           finally return (et-q ((Q:NEVER)))))


;;; ============================================================
;;; Structure
;;;; Parsing

;; A "type structure" is a list of lists of the following form:
;; \(`S:DT' NAME ARGS...)
;; \(`S:ALIAS' NAME ARGS...)

;; Types only:
;; \(`S:BIND' VAR TYPE)
;; \(`S:TYPEOF' VAR)
;; \(`S:BINDS-OF' TYPE)
;; \(`S:TYPE' TYPE) - used for type alias expansion

;; Matchers only:
;; \(`S:GENERIC' VAR)
;; \(`S:SET' VAR DNF)
;; \(`S:MATCHER-DNF' DNF) - used for matcher alias expansion

(defun et-parse-structure (spec generics)
  (let ((case-fold-search nil)
        (parse (lambda (arg) (et-parse-structure arg generics))))

    (pcase spec
      ((pred symbolp) (et--parse-string (symbol-name spec) generics))
      (`(:parse ,(and str (pred stringp))) (et--parse-string str generics))

      ((or (pred stringp) (pred numberp)) (et-q (((S:DT Literal ,spec)))))

      (`(:structure ,structure) structure)

      (`(:bind ,var ,type) (et-q (((S:BIND ,var ,(et-parse-structure type generics))))))
      (`(:typeof ,var) (et-q (((S:TYPEOF ,var)))))

      (`(:or . ,args) (mapcan parse args))
      (`(:and . ,args)
       (or args (error "`and' cannot be empty"))
       (cl-reduce #'et--dnf-and (mapcar parse args)))

      (`(:literal ,val) (et-q (((S:DT Literal ,val)))))

      (`(:binds-of ,inner) (et-q (((S:BINDS-OF ,(et-parse-structure inner generics))))))

      (`(:set ,var ,type)
       (or (memq var generics) (error "Not a generic: %s" var))
       (et-q (((S:SET ,var ,(funcall parse type))))))

      ('(Nil) (et-q (((S:DT Literal nil)))))
      ('(True) (et-q (((S:DT Literal t)))))

      (`(,(and name (pred symbolp) (pred (not keywordp))) . ,args)
       (cond
        ((memq name generics)
         (or (null args) (error "Generic type cannot have arguments"))
         (et-q (((S:GENERIC ,name)))))

        ((et--datatype-name? name)
         (et-q (((S:DT ,name ,@(et--datatype-map-type-args name args parse))))))

        (t (et-q (((S:ALIAS ,name ,@(mapcar parse args))))))))

      (_ (error "Invalid parse syntax: %s" spec)))))

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
   ((string-match "^{\\(.*\\)}$" s) (et--parse-string (substring s 1 -1) generics))

   ;; @symbol
   ((string-match "^@\\(.*\\)$" s)
    (et-parse-structure (list :literal (intern (match-string 1 s))) generics))

   ;; Var=Type
   ((string-match "^\\([-a-zA-Z0-9]*\\)=\\(.*\\)$" s)
    (let ((var (intern (match-string 1 s)))
          (expr (match-string 2 s)))
      (et-parse-structure (list :set var (list :parse expr)) generics)))

   ;; Name or Name<...>
   ((string-match "^\\([-a-zA-Z0-9:]+\\)\\(?:<\\(.*\\)>\\)?$" s)
    (let* ((name (intern (match-string 1 s)))
           (inner (match-string 2 s)))
      (cl-loop for arg-str in (when inner (et--split-at-depth inner ?~))
               collect (list :parse arg-str) into args
               finally return (et-parse-structure (cons name args) generics))))

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


;;;; Printing

(defun et--format-structure (structure)
  "Format a structure DNF to a human-readable string."
  (if (null structure) "never"
    (mapconcat #'et--format-structure-case structure " | ")))

(defun et--format-structure-case (case)
  (if (null case) "any"
    (mapconcat #'et--format-structure-factor case " & ")))

(defun et--format-structure-factor (factor)
  (pcase factor
    (`(S:GENERIC ,var) (format "@%s" (symbol-name var)))
    (`(S:SET ,var ,sub) (format "%s=%s" var (et--format-structure sub)))
    (`(,(or 'S:DT 'S:ALIAS) ,name . ,args) (et--format-structure-named name args))
    (`(S:BIND ,var ,type-struct) (format "{%s : %s}" (et-var-name var) (et--format-structure type-struct)))
    (`(S:TYPEOF ,var) (format "{typeof %s}" (et-var-name var)))
    (_ (error "Invalid structure factor: %s" factor))))

(defun et--format-structure-named (name args)
  (pcase (cons name args)
    (`(Literal ,val)
     (format "`%s'" (prin1-to-string val)))

    ((and `(Cons ,left-sub ,right-sub) (guard nil))
     (let ((elems (list (et--format-structure left-sub))))
       (while (pcase right-sub
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     (`(S:DT Cons ,car-sub ,cdr-sub)
                      (nconc elems (list (et--format-structure car-sub)))
                      (setq right-sub cdr-sub)
                      t))))))
       (let ((tail-nil-p
              (and (= (length right-sub) 1)
                   (= (length (car right-sub)) 1)
                   (equal (car (car right-sub)) '(S:DT Literal nil)))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et--format-structure right-sub))))))

    (_
     (let* ((name-str (symbol-name name))
            (strs (if (not (et--datatype-name? name))
                      (mapcar #'et--format-structure args)
                    (et--datatype-map-args
                     name args
                     (lambda (arg role)
                       (if (eq role 'CONST) (format "%s" arg)
                         (et--format-structure arg)))))))
       (if (null args) name-str
         (format "%s<%s>" name-str (string-join strs ", ")))))))


;;;; To/from type

(defun et-structure-to-type (structure)
  "Convert a structure to an `et-type'.

This function will not succeed for all structures. Structures can
represent multiple types in a single case, but types do not support
this, so it will fail in this case.

Also, there are certain structure types that are designed for matchers,
which are invalid for types."

  (cl-loop for factors in structure
           collect
           (cl-loop for factor in factors
                    collect
                    (pcase factor
                      (`(S:TYPE ,type) (apply #'list (et-type-cases type)))
                      (`(S:DT ,name . ,args)
                       (let ((new-args (et--datatype-map-type-args name args #'et-structure-to-type)))
                         (list (make-et-type-case :value (make-et-datatype :name name :args new-args)))))
                      (`(S:ALIAS ,name . ,args)
                       (list (make-et-type-case :value (make-et-alias :name name :args (mapcar #'et-structure-to-type args)))))
                      (`(S:BIND ,var ,struct)
                       (list (make-et-type-case
                              :value (make-et-datatype :name 'Any)
                              :binds (list (cons var (et-structure-to-type struct))))))
                      (`(S:TYPEOF ,var)
                       (list (make-et-type-case :value (make-et-datatype :name 'Any) :typeofs (list var))))
                      (`(S:BINDS-OF ,struct) (et--binds-of-cases (et-structure-to-type struct)))
                      (_ (error "Invalid structure factor for type: %s" factor)))
                    into and-case-lists
                    finally return
                    (apply #'et--and (cl-loop for cs in and-case-lists
                                              collect (make-et-type :cases cs))))
           into or-types
           finally return (apply #'et--or or-types)))

(defun et-type-to-structure (type)
  "Convert an `et-type' to a structure DNF."
  (et--verify-type type)
  (cl-loop for case in (et-type-cases type)
           for value = (et-type-case-value case)
           collect
           (cons
            (cl-typecase value
              (et-datatype
               (et-ql S:DT ,(et-datatype-name value)
                      ,@(et--datatype-map-type-args
                         (et-datatype-name value)
                         (et-datatype-args value)
                         #'et-type-to-structure)))
              (et-alias
               (et-ql S:ALIAS ,(et-alias-name value)
                      ,@(mapcar #'et-type-to-structure (et-alias-args value))))
              (t (error "Unsupported type case value: %s" value)))

            (nconc
             (cl-loop for (var . type) in (et-type-case-binds case)
                      collect (et-ql S:BIND ,var ,(et-type-to-structure type)))
             (cl-loop for var in (et-type-case-typeofs case)
                      collect (et-ql S:TYPEOF ,var))))))


;;;; Parse/print type

(defun et-parse-type (spec)
  "Parse SPEC as an `et-type'."
  (et-structure-to-type (et-parse-structure spec nil)))

(defmacro et (&rest args)
  `(et-parse-type (et-q ,(if (eq (length args) 1) (car args) args))))


(defun et-pp (type)
  (et--format-structure (et-type-to-structure type)))

(cl-defmethod cl-print-object ((type et-type) stream)
  (princ (format "#<%s>" (et-pp type)) stream))


;;;; To/from matcher

(defun et-structure-to-matcher-dnf (structure generics)
  (let* ((convert-sub (lambda (sub) (et-structure-to-matcher-dnf sub generics))))
    (cl-loop for case in structure
             nconc
             (cl-loop for factor in case
                      collect
                      (pcase factor
                        (`(S:DT ,name . ,args)
                         (et-q (((M:DATATYPE ,name ,@(et--datatype-map-type-args name args convert-sub))))))
                        (`(S:ALIAS ,name . ,args)
                         (et-q (((M:ALIAS ,name ,@(mapcar convert-sub args))))))
                        (`(S:GENERIC ,var) (et-q (((M:MATCH ,var)))))
                        (`(S:SET ,var ,inner-dnf)
                         (et-q (((M:SET ,var ,(et-structure-to-type inner-dnf))))))
                        (`(S:MATCHER-DNF ,matcher-dnf) (copy-tree matcher-dnf))
                        (_ (error "Invalid match factor: %s" factor)))
                      into and-items
                      finally return (apply #'et--dnf-and and-items)))))


(defun et-matcher-to-structure (matcher)
  "Convert an `et-matcher' to a structure DNF."
  (et--verify-matcher matcher)

  (let* ((generics (et-matcher-generics matcher))
         (convert-sub
          (lambda (sub-dnf)
            (et-matcher-to-structure
             (make-et-matcher :generics generics :dnf sub-dnf)))))
    (cl-loop for case in (et-matcher-dnf matcher)
             collect
             (cl-loop for factor in case
                      collect
                      (pcase factor
                        (`(M:DATATYPE ,name . ,args)
                         (et-q (S:DT ,name ,@(et--datatype-map-type-args name args convert-sub))))
                        (`(M:ALIAS ,name . ,args)
                         (et-q (S:ALIAS ,name ,@(mapcar convert-sub args))))
                        (`(M:MATCH ,var) (et-q (S:GENERIC ,var)))
                        (`(M:SET ,var ,type)
                         (et-q (S:SET ,var ,(et-type-to-structure type)))))))))


;;;; Parse/print matcher

(defmacro et-matcher (generics &rest args)
  (declare (indent 1))
  (or (vectorp generics) (error "Write the generics as a vector"))
  `(et-parse-matcher (et-q ,(if (eq (length args) 1) (car args) args))
                     (et-q ,(append generics nil))))

(defun et-parse-matcher (spec generics)
  "Parse SPEC as an `et-matcher' with GENERICS."
  (make-et-matcher
   :generics generics
   :dnf (et-structure-to-matcher-dnf (et-parse-structure spec generics) generics)))


(defun et-pp-matcher (matcher)
  "Format an `et-matcher' into a human-readable string."
  (let* ((generics (et-matcher-generics matcher))
         (body (et--format-structure (et-matcher-to-structure matcher))))
    (format "[%s] %s" (mapconcat #'symbol-name generics " ") body)))

(cl-defmethod cl-print-object ((matcher et-matcher) stream)
  (princ (format "#<%s>" (et-pp-matcher matcher)) stream))


;;; ============================================================
;;; Type features
;;;; Subtype

(defun et-datatype-subtype? (sub super)
  (cl-assert (et-datatype-p sub))
  (cl-assert (et-datatype-p super))

  (cl-flet ((valid-if (valid) (if valid nil (et-ql (Q:NEVER)))))
    (let ((constraints
           (et--datatype-constraints
            (et-datatype-name sub) (et-datatype-args sub)
            (et-datatype-name super) (et-datatype-args super)
            (lambda (a b) (valid-if (et-subtype? a b)))
            (lambda (a b) (valid-if (et-subtype? b a)))
            (lambda (a b) (valid-if (and (et-subtype? a b) (et-subtype? b a))))
            (lambda (literal b) (valid-if (and (et-subtype? (et-literal literal) b)))))))

      (not (member '(Q:NEVER) constraints)))))

(defun et--binds-subtype? (sub-binds super-binds)
  (cl-loop for (var . super-type) in super-binds
           for sub-type = (alist-get var sub-binds)
           always (and sub-type (et-subtype? sub-type super-type))))

(defun et--case-subtype? (sub super)
  (cl-assert (et-type-case-p sub))
  (cl-assert (et-type-case-p super))

  (and (cl-loop with sub-vars = (et-type-case-typeofs sub)
                for super-var in (et-type-case-typeofs super)
                always (memq super-var sub-vars))
       ;; Macro expansion in `et-subtype?' means that the value should always be a datatype
       (et-datatype-subtype? (et-type-case-value sub) (et-type-case-value super))
       (et--binds-subtype? (et-type-case-binds sub) (et-type-case-binds super))))

;; Techincally, this function is duplicated logic. In theory, it
;; should convert one of the types to a matcher, perform matching, and
;; then check if the resulting constraints are valid. Since there are
;; no generics, the resulting constraints would be either nil, or just
;; contain Q:NEVER, which would tell us if it is a subtype.

(defun et-subtype? (sub super)
  (et--verify-type sub)
  (et--verify-type super)

  (if (equal sub super) t ; Not strictly necessary, but improves efficiency

    (setq sub (et-expand-all-aliases sub))
    (setq super (et-expand-all-aliases super))

    (cl-loop for sub-case in (et-type-cases sub)
             always
             (cl-loop for super-case in (et-type-cases super)
                      thereis
                      (et--case-subtype? sub-case super-case)))))


;;;; Simplify

(defun et-simplify-type (type)
  (et--verify-type type)

  (cl-loop for (case . rest) on (et-type-cases type)
           ;; Check if this case is redundant
           unless (cl-loop for c in (append new-cases rest)
                           ;; case is a subtype of c, so case is redundant
                           thereis
                           (and (et-subtype? (make-et-type :cases (list case))
                                             (make-et-type :cases (list c)))))

           collect case into new-cases
           finally return (make-et-type :cases new-cases)))


;;;; And/Or

(defun et--or (&rest types)
  "Return the exact type union of TYPES."
  (mapc #'et--verify-type types)

  (cl-loop for type in types
           nconc (apply #'list (et-type-cases type)) into cases
           finally return (make-et-type :cases cases)))

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
              finally return
              (let ((result (make-et-type :cases all-cases)))
                ;; This assertion should pass if there are no bugs
                (when et-debug
                  (or (and (et-subtype? result a) (et-subtype? result b))
                      (error "`et--and' determined incorrect intersection")))
                result)))))

(defun et--intersect-binds (a-binds b-binds)
  "Create a list of binds which are true if both A-BINDS and B-BINDS are."

  (cl-loop for (var . binds) in (seq-group-by #'car (append a-binds b-binds))
           collect (cons var (apply #'et--and (mapcar #'cdr binds)))))

(defun et--intersect-cases (a-case b-case)
  "Return a list of cases resulting from intersecting A-CASE and B-CASE."
  (cl-assert (et-type-case-p a-case))
  (cl-assert (et-type-case-p b-case))

  (let* ((a (et-type-case-value a-case))
         (b (et-type-case-value b-case))
         (a-case-raw (make-et-type-case :value a))
         (b-case-raw (make-et-type-case :value b))
         ;; Check if one of the values is a subtype of the other
         (sub-val (cond ((et-subtype? (et-type a-case-raw) (et-type b-case-raw)) a)
                        ((et-subtype? (et-type b-case-raw) (et-type a-case-raw)) b)))
         (make-case
          (lambda (val)
            (make-et-type-case
             :value val
             :binds (et--intersect-binds (et-type-case-binds a-case) (et-type-case-binds b-case))
             :typeofs (delete-dups (append (et-type-case-typeofs a-case)
                                           (et-type-case-typeofs b-case)
                                           nil))))))

    (cond
     (sub-val (list (funcall make-case sub-val)))

     ((et-alias-p a) (cl-loop for exp-case in (et-type-cases (et-alias-expand a))
                              nconc (et--intersect-cases exp-case b-case)))
     ((et-alias-p b) (cl-loop for exp-case in (et-type-cases (et-alias-expand b))
                              nconc (et--intersect-cases a-case exp-case)))

     ((and (et-datatype-p a) (et-datatype-p b))
      (let ((dt (et--intersect-datatypes a b)))
        (if (eq dt 'INVALID) nil
          (list (funcall make-case dt)))))

     (t (error "Signals not yet supported")))))

(defun et--intersect-datatypes (a b)
  "Returns the datatype resulting from intersecting A and B, or `INVALID'.

This cannot always return the exact intersection, but it will always
return a subtype of the intersection. It is garunteed to return a type
that is a subtype of both A and B."
  (cl-assert (et-datatype-p a))
  (cl-assert (et-datatype-p b))

  (let* ((a-name (et-datatype-name a))
         (b-name (et-datatype-name b))
         (a-args (et-datatype-args a))
         (b-args (et-datatype-args b))
         (sub-name (cond ((eq a-name b-name) a-name)
                         ((memq a-name (et--datatype-parents b-name)) b-name)
                         ((memq b-name (et--datatype-parents a-name)) a-name))))
    (cond
     ((et-datatype-subtype? a b) a)
     ((et-datatype-subtype? b a) b)

     (sub-name
      (let ((arg-intersection (et--datatype-intersect-args sub-name a-args b-args #'et--and #'et--or)))
        (if (eq arg-intersection 'INVALID) 'INVALID
          (make-et-datatype :name sub-name :args arg-intersection))))

     ((null sub-name) 'INVALID))))


;;;; Satisfy constraints

(defun et--sub-match (matcher type)
  (let ((constraints (et--sub-constraints matcher type)))
    (et--match-satisfy-constraints-smallest
     (et-matcher-generics matcher) constraints)))

(defun et--super-match (matcher type)
  (let ((constraints (et--super-constraints matcher type)))
    (et--match-satisfy-constraints-biggest
     (et-matcher-generics matcher) constraints)))

(defun et--match-satisfy-constraints-biggest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns the symbol NEVER if invalid."
  (if (member '(Q:NEVER) constraints) 'INVALID
    (cl-loop
     for gen in generics
     for gen-result =
     (let ((guess
            (cl-loop for (fact g type) in constraints
                     when (and (eq g gen) (memq fact '(Q:EQ Q:LEQ)))
                     collect type into types
                     finally return (et-simplify-type (apply #'et--and types)))))
       (if (cl-loop for (fact g type) in constraints
                    always
                    (or (not (eq g gen))
                        (not (memq fact '(Q:EQ Q:GEQ)))
                        (et-subtype? type guess)))
           guess (et-never)))
     when (equal gen-result (et-never))
     do (cl-return 'INVALID)
     collect gen-result)))

(defun et--match-satisfy-constraints-smallest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns the symbol NEVER if invalid.

However, unlike `et--match-satisfy-constraints-biggest', this allows
values to be the never type."
  (if (member '(Q:NEVER) constraints) 'INVALID
    (cl-loop
     for gen in generics
     for gen-result =
     (let ((guess
            (cl-loop for (fact g type) in constraints
                     when (and (eq g gen) (memq fact '(Q:EQ Q:GEQ)))
                     collect type into types
                     finally return (et-simplify-type (apply #'et--or types)))))
       (if (cl-loop for (fact g type) in constraints
                    always
                    (or (not (eq g gen))
                        (not (memq fact '(Q:EQ Q:LEQ)))
                        (et-subtype? guess type)))
           guess 'INVALID))
     ;; Unlike biggest, the never type actually represents a valid possible answer
     when (equal gen-result 'INVALID)
     do (cl-return 'INVALID)
     collect gen-result)))


;;;; Binds of

(defun et--remove-type-binds (type)
  (cl-assert (et-type-p type))

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           collect
           (make-et-type-case
            :value
            (cl-typecase val
              (et-alias
               (make-et-alias
                :name (et-alias-name val)
                :args (mapcar #'et--remove-type-binds (et-alias-args val))))
              (et-datatype
               (make-et-datatype
                :name (et-datatype-name val)
                :args (et--datatype-map-type-args (et-datatype-name val) (et-datatype-args val)
                                                  #'et--remove-type-binds)))
              (t (error "Invalid case val: %s" val))))
           into cases
           finally return (make-et-type :cases cases)))

(defun et--binds-of-cases (type)
  (cl-loop for case in (et-type-cases type)
           collect (make-et-type-case
                    :value (make-et-datatype :name 'Any)
                    :binds (et--intersect-binds
                            (et-type-case-binds case)
                            (cl-loop for var in (et-type-case-typeofs case)
                                     collect (cons var (et--remove-type-binds type)))))
           into new-cases
           finally return (delete-dups new-cases)))


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

(defun et--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (cdr path) (nth (car path) tree))))


;;;; Binds

(defvar et--binds nil
  "Stack of (SYMBOL . `et-var').")
(defvar et--narrow-binds nil
  "Stack of (`et-var' . `et-type').")

(defun et--var-bind (var)
  (cl-assert (et-var-p var))
  (or (alist-get var et--narrow-binds)
      (et-var-type var)))

(defmacro et-with-vars (vars &rest body)
  (declare (indent 1))
  `(let* ((vars (cl-loop for v in ,vars collect (cons (et-var-name v) v)))
          (et--binds (append vars et--binds)))
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
             for binds = (et-pp type) ; TODO: display just binds instead of whole type
             when binds
             collect (format fmt (et-pp-binds binds)) into strs
             finally do
             (when strs
               (et-warn '(0) (string-join strs "\\n"))))))


;;;; Define checker

(defmacro et-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(prog1 ',expr-type
     (setf (get ',expr-type 'et-checker)
           (lambda . ,(cl--transform-lambda
                       (cons arglist body)
                       (format "et--checker:%s" expr-type))))))

(defun et--type-checker-body (arglist-matcher return-struct exprs)
  (let* ((arg-types (cl-loop for _expr in exprs
                             for idx upfrom 1
                             collect (et-check-path idx)))
         (args-type (cl-loop with acc = (et-literal nil)
                             for arg-type in (nreverse arg-types)
                             do (setq acc (et-dt 'Cons:RR arg-type acc))
                             finally return acc))
         (result (et--sub-match arglist-matcher args-type)))
    (when (eq result 'INVALID)
      (error "Invalid arguments! Expected %s, got %s"
             (et-pp-matcher arglist-matcher) (et-pp args-type)))

    ;; Replace all places where the generic variable appeared in the return type
    ;; with the value determined for that generic
    (cl-loop for gen in (et-matcher-generics arglist-matcher)
             for type in result
             do (setq return-struct
                      (cl-subst (et-q (S:TYPE ,type))
                                (et-q (S:ALIAS ,gen))
                                return-struct
                                :test #'equal)))

    (et-structure-to-type return-struct)))

(defmacro et-define-type-checker (func &rest arguments)
  "Define a checker using argument and return types.

FUNC is the function to define the checker for.

GENERICS is a vector of symbols, representing generic variables. Each
generic variable should be uppercase.

ARGLIST is a parsable expression to use to match the arglist against.

RETURN is a parsable expression to use for the return type. This can use
the generic variable names as aliases, and they will be correctly
substituted.

\(fn FUNC [GENERICS] ARGLIST RETURN)"
  (declare (indent 2))
  (cl-assert (symbolp func))

  (let ((generics
         (when (vectorp (car arguments))
           ;; Make sure the generics have the correct format
           (cl-loop for var across (car arguments)
                    do (or (symbolp var) (error "Generic vars must be symbols"))
                    do (or (let ((case-fold-search nil))
                             (string-match-p "^[A-Z]" (format "%s" var)))
                           (error "Generic vars must start with an uppercase letter")))
           (append (pop arguments) nil))))
    (unless (eq (length arguments) 2)
      (error "Incorrect number of arguments"))

    `(et-define-checker ,func (&rest exprs)
       (et--type-checker-body
        ,(et-parse-matcher (car arguments) generics)
        (copy-tree ',(et-parse-structure (cadr arguments) nil))
        exprs))))


;;;; Check

(defun et-check ()
  "Returns the type of the current expr, if typechecking did not error."
  (et--verify-type
   (pcase et--current-expr
     (`(,func . ,args)
      (or (apply (or (get func 'et-checker) (error "No checker for function: %s" func))
                 args)
          (error "Checker for %s returned nil" func)))

     ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

      (let ((var (or (alist-get sym et--binds) (error "Free variable: %s" sym))))
        (et--and
         (or (et--var-bind var)
             (error "Free variable: %s" sym))
         (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                     :typeofs (list var))))))

     (expr (et-literal expr)))))


;;;; Check position helpers

(defun et-check-path (&rest path)
  (et-with-path path (et-check)))

(defun et-check-tail (start)
  (cl-loop for idx upfrom start below (length et--current-expr)
           for type = (et-with-path (list idx) (et-check))
           finally return (or type (et-literal nil))))


;;;; Root level functions

(defmacro et--root (expr &rest body)
  (declare (indent 1))
  `(progn
     (cl-assert (null et--current-expr))
     (cl-assert (null et--current-path))
     (cl-assert (null et--binds))
     (let ((et--current-expr ,expr))
       ,@body)))

(defmacro et-root-block (&rest body)
  (et--root (cons #'progn body)
    (et-check-tail 1)
    et--current-expr))

(defun et-root-check (expr)
  (et--root expr (et-check)))

(defmacro et-root-check-call (func &rest arg-types)
  `(et--root ',(cons func (cl-loop for type in arg-types collect (list :type type)))
     (et-check)))

(defun et-resolve (type)
  (setq type (et-parse-type type))

  (let ((expr-type (et-check)))
    (unless (et-subtype? expr-type type)
      (error "Type %s is not assignable to type %s"
             (et-pp expr-type) (et-pp type)))))

(defun et-root-resolve (type expr)
  (et--root expr (et-resolve type)))


;;; ============================================================
;;; Application
;;;; Built-in aliases

(et-defalias Nil () (Literal nil))
(et-defalias True () (Literal t))
(et-defalias Boolean () (:or True Nil))

(et-defalias List (elem) (:or Nil (Cons ,elem (List ,elem))))
(et-defalias List:R (elem) (:or Nil (Cons:RR ,elem (List:R ,elem))))

(et-defalias Tree (elem) (:or ,elem (List (Tree ,elem))))
(et-defalias Tree:R (elem) (:or ,elem (:List:r (:Tree:r ,elem))))

(et-defalias Alist (key val) (List (Cons ,key ,val)))
(et-defalias Alist:R (key val) (List:R (Cons:RR ,key ,val)))

(defun et--expand-tuple (cons args)
  (if (null args) 'Nil
    (et-q (,cons ,(car args) ,(et--expand-tuple cons (cdr args))))))

(et-defalias Tuple (&rest args) ,(et--expand-tuple 'Cons args))
(et-defalias Tuple:R (&rest args) ,(et--expand-tuple 'Cons:RR args))


;;; ============================================================
;;; Utils
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

(defmacro et-assert-string= (string expr2)
  (declare (indent 1))
  (cl-assert (stringp string))
  (let ((val-str (cl-prin1-to-string (eval expr2))))
    (or (equal string val-str)
        (error "Expressions not equal: \"%s\" \"%s\"" string val-str))))

(defmacro et-assert-true (expr)
  (or (eval expr) (error "Returned nil")))

(defmacro et-assert-nil (expr)
  (when (eval expr) (error "Returned non-nil")))


;;;; Testing checkers

(et-define-checker :assert-subtype (_expr type)
  (let ((expr-type (et-check-path 1)))
    (or (et-subtype? expr-type (eval type))
        (error "Not subtype: %s" (et-pp expr-type)))
    (setq et--current-expr "dummy")
    (et-literal nil)))

(et-define-checker :assert-error (_expr)
  (condition-case _err (et-check-path 1)
    (error (setq et--current-expr nil) (et-literal nil))
    (:success (error "Didn't error"))))

(et-define-checker :typeof (_expr)
  (et-warn '(0) "%s" (et-pp (et-check-path 1)))
  (setq et--current-expr nil)
  (et-literal nil))

(et-define-checker :narrows ()
  (cl-loop for ((var . _) . type) in (reverse et--narrow-binds)
           collect (format "%s: %s" var (et-pp type)) into strs
           finally do
           (et-warn '(0) "%s" (string-join strs "\\n")))
  (setq et--current-expr nil)
  (et-literal nil))


;;; ============================================================
;;; Provide

(provide 'et-redo)


;;; et.el ends here
