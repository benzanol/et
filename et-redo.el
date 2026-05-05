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


;;; ============================================================
;;; Macros
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


(defun et--datatype-parents (dt-name)
  "Return a list of datatype names which are parents of `DT-NAME'.

If B is a parent of A, then swapping the name A for B in any datatype
creates a strictly larger type.

For example, (:cons CAR CDR) <= (:cons:rr CAR CDR) no matter what CAR
and CDR are, so :cons:rr is a parent type of :cons."
  (pcase dt-name
    (':cons (list :cons:rr :cons:wo :cons:wr :cons:rw))
    (':integer (list :number))))

(defun et--datatype-arg-roles (dt-name dt-args)
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'.

The resulting list must be the exact length of DT-ARGS, and each element
corresponds to the role of each argument in `dt-args'. `CONST' indicates
an argument which is a literal Lisp value. `CO'/`CONTRA'/`ISO' indicate
that the argument is a type argument, and whether the type argument is
covariant, contravariant, or isovariant."

  (pcase (cons dt-name dt-args)
    (`(,(guard (alist-get dt-name et-scoped-datatypes))) nil)
    (`(:literal ,_arg) (list 'CONST))
    (`(:cons ,_car ,_cdr) (list 'ISO 'ISO))
    (`(:cons:rr ,_car ,_cdr) (list 'CO 'CO))
    (`(:cons:wo ,_car ,_cdr) (list 'CONTRA 'CONTRA))
    (`(:vector ,_elem) (list 'ISO))
    (`(:plist . ,args)
     (cl-loop for (prop _val) on args by #'cddr
              do (or (keywordp prop) (error "Expected keyword, found %s" prop))
              nconc (list 'CONST 'CO)))
    (`(,(or :integer :number :string :symbol :any)) nil)
    (_ (error "Invalid datatype: %s %s" dt-name dt-args))))

(defun et--datatype-constraints (sub-name sub-args super-name super-args co contra iso co-literal)
  (cl-flet ((valid-if (valid) (if valid nil (et-ql (q:never)))))

    (pcase (list sub-name super-name)
      ('(:plist :plist)
       (cl-loop for (prop super-val) on super-args by #'cddr
                for sub-val = (plist-get sub-args prop)
                unless sub-val return (et-ql (q:never))
                nconc (funcall co sub-val super-val)))

      (`(:literal ,_)
       (let ((val (car sub-args)))
         (pcase super-name
           (:integer (valid-if (integerp val)))
           (:number (valid-if (numberp val)))
           (:string (valid-if (stringp val)))
           (:symbol (valid-if (symbolp val)))
           (:cons
            (if (not (consp val)) (valid-if nil)
              (nconc (funcall co-literal (car val) (car super-args))
                     (funcall co-literal (cdr val) (cadr super-args))))))))

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
  "An alist where each entry is (NAME-KEYWORD TYPE-FN PROPS...).

TYPE-FN is a function which takes the alias arguments and returns a
structure which can be parsed by `et-parse-type'.")

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

(defun et--alias-call (name args)
  "Call the alias expansion function for alias NAME with args ARGS."
  (let ((type-fn (or (car (alist-get name et-aliases)) (error "Alias %s is not defined" name))))
    (apply type-fn args)))

(defun et-alias-expand (alias)
  "Expand an alias to a type."
  (let ((s-args (cl-loop for type in (et-alias-args alias)
                         collect (et-q (:structure (((TYPE ,type))))))))
    (et-parse-type (et--alias-call (et-alias-name alias) s-args))))

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
  (unless (et-type-p type)
    (error "Not a type: %s" type))

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

(advice-add #'make-et-type :filter-return #'et--verify-type)


;;; ============================================================
;;; Matcher
;;;; Matcher struct

(cl-defstruct et-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the possible match factors in disjunctive-nominal form. It is a
list of cases, each of which is a list of match factors.

Each match factor is one of:
  (m:datatype DT-NAME ARGS...)
  (m:match VAR)
  (m:set VAR `et-type')

ARGS is a mix of constant args (where the corresponding arg role is
'CONST,) and DNFs for the other role types."
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
             (et--datatype-map-args
              name args
              (lambda (arg role)
                (pcase role
                  ('CONST nil)
                  ((or 'CO 'CONTRA 'ISO) (make-et-matcher :generics generics :dnf arg))
                  (_ (error "Unknown role type: %s" role))))))
            (`(m:alias ,(pred keywordp) . ,args)
             (dolist (arg args) (make-et-matcher :generics generics :dnf arg)))
            (`(m:match ,(pred genericp)))
            (`(m:set ,(pred genericp) ,(pred et-type-p)))
            (_ (error "Invalid match factor: %s" factor)))))
      matcher)))

(advice-add #'make-et-matcher :filter-return #'et--verify-matcher)


;;;; Expand matcher aliaes

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

(defun et--expand-alias-as-matcher-dnf (name args generics)
  "Expand an alias within a matcher."
  (let* ((s-args (cl-loop for arg in args collect (et-q (:structure (((MATCHER-DNF ,arg)))))))
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
                      (`(m:alias ,name . ,args)
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
             finally return (if (member '(q:never) result) (et-q ((q:never)))
                              (delete-dups result)))))

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
               (et-q ((q:never)))))))

(defun et--sub-or-super-constraints-3 (match-factor case generics &optional is-super)
  (et--verify-matcher (make-et-matcher :generics generics :dnf (list (list match-factor))))
  (et--verify-type (make-et-type :cases (list case)))

  (pcase match-factor
    (`(m:match ,var) (et-q ((,(if is-super 'q:leq 'q:geq) ,var ,(et-type case)))))
    (`(m:set ,var ,type) (et-q ((,(if is-super 'q:leq 'q:geq) ,var ,type))))
    (`(m:datatype ,mdt-name . ,mdt-args)
     (pcase (et-type-case-value case)
       ((pred et-alias-p) (et-q ((q:never))))
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
         (lambda (literal dnf) (et--sub-constraints (make-matcher dnf) (et-dt :literal literal))))
      ;; supertype matching (sub=MATCHER < super=TYPE)
      (et--datatype-constraints
       m-name m-args t-name t-args
       (lambda (dnf type) (et--super-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et--sub-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et-iso-match (make-matcher dnf) type))
       (lambda (literal type)
         (let ((literal-m (make-matcher (et-q (((m:datatype :literal ,literal)))))))
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
             finally return (if (member '(q:never) result) (et-q ((q:never)))
                              (delete-dups result)))))

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
           finally return (et-q ((q:never)))))


;;;; Satisfy constraints

(defun et--match-satisfy-constraints-biggest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns the symbol NEVER if invalid."
  (if (member '(q:never) constraints) 'INVALID
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
  (if (member '(q:never) constraints) 'INVALID
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
                        (et-subtype? guess type)))
           guess 'INVALID))
     ;; Unlike biggest, the never type actually represents a valid possible answer
     when (equal gen-result 'INVALID)
     do (cl-return 'INVALID)
     collect gen-result)))


;;;; Putting it together

(defun et-sub-match (matcher type)
  (let ((constraints (et--sub-constraints matcher type)))
    (et--match-satisfy-constraints-smallest
     (et-matcher-generics matcher) constraints)))

(defun et-super-match (matcher type)
  (let ((constraints (et--super-constraints matcher type)))
    (et--match-satisfy-constraints-biggest
     (et-matcher-generics matcher) constraints)))


;;; ============================================================
;;; Structure
;;;; Parsing

;; A "type structure" is a list of lists of the following form:
;; \(`DT' NAME ARGS...)
;; \(`ALIAS' NAME ARGS...)
;; \(`GENERIC' VAR)
;; \(`SET' VAR DNF)
;; \(`MATCHER-DNF' MATCHER-DNF) - used for matcher alias expansion
;; \(`TYPE' TYPE) - used for type alias expansion

(defun et-parse-structure (spec generics)
  (let ((case-fold-search nil)
        (parse (lambda (arg) (et-parse-structure arg generics))))

    (pcase spec
      ((pred stringp) (et--parse-string spec generics))
      ((pred keywordp) (et--parse-string (substring (symbol-name spec) 1) generics))

      (`(:structure ,structure) structure)

      (`(:or . ,args) (mapcan parse args))
      (`(:and . ,args)
       (or args (error "`and' cannot be empty"))
       (cl-reduce #'et--dnf-and (mapcar parse args)))

      (`(:never) nil)
      (`(:any) (et-q (((DT :any)))))
      (`(:nil) (et-q (((DT :literal nil)))))
      (`(:t) (et-q (((DT :literal t)))))

      (`(:sym ,val)
       (when (stringp val) (setq val (intern val)))
       (or (symbolp val) (error "Not a symbol: %s" val))
       (et-q (((DT :literal ,val)))))

      (`(:num ,val)
       (and (stringp val) (string-match-p "^[0-9][0-9_]*\\.?[0-9_]*$" val)
            (setq val (string-to-number val)))
       (or (numberp val) (error "Not a number: %s" val))
       (et-q (((DT :literal ,val)))))

      (`(:str ,str)
       (or (stringp str) (error "Not a string: %s" str))
       (et-q (((DT :literal ,str)))))

      (`(:set ,var ,type)
       (or (memq var generics) (error "Not a generic: %s" var))
       (et-q (((SET ,var ,(funcall parse type))))))

      (`(,(and name (pred keywordp)) . ,args)
       (cond
        ((memq name generics)
         (or (null args) (error "Generic type cannot have arguments"))
         (et-q (((GENERIC ,name)))))

        ((string-match-p "^:[A-Z]" (symbol-name name))
         (et-q (((ALIAS ,name ,@(mapcar parse args))))))

        (t (et-q (((DT ,name ,@(et--datatype-map-type-args name args parse))))))))

      (_ (error "Invalid structure spec: %s" spec)))))

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
      (et-parse-structure (list :set var expr) generics)))

   ;; Name or Name<...>
   ((string-match "^\\([A-Za-z][a-zA-Z0-9:]*\\)\\(?:<\\(.*\\)>\\)?$" s)
    (let* ((kwd (intern (concat ":" (match-string 1 s))))
           (inner (match-string 2 s))
           (arg-strs (when inner (et--split-at-depth inner ?~))))
      (et-parse-structure (cons kwd arg-strs) generics)))

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
    (`(GENERIC ,var) (format "@%s" (substring (symbol-name var) 1)))
    (`(SET ,var ,sub) (format "%s=%s" var (et--format-structure sub)))
    (`(DT ,name . ,args) (et--format-structure-dt name args))
    (`(ALIAS ,name . ,args) (et--format-structure-alias name args))
    (_ (error "Invalid structure factor: %s" factor))))

(defun et--format-structure-dt (name args)
  (pcase (cons name args)
    (`(:literal ,val)
     (if (and (symbolp val) val (not (eq val t)))
         (format "`%s'" val)
       (prin1-to-string val)))

    (`(:cons ,left-sub ,right-sub)
     (let ((elems (list (et--format-structure left-sub))))
       (while (pcase right-sub
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     (`(DT :cons ,car-sub ,cdr-sub)
                      (nconc elems (list (et--format-structure car-sub)))
                      (setq right-sub cdr-sub)
                      t))))))
       (let ((tail-nil-p
              (and (= (length right-sub) 1)
                   (= (length (car right-sub)) 1)
                   (equal (car (car right-sub)) '(DT :literal nil)))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et--format-structure right-sub))))))

    (_
     (let* ((name-str (substring (symbol-name name) 1))
            (strs (et--datatype-map-args
                   name args
                   (lambda (arg role)
                     (if (eq role 'CONST) (format "%s" arg)
                       (et--format-structure arg))))))
       (if (null args) name-str
         (format "%s<%s>" name-str (string-join strs ", ")))))))

(defun et--format-structure-alias (name args)
  (let* ((name-str (substring (symbol-name name) 1))
         (strs (mapcar #'et--format-structure args)))
    (if (null args) name-str
      (format "%s<%s>" name-str (string-join strs ", ")))))


;;;; To/from type

(defun et-structure-to-type (structure)
  "Convert a structure to an `et-type'."
  (cl-loop for case in structure
           when (cdr case) do (error "Type cannot represent AND: %s" case)
           for factor = (car case)
           nconc
           (if (eq (car factor) 'TYPE)
               (apply #'list (et-type-cases (cadr factor)))
             (list (make-et-type-case
                    :value
                    (pcase factor
                      (`(DT ,name . ,args)
                       (make-et-datatype
                        :name name
                        :args (et--datatype-map-type-args name args #'et-structure-to-type)))
                      (`(ALIAS ,name . ,args)
                       (make-et-alias :name name :args (mapcar #'et-structure-to-type args)))
                      (f (error "Invalid type factor: %s" f))))))
           into cases
           finally return (make-et-type :cases cases)))

(defun et-type-to-structure (type)
  "Convert an `et-type' to a structure DNF."
  (et--verify-type type)
  (cl-loop for case in (et-type-cases type)
           for value = (et-type-case-value case)
           collect
           (list
            (cl-typecase value
              (et-datatype
               (et-q (DT ,(et-datatype-name value)
                         ,@(et--datatype-map-type-args
                            (et-datatype-name value)
                            (et-datatype-args value)
                            #'et-type-to-structure))))
              (et-alias
               (et-q (ALIAS ,(et-alias-name value)
                            ,@(mapcar #'et-type-to-structure (et-alias-args value)))))
              (t (error "Unsupported type case value: %s" value))))))


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
                        (`(DT ,name . ,args)
                         (et-q (((m:datatype ,name ,@(et--datatype-map-type-args name args convert-sub))))))
                        (`(ALIAS ,name . ,args)
                         (et-q (((m:alias ,name ,@(mapcar convert-sub args))))))
                        (`(GENERIC ,var) (et-q (((m:match ,var)))))
                        (`(SET ,var ,inner-dnf)
                         (et-q (((m:set ,var ,(et-structure-to-type inner-dnf))))))
                        (`(MATCHER-DNF ,matcher-dnf) (copy-tree matcher-dnf))
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
                        (`(m:datatype ,name . ,args)
                         (et-q (DT ,name ,@(et--datatype-map-type-args name args convert-sub))))
                        (`(m:alias ,name . ,args)
                         (et-q (ALIAS ,name ,@(mapcar convert-sub args))))
                        (`(m:match ,var) (et-q (GENERIC ,var)))
                        (`(m:set ,var ,type)
                         (et-q (SET ,var ,(et-type-to-structure type)))))))))


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
;;; Helpters
;;;; Subtype

(defun et-datatype-subtype? (sub super)
  (cl-assert (et-datatype-p sub))
  (cl-assert (et-datatype-p super))

  (cl-flet ((valid-if (valid) (if valid nil (et-ql (q:never)))))
    (not (member
          '(q:never)
          (et--datatype-constraints
           (et-datatype-name sub) (et-datatype-args sub)
           (et-datatype-name super) (et-datatype-args super)
           (lambda (a b) (valid-if (et-subtype? a b)))
           (lambda (a b) (valid-if (et-subtype? b a)))
           (lambda (a b) (valid-if (and (et-subtype? a b) (et-subtype? b a))))
           (lambda (literal b) (valid-if (and (et-subtype? (et-dt :literal literal) b)))))))))

(defun et-subtype? (sub super)
  (et--verify-type sub)
  (et--verify-type super)

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
  (cl-assert (keywordp name))
  (cl-assert (string-match-p "^:[a-z]" (symbol-name name)))
  (et-type (make-et-datatype :name name :args args)))

(defun et-alias (name &rest args)
  (cl-assert (keywordp name))
  (cl-assert (string-match-p "^:[A-Z]" (symbol-name name)))
  (et-type (make-et-alias :name name :args args)))

(defun et-any () (et-dt :any))
(defun et-never () (make-et-type :cases nil))


;;;; Built-in aliases

(et-defalias :Boolean ()
  (et-q (:or :t :nil)))

(et-defalias :List (elem) (et-q (:or :nil (:cons ,elem (:List ,elem)))))
(et-defalias :List:r (elem) (et-q (:or :nil (:cons:rr ,elem (:List:r ,elem)))))

(et-defalias :Tree (elem) (et-q (:or ,elem (:List (:Tree ,elem)))))
(et-defalias :Tree:r (elem) (et-q (:or ,elem (:List:r (:Tree:r ,elem)))))

(et-defalias :Alist (key val) (et-q (:List (:cons ,key ,val))))
(et-defalias :Alist:r (key val) (et-q (:List:ro (:cons:rr ,key ,val))))

(defun et--expand-tuple (cons args)
  (if (null args) :nil
    (et-q (,cons ,(car args) ,(et--expand-tuple cons (cdr args))))))

(et-defalias :Tuple (&rest args) (et--expand-tuple :cons args))
(et-defalias :Tuple:r (&rest args) (et--expand-tuple :cons:rr args))


;;; ============================================================
;;; Provide

(provide 'et-redo)


;;; et-redo.el ends here
