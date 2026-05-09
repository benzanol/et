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
;;; Testing macros
;;;; Flycheck rebasing

(defvar et--error-path nil)

(defmacro et-with-error-path (path &rest body)
  (declare (indent 1))
  `(let* ((et--error-path (append et--error-path ,path nil)))
     (condition-case err (progn ,@body)
       (error (et-error nil (error-message-string err)) nil))))

(defun et-error (path string &rest args)
  (setq string (concat string (et--error-message-suffix path)))
  (byte-compile-warn "%s" (if args (apply #'format string args) string)))

(defun et--error-message-suffix (path)
  (format "\0;;flycheck-path:%s" (append et--error-path path)))

(defun et--traverse-buffer-expr (path)
  (dolist (idx path)
    (cond
     ((looking-at-p "[`']")
      (if (eq idx 1)
          (forward-char 1)
        (error "Only valid subexpr of quote is 1")))

     ((looking-at-p "[([]")
      (forward-char 1)
      (dotimes (_ idx) (forward-sexp))
      (forward-sexp)
      (backward-sexp))

     (t (error "Invalid expression container: %s" (thing-at-point 'char))))))

(defun et--flycheck-reposition-error (err)
  "If ERR has a ;;flycheck-path: sentinel, reposition it."
  (ignore ; return nil so other handlers still run
   (ignore-errors
     (when-let* ((msg (flycheck-error-message err))
                 (match (string-match "\0;;flycheck-path:\\((.*)\\)" msg))
                 (path (car (read-from-string (match-string 1 msg))))
                 (prev-start t))

       ;; Set the level of warnings to info
       (if (eq (flycheck-error-level err) 'warning)
           (setf (flycheck-error-level err) 'info))

       ;; Find the macro call in the buffer and walk the path
       (with-current-buffer (flycheck-error-buffer err)
         (save-excursion
           (goto-char (flycheck-error-pos err)) ; start near the error
           (beginning-of-defun)

           (et--traverse-buffer-expr path)

           (setf (flycheck-error-line err) (line-number-at-pos))
           (setf (flycheck-error-column err) (1+ (current-column)))
           (forward-sexp)
           (setf (flycheck-error-end-line err) (line-number-at-pos))
           (setf (flycheck-error-end-column err) (1+ (current-column)))

           ;; Strip the path from the displayed message
           (setf (flycheck-error-message err)
                 (replace-regexp-in-string "\\\\n" "\n" (substring msg 0 match)))))))))

(add-hook 'flycheck-process-error-functions #'et--flycheck-reposition-error)


;;;; Repeat

(defmacro et-repeat (var repls &rest body)
  (declare (indent 2))
  (cl-assert (vectorp repls))
  (cl-loop for repl across repls
           collect (cl-subst repl var body) into all
           finally return (cons #'ignore all)))


;;;; Testing

(eval-and-compile
  (defvar et-run-tests (if noninteractive t nil)
    "Whether to run et tests when compiling source files."))

(defmacro et-test (&rest body)
  "BODY can start with a series of VAR VECTOR... forms."

  (when (and et-run-tests (null load-file-name))
    ;; Require the file without tests
    (when (stringp (car command-line-args-left))
      (let ((et-run-tests nil))
        (load-file (car command-line-args-left))))

    ;; Repeat var
    (let* ((evaller
            (lambda (body start-idx)
              (cl-loop for expr in body
                       for idx upfrom 0
                       do (et-with-error-path (list (+ idx start-idx))
                            (or (eval expr) (error "Returned nil"))))))
           repeat-var repeat-forms)

      (when (symbolp (car body))
        (setq repeat-var (pop body)
              repeat-forms (pop body)))

      (if repeat-var
          (cl-loop for form across repeat-forms
                   do (funcall evaller (cl-subst form repeat-var body) 3))
        (funcall evaller body 1)))))

(defmacro et-assert-error (expr)
  (declare (indent 1))
  `(et-with-error-path (list 1)
     (condition-case val ,expr
       (error t)
       (:success (error "=> %s" val)))))


;;; ============================================================
;;; Utils
;;;; Quote macro

(eval-and-compile
  (defun et--copy-quotes (expr)
    (cond ((eq (car-safe expr) #'quote) (list #'copy-tree expr))
          ((consp expr) (cons (et--copy-quotes (car expr)) (et--copy-quotes (cdr expr))))
          (t expr))))

(et-test
 (equal '(a (copy-tree '(b)) c (copy-tree ''(((1 2 'hi)))))
        (et--copy-quotes '(a '(b) c ''(((1 2 'hi)))))))


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
  ;; Positive and Negative do not serve much practical purpose, but
  ;; exist primarily to give an example of datatypes without a well
  ;; defined intersection (Positive ∩ Integer).
  (not (not (memq name '(Any Literal NonNil Function
                             Number Integer Positive Negative
                             String Symbol NonNilSymbol
                             Vector:A Vector:- Vector:R Vector:W
                             Cons:AA Cons:-- Cons:RR Cons:WW Cons:RW Cons:WR)))))

(defun et--datatype-arg-roles (dt-name dt-args)
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'.

The resulting list must be the exact length of DT-ARGS, and each element
corresponds to the role of each argument in `dt-args'. `CONST' indicates
an argument which is a literal Lisp value. `CO'/`CONTRA'/`ISO' indicate
that the argument is a type argument, and whether the type argument is
covariant, contravariant, or isovariant. The `IGNORE' role means that
this argument does not contribute to whether one type extends another."

  (pcase (cons dt-name dt-args)
    (`(,(guard (alist-get dt-name et-scoped-datatypes))) nil)
    (`(Literal ,_arg) (et-ql CONST))
    (`(Function ,_args ,_ret) (et-ql CONTRA CO))
    (`(Cons:AA ,_car ,_cdr) (et-ql ISO ISO))
    (`(Cons:RR ,_car ,_cdr) (et-ql CO CO))
    (`(Cons:WW ,_car ,_cdr) (et-ql CONTRA CONTRA))
    (`(Cons:RW ,_car ,_cdr) (et-ql CO CONTRA))
    (`(Cons:WR ,_car ,_cdr) (et-ql CONTRA CO))
    (`(Cons:-- ,_car ,_cdr) (et-ql IGNORE IGNORE))
    (`(Vector:A ,_elem) (et-ql ISO))
    (`(Vector:R ,_elem) (et-ql CO))
    (`(Vector:W ,_elem) (et-ql CONTRA))
    (`(Vector:- ,_elem) (et-ql IGNORE))
    (`(PList . ,args)
     (cl-loop for (prop _val) on args by #'cddr
              do (or (keywordp prop) (error "Expected keyword, found %s" prop))
              nconc (et-ql CONST CO)))
    (`(,(or Integer Number String Symbol Any NonNil NonNilSymbol)) nil)
    (_ (error "Invalid datatype: %s %s" dt-name dt-args))))

(defun et--datatype-parents (dt-name)
  "Return a list of datatype names which are parents of `DT-NAME'.

If B is a parent of A, then swapping the name A for B in any datatype
creates a strictly larger type.

For example, (Cons:AA CAR CDR) <= (Cons:RR CAR CDR) no matter what CAR
and CDR are, so Cons:RR is a parent type of Cons:AA."
  (pcase dt-name
    ('Cons:AA (et-ql Cons:RR Cons:WW Cons:WR Cons:RW Cons:--))
    ((or 'Cons:RR 'Cons:WW 'Cons:WR 'Cons:RW) (et-ql Cons:--))
    ('Vector:A (et-ql Vector:R Vector:W Vector:-))
    ((or 'Vector:R 'Vector:W) (et-ql Vector:-))
    ('Integer (et-ql Number))
    ('Positive (et-ql Number))
    ('Negative (et-ql Number))
    ('NonNilSymbol (et-ql NonNil Symbol))))

(defun et--datatype-might-overlap-nontrivial? (a b)
  "Return whether datatypes A and B might overlap.

This function is designed for `nontrivial' cases, in that it assumes
that A and B are not subtypes of each other."
  (let* ((a-name (et-datatype-name a))
         (b-name (et-datatype-name b))
         (cons-types '(PList Function Cons:AA Cons:-- Cons:RR Cons:WW Cons:RW Cons:WR))
         (pos-types '(Integer Positive))
         (neg-types '(Integer Negative))
         (symbol-types '(Symbol NonNilSymbol))
         (nil-dt (make-et-datatype :name 'Literal :args (list nil))))

    (cond
     ;; One is a NonNil
     ((or (and (eq a-name 'NonNil) (not (equal b nil-dt)))
          (and (eq b-name 'NonNil) (not (equal a nil-dt))))
      t)

     ;; Two literals
     ((and (eq a-name 'Literal) (eq b-name 'Literal))
      (eq (car (et-datatype-args a)) (car (et-datatype-args b))))

     ;; Two elements of the same group
     ((or (eq a-name b-name)
          (memq a-name (et--datatype-parents b-name))
          (memq b-name (et--datatype-parents a-name))

          (and (memq a-name symbol-types) (memq b-name symbol-types))
          (and (memq a-name cons-types) (memq b-name cons-types))
          (and (memq a-name pos-types) (memq b-name pos-types))
          (and (memq a-name neg-types) (memq b-name neg-types)))))))

(defun et--datatype-intersect-args-nontrivial (name args1 args2 intersect union)
  "Return a list of arguments intersecting ARGS1 and ARGS2.

The goal of this function is to determine a list of arguments
INTERSECTION-ARGS such that (NAME INTERSECTION-ARGS) is a subtype of
both (NAME ARGS1) and (NAME ARGS2).

If no such list is found, then return the symbol `INVALID'.

This function is designed for `nontrivial' cases in that it assumes that
neither datatype is already a subset of the other, in which case the
subset args would be a trivial solution to this function. This is so
that this function can focus on the non-trivial cases where neither is a
subset of the other.

INTERSECT and UNION are functions which each take 2 elements from
ARGS1/ARGS2 and return a new arg, either the intersection or union of
the two args respectively."

  (cond
   ;; Handling for normal datatypes, where arguments have a fixed order
   ((memq name '(Function
                 Cons:-- Cons:AA Cons:RR Cons:WW Cons:WR Cons:RW
                 Vector:A Vector:- Vector:R Vector:W))
    (cl-assert (eq (length args1) (length args2)))
    (cl-loop for role in (et--datatype-arg-roles name args1)
             for arg1 in args1
             for arg2 in args2
             for new-arg = (pcase role
                             ('CO (funcall intersect arg1 arg2))
                             ('CONTRA (funcall union arg1 arg2))
                             ('ISO (if (equal arg1 arg2) arg1 'INVALID))
                             ('IGNORE (et-any))
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
           ('Function (valid-if (eq 'lambda (car-safe val))))
           ('Integer (valid-if (integerp val)))
           ('Positive (valid-if (and (numberp val) (> val 0))))
           ('Negative (valid-if (and (numberp val) (< val 0))))
           ('Number (valid-if (numberp val)))
           ('String (valid-if (stringp val)))
           ('Symbol (valid-if (symbolp val)))
           ('Vector:- (valid-if (vectorp val)))
           ('Vector:R (if (not (vectorp val)) (valid-if nil)
                        (cl-loop for elem across val
                                 nconc (funcall co-literal elem (car super-args)))))
           ('Cons:-- (valid-if (consp val)))
           ('Cons:RR
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
                        ('IGNORE nil)
                        (_ (error "Unknown argument role: %s" role)))))

      (_ (valid-if nil)))))


;;;; Datatype mappers

(defun et--datatype-map-args (dt-name dt-args func)
  "Apply FUNC to each argument, returning the resulting list.

FUNC is called with two arguments, ARG and ROLE, where role is one of
`CONST', `CO', `CONTRA', `ISO', or `IGNORE'."
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


;;;; Built-in aliases

(et-defalias Nil () (Literal nil))
(et-defalias True () (Literal t))
(et-defalias Boolean () (or True Nil))

(et-defalias List:R (elem) (or Nil (Cons:RR ,elem (List:R ,elem))))
(et-defalias NonNilList:R (elem) (Cons:RR ,elem (List:R ,elem)))

(et-defalias Tree:R (elem) (or ,elem (:List:R (:Tree:R ,elem))))

(et-defalias Alist:R (key val) (List:R (Cons:RR ,key ,val)))

(defun et--expand-tuple (cons args)
  (if (null args) 'Nil
    (et-q (,cons ,(car args) ,(et--expand-tuple cons (cdr args))))))

(et-defalias Tuple:R (&rest args) ,(et--expand-tuple 'Cons:RR args))
(et-defalias Args (&rest args) ,(et--expand-tuple 'Cons:RR args))


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
                    ((or 'CO 'CONTRA 'ISO 'IGNORE) (make-et-matcher :generics generics :dnf arg))
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
;; \(`S:TYPE' TYPE) - an already compiled type
;; \(`S:BIND' VAR TYPE)
;; \(`S:TYPEOF' VAR)
;; \(`S:BINDS-OF' TYPE)
;; \(`S:SUBTRACT' TYPE1 TYPE2)
;; \(`S:INFER' GENERICS MATCHER TYPE Y-RESULT N-RESULT)

;; Matchers only:
;; \(`S:MATCHER-DNF' DNF) - an already compiled matcher dnf
;; \(`S:GENERIC' VAR)
;; \(`S:SET' VAR DNF)

;; Variables used for testing
(defvar et--test-variables
  (list (cons '$a (make-et-var :name '$a :type (et-dt 'Any)))
        (cons '$b (make-et-var :name '$b :type (et-dt 'Any)))
        (cons '$c (make-et-var :name '$c :type (et-dt 'Any)))))

(defun et-parse-structure (spec generics)
  (let ((case-fold-search nil)
        (parse (lambda (arg) (et-parse-structure arg generics))))

    (pcase spec
      ;; Parse a symbol
      ((pred symbolp) (et--parse-string (symbol-name spec) generics))

      ;; Parse a string
      (`(:parse ,(and str (pred stringp))) (et--parse-string str generics))
      ;; Insert a literal structure
      (`(:structure ,structure) structure)

      ;; Literal number or string
      ((or (pred stringp) (pred numberp)) (et-q (((S:DT Literal ,spec)))))

      ;; Type shortcuts
      ('(Nil) (et-q (((S:DT Literal nil)))))
      ('(True) (et-q (((S:DT Literal t)))))
      (`(Never) nil)
      (`(Any) (et-q (((S:DT Any)))))
      (`(or . ,args) (mapcan parse args))
      (`(and . ,args)
       (or args (error "`and' cannot be empty"))
       (cl-reduce #'et--dnf-and (mapcar parse args)))
      (`(literal ,val) (et-q (((S:DT Literal ,val)))))

      ;; Type utilities
      (`(bind ,var ,type) (et-q (((S:BIND ,var ,(et-parse-structure type generics))))))
      (`(typeof ,var) (et-q (((S:TYPEOF ,var)))))
      (`(bindsof ,inner) (et-q (((S:BINDS-OF ,(et-parse-structure inner generics))))))
      (`(subtract ,type1 ,type2)
       (et-q (((S:SUBTRACT ,(et-parse-structure type1 generics)
                           ,(et-parse-structure type2 generics))))))
      (`(infer ,type ,gens ,matcher ,yes ,no)
       (setq type (et-parse-structure type generics))
       (if (vectorp gens) (setq gens (append gens nil)) (error "Generics must be a vector: %s" gens))
       (setq matcher (et-parse-structure matcher gens))
       (setq yes (et-parse-structure yes (append gens generics)))
       (setq no (et-parse-structure no generics))
       (et-q (((S:INFER ,type ,gens ,matcher ,yes ,no)))))

      (`(set ,var ,type)
       (or (memq var generics) (error "Not a generic: %s" var))
       (et-q (((S:SET ,var ,(funcall parse type))))))

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
   ;; Literal number
   ((string-match "^[0-9]+\\(\\.[0-9]+\\)?$" s)
    (et-parse-structure (list 'literal (string-to-number s)) nil))

   ;; Parenthesized expression
   ((string-match "^{\\(.*\\)}$" s) (et--parse-string (substring s 1 -1) generics))

   ;; @symbol  ->  Literal symbol
   ((string-match "^@\\(.*\\)$" s)
    (et-parse-structure (list 'literal (intern (match-string 1 s))) generics))
   ;; %string  ->  Literal string
   ((string-match "^%\\(.*\\)$" s)
    (et-parse-structure (list 'literal (match-string 1 s)) generics))

   ;; $TestVar=Type  ->  Bind to TestVar
   ((string-match "^\\(\\$[a-z]\\)::\\(.*\\)$" s)
    (et-parse-structure
     (list 'bind (or (alist-get (intern (match-string 1 s)) et--test-variables)
                     (error "Invalid test variable: %s" (match-string 1 s)))
           (list :parse (match-string 2 s)))
     generics))
   ;; ::$TestVar  ->  Typeof TestVar
   ((string-match "^::\\(\\$[a-z]\\)$" s)
    (et-parse-structure
     (list 'typeof (or (alist-get (intern (match-string 1 s)) et--test-variables)
                       (error "Invalid test variable: %s" (match-string 1 s))))
     generics))

   ;; Var=Type  ->  Matcher set
   ((string-match "^\\([-a-zA-Z0-9]*\\)=\\(.*\\)$" s)
    (let ((var (intern (match-string 1 s)))
          (expr (match-string 2 s)))
      (et-parse-structure (list 'set var (list :parse expr)) generics)))

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
  (if (null structure) "Never"
    (mapconcat #'et--format-structure-case structure " | ")))

(defun et--format-structure-case (case)
  (if (null case) "Any"
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

    ((and `(Cons:RR ,left-sub ,right-sub) (guard nil))
     (let ((elems (list (et--format-structure left-sub))))
       (while (pcase right-sub
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     (`(S:DT Cons:RR ,car-sub ,cdr-sub)
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

(defun et-structure-to-type (structure &optional gen-repls)
  "Convert STRUCTURE to an `et-type'.

GEN-REPLS is an alist of symbols to `et-type's. Each time S:GENERIC
appears in STRUCTURE, it will be replaced with the corresponding value
in GEN-REPLS, if it exists.

This function will not succeed for all structures. Structures can
represent multiple types in a single case, but types do not support
this, so it will fail in this case.

Also, there are certain structure types that are designed for matchers,
which are invalid for types."

  (let* ((to-type (lambda (sub) (et-structure-to-type sub gen-repls))))
    (cl-loop for factors in structure
             collect
             (cl-loop for factor in factors
                      collect
                      ;; Return a list of cases
                      (pcase factor
                        (`(S:TYPE ,type) (apply #'list (et-type-cases type)))
                        (`(S:GENERIC ,name)
                         (et-type-cases
                          (or (alist-get name gen-repls)
                              (error "Replacement not provided for generic %s" gen-repls))))
                        (`(S:DT ,name . ,args)
                         (let ((new-args (et--datatype-map-type-args name args to-type)))
                           (list (make-et-type-case :value (make-et-datatype :name name :args new-args)))))
                        (`(S:ALIAS ,name . ,args)
                         (list (make-et-type-case
                                :value (make-et-alias
                                        :name name
                                        :args (mapcar to-type args)))))
                        (`(S:BIND ,var ,struct)
                         (list (make-et-type-case
                                :value (make-et-datatype :name 'Any)
                                :binds (list (cons var (funcall to-type struct))))))
                        (`(S:TYPEOF ,var)
                         (list (make-et-type-case :value (make-et-datatype :name 'Any) :typeofs (list var))))
                        (`(S:BINDS-OF ,struct)
                         (let* ((type (funcall to-type struct)))
                           (if (et-never-p type) nil
                             (list (make-et-type-case
                                    :value (make-et-datatype :name 'Any)
                                    :binds (et--type-binds type))))))
                        (`(S:SUBTRACT ,type1 ,type2)
                         (et-type-cases (et--subtract (funcall to-type type1)
                                                      (funcall to-type type2))))
                        (`(S:INFER ,type ,gens ,matcher ,yes ,no)
                         (setq type (funcall to-type type))
                         (setq matcher (make-et-matcher
                                        :generics gens
                                        :dnf (et-structure-to-matcher-dnf matcher gens)))
                         (let* ((result (et--sub-match matcher type)))
                           (et-type-cases
                            (if (eq result 'INVALID) (et-structure-to-type no gen-repls)
                              (let* ((new-repls (cl-loop for gen in gens
                                                         for gen-type in result
                                                         collect (cons gen gen-type))))
                                (et-structure-to-type yes (append new-repls gen-repls)))))))
                        (_ (error "Invalid structure factor for type: %s" factor)))
                      into and-case-lists
                      finally return
                      (apply #'et--supersect
                             (cl-loop for cs in and-case-lists
                                      collect (make-et-type :cases cs))))
             into or-types
             finally return (apply #'et--or or-types))))


(defun et-type-to-structure (type)
  "Convert an `et-type' to a structure DNF."
  (et--verify-type type)
  (cl-loop for case in (et-type-cases type)
           for value = (et-type-case-value case)
           collect
           (cons
            (pcase value
              ((cl-struct et-datatype name args)
               (et-ql S:DT ,(et-datatype-name value)
                      ,@(et--datatype-map-type-args name args #'et-type-to-structure)))
              ((cl-struct et-alias name args)
               (et-ql S:ALIAS ,name ,@(mapcar #'et-type-to-structure args)))
              (_ (error "Unsupported type case value: %s" value)))

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



(et-test
 (equal (et Cons:RR<1~@abc>)
        (et-type (make-et-datatype :name 'Cons:RR :args (list (et-literal 1) (et-literal 'abc)))))

 (equal (et Number)
        (et-type (make-et-datatype :name 'Number)))

 (equal (et (:structure (((S:TYPE ,(et or Abc Xyz))))))
        (et-type (make-et-alias :name 'Abc) (make-et-alias :name 'Xyz)))

 (equal (et {$a::5}&4)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Literal :args (list 4))
                  :binds (list (cons (alist-get '$a et--test-variables) (et-literal 5))))))

 (equal (et {::$a}&{$b::6}&Integer)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Integer)
                  :binds (list (cons (alist-get '$b et--test-variables) (et-literal 6)))
                  :typeofs (list (alist-get '$a et--test-variables)))))

 (equal (et bindsof<{::$a}&{$a::2|3}&{1|2}>)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Any)
                  :binds (list (cons (alist-get '$a et--test-variables) (et-literal 2))))))

 ;; Test that bindsof never is never
 (equal (et Never) (et bindsof (and Integer&{::$a} String)))

 ;; Test when a predicate is redundant (both directions)
 (et-subtype? (et or (and True (bindsof (and Integer&{::$a} String)))
                  (and Nil (bindsof (subtract Integer&{::$a} String))))
              (et Nil))
 (et-subtype? (et or (and True (bindsof (and Integer&{::$a} Number)))
                  (and Nil (bindsof (subtract Integer&{::$a} Number))))
              (et True))

 ;; Test infer
 (equal (et Never)
        (et infer Cons:RR<%hi~%hi> [T] Cons:RR<T&Integer~String> Vector:R<T> Never))
 (equal (et Vector:R<12>)
        (et infer Cons:RR<12~%hi> [T] Cons:RR<T&Integer~String> Vector:R<T> Never))
 (equal (et Vector:R<Integer>)
        (et infer Cons:RR<Integer~%hi> [T] Cons:RR<T&Integer~String> Vector:R<T> Never))
 (equal (et Never)
        (et infer Cons:RR<Number~%hi> [T] Cons:RR<T&Integer~String> Vector:R<T> Never))
 (equal (et Vector:R<12>)
        (et infer List:R<12> [T] List:R<T&Integer> Vector:R<T> Never))
 (equal (et Vector:R<1|2>)
        (et infer Cons:RR<1~Cons:RR<2~Nil>> [T] List:R<T&Integer> Vector:R<T> Never))
 (equal (et Vector:R<1|2|3>)
        (et infer Cons:RR<1~Cons:RR<2~List:R<3>>> [T] List:R<T&Integer> Vector:R<T> Never))
 (equal (et Never)
        (et infer List:R<Number> [T] List:R<T&Integer> Vector:R<T> Never))
 (equal (et Vector:R<Never>)
        (et infer Nil [T] List:R<T&Integer> Vector:R<T> Never)))


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
;;;; Union

(defun et--or (&rest types)
  "Return the exact type union of TYPES."
  (mapc #'et--verify-type types)

  (cl-loop for type in types
           nconc (apply #'list (et-type-cases type)) into cases
           finally return (make-et-type :cases cases)))


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

(et-test
 (et-subtype? (et Integer) (et Number))
 (et-subtype? (et Integer) (et Any))

 (et-subtype? (et Cons:RR<Integer~Integer>) (et Cons:RR<Number~Number>))
 (not (et-subtype? (et Cons:RR<Number~Number>) (et Cons:RR<Integer~Integer>)))

 (et-subtype? (et Cons:WW<Number~Number>) (et Cons:WW<Integer~Integer>))
 (not (et-subtype? (et Cons:WW<Integer~Integer>) (et Cons:WW<Number~Number>)))

 (et-subtype? (et Cons:WR<Number~Integer>) (et Cons:WR<Integer~Number>))
 (et-subtype? (et Cons:RW<Integer~Number>) (et Cons:RW<Number~Integer>))

 (et-subtype? (et Literal (4 . 5)) (et Cons:RR<Integer~Integer>))
 (not (et-subtype? (et Literal (4 . 5)) (et Cons:AA<Integer~Integer>)))
 (not (et-subtype? (et Literal (4 . 5.5)) (et Cons:RR<Integer~Integer>)))
 (et-subtype? (et Literal (4 . 5)) (et Cons:--<String~String>))

 (et-subtype? (et Literal [4 5 6]) (et Vector:R<Integer>))
 (not (et-subtype? (et Literal [4 5 6]) (et Vector:A<Integer>)))
 (not (et-subtype? (et Literal [4 5 6.6]) (et Vector:R<Integer>)))
 (et-subtype? (et Literal [4 5 6]) (et Vector:-<String>))

 (et-subtype? (et List:R<Integer>)
              (et Nil|Cons:RR<Number~List:R<Integer>>))

 ;; Check function subtypes
 (et-subtype? (et Function Integer Integer) (et Function Integer Integer))
 (et-subtype? (et Function Number Integer) (et Function Integer Number)))


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


;;;; Intersection

;; `et--subsect' generates a type which is a subset of ALL of the
;; provided types. In other words, it will approximate a smaller type
;; (i.e. never)

;; Thus, if v is (et--subsect A B), then v is both A and B, but but v
;; could be A and B but NOT (et--subsect A B).

;; This makes `et--subsect' useful for satisfying a list of subtype
;; constraints.

;; `et--supersect' generates a type which is a SUPERSET of the
;; intersection of the provided types. It will approximate by choosing
;; one of the types to use as the innacurate intersection.

;; Thus, if v is both A and B, then it is (et--supersect A B), but v
;; could be (et--supersect A B) but NOT be A or NOT be B.

;; This makes `et--supersect' useful for type narrowing, because unlike
;; `et--subsect', it will never make the type TOO narrow.

;; For example,
;; (et--subsect Integer Positive) -> Never
;; (et--supersect Integer Positive) -> Integer

(defun et--subsect (&rest types) (apply #'et--intersect t types))
(defun et--supersect (&rest types) (apply #'et--intersect nil types))
(defun et--non-nil (type) (et--supersect type (et NonNil)))

(et-test
 (equal (et-never) (et--subsect (et Integer) (et Positive)))
 (equal (et Integer) (et--supersect (et Integer) (et Positive))))

(defun et--intersect (subsect? &rest types)
  "Return the type intersection of TYPES."
  (mapc #'et--verify-type types)

  (pcase types
    ('nil (et-any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et--intersect subsect? a (apply #'et--intersect subsect? b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in (et-type-cases a)
              nconc
              (cl-loop for b-case in (et-type-cases b)
                       nconc (et--intersect-cases subsect? a-case b-case))
              into all-cases
              finally return
              (let ((result (make-et-type :cases all-cases)))
                ;; This assertion should pass if there are no bugs
                (when (and et-debug subsect?)
                  (or (and (et-subtype? result a) (et-subtype? result b))
                      (error "`et--subsect' determined incorrect intersection")))
                result)))))

(defun et--intersect-binds (subsect? a-binds b-binds)
  "Create a list of binds which are a subtype of both A-BINDS and B-BINDS."

  (cl-loop for (var . binds) in (seq-group-by #'car (append a-binds b-binds))
           collect (cons var (apply #'et--intersect subsect? (mapcar #'cdr binds)))))

(defun et--intersect-cases (subsect? a-case b-case)
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
             :binds (et--intersect-binds subsect? (et-type-case-binds a-case) (et-type-case-binds b-case))
             :typeofs (delete-dups (append (et-type-case-typeofs a-case)
                                           (et-type-case-typeofs b-case)
                                           nil))))))

    (cond
     (sub-val (list (funcall make-case sub-val)))

     ((et-alias-p a) (cl-loop for exp-case in (et-type-cases (et-alias-expand a))
                              nconc (et--intersect-cases subsect? exp-case b-case)))
     ((et-alias-p b) (cl-loop for exp-case in (et-type-cases (et-alias-expand b))
                              nconc (et--intersect-cases subsect? a-case exp-case)))

     ((and (et-datatype-p a) (et-datatype-p b))
      (let ((dt (et--intersect-datatypes subsect? a b)))
        (if (not (eq dt 'INVALID)) (list (funcall make-case dt))
          ;; This is where subsect and supersect differ
          (if subsect? nil
            (if (et--datatype-might-overlap-nontrivial? a b)
                (list (funcall make-case a))
              nil)))))

     (t (error "Invalid type values")))))

(defun et--intersect-datatypes (subsect? a b)
  "Returns the datatype resulting from intersecting A and B, or `INVALID'."

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
      (let ((arg-intersection
             (et--datatype-intersect-args-nontrivial
              sub-name a-args b-args
              (lambda (a b) (et--intersect subsect? a b)) #'et--or)))

        (if (eq arg-intersection 'INVALID) 'INVALID
          (make-et-datatype :name sub-name :args arg-intersection))))

     ((null sub-name) 'INVALID))))


;;;; Subtract

(defun et--subtract (a b)
  "Subtract type A from B.

This function errs on the side of subtracting less. In other words,
returning A itself is a valid approximation."
  (et--verify-type a)
  (et--verify-type b)

  (cl-loop for a-case in (et-type-cases a)
           nconc
           (cl-loop for b-case in (et-type-cases b)
                    nconc (et--subtract-cases a-case b-case))
           into all-cases
           finally return
           (let ((result (make-et-type :cases all-cases)))
             result)))

(defun et--subtract-binds (a-binds b-binds)
  (cl-loop for (var . a-type) in a-binds
           for b-type = (alist-get var b-binds)
           collect (cons var (if b-type (et--subtract a-type b-type) a-type))))

(defun et--subtract-cases (a-case b-case)
  (cl-assert (et-type-case-p a-case))
  (cl-assert (et-type-case-p b-case))

  (let* ((a (et-type-case-value a-case))
         (b (et-type-case-value b-case))
         (a-case-raw (make-et-type-case :value a))
         (b-case-raw (make-et-type-case :value b))
         (make-case
          (lambda (val)
            (make-et-type-case
             :value val
             :binds (et--subtract-binds (et-type-case-binds a-case) (et-type-case-binds b-case))
             :typeofs (et-type-case-typeofs a-case)))))

    (cond
     ;; Subtracting gives never
     ((et-subtype? (et-type a-case-raw) (et-type b-case-raw)) nil)

     ((et-alias-p a) (cl-loop for exp-case in (et-type-cases (et-alias-expand a))
                              nconc (et--subtract-cases exp-case b-case)))
     ((et-alias-p b) (cl-loop for exp-case in (et-type-cases (et-alias-expand b))
                              nconc (et--subtract-cases a-case exp-case)))

     ;; Todo: Handle more complex cases
     (t (list (funcall make-case a))))))

(et-test
 (equal (et 1|3) (et--subtract (et 1|2|3) (et 2)))
 (equal (et-never) (et--subtract (et Integer) (et Number)))
 (equal (et Number) (et--subtract (et Number) (et Integer)))
 (equal (et String|Number) (et--subtract (et String|Number) (et Integer)))
 (equal (et NonNil) (et--subtract (et NonNil) (et Integer))))


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
                     finally return (et-simplify-type (apply #'et--subsect types)))))
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


;;;; Binds utils

(defun et--remove-type-binds (type)
  "Recursively remove bindings/typeofs from TYPE."
  (cl-assert (et-type-p type))

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           collect
           (make-et-type-case
            :value
            (pcase val
              ((cl-struct et-alias name args)
               (make-et-alias :name name :args (mapcar #'et--remove-type-binds args)))
              ((cl-struct et-datatype name args)
               (let ((new-args (et--datatype-map-type-args name args #'et--remove-type-binds)))
                 (make-et-datatype :name name :args new-args)))
              (_ (error "Invalid case val: %s" val))))
           into cases
           finally return (make-et-type :cases cases)))

(defun et--type-binds (type)
  ;; binds is an alist of `et-var' to a list of types (which will be `et--or'ed)
  (let ((binds-alist nil))
    (dolist (case (et-type-cases type))
      (let* ((binds (et-type-case-binds case))
             (typeofs (et-type-case-typeofs case)))
        (dolist (var (delete-dups (append (mapcar #'car binds) typeofs nil)))
          (let* ((b-type (alist-get var binds))
                 (ti-type (when (memq var typeofs)
                            (et--remove-type-binds (et-type case)))))
            (push (if (and b-type ti-type) (et--supersect b-type ti-type)
                    (or b-type ti-type))
                  (alist-get var binds-alist))))))
    (cl-loop for (var . types) in (nreverse binds-alist)
             collect (cons var (et-simplify-type (apply #'et--or (nreverse types)))))))

(et-test
 (equal (list (cons (alist-get '$a et--test-variables) (et 2|3)))
        (et--type-binds (et $a::{2|3}&{1|2})))
 (equal (list (cons (alist-get '$a et--test-variables) (et 1|2)))
        (et--type-binds (et {::$a}&{1|2})))
 (equal (list (cons (alist-get '$a et--test-variables) (et 2)))
        (et--type-binds (et {::$a}&{$a::{2|3}}&{1|2}))))


(defun et--replace-type-binds (type binds)
  (cl-loop for case in (et-type-cases (et--remove-type-binds type))
           collect (make-et-type-case :value (et-type-case-value case) :binds binds)
           into cases
           finally return (make-et-type :cases cases)))


;;; ============================================================
;;; Provide

(provide 'et)


;;; et.el ends here
