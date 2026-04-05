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

(require 'flycheck)
(require 'seq)


;;; ============================================================
;;; Utils
;;;; Assert at compile type

(defmacro types-assert-error (expr)
  (condition-case _err (eval expr)
    (error nil)
    (:success (error "Expected error"))))

(defmacro types-assert-success (expr)
  (ignore (eval expr)))


;;;; Errored "or"

(defmacro types-or (&rest forms)
  `(or ,@(cl-loop for tail on forms
                  if (eq (length tail) 1)
                  collect (car tail)
                  else
                  collect `(ignore-errors (or ,(car tail) t)))))


;;;; Flycheck move error

(defun types--flycheck-reposition-error (err)
  "If ERR has a ;;flycheck-path: sentinel, reposition it."
  (with-demoted-errors "Error in reposition: %s"
    (when-let* ((msg (flycheck-error-message err))
                (match (string-match "\0;;flycheck-path:\\((.*)\\)" msg))
                (path (car (read-from-string (match-string 1 msg))))
                (prev-start t))

      ;; Strip the sentinel from the displayed message
      (setf (flycheck-error-message err)
            (substring msg 0 match))
      (if (eq (flycheck-error-level err) 'warning)
          (setf (flycheck-error-level err) 'info))

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


;;;; Type parsing

(defun types-parse (spec)
  "Parse a type keyword SPEC into a type expression.

Syntax (within the keyword name, after the leading colon):
  Foo            → (:Foo)
  Foo<A~B>       → (:Foo (:A) (:B))
  {expr}         → (:Literal expr)
  A|B            → (:Or (:A) (:B))
  A&B            → (:And (:A) (:B))
  A|B&C          → (:Or (:A) (:And (:B) (:C)))

Operator precedence: & binds tighter than |.
Type names must match [A-Z][a-zA-Z0-9]*."
  (if (and (listp spec) (keywordp (car spec)))
      spec

    (unless (keywordp spec)
      (error "Type must be a keyword"))
    (types--parse-string (substring (symbol-name spec) 1))))

(defun types--parse-string (s)
  "Parse type string S (no leading colon).
Splits on | at depth 0, then & at depth 0, then parses atoms."
  (when (string-empty-p s)
    (error "Empty type expression"))

  (let ((or-elements
         (cl-loop for or-seg in (types--split-at-depth s ?|)
                  when (string-empty-p or-seg)
                  do (error "Empty segment in union type: %s" s)
                  collect
                  (let* ((and-elements
                          (cl-loop for and-seg in (types--split-at-depth or-seg ?&)
                                   when (string-empty-p and-seg)
                                   do (error "Empty segment in intersection type: %s" s)
                                   collect (types--parse-atom and-seg))))
                    (if (cdr and-elements) (cons :And and-elements) (car and-elements))))))
    (if (cdr or-elements) (cons :Or or-elements) (car or-elements))))

(defun types--parse-atom (s)
  "Parse a single type atom, or error.

S can have one of the following forms:
- {TYPE}             -> (types--parse-type TYPE)
- Name               -> (:Name)
- Name<T1~T2~...~TN> -> (:Name (types--parse-string T1) ...)
- nil                -> (:Literal nil)
- t                  -> (:Literal t)
- str<STRING>        -> (:Literal STRING)
- sym<SYMBOL-NAME>   -> (:Literal (intern SYMBOL-NAME))
- num<NUMBER>        -> (:Literal (string-to-number NUMBER))

An atom is a parenthesized {type}, generic Name<A~B>, or plain Name."
  (let ((case-fold-search nil))
    (cond
     ;; {type}
     ((eq (aref s 0) ?{)
      (unless (eq (aref s (1- (length s))) ?})
        (error "Unclosed literal brace in: %s" s))
      (types--parse-string (substring s 1 -1)))

     ((equal s "nil") (list :Literal nil))
     ((equal s "t") (list :Literal t))

     ;; Name or Name<...>
     ((string-match "^\\([A-Za-z][a-zA-Z0-9]*\\)" s)
      (let ((name (match-string 1 s))
            (rest-start (match-end 1))
            (inner nil))
        (if (= rest-start (length s))
            (if (string-match-p "^[A-Z]" name)
                (list (intern (format ":%s" name)))
              (error "Type name %s must be capitalized" name))

          (unless (eq (aref s rest-start) ?<)
            (error "Unexpected character after type name in: %s" s))
          (unless (eq (aref s (1- (length s))) ?>)
            (error "Unclosed angle bracket in: %s" s))
          (setq inner (substring s (1+ rest-start) (1- (length s))))

          ;; If it is lowercase, it represents a literal
          (if (string-match-p "^[a-z]" name)
              (pcase name
                ('"sym" (list :Literal (intern inner)))
                ('"str" (list :Literal inner))
                ('"num" (list :Literal (string-to-number inner)))
                (_ (error "Invalid literal type: %s" name)))

            (let ((parts (types--split-at-depth inner ?~)))
              (when (and (= (length parts) 1) (string-empty-p (car parts)))
                (error "Empty type parameters in: %s" s))
              (cons (intern (format ":%s" name))
                    (cl-loop for p in parts
                             collect (types--parse-string p))))))))

     (t (error "Invalid type syntax: %s" s)))))

(defun types--split-at-depth (s delim)
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


;;;; Type printing

(defun types-format (type)
  "Format a type expression back into a human-readable string."
  (pcase type
    (`(:Literal ,val)
     (if (and (symbolp val) val (not (eq val t)))
         (format "`%s'" val)
       (prin1-to-string val)))

    (`(:Cons ,elem ,tail)
     (let* ((elems (list (types-format elem))))
       (while (pcase tail
                (`(:Cons ,tail-car ,tail-cdr)
                 (nconc elems (list (types-format tail-car)))
                 (setq tail tail-cdr)
                 t)))
       (unless (equal tail '(:Literal nil))
         (setcdr (last elems) (types-format tail)))
       (format "%s" elems)))

    (`(:Or . ,elts)
     (mapconcat #'types-format elts " | "))
    (`(:And . ,elts)
     (mapconcat
      (lambda (e)
        (if (eq (car-safe e) :Or)
            (format "(%s)" (types-format e))
          (types-format e)))
      elts " & "))
    (`(,(and kw (guard (keywordp kw))) . ,args)
     (let ((name (substring (symbol-name kw) 1)))
       (if args
           (format "%s<%s>" name (mapconcat #'types-format args ", "))
         name)))
    (_ (error "Invalid type expression: %S" type))))


;;;; Is subtype

(defun types-subtype? (a b)
  "Can an object of type A be assigned to a variable of type B?"
  (pcase (list a b)
    (`((:Literal ,value) (:Number)) (numberp value))
    (`((:Literal ,value) (:Integer)) (and (numberp value) (eq (mod value 1) 0)))
    (`((:Literal ,value) (:String)) (stringp value))
    (`((:Literal ,value) (:Symbol)) (symbolp value))
    (`((:Literal ,value) (:Boolean)) (or (null value) (eq value t)))
    (`((:Literal ,value) (:Nil)) (null value))
    (`((:Literal ,value) (:List ,elem))
     (and (listp value) (cl-loop for e in value always (types-subtype? `(:Literal ,e) elem))))
    (`((:Literal (,lval . ,rval)) (:Cons ,ltype ,rtype))
     (and (types-subtype? `(:Literal ,lval) ltype)
          (types-subtype? `(:Literal ,rval) rtype)))

    (`(,_ (:And . ,cases)) (cl-loop for case in cases always (types-subtype? a case)))
    (`(,_ (:Or . ,cases)) (cl-loop for case in cases thereis (types-subtype? a case)))

    (`((:And . ,cases) ,_) (cl-loop for case in cases thereis (types-subtype? a case)))
    (`((:Or . ,cases) ,_) (cl-loop for case in cases always (types-subtype? case b)))

    ((guard (equal a b)) t)
    (`(,_ (:Any)) t)
    ('((:Integer) (:Number)) t)
    (`((:List ,ae) (:List ,ab)) (types-subtype? ae ab))
    (`((:Cons ,al ,ar) (:Cons ,bl ,br)) (and (types-subtype? al bl) (types-subtype? ar br)))
    (`((:Cons ,l ,r) (:List ,elem)) (and (types-subtype? l elem) (types-subtype? r b)))
    ;; This one doesn't quite work, because a list could be nil
    ;; (`((:List ,elem) (:Cons ,l ,r)) (and (types-subtype? elem l) (types-subtype? a r)))
    ))


;;;; Simplify type

(defun types--simplify (type)
  (pcase type
    (`(:Or ,single) single)
    (`(:And ,single) single)

    (`(:Or . ,cases)
     (setq cases (cl-loop for case in cases
                          nconc (pcase case
                                  (`(:Or . ,inner) inner)
                                  (_ (list case)))))
     (setq cases (seq-uniq cases))
     (if (cdr cases) (cons :Or cases) (car cases)))

    (`(:And . ,cases)
     (setq cases (cl-loop for case in cases
                          nconc (pcase case
                                  (`(:And . ,inner) inner)
                                  (_ (list case)))))
     (setq cases (seq-uniq cases))
     ;; Remove all cases which are a subtype of another case (redundant)
     (setq cases
           (cl-loop for (case . rest) on cases
                    unless (cl-loop for c in (append new-cases rest)
                                    thereis (types-subtype? case c))
                    collect case into new-cases
                    finally return new-cases))

     (if (cdr cases) (cons :And cases) (car cases)))

    (_ type)))


;;; ============================================================
;;; Checking
;;;; Global variables

(defvar types--current-expr nil)
(defvar types--current-path nil)

(defun types--error-advice (error string &rest args)
  (if types--current-path
      (apply error (format "%s\0;;flycheck-path:%s" string types--current-path)
             args)
    (apply error string args)))

(advice-add #'error :around #'types--error-advice)

(defun types-warn (msg &rest args)
  (setq msg (format "%s\0;;flycheck-path:%s" msg types--current-path))
  (apply #'byte-compile-warn msg args))

(defmacro types--label-errors (expr)
  `(condition-case err ,expr
     (error
      (let ((str (error-message-string err)))
        (if (string-match-p "\0;;flycheck-path:([0-9 ]*)\\'" str)
            (error str)
          (error (format "%s\0;;flycheck-path:%s" str types--current-path)))))))

(defun types--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (types--traverse-tree (cdr path) (nth (car path) tree))))

(defmacro types-with-path (path &rest body)
  (declare (indent 1))
  (let ((path-var (make-symbol "path"))
        (parent-var (make-symbol "parent"))
        (expr-var (make-symbol "expr")))
    `(let* ((,path-var ,path)
            (,parent-var (types--traverse-tree (butlast ,path-var) types--current-expr))
            (,expr-var (nth (car (last ,path-var)) ,parent-var))
            (types--current-path (append types--current-path ,path-var))
            (types--current-expr ,expr-var))
       ;; (types--label-errors
       (prog1 (progn ,@body)
         (unless (eq types--current-expr ,expr-var)
           (setf (nth (car (last ,path-var)) ,parent-var)
                 types--current-expr)))
       ;; )
       )))

(defvar types--binds nil)

(defmacro types-with-binds (binds &rest body)
  (declare (indent 1))
  `(let ((types--binds (append ,binds types--binds)))
     ,@body))

(defmacro types--root (expr &rest body)
  (declare (indent 1))
  `(progn
     (cl-assert (null types--current-expr))
     (cl-assert (null types--current-path))
     (cl-assert (null types--binds))
     (let ((types--current-expr ,expr))
       ,@body)))


;;;; Checkers

(defmacro types-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(setf (get ',expr-type 'types-checker)
         (lambda . ,(cl--transform-lambda (cons arglist body) (format "types--checker:%s" expr-type)))))

(defun types-check ()
  "Returns the type of the current expr, if typechecking did not error."
  (types--simplify
   (pcase types--current-expr
     (`(,func . ,args)
      (or (apply (or (get func 'types-checker)
                     (error "No checker for function: %s" func))
                 args)
          (error "Checker returned nil")))
     ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))
      (or (alist-get sym types--binds)
          (error "Free variable: %s" sym)))
     (expr (list :Literal expr)))))


;;;; Check subexpression helper

(defun types-check-arg (where subexpr)
  "Type check an argument of the current expression, returning the type."
  (cl-assert (and types--current-expr (listp types--current-expr)))
  (let* ((expr types--current-expr)
         (position
          (cond
           ((numberp where) where)

           ((keywordp where)
            ;; This method for calculating the position could technically
            ;; fail Suppose a function has arglist (arg1 arg2 &key mykey),
            ;; and someone calls (func :mykey 7 :mykey 7). The position for
            ;; where=:mykey and subexpr=7 would be determined to be 2,
            ;; rather than the correct position of 4. A fully correct
            ;; strategy would require parsing the function arglist.
            (or (cl-loop for tail on (cdr expr)
                         for idx upfrom 1
                         when (and (eq (car tail) where) (eq (cadr tail) subexpr))
                         do (cl-return (1+ idx)))
                (error "Keyword argument %s with value %s not found" where subexpr)))

           (t (error "WHERE must be either an argument index or keyword")))))

    (cl-assert (eq (nth position expr) subexpr))
    (types-with-path (list position)
      (types-check))))


;;;; Check a block

(defun types-check-block (start)
  (cl-loop for idx upfrom start below (length types--current-expr)
           for type = (types-with-path (list idx) (types-check))
           finally return type))


;;;; Root level functions

(defmacro types-root-block (&rest body)
  (types--root (cons #'progn body)
    (types-check-block 1)
    types--current-expr))

(defun types-root-check (expr)
  (types--root expr (types-check)))

(defun types-resolve (type)
  (setq type (types-parse type))

  (let ((expr-type (types-check)))
    (unless (types-subtype? expr-type type)
      (error "Type %s is not assignable to type %s"
             (types-format expr-type) (types-format type)))))

(defun types-root-resolve (type expr)
  (types--root expr (types-resolve type)))


;;; ============================================================
;;; Control flow
;;;; Let

(types-define-checker let* (varlist &rest _body)
  (let ((let-binds-rev nil))
    ;; Process let forms
    (cl-loop
     for form in varlist
     for idx upfrom 0
     do
     (types-with-path (list 1 idx)
       (pcase form
         (`(,var ,type ,val)
          ;; Parse the type
          (types-with-path (list 1) (setq type (types-parse type)))

          ;; Type-check the value
          (types-with-binds let-binds-rev
            (types-with-path (list 2)
              (types-resolve type)))

          (setq types--current-expr (list var val))
          (push (cons var type) let-binds-rev))
         (`(,var ,_val)
          (let ((type
                 (types-with-binds let-binds-rev
                   (types-with-path (list 1)
                     (types-check)))))
            (push (cons var type) let-binds-rev)
            (types-with-path (list 0)
              (types-warn "%s: %s" var (types-format type))))))))

    (types-with-binds let-binds-rev
      (types-check-block 2))))


;;;; Dolist

(types-define-checker dolist (spec &rest)
  (let (variable type)
    (pcase spec
      ;; With explicit type
      (`(,var ,etype ,_val)
       (types-with-path (list 1 1)
         (setq type (types-parse etype)))
       (setq variable var)
       (types-with-path (list 1 2)
         (types-resolve `(:List ,type))))

      ;; With implicit type
      (`(,var ,_val)
       (setq variable var)
       (types-with-path (list 1 1)
         (pcase (types-check)
           (`(:List ,elem) (setq type elem))
           (other (error "Expected list, found %s" other))))
       (types-with-path (list 1 0)
         (types-warn "%s: %s" var (types-format type))))

      (_ (error "Invalid dolist variable spec")))

    ;; Check the body
    (types-with-binds (list (cons variable type))
      (types-check-block 2))

    (list :Nil)))


;;;; Setq

(types-define-checker setq (&rest args)
  (unless (eq (mod (length args) 2) 0)
    (types-with-path (list (length args))
      (error "Unmatched variable")))

  (cl-loop for (var _val) on args by #'cddr
           for idx upfrom 0 by 2
           for type = (or (alist-get var types--binds)
                          (types-with-path (list (1+ idx))
                            (error "Assignment to free variable")))
           do (types-with-path (list (+ idx 2))
                (types-resolve type))

           finally return type))


;;; ============================================================
;;; Types
;;;; Quoted

(types-define-checker quote (expr)
  (list :Literal expr))


;;;; Arithmetic

(defun types--check-arithmetic-function (args)
  (cl-loop with is-integer = t
           for arg in args
           for idx upfrom 1
           for type = (types-check-arg idx arg)
           do (or (types-subtype? type '(:Number))
                  (types-with-path (list idx)
                    (error "Argument must be a number, got %s" type)))
           do (setq is-integer (and is-integer (types-subtype? type '(:Integer))))
           finally return (if is-integer (list :Integer) (list :Number))))

(types-define-checker + (&rest args) (types--check-arithmetic-function args))
(types-define-checker - (&rest args) (types--check-arithmetic-function args))
(types-define-checker * (&rest args) (types--check-arithmetic-function args))
(types-define-checker / (&rest args) (types--check-arithmetic-function args))
(types-define-checker 1+ (arg) (types--check-arithmetic-function (list arg)))
(types-define-checker 1- (arg) (types--check-arithmetic-function (list arg)))


;;;; cons/list

(types-define-checker cons (lval rval)
  (list :Cons
        (types-check-arg 1 lval)
        (types-check-arg 2 rval)))


(types-define-checker list (&rest args)
  (cl-loop with type = (list :Literal nil)
           for arg in (reverse args)
           for idx downfrom (length args)
           do (setq type (list :Cons (types-check-arg idx arg) type))
           finally return type))


;;;; car/cdr

(types-define-checker car (expr)
  (let ((expr-type (types-check-arg 1 expr)))
    (pcase expr-type
      (`(:Cons ,car ,_) car)
      (`(:List ,elem) `(:Or (:Literal nil) ,elem))
      (_ (types-with-path 1 (error "Expected list or cons, found %s" expr-type))))))


(types-define-checker cdr (expr)
  (let ((expr-type (types-check-arg 1 expr)))
    (pcase expr-type
      (`(:Cons ,_ ,cdr) cdr)
      (`(:List ,elem) `(:List ,elem))
      (_ (types-with-path 1 (error "Expected list or cons, found %s" expr-type))))))


;;;; and/or

(types-define-checker and (&rest args)
  (if (null args)
      '(:Literal t)
    (cl-loop for arg in args
             for idx upfrom 1
             for type = (types-check-arg idx arg)
             finally return type)))

(types-define-checker or (&rest args)
  (if (null args)
      '(:Literal nil)
    (let ((types (cl-loop for arg in args
                          for idx upfrom 1
                          collect (types-check-arg idx arg))))
      ;; For all but the last type, strip (:Literal nil) from :Or unions,
      ;; since a non-nil value would have short-circuited to return that value.
      (let ((stripped
             (cl-loop for type in (butlast types)
                      collect
                      (pcase type
                        (`(:Or . ,cases)
                         (let ((non-nil (cl-remove '(:Literal nil) cases :test #'equal)))
                           (pcase non-nil
                             (`(,single) single)
                             (_ (cons :Or non-nil)))))
                        (_ type))))
            (last-type (car (last types))))
        (let ((all-types (append stripped (list last-type))))
          (if (cl-every (lambda (type) (equal type (car all-types))) (cdr all-types))
              (car all-types)
            (cons :Or all-types)))))))


;;; ============================================================
;;; Provide

(provide 'types)


;;; types.el ends here
