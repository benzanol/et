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


;;; ============================================================
;;; Utils
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


;;; ============================================================
;;; Core
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
  (unless (keywordp spec)
    (error "Type must be a keyword"))
  (types--parse-string (substring (symbol-name spec) 1)))

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

   (t (error "Invalid type syntax: %s" s))))

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


;;;; Is subtype

(defun types-a-is-b (a b)
  "Can an object of type A be assigned to a variable of type B?"
  (pcase (list a b)
    (`((:Literal ,value) (:Number)) (numberp value))
    (`((:Literal ,value) (:Integer)) (and (numberp value) (eq (mod value 1) 0)))
    (`((:Literal ,value) (:String)) (stringp value))
    (`((:Literal ,value) (:Symbol)) (symbolp value))
    (`((:Literal ,value) (:Boolean)) (or (null value) (eq value t)))
    (`((:Literal ,value) (:Nil)) (null value))
    (`((:Literal ,value) (:List ,elem))
     (and (listp value) (cl-loop for e in value always (types-a-is-b `(:Literal ,e) elem))))
    (`((:Literal (,lval . ,rval)) (:Cons ,ltype ,rtype))
     (and (types-a-is-b `(:Literal ,lval) ltype)
          (types-a-is-b `(:Literal ,rval) rtype)))

    (`(,_ (:And . ,cases)) (cl-loop for case in cases always (types-a-is-b a case)))
    (`(,_ (:Or . ,cases)) (cl-loop for case in cases thereis (types-a-is-b a case)))

    ((guard (equal a b)) t)
    (`(,_ (:Any)) t)
    ('((:Integer) (:Number)) t)
    (`((:List ,ae) (:List ,ab)) (types-a-is-b ae ab))
    (`((:Cons ,al ,ar) (:Cons ,bl ,br)) (and (types-a-is-b al bl) (types-a-is-b ar br)))
    (`((:Cons ,l ,r) (:List ,elem)) (and (types-a-is-b l elem) (types-a-is-b r b)))
    ;; This one doesn't quite work, because a list could be nil
    ;; (`((:List ,elem) (:Cons ,l ,r)) (and (types-a-is-b elem l) (types-a-is-b a r)))
    ))


;;;; Flycheck

(defvar types--flycheck-path nil)

(defmacro types--error (msg &rest args)
  `(error ,(format "%s\0;;flycheck-path:%%s" msg)
          ,@args types--flycheck-path))

(defmacro types--with-path (path &rest body)
  (declare (indent 1))
  `(let ((types--flycheck-path (append types--flycheck-path ,path)))
     ,@body))


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


;;;; Checkers

(defvar types--binds nil)

(defmacro types--with-binds (binds &rest body)
  (declare (indent 1))
  `(let ((types--binds (append ,binds types--binds)))
     ,@body))


(defmacro types-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(setf (get ',expr-type 'types-checker)
         (lambda . ,(cl--transform-lambda (cons arglist body) (format "types--checker:%s" expr-type)))))

(defvar types--current-checking-expr nil)

(defun types-check (expr)
  "Returns a cons cell (TYPE . COMPILED).

TYPE is the type that EXPR was determined to be.
COMPILED is the compiled version of expr."

  (pcase expr
    (`(,func . ,args)
     (let ((types--current-checking-expr (apply #'list expr)))
       (cons (apply (or (get func 'types-checker) (types--error "No checker for function: %s" func))
                    args)
             types--current-checking-expr)))
    (_
     (cons
      (if (and (symbolp expr) expr (not (eq expr t)))
          (or (alist-get expr types--binds)
              (types--error "Free variable: %s" expr))
        (list :Literal expr))

      expr))))


;;;; Check subexpression helper

(defun types-check-subexpr (where subexpr)
  "Type check an argument of the current expression, returning the type.

Set the value of `types--flycheck-path' to the path to the
sub-expression, and calls `types-check' to do the actual checking. Then,
update `types--current-checking-expr' to use the compiled version of the
subexpression."
  (let* ((expr types--current-checking-expr)
         (_ (cl-assert (and expr (listp expr))))
         (position
          (cond
           ((numberp where) (1+ where))

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
    (let ((result (types--with-path (list position) (types-check subexpr))))
      ;; Update the current expression to use the compiled argument
      (setf (nth position types--current-checking-expr) (cdr result))
      ;; Return just the type
      (car result))))


;;;; Check a block

(defun types-check-block (offset body)
  "Returns (TYPE COMPILED-BLOCK...)."
  (cl-loop for expr in body
           for idx upfrom offset
           for (type . compiled) = (types--with-path (list idx) (types-check expr))
           for block-type = type
           collect compiled into compiled-block
           finally return (cons block-type compiled-block)))

(defmacro types-block (&rest body)
  (cons #'progn (cdr (types-check-block 1 body))))


;;;; Resolve

(defun types-resolve (type expr)
  (setq type (types-parse type))
  (let ((expr-type (car (types-check expr))))
    (or (types-a-is-b expr-type type)
        (error "Type %s is not assignable to type %s" expr-type type))))


;;; ============================================================
;;; Control flow
;;;; Let

(types-define-checker let* (varlist &rest body)
  (let ((let-binds-rev nil)
        (new-varlist-rev nil))
    ;; Process let forms
    (dolist (form varlist)
      (types--with-path (list 1 (length new-varlist-rev))
        (pcase form
          (`(,var ,type ,val)
           ;; Parse the type
           (unless (setq type (ignore-errors (types-parse type)))
             (types--with-path (list 1) (types--error "Invalid type format")))

           ;; Type-check the value
           (types--with-binds let-binds-rev
             (types--with-path (list 2)
               (let ((result (types-check val)))
                 (setq val (cdr result))
                 (or (types-a-is-b (car result) type)
                     (types--error "Type %s is not assignable to type %s" (car result) type)))))

           (push (list var val) new-varlist-rev)
           (push (cons var type) let-binds-rev))
          (_ (push form new-varlist-rev)))))

    (let ((result (types--with-binds let-binds-rev (types-check-block 2 body))))
      (setq types--current-checking-expr `(let ,(nreverse new-varlist-rev) . ,(cdr result)))
      (car result))))


;;;; Dolist

(types-define-checker dolist ((var etype lst) &rest body)
  ;; Parse the type
  (types--with-path (list 1 1)
    (setq etype (types-parse etype)))

  ;; Check if the list has the correct type
  (types--with-path (list 1 2)
    (let ((result (types-check lst)))
      (setq lst (cdr result))
      (or (types-a-is-b (car result) (list :List etype))
          (types--error "Type %s is not assignable to type %s"
                        (car result) (list :List etype)))))

  (let ((result (types--with-binds (list (cons var etype)) (types-check-block 2 body))))
    (setq types--current-checking-expr `(dolist (,var ,lst) . ,(cdr result)))
    (list :Nil)))


;;;; Setq

(types-define-checker setq (&rest args)
  (unless (eq (mod (length args) 2) 0)
    (types--with-path (list (length args))
      (types--error "Unmatched variable")))

  (cl-loop for (var val) on args by #'cddr
           for idx upfrom 0 by 2
           for type = (or (alist-get var types--binds)
                          (types--with-path (list (1+ idx))
                            (types--error "Assignment to free variable")))
           for found-type = (types-check-subexpr (1+ idx) val)
           do (or (types-a-is-b found-type type)
                  (types--with-path (list (+ 2 idx))
                    (types--error "Cannot assign %s to type %s" found-type type)))

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
           for idx upfrom 0
           for type = (types-check-subexpr idx arg)
           do (or (types-a-is-b type '(:Number))
                  (types--with-path (list (1+ idx))
                    (types--error "Argument must be a number")))
           do (setq is-integer (and is-integer (types-a-is-b type '(:Integer))))
           finally return (if is-integer (list :Integer) (list :Number))))

(types-define-checker + (&rest args) (types--check-arithmetic-function args))
(types-define-checker - (&rest args) (types--check-arithmetic-function args))
(types-define-checker * (&rest args) (types--check-arithmetic-function args))
(types-define-checker / (&rest args) (types--check-arithmetic-function args))
(types-define-checker 1+ (arg) (types--check-arithmetic-function (list arg)))
(types-define-checker 1- (arg) (types--check-arithmetic-function (list arg)))


;;;; cons

(types-define-checker cons (lval rval)
  (list :Cons
        (types-check-subexpr 0 lval)
        (types-check-subexpr 1 rval)))


;;;; list

(types-define-checker list (&rest args)
  (cl-loop with type = (list :Literal nil)
           for arg in (reverse args)
           for idx downfrom (1- (length args))
           do (setq type (list :Cons (types-check-subexpr idx arg) type))
           finally return type))


;;;; car

(types-define-checker car (expr)
  (let ((expr-type (types-check-subexpr 0 expr)))
    (pcase expr-type
      (`(:Cons ,car ,_) car)
      (`(:List ,elem) `(:Or (:Literal nil) ,elem))
      (_ (types--with-path 1 (types--error "Expected list or cons, found %s" expr-type))))))


;;;; cdr

(types-define-checker cdr (expr)
  (let ((expr-type (types-check-subexpr 0 expr)))
    (pcase expr-type
      (`(:Cons ,_ ,cdr) cdr)
      (`(:List ,elem) `(:List ,elem))
      (_ (types--with-path 1 (types--error "Expected list or cons, found %s" expr-type))))))


;;; ============================================================
;;; Provide

(provide 'types)


;;; types.el ends here
