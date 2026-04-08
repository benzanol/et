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
;;; Type definitions
;;; ============================================================
;;; Typesystem core
;;;; Structs

(cl-defstruct et-type
  "Struct representing a root-level et type.

CASES is a list of `et-case' instances being unioned."
  cases)

(cl-defstruct et-case
  "Struct representing a case of an et type.

FACTORS is a list of datatypes which being intersected.

BINDS is an alist of (variable-symbol . type)."
  factors
  binds)

(cl-defmethod cl-print-object ((type et-type) stream)
  (princ (et-pp type) stream))

(defun et-datatype (dt) (make-et-type :cases (list (make-et-case :factors (list dt)))))
(defun et-dt (&rest dt) (make-et-type :cases (list (make-et-case :factors (list dt)))))
(defun et-any () (make-et-type :cases (list (make-et-case :factors nil))))
(defun et-never () (make-et-type :cases nil))
(defun et-literal (value) (et-datatype `(:Literal ,value)))
(defun et-nil () (et-datatype `(:Literal nil)))

(defun et-type-factors (type)
  "Expand out the factors as a list of lists."
  (cl-loop for case in (et-type-cases type)
           collect (cl-loop for factor in (et-case-factors case)
                            collect factor)))

(defun et-ever? (type)
  "Return non-nil if TYPE is not the never type."
  (> (length (et-type-cases type)) 0))

;; Each FACTOR is a DATATYPE, which is one of
;; (:Number/Integer/String/Symbol)
;; (:Literal VALUE)
;; (:Cons LEFT RIGHT)
;; (:List ELEM)


;;;; Subtype

(defun et-subtype? (a-type b-type)
  ;; For a<b, we must have a-case<b FOR ALL a cases
  (or (equal a-type b-type)
      (cl-loop for a-case in (et-type-cases a-type)
               always
               ;; For a-case<b, we must have a-factor<b FOR ANY a factor
               (cl-loop for a-factor in (et-case-factors a-case)
                        thereis
                        ;; For a-factor<b, we must have a-factor<b-case FOR ANY b case
                        (cl-loop for b-case in (et-type-cases b-type)
                                 thereis
                                 ;; For a-factor<b-case, we must have a-factor<b-factor FOR ALL b factors
                                 (cl-loop for b-factor in (et-case-factors b-case)
                                          always (et--datatype-subtype? a-factor b-factor)))))))

(defun et--datatype-subtype? (a b)
  (pcase (list a b)
    ((guard (equal a b)) t)

    ;; General
    ('((:Integer) (:Number)) t)
    (`((:List ,ae) (:List ,be)) (et-subtype? ae be))
    (`((:Cons ,al ,ar) (:Cons ,bl ,br)) (and (et-subtype? al bl) (et-subtype? ar br)))
    (`((:Cons ,l ,r) (:List ,elem))
     (and (et-subtype? l elem) (et-subtype? r (et-datatype b))))

    ;; Literals
    (`((:Literal ,value) (:Number)) (numberp value))
    (`((:Literal ,value) (:Integer)) (and (numberp value) (eq (mod value 1) 0)))
    (`((:Literal ,value) (:String)) (stringp value))
    (`((:Literal ,value) (:Symbol)) (symbolp value))
    (`((:Literal ,value) (:Boolean)) (or (null value) (eq value t)))
    (`((:Literal ,value) (:List ,elem))
     (and (listp value) (cl-loop for e in value always (et-subtype? (et-literal e) elem))))
    (`((:Literal (,lval . ,rval)) (:Cons ,ltype ,rtype))
     (and (et-subtype? (et-literal lval) ltype)
          (et-subtype? (et-literal rval) rtype)))))


;;;; Disjoint

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

(defun et--datatype-disjoint? (a b)
  (let ((repr-types '(:Number :Integer :String :Symbol :Cons)))
    (pcase (list a b)
      ;; Different literals
      (`((:Literal ,a-val) (:Literal ,b-val)) (not (equal a-val b-val)))

      ;; Literal which is not an instance of a repr type
      ((or `((:Literal ,val) ,other) `(,other (:Literal ,val)))
       (and (memq (car other) repr-types)
            (not (et--datatype-subtype? `(:Literal ,val) other))))

      ;; Repr types are either subtypes of each other (:Integer<:Number) or disjoint
      ((guard (and (memq (car a) repr-types)
                   (memq (car b) repr-types)))
       (not (or (et--datatype-subtype? a b) (et--datatype-subtype? b a)))))))


;;;; Subtract

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
                                               (et--subtract-datatype a-factor b-factor)
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

(defun et--subtract-datatype (datatype remove)
  "Helper function for `et-subtract' handling datatypes.

Both DATATYPE and REMOVE are datatypes."
  (pcase (list datatype remove)
    ;; Return the never type
    ((guard (et--datatype-subtype? datatype remove)) (et-never))
    ;; Nil removed from list is a cons
    (`((:List ,elem) (:Literal nil))
     (et-and (et-datatype datatype) (et-datatype `(:Cons ,elem ,(et-any)))))
    ;; Cons removed from list is nil
    (`((:List ,_) (:Cons ,_ ,_))
     (et-datatype `(:Literal nil)))

    (_ (et-datatype datatype))))


;;;; Simplify

(defun et--intersect-datatypes (a b)
  "Attempt to merge two datatypes into a single equivalent datatype.

Returns a single datatype, or nil if it is impossible."

  (pcase (list a b)
    ((guard (et--datatype-subtype? a b)) a)
    ((guard (et--datatype-subtype? b a)) b)

    ;; Merge list element
    (`((:List ,a-elem) (:List ,b-elem)) `(:List ,(et-and a-elem b-elem)))

    ;; Merge cons elements
    (`((:Cons ,a-left ,a-right) (:Cons ,b-left ,b-right))
     `(:Cons ,(et-and a-left b-left) ,(et-and a-right b-right)))

    (_ nil)))

(defun et--simplify-factors (factors)
  "Given FACTORS, return an equivalent but simplified list of factors."

  ;; Search through every possible pair of factors to check if
  ;; `et--intersect-datatypes' can merge them. If it can, then
  ;; replace both factors with the merged factor.
  (cl-loop for (next . tail) on factors
           unless (cl-loop for new-tail on new-factors
                           for simple = (et--intersect-datatypes next (car new-tail))
                           when simple do (setcar new-tail simple)
                           thereis simple)
           collect next into new-factors
           finally return new-factors))

(defun et-simplify (type)
  "Simplify TYPE."

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
                       for and-type = (et--replace-type-binds (et-and (cdr varspec) bind-type) nil)
                       ;; If the bind is never then remove this entire case
                       when (not (et-ever? and-type))
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

(defun et-or (&rest types)
  (cl-loop for type in types
           append (et-type-cases type) into cases
           finally return (et-simplify (make-et-type :cases cases))
           ;; finally return (make-et-type :cases cases)
           ))

(defun et-and (&rest types)
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
              finally return (et-simplify (make-et-type :cases all-cases))
              ;; finally return (make-et-type :cases all-cases)
              ))))


;;;; Parsing

(defun et-parse (spec)
  "Parse a type keyword SPEC into an `et-type'.

Syntax (within the keyword name, after the leading colon):
  Foo            → (:Foo)
  Foo<A~B>       → (:Foo (:A) (:B))
  A|B            → union of A, B
  A&B            → intersection of A, B
  A|B&C          → union of A with intersection of B,C

Operator precedence: & binds tighter than |.
Type names must match [A-Z][a-zA-Z0-9]*."
  (cond ((et-type-p spec) spec)
        ((and (listp spec) (keywordp (car spec))) (et-datatype spec))
        ((keywordp spec) (et--parse-string (substring (symbol-name spec) 1)))
        ((error "Invalid type spec: %s" spec))))

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
                    finally return (apply #'et-and and-parts))
           into or-parts
           finally return (apply #'et-or or-parts)))

(defun et--parse-atom (s)
  "Parse a single type atom into an `et-type'.

Returns an `et-type' for any of:
  {EXPR}           — grouped compound expression, parsed recursively
  nil              — (:Literal nil)
  t                — (:Literal t)
  str<STRING>      — (:Literal STRING)
  sym<SYMBOL>      — (:Literal (intern SYMBOL))
  num<NUMBER>      — (:Literal (string-to-number NUMBER))
  Name             — plain datatype like (:Number), (:String), etc.
  Name<T1~T2~...>  — generic datatype with et-type params"
  (let ((case-fold-search nil))
    (cond
     ;; {expr} — grouped/parenthesized compound expression
     ((eq (aref s 0) ?{)
      (unless (eq (aref s (1- (length s))) ?})
        (error "Unclosed brace in: %s" s))
      (et--parse-string (substring s 1 -1)))

     ;; Literal nil / t
     ((equal s "nil") (et-literal nil))
     ((equal s "t")   (et-literal t))

     ;; Any/Never
     ((equal s "Any") (et-any))
     ((equal s "Never") (et-never))

     ;; Name or Name<...>
     ((string-match "^\\([A-Za-z][a-zA-Z0-9]*\\)" s)
      (let ((name (match-string 1 s))
            (rest-start (match-end 1))
            (inner nil))
        (if (= rest-start (length s))
            ;; Plain name, no angle brackets
            (if (string-match-p "^[A-Z]" name)
                (et-datatype (list (intern (format ":%s" name))))
              (error "Type name %s must be capitalized" name))

          ;; Has <...> suffix
          (unless (eq (aref s rest-start) ?<)
            (error "Unexpected character after type name in: %s" s))
          (unless (eq (aref s (1- (length s))) ?>)
            (error "Unclosed angle bracket in: %s" s))
          (setq inner (substring s (1+ rest-start) (1- (length s))))

          ;; Lowercase prefix → literal constructor
          (if (string-match-p "^[a-z]" name)
              (pcase name
                ("sym" (et-literal (intern inner)))
                ("str" (et-literal inner))
                ("num" (et-literal (string-to-number inner)))
                (_ (error "Invalid literal type: %s" name)))

            ;; Uppercase generic: params are full et-type values
            (let ((parts (et--split-at-depth inner ?~)))
              (when (and (= (length parts) 1) (string-empty-p (car parts)))
                (error "Empty type parameters in: %s" s))
              (et-datatype
               (cons (intern (format ":%s" name))
                     (cl-loop for p in parts
                              collect (et--parse-string p)))))))))

     (t (error "Invalid type syntax: %s" s)))))

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
    (`(:Literal ,val)
     (if (and (symbolp val) val (not (eq val t)))
         (format "`%s'" val)
       (prin1-to-string val)))
    (`(:Cons ,left ,right)
     (let* ((elems (list (et-pp left))))
       (while (pcase right
                ((and (pred et-type-p) r)
                 (let ((rcases (et-type-cases r)))
                   (when (and (= (length rcases) 1)
                              (= (length (et-case-factors (car rcases))) 1)
                              (null (et-case-binds (car rcases))))
                     (let ((inner (car (et-case-factors (car rcases)))))
                       (pcase inner
                         (`(:Cons ,car-type ,cdr-type)
                          (nconc elems (list (et-pp car-type)))
                          (setq right cdr-type)
                          t)
                         (_ nil))))))))
       ;; Check if the tail is (:Literal nil)
       (let ((tail-nil-p
              (and (et-type-p right)
                   (let ((rcases (et-type-cases right)))
                     (and (= (length rcases) 1)
                          (let ((f (et-case-factors (car rcases))))
                            (and (= (length f) 1)
                                 (null (et-case-binds (car rcases)))
                                 (equal (car f) '(:Literal nil)))))))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et-pp right))))))
    (`(,(and kw (guard (keywordp kw))) . ,args)
     (let ((name (substring (symbol-name kw) 1)))
       (if args
           (format "%s<%s>" name (mapconcat #'et-pp args ", "))
         name)))
    (_ (error "Invalid datatype: %S" dt))))


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


;;;; Checkers

(defmacro et-define-checker (expr-type arglist &rest body)
  (declare (indent 2))
  (cl-assert (symbolp expr-type))
  (cl-assert (listp arglist))

  `(setf (get ',expr-type 'et-checker)
         (lambda . ,(cl--transform-lambda (cons arglist body) (format "et--checker:%s" expr-type)))))

(defun et-check ()
  "Returns the type of the current expr, if typechecking did not error."
  (pcase et--current-expr
    (`(,func . ,args)
     (or (apply (or (get func 'et-checker)
                    (error "No checker for function: %s" func))
                args)
         (error "Checker returned nil")))
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
;;; Control flow
;;;; let

(et-define-checker let* (varlist &rest _body)
  (let ((let-binds-rev nil))
    ;; Process let forms
    (cl-loop
     for form in varlist
     for idx upfrom 0
     do
     (et-with-path (list 1 idx)
       (pcase form
         (`(,var ,type ,val)
          ;; Parse the type
          (et-with-path (list 1) (setq type (et-parse type)))

          ;; Type-check the value
          (et-with-binds let-binds-rev
            (et-with-path (list 2)
              (et-resolve type)))

          (setq et--current-expr (list var val))
          (push (cons var type) let-binds-rev))
         (`(,var ,_val)
          (let ((type
                 (et-with-binds let-binds-rev
                   (et-with-path (list 1)
                     (et-check)))))
            (push (cons var type) let-binds-rev)
            (et-warn '(0) "%s: %s" var (et-pp type)))))))

    (et-with-binds let-binds-rev
      (et-check-tail 2))))


;;;; dolist

(et-define-checker dolist (spec &rest)
  (let (variable type)
    (pcase spec
      ;; With explicit type
      (`(,var ,etype ,_val)
       (et-with-path (list 1 1)
         (setq type (et-parse etype)))
       (setq variable var)
       (et-with-path (list 1 2)
         (et-resolve (et-dt :List type))))

      ;; With implicit type
      (`(,var ,_val)
       (setq variable var)
       (et-with-path (list 1 1)
         (pcase (et-type-factors (et-check))
           (`(((:List ,elem))) (setq type elem))
           (other (error "Expected list, found %s" other))))
       (et-warn '(1 0) "%s: %s" var (et-pp type)))

      (_ (error "Invalid dolist variable spec")))

    ;; Check the body
    (et-with-binds (list (cons variable type))
      (et-check-tail 2))

    (et-nil)))


;;;; setq

(et-define-checker setq (&rest args)
  (unless (eq (mod (length args) 2) 0)
    (et-with-path (list (length args))
      (error "Unmatched variable")))

  (cl-loop for (var _val) on args by #'cddr
           for idx upfrom 0 by 2
           for type = (or (et--get-var-bind var)
                          (et-with-path (list (1+ idx))
                            (error "Assignment to free variable")))
           do (et-with-path (list (+ idx 2))
                (et-resolve type))

           finally return type))


;;;; and/or

(defun et--and-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were non-nil
  (let* ((non-nil-binds (et--type-binds (et-subtract cond-type (et-nil))))
         (output-type (et-with-narrow-binds non-nil-binds (funcall checker)))

         (output-non-nil (et-subtract output-type (et-nil)))
         ;; If `and' returns non-nil, then both non-nil binds will be true (intersect them)
         (merged-non-nil-binds
          (et--intersect-binds non-nil-binds (et--type-binds output-non-nil))))

    (et-or (et--replace-type-binds output-non-nil merged-non-nil-binds)
           ;; If `and' returns nil, it could be from either `cond-type' OR `output-type' being nil
           (et-and cond-type (et-nil))
           (et-and output-type (et-nil)))))

(defun et--or-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were nil
  (let* ((nil-binds (et--type-binds (et-and cond-type (et-nil))))
         (output-type (et-with-narrow-binds nil-binds (funcall checker)))

         (output-nil (et-and output-type (et-nil)))
         ;; If `or' returns nil, then both nil binds will be true (intersect them)
         (merged-nil-binds
          (et--intersect-binds nil-binds (et--type-binds output-nil))))

    (et-or (et--replace-type-binds output-nil merged-nil-binds)
           ;; If `or' returns non-nil, it could be from either `cond-type' OR `output-type'
           (et-subtract cond-type (et-nil))
           (et-subtract output-type (et-nil)))))

(et-define-checker and (&rest args)
  (cl-loop with acc-type = (et-literal t)
           for pos upfrom 1 to (length args)
           do (cl-callf et--and-return-type acc-type
                (lambda () (et-with-path (list pos) (et-check))))
           finally return acc-type))

(et-define-checker or (&rest args)
  (cl-loop with acc-type = (et-nil)
           for pos upfrom 1 to (length args)
           do (cl-callf et--or-return-type acc-type
                (lambda () (et-with-path (list pos) (et-check))))
           finally return acc-type))


;;;; if

(et-define-checker if (_cond _then &rest _else)
  (let* ((cond-type (et-check-path 1)))

    (et-warn-narrows "non-nil:\\n%s" (et-subtract cond-type (et-nil))
                     "nil:\\n%s" (et-and cond-type (et-nil)))

    (et-or (et--and-return-type cond-type (lambda () (et-check-path 2)))
           (et--or-return-type cond-type (lambda () (et-check-tail 3))))))

(et-define-checker when (_cond &rest then)
  (let* ((cond-type (et-check-path 1)))
    (et-warn-narrows "non-nil:\\n%s" (et-subtract cond-type (et-nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et-nil)
      (et--and-return-type cond-type (lambda () (et-check-tail 2))))))

(et-define-checker unless (_cond &rest _else)
  (let* ((cond-type (et-check-path 1)))
    (et-warn-narrows "nil:\\n%s" (et-and cond-type (et-nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-check-tail 2)))))


;;; ============================================================
;;; Function types
;;;; Quoted

(et-define-checker quote (expr)
  (et-literal expr))


;;;; Arithmetic

(defun et--check-arithmetic-function (args)
  (cl-loop with is-integer = t
           for pos upfrom 1 to (length args)
           for type = (et-check-path pos)
           do (or (et-subtype? type (et-dt :Number))
                  (et-with-path (list pos)
                    (error "Argument must be a number, got %s" (et-pp type))))
           do (setq is-integer (and is-integer (et-subtype? type (et-dt :Integer))))
           finally return (et-dt (if is-integer :Integer :Number))))

(et-define-checker + (&rest args) (et--check-arithmetic-function args))
(et-define-checker - (&rest args) (et--check-arithmetic-function args))
(et-define-checker * (&rest args) (et--check-arithmetic-function args))
(et-define-checker / (&rest args) (et--check-arithmetic-function args))
(et-define-checker 1+ (arg) (et--check-arithmetic-function (list arg)))
(et-define-checker 1- (arg) (et--check-arithmetic-function (list arg)))


;;;; cons/list

(et-define-checker cons (_lval _rval)
  (et-dt :Cons
         (et-check-path 1)
         (et-check-path 2)))


(et-define-checker list (&rest args)
  (cl-loop with type = (et-literal nil)
           for idx downfrom (length args) to 1
           do (setq type (et-dt :Cons (et-check-path idx) type))
           finally return type))


;;;; car/cdr

(et-define-checker car (_expr)
  (pcase (et-type-factors (et-check-path 1))
    (`(((:Cons ,car ,_))) car)
    (`(((:List ,elem))) (et-or (et-nil) elem))
    (wrong (et-with-path 1 (error "Expected list or cons, found %s" wrong)))))


(et-define-checker cdr (_expr)
  (pcase (et-type-factors (et-check-path 1))
    (`(((:Cons ,_ ,cdr))) cdr)
    (`(((:List ,elem))) (et-dt :List elem))
    (wrong (et-with-path 1 (error "Expected list or cons, found %s" wrong)))))


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-checker ,name (_expr)
     (let* ((type ,type)
            (expr-type (et-check-path 1))
            (t-case (et-and expr-type type))
            (nil-case (et-subtract expr-type type))
            (t-type (et--replace-type-binds (et-literal t) (et--type-binds t-case)))
            (nil-type (et--replace-type-binds (et-nil) (et--type-binds nil-case))))

       (if (et-ever? t-case)
           (if (et-ever? nil-case) (et-or t-type nil-type) t-type)
         (if (et-ever? nil-case) nil-type (et-never))))))


(et-define-predicate stringp (et-dt :String))
(et-define-predicate numberp (et-dt :Number))
(et-define-predicate integerp (et-dt :Integer))
(et-define-predicate consp (et-dt :Cons (et-any) (et-any)))
;; listp does not technically check if it is a valid list
(et-define-predicate listp (et-or (et-dt nil) (et-dt :Cons (et-any) (et-any))))
(et-define-predicate null (et-nil))
(et-define-predicate not (et-nil))


;;; ============================================================
;;; Utils
;;;; Assert at compile type

(defmacro et-assert-error (expr)
  (condition-case _err (eval expr)
    (error nil)
    (:success (error "Expected error"))))

(defmacro et-assert-success (expr)
  (ignore (eval expr)))


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
