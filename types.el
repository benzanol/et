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

(add-hook 'flycheck-process-error-functions #'types--flycheck-reposition-error)


;;;; Assert at compile type

(defmacro types-assert-error (expr)
  (condition-case _err (eval expr)
    (error nil)
    (:success (error "Expected error"))))

(defmacro types-assert-success (expr)
  (ignore (eval expr)))


;;;; Type parsing

(defun types-parse (spec)
  "Parse a type keyword SPEC into a type expression.

Syntax (within the keyword name, after the leading colon):
  Foo            → (:Foo)
  Foo<A~B>       → (:Foo (:A) (:B))
  {expr}         → (:Literal expr)
  A|B            → (`types-or' (:A) (:B))
  A&B            → (`types-and' (:A) (:B))
  A|B&C          → (`types-or' (:A) (`types-and' (:B) (:C)))

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
                    (apply #'types-and and-elements)))))
    (apply #'types-or or-elements)))

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

    (`(:Logical) "nothing")
    (`(:Logical ()) "anything")

    (`(:Logical . ,cases)
     (mapconcat
      (lambda (reqs)
        (mapconcat
         (lambda (req)
           (types-format req))
         reqs " & "))
      cases " | "))

    (`(:Bind (,var . ,_) ,type) (format "{%s: %s}" var (types-format type)))

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
    ((guard (equal a b)) t)

    ;; Logical
    (`(,_ (:Logical . ,clauses))
     ;; b is an Or of Ands - a must be subtype of at least one And-clause
     (cl-loop for clause in clauses
              thereis (cl-loop for case in clause
                               always (types-subtype? a case))))
    (`((:Logical . ,clauses) ,_)
     ;; a is an Or of Ands - every And-clause must be subtype of b
     (cl-loop for clause in clauses
              always (cl-loop for case in clause
                              thereis (types-subtype? case b))))

    ;; General
    (`(,_ (:Any)) t)
    ('((:Integer) (:Number)) t)
    (`((:List ,ae) (:List ,ab)) (types-subtype? ae ab))
    (`((:Cons ,al ,ar) (:Cons ,bl ,br)) (and (types-subtype? al bl) (types-subtype? ar br)))
    (`((:Cons ,l ,r) (:List ,elem)) (and (types-subtype? l elem) (types-subtype? r b)))

    ;; Literals
    (`((:Literal ,value) (:Number)) (numberp value))
    (`((:Literal ,value) (:Integer)) (and (numberp value) (eq (mod value 1) 0)))
    (`((:Literal ,value) (:String)) (stringp value))
    (`((:Literal ,value) (:Symbol)) (symbolp value))
    (`((:Literal ,value) (:Boolean)) (or (null value) (eq value t)))
    (`((:Literal ,value) (:List ,elem))
     (and (listp value) (cl-loop for e in value always (types-subtype? `(:Literal ,e) elem))))
    (`((:Literal (,lval . ,rval)) (:Cons ,ltype ,rtype))
     (and (types-subtype? `(:Literal ,lval) ltype)
          (types-subtype? `(:Literal ,rval) rtype)))))


;;;; Logical types

(defun types-base-nonoverlapping (type1 type2)
  (and (not (types-subtype? type1 type2))
       (not (types-subtype? type2 type1))))

(defun types--simplify-and (a b)
  "Perform an and of two non-:Logical types.

Returns a single type, or nil if the and cannot be simplified to a
single non-logical type."
  (pcase (list a b)
    ((guard (types-subtype? a b)) a)
    ((guard (types-subtype? b a)) b)
    (`((:Bind ,varspec ,a-bind) (:Bind ,varspec ,b-bind))
     `(:Bind ,varspec ,(types-and a-bind b-bind)))
    (_ nil)))

(defun types--incompatible? (a b)
  "Return non-nil if types A and B have no intersection.

This is not intended to check intersection for logical types, as it is
intended as a helper for `types-and'. To check intersection for
arbitrary types, check if `types-and' returns (:Logical), the never
type."
  (let ((data-types '(:Number :Integer :String :Symbol)))
    (pcase (list a b)
      ;; Different literals
      (`((:Literal ,a-val) (:Literal ,b-val)) (not (equal a-val b-val)))

      ;; Literal which is not an instance of a data type
      ((or `((:Literal ,val) ,other) `(,other (:Literal ,val)))
       (and (memq (car-safe other) data-types)
            (not (types-subtype? `(:Literal ,val) other))))

      ;; Incompatible data types
      ((guard (and (memq (car-safe a) data-types)
                   (memq (car-safe b) data-types)))
       (not (or (types-subtype? a b) (types-subtype? b a))))

      (_ nil))))

(defun types--simplify-requirements (reqs)
  "Simplify REQS into a simpler but equivalent requirement list.

Returns nil if any requirements are incompatible."

  ;; Check if any types are incompatible
  (when (cl-loop for (a . rest) on reqs
                 always (cl-loop for b in rest
                                 always (not (types--incompatible? a b))))

    ;; Search through every possible pair of requirements, to check
    ;; if `types--simplify-and' returns a simple merging of the
    ;; two. If it does, then replace both requirements with it.
    (cl-loop for (req . tail) on reqs
             unless (cl-loop for new-tail on new-reqs
                             for simple = (types--simplify-and req (car new-tail))
                             when simple do (setcar new-tail simple)
                             thereis simple)
             collect req into new-reqs
             finally return new-reqs)))

(defun types-and (&rest args)
  (pcase args
    ('nil `(:Logical ()))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (types-and a (apply #'types-and b c rest)))

    ;; Expand out into all possible pairings of cases, and perform
    ;; `types--simplify-requirements' on each possible pairing. Then
    ;; perform `types-or' on the resulting pairings.
    (`(,(and a (or `(:Logical . ,a-cases) (let a-cases `((,a)))))
       ,(and b (or `(:Logical . ,b-cases) (let b-cases `((,b))))))
     (types--assert-type a)
     (types--assert-type b)

     (let ((cases
            (cl-loop for ac in a-cases
                     append (cl-loop for bc in b-cases
                                     for reqs = (types--simplify-requirements (append ac bc))
                                     when reqs collect reqs))))
       (apply #'types-or (cl-loop for case in cases collect (list :Logical case)))))
    (_ (error "Should be unreachable"))))

(defun types-or (&rest args)
  (pcase args
    ('nil `(:Logical))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (types-or a (apply #'types-or b c rest)))

    (`(,(and a (or `(:Logical . ,a-cases) (let a-cases `((,a)))))
       ,(and b (or `(:Logical . ,b-cases) (let b-cases `((,b))))))
     (types--assert-type a)
     (types--assert-type b)

     (let* ((cases
             (cl-loop for (case . rest) on (seq-uniq (append a-cases b-cases))
                      unless
                      (cl-loop for c in (append new-cases rest)
                               ;; case is a subtype of c, so case is redundant
                               thereis (types-subtype? `(:Logical ,case) `(:Logical ,c)))
                      collect case into new-cases
                      finally return new-cases)))
       (pcase cases
         (`((,type)) type)
         (_ (cons :Logical cases)))))

    (_ (error "Should be unreachable"))))


;;;; Is

(defun types--is-bindings (type)
  "Given a TYPE that was truthy, return bindings it implies.

Returns a list of (VARSPEC . TYPE)."
  (types--assert-type type)

  (pcase type
    (`(:Bind ,varspec ,type)
     (list (cons varspec type)))
    (`(:Logical ,only) (mapcan #'types--is-bindings only))
    (`(:Logical ,first . ,rest)
     (let ((rest-bind-alists
            (cl-loop for case in rest
                     collect (types--is-bindings `(:Logical ,case)))))
       (cl-loop for (varspec . type) in (types--is-bindings `(:Logical ,first))
                for types = (cl-loop for entry-alist in rest-bind-alists
                                     for entry-type = (alist-get varspec entry-alist)
                                     always entry-type collect entry-type)
                when types
                collect (cons varspec (apply #'types-or type types)))))
    (_ nil)))

(defun types--is-binding-type (type)
  (apply #'types-and
         (cl-loop for (varspec . type) in (types--is-bindings type)
                  collect (list :Bind varspec type))))

(defmacro types-with-is-bindings (format type &rest body)
  (declare (indent 2))
  `(let ((binds (cl-loop for ((var . base-type) . type) in (types--is-bindings ,type)
                         collect (cons var (types-and base-type type))))
         (format ,format))
     (types-with-path (list 0)
                      (when format
                        (types-warn format
                                    (cl-loop for (var . type) in binds
                                             collect (format "%s: %s" var (types-format type)) into strs
                                             finally return (string-join strs "\\n")))))
     (types-with-binds binds ,@body)))


;;;; Exclude

(defun types-exclude (type exclude)
  "Create a subtype of TYPE with some elements of EXCLUDE removed.

This function will do its best to exclude all types, but it is not
perfect. For example, there is no type to represent (:Number) with all
of (:Integer) excluded, so it will just return (:Number).

In cases where EXCLUDE contains all elements of TYPE, return (:Logical),
representing a never type."
  (types--assert-type type)
  (types--assert-type exclude)

  (pcase type
    (`(:Logical . ,cases)
     (apply #'types-or
            (cl-loop for case in cases
                     collect (types--exclude-case case exclude))))
    (_ (types--exclude-case (list type) exclude))))

(defun types--exclude-case (reqs exclude)
  "Helper function for `types-exclude' handling a case of a :Logical."
  (pcase exclude
    (`(:Logical . ,ex-cases)
     ;; Must exclude every OR branch of exclude
     (cl-loop with acc = `(:Logical (,@reqs))
              for ex-case in ex-cases
              do (setq acc (types-exclude acc `(:Logical (,@ex-case))))
              finally return acc))
    ((guard (types-subtype? `(:Logical (,@reqs)) exclude)) `(:Logical))
    (_ (apply #'types-and reqs))))


;;;; Is valid type

(defun types--assert-type (type)
  (or (and (listp type)
           (keywordp (car type)))
      (error "Not a valid type: %s" type)))


;;; ============================================================
;;; Type
;;;; Structs

(cl-defstruct et-type cases)
(cl-defstruct et-case factors binds)

(defun et-datatype (dt) (make-et-type :cases (list (make-et-case :factors (list dt)))))
(defun et-any () (make-et-type :cases (list (make-et-case :factors nil))))
(defun et-never () (make-et-type :cases nil))

;; Each FACTOR is a DATATYPE, which is one of
;; (:Number/Integer/String/Symbol/Boolean)
;; (:Literal VALUE)
;; (:Cons LEFT RIGHT)
;; (:List ELEM)
;; (:Vector ELEM)
;; (:HashMap KEY VALUE)
;; (:Plist :KEY TYPE ...)


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
    (`((:Cons ,l ,r) (:List ,elem)) (and (et-subtype? l elem) (et-subtype? r b)))

    ;; Literals
    (`((:Literal ,value) (:Number)) (numberp value))
    (`((:Literal ,value) (:Integer)) (and (numberp value) (eq (mod value 1) 0)))
    (`((:Literal ,value) (:String)) (stringp value))
    (`((:Literal ,value) (:Symbol)) (symbolp value))
    (`((:Literal ,value) (:Boolean)) (or (null value) (eq value t)))
    (`((:Literal ,value) (:List ,elem))
     (and (listp value) (cl-loop for e in value always (et-subtype? `(:Literal ,e) elem))))
    (`((:Literal (,lval . ,rval)) (:Cons ,ltype ,rtype))
     (and (et-subtype? `(:Literal ,lval) ltype)
          (et-subtype? `(:Literal ,rval) rtype)))))


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


;;;; Exclude

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
                                      collect (et--subtract-datatype a-factor b-factor)
                                      into b-factor-results
                                      ;; union of factor subtractions = list of cases with one factor each
                                      finally return (apply #'et-or b-factor-results))
                             into b-case-results
                             ;; intersection of case subtractions
                             finally return (apply #'et-and b-case-results))
                    into a-factor-results
                    finally return (apply #'types-or a-factor-results))
           into a-case-results
           finally return (apply #'types-and a-case-results)))

(defun et--subtract-datatype (datatype remove)
  "Helper function for `types-exclude' handling datatypes.

Both DATATYPE and REMOVE are datatypes."
  (pcase (list datatype remove)
    ;; Return the never type
    ((guard (et--datatype-subtype? datatype remove)) (make-et-type :cases nil))
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
    ((guard (types-subtype? a b)) a)
    ((guard (types-subtype? b a)) b)

    ;; Merge list element
    (`((:List ,a-elem) (:List ,b-elem)) `(:List ,(types-and a-elem b-elem)))

    ;; Merge cons elements
    (`((:Cons ,a-left ,a-right) (:Cons ,b-left ,b-right))
     `(:Cons ,(types-and a-left b-left) ,(types-and a-right b-right)))

    (_ nil)))

(defun et--simplify-factors (factors)
  "Given FACTORS, return an equivalent but simplified list of factors."

  ;; Check if any types are incompatible
  (when (cl-loop for (a . rest) on factors
                 always (cl-loop for b in rest
                                 always (not (et--datatype-disjoint? a b))))

    ;; Search through every possible pair of factors to check if
    ;; `et--intersect-datatypes' can merge them. If it can, then
    ;; replace both factors with the merged factor.
    (cl-loop for (next . tail) on factors
             unless (cl-loop for new-tail on new-factors
                             for simple = (et--intersect-datatypes next (car new-tail))
                             when simple do (setcar new-tail simple)
                             thereis simple)
             collect next into new-factors
             finally return new-factors)))

(defun et-simplify (type)
  "Simplify TYPE."

  (cl-loop for (case . rest) on (et-type-cases type)
           for simple-case =
           (make-et-case :factors (et--simplify-factors (et-case-factors case))
                         :binds (et-case-binds case))
           unless
           (cl-loop for c in (append new-cases rest)
                    ;; case is a subtype of c, so case is redundant
                    thereis (et-subtype? (make-et-type :cases (list simple-case))
                                         (make-et-type :cases (list c))))
           collect simple-case into new-cases
           finally return (make-et-type :cases new-cases)))


;;;; And/or

(defun et-or (&rest types)
  (cl-loop for type in types
           append (et-type-cases type) into cases
           finally return (et-simplify (make-et-type :cases cases))))

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
                       collect (make-et-case :factors factors :binds nil))
              into all-cases
              finally return (et-simplify (make-et-type :cases all-cases))))))


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
  (pcase types--current-expr
    (`(,func . ,args)
     (or (apply (or (get func 'types-checker)
                    (error "No checker for function: %s" func))
                args)
         (error "Checker returned nil")))
    ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

     ;; Allow for type narrowing on variable names.
     ;; For example, if a: Number | nil,
     ;; then return the type Number&{a: Number} | nil
     (if-let ((varspec (assoc sym types--binds))
              (non-nil (types-exclude (cdr varspec) '(:Literal nil))))
         (if (equal non-nil (cdr varspec)) (cdr varspec)
           (types-or (types-and non-nil `(:Bind ,varspec ,non-nil))
                     (types-and (cdr varspec) `(:Literal nil) `(:Bind ,varspec (:Literal nil)))))
       (error "Free variable: %s" sym)))

    (expr (list :Literal expr))))


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
           finally return (or type `(:Literal nil))))


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


;;;; If

(types-define-checker if (cond then &optional _else)
                      (let* ((cond-type (types-check-arg 1 cond)))
                        (byte-compile-warn "%s" (types-and cond-type '(:Literal nil)))
                        (types-or
                         (types-with-is-bindings "non-nil case:\\n%s" (types-exclude cond-type '(:Literal nil))
                           (types-check-arg 2 then))
                         (types-with-is-bindings "nil case:\\n%s" (types-and cond-type '(:Literal nil))
                           (types-check-block 3)))))


(types-define-checker when (cond &rest _body)
                      (let* ((cond-type (types-check-arg 1 cond)))
                        (types-with-is-bindings "%s" (types-exclude cond-type '(:Literal nil))
                          (types-check-block 2))))

(types-define-checker unless (cond &rest _body)
                      (let* ((cond-type (types-check-arg 1 cond)))
                        (types-with-is-bindings "%s" (types-and cond-type '(:Literal nil))
                          (types-check-block 2))))


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
                          (`(:List ,elem) (types-or `(:Literal nil) elem))
                          (_ (types-with-path 1 (error "Expected list or cons, found %s" expr-type))))))


(types-define-checker cdr (expr)
                      (let ((expr-type (types-check-arg 1 expr)))
                        (pcase expr-type
                          (`(:Cons ,_ ,cdr) cdr)
                          (`(:List ,elem) `(:List ,elem))
                          (_ (types-with-path 1 (error "Expected list or cons, found %s" expr-type))))))


;;;; and/or

(defun types--and-return-type (arg-types)
  (if (null arg-types)
      `(:Literal t)
    (types-or
     ;; In the nil case, ONE of the nil bindings matched (reduce with `types-or')
     (cl-loop for arg-type in arg-types
              for type = (types-and arg-type '(:Literal nil))
              collect (types--is-binding-type type) into binds
              finally return (types-and '(:Literal nil) (apply #'types-or binds)))
     ;; In the non-nil case, ALL of the non-nil bindings matched (reduce with `types-and')
     (cl-loop for arg-type in arg-types
              for type = (types-exclude arg-type '(:Literal nil))
              collect (types--is-binding-type type) into binds
              finally return (apply #'types-and type binds)))))

(defun types--or-return-type (arg-types)
  (types-or
   ;; In the nil case, ALL of the nil bindings matched (reduce with `types-and')
   (cl-loop for arg-type in arg-types
            for type = (types-and arg-type '(:Literal nil))
            collect (types--is-binding-type type) into binds
            finally return (types-and '(:Literal nil) (apply #'types-and binds)))
   ;; In the non-nil case, ONE of the args matched (reduce with `types-or')
   (cl-loop for arg-type in arg-types
            collect (types-exclude arg-type '(:Literal nil)) into non-nil-types
            finally return (apply #'types-or non-nil-types))))

(types-define-checker and (&rest args)
                      (types--and-return-type
                       (cl-loop for arg in args
                                for idx upfrom 1
                                collect (types-check-arg idx arg))))

(types-define-checker or (&rest args)
                      (types--or-return-type
                       (cl-loop for arg in args
                                for idx upfrom 1
                                collect (types-check-arg idx arg))))


;;;; Predicates

(defmacro types-define-predicate (name type)
  `(types-define-checker ,name (expr)
                         (if (symbolp expr)
                             (let* ((varspec (or (assoc expr types--binds)
                                                 (error "Invalid variable %s" expr)))
                                    (type ',type)
                                    (nil-type (types-exclude (cdr varspec) type)))
                               `(:Logical ((:Literal t) (:Bind ,varspec ,type))
                                          ((:Literal nil) (:Bind ,varspec ,nil-type))))
                           ;; Compile the argument
                           (types-check-arg 1 expr)
                           `(:Boolean))))

(types-define-predicate stringp (:String))
(types-define-predicate numberp (:Number))
(types-define-predicate integerp (:Integer))
(types-define-predicate consp (:Cons (:Logical ()) (:Logical ())))
(types-define-predicate listp (:Logical ((:Cons (:Logical ()) (:Logical ()))) ((:Literal nil))))
(types-define-predicate null (:Literal nil))
(types-define-predicate not (:Literal nil))


;;; ============================================================
;;; Provide

(provide 'types)


;;; types.el ends here
