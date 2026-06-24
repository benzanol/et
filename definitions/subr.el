;;; subr.el --- Type definitions for lisp/subr.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools


;;; Commentary:

;; Type definitions for functions defined in Emacs' lisp/subr.el.


;;; Code:

(require 'et-check)


;;; ============================================================
;;; c[ad]+r accessors

;; Each composes `MatchCar'/`MatchCdr' to peel the right car/cdr path.

(et-declare
 (@function caar (x) (@generics [T]) (x (MatchCar (MatchCar T))) (@return T))
 (@function cadr (x) (@generics [T]) (x (MatchCdr (MatchCar T))) (@return T))
 (@function cdar (x) (@generics [T]) (x (MatchCar (MatchCdr T))) (@return T))
 (@function cddr (x) (@generics [T]) (x (MatchCdr (MatchCdr T))) (@return T))

 (@function caaar (x) (@generics [T]) (x (MatchCar (MatchCar (MatchCar T)))) (@return T))
 (@function caadr (x) (@generics [T]) (x (MatchCdr (MatchCar (MatchCar T)))) (@return T))
 (@function cadar (x) (@generics [T]) (x (MatchCar (MatchCdr (MatchCar T)))) (@return T))
 (@function caddr (x) (@generics [T]) (x (MatchCdr (MatchCdr (MatchCar T)))) (@return T))
 (@function cdaar (x) (@generics [T]) (x (MatchCar (MatchCar (MatchCdr T)))) (@return T))
 (@function cdadr (x) (@generics [T]) (x (MatchCdr (MatchCar (MatchCdr T)))) (@return T))
 (@function cddar (x) (@generics [T]) (x (MatchCar (MatchCdr (MatchCdr T)))) (@return T))
 (@function cdddr (x) (@generics [T]) (x (MatchCdr (MatchCdr (MatchCdr T)))) (@return T)))

(et-test
 (et-assert-resolve 2 (cadr '(1 2 3)))
 (et-assert-resolve 3 (caddr '(1 2 3)))
 (et-assert-resolve 0 (cadar '((-1 0) 1 2 3)))
 (et-assert-resolve List<3> (cdddr '((-1 0) 1 2 3))))


;;; ============================================================
;;; Association lists

(et-declare
 (@function alist-get (key alist)
            (@generics [V])
            (key Any) (alist ListR<ConsR<Any~V>>)
            (@return V|Nil)))

(et-test
 (et-assert-call-errors alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)
 (et-assert-call 2|Nil alist-get Integer AList<1~2>)
 (et-assert-resolve 2|4|Nil (alist-get 4 (list (cons 1 2) (cons 3 4)))))


;;; ============================================================
;;; List utilities

(et-declare
 (@function delete-dups (list)
            (@generics [T]) (list ListR<T>) (@return List<T>))
 (@function last (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListR<T>))
 (@function butlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListFresh<T>))
 (@function nbutlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListR<T>))
 (@function flatten-tree (tree)
            (@generics [T]) (tree TreeR<T>) (@return ListFresh<T>))
 (@function number-sequence (from &optional to inc)
            (from Number) (to Number) (inc Number) (@return ListFresh<Number>))

 ;; `copy-tree' deeply freshens its argument's structure.
 (@function copy-tree (tree &optional vecp)
            (@generics [T]) (tree T) (vecp Any)
            (@return (eval et--freshen-type T))))

(et-test
 (et-assert-call List<Integer> delete-dups ListR<Integer>)
 (et-assert-call ListR<Integer> last ListR<Integer>)
 (et-assert-call ListFresh<Integer> butlast ListR<Integer>)
 (et-assert-call ListFresh<Integer> flatten-tree TreeR<Integer>)
 (et-assert-call ListFresh<Number> number-sequence Integer Integer))

(et-test
 ;; copy-tree freshens deeply (compared by equivalence, not raw `equal').
 (let ((got (et-result-value (et-typecheck-call copy-tree (TupleR Cons<1~2> Cons<3~4>))))
       (want (et ConsFresh<ConsFresh<1~2>~ConsFresh<ConsFresh<3~4>~Nil>>)))
   (and (et-subtype? got want) (et-subtype? want got))))


;;; ============================================================
;;; Symbols and errors

(et-declare
 (@function gensym (&optional prefix)
            (prefix String) (@return Var))
 (@function error (string &rest args)
            (string String) (args ListR<Any>) (@return Never)))

(et-test
 (et-assert-resolve Var (gensym))
 (et-assert-resolve Var (gensym "pre"))
 (et-assert-resolve Never (error "boom %d" 1))
 (et-assert-resolve-errors (gensym 5)))


;;; ============================================================
;;; Strings

(et-declare
 (@function string-prefix-p (prefix string &optional ignore-case)
            (prefix String) (string String) (ignore-case Any) (@return Boolean))
 (@function string-suffix-p (suffix string &optional ignore-case)
            (suffix String) (string String) (ignore-case Any) (@return Boolean))
 (@function string-trim (string &optional trim-left trim-right)
            (string String) (trim-left String) (trim-right String) (@return String))
 (@function string-replace (from-string to-string in-string)
            (from-string String) (to-string String) (in-string String) (@return String))
 (@function split-string (string &optional separators omit-nulls trim)
            (string String) (separators String) (omit-nulls Any) (trim String)
            (@return ListFresh<String>)))

(et-test
 (et-assert-resolve Boolean (string-prefix-p "a" "abc"))
 (et-assert-resolve Boolean (string-suffix-p "c" "abc"))
 (et-assert-resolve String (string-trim "  hi  "))
 (et-assert-resolve String (string-replace "a" "b" "abc"))
 (et-assert-resolve ListFresh<String> (split-string "a b c"))
 (et-assert-resolve-errors (string-trim 5)))


;;; ============================================================
;;; Macros

(et-declare
 (@macro if-let* :expand t)
 (@macro if-let :expand t)
 (@macro when-let :expand t)
 (@macro when-let* :expand t))


;;; ============================================================
;;; Control-flow macros

;; `dolist'/`when'/`unless' are macros defined in lisp/subr.el. They
;; carry runtime logic (binding a loop variable, narrowing the
;; condition), so they are written as checkers rather than `@function'
;; declarations. `when'/`unless' reuse the narrowing helpers
;; `et--and-return-type'/`et--or-return-type' that live with `and'/`or'
;; in eval.c.el.

(et-define-pcase-checker dolist
    `(,(or (and form `(,name ,elem-spec ,_lst)
                (let elem-type (et-parse-type elem-spec))
                (let _1 (setcdr form (cddr form)))
                (let _2 (et-checker-resolve (et-alias 'ListR elem-type) 1 1)))
           (and `(,name ,_lst)
                (let elem-type (et-checker-infer (et-checker-sub 1 1) [T] ListR<T> T))))
      . ,_body)
  (et-with-vars (list (et-new-var name elem-type))
    (et-checker-sub 2)))

(et-define-pcase-checker when `(,_cond . ,then)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "WHEN:\\n%s" (et--non-nil cond-type))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et Nil)
      (et--and-return-type cond-type (lambda () (et-checker-tail 2))))))

(et-define-pcase-checker unless `(,_cond . ,_else)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "UNLESS:\\n%s" (et--supersect cond-type (et Nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-checker-tail 2)))))

(et-test
 (equal (et String|Nil)
        (et--remove-type-binds
         (et-typecheck
          (let* ((a String|Number 4))
            (when (stringp a) a))))))


(provide 'subr)
;;; subr.el ends here
