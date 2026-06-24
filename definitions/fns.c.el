;;; fns.c.el --- Type definitions for src/fns.c -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools


;;; Commentary:

;; Type definitions for builtins defined in Emacs' src/fns.c.


;;; Code:

(require 'et-check)


;;; ============================================================
;;; List access

(et-declare
 (@function nth (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return T|Nil))
 (@function nthcdr (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListR<T>))
 (@function take (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListFresh<T>))
 (@function ntake (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListR<T>))
 (@function length (sequence)
            (sequence String|ListR<Any>|VectorR<Any>) (@return Integer))
 (@function safe-length (list) (list Any) (@return Integer))
 (@function proper-list-p (object) (object Any) (@return Integer|Nil)))

(et-test
 (et-assert-call Number|String|Nil nth Integer ConsR<Number~ListR<String>>)
 (et-assert-call ListR<Number|String> nthcdr Integer ConsR<Number~ListR<String>>)
 (et-assert-call ListR<Never> nthcdr Integer Nil)
 (et-assert-call ListFresh<Integer> take Integer ListR<Integer>)
 (et-assert-call ListR<Integer> ntake Integer ListR<Integer>))

(et-test
 (et-assert-call Integer length VectorR<Number>|ListR<String>)
 (et-assert-call-errors length VectorR<Number>|ListR<String>|Number)
 (et-assert-call Integer safe-length ListR<Any>)
 (et-assert-call Integer|Nil proper-list-p ListR<Any>))


;;; ============================================================
;;; Membership and association lists

(et-declare
 (@function memq (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>|Nil))
 (@function member (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>|Nil))
 (@function delq (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>))
 (@function delete (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>))

 (@function assq (key alist)
            (@generics [C]) (key Any) (alist ListR<C&Cons>) (@return C|Nil))
 (@function assoc (key alist)
            (@generics [C]) (key Any) (alist ListR<C&Cons>) (@return C|Nil))
 (@function rassq (value alist)
            (@generics [C]) (value Any) (alist ListR<C&Cons>) (@return C|Nil))
 (@function rassoc (value alist)
            (@generics [C]) (value Any) (alist ListR<C&Cons>) (@return C|Nil)))

(et-test
 (et-assert-call ListR<Integer>|Nil memq Any ListR<Integer>)
 (et-assert-call ListR<String>|Nil member Any ListR<String>)
 (et-assert-call ListR<Integer> delq Any ListR<Integer>)
 (et-assert-resolve Cons<1~2>|Nil (assq 1 (list (cons 1 2))))
 (et-assert-resolve Cons<1~2>|Nil (rassq 2 (list (cons 1 2)))))


;;; ============================================================
;;; Mapping

(et-declare
 (@function mapcar (function sequence)
            (@generics [T R])
            (function Function<Args<T>~R>)
            (sequence ListR<T>)
            (@return ListFresh<R>))
 (@function mapc (function sequence)
            (@generics [T R])
            (function Function<Args<T>~R>)
            (sequence ListR<T>)
            (@return Nil))
 (@function mapconcat (function sequence &optional separator)
            (@generics [T])
            (function Function<Args<T>~String>)
            (sequence ListR<T>)
            (separator String)
            (@return String)))

(et-test
 (et-assert-call ListFresh<String> mapcar Function<Args<Integer>~String> ListR<Integer>)
 (et-assert-call Nil mapc Function<Args<Integer>~String> ListR<Integer>)
 (et-assert-call String mapconcat Function<Args<Integer>~String> ListR<Integer>)
 (et-assert-call String mapconcat Function<Args<Integer>~String> ListR<Integer> String)
 (et-assert-call-errors mapcar Function<Args<String>~String> ListR<Integer>))


;;; ============================================================
;;; append / nconc

;; `append' builds a fresh spine for every argument except the last,
;; which it shares (so its element types stay covariant). `append's
;; `@return' resolves `et--append-return-type' lazily (at check time, via
;; the `(eval ...)' type form below), so the helper only needs to be
;; bound by then -- not when this declaration is parsed.
(defun et--append-return-type (input-type)
  (or (et-checker-infer input-type [] Nil Nil)
      (et-checker-infer input-type [S] (ConsR S Nil) S)
      (et-checker-infer input-type [E R] (ConsR List<E> R)
                        (AppendFresh E (eval et--append-return-type R)))))

(et-declare
 (@alias AppendFresh [E R] (or R (ConsFresh E (AppendFresh E R))))

 (@function append (&rest sequences)
            (@generics [A])
            (sequences A)
            (@return (eval et--append-return-type A)))

 (@function nconc (&rest lists)
            (@generics [E])
            (lists ListR<List<E>>)
            (@return List<E>)))

(et-test
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] List<T>)
          (et-result-value (et-typecheck-call append List<1> List<Integer> List<Number>)))))

 ;; A List<Integer> tail cannot be widened to List<Number>: nconc-ing a
 ;; 0.5 onto the end would violate the Integer tail. Adding Nil or
 ;; inferring a ListR makes it valid (see following cases).
 (not (et-match-result-success
       (et-sub-match
        (et-matcher [T] List<T>)
        (et-result-value (et-typecheck-call append List<1> List<Number> List<Integer>)))))
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] List<T>)
          (et-result-value (et-typecheck-call append List<1> List<Number> List<Integer> Nil)))))
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] ListR<T>)
          (et-result-value (et-typecheck-call append List<1> List<Integer> List<Number> List<1>))))))


;;; ============================================================
;;; reverse / nreverse / copy

(et-declare
 (@function reverse (sequence)
            (@generics [T]) (sequence ListR<T>) (@return List<T>))
 (@function nreverse (sequence)
            (@generics [T]) (sequence List<T>) (@return List<T>))
 (@function copy-sequence (sequence)
            (@generics [T]) (sequence ListR<T>) (@return ListFresh<T>)))

(et-test
 (et-assert-call List<Integer> reverse ListR<Integer>)
 (et-assert-call List<Integer> nreverse List<Integer>)
 (et-assert-call ListFresh<Integer> copy-sequence ListR<Integer>))


;;; ============================================================
;;; Equality

;; (`eq' lives in data.c; `eql'/`equal' are defined in fns.c.)
(et-declare
 (@function eql (a b)
            (@generics [A B]) (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))
 (@function equal (a b)
            (@generics [A B]) (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B)))))))

(et-test
 (et-assert-resolve Boolean (equal 1 "2"))
 (et-assert-resolve Boolean (eql 1 2)))


;;; ============================================================
;;; Strings

(et-declare
 (@function string-search (needle haystack &optional start-pos)
            (needle String) (haystack String) (start-pos Integer)
            (@return Integer|Nil))
 (@function string-lessp (string1 string2)
            (string1 String) (string2 String) (@return Boolean))
 (@function string-equal (string1 string2)
            (string1 String) (string2 String) (@return Boolean)))

(et-test
 (et-assert-resolve Integer|Nil (string-search "a" "abc"))
 (et-assert-resolve Boolean (string-lessp "a" "b"))
 (et-assert-resolve Boolean (string-equal "a" "a"))
 (et-assert-resolve-errors (string-search "a" 5)))


;;; ============================================================
;;; Property lists

;; `plist-get'/`plist-put' need to look a literal key up in a `PList'
;; type, so they are checkers (with runtime logic) rather than static
;; `@function' declarations.

(defun et--plist-lookup (plist-type key-type error-fn)
  "Look up KEY-TYPE in PLIST-TYPE, returning the value type or nil.

PLIST-TYPE and KEY-TYPE should already be expanded.  KEY-TYPE must be a
literal or union of literals.  PLIST-TYPE must be a single PList case.
ERROR-FN is called with a format string and args on failure, and the
function returns nil."
  (when-let*
      ((plist-type (et-expand-all-aliases (et--remove-type-binds plist-type)))
       (key-type (et-expand-all-aliases (et--remove-type-binds key-type)))
       (keys
        (cl-loop for case in (et-type-cases key-type)
                 for val = (et-type-case-value case)
                 if (and (et-datatype-p val)
                         (eq (et-datatype-name val) 'Literal))
                 collect (car (et-datatype-args val))
                 else do (funcall error-fn "Key must be a literal, found %s" (et-pp key-type))
                 and return nil))
       (plist-args
        (pcase (et-type-cases plist-type)
          (`(,(cl-struct et-type-case
                         (value (cl-struct et-datatype (name 'PList) (args args)))))
           args)
          (_ (funcall error-fn "Expected a PList type, found %s" (et-pp plist-type))
             nil)))
       (val-types
        (cl-loop for k in keys
                 for v = (plist-get plist-args k)
                 if v collect v
                 else do (funcall error-fn "Key %s not found in %s"
                                  (et-pp (et-literal k)) (et-pp plist-type))
                 and return nil)))
    (apply #'et--or val-types)))


(et-define-pcase-checker plist-get `(,_plist ,_key)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2)))
    (et--plist-lookup plist-type key-type
                      (lambda (fmt &rest args) (apply #'et-err 0 fmt args)))))


(et-define-pcase-checker plist-put `(,_plist ,_key ,_val)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2))
         (val-type (et-checker-sub 3))
         (existing-val-type
          (et--plist-lookup plist-type key-type
                            (lambda (fmt &rest args) (apply #'et-err 0 fmt args)))))
    (when existing-val-type
      (if (et-subtype? val-type existing-val-type)
          plist-type
        (et-err 3 "Expected %s, found %s" (et-pp existing-val-type) (et-pp val-type))))))


(provide 'fns.c)
;;; fns.c.el ends here
