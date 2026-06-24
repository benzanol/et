;;; et-types.el --- Typesystem for emacs lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
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

(require 'et-check)


;;; ============================================================
;;; Definitions
;;;; Ignore certain forms

(et-define-checker declare (et Nil))


;;;; Function checkers

;; (et-define-pcase-checker lambda body
;;   (setq body (et--funcdef-inline-to-declare (copy-tree body)))
;;   (et-checker-function-body (et-parse-function-type body #'lambda) 1))

;; (et-test
;;  ;; Inline typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
;;    (lambda ([x Integer]) (+ x 1)))

;;  ;; Untyped args default to Any
;;  (et-assert-resolve Function<ConsR<Any~Nil>~Any>
;;    (lambda (x) x))

;;  ;; &optional — cdr is Nil|ConsR<String~Nil>
;;  (et-assert-resolve (Function ConsR<Integer~Nil|ConsR<String~Nil>> Integer)
;;    (lambda ([x Integer] &optional [y String]) x))

;;  ;; Multiple body forms — return type is last
;;  (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
;;    (lambda ([x Integer] [y String]) (+ x 1) y))

;;  ;; Empty arglist
;;  (et-assert-resolve Function<Nil~Integer>
;;    (lambda () 1))

;;  ;; &rest
;;  (et-assert-resolve (Function ConsR<Integer~ListR<Any>> Integer)
;;    (lambda ([x Integer] &rest args) x))
;;  (et-assert-resolve (Function ConsR<Any~ListR<Any>> ListR<Any>)
;;    (lambda (x &rest args) args)))

;; (et-test
;;  ;; Body return type with typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
;;    (lambda ([x Integer]) (+ x 1)))

;;  ;; Untyped args default to Any, body uses them
;;  (et-assert-resolve Function<ConsR<Any~Nil>~Any>
;;    (lambda (x) x))

;;  ;; &optional arg becomes Nil|Type
;;  (et-assert-resolve Function<ConsR<Integer~Nil|ConsR<String~Nil>>~Integer>
;;    (lambda ([x Integer] &optional [y String]) (+ x 1)))

;;  ;; Multiple body forms, return type is last
;;  (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
;;    (lambda ([x Integer] [y String]) (+ x 1) y))

;;  ;; Empty arglist
;;  (et-assert-resolve Function<Nil~Integer>
;;    (lambda () 1)))

;; Return type annotation tests
;; (et-test
;;  ;; Inline -> return type: uses declared type
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number (+ x 1)))

;;  ;; Inline -> with typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number x))

;;  ;; Inline -> is stripped from compiled output
;;  (let* ((result (et--check '(lambda (x) -> Integer 1))))
;;    (equal (et-result-compiled result) '(lambda (x) 1)))

;;  ;; Inline -> with body type mismatch produces error
;;  (et-assert-resolve-errors
;;   (lambda ([x Integer]) -> String x))

;;  ;; Inline -> with empty arglist
;;  (et-assert-resolve Function<Nil~Number>
;;    (lambda () -> Number 1))

;;  ;; Inline -> with multiple body forms
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number "ignored" x)))


;;; ============================================================
;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Derive type
               (and `(,name ,_val)
                    (let type (et-with-vars vars (et-checker-sub 1 (length vars) 1))))
               ;; No value (nil variable)
               (and name (pred symbolp)
                    (let type (et Nil))))
              ;; Add the var to the list
              (let var (et-new-var name (et--unfreshen-type (or type (et-never)))))))
      . ,_body)

  (et-with-vars vars
    (et-checker-tail 2)))


;;;; dolist

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


;;;; setq

(et-define-pcase-checker setq (and args (guard (eq 0 (mod (length args) 2))))
  (cl-loop for (var _val) on args by #'cddr
           for var-pos upfrom 1 by 2
           for type = (or (et-get-symbol-type var)
                          (et-err var-pos "Assignment to free variable"))
           do (et-checker-resolve type (1+ var-pos))
           finally return type))


;;;; and/or

(defun et--and-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were non-nil
  (let* ((non-nil-binds (et--type-binds (et--non-nil cond-type)))
         (output-type (et-with-narrow-binds non-nil-binds (funcall checker)))

         (output-non-nil (et--non-nil output-type))
         ;; If `and' returns non-nil, then both non-nil binds will be true (intersect them)
         (merged-non-nil-binds
          (et--intersect-binds nil non-nil-binds (et--type-binds output-non-nil))))

    (et--or (et--replace-type-binds output-non-nil merged-non-nil-binds)
            ;; If `and' returns nil, it could be from either `cond-type' OR `output-type' being nil
            (et--supersect cond-type (et Nil))
            (et--supersect output-type (et Nil)))))

(et-test
 (equal (et $a::2&True)
        (et--and-return-type (et $a::{1|2}&True) (lambda () (et $a::{2|3}&True)))))

(defun et--or-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were nil
  (let* ((nil-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (output-type (et-with-narrow-binds nil-binds (funcall checker)))

         (output-nil (et--supersect output-type (et Nil)))
         ;; If `or' returns nil, then both nil binds will be true (intersect them)
         (merged-nil-binds
          (et--intersect-binds nil nil-binds (et--type-binds output-nil))))

    (et--or (et--replace-type-binds output-nil merged-nil-binds)
            ;; If `or' returns non-nil, it could be from either `cond-type' OR `output-type'
            (et--non-nil cond-type)
            (et--non-nil output-type))))

(et-define-pcase-checker and args
  (cl-loop with acc-type = (et-literal t)
           for pos upfrom 1 to (length args)
           do (cl-callf et--and-return-type acc-type (lambda () (et-checker-sub pos)))
           finally return (et-simplify-type acc-type)))

(et-define-pcase-checker or args
  (cl-loop with acc-type = (et Nil)
           for pos upfrom 1 to (length args)
           do (cl-callf et--or-return-type acc-type (lambda () (et-checker-sub pos)))
           finally return (et-simplify-type acc-type)))


;;;; if

(et-define-pcase-checker if `(,_cond ,_then . ,_else)
  (let* ((cond-type (et-checker-sub 1))

         ;; Then branch: cond was non-nil
         (non-nil-cond (et--non-nil cond-type))
         (non-nil-binds (et--type-binds non-nil-cond))
         (then-type (et-with-narrow-binds non-nil-binds
                      (et-checker-sub 2)))

         ;; Else branch: cond was nil
         (nil-cond (et--supersect cond-type (et Nil)))
         (nil-binds (et--type-binds nil-cond))
         (else-type (et-with-narrow-binds nil-binds
                      (et-checker-tail 3))))

    (et-checker-hint-narrows
     0
     "IF:\\n%s" non-nil-cond
     "ELSE:\\n%s" nil-cond)

    (et-simplify-type (et--or then-type else-type))))

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
            (when (stringp a) a)))))
 (et-subtype? (et-typecheck
               (let* ((a String|Number 4))
                 (if (stringp a) a "hello!")))
              (et String)))


;;; ============================================================
;;; Function types
;;;; Quotes
;;;;; quote

(et-define-pcase-checker quote `(,expr)
  (et-literal expr))

(et-test
 (et-assert-resolve Integer '1)
 (et-assert-resolve Number '1.1)
 (et-assert-resolve String '"hi")
 (et-assert-resolve Symbol 'a)
 (et-assert-no-resolve Integer '1.1)
 (et-assert-no-resolve Integer ''1)
 (et-assert-no-resolve Number ''1.1)
 (et-assert-no-resolve String ''"hi")
 (et-assert-no-resolve Symbol ''a)

 (et-assert-resolve ConsR<Any~Any> '(1 2 3))
 (et-assert-resolve ListR<Symbol> '(a b c))
 (et-assert-resolve ListR<Integer> '())
 (et-assert-no-resolve ListR<Integer> '(1 2 '3))
 (et-assert-no-resolve ListR<Integer> '(1 2 3.3))
 (et-assert-no-resolve ListR<Integer> ''(1 2 3))
 (et-assert-no-resolve ListR<Integer> ''())

 (et-assert-resolve ConsR<Integer~Integer> '(1 . 2))
 (et-assert-no-resolve ConsR<Integer~Integer> '(1 . 2.2))
 (et-assert-no-resolve ConsR<Integer~Integer> '(1.1 . 2))
 (et-assert-resolve ConsR<Symbol~ListR<String>> '(a "2" "3")))


;;;;; function

(et-define-pcase-checker function `(,inner)
  (pcase inner
    ;; Lambda expression: typecheck it as a lambda
    (`(lambda . ,_rest)
     (et-checker-sub 1))

    ;; Symbol: look up its function type
    ((and sym (pred symbolp))
     (if-let* ((func-type (get sym 'et-function-type)))
         func-type
       (et-err nil "No function type for `%s'" sym)))

    ;; Anything else is invalid
    (_ (et-err nil "Invalid argument to function: %s" inner))))

(et-test
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   #'(lambda ([x Integer]) x))

 (not (et-never-p (et-result-type (et--check '#'+))))
 (et-never-p (et-result-type (et--check '#'function))))


;;;; Arithmetic

(et-define-type-checker (+ -) [N]
  (or Nil&{N=0} ListR<Integer>&{N=Integer} ListR<Number>&{N=Number})
  N)

(et-define-type-checker * [N]
  (or Nil&{N=1} ListR<Integer>&{N=Integer} ListR<Number>&{N=Number})
  N)

(et-define-type-checker / [N]
  (or NonNilListR<Integer>&{N=Integer} NonNilListR<Number>&{N=Number})
  N)

(et-define-type-checker (1+ 1-) [N] (Args Integer&{N=Integer}|Number&{N=Number}) N)

;; (et-typecheck-call + Integer Integer 1 2.1 3)

(et-test
 op [+ - * /]
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Number op Integer Integer 1 2.1 3)
 (et-assert-call Integer op 1))

(et-test
 (et-assert-call 0 +)
 (et-assert-call 0 -)
 (et-assert-call 1 *)
 (et-assert-call-errors /))


;;;; Sequences
;;;;; cons

(et-define-type-checker cons [L R] (Args L R) ConsFresh<L~R>)


(et-test
 (et-assert-resolve ConsFresh<Integer~String> (cons 1 "2"))
 (et-assert-resolve Cons<Integer~String> (cons 1 "2"))
 (et-assert-no-resolve ConsFresh<Integer~String> (cons "1" 2))
 (et-assert-no-resolve Cons<Integer~String> (cons "1" 2))
 (et-assert-resolve ConsFresh<Integer~ListR<String>> (cons 1 nil))
 (et-assert-resolve Cons<Integer~List<String>> (cons 1 (cons "2" nil)))

 (et-assert-resolve List<Integer> (cons 1 (cons 2 nil)))
 (et-assert-no-resolve List<Integer> (cons 1 (cons "2" nil)))
 (et-assert-no-resolve List<Integer> (cons "1" (cons 2 nil)))
 (et-assert-no-resolve List<Integer> (cons 1 (cons 2 t))))


;;;;; list/copy-tree

(et-define-type-checker list [T] T (eval et--freshen-type-shallow T))
(et-define-type-checker copy-tree [T] (Args T) (eval et--freshen-type T))

(et-test
 (equal (et ConsFresh<Cons<1~2>~ConsFresh<Cons<3~4>~Nil>>)
        (et-typecheck-call list Cons<1~2> Cons<3~4>))
 (equal (et ConsFresh<ConsFresh<1~2>~ConsFresh<ConsFresh<3~4>~Nil>>)
        (et-typecheck-call copy-tree (TupleR Cons<1~2> Cons<3~4>)))

 (et-assert-resolve ConsR<Integer~ListR<String>> (list 1 "2"))
 (et-assert-no-resolve ConsR<Integer~String> (list "1" 2))
 (et-assert-no-resolve ConsR<Integer~String> (list))

 (et-assert-resolve ListR<Integer> (list 1 2 3))
 (et-assert-resolve ListR<Integer> (list 1))
 (et-assert-no-resolve ListR<Integer> (list 1 "2" 3)))


;;;;; car/cdr

(et-defalias MatchCar [T]
  :matcher-only t
  (or (and Nil (set T Nil))
      (ConsR T Any)))

(et-defalias MatchCdr [T]
  :matcher-only t
  (or (and Nil (set T Nil))
      (ConsR Any T)))

(et-define-type-checker car [T] (Args (MatchCar T)) T)
(et-define-type-checker cdr [T] (Args (MatchCdr T)) T)

(et-test
 (et-assert-resolve Integer (car (list 1 2.2 3)))
 (et-assert-no-resolve Integer (car (list 1.1 2 3)))
 (et-assert-resolve Integer (car (cons 1 "3")))
 (et-assert-resolve ListR<Integer> (car (cons (list 1) "3")))
 (et-assert-resolve ConsR<Integer~Any> (car (cons (list 1) "3")))
 (et-assert-resolve Integer (car (car (cons (list 1) "3"))))
 (et-assert-no-resolve Integer (car (car (cons (list 1.1) "3"))))

 (et-assert-call Never cdr Never)
 (et-assert-call Nil cdr Nil)
 (et-assert-call Nil|String cdr Nil|ConsR<Integer~String>)
 (et-assert-call-errors cdr Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors cdr :any)

 (et-assert-call Never car Never)
 (et-assert-call Nil car Nil)
 (et-assert-call Nil|Integer car Nil|ConsR<Integer~String>)
 (et-assert-call-errors car Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors car Any)

 (et-assert-call Nil|Integer car ListR<Integer>)

 (et-assert-call Nil|Integer|String car ListR<Integer>|ConsR<String~Nil>)

 (et-assert-error
  (et-root-check-call car ListR<Integer>|ConsR<String~Nil>|String)))

(et-test
 (et-assert-resolve ListR<Number> (cdr (list 1 2.2 3)))
 (et-assert-resolve ListR<Integer> (cdr (list 1.1 2 3)))
 (et-assert-no-resolve ListR<Integer> (car (list 1 2.2 3)))

 (et-assert-resolve Integer (cdr (cons "1" 2)))
 (et-assert-no-resolve Integer (cdr (cons 1 "2")))

 (et-assert-resolve ListR<Integer> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Any> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Boolean> (cdr (cons "1" (list 2))))
 (et-assert-no-resolve ConsR<Integer~Boolean> (cdr (cons "1" (list 2 3))))
 (et-assert-resolve Integer (car (cdr (cons "1" (list 2)))))

 (et-assert-resolve Boolean (cdr (cdr (cdr (list 1 2 3)))))
 (et-assert-no-resolve Boolean (cdr (cdr (list 1 2 3))))

 (et-assert-call ListR<Integer> cdr ListR<Integer>)
 (et-assert-call ListR<Integer>|String cdr ListR<Integer>|ConsR<Nil~String>)
 (et-assert-call-errors car ListR<Integer>|ConsR<String~Nil>|String))


;;;;; c[ad][ad][ad]?r

(et-define-type-checker caar [T] (Args (MatchCar (MatchCar T))) T)
(et-define-type-checker cddr [T] (Args (MatchCdr (MatchCdr T))) T)

(et-define-type-checker cadr [T] (Args (MatchCdr (MatchCar T))) T)
(et-define-type-checker cdar [T] (Args (MatchCar (MatchCdr T))) T)

(et-define-type-checker caaar [T] (Args (MatchCar (MatchCar (MatchCar T)))) T)
(et-define-type-checker cdddr [T] (Args (MatchCdr (MatchCdr (MatchCdr T)))) T)

(et-define-type-checker caadr [T] (Args (MatchCdr (MatchCar (MatchCar T)))) T)
(et-define-type-checker cddar [T] (Args (MatchCar (MatchCdr (MatchCdr T)))) T)

(et-define-type-checker caddr [T] (Args (MatchCdr (MatchCdr (MatchCar T)))) T)
(et-define-type-checker cdaar [T] (Args (MatchCar (MatchCar (MatchCdr T)))) T)

(et-define-type-checker cadar [T] (Args (MatchCar (MatchCdr (MatchCar T)))) T)
(et-define-type-checker cdadr [T] (Args (MatchCdr (MatchCar (MatchCdr T)))) T)

(et-test
 (et-assert-resolve 2 (cadr '(1 2 3)))
 (et-assert-resolve 3 (caddr '(1 2 3)))

 (et-assert-resolve 0 (cadar '((-1 0) 1 2 3)))
 (et-assert-resolve List<3> (cdddr '((-1 0) 1 2 3))))


;;;;; setcar

(et-define-type-checker setcar [A] (Args A Nil|ConsW<A~Never>) A)

(et-test
 (et-typecheck-call setcar Number ConsW<Number~Number>))


;;;;; nth/nthcdr

(et-define-type-checker nth [T] (Args Integer ListR<T>) T|Nil)

(et-define-type-checker nthcdr [T] (Args Integer ListR<T>) ListR<T>)

(et-test
 (et-assert-call Number|String|Nil nth Integer ConsR<Number~ListR<String>>)

 (et-assert-call ListR<Number|String> nthcdr Integer ConsR<Number~ListR<String>>)
 (et-assert-call ListR<Never> nthcdr Integer Nil))


;;;;; length

(et-define-type-checker length [] (Args String|ListR<Any>|VectorR<Any>) Integer)

(et-test
 (et-assert-call Integer length VectorR<Number>|ListR<String>)
 (et-assert-call-errors length VectorR<Number>|ListR<String>|Number))


;;;;; vector

(et-define-type-checker vector [T] ListR<T> Vector<T>)

(et-test
 (et-assert-call Vector<1|2> vector 1 2)
 (et-assert-call Vector<Never> vector)
 (et-assert-call Vector<Number> vector Integer Number Positive))


;;;;; aref

(et-define-type-checker aref [T] (Args VectorR<T>|{String&T=Integer} Integer) T)

(et-test
 (et-assert-call Integer aref String Integer)
 (et-assert-call Symbol|Integer aref (or VectorR<Symbol> String) Integer)
 (et-assert-call-errors aref (or VectorR<Symbol> String ListR<Any>) Integer))


;;;;; alists

(et-define-type-checker (assq assoc rassq rassoc) [C] (Args Any ListR<C&Cons>) C|Nil)
(et-define-type-checker alist-get [V] (Args Any ListR<ConsR<Any~V>>) V|Nil)

(et-typecheck-call alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)


(et-test
 (et-assert-call-errors alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)
 (et-assert-call 2|Nil alist-get Integer AList<1~2>)
 (et-assert-resolve 2|4|Nil (alist-get 4 (list (cons 1 2) (cons 3 4)))))


;;;;; mapcar/mapc/mapconcat

(et-define-type-checker mapcar [T R] (Args Function<Args<T>~R> ListR<T>) ListFresh<R>)
(et-define-type-checker mapc [T R] (Args Function<Args<T>~R> ListR<T>) Nil)

(et-define-type-checker mapconcat [T R]
  (ArgsWithTail Function<Args<T>~String> ListR<T> Nil|ConsR<String~Nil>)
  String)


;;;;; append/nconc

(et-defalias AppendFresh [E R]
  (or R (ConsFresh E (AppendFresh E R))))

(defun et--append-return-type (input-type)
  (or (et-checker-infer input-type [] Nil Nil)
      (et-checker-infer input-type [S] (ConsR S Nil) S)
      (et-checker-infer input-type [E R] (ConsR List<E> R)
                        (AppendFresh E (eval et--append-return-type R)))))

(et-define-type-checker append [A] A (eval et--append-return-type A))

(et-define-type-checker nconc [E] ListR<List<E>> List<E>)

(et-test
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] List<T>)
          (et-result-value (et-typecheck-call append List<1> List<Integer> List<Number>)))))

 ;; I thought this was a bug at first, but it is actually correct!
 ;; Since the tail is List<Integer>, this is NOT a valid List<Number>,
 ;; since List<Number> means you could nconc 0.5 onto the end, which
 ;; would add 0.5 to a list of Integers. Either inferring a ListR, or
 ;; adding Nil to the end make the check valid, as is shown in the
 ;; following tests
 (equal 'INVALID
        (et-sub-match
         (et-matcher [T] List<T>)
         (et-typecheck-call append List<1> List<Number> List<Integer>)))
 (equal (list (et Number))
        (et-sub-match
         (et-matcher [T] List<T>)
         (et-typecheck-call append List<1> List<Number> List<Integer> Nil)))
 (equal (list (et Number))
        (et-sub-match
         (et-matcher [T] ListR<T>)
         (et-typecheck-call append List<1> List<Integer> List<Number> List<1>))))


;;;;; reverse/nreverse

(et-define-type-checker reverse [T] (Args ListR<T>) List<T>)
(et-define-type-checker nreverse [T] (Args List<T>) List<T>)


;;;;; delete-dups

(et-define-type-checker delete-dups [T] (Args ListR<T>) List<T>)


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-type-checker ,name [T]
     (Args T)
     (or (and True (bindsof (and T ,type)))
         (and Nil (bindsof (subtract T ,type))))))

(et-define-predicate stringp String)
(et-define-predicate symbolp Symbol)
(et-define-predicate numberp Number)
(et-define-predicate integerp Integer)
(et-define-predicate consp Cons)
(et-define-predicate listp Nil|Cons)
(et-define-predicate null Nil)
(et-define-predicate not Nil)

(et-test
 (et-assert-call True&{$a::Cons}
                 consp Cons&{::$a})

 (et-assert-call (or True&{$a::String} Nil&{$a::Number})
                 stringp {::$a}&{String|Number})

 ;; This tests whether et--supersect works correctly when it cannot determine a definite subtype.
 ;; There is no defined intersection of Positive and Integer, so it must make an approximation.
 ;; Approximating to never would incorrectly determine that this call will always determine nil.
 ;; So, in order to ensure correctness, it must assume a SUPERSET of the real intersection (by picking either Positive or Integer.)
 (not (et-subtype? (et-typecheck-call integerp Positive&{::$a}) (et Nil))))


;;;; Funcall

(et-define-checker apply
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tailed-tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et-checker-funcall func-type args-type)))
    (if (et-type-p output-type) output-type
      (et-err 0 "Cannot apply %s with args %s" func-type args-type))))

(et-define-checker funcall
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et-checker-funcall func-type args-type)))
    (if (et-type-p output-type) output-type
      (et-err 0 "Cannot call %s with args %s" func-type args-type))))

(et-test
 (et-assert-resolve Integer (funcall (lambda ([x Integer] [y Integer]) x) 1 2))
 (et-assert-resolve-errors (funcall (lambda ([x Integer] [y Integer]) x) 1 2.5))
 (et-assert-resolve-errors (funcall (lambda ([x Integer] [y Integer]) x) 1))
 (et-assert-resolve-errors (funcall (lambda ([x Integer]) x) 1 2))
 (et-assert-resolve-errors (funcall (lambda () x) 1))
 (et-assert-resolve-errors (funcall (lambda ([x Integer]) x)))

 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) (list 1 2 3)))
 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 (list 3)))
 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 3 nil))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 (list 3 4)))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 (list 3)))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) (list 3)))

 (et-assert-resolve Integer (apply (lambda () 0) nil))
 (et-assert-resolve-errors (apply (lambda () 0) 1 nil))

 (et-assert-resolve Integer (apply #'+ 1 2 (list 3 4)))
 (et-assert-resolve Integer (apply #'+ 1 2 nil))
 (et-assert-resolve Number (apply #'+ 1 2 (list 3 4.4 5)))
 (et-assert-resolve 0 (apply #'+ nil))
 (et-assert-resolve-errors (apply #'+ 1 2 (list "3")))
 (et-assert-resolve-errors (apply #'+ 1 2 3)))


;;;; Equality

(et-define-type-checker (eq eql equal) [A B]
  (Args A B)
  (or Nil (and True (bindsof (and A B)))))

(et-define-type-checker = [(<= A Number) (<= B Number)]
  (Args A B)
  (or Nil (and True (bindsof (and A B)))))

(et-define-type-checker (< <= > >=) (Args Number Number) Boolean)


;;;; Plist

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


;;;; cl-loop

(defvar et--loop-keywords
  '(for as with do doing initially finally repeat
        while until always never thereis
        collect collecting append appending nconc nconcing
        concat vconcat count counting sum summing
        maximize maximizing minimize minimizing
        if when unless else and end
        into named return)
  "All recognized `cl-loop' clause keywords.")

(defun et--loop-keyword-p (sym)
  "Return non-nil if SYM is a recognized `cl-loop' keyword."
  (memq sym et--loop-keywords))

(defun et--loop-infer-elem-type (list-type)
  "Infer the element type of LIST-TYPE for `for VAR in LIST'.
Returns the element type, or Any if undetermined."
  (or (et-checker-infer list-type [T] ListR<T> T)
      (et-any)))

(defun et--loop-infer-vector-elem-type (vec-type)
  "Infer the element type of VEC-TYPE for `for VAR across ARRAY'.
Handles both vectors (element type) and strings (Integer char codes)."
  (or (et-checker-infer vec-type [T] VectorR<T> T)
      (and (et-subtype? vec-type (et String)) (et Integer))
      (et-any)))

(defun et--loop-numeric-var-type (bound-types)
  "Determine the loop variable type from numeric BOUND-TYPES.
Integer if all bounds are Integer, otherwise Number."
  (if (cl-every (lambda (bt) (et-subtype? bt (et Integer))) bound-types)
      (et Integer)
    (et Number)))


;;;;; Accumulators

;; A cl-loop has a single implicit accumulator, plus one named
;; accumulator per `into VAR'. Every bare accumulation clause feeds the
;; implicit accumulator, and they must all belong to the same category
;; (you cannot `collect' and `sum' into the same place). An
;; `et--loop-acc' tracks the mutable state of one such accumulator.

(cl-defstruct et--loop-acc
  "Mutable state of one cl-loop accumulator (the bare one or an `into VAR')."
  (cat nil)        ; nil | `list' | `numeric' | `string' | `vector'
  (elems nil)      ; contributed element/value types (to be unioned)
  (nonfresh nil))  ; t once an `nconc' splices foreign conses (not fresh)

(defun et--loop-accum-category (kw)
  "Return the accumulation category of clause keyword KW."
  (pcase kw
    ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing) 'list)
    ((or 'count 'counting 'sum 'summing
         'maximize 'maximizing 'minimize 'minimizing) 'numeric)
    ('concat 'string)
    ('vconcat 'vector)))

(defun et--loop-acc-add (acc kw form-type pos)
  "Record an accumulation of clause KW with FORM-TYPE into ACC.
POS is the clause index of the form, for error reporting."
  ;; The accumulated value is no longer the loop variable, so drop any
  ;; narrowing binds (e.g. `{typeof x}') it picked up: they are
  ;; meaningless once detached from the variable, and leaving them inside
  ;; an accumulator alias breaks later alias expansion when an `into VAR'
  ;; is referenced.
  (setq form-type (et--remove-type-binds form-type))
  (let ((cat (et--loop-accum-category kw)))
    (cond
     ((null (et--loop-acc-cat acc)) (setf (et--loop-acc-cat acc) cat))
     ((not (eq (et--loop-acc-cat acc) cat))
      (et-err (1+ pos) "Cannot combine a `%s' clause with `%s' accumulation"
              kw (et--loop-acc-cat acc))))
    (pcase cat
      ('list
       (pcase kw
         ;; collect: the form is itself one element
         ((or 'collect 'collecting) (push form-type (et--loop-acc-elems acc)))
         ;; append/nconc: the form is a list spliced in element-wise
         (_ (push (et--loop-infer-elem-type form-type) (et--loop-acc-elems acc))
            (when (memq kw '(nconc nconcing))
              (setf (et--loop-acc-nonfresh acc) t)))))
      ;; count yields an Integer regardless of the form; the rest add the form
      ('numeric (push (if (memq kw '(count counting)) (et Integer) form-type)
                      (et--loop-acc-elems acc)))
      ('vector (push form-type (et--loop-acc-elems acc)))
      ('string nil))))

(defun et--loop-acc-type (acc)
  "Compute the result type accumulated in ACC, or nil if ACC is empty."
  (pcase (et--loop-acc-cat acc)
    ('nil nil)
    ('list
     (let ((elem (apply #'et--or (et--loop-acc-elems acc))))
       ;; nconc shares the body's conses; collect/append build fresh spines
       (et-alias (if (et--loop-acc-nonfresh acc) 'List 'ListFresh) elem)))
    ('numeric
     (if (cl-every (lambda (ty) (et-subtype? ty (et Integer))) (et--loop-acc-elems acc))
         (et Integer) (et Number)))
    ('string (et String))
    ('vector (et-dt 'VectorFresh (apply #'et--or (et--loop-acc-elems acc))))))


;;;;; Walker state

;; The walker threads a cursor over the flat clause list, plus the
;; loop-global accumulation/return state. These are genuinely
;; loop-global, so they are dynamically scoped and mutated in place;
;; lexical recursion is reserved for the things that are actually
;; lexically scoped: variable bindings (`et--binds') and conditional
;; narrowing (`et--narrow-binds').

(defvar et--loop-clauses nil "Clause list of the cl-loop being checked.")
(defvar et--loop-len 0 "Length of `et--loop-clauses'.")
(defvar et--loop-pos 0 "Cursor index into `et--loop-clauses'.")
(defvar et--loop-bare nil "The bare implicit accumulator, an `et--loop-acc'.")
(defvar et--loop-into nil "Alist of NAME -> `et--loop-acc' for `into VAR'.")
(defvar et--loop-returns nil "Types contributed by `return' clauses.")
(defvar et--loop-thereis nil "Types contributed by `thereis' clauses.")
(defvar et--loop-bool nil "Non-nil if an `always'/`never' clause is present.")
(defvar et--loop-finally nil "The `finally return' type, overriding all else.")


;;;;; Cursor

(defun et--loop-peek ()
  "Return the clause at the cursor without advancing."
  (and (< et--loop-pos et--loop-len) (nth et--loop-pos et--loop-clauses)))

(defun et--loop-advance ()
  "Return the clause at the cursor and advance past it."
  (prog1 (et--loop-peek) (cl-incf et--loop-pos)))

(defun et--loop-eat (&rest kws)
  "If the cursor is on one of KWS, consume it and return non-nil."
  (when (and (< et--loop-pos et--loop-len) (memq (et--loop-peek) kws))
    (et--loop-advance) t))

(defun et--loop-check-expr ()
  "Type-check the form at the cursor in the current scope, then advance."
  (prog1 (et-checker-sub (1+ et--loop-pos)) (cl-incf et--loop-pos)))

(defun et--loop-check-body ()
  "Check consecutive forms until the next loop keyword; return the last type."
  (let ((ty (et Nil)))
    (while (and (< et--loop-pos et--loop-len) (not (et--loop-keyword-p (et--loop-peek))))
      (setq ty (et--loop-check-expr)))
    ty))


;;;;; Bindings

(defun et--loop-bind (name type)
  "Bind loop variable NAME to TYPE for all subsequent clauses.
Pushes onto the loop-local `et--binds'; subsequent clauses see NAME
\(sequential, `let*'-style scope)."
  (when (and name (symbolp name))
    (push (cons name (et-new-var name (et--unfreshen-type type))) et--binds)))

(defun et--loop-bind-pattern (pat type)
  "Bind PAT, a symbol or destructuring pattern, for subsequent clauses.
A plain symbol gets TYPE.  A destructuring pattern binds each of its
symbols to Any (precise destructuring element types are not inferred)."
  (cond
   ((null pat) nil)
   ((symbolp pat) (et--loop-bind pat type))
   (t (dolist (sym (flatten-tree pat))
        (when (and sym (symbolp sym)) (et--loop-bind sym (et-any)))))))


;;;;; for / as

(defun et--loop-clause-for ()
  "Parse a `for'/`as' clause, including parallel `and'-joined bindings.
Each binding's initializer is checked in the scope *before* the group is
bound, so `and'-joined bindings do not see one another (parallel), while
separate `for' clauses do (sequential)."
  (let ((group nil))
    (cl-loop
     (let* ((var (et--loop-advance))
            (type (et--loop-for-spec var)))
       (push (cons var type) group))
     ;; Continue the parallel group only if `and' is followed by another
     ;; binding (a var), not by a clause of a different kind.
     (unless (eq (et--loop-peek) 'and) (cl-return))
     (let* ((after (nth (1+ et--loop-pos) et--loop-clauses))
            (head (if (memq after '(for as)) (nth (+ 2 et--loop-pos) et--loop-clauses) after)))
       (unless (and head (or (symbolp head) (consp head)) (not (et--loop-keyword-p head)))
         (cl-return)))
     (et--loop-advance)            ; consume `and'
     (et--loop-eat 'for 'as))
    ;; Bind the whole group at once.
    (dolist (b (nreverse group))
      (et--loop-bind-pattern (car b) (cdr b)))))

(defun et--loop-for-spec (var)
  "Parse the iteration spec following `for VAR' and return VAR's type.
Initializer expressions are checked in the current scope."
  (pcase (et--loop-peek)
    ;; VAR from/upfrom/downfrom EXPR [to/... EXPR] [by EXPR]
    ((or 'from 'upfrom 'downfrom)
     (et--loop-advance)
     (let ((bounds (list (et--loop-check-expr))))
       (when (et--loop-eat 'to 'upto 'downto 'above 'below)
         (push (et--loop-check-expr) bounds))
       (when (et--loop-eat 'by) (push (et--loop-check-expr) bounds))
       (et--loop-numeric-var-type bounds)))

    ;; VAR to/upto/downto/above/below EXPR [by EXPR]  (implicit start 0)
    ((or 'to 'upto 'downto 'above 'below)
     (et--loop-advance)
     (let ((bounds (list (et Integer) (et--loop-check-expr))))
       (when (et--loop-eat 'by) (push (et--loop-check-expr) bounds))
       (et--loop-numeric-var-type bounds)))

    ;; VAR = EXPR1 [then EXPR2] ; EXPR2 sees VAR bound to its prior value
    ('=
     (et--loop-advance)
     (let ((init (et--loop-check-expr)))
       (if (et--loop-eat 'then)
           (let ((et--binds et--binds))
             (et--loop-bind-pattern var init)
             (et--or init (et--loop-check-expr)))
         init)))

    ;; VAR in/in-ref LIST [by FUNC]
    ((or 'in 'in-ref)
     (et--loop-advance)
     (let ((lst (et--loop-check-expr)))
       (when (et--loop-eat 'by) (et--loop-check-expr))
       (et--loop-infer-elem-type lst)))

    ;; VAR on LIST [by FUNC] ; VAR is bound to successive tails
    ('on
     (et--loop-advance)
     (let ((lst (et--loop-check-expr)))
       (when (et--loop-eat 'by) (et--loop-check-expr))
       lst))

    ;; VAR across/across-ref ARRAY
    ((or 'across 'across-ref)
     (et--loop-advance)
     (et--loop-infer-vector-elem-type (et--loop-check-expr)))

    ;; VAR being ...
    ('being (et--loop-advance) (et--loop-for-being))

    ;; Bare `for VAR' or an unrecognized spec
    (_ (et-any))))

(defun et--loop-for-being ()
  "Parse a `for VAR being ...' spec and return VAR's type.
May bind a secondary `using (FN VAR2)' variable."
  (et--loop-eat 'the 'each)
  (pcase (et--loop-peek)
    ((or 'elements 'element)
     (et--loop-advance)
     (et--loop-eat 'of 'of-ref)
     (let ((seq (et--loop-check-expr)))
       (prog1 (et--or (et--loop-infer-elem-type seq)
                      (et--loop-infer-vector-elem-type seq))
         (et--loop-being-using))))
    ((or 'hash-keys 'hash-key 'hash-values 'hash-value)
     (et--loop-advance)
     (et--loop-eat 'of)
     (et--loop-check-expr)
     (et--loop-being-using)
     (et-any))
    ((or 'symbols 'symbol)
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (et Symbol))
    ((or 'key-codes 'key-bindings 'key-seqs)
     (et--loop-advance)
     (et--loop-eat 'of)
     (et--loop-check-expr)
     (et--loop-being-using)
     (et-any))
    ((or 'overlays 'intervals)
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (when (et--loop-eat 'from) (et--loop-check-expr))
     (when (et--loop-eat 'to) (et--loop-check-expr))
     (et-any))
    ((or 'frames 'buffers) (et--loop-advance) (et-any))
    ('windows
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (et-any))
    (_ (et-any))))

(defun et--loop-being-using ()
  "Consume an optional `using (FN VAR2)' spec, binding VAR2."
  (when (et--loop-eat 'using)
    (let ((spec (et--loop-advance)))
      (when (consp spec)
        (et--loop-bind (cadr spec)
                       (if (eq (car spec) 'index) (et Integer) (et-any)))))))


;;;;; with

(defun et--loop-clause-with ()
  "Parse `with VAR [= EXPR] [and VAR [= EXPR]]...'.
Separate `with' clauses are sequential; `and'-joined ones are parallel,
so their initializers are checked before any of the group is bound."
  (let ((group nil))
    (cl-loop
     (let* ((var (et--loop-advance))
            (type (if (et--loop-eat '=) (et--loop-check-expr) (et Nil))))
       (push (cons var type) group))
     (unless (et--loop-eat 'and) (cl-return)))
    (dolist (b (nreverse group))
      (et--loop-bind-pattern (car b) (cdr b)))))


;;;;; Accumulation clauses

(defun et--loop-into-acc (name)
  "Return the `et--loop-acc' for `into NAME', creating it if needed."
  (or (alist-get name et--loop-into)
      (let ((acc (make-et--loop-acc)))
        (push (cons name acc) et--loop-into)
        acc)))

(defun et--loop-clause-accum (kw)
  "Process an accumulation clause whose keyword KW was already consumed.
The cursor is on the form to accumulate."
  (let* ((pos et--loop-pos)
         (form-type (et--loop-check-expr)))
    (if (et--loop-eat 'into)
        (et--loop-acc-add (et--loop-into-acc (et--loop-advance)) kw form-type pos)
      (et--loop-acc-add et--loop-bare kw form-type pos))))


;;;;; Conditional clauses

(defun et--loop-clause-cond (kw)
  "Process `if'/`when'/`unless' (KW already consumed).
The condition narrows the variable bindings visible to the inner
clauses: the THEN side sees the condition's non-nil narrowing, the ELSE
side sees its nil narrowing (swapped for `unless')."
  (let* ((cond-type (et--loop-check-expr))
         (pos-binds (et--type-binds (et--non-nil cond-type)))
         (neg-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (then-binds (if (eq kw 'unless) neg-binds pos-binds))
         (else-binds (if (eq kw 'unless) pos-binds neg-binds)))
    (et-with-narrow-binds then-binds
      (et--loop-cond-clauses))
    (when (et--loop-eat 'else)
      (et-with-narrow-binds else-binds
        (et--loop-cond-clauses)))
    (et--loop-eat 'end)))

(defun et--loop-cond-clauses ()
  "Process a chain of inner conditional clauses joined by `and'."
  (et--loop-cond-one)
  (while (et--loop-eat 'and) (et--loop-cond-one)))

(defun et--loop-cond-one ()
  "Process a single inner clause of a conditional."
  (pcase (et--loop-peek)
    ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing
         'concat 'vconcat 'count 'counting 'sum 'summing
         'maximize 'maximizing 'minimize 'minimizing)
     (et--loop-clause-accum (et--loop-advance)))
    ('return (et--loop-advance) (push (et--loop-check-expr) et--loop-returns))
    ((or 'do 'doing) (et--loop-advance) (et--loop-check-body))
    ((or 'if 'when 'unless) (et--loop-clause-cond (et--loop-advance)))
    (_ nil)))


;;;;; finally

(defmacro et--loop-with-into-vars (&rest body)
  "Evaluate BODY with every `into VAR' bound to its accumulated type.
By the time a `finally' clause is reached, all `into' accumulations have
been walked, so their types are final."
  ;; LIMITATION: referencing a variable whose type is a recursive list
  ;; alias (e.g. `ListFresh') currently triggers a pre-existing failure
  ;; deep in the core (`et--supersect' builds a matcher from the alias
  ;; and chokes on its type argument). So `finally return VAR' for an
  ;; `into VAR' accumulator surfaces that core error. This is not
  ;; specific to this checker -- the previous cl-loop checker bound
  ;; `into' variables the same way and hit the same path.
  `(let ((et--binds et--binds))
     (cl-loop for (name . acc) in et--loop-into
              for ty = (et--loop-acc-type acc)
              when ty do (et--loop-bind name ty))
     ,@body))

(defun et--loop-clause-finally ()
  "Process a `finally' clause (the `finally' keyword already consumed)."
  (et--loop-with-into-vars
   (if (et--loop-eat 'return)
       (setq et--loop-finally (et--loop-check-expr))
     (et--loop-eat 'do)
     (et--loop-check-body))))


;;;;; Walk

(defun et--loop-walk ()
  "Walk the clause list, type-checking each clause and threading scope."
  (while (< et--loop-pos et--loop-len)
    (let ((kw (et--loop-advance)))
      (pcase kw
        ((or 'for 'as) (et--loop-clause-for))
        ('with (et--loop-clause-with))
        ((or 'do 'doing) (et--loop-check-body))
        ('initially (et--loop-eat 'do) (et--loop-check-body))
        ('finally (et--loop-clause-finally))
        ('repeat (et--loop-check-expr))
        ((or 'while 'until) (et--loop-check-expr))
        ((or 'always 'never) (et--loop-check-expr) (setq et--loop-bool t))
        ('thereis (push (et--loop-check-expr) et--loop-thereis))
        ('return (push (et--loop-check-expr) et--loop-returns))
        ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing
             'concat 'vconcat 'count 'counting 'sum 'summing
             'maximize 'maximizing 'minimize 'minimizing)
         (et--loop-clause-accum kw))
        ((or 'if 'when 'unless) (et--loop-clause-cond kw))
        ('named (et--loop-advance))
        ;; A stray non-keyword token (e.g. a parse desync); ignore it.
        (_ nil)))))


;;;;; Result type

(defun et--loop-result-type ()
  "Compute the overall return type of the walked loop.
Precedence: `finally return' > `always'/`never' > `thereis' > the
implicit accumulator combined with any body `return' values."
  (cond
   (et--loop-finally et--loop-finally)
   (et--loop-bool (et Boolean))
   (et--loop-thereis (apply #'et--or (et Nil) et--loop-thereis))
   (t (let* ((bare (et--loop-acc-type et--loop-bare))
             ;; The value on normal loop completion
             (base (or bare (et Nil))))
        ;; A body `return' may or may not fire, so it unions with `base'
        (if et--loop-returns
            (apply #'et--or base et--loop-returns)
          base)))))


;;;;; Checker

(et-define-checker cl-loop
  (let* ((et--loop-clauses (cdr et--checker-expr))
         (et--loop-len (length et--loop-clauses))
         (et--loop-pos 0)
         ;; Loop variables are pushed onto a private copy of `et--binds'
         (et--binds et--binds)
         (et--loop-bare (make-et--loop-acc))
         (et--loop-into nil)
         (et--loop-returns nil)
         (et--loop-thereis nil)
         (et--loop-bool nil)
         (et--loop-finally nil))
    (et--loop-walk)
    ;; Return the raw type; the framework (`et-typecheck') simplifies
    ;; downstream, as it does for every other checker.
    (et--loop-result-type)))


;;;; Some string/symbol functions

(et-define-type-checker gensym [] Nil|Args<String> Var)
(et-define-type-checker intern [] (Args String) Symbol)
(et-define-type-checker symbol-name [] (Args Symbol) String)
(et-define-type-checker format [] (ArgsWithTail String ListR<Any>) String)
(et-define-type-checker error [] (ArgsWithTail String ListR<Any>) Never)


;;;; Tests

(et-test
 ;; ---- for VAR in LIST: infers element type ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list 1 2 3) collect x))

 ;; ---- for VAR from/to: numeric bounds determine Integer vs Number ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i from 1 to 10 collect i))
 (et-assert-resolve ListFresh<Number>
   (cl-loop for i from 1.0 to 10 collect i))

 ;; ---- for VAR = EXPR then EXPR ----
 (et-assert-resolve ListFresh<Integer|String>
   (cl-loop for x = 1 then "hi" collect x))

 ;; ---- for VAR across ARRAY ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for c across "hello" collect c))

 ;; ---- for VAR on LIST ----
 (et-assert-resolve ListFresh<ListR<Integer>>
   (cl-loop for tail on (list 1 2 3) collect tail))

 ;; ---- with VAR = EXPR ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop with base = 10
            for i from 1 to 5
            collect (+ base i)))

 ;; ---- append: fresh list of elem type ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list (list 1) (list 2)) append x))

 ;; ---- nconc: NON-fresh list (shares conses with body) ----
 (et-assert-resolve ListR<1|2>
   (cl-loop for x in (list (list 1) (list 2)) nconc x))

 ;; ---- concat ----
 (et-assert-resolve String
   (cl-loop for x in (list "a" "b") concat x))

 ;; ---- vconcat ----
 (et-assert-resolve Vector<1|2|3>
   (cl-loop for x in (list 1 2 3) vconcat x))

 ;; ---- count ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) count x))

 ;; ---- sum: Integer when summing Integers ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) sum x))

 ;; ---- sum: Number when summing Numbers ----
 (et-assert-resolve Number
   (cl-loop for x in (list 1 2.5 3) sum x))

 ;; ---- maximize ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) maximize x))

 ;; ---- always/never returns Boolean ----
 (et-assert-resolve Boolean
   (cl-loop for x in (list 1 2 3) always (integerp x)))
 (et-assert-resolve Boolean
   (cl-loop for x in (list 1 2 3) never (stringp x)))

 ;; ---- thereis: form-type | Nil ----
 (et-assert-resolve Integer|Nil
   (cl-loop for x in (list 1 2 3)
            thereis (and (integerp x) x)))

 ;; ---- return inside body ----
 (et-assert-resolve String|Nil
   (cl-loop for x in (list 1 2 3)
            if (eq x 2) return "found"))

 ;; ---- finally return overrides accumulation ----
 (et-assert-resolve String
   (cl-loop for x in (list 1 2 3)
            collect x
            finally return "done"))

 ;; ---- no accumulation returns Nil ----
 (et-assert-resolve Nil
   (cl-loop for x in (list 1 2 3) do (+ x 1)))

 ;; ---- collect into VAR: bare return is Nil, not the accumulator ----
 (et-assert-resolve Nil
   (cl-loop for x in (list 1 2 3)
            collect x into result))

 ;; ---- collect into VAR + finally return: uses accumulator ----
 (et-assert-resolve List<1|2|3>
   (cl-loop for x in (list 1 2 3)
            collect x into result
            finally return result))

 ;; ---- repeat ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop repeat 5 collect 1))

 ;; ---- while ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i from 1
            while (< i 10)
            collect i))

 ;; ---- when ... collect (conditional accumulation) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list 1 2 3)
            when (integerp x) collect x))

 ;; ---- multiple for clauses: second sees first ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for xs in (list (list 1) (list 2))
            for x in xs
            collect x))

 ;; ---- for VAR = EXPR (no then) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x = 5 repeat 3 collect x))

 ;; ---- with VAR (no = EXPR) defaults to Nil ----
 (et-assert-resolve Nil
   (cl-loop with x repeat 1 do x))

 ;; ---- for VAR to EXPR (implicit from 0) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i to 5 collect i))

 ;; ---- for VAR downfrom EXPR to EXPR by EXPR ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i downfrom 10 to 0 by 2 collect i)))


;;; ============================================================
;;; et macros

(et-define-pcase-checker et: `(,_expr ,type-spec)
  (let* ((actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    (unless (et-subtype? actual declared)
      (et-err 0 "Expected %s, found %s" (et-pp declared) (et-pp actual)))
    declared))

(et-define-pcase-checker et! `(,_expr ,type-spec)
  (let* ((actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    (when (and (not (et-never-p actual))
               (not (et-never-p declared))
               (et-never-p (et--supersect actual declared)))
      (et-err 0 "Types %s and %s have no overlap. Use et!! to supress this warning."
              (et-pp declared) (et-pp actual)))
    declared))

(et-define-pcase-checker et!! `(,_expr ,type-spec)
  (let* ((_actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    declared))


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et-types.el ends here
