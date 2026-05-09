;;; et-types.el --- Typesystem for emacs lisp        -*- lexical-binding: t; -*-

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

(require 'et-check)


;;; ============================================================
;;; Definitions

(et-define-checker lambda
  (pcase-let ((`(,input ,ret) (et-checker-funcdef 1)))
    (et-dt 'Function input ret)))

(et-define-pcase-checker defun
    `(,_name . ,_body)
  (pcase-let ((`(,input ,ret) (et-checker-funcdef 2)))
    (et-dt 'Function input ret)))

(et-test
 ;; Body return type with typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   (lambda ([x Integer]) (+ x 1)))

 ;; Untyped args default to Any, body uses them
 (et-assert-resolve Function<ConsR<Any~Nil>~Any>
   (lambda (x) x))

 ;; &optional arg becomes Nil|Type
 (et-assert-resolve Function<ConsR<Integer~ConsR<Nil|String~Nil>>~Integer>
   (lambda ([x Integer] &optional [y String]) (+ x 1)))

 ;; Multiple body forms, return type is last
 (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
   (lambda ([x Integer] [y String]) (+ x 1) y))

 ;; Empty arglist
 (et-assert-resolve Function<Nil~Integer>
   (lambda () 1)))


;;; ============================================================
;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Explicit type annotation
               (and form `(,name ,(app et-parse-type type) ,_val)
                    (let _1 (setcdr form (cddr form)))
                    (let _2 (et-with-vars vars (et-checker-resolve type 1 (length vars) 1))))
               ;; Implicit type
               (and `(,name ,_val)
                    (let type (et-with-vars vars (et-checker-sub 1 (length vars) 1))))
               ;; No value (nil variable)
               (and name (pred symbolp)
                    (let type (et Nil))))
              ;; Add the var to the list
              (let var (et-new-var name (or type (et-never))))))
      . ,_body)

  (et-with-vars vars
    (et-checker-sub 2)))


;;;; dolist

(et-define-pcase-checker dolist
    `(,(or (and form `(,name ,(app et-parse-type elem-type) ,_lst)
                (let _1 (setcdr form (cddr form)))
                (let _2 (et-checker-resolve (et-alias 'ListR elem-type) 1 1)))
           (and `(,name ,_lst)
                (let elem-type (et-checker-infer (et-checker-sub 1 1) [T] ListR<T> T))))
      . ,_body)

  (et-with-vars (list (et-new-var name elem-type))
    (et-checker-sub 1)))


;;;; setq

(et-define-pcase-checker setq (and args (guard (eq 0 (mod (length args) 2))))
  (cl-loop for (var _val) on args by #'cddr
           for var-pos upfrom 1 by 2
           for type = (or (et-get-symbol-type var)
                          (et-checker-err var-pos "Assignment to free variable"))
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
           finally return acc-type))

(et-define-pcase-checker or args
  (cl-loop with acc-type = (et Nil)
           for pos upfrom 1 to (length args)
           do (cl-callf et--or-return-type acc-type (lambda () (et-checker-sub pos)))
           finally return acc-type))


;;;; if

(et-define-pcase-checker if `(,_cond ,_then . ,_else)
  (let* ((cond-type (et-checker-sub 1)))

    (et-checker-hint-narrows
     "IF:\\n%s" (et--supersect cond-type (et NonNil))
     "ELSE:\\n%s" (et--supersect cond-type (et Nil)))

    (et--or (et--and-return-type cond-type (lambda () (et-checker-sub 2)))
            (et--or-return-type cond-type (lambda () (et-checker-tail 3))))))

(et-define-pcase-checker when `(,_cond . ,then)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows "WHEN:\\n%s" (et--non-nil cond-type))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et Nil)
      (et--and-return-type cond-type (lambda () (et-checker-tail 2))))))

(et-define-pcase-checker unless `(,_cond . ,_else)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows "UNLESS:\\n%s" (et--supersect cond-type (et Nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-checker-tail 2)))))


;;; ============================================================
;;; Function types
;;;; Quoted

(et-define-pcase-checker quote `(,expr)
  (et-literal expr))

(et-test
 (et-assert-resolve Integer '1)
 (et-assert-resolve Number '1.1)
 (et-assert-resolve String '"hi")
 (et-assert-resolve Symbol 'a)
 (et-assert-error (et-root-resolve 'Integer ''1.1))
 (et-assert-error (et-root-resolve 'Integer '''1))
 (et-assert-error (et-root-resolve 'Number '''1.1))
 (et-assert-error (et-root-resolve 'String '''"hi"))
 (et-assert-error (et-root-resolve 'Symbol '''a))

 (et-assert-resolve ConsR<Any~Any> '(1 2 3))
 (et-assert-resolve ListR<Symbol> '(a b c))
 (et-assert-resolve ListR<Integer> '())
 (et-assert-error (et-root-resolve 'ListR<Integer> ''(1 2 '3)))
 (et-assert-error (et-root-resolve 'ListR<Integer> ''(1 2 3.3)))
 (et-assert-error (et-root-resolve 'ListR<Integer> '''(1 2 3)))
 (et-assert-error (et-root-resolve 'ListR<Integer> '''()))

 (et-assert-resolve ConsR<Integer~Integer> '(1 . 2))
 (et-assert-error (et-root-resolve 'ConsR<Integer~Integer> ''(1 . 2.2)))
 (et-assert-error (et-root-resolve 'ConsR<Integer~Integer> ''(1.1 . 2)))
 (et-assert-resolve ConsR<Symbol~ListR<String>> '(a "2" "3")))


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

(et-define-type-checker cons [L R] (Args L R) Cons<L~R>)

(et-test
 (et-assert-resolve ConsR<Integer~String> (cons 1 "2"))
 (et-assert-error (et-root-resolve 'ConsR<Integer~String> '(cons "1" 2)))
 (et-assert-resolve ConsR<Integer~ListR<String>> (cons 1 nil))
 (et-assert-resolve ConsR<Integer~ListR<String>> (cons 1 (cons "2" nil)))

 (et-assert-resolve ListR<Integer> (cons 1 (cons 2 nil)))
 (et-assert-error (et-root-resolve 'ListR<Integer> '(cons 1 (cons "2" nil))))
 (et-assert-error (et-root-resolve 'ListR<Integer> '(cons "1" (cons 2 nil))))
 (et-assert-error (et-root-resolve 'ListR<Integer> '(cons 1 (cons 2 t)))))


;;;;; list

(et-define-type-checker list [T] T T)

(et-test
 (et-assert-resolve ConsR<Integer~ListR<String>> (list 1 "2"))
 (et-assert-error (et-root-resolve 'ConsR<Integer~String> '(list "1" 2)))
 (et-assert-error (et-root-resolve 'ConsR<Integer~String> '(list)))

 (et-assert-resolve ListR<Integer> (list 1 2 3))
 (et-assert-resolve ListR<Integer> (list 1))
 (et-assert-error (et-root-resolve 'ListR<Integer> '(list 1 "2" 3))))


;;;;; car

(et-define-type-checker car [L] (Args (or Nil&L=Nil ConsR<L~Any>)) L)

(et-test
 (et-assert-resolve Integer (car (list 1 2.2 3)))
 (et-assert-error (et-root-resolve 'Integer '(car (list 1.1 2 3))))
 (et-assert-resolve Integer (car (cons 1 "3")))
 (et-assert-resolve ListR<Integer> (car (cons (list 1) "3")))
 (et-assert-resolve ConsR<Integer~Any> (car (cons (list 1) "3")))
 (et-assert-resolve Integer (car (car (cons (list 1) "3"))))
 (et-assert-error (et-root-resolve 'Integer '(car (car (cons (list 1.1) "3")))))

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


;;;;; cdr

(et-define-type-checker cdr [R] (Args (or Nil&R=Nil ConsR<Any~R>)) R)

(et-test
 (et-assert-resolve ListR<Number> (cdr (list 1 2.2 3)))
 (et-assert-resolve ListR<Integer> (cdr (list 1.1 2 3)))
 (et-assert-error (et-root-resolve 'ListR<Integer> '(car (list 1 2.2 3))))

 (et-assert-resolve Integer (cdr (cons "1" 2)))
 (et-assert-error (et-root-resolve 'Integer '(cdr (cons 1 "2"))))

 (et-assert-resolve ListR<Integer> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Any> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Boolean> (cdr (cons "1" (list 2))))
 (et-assert-error (et-root-resolve 'ConsR<Integer~Boolean> '(cdr (cons "1" (list 2 3)))))
 (et-assert-resolve Integer (car (cdr (cons "1" (list 2)))))

 (et-assert-resolve Boolean (cdr (cdr (cdr (list 1 2 3)))))
 (et-assert-error (et-root-resolve 'Boolean '(cdr (cdr (list 1 2 3)))))

 (et-assert-call ListR<Integer> cdr ListR<Integer>)
 (et-assert-call ListR<Integer>|String cdr ListR<Integer>|ConsR<Nil~String>)
 (et-assert-call-errors car ListR<Integer>|ConsR<String~nil>|String))


;;;;; setcar

(et-define-type-checker setcar [A] (Args A Nil|ConsW<A~Never>) A)

(et-test
 (et-typecheck-call setcar Number ConsW<Number~Number>)
 )


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


;;;;; aref

(et-define-type-checker aref [T] (Args VectorR<T>|{String&T=Integer} Integer) T)

(et-test
 (et-assert-call Integer aref String Integer)
 (et-assert-call Symbol|Integer aref (or VectorR<Symbol> String) Integer)
 (et-assert-call-errors aref (or VectorR<Symbol> String ListR<Any>) Integer))


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-type-checker ,name [T]
     (Args T)
     (or (and True (bindsof (and T ,type)))
         (and Nil (bindsof (subtract T ,type))))))

(et-define-predicate stringp String)
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


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et-types.el ends here
