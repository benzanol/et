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
;;;; Ignore certain forms

(et-define-checker declare (et Nil))


;;;; Function checkers

(et-define-pcase-checker lambda body
  (setq body (et--funcdef-inline-to-declare (copy-tree body)))
  (et-checker-function-body (et-parse-function-type body #'lambda) 1))

(et-test
 ;; Inline typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   (lambda ([x Integer]) (+ x 1)))

 ;; Untyped args default to Any
 (et-assert-resolve Function<ConsR<Any~Nil>~Any>
   (lambda (x) x))

 ;; &optional — cdr is Nil|ConsR<String~Nil>
 (et-assert-resolve (Function ConsR<Integer~Nil|ConsR<String~Nil>> Integer)
   (lambda ([x Integer] &optional [y String]) x))

 ;; Multiple body forms — return type is last
 (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
   (lambda ([x Integer] [y String]) (+ x 1) y))

 ;; Empty arglist
 (et-assert-resolve Function<Nil~Integer>
   (lambda () 1))

 ;; &rest
 (et-assert-resolve (Function ConsR<Integer~ListR<Any>> Integer)
   (lambda ([x Integer] &rest args) x))
 (et-assert-resolve (Function ConsR<Any~ListR<Any>> ListR<Any>)
   (lambda (x &rest args) args)))

(et-test
 ;; Body return type with typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   (lambda ([x Integer]) (+ x 1)))

 ;; Untyped args default to Any, body uses them
 (et-assert-resolve Function<ConsR<Any~Nil>~Any>
   (lambda (x) x))

 ;; &optional arg becomes Nil|Type
 (et-assert-resolve Function<ConsR<Integer~Nil|ConsR<String~Nil>>~Integer>
   (lambda ([x Integer] &optional [y String]) (+ x 1)))

 ;; Multiple body forms, return type is last
 (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
   (lambda ([x Integer] [y String]) (+ x 1) y))

 ;; Empty arglist
 (et-assert-resolve Function<Nil~Integer>
   (lambda () 1)))

;; Return type annotation tests
(et-test
 ;; Inline -> return type: uses declared type
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number (+ x 1)))

 ;; Inline -> with typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number x))

 ;; Inline -> is stripped from compiled output
 (let* ((result (et--check '(lambda (x) -> Integer 1))))
   (equal (et-result-compiled result) '(lambda (x) 1)))

 ;; Inline -> with body type mismatch produces error
 (et-assert-resolve-errors
  (lambda ([x Integer]) -> String x))

 ;; Inline -> with empty arglist
 (et-assert-resolve Function<Nil~Number>
   (lambda () -> Number 1))

 ;; Inline -> with multiple body forms
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number "ignored" x)))


;;;; Var checkers

(defun et--parse-defvar-docstring (docstring)
  "Parse a @et-type annotation from DOCSTRING, returning a type or nil."
  (when (and (stringp docstring) (string-match "@et-type" docstring))
    (when-let* ((type-expr (ignore-errors (car (read-from-string docstring (match-end 0))))))
      (condition-case _err (et-parse-type type-expr)
        (error nil)))))

(et-define-pcase-checker defvar
    `(,(and (pred symbolp) name) .
      ,(or 'nil `(,val . ,(or 'nil `(,(and docstring (pred stringp)))))))
  (let* ((declared-type (et--parse-defvar-docstring docstring))
         (value-type (or (when val (et-checker-sub 2)) (et Nil))))

    (when declared-type
      (or (et-subtype? value-type declared-type)
          (et-checker-err 2 "Initial value type %s does not satisfy declared type %s"
                          (et-pp value-type) (et-pp declared-type))))

    (setcar et--checker-expr 'defvar)
    (put name 'et-variable-type (or declared-type value-type))))

(defmacro et-defvar (&rest args)
  "Define a type-checked function.

\(fn NAME [INIT-VAL] [DOCSTRING])"
  (declare (doc-string 3) (indent 2))

  (let* ((result (et--check (cons #'defvar args))))
    (et-show-result-errors result)
    (et-result-compiled result)))


;;; ============================================================
;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Explicit type annotation
               (and form `(,name ,type-spec ,_val)
                    (let type (et-parse-type type-spec))
                    (let _1 (setcdr form (cddr form)))
                    (let _2 (et-with-vars vars (et-checker-resolve type 1 (length vars) 1))))
               ;; Implicit type
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
        (et-typecheck
         (let* ((a String|Number 4))
           (when (stringp a) a))))
 (et-subtype? (et String)
              (et-typecheck
               (let* ((a String|Number 4))
                 (if (stringp a) a "hello!")))))


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
       (et-checker-err "No function type for `%s'" sym)))

    ;; Anything else is invalid
    (_ (et-checker-err "Invalid argument to function: %s" inner))))

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


;;;;; list

(et-define-arb-checker list (input)
  (et--freshen-type input))

(et-test
 (et-assert-resolve ConsR<Integer~ListR<String>> (list 1 "2"))
 (et-assert-no-resolve ConsR<Integer~String> (list "1" 2))
 (et-assert-no-resolve ConsR<Integer~String> (list))

 (et-assert-resolve ListR<Integer> (list 1 2 3))
 (et-assert-resolve ListR<Integer> (list 1))
 (et-assert-no-resolve ListR<Integer> (list 1 "2" 3)))


;;;;; car

(et-define-type-checker car [L] (Args (or Nil&L=Nil ConsR<L~Any>)) L)

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


;;;;; cdr

(et-define-type-checker cdr [R] (Args (or Nil&R=Nil ConsR<Any~R>)) R)

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

(et-test
 (et-assert-call-errors alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)
 (et-assert-call 2|Nil alist-get Integer AList<1~2>)
 (et-assert-resolve 2|4|Nil (alist-get 4 (list (cons 1 2) (cons 3 4)))))


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


;;;; Funcall

(et-define-checker apply
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tailed-tuple 'ConsR (et-checker-remaining 2)))
         (result (et--funcall func-type args-type)))
    (or result
        (et-checker-err "Cannot apply %s with args %s" (et-pp func-type) (et-pp args-type)))))

(et-define-checker funcall
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et--funcall func-type args-type)))
    (or output-type
        (et-checker-err "Cannot call %s with args %s" (et-pp func-type) (et-pp args-type)))))

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
                      (lambda (fmt &rest args) (apply #'et-checker-err 0 fmt args)))))


(et-define-pcase-checker plist-put `(,_plist ,_key ,_val)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2))
         (val-type (et-checker-sub 3))
         (existing-val-type
          (et--plist-lookup plist-type key-type
                            (lambda (fmt &rest args) (apply #'et-checker-err 0 fmt args)))))
    (when existing-val-type
      (if (et-subtype? val-type existing-val-type)
          plist-type
        (et-checker-err 3 "Expected %s, found %s" (et-pp existing-val-type) (et-pp val-type))))))


;;;; cl-loop
;; The following was generated by Claude Opus 4.6


;;;;; Token stream
;;
;; STATE is a cons (INDEX . BODY).  The et--checker-expr position of
;; the current token is (1+ INDEX).

(defun et--cll-peek (state)
  (nth (car state) (cdr state)))

(defun et--cll-advance! (state)
  (prog1 (et--cll-peek state) (cl-incf (car state))))

(defun et--cll-pos (state)
  (1+ (car state)))

(defun et--cll-done-p (state)
  (>= (car state) (length (cdr state))))

(defconst et--cll-clause-keywords
  '(for as with do doing initially finally
        collect collecting append appending nconc nconcing
        concat vconcat count counting sum summing
        maximize maximizing minimize minimizing
        repeat while until always never thereis
        if when unless named return))

(defun et--cll-clause-kw-p (x)
  (and (symbolp x) (memq x et--cll-clause-keywords)))

(defun et--cll-canonical-accum (kw)
  (pcase kw
    ((or 'collect 'collecting) 'collect)
    ((or 'append 'appending) 'append)
    ((or 'nconc 'nconcing) 'nconc)
    ('concat 'concat)   ('vconcat 'vconcat)
    ((or 'count 'counting) 'count)
    ((or 'sum 'summing) 'sum)
    ((or 'maximize 'maximizing) 'maximize)
    ((or 'minimize 'minimizing) 'minimize)))

(defun et--cll-skip-non-kw (state &optional extra-stops)
  "Advance past non-keyword tokens, returning their positions."
  (cl-loop until (or (et--cll-done-p state)
                     (et--cll-clause-kw-p (et--cll-peek state))
                     (memq (et--cll-peek state) extra-stops))
           collect (prog1 (et--cll-pos state) (et--cll-advance! state))))

(defun et--cll-consume-expr (state)
  "Consume one expression, returning its position."
  (prog1 (et--cll-pos state) (et--cll-advance! state)))


;;;;; Phase 1: Collect variables and clause descriptors
;;
;; Variable descriptors: (NAME KIND . DATA)
;; Clause descriptors: plists with :kind

(defun et--cll-collect (state)
  "Walk STATE, returning (VARS . CLAUSES)."
  (let* ((vars nil) (clauses nil))
    (while (not (et--cll-done-p state))
      (let* ((kw (et--cll-advance! state)))
        (pcase kw
          ((or 'for 'as)
           (push (et--cll-collect-for state) vars))

          ('with
           (let* ((var (et--cll-advance! state))
                  (val-pos (when (eq (et--cll-peek state) '=)
                             (et--cll-advance! state)
                             (et--cll-consume-expr state))))
             (push (list var 'with val-pos) vars)))

          ((or 'do 'doing)
           (push (list :kind 'do :exprs (et--cll-skip-non-kw state)) clauses))

          ('initially
           (when (eq (et--cll-peek state) 'do) (et--cll-advance! state))
           (et--cll-skip-non-kw state))

          ('finally
           (pcase (et--cll-peek state)
             ('return (et--cll-advance! state)
                      (push (list :kind 'finally-return :pos (et--cll-consume-expr state)) clauses))
             (_ (when (eq (et--cll-peek state) 'do) (et--cll-advance! state))
                (push (list :kind 'do :exprs (et--cll-skip-non-kw state)) clauses))))

          ('return
           (push (list :kind 'return :pos (et--cll-consume-expr state)) clauses))

          ('repeat
           (push (list :kind 'repeat :pos (et--cll-consume-expr state)) clauses))

          ((and (or 'while 'until 'always 'never 'thereis) k)
           (push (list :kind k :pos (et--cll-consume-expr state)) clauses))

          ((guard (et--cll-canonical-accum kw))
           (push (et--cll-collect-accum state kw) clauses))

          ((and (or 'if 'when 'unless) k)
           (push (et--cll-collect-conditional state k) clauses))

          ('named (et--cll-advance! state))

          (_ (error "Unknown cl-loop keyword: %s" kw)))))

    (cons (nreverse vars) (nreverse clauses))))


;;;;; For-clause

(defun et--cll-collect-for (state)
  "Collect a for-clause, returning (NAME KIND . DATA)."
  (let* ((var (et--cll-advance! state))
         (spec (et--cll-peek state)))

    (pcase spec
      ((or 'from 'upfrom 'downfrom)
       (et--cll-advance! state)
       (et--cll-consume-expr state) ; from-expr
       (et--cll-eat-arith-tail state)
       (list var 'arith))

      ((or 'to 'upto 'downto 'above 'below)
       (et--cll-eat-arith-tail state)
       (list var 'arith))

      ('= (et--cll-advance! state)
          (let* ((init-pos (et--cll-consume-expr state))
                 (then-pos (when (eq (et--cll-peek state) 'then)
                             (et--cll-advance! state)
                             (et--cll-consume-expr state))))
            (list var 'equals init-pos then-pos)))

      ((and (or 'in 'on 'in-ref) k)
       (et--cll-advance! state)
       (let* ((pos (et--cll-consume-expr state)))
         (when (eq (et--cll-peek state) 'by)
           (et--cll-advance! state) (et--cll-advance! state))
         (list var k pos)))

      ((and (or 'across 'across-ref) k)
       (et--cll-advance! state)
       (list var k (et--cll-consume-expr state)))

      ('being
       (et--cll-advance! state)
       (when (memq (et--cll-peek state) '(the each)) (et--cll-advance! state))
       (let* ((being-kw (et--cll-advance! state))
              (of-pos nil))
         (while (memq (et--cll-peek state) '(of of-ref from to using))
           (pcase (et--cll-advance! state)
             ((or 'of 'of-ref) (setq of-pos (et--cll-consume-expr state)))
             ((or 'from 'to) (et--cll-consume-expr state))
             ('using (et--cll-advance! state))))
         (list var 'being being-kw of-pos)))

      (_ (list var 'arith)))))

(defun et--cll-eat-arith-tail (state)
  "Consume optional to/by tokens."
  (when (memq (et--cll-peek state) '(to upto downto above below))
    (et--cll-advance! state) (et--cll-consume-expr state))
  (when (eq (et--cll-peek state) 'by)
    (et--cll-advance! state) (et--cll-consume-expr state)))


;;;;; Accumulation

(defun et--cll-collect-accum (state kw)
  (let* ((canonical (et--cll-canonical-accum kw))
         (expr-pos (et--cll-consume-expr state))
         (into-var (when (eq (et--cll-peek state) 'into)
                     (et--cll-advance! state) (et--cll-advance! state))))
    (list :kind 'accum :accum canonical :pos expr-pos :into into-var)))


;;;;; Conditional

(defconst et--cll-inner-stops '(and else end))

(defun et--cll-collect-conditional (state cond-kind)
  (let* ((cond-pos (et--cll-consume-expr state))
         (then (et--cll-collect-branch state))
         (else-branch (when (eq (et--cll-peek state) 'else)
                        (et--cll-advance! state)
                        (et--cll-collect-branch state))))
    (when (eq (et--cll-peek state) 'end) (et--cll-advance! state))
    (list :kind 'cond :cond-kind cond-kind :cond-pos cond-pos
          :then then :else else-branch)))

(defun et--cll-collect-branch (state)
  (let* ((clauses (list (et--cll-collect-inner state))))
    (while (eq (et--cll-peek state) 'and)
      (et--cll-advance! state)
      (push (et--cll-collect-inner state) clauses))
    (nreverse clauses)))

(defun et--cll-collect-inner (state)
  (pcase (et--cll-peek state)
    ((and (or 'if 'when 'unless) k)
     (et--cll-advance! state)
     (et--cll-collect-conditional state k))

    ((or 'do 'doing)
     (et--cll-advance! state)
     (list :kind 'do :exprs (et--cll-skip-non-kw state et--cll-inner-stops)))

    ('return
     (et--cll-advance! state)
     (list :kind 'return :pos (et--cll-consume-expr state)))

    ((and (guard (et--cll-canonical-accum (et--cll-peek state))) _)
     (let* ((kw (et--cll-advance! state)))
       (et--cll-collect-accum state kw)))

    (_ (list :kind 'do :exprs (list (et--cll-consume-expr state))))))


;;;;; Phase 2: Type checking
;;;;; Infer helpers
;;
;; Pre-compiled matchers for inferring element types from containers.
;; `et-checker-infer' is a macro (compiles at expansion time), but our
;; checker runs at runtime, so we pre-compile and call `et--infer'.

(defvar et--cll-list-matcher (et-parse-matcher 'ListR<T> '(T)))
(defvar et--cll-list-output (et-parse-structure 'T '(T)))
(defvar et--cll-vec-matcher (et-parse-matcher 'VectorR<T> '(T)))
(defvar et--cll-vec-output (et-parse-structure 'T '(T)))

(defun et--cll-infer-list-elem (type)
  "Infer the element type of a list TYPE, or nil."
  (et--infer et--cll-list-matcher type et--cll-list-output))

(defun et--cll-infer-vec-elem (type)
  "Infer the element type of a vector TYPE, or nil."
  (et--infer et--cll-vec-matcher type et--cll-vec-output))


;;;;; Variable types

(defun et--cll-var-type (desc)
  "Determine variable type from descriptor DESC = (NAME KIND . DATA)."
  (pcase (cdr desc)
    (`(with ,val-pos)
     (if val-pos (et-checker-sub val-pos) (et Nil)))

    (`(arith) (et Integer))

    (`(equals ,init-pos ,then-pos)
     (let* ((init (et-checker-sub init-pos)))
       (if then-pos (et--or init (et-checker-sub then-pos)) init)))

    (`(,(or 'in 'in-ref) ,pos)
     (or (et--cll-infer-list-elem (et-checker-sub pos)) (et Any)))

    (`(on ,pos) (et-checker-sub pos))

    (`(,(or 'across 'across-ref) ,pos)
     (let* ((at (et-checker-sub pos)))
       (if (et-subtype? at (et String)) (et Integer)
         (or (et--cll-infer-vec-elem at) (et Any)))))

    (`(being ,being-kw ,of-pos)
     (et--cll-being-type being-kw of-pos))

    (_ (et Any))))

(defun et--cll-being-type (being-kw of-pos)
  (pcase being-kw
    ((or 'elements 'element)
     (if-let* ((of-pos) (ot (et-checker-sub of-pos)))
         (if (et-subtype? ot (et String)) (et Integer)
           (or (et--cll-infer-list-elem ot)
               (et--cll-infer-vec-elem ot)
               (et Any)))
       (et Any)))
    ((or 'symbols 'symbol 'present-symbols 'external-symbols) (et Symbol))
    ((or 'hash-keys 'hash-key 'hash-values 'hash-value) (et Any))
    ((or 'key-codes 'key-code) (et Integer))
    ((or 'key-seqs 'key-seq) (et-alias 'VectorR (et Integer)))
    (_ (when of-pos (et-checker-sub of-pos)) (et Any))))


;;;;; Accumulation return type

(defun et--cll-accum-type (kind elem-type)
  (pcase kind
    ('collect  (et-alias 'List elem-type))
    ((or 'append 'nconc)
     (et-alias 'List (or (et--cll-infer-list-elem elem-type) (et Any))))
    ('concat   (et String))
    ('vconcat  (et-alias 'VectorR (or (et--cll-infer-vec-elem elem-type) elem-type)))
    ('count    (et Integer))
    ('sum      (if (et-subtype? elem-type (et Integer)) (et Integer) (et Number)))
    ((or 'maximize 'minimize) (et--or elem-type (et Nil)))))


;;;;; Check clauses

(defun et--cll-check-clause (clause accums)
  "Check CLAUSE, mutating ACCUMS = (FREE-TYPES . (:return R :ant A))."
  (pcase (plist-get clause :kind)
    ('do (dolist (p (plist-get clause :exprs)) (et-checker-sub p)))

    ('finally-return
     (plist-put (cdr accums) :return (et-checker-sub (plist-get clause :pos))))

    ('return (et-checker-sub (plist-get clause :pos)))

    ('repeat (et-checker-resolve 'Integer (plist-get clause :pos)))

    ((or 'while 'until) (et-checker-sub (plist-get clause :pos)))

    ((or 'always 'never 'thereis)
     (et-checker-sub (plist-get clause :pos))
     (plist-put (cdr accums) :ant t))

    ('accum
     (let* ((elem (et-checker-sub (plist-get clause :pos)))
            (rt (et--cll-accum-type (plist-get clause :accum) elem)))
       (unless (plist-get clause :into) (push rt (car accums)))))

    ('cond (et--cll-check-cond clause accums))))

(defun et--cll-check-cond (clause accums)
  (let* ((cond-type (et-checker-sub (plist-get clause :cond-pos)))
         (non-nil-binds (et--type-binds (et--non-nil cond-type)))
         (nil-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (check (lambda (branch binds)
                  (et-with-narrow-binds binds
                    (dolist (c branch) (et--cll-check-clause c accums))))))
    (pcase (plist-get clause :cond-kind)
      ((or 'if 'when)
       (funcall check (plist-get clause :then) non-nil-binds)
       (funcall check (plist-get clause :else) nil-binds))
      ('unless
          (funcall check (plist-get clause :then) nil-binds)
        (funcall check (plist-get clause :else) non-nil-binds)))))


;;;;; Main checker

(et-define-checker cl-loop
  (let* ((state (cons 0 (cdr et--checker-expr)))
         (collected (et--cll-collect state))
         (var-descs (car collected))
         (clauses (cdr collected))
         (et-vars (cl-loop for d in var-descs
                           collect (et-new-var (car d) (et--cll-var-type d))))
         (accums (cons nil (list :return nil :ant nil))))
    (et-with-vars et-vars
      (dolist (c clauses) (et--cll-check-clause c accums)))
    (pcase nil
      ((guard (plist-get (cdr accums) :return)) (plist-get (cdr accums) :return))
      ((guard (plist-get (cdr accums) :ant)) (et Boolean))
      ((guard (car accums)) (apply #'et--or (car accums)))
      (_ (et Nil)))))


;;;;; Tests

(defmacro et-assert-loop (type &rest loop-body)
  (declare (indent 1))
  `(et-assert-resolve ,type (cl-loop ,@loop-body)))

(et-test
 ;; Arithmetic for variants
 (et-assert-loop Nil for i from 1 to 10 do (+ i 1))
 (et-assert-loop ListR<Integer> for i from 1 to 10 collect i)
 (et-assert-loop ListR<Integer> for i upfrom 0 to 5 collect i)
 (et-assert-loop ListR<Integer> for i downfrom 10 to 1 collect i)
 (et-assert-loop ListR<Integer> for i from 1 to 10 by 2 collect i)
 (et-assert-loop ListR<Integer> for i to 10 collect i)
 ;; Bare var
 (et-assert-loop ListR<Integer> for i repeat 5 collect i)

 ;; for = / = then
 (et-assert-loop ListR<Integer> for x = 0 repeat 5 collect x)
 (et-assert-loop ListR<Integer|String> for x = 0 then "s" repeat 5 collect x)

 ;; for in/on/across
 (et-assert-loop ListR<Integer> for x in (list 1 2 3) collect x)
 (et-assert-loop ListR<ListR<Integer>> for x on (list 1 2 3) collect x)
 (et-assert-loop ListR<Integer> for x across (vector 1 2 3) collect x)
 (et-assert-loop ListR<Integer> for c across "hello" collect c)

 ;; All accumulators
 (et-assert-loop Integer for i from 1 to 10 sum i)
 (et-assert-loop Integer for i from 1 to 10 count (> i 5))
 (et-assert-loop String for s in (list "a" "b") concat s)
 (et-assert-loop VectorR<Integer> for i from 1 to 3 vconcat (vector i))
 (et-assert-loop Integer|Nil for i from 1 to 10 maximize i)
 (et-assert-loop Integer|Nil for i from 1 to 10 minimize i)
 (et-assert-loop ListR<Integer> for x in (list (list 1) (list 2)) append x)
 (et-assert-loop ListR<Integer> for x in (list (list 1) (list 2)) nconc x)

 ;; into -> nil return
 (et-assert-loop Nil for i from 1 to 10 collect i into result)

 ;; with
 (et-assert-loop ListR<Integer> with acc = 0 for i from 1 to 5 collect (+ acc i))
 (et-assert-loop Nil with x for i from 1 to 5 do (+ i 1))

 ;; finally return
 (et-assert-loop String for i from 1 to 10 collect i finally return "done")

 ;; Conditionals
 (et-assert-loop ListR<Integer> for i from 1 to 10 when (> i 5) collect i)
 (et-assert-loop ListR<Integer> for i from 1 to 10 unless (> i 5) collect i)
 (et-assert-loop ListR<Integer|String>
   for i from 1 to 10 if (> i 5) collect i else collect "lo")
 (et-assert-loop ListR<Integer|String>
   for i from 1 to 10 if (> i 5) collect i and do (+ i 1) else collect "lo")
 (et-assert-loop ListR<Integer|String|@other>
   for i from 1 to 10
   if (> i 8) collect i
   else if (> i 4) collect "mid"
   else collect 'other)

 ;; always/never/thereis
 (et-assert-loop Boolean for i from 1 to 10 always (> i 0))
 (et-assert-loop Boolean for i from 1 to 10 never (> i 100))
 (et-assert-loop Boolean for i from 1 to 10 thereis (> i 5))

 ;; repeat / while / until
 (et-assert-loop ListR<Integer> repeat 5 collect 1)
 (et-assert-loop Nil for i from 1 while (> i 0) do (+ i 1))

 ;; Multiple for clauses
 (et-assert-loop ListR<ConsR<Integer~String>>
   for i from 1 to 3 for s in (list "a" "b" "c") collect (cons i s))

 ;; Return inside conditional
 (et-assert-loop ListR<Integer>
   for i from 1 to 10 collect i if (> i 5) return nil)

 ;; Being
 (et-assert-loop ListR<Integer>
   for x being the elements of (list 1 2 3) collect x)
 (et-assert-loop ListR<Symbol> for s being the symbols collect s)

 ;; Named
 (et-assert-loop Nil named my-loop for i from 1 to 3 do (+ i 1))

 ;; initially/finally do
 (et-assert-loop Nil
   for i from 1 to 10 initially (+ 1 2) do (+ i 1) finally (+ 3 4))

 ;; Mixed accumulator (one into, one free)
 (et-assert-loop ListR<Integer>
   for i from 1 to 10 sum i into total collect i)

 ;; Conditional do
 (et-assert-loop Nil for i from 1 to 10 when (> i 5) do (+ i 1)))


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et-types.el ends here
