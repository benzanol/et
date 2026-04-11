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

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/my-packages/typesystem")
  (require 'et))


;;; ============================================================
;;; Control flow
;;;; let

(et-define-checker let* (varlist &rest _body)
  ;; Process let forms
  (cl-loop
   with let-binds-rev = nil
   for form in varlist
   for idx upfrom 0
   do
   (et-with-path (list 1 idx)
     (pcase form
       ;; Binding with a type annotation
       (`(,var ,type ,val)
        ;; Parse the type
        (et-with-path (list 1) (setq type (et-parse type)))
        ;; Ensure the value fits the type
        (et-with-binds let-binds-rev (et-with-path (list 2) (et-resolve type)))
        ;; Push the binding
        (setq et--current-expr (list var val))
        (push (cons var type) let-binds-rev))

       ;; Binding with no type annotation
       (`(,var ,_val)
        (let ((type (et-with-binds let-binds-rev (et-with-path (list 1) (et-check)))))
          (push (cons var type) let-binds-rev)
          (et-warn '(0) "%s: %s" var (et-pp type))))

       (wrong (error "Invalid let binding: %s" wrong))))

   finally return
   (et-with-binds let-binds-rev
     (et-check-tail 2))))


;;;; dolist

(et-define-checker dolist (spec &rest)
  (let (variable type)
    (pcase spec
      ;; With explicit type
      (`(,var ,etype ,_val)
       (et-with-path `(1 1) (setq type (et-parse etype)))
       (setq variable var)
       ;; Ensure the value fits the type
       (et-with-path `(1 2) (et-resolve (et-alias :List type))))

      ;; With implicit type
      (`(,var ,_val)
       (setq variable var)
       (et-with-path `(1 1)
         (let* ((list-type (et-check))
                (infer (et-infer-subtype [elem] (et-alias :List elem) list-type)))
           (unless infer (error "Expected list, found %s" (et-pp list-type)))
           (setq type (car infer))))
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
           do (or (et-subtype? type (et-dt :number))
                  (et-with-path (list pos)
                    (error "Argument must be a number, got %s" (et-pp type))))
           do (setq is-integer (and is-integer (et-subtype? type (et-dt :integer))))
           finally return (et-dt (if is-integer :integer :number))))

(et-define-checker + (&rest args) (et--check-arithmetic-function args))
(et-define-checker - (&rest args) (et--check-arithmetic-function args))
(et-define-checker * (&rest args) (et--check-arithmetic-function args))
(et-define-checker / (&rest args) (et--check-arithmetic-function args))
(et-define-checker 1+ (arg) (et--check-arithmetic-function (list arg)))
(et-define-checker 1- (arg) (et--check-arithmetic-function (list arg)))


;;;; cons/list

(et-define-checker cons (_lval _rval)
  (et-dt :cons
         (et-check-path 1)
         (et-check-path 2)))


(et-define-checker list (&rest args)
  (cl-loop with type = (et-literal nil)
           for idx downfrom (length args) to 1
           do (setq type (et-dt :cons (et-check-path idx) type))
           finally return type))


;;;; car/cdr

(et-define-type-checker car [:T]
  (:or :infer<T=nil> (:cons :T :any))
  :T)

(et-define-type-checker cdr [:T]
  (:or :infer<T=nil> (:cons :any :T))
  :T)


(et-define-checker cdr (_expr)
  (let* ((type (et-check-path 1))
         (infer (et-infer-supertype [cdr]
                  (et-raw-or (et-dt :cons (et-any) cdr)
                             (et-dt :infer-supertype 'cdr (et-nil)))
                  type)))
    (or (car infer) (error "Expected cons or nil, got %s" (et-pp type)))))


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-checker ,name (_expr)
     (let* ((type ,type)
            (expr-type (et-check-path 1))
            (t-case (et-and expr-type type))
            (nil-case (et-subtract expr-type type))
            (t-type (if (et-never? t-case) (et-never)
                      (et--replace-type-binds (et-literal t) (et--type-binds t-case))))
            (nil-type (if (et-never? nil-case) (et-never)
                        (et--replace-type-binds (et-nil) (et--type-binds nil-case)))))
       (et-or t-type nil-type))))


(et-define-predicate stringp (et-dt :string))
(et-define-predicate numberp (et-dt :number))
(et-define-predicate integerp (et-dt :integer))
(et-define-predicate consp (et-dt :cons (et-any) (et-any)))
;; listp does not technically check if it is a valid list
(et-define-predicate listp (et-or (et-dt nil) (et-dt :cons (et-any) (et-any))))
(et-define-predicate null (et-nil))
(et-define-predicate not (et-nil))


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et.el ends here
