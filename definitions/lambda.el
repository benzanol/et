;;; lambda.el --- Tests for lambda expressions -*- lexical-binding: t; -*-
;;; Code:

(et-test
 (et-assert-resolve Function<Args<Integer>~String>
   (lambda (x)
     (declare (et (x Integer) (@return String)))
     (format "%s" x)))

 (et-assert-resolve Function<Args<Integer>~String>
   (et: Function<Args<Integer>~String>
     (lambda (x) (format "%s" x))))

 (et-assert-resolve-errors
  (et: Function<Args<Integer>~String>
    (lambda (x) x)))

 (et-assert-resolve Function<Args<Any>~Any>
   (lambda (x) x)))


;;; lambda.el ends here
