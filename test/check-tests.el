;; -*- lexical-binding: t -*-

(require 'et-cache)


;;; Testing macros

(defmacro et:test-check-succeeds (name &rest body)
  (declare (indent 1) 
           (et ($arglist (name &rest body)
                         ($eval (prog1 (et Nil)
                                  (et-at-offset 2 (et-process-exprs body :check t)))))))
  `(ert-deftest ,name ()
     (et:check-setup-definitions)
     (let ((result (et-check ,@body)))
       (ert-info ((et-pp-result result))
         (should-not (et:result->failed result))))))

(defmacro et:test-check-fails (name &rest body)
  (declare (indent 1)
           (et ($arglist (name &rest body)
                         ($eval (prog1 (et Nil)
                                  (et-at-offset 2 (et-process-exprs body :check t)))))))
  `(ert-deftest ,name ()
     (et:check-setup-definitions)
     (let ((result (et-check ,@body)))
       (ert-info ((et-pp-result result))
         (should (et:result->failed result))))))


;;; while-stack

;; Nothing forcing the type of 'stack', so it gets inferred to a weird (but correct) loop type
(et:test-check-succeeds while-stack-no-force
  (let ((stack (cons 2 (cons 1 nil)))
        (finished nil)
        (i 3))
    (:assert-type= i Integer)
    (:assert-is? stack &List<Integer>)
    (while (not finished)
      (setq stack (cons i stack))
      (setq i (1+ i))
      (when (eq i (:type Integer))
        (setq finished t)))))

;; Stack gets coerced to List<Integer> because of the return value
(et:test-check-succeeds while-stack-with-return
  (et-lambda () List<Integer>
    (let ((stack (cons 2 (cons 1 nil)))
          (finished nil)
          (i 3))
      (:assert-type= i Integer)
      (:assert-type= stack List<Integer>)
      (while (not finished)
        (setq stack (cons i stack))
        (setq i (1+ i))
        (when (eq i (:type Integer))
          (setq finished t)))
      stack)))


;; Nothing to coerce stack, so it fails
(et:test-check-fails while-stack-without-return
  (et-lambda () Any
    (let ((stack (cons 2 (cons 1 nil)))
          (finished nil)
          (i 3))
      (:assert-type= i Integer)
      ;; Should error because there is a weird loop alias
      (:assert-type= stack List<Integer>)
      (while (not finished)
        (setq stack (cons i stack))
        (setq i (1+ i))
        (when (eq i (:type Integer))
          (setq finished t)))
      stack)))


;;; lambda-parameters

(et:test-check-succeeds lambda-parameters-are-inferred
  (et-defvar my-fn (or Nil (fn (&tuple Integer String) Integer)) nil)
  (let* ((b (lambda (x y)
              (:assert-type= x Integer)
              (:assert-type= y String)
              (+ x (length y)))))
    (:assert-type= b (or Nil (fn (&tuple Integer String) Integer)))
    (setq b my-fn)))

(et:test-check-fails lambda-parameters-are-checked
  (et-defvar my-fn (or Nil (fn (&tuple Integer String) Integer)) nil)
  (let* ((b (lambda (x y)
              (:assert-type= x Any)
              (+ x (length y)))))
    (setq b my-fn)))
