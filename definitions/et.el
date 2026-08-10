;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Function checkers

(et-defvar et:checker--checking-defun Nil|Var nil)

(et-defvar et-checking-defun Var|Nil nil
  "The defun currently being processed.")

(et-set-checker #'defun #'et:checker--defun)
(et-set-checker #'cl-defun #'et:checker--defun)
(et-defun et:checker--defun () EtType
  (let* ((name (cadr (et-cur-expr))))
    (when-let* ((func-type (et-symbol-func-type name))
                ((not (plist-get (et-symbol-func-props name) :skip)))
                (et-checking-defun name))
      (et-check-function-body (et-symbol-func-params name) func-type 3))
    (et-literal name)))



(et-set-checker #'lambda #'et:checker--lambda)
(et-defun et:checker--lambda () EtType
  (let* ((arglist (cadr (et-cur-expr)))
         (body (cddr (et-cur-expr)))
         (params (et-at 1 (et-parse-arglist arglist)))
         (untyped-input nil)
         (func-type
          (or (when-let* ((decls (et-at-offset 2 (et-find-and-parse-func-decls params body)))
                          (func-type (plist-get decls :definition)))
                (when (et:type-p func-type) func-type))
              ;; From recommendation
              (when-let* ((rec (et-cur-recommendation))
                          (just-fn (et-supersect rec (et AnyFn))))
                (unless (et-never-p just-fn) just-fn))
              ;; All params are Any
              (progn (setq untyped-input (et-untyped-func-input params))
                     (et-dt 'Function untyped-input (et Any)))))
         (actual-ret (et-check-function-body params func-type 2)))
    ;; If the function is untyped, then we should use the actual return type
    ;; as the function's return type
    (if untyped-input (et-dt 'Function untyped-input actual-ret) func-type)))


;;; ============================================================
;;; et checkers

(et-declare
 (@check et: ($pcase `(,spec ,_expr)
                     ($temp-var type ($eval (et-parse-type spec))
                                ($prog1 ($var type)
                                        ($expect ($var type) ($at 2))))))
 (@check et! ($pcase `(,spec ,_expr)
                     ($temp-var type ($eval (et-parse-type spec))
                                ($prog1 ($var type)
                                        ($with-rec ($var type) ($at 2)))))))

(et-set-checker #'et-defun #'et:checker--et-defun)
(et-defun et:checker--et-defun () EtType
  (let* ((name (cadr (et-cur-expr))))
    (when-let* ((func-type (et-symbol-func-type name))
                ((not (plist-get (et-symbol-func-props name) :skip)))
                (et-checking-defun name))
      (et-check-function-body (et-symbol-func-params name) func-type 4))
    (et-literal name)))


;;; ============================================================
