;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Cl-loop

(et-defun et:def--clloop-parse-clauses (exprs: Sexps) Cons<Sexps~Sexps>|Nil
  (when-let* ((result (et:def--clloop-parse-2 exprs)))
    (let* ((rest (car result))
           (clause (take (- (length exprs) (length rest)) exprs)))
      (if (eq 'and (car rest))
          (when-let* ((rec (et:def--clloop-parse-clauses (cdr rest))))
            (cons (cons clause (car rec)) (cdr rec)))
        (cons (list clause) rest)))))

(pcase-defmacro et:def--clloop-clauses (clauses rest)
  (list 'app #'et:def--clloop-parse-clauses
        (list '\` (cons (list '\, rest) (list '\, clauses)))))

(defmacro et:def--clloop-pcase (body &rest pats)
  (declare (indent 1))
  (cl-loop for (pat chk) in pats
           for expanded = (macroexpand-all `(et-chk ,chk))
           collect `(,pat (cons rest (lambda () ,expanded))) into exprs
           finally return `(pcase ,body ,@exprs)))

(et-defvar et:def--clloop-vars Alist<Var~EtType> nil)

(et-defun et:def--clloop-parse-2 (body: Sexps) Cons<Sexps~EtCheckFn>|Nil
  (let* ((rest nil))
    (et:def--clloop-pcase body
      ;; Empty
      ('nil ($nil))
      ;; do
      (`(do ,expr . ,rest)
       ($progn ($exp expr) ($clloop-1 rest)))
      (`(return ,expr . ,rest)
       ($progn ($exp expr) ($clloop-1 rest))) ;todo
      ;; Assignment
      (`(for ,pat = ,expr1 . ,(or `(then . ,rest) rest))
       ($cl-bind pat ($exp expr1) ($clloop-1 rest)))
      ;; Looping
      (`(for ,var ,(or 'from 'upfrom 'downfrom) ,start
             ,(or 'to 'upto 'downto 'above 'below) ,end
             . ,(or `(by ,by . ,rest) rest))
       ;; todo: start/by can be non-Integers, but if so, neither is var
       ($progn ($expect ($type Integer) ($exp start))
               ($expect ($type Number) ($exp end))
               ($if-eval by ($expect ($type Integer) ($exp by)) ($nil))
               ($bind [_ var ($type Integer)] ($clloop-1 rest))))
      (`(for ,pat ,(and in (or 'in 'on 'in-ref)) ,list
             . ,(or `(by ,by . ,rest) rest))
       ($cl-bind pat ($infer [T] &List<T> T ($exp list)) ($clloop-1 rest)))
      (`(for ,pat ,(and across (or 'across 'across-ref)) ,arr . ,rest)
       ($cl-bind pat ($infer [T] ArefSeq<T> T ($exp arr)) ($clloop-1 rest)))
      (`(repeat ,times . ,rest)
       ($progn ($expect ($type Integer) ($exp times)) ($clloop-1 rest)))
      (`(while ,cond . ,rest) ($loop ($if ($exp cond) ($exp rest) ($nil)))) ;todo
      (`(until ,cond . ,rest) ($loop ($if ($exp cond) ($nil) ($exp rest)))) ;todo
      ;; Existence
      (`(,(or 'always 'never 'thereis) ,expr) ($progn ($exp expr) ($type Boolean)))
      ;; Conditional: todo
      (`(if ,cond ,(et:def--clloop-clauses
                    thens
                    (or `(else . ,(et:def--clloop-clauses elses rest))
                        rest)))
       ($if ($exp cond) ($nil) ($nil)))
      (`(when ,cond . ,(et:def--clloop-clauses thens rest))
       ($nil))
      (`(unless ,cond . ,(et:def--clloop-clauses elses rest))
       ($nil))
      ;; Collecting
      (`(collect ,expr into ,(and (pred symbolp) var) . ,rest)
       ($eval (if-let* ((exist (alist-get var et:def--clloop-vars)))
                  (et-chk ($expect ($infer [T] List<T> T ($eval exist)) ($exp expr))
                          ($clloop-1 rest))
                (let* ((vartype (et-reify-type (et List ,(et-chk ($exp expr))))))
                  (setf (alist-get var et:def--clloop-vars) vartype)
                  (et-chk ($bind [_ var ($eval vartype)]
                                 ($clloop-1 rest)))))))
      (`(collect ,expr)
       ($infer [T] T List<T> ($exp expr)))
      ;; Final
      (`(finally return ,expr)
       ($exp expr))
      (`(finally . ,(or `(do . ,exprs) exprs))
       ($prog1 ($nil) ($exps exprs))))))


(et-defun et:def--clloop-parse-1 (body: Sexps) EtType
  (funcall (cdr (or (et:def--clloop-parse-2 body)
                    (error "Failed to parse cl-loop")))))

(et-define-check-macro $clloop-1 (body)
  `(et:def--clloop-parse-1 ,body))

(et-defun et:def--clloop-parse (body: Sexps) EtType
  (let* ((et:def--clloop-vars nil))
    (et:def--clloop-parse-1 body)))

(et-declare
 (@check cl-loop
         ($eval (et:def--clloop-parse (cdr (et-cur-expr))))))


;;; ============================================================
