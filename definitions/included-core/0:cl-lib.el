;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Cl-loop

(et-defun et-clloop-parse-clauses (exprs: Sexps) Cons<Sexps~Sexps>|Nil
  (when-let* ((result (et-parse-clloop exprs)))
    (let* ((rest (car result))
           (clause (take (- (length exprs) (length rest)) exprs)))
      (if (eq 'and (car rest))
          (when-let* ((rec (et-clloop-parse-clauses (cdr rest))))
            (cons (cons clause (car rec)) (cdr rec)))
        (cons (list clause) rest)))))

(pcase-defmacro et-clloop-clauses (clauses rest)
  (list 'app #'et-clloop-parse-clauses
        (list '\` (cons (list '\, rest) (list '\, clauses)))))

(defmacro et-clloop-pcase (body &rest pats)
  (declare (indent 1))
  (cl-loop for (pat chk) in pats
           collect `(,pat (cons rest (lambda () (et-chk ,chk)))) into exprs
           finally return `(pcase ,body ,@exprs)))

(et-defun et-clloop-parse (body) Cons<Sexps~fn>|Nil
  (let* ((rest nil))
    (et-clloop-pcase body
      ;; Empty
      ('nil ($nil))
      ;; Assignment
      (`(for ,var = ,expr1 . ,(or `(then . ,rest) rest))
       ($bind [v var ($exp expr1)]
              ($recurse rest)))
      ;; Looping
      (`(for ,var ,(or 'from 'upfrom 'downfrom) ,start
             ,(or 'to 'upto 'downto 'above 'below) ,end
             . ,(or `(by ,by . ,rest) rest))
       ;; todo: start/by can be non-Integers, but if so, neither is var
       ($progn ($expect ($type Integer) ($exp start))
               ($expect ($type Number) ($exp end))
               ($if-eval by ($expect ($type Integer) ($exp by)) ($nil))
               ($bind [_ var ($type Integer)] ($recurse rest))))
      (`(for ,var ,(and in (or 'in 'on 'in-ref)) ,list
             . ,(or `(by ,by . ,rest) rest))
       ($bind [_ var ($infer [T] &List<T> T ($exp list))] ($recurse rest)))
      (`(for ,var ,(and across (or 'across 'across-ref)) ,arr . ,rest)
       ($bind [_ var ($infer [T] ArefSeq<T> T ($exp arr))] ($recurse rest)))
      (`(repeat ,times . ,rest)
       ($progn ($expect ($type Integer) ($exp times)) ($recurse rest)))
      (`(while ,cond . ,rest) ($loop ($if ($exp cond) ($exp rest) ($nil)))) ;todo
      (`(until ,cond . ,rest) ($loop ($if ($exp cond) ($nil) ($exp rest)))) ;todo
      ;; Existence
      (`(,(or 'always 'never 'thereis) ,expr) ($progn ($exp expr) ($type Boolean)))
      ;; Conditional
      (`(if ,cond ,(et-clloop-clauses
                    thens
                    (or `(else . ,(et-clloop-clauses elses rest))
                        rest)))
       ($if ($exp cond) ($nil) ($nil)))
      (`(when ,cond . ,(et-clloop-clauses thens rest))
       ($nil))
      (`(unless ,cond . ,(et-clloop-clauses elses rest))
       ($nil))
      ;; Final
      (`(finally return ,expr)
       ($exp expr))
      (`(finally ,(or `(do . ,exprs) exprs))
       ($prog1 ($nil) ($exps exprs))))))



(et-declare
 (@check cl-loop
         ($eval (funcall (cdr (or (et-clloop-parse (cdr (et-cur-expr)))
                                  (error "Failed to parse cl-loop")))))))


;;; ============================================================
