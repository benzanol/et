;; -*- lexical-binding: t; -*-


;;; Pcase pattern parser

(defun et:std--pcase-current-type (var env)
  (or (alist-get var (et-env->narrows env))
      (et:type-var->type var)))

(defun et:std--pcase-narrow (var type env)
  (et-copy-with
   env :narrows
   (et:check--narrows-and
    (et-env->narrows env)
    (cons (cons var type) (et-type-binds type)))))

(defun et:std--pcase-never (var env)
  (et:std--pcase-narrow var (et-never) env))

(defun et:std--pcase-unreachable-p (env)
  (cl-loop for (_ . type) in (et-env->narrows env)
           thereis (et-never-p type)))

(defun et:std--pcase-remove-vars (type vars)
  (et:algebra--transform-type
   type nil
   (lambda (_ case value)
     (et-copy-with
      case
      :value value
      :binds
      (cl-loop for (var . bound) in (et:type-case->binds case)
               unless (memq var vars)
               collect (cons var (et:std--pcase-remove-vars bound vars)))
      :typeofs
      (cl-remove-if (lambda (var) (memq var vars))
                    (et:type-case->typeofs case))))))

(defun et:std--pcase-remove-env-vars (env vars)
  (et-copy-with
   env
   :narrows
   (cl-loop for (var . type) in (et-env->narrows env)
            unless (memq var vars)
            collect (cons var (et:std--pcase-remove-vars type vars)))
   :binds
   (cl-loop for (name . type) in (et-env->binds env)
            collect (cons name (et:std--pcase-remove-vars type vars)))))

(defun et:std--pcase-local-vars (env)
  (cl-loop for (name . type) in (et-env->binds env)
           collect
           (et:type-var-new
            :name name :type (et:algebra-remove-binds type))))

(defun et:std--pcase-local-narrows (env locals)
  (cl-mapcar (lambda (local bind) (cons local (cdr bind)))
             locals (et-env->binds env)))

(defun et:std--pcase-merge-envs (envs)
  (et-env-new
   :narrows
   (cl-reduce #'et:flow--narrows-or
              (mapcar #'et-env->narrows envs))
   :binds
   (cl-loop
    for name in (delete-dups
                 (cl-loop for env in envs
                          append (mapcar #'car (et-env->binds env))))
    collect
    (cons name
          (apply #'et-union
                 (mapcar
                  (lambda (env)
                    (or (alist-get name (et-env->binds env))
                        (et-any)))
                  envs))))
   :fbinds (et-env->fbinds (car envs))))

(defun et:std--pcase-output-envs (var output env)
  (cl-loop
   for expected in (list (et NonNil) (et Nil))
   for branch = (et-supersect output expected)
   for binds = (et-type-binds branch)
   for type = (if (et-never-p branch)
                  (et-never)
                (or (alist-get var binds)
                    (et:std--pcase-current-type var env)))
   collect
   (et-copy-with
    env
    :narrows
    (et:check--narrows-and
     (et-env->narrows env)
     (cons
      (cons var type)
      (cl-remove-if-not
       (lambda (bind)
         (or (eq (car bind) var)
             (et:check-var-in-scope? (car bind))))
       binds))))))

(defun et:std--pcase-check-expression (expression env continue &optional recommendation)
  "Check EXPRESSION in ENV, then call CONTINUE with its type and new ENV."
  (let* ((local-vars (et:std--pcase-local-vars env))
         (locals (cl-mapcar #'cons (mapcar #'car (et-env->binds env)) local-vars))
         (local-narrows (et:std--pcase-local-narrows env local-vars))
         branches)
    (et-with-vars local-vars
      (et-with-fbinds (et-env->fbinds env)
        (et-with-narrows
            (et:check--narrows-and (et-env->narrows env) local-narrows)
          (let ((output (et-check-expansion recommendation expression)))
            (setq branches
                  (funcall continue output
                           (et-copy-with env :narrows (et-cur-narrows))))))))
    (cl-loop
     for branch in branches
     for updated =
     (et-copy-with
      branch
      :binds
      (append
       (cl-remove-if
        (lambda (bind) (assq (car bind) locals))
        (et-env->binds branch))
       (cl-loop for (name . local) in locals
                collect
                (cons name
                      (or (alist-get local (et-env->narrows branch))
                          (et:type-var->type local))))))
     collect
     (et:std--pcase-remove-env-vars updated local-vars))))

(defun et:std--pcase-call (var function env continue)
  (if (eq (car-safe function) 'lambda)
      (let* ((input
              (et-add-typeof (et:std--pcase-current-type var env) var))
             (input-list (et-tuple 'Cons (list input)))
             (plain-input-list
              (et-tuple 'Cons (list (et:algebra-remove-binds input)))))
        (et:std--pcase-check-expression
         function env
         (lambda (function-type checked-env)
           (let ((result (et:algebra-funcall function-type input-list)))
             (if (et:match-result->success result)
                 (funcall continue (et:match-result->value result) checked-env)
               (et-err nil "Pcase function does not accept %s" input)
               (list checked-env checked-env))))
         (et-dt 'Function plain-input-list (et Any))))
    (let* ((argument (make-symbol "pcase-value"))
           (call-env
            (et-copy-with
             env :binds
             (cons (cons argument
                         (et-add-typeof
                          (et:std--pcase-current-type var env) var))
                   (et-env->binds env))))
           (branches
            (et:std--pcase-check-expression
             (pcase--funcall function argument nil) call-env continue)))
      (cl-loop for branch in branches
               collect
               (et-copy-with
                branch :binds
                (cl-remove argument (et-env->binds branch) :key #'car))))))


;;;; Pattern handlers

(defun et:std--pcase-atom (var pattern env)
  (cond
   ((memq pattern '(_ t))
    (list env (et:std--pcase-never var env)))
   ((and (symbolp pattern) pattern (not (keywordp pattern)))
    (if-let* ((bound (alist-get pattern (et-env->binds env))))
        (let* ((current (et:std--pcase-current-type var env))
               (matched (et-supersect current bound))
               (same-value (alist-get var (et-type-binds bound)))
               (unmatched
                (cond
                 (same-value (et:std--pcase-never var env))
                 ((pcase (et-type-single
                          (et:algebra-remove-binds bound))
                    ((cl-struct et:type-dt
                                (name 'Literal)
                                (args `(,(and value
                                              (guard
                                               (or (symbolp value)
                                                   (numberp value)))))))
                     t))
                  (et:std--pcase-narrow
                   var (et-subtract current bound) env))
                 (t env))))
          (list
           (et-copy-with
            (et:std--pcase-narrow var matched env)
            :binds
            (cons (cons pattern matched)
                  (cl-remove pattern (et-env->binds env) :key #'car)))
           unmatched))
      (list
       (et-copy-with
        env :binds
        (cons
         (cons pattern
               (et-add-typeof (et:std--pcase-current-type var env) var))
         (et-env->binds env)))
       (et:std--pcase-never var env))))
   (t
    (et:std--pcase-literal var pattern env))))

(defun et:std--pcase-literal (var value env)
  (let* ((current (et:std--pcase-current-type var env))
         (literal (et-literal value)))
    (list
     (et:std--pcase-narrow var (et-supersect current literal) env)
     (et:std--pcase-narrow var (et-subtract current literal) env))))

(defun et:std--pcase-and (var patterns env)
  (let ((matched env)
        unmatched)
    (dolist (pattern patterns)
      (pcase-let ((`(,yes ,no) (et:std--pcase-envs var pattern matched)))
        (setq matched yes)
        (push no unmatched)))
    (list
     matched
     (if unmatched
         (et-copy-with
          (et:std--pcase-merge-envs unmatched)
          :binds (et-env->binds env))
       (et:std--pcase-never var env)))))

(defun et:std--pcase-or (var patterns env)
  (let (matched
        (unmatched env))
    (dolist (pattern patterns)
      (pcase-let ((`(,yes ,no) (et:std--pcase-envs var pattern unmatched)))
        (push yes matched)
        (setq unmatched no)))
    (list
     (if matched
         (et:std--pcase-merge-envs matched)
       (et:std--pcase-never var env))
     (et-copy-with unmatched :binds (et-env->binds env)))))

(defun et:std--pcase-not (var pattern env)
  (pcase-let ((`(,yes ,no) (et:std--pcase-envs var pattern env)))
    (list
     (et-copy-with no :binds (et-env->binds env))
     (et-copy-with yes :binds (et-env->binds env)))))

(defun et:std--pcase-pred (var function env)
  (et:std--pcase-call var function env
                      (lambda (output call-env)
                        (et:std--pcase-output-envs var output call-env))))

(defun et:std--pcase-app (var function pattern env)
  (et:std--pcase-call
   var function env
   (lambda (output call-env)
     (let* ((child
             (et:type-var-new :name (make-symbol "pcase-app") :type output))
            (child-env
             (et:std--pcase-narrow child output call-env))
            (branches (et:std--pcase-envs child pattern child-env)))
       (cl-loop
        for branch in branches
        for matched = t then nil
        for child-type = (et:std--pcase-current-type child branch)
        collect
        (et:std--pcase-remove-env-vars
         (et-copy-with
          branch
          :narrows
          (if (et-never-p child-type)
              (et:check--narrows-and
               (et-env->narrows branch) (list (cons var (et-never))))
            (et-env->narrows branch))
          :binds
          (if matched
              (et-env->binds branch)
            (et-env->binds call-env)))
         (list child)))))))

(defun et:std--pcase-guard (var expression env)
  (et:std--pcase-check-expression
   expression env
   (lambda (output checked-env)
     (et:std--pcase-output-envs var output checked-env))))


;;;; Pattern dispatcher

(defun et:std--pcase-envs (var pattern &optional env)
  (unless env
    (setq env
          (et-env-new
           :narrows
           (et:check--narrows-and
            (et-cur-narrows)
            (list (cons var (et:check-var-type var)))))))
  (if (et:std--pcase-unreachable-p env)
      (list env env)
    (pcase pattern
      ((pred atom) (et:std--pcase-atom var pattern env))
      (`(quote ,value) (et:std--pcase-literal var value env))
      (`(and . ,patterns) (et:std--pcase-and var patterns env))
      (`(or . ,patterns) (et:std--pcase-or var patterns env))
      (`(not ,inner) (et:std--pcase-not var inner env))
      (`(pred ,function) (et:std--pcase-pred var function env))
      (`(app ,function ,inner)
       (et:std--pcase-app var function inner env))
      (`(guard ,expression) (et:std--pcase-guard var expression env))
      (_
       (let ((expanded (pcase--macroexpand pattern)))
         (if (equal expanded pattern)
             (progn
               (et-err nil "Invalid pcase pattern: %s" pattern)
               (list env env))
           (et:std--pcase-envs var expanded env)))))))


;;; Pcase checker

(et-define-check-macro $pcase-envs (envs &rest branches)
  (let ((envs-var (gensym "envs"))
        (types-var (gensym "types"))
        (narrows-var (gensym "narrows")))
    `(let ((,envs-var ,envs)
           (,types-var nil)
           (,narrows-var nil))
       ,@(cl-loop
          for branch in branches
          for index upfrom 0
          for env-var = (gensym "env")
          for locals-var = (gensym "locals")
          for local-narrows-var = (gensym "local-narrows")
          for result-var = (gensym "result")
          collect
          `(let* ((,env-var (nth ,index ,envs-var))
                  (,locals-var
                   (et:std--pcase-local-vars ,env-var))
                  (,local-narrows-var
                   (et:std--pcase-local-narrows ,env-var ,locals-var))
                  (,result-var
                   (et-with-vars ,locals-var
                     (et-with-fbinds (et-env->fbinds ,env-var)
                       (if (et:std--pcase-unreachable-p ,env-var)
                           (cons (et-never) nil)
                         (et-with-narrows
                             (et:check--narrows-and
                              (et-env->narrows ,env-var)
                              ,local-narrows-var)
                           (let ((type (et-chk ,branch)))
                             (cons type (et-cur-narrows)))))))))
             (unless (et-never-p (car ,result-var))
               (push
                (et:std--pcase-remove-vars
                 (car ,result-var) ,locals-var)
                ,types-var)
               (push
                (seq-filter
                 (lambda (narrow)
                   (et:check-var-in-scope? (car narrow)))
                 (et-env->narrows
                  (et:std--pcase-remove-env-vars
                   (et-env-new :narrows (cdr ,result-var))
                   ,locals-var)))
                ,narrows-var))))
       (et-set-narrows
        (when ,narrows-var
          (cl-reduce #'et:flow--narrows-or ,narrows-var)))
       (apply #'et-union ,types-var))))

(et-declare
 (@check pcase
         ($pcase
          `(,_value) ($type Nil)
          `(,value (,pattern . ,body) . ,rest)
          ($bind [var ($at 1)]
                 ($pcase-envs
                  (et:std--pcase-envs var pattern)
                  ($tail 2 1)
                  ($exp `(pcase (:var ,var) ,@rest)))))))
