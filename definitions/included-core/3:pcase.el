;; -*- lexical-binding: t; -*-


;;; Pcase pattern parser

(defun et:std--pcase-merge-envs (envs)
  (let* ((narrows
          (cl-reduce #'et:flow--narrows-or
                     (mapcar #'et-env->narrows envs)))
         (names
          (delete-dups
           (cl-loop for env in envs
                    append (mapcar #'car (et-env->binds env)))))
         (binds
          (cl-loop for name in names
                   collect
                   (cons name
                         (apply
                          #'et-union
                          (mapcar
                           (lambda (env)
                             (or (alist-get name (et-env->binds env))
                                 (et-any)))
                           envs))))))
    (et-env-new
     :narrows narrows
     :binds binds
     :fbinds (et-env->fbinds (car envs)))))

(defun et:std--pcase-output-envs (var output env)
  (cl-loop
   for expected in (list (et NonNil) (et Nil))
   for branch = (et-supersect output expected)
   for binds = (et-type-binds branch)
   for type = (if (et-never-p branch) (et-never)
                (or (alist-get var binds)
                    (alist-get var (et-env->narrows env))
                    (et:type-var->type var)))
   collect
   (et-env-new
    :narrows
    (et:check--narrows-and
     (et-env->narrows env)
     (cons
      (cons var type)
      (cl-remove-if-not
       (lambda (bind)
         (or (eq (car bind) var)
             (et:check-var-in-scope? (car bind))))
       binds)))
    :binds (et-env->binds env)
    :fbinds (et-env->fbinds env))))


;;;; Pattern handlers

(defun et:std--pcase-atom (var pattern env &optional literal)
  (let ((current (or (alist-get var (et-env->narrows env))
                     (et:type-var->type var))))
    (cond
     ((and (not literal) (eq pattern '_))
      (list
       env
       (et-copy-with
        env :narrows
        (et:check--narrows-and
         (et-env->narrows env) (list (cons var (et-never)))))))
     ((and (not literal)
           (symbolp pattern)
           pattern
           (not (eq pattern t))
           (not (keywordp pattern)))
      (list
       (et-copy-with
        env :binds
        (cons (cons pattern current)
              (cl-remove pattern (et-env->binds env) :key #'car)))
       (et-copy-with
        env :narrows
        (et:check--narrows-and
         (et-env->narrows env) (list (cons var (et-never)))))))
     (t
      (let* ((literal (et-literal pattern))
             (matched (et-supersect current literal))
             (unmatched (et-subtract current literal)))
        (list
         (et-copy-with
          env :narrows
          (et:check--narrows-and
           (et-env->narrows env)
           (cons (cons var matched) (et-type-binds matched))))
         (et-copy-with
          env :narrows
          (et:check--narrows-and
           (et-env->narrows env)
           (cons (cons var unmatched) (et-type-binds unmatched))))))))))

(defun et:std--pcase-quote (var pattern env)
  (pcase pattern
    (`(quote ,value)
     (et:std--pcase-atom var value env t))
    (_ (list env env))))

(defun et:std--pcase-and (var pattern env)
  (let ((matched env)
        (unmatched nil)
        (entry-binds (et-env->binds env)))
    (dolist (inner (cdr pattern))
      (pcase-let ((`(,yes ,no)
                   (et:std--pcase-envs var inner matched)))
        (setq matched yes)
        (push no unmatched)))
    (list
     matched
     (if unmatched
         (et-copy-with
          (et:std--pcase-merge-envs unmatched)
          :binds entry-binds)
       (et-copy-with
        env :narrows
        (et:check--narrows-and
         (et-env->narrows env) (list (cons var (et-never)))))))))

(defun et:std--pcase-or (var pattern env)
  (let ((matched nil)
        (unmatched env))
    (dolist (inner (cdr pattern))
      (pcase-let ((`(,yes ,no)
                   (et:std--pcase-envs var inner unmatched)))
        (push yes matched)
        (setq unmatched no)))
    (list
     (if matched
         (et:std--pcase-merge-envs matched)
       (et-copy-with
        env :narrows
        (et:check--narrows-and
         (et-env->narrows env) (list (cons var (et-never))))))
     (et-copy-with unmatched :binds (et-env->binds env)))))

(defun et:std--pcase-not (var pattern env)
  (pcase pattern
    (`(not ,inner)
     (pcase-let ((`(,yes ,no)
                  (et:std--pcase-envs var inner env)))
       (list
        (et-copy-with no :binds (et-env->binds env))
        (et-copy-with yes :binds (et-env->binds env)))))
    (_ (list env env))))

(defun et:std--pcase-pred (var pattern env)
  (pcase pattern
    (`(pred ,function)
     (let* ((name
             (pcase function
               ((or `(function ,symbol) `(quote ,symbol)) symbol)
               (symbol symbol)))
            (function-type
             (and (symbolp name) (et-get-fbind name)))
            (input
             (et-add-typeof
              (or (alist-get var (et-env->narrows env))
                  (et:type-var->type var))
              var)))
       (if (null function-type)
           (progn
             (et-err nil "No function type for pcase predicate `%s'" name)
             (list env env))
         (let* ((result
                 (et-funcall function-type
                             (et-tuple 'Cons (list input)))))
           (if (et:match-result->success result)
               (et:std--pcase-output-envs
                var (et:match-result->value result) env)
             (et-err nil "Pcase predicate `%s' does not accept %s"
                     name input)
             (list env env))))))
    (_ (list env env))))

(defun et:std--pcase-app (var pattern env)
  (pcase pattern
    (`(app ,function ,inner)
     (let* ((name
             (pcase function
               ((or `(function ,symbol) `(quote ,symbol)) symbol)
               (symbol symbol)))
            (function-type
             (and (symbolp name) (et-get-fbind name)))
            (input
             (et-add-typeof
              (or (alist-get var (et-env->narrows env))
                  (et:type-var->type var))
              var)))
       (if (null function-type)
           (progn
             (et-err nil "No function type for pcase application `%s'" name)
             (list env env))
         (let ((result
                (et-funcall function-type
                            (et-tuple 'Cons (list input)))))
           (if (not (et:match-result->success result))
               (progn
                 (et-err nil "Pcase application `%s' does not accept %s"
                         name input)
                 (list env env))
             (let* ((output (et:match-result->value result))
                    (child
                     (et:type-var-new
                      :name (make-symbol "pcase-app") :type output))
                    (child-env
                     (et-copy-with
                      env :narrows
                      (et:check--narrows-and
                       (et-env->narrows env)
                       (cons (cons child output)
                             (et-type-binds output)))))
                    (branches
                     (et:std--pcase-envs child inner child-env)))
               (cl-loop
                for branch in branches
                for index upfrom 0
                for child-type = (alist-get child (et-env->narrows branch))
                for narrows = (cl-remove child (et-env->narrows branch)
                                         :key #'car)
                collect
                (et-copy-with
                 branch
                 :narrows
                 (if (et-never-p child-type)
                     (et:check--narrows-and
                      narrows (list (cons var (et-never))))
                   narrows)
                 :binds
                 (if (= index 1)
                     (et-env->binds env)
                   (et-env->binds branch))))))))))
    (_ (list env env))))

(defun et:std--pcase-guard (var pattern env)
  (pcase pattern
    (`(guard ,expression)
     (let ((output
            (et-with-binds (et-env->binds env)
              (et-with-fbinds (et-env->fbinds env)
                (et-with-narrows (et-env->narrows env)
                  (et-check-expansion nil expression))))))
       (et:std--pcase-output-envs var output env)))
    (_ (list env env))))

(defun et:std--pcase-expand (var pattern env)
  (let ((expanded (pcase--macroexpand pattern)))
    (if (equal expanded pattern)
        (list env env)
      (et:std--pcase-envs var expanded env))))


;;;; Pattern registry and checker

(put 'quote 'et:std--pcase-pattern #'et:std--pcase-quote)
(put 'and 'et:std--pcase-pattern #'et:std--pcase-and)
(put 'or 'et:std--pcase-pattern #'et:std--pcase-or)
(put 'not 'et:std--pcase-pattern #'et:std--pcase-not)
(put 'pred 'et:std--pcase-pattern #'et:std--pcase-pred)
(put 'app 'et:std--pcase-pattern #'et:std--pcase-app)
(put 'guard 'et:std--pcase-pattern #'et:std--pcase-guard)
(put 'let 'et:std--pcase-pattern #'et:std--pcase-expand)
(put (intern "`") 'et:std--pcase-pattern #'et:std--pcase-expand)

(defun et:std--pcase-envs (var pattern &optional env)
  (unless env
    (setq env
          (et-env-new
           :narrows
           (et:check--narrows-and
            (et-cur-narrows)
            (list (cons var (et:check-var-type var)))))))
  (if (cl-loop for (_ . type) in (et-env->narrows env)
               thereis (et-never-p type))
      (list env env)
    (if (atom pattern)
        (et:std--pcase-atom var pattern env)
      (if-let* ((handler (get (car pattern) 'et:std--pcase-pattern)))
          (funcall handler var pattern env)
        (et:std--pcase-expand var pattern env)))))


;;; Pcase

(et-declare
 (@check pcase
         ($pcase
          (`(,_val) ($type Nil))
          (`(,val (,pat . ,body) . ,rest)
           ($temp-var var ($at 1)
                      ($envs (et:std--pcase-envs var pat)
                             ($tail 2 1)
                             ($exp `(pcase (:var ,var) ,@rest))))))))
