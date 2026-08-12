;;; custom.el --- Custom variable definitions for et -*- lexical-binding: t; -*-

(defun et--custom-type-positional-args (args)
  "Return non-keyword widget ARGS."
  (cl-loop while args
           if (keywordp (car args)) do (setq args (cddr args))
           else collect (pop args)))

(defun et--custom-type-first-positional (args)
  (car (et--custom-type-positional-args args)))

(defun et--custom-type-keyword (args key)
  (or (plist-get args key) 'sexp))

(defun et--custom-eval-type-expression (expr)
  "Evaluate a defcustom :type expression EXPR."
  (condition-case-unless-debug err
      (eval expr t)
    (error (et-fatal nil "Error evaluating custom :type: %s"
                     (error-message-string err)))))

(defun et--custom-type-to-et-type (custom-type)
  "Convert a Custom CUSTOM-TYPE widget spec to an et type."
  (pcase custom-type
    ((or 'sexp 'default 'other 'restricted-sexp 'lazy)
     (et Any))

    ('hook
     (et-alias 'List (et-alias 'AnyFn)))

    ((or 'boolean 'toggle 'checkbox)
     (et Boolean))

    ((or 'integer 'natnum 'character)
     (et Integer))

    ((or 'number 'float)
     (et Number))

    ((or 'string 'regexp 'file 'directory 'color 'key 'key-sequence
         'coding-system 'charset 'mule-input-method-string 'text)
     (et String))

    ((or 'symbol 'function 'variable 'face 'group)
     (et Symbol))

    ((or `(,(or 'sexp 'default 'other 'restricted-sexp 'lazy) . ,_))
     (et Any))

    (`(,(or 'boolean 'toggle 'checkbox) . ,_)
     (et Boolean))

    (`(,(or 'integer 'natnum 'character) . ,_)
     (et Integer))

    (`(,(or 'number 'float) . ,_)
     (et Number))

    (`(,(or 'string 'regexp 'file 'directory 'color 'key 'key-sequence
           'coding-system 'charset 'mule-input-method-string 'text)
       . ,_)
     (et String))

    (`(,(or 'symbol 'function 'variable 'face 'group) . ,_)
     (et Symbol))

    (`(,(or 'choice 'radio 'menu-choice) . ,args)
     (apply #'et--or
            (mapcar #'et--custom-type-to-et-type
                    (et--custom-type-positional-args args))))

    (`(,(or 'const 'item 'function-item 'variable-item) . ,args)
     (et-literal
      (if (plist-member args :value)
          (plist-get args :value)
        (car (last (et--custom-type-positional-args args))))))

    (`(list . ,args)
     (apply #'et-alias 'Tuple
            (mapcar #'et--custom-type-to-et-type
                    (et--custom-type-positional-args args))))

    (`(vector . ,args)
     (et-alias 'Vector
               (apply #'et--or
                      (mapcar #'et--custom-type-to-et-type
                              (et--custom-type-positional-args args)))))

    (`(cons . ,args)
     (let* ((positional (et--custom-type-positional-args args))
            (car-type (if positional
                          (et--custom-type-to-et-type (car positional))
                        (et Any)))
            (cdr-type (if (cdr positional)
                          (et--custom-type-to-et-type (cadr positional))
                        (et Any))))
       (et-alias 'Cons car-type cdr-type)))

    (`(,(or 'repeat 'editable-list) . ,args)
     (et-alias 'List
               (et--custom-type-to-et-type
                (or (et--custom-type-first-positional args) 'sexp))))

    (`(,(or 'set 'checklist) . ,args)
     (et-alias 'List
               (apply #'et--or
                      (mapcar #'et--custom-type-to-et-type
                              (et--custom-type-positional-args args)))))

    (`(alist . ,args)
     (et-alias 'AList
               (et--custom-type-to-et-type (et--custom-type-keyword args :key-type))
               (et--custom-type-to-et-type (et--custom-type-keyword args :value-type))))

    (`(plist . ,args)
     (et-alias 'KVPList
               (et--custom-type-to-et-type (et--custom-type-keyword args :key-type))
               (et--custom-type-to-et-type (et--custom-type-keyword args :value-type))))

    (`(hook . ,_)
     (et-alias 'List (et-alias 'AnyFn)))

    (_
     (et Any))))

(et-define-identifier defcustom (name _standard _doc &rest args)
  (unless (symbolp name)
    (et-fatal 1 "Custom variable name must be a symbol"))
  (when-let* ((type-pos (cl-loop for (key _value) on args by #'cddr
                                 for pos from 0 by 2
                                 when (eq key :type) return pos)))
    (let ((type-spec (nth (1+ type-pos) args)))
      (list
       :declare
       `(lambda ()
          (et-at ,(+ 5 type-pos)
            (et--declare-variable-type
             ',name
             (et--custom-type-to-et-type
              (et--custom-eval-type-expression ',type-spec)))))))))

(et-define-pcase-checker defcustom `(,(and (pred symbolp) name) . ,_)
  (if-let* ((declared-type (get name 'et-variable-type))
            (value-type (et-checker-sub 2)))
      (unless (et-subtype? value-type declared-type)
        (et-err 2 "Expected %s, found %s" declared-type value-type))

    (et--declare-variable-type name (et Any)))

  (et-literal name))

(et-test
 (let* ((result (et-result-boundary
                  (et--process-exprs
                   '((defcustom et--custom-test-string "x" "Doc." :type 'string)
                     (defcustom et--custom-test-choice 1 "Doc."
                       :type '(choice (const 1) string))
                     (defcustom et--custom-test-list (list 1 "x") "Doc."
                       :type '(list integer string))
                     (defcustom et--custom-test-number 1 "Doc." :type 'number)
                     (defcustom et--custom-test-hook nil "Doc."
                       :type 'hook)))))
        (type (lambda (sym) (get sym 'et-variable-type))))
   (and (not (et-result-failed result))
        (et-subtype? (funcall type 'et--custom-test-string) (et String))
        (et-subtype? (funcall type 'et--custom-test-choice) (et 1|String))
        (et-subtype? (funcall type 'et--custom-test-list) (et Tuple<Integer~String>))
        (et-subtype? (funcall type 'et--custom-test-number) (et Number))
        (et-subtype? (funcall type 'et--custom-test-hook) (et List<AnyFn>)))))
