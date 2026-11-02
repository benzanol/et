;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; The defcustom macro

(defun et:custom--type-positional-args (args)
  "Return the non-keyword arguments from Custom widget ARGS."
  (cl-loop while args
           if (keywordp (car args)) do (setq args (cddr args))
           else collect (pop args)))

(defun et:custom--type-keyword (args key)
  "Return the Custom widget value for KEY in ARGS, defaulting to `sexp'."
  (or (plist-get args key) 'sexp))

(defun et:custom--eval-type (expr)
  "Evaluate a `defcustom' :type expression EXPR."
  (condition-case-unless-debug err
      (eval expr t)
    (error
     (et-fatal nil "Error evaluating custom :type: %s"
               (error-message-string err)))))

(defun et:custom--type-to-et-type (custom-type)
  "Convert common Custom widget type CUSTOM-TYPE to an `et:type'.

Unknown and structurally complicated widget types deliberately fall back to
`Any'."
  (pcase custom-type
    ((or 'sexp 'default 'other 'restricted-sexp 'lazy)
     (et Any))
    ('hook
     (et List<AnyFn>))
    ((or 'boolean 'toggle 'checkbox)
     (et Boolean))
    ((or 'integer 'natnum 'character)
     (et Integer))
    ((or 'number 'float)
     (et Number))
    ((or 'string 'regexp 'file 'directory 'color 'key 'key-sequence
         'coding-system 'charset 'mule-input-method-string 'text)
     (et String))
    ((or 'symbol 'variable 'face 'group)
     (et Symbol))
    ('function
     (et Symbol|AnyFn))

    (`(,(or 'sexp 'default 'other 'restricted-sexp 'lazy) . ,_)
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
    (`(,(or 'symbol 'variable 'face 'group) . ,_)
     (et Symbol))
    (`(function . ,_)
     (et Symbol|AnyFn))
    (`(hook . ,_)
     (et List<AnyFn>))

    (`(,(or 'choice 'radio 'menu-choice) . ,args)
     (apply #'et-union
            (mapcar #'et:custom--type-to-et-type
                    (et:custom--type-positional-args args))))
    (`(,(or 'const 'item 'function-item 'variable-item) . ,args)
     (et-literal
      (if (plist-member args :value)
          (plist-get args :value)
        (car (last (et:custom--type-positional-args args))))))
    (`(list . ,args)
     (et-tuple
      'Cons
      (mapcar #'et:custom--type-to-et-type
              (et:custom--type-positional-args args))))
    (`(vector . ,args)
     (et-alias
      'Vector
      (apply #'et-union
             (mapcar #'et:custom--type-to-et-type
                     (et:custom--type-positional-args args)))))
    (`(cons . ,args)
     (let* ((positional (et:custom--type-positional-args args)))
       (et-alias
        'Cons
        (if positional
            (et:custom--type-to-et-type (car positional))
          (et Any))
        (if (cdr positional)
            (et:custom--type-to-et-type (cadr positional))
          (et Any)))))
    (`(,(or 'repeat 'editable-list) . ,args)
     (et-alias
      'List
      (et:custom--type-to-et-type
       (or (car (et:custom--type-positional-args args)) 'sexp))))
    (`(,(or 'set 'checklist) . ,args)
     (et-alias
      'List
      (apply #'et-union
             (mapcar #'et:custom--type-to-et-type
                     (et:custom--type-positional-args args)))))
    (`(alist . ,args)
     (et-alias
      'Alist
      (et:custom--type-to-et-type
       (et:custom--type-keyword args :key-type))
      (et:custom--type-to-et-type
       (et:custom--type-keyword args :value-type))))
    (`(plist . ,args)
     (et-alias
      'PlistOf
      (et:custom--type-to-et-type
       (et:custom--type-keyword args :key-type))
      (et:custom--type-to-et-type
       (et:custom--type-keyword args :value-type))))

    ;; TODO: Convert more specialized and user-defined widget types.
    (_ (et Any))))

(defun et:custom--identify-defcustom (name _standard _doc &rest args)
  "Identify a `defcustom' form and declare its global variable type."
  (unless (symbolp name)
    (et-fatal 1 "Custom variable name must be a symbol"))
  (let* ((type-pos
          (cl-loop for (key _value) on args by #'cddr
                   for pos from 0 by 2
                   when (eq key :type) return pos))
         (type-expr (and type-pos (nth (1+ type-pos) args))))
    (list
     :declare
     (lambda ()
       (et-set-global-var-type
        name
        (if type-pos
            (et-at (+ 5 type-pos)
              (et:custom--type-to-et-type
               (et:custom--eval-type type-expr)))
          (et Any)))))))

(et-set-identifier #'defcustom #'et:custom--identify-defcustom)

(et-declare
 (@def custom-initialize-default (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-set (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-reset (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-changed (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-delay (symbol: Symbol value: Sexp) Any)
 (@def custom-declare-variable (symbol: [T] default: Sexp doc: String? &rest args: &List) T)
 ;; SYMBOL is syntax rather than an evaluated argument.  STANDARD is checked
 ;; against the type installed by `et:custom--identify-defcustom'; DOC and the
 ;; keyword values are ordinary evaluated expressions.
 (@check defcustom
         ($pcase
          `(,(and name (pred symbolp)) ,_standard ,_doc . ,_args)
          ($prog1
           ($eval (et-literal name))
           ($expect ($eval (or (et-global-var-type name) (et-any))) ($at 2))
           ($expect
            ($type
             (Args*
              String?
              (or
               Nil
               (&Plist
                :type Any
                :options &List
                :initialize Nil|Symbol|AnyFn
                :set Nil|Symbol|AnyFn
                :get Nil|Symbol|AnyFn
                :require Symbol?
                :set-after &List<Symbol>
                :risky Any
                :safe Any
                :local Any
                :group Symbol?
                :link &List
                :version String?
                :package-version Nil|&Cons<Symbol_String>
                :tag String?
                :load Nil|String|Symbol))))
            ($tail-tuple 3)))))))

;;; ============================================================
;;; The defface macro

(et-declare
 ;; defface is a declarative macro: FACE is never evaluated, and SPEC is
 ;; stored as data describing display conditions and attributes rather
 ;; than evaluated. There is no checker shortcut for a form that quotes
 ;; one operand and treats another as a structured spec instead of code.
 (@check defface ($todo)))

;;; ============================================================
;;; The defgroup macro

(et-declare
 (@def custom-current-group () Symbol)
 (@def custom-declare-group (symbol: [T] members: List<Cons<Symbol_Cons<Symbol_Nil>>> doc: String? &rest args: &List) T)
 ;; SYMBOL is the only operand which `defgroup' quotes.  MEMBERS, DOC, and
 ;; keyword values are evaluated normally, while the keyword names themselves
 ;; are part of the macro's syntax.
 (@check defgroup
         ($pcase
          `(,(and symbol (pred symbolp)) ,_members ,_doc . ,_args)
          ($prog1
           ($eval (et-literal symbol))
           ($expect
            ($type
             (Args*
              &List<&Cons<Symbol_&Cons<Symbol_Nil>>>
              String?
              (or
               Nil
               (&Plist
                :prefix String?
                :group Symbol?
                :version String?
                :package-version Nil|&Cons<Symbol_String>
                :link &List
                :load Nil|String|Symbol
                :tag String?
                :set-after &List<Symbol>))))
            ($tail-tuple 2))))))
 (@def custom-add-to-group (group: Symbol option: Symbol widget: Symbol) List<Any>)
 (@def custom-group-of-mode (mode: Symbol) Symbol))

;;; ============================================================
;;; Properties

(et-declare
 (@def custom-handle-all-keywords (symbol: Symbol args: &List type: Symbol) Nil)
 (@def custom-handle-keyword (symbol: Symbol keyword: Symbol value: Any type: Symbol) Any)
 (@def custom-add-dependencies (symbol: Symbol value: List<Symbol>) List<Symbol>)
 (@def [custom-add-option custom-add-frequent-value] (symbol: Symbol option: Any) List<Any>)
 (@def custom-add-link (symbol: Symbol widget: &List) List<Any>)
 (@def custom-add-version (symbol: Symbol version: String) String)
 (@def custom-add-package-version (symbol: Symbol version: &Cons<Symbol_String>) &Cons<Symbol_String>)
 (@def custom-add-load (symbol: Symbol load: String|Symbol) List<String|Symbol>)
 (@def custom-autoload (symbol: Symbol load: String|Symbol &optional noset: Bool) List<String|Symbol>)
 (@def custom-variable-p (variable: Any) Bool)
 (@def custom-note-var-changed (variable: Symbol) Any)
 (@def custom-load-symbol (symbol: Symbol) Nil)
 (@def custom-set-default (variable: Symbol value: [T]) T)
 (@def custom-set-minor-mode (variable: Symbol value: Bool) Any)
 ;; The return type depends on the shape of SEXP: a value that is already
 ;; self-quoting (not a cons, and either a keyword, non-symbol, or
 ;; boolean) is returned unchanged, while any other value is wrapped in a
 ;; quote form. The type language cannot yet express a return type that
 ;; branches on the shape of a value like this.
 (@def custom-quote (sexp: [T]) Todo)
 (@def customize-mark-to-save (symbol: Symbol) Boolean)
 (@def customize-mark-as-set (symbol: Symbol) Boolean)
 (@def custom-reevaluate-setting (symbol: Symbol) Any))

;;; ============================================================
;;; Custom themes

(et-declare
 (@def custom-theme-p (theme: Symbol) Bool)
 (@def custom-check-theme (theme: Symbol) Nil)
 (@def custom-push-theme (prop: @theme-value|@theme-face|@theme-icon symbol: Symbol theme: Symbol mode: @set|@reset &optional value: Any) Any)
 (@def custom-fix-face-spec (spec: &List) List<Any>)
 (@def custom-set-variables (&rest args: &List<List<Any>>) Nil)
 (@def custom-theme-set-variables (theme: Symbol &rest args: &List<List<Any>>) Nil))

;;; ============================================================
;;; Defining themes

(et-declare
 ;; deftheme is a declarative macro: THEME is never evaluated, and
 ;; DOC/PROPERTIES describe the theme rather than code to run. There is
 ;; no checker shortcut for a form that quotes one operand and packages
 ;; the remaining operands as a property list without evaluating them.
 (@check deftheme ($todo))
 (@def custom-declare-theme (theme: Symbol feature: Symbol &optional doc: String? properties: &List) Any)
 (@def custom-make-theme-feature (theme: Symbol) Symbol)
 (@def provide-theme (theme: Symbol) Symbol)
 (@def require-theme (feature: Symbol &optional noerror: Bool) Symbol)
 (@def load-theme (theme: Symbol &optional no-confirm: Bool no-enable: Bool) True)
 (@def theme-list-variants (theme: Symbol &rest list: &List<Symbol>) List<Symbol>)
 (@def [theme-choose-variant toggle-theme] (&optional no-confirm: Bool no-enable: Bool) True)
 (@def custom-theme-load-confirm (hash: String) Boolean)
 (@def custom-theme-name-valid-p (name: Any) Boolean)
 (@def custom-available-themes () List<Symbol>))

;;; ============================================================
;;; Enabling and disabling loaded themes

(et-declare
 (@def enable-theme (theme: Symbol) Nil)
 (@def custom-theme-enabled-p (theme: Symbol) Bool)
 (@def disable-theme (theme: Symbol) Nil)
 (@def custom-variable-theme-value (variable: Symbol) List<Any>)
 (@def custom-theme-recalc-variable (variable: Symbol) Any)
 (@def custom-theme-recalc-face (face: Symbol) Nil))

;;; ============================================================
;;; XEmacs compatibility functions

(et-declare
 (@def custom-theme-reset-variables (theme: Symbol &rest args: &List<Cons<Symbol_Cons<Any_Nil>>>) Nil)
 (@def custom-reset-variables (&rest args: &List<Cons<Symbol_Cons<Any_Nil>>>) Nil)
 (@def custom-add-choice (variable: Symbol choice: Any) List<Any>))

;;; ============================================================
