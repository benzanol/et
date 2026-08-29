;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; The defcustom macro

(et-declare
 (@def custom-initialize-default (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-set (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-reset (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-changed (symbol: Symbol exp: Sexp) Any)
 (@def custom-initialize-delay (symbol: Symbol value: Sexp) Any)
 (@def custom-declare-variable (symbol: [T] default: Sexp doc: String? &rest args: &List) T)
 ;; defcustom is a declarative macro: SYMBOL is never evaluated, and
 ;; STANDARD is wrapped in a lambda so it is only evaluated later (under
 ;; lexical scoping) when the variable is first initialized. There is no
 ;; checker shortcut for a form that quotes one operand and defers
 ;; evaluation of another while also handling an open keyword-argument tail.
 (@check defcustom ($todo)))

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
 (@def custom-declare-group (symbol: [T] members: List<Cons<Symbol~Cons<Symbol~Nil>>> doc: String? &rest args: &List) T)
 ;; defgroup is a declarative macro: SYMBOL is never evaluated, and
 ;; MEMBERS/DOC/ARGS describe a group structure rather than code to run.
 ;; There is no checker shortcut for a form that quotes one operand while
 ;; handling an open keyword-argument tail.
 (@check defgroup ($todo))
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
 (@def custom-add-package-version (symbol: Symbol version: &Cons<Symbol~String>) &Cons<Symbol~String>)
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
 (@def custom-theme-reset-variables (theme: Symbol &rest args: &List<Cons<Symbol~Cons<Any~Nil>>>) Nil)
 (@def custom-reset-variables (&rest args: &List<Cons<Symbol~Cons<Any~Nil>>>) Nil)
 (@def custom-add-choice (variable: Symbol choice: Any) List<Any>))

;;; ============================================================
