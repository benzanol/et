;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Help window state

(et-declare
 (@variable help-window-point-marker Marker)
 (@variable help-window-old-frame Frame?)
 (@variable help-buffer-under-preparation Boolean))

;;; ============================================================
;;; Help key and help map

(et-declare
 (@def help-key () String)
 (@variable help-map Cons<@keymap~Any>)
 (@variable help-button-cache Any))

;;; ============================================================
;;; Quick help

(et-declare
 (@variable help-quick-sections List<Cons<String~List<Cons<Symbol~String>>>>)
 (@variable help-quick-use-map Cons<@keymap~Any>)
 (@def help-quick () String)
 (@def help-quick-toggle () String)
 (@def cheat-sheet () String))

;;; ============================================================
;;; Help quit and return messages

(et-declare
 (@def help-quit () Nil)
 (@variable help-return-method
            (or Nil (Cons Window True|@quit-window) (Tuple Window Buffer Integer Integer)))
 (@def help-print-return-message (&optional function: fn1<String>?) Any))

;;; ============================================================
;;; Help for help (`C-h C-h')

(et-declare
 (@variable help-for-help-buffer-name String)
 (@def help-for-help () Any)
 (@def help () Any))

;;; ============================================================
;;; Function at point

(et-declare
 (@def function-called-at-point () Symbol?))

;;; ============================================================
;;; User help functions

(et-declare
 (@def view-help-file (file: String &optional dir: String?) Any)
 (@def describe-distribution () Any)
 (@def describe-copying () Any)
 (@def describe-gnu-project () Any)
 (@def describe-no-warranty () Any)
 (@def describe-prefix-bindings () Any)
 (@def view-emacs-news (&optional version: Integer|String|Cons<Integer~Nil>?) Any)
 (@def view-emacs-todo (&optional arg: Any) Any)
 (@def view-echo-area-messages () Window?)
 (@def view-order-manuals () Any)
 (@def view-emacs-FAQ () Any)
 (@def view-emacs-problems () Any)
 (@def view-emacs-debugging () Any)
 (@def view-external-packages () Any)
 (@def view-lossage () Any))

;;; ============================================================
;;; Key bindings

(et-declare
 (@variable describe-bindings-outline Boolean)
 (@variable describe-bindings-show-prefix-commands Boolean)
 (@variable describe-bindings-outline-rules
            List<Cons<@match-regexp~String>|Cons<@custom-function~AnyFn>>)
 (@def describe-bindings (&optional prefix: String|Vector<Any>|List<Any>? buffer: Buffer|String?) Any)
 (@def where-is (definition: Any &optional insert: Bool) Nil)
 (@def help-key-description (key: String|Vector<Any> untranslated: String|Vector<Any>?) String)
 (@def describe-key-briefly
       (&optional key-list: (or Nil (List (Cons String|Vector<Any> String|Vector<Any>?)) String Vector<Any>)
        insert: Bool buffer: Buffer?)
       Nil|String)
 (@def describe-key
       (&optional key-list: (or Nil (List (Cons String|Vector<Any> String|Vector<Any>?)) String Vector<Any>)
        buffer: Buffer? up-event: Any)
       Any)
 (@def search-forward-help-for-help () Any))

;;; ============================================================
;;; Minor mode help

(et-declare
 (@def describe-minor-mode (minor-mode: Symbol|String) Any)
 (@def describe-minor-mode-completion-table-for-symbol () List<String>)
 (@def describe-minor-mode-from-symbol (symbol: Symbol) Any)
 (@def describe-minor-mode-completion-table-for-indicator () List<String>)
 (@def describe-minor-mode-from-indicator (indicator: String|Cons &optional event: Any) Any)
 (@def lookup-minor-mode-from-indicator (indicator: String) Symbol?))

;;; ============================================================
;;; Substituting command keys in documentation

(et-declare
 (@variable help-link-key-to-documentation Boolean)
 (@def substitute-command-keys (string: String? &optional no-face: Bool include-menus: Bool) String?)
 (@def substitute-quotes (string: String) String))

;;; ============================================================
;;; Describing keymaps

(et-declare
 (@def describe-map
       (map: &Cons<@keymap~Any>|Symbol
        &optional prefix: String|Vector<Any>? transl: Bool partial: Any shadow: Any
        nomenu: Bool mention-shadow: Bool buffer: Buffer?)
       Any))

;;; ============================================================
;;; Resizing temporary buffer windows

(et-declare
 (@variable temp-buffer-max-height Integer|fn1<Buffer~Integer>)
 (@variable temp-buffer-max-width Integer|fn1<Buffer~Integer>)
 (@variable temp-buffer-resize-mode Boolean)
 (@def temp-buffer-resize-mode (&optional arg: Bool) Any)
 (@variable resize-temp-buffer-window-inhibit Boolean)
 (@def resize-temp-buffer-window (&optional window: Window?) Any))

;;; ============================================================
;;; Help windows

(et-declare
 (@variable help-window-select Nil|@other|True)
 (@variable help-window-keep-selected Boolean)
 (@variable help-enable-auto-load Boolean)
 (@variable help-enable-autoload Boolean)
 (@def help-window-display-message
       (quit-part: String? window: Window &optional scroll: Nil|@other|True)
       String)
 (@def help-window-setup (window: Window &optional value: [T]) T)
 (@check with-help-window ($body Buffer|String))
 (@def help-form-show () Any))

;;; ============================================================
;;; Function documentation strings

(et-declare
 (@def help-split-fundoc
       ([A] docstring: String? def: Any &optional section: A)
       (switch A
               [Nil Nil|Cons<String~String?>]
               [True Cons<String?~String?>]
               [@usage String?]
               [@doc String?]
               Nil))
 (@def help-add-fundoc-usage (docstring: String? arglist: True|String|List<Any>) String)
 (@def help-function-arglist (def: Any &optional preserve-names: Bool) List<Any>|String|True)
 (@def help-make-usage (function: Any arglist: List<Any>) ConsFresh<Symbol~ListFresh<Any>>))

;;; ============================================================
;;; Confusable characters

(et-declare
 (@variable help-uni-confusables Alist<Integer~String>)
 (@variable help-uni-confusables-regexp String)
 (@def help-uni-confusable-suggestions (string: String) String?)
 (@def help-command-error-confusable-suggestions (data: Any context: Any signal: Any) Any))

;;; ============================================================
;;; Obsolete aliases

(et-declare
 (@def help-for-help-internal () Any)
 (@def describe-map-tree
       (startmap: &Cons<@keymap~Any>|Symbol
        &optional partial: Any shadow: &Cons<@keymap~Any>|Symbol|Nil title: String?
        no-menu: Bool transl: Bool always-title: Bool mention-shadow: Bool buffer: Buffer?)
       Any))

;;; ============================================================
