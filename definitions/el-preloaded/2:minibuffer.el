;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Completion table manipulation

(et-declare
 (@def completion-boundaries
       (string: String collection: CompletionTable pred: CompletionPredicate? suffix: String)
       Cons<Integer~Integer>)
 (@def completion-metadata (string: String table: CompletionTable pred: CompletionPredicate?)
       Cons<@metadata~Alist<Symbol~Any>>)
 (@def completion-metadata-get (metadata: &Cons<@metadata~Alist<Symbol~Any>> prop: NonNilSymbol) Any)
 (@def completion-category-get (cat: Symbol prop: Symbol) &Cons<Symbol~Any>|Nil)
 (@def define-completion-category
       ([(<= T Symbol)] name: T &optional parents: Symbol|List<Symbol>? doc: String? &rest defaults: &List)
       T)
 (@def complete-with-action
       ([(<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>)]
        action: A collection: CompletionTable string: String predicate: CompletionPredicate?)
       (switch A
               [Nil Boolean|String]
               [True &List<String>]
               [@lambda Bool]
               [@metadata Cons<@metadata~Alist<Symbol~Any>>|Nil]
               [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>|Nil]
               Bool))
 (@def completion-table-dynamic (fun: fn1<String~CompletionTable> &optional switch-buffer: Bool)
       CompletionFunction)
 (@def completion-table-with-cache (fun: fn1<String~CompletionTable> &optional ignore-case: Bool)
       CompletionFunction)
 ;; The macro initializes VAR lazily by mutating it in place when first used
 ;; as a completion table. There is no checker shortcut for a macro that
 ;; expands into code performing a conditional variable assignment tied to
 ;; the caller's own variable.
 (@check lazy-completion-table ($todo))
 (@def completion-table-case-fold (table: CompletionTable &optional dont-fold: Bool) CompletionFunction)
 (@def completion-table-with-metadata (table: CompletionTable metadata: &Alist<Symbol~Any>)
       CompletionFunction)
 (@def completion-table-subvert (table: CompletionTable s1: String s2: String) CompletionFunction)
 (@def completion-table-with-context
       ([(<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>)]
        prefix: String table: CompletionTable string: String pred: CompletionPredicate? action: A)
       (switch A
               [Nil Boolean|String]
               [True &List<String>]
               [@lambda Bool]
               [@metadata Cons<@metadata~Alist<Symbol~Any>>|Nil]
               [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>|Nil]
               Any))
 (@def completion-table-with-terminator
       ([(<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>)]
        terminator: String|Cons<String|fn1<String~String>~String>
        table: CompletionTable string: String pred: CompletionPredicate? action: A)
       (switch A
               [Nil Boolean|String]
               [True &List<String>]
               [@lambda Nil]
               [@metadata Cons<@metadata~Alist<Symbol~Any>>|Nil]
               [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>|Nil]
               Any))
 (@def completion-table-with-predicate
       ([(<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>)]
        table: CompletionTable pred1: CompletionPredicate? strict: Bool
        string: String pred2: CompletionPredicate? action: A)
       (switch A
               [Nil Boolean|String]
               [True &List<String>]
               [@lambda Bool]
               [@metadata Cons<@metadata~Alist<Symbol~Any>>|Nil]
               [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>|Nil]
               Any))
 (@def completion-table-in-turn (&rest tables: &List<CompletionTable>) CompletionFunction)
 (@def completion-table-merge (&rest tables: &List<CompletionTable>) CompletionFunction)
 (@def completion-table-with-quoting
       (table: CompletionTable unquote: fn1<String~String>
        requote: fn2<Integer~String~Cons<Integer~fn1<String~String>>>)
       CompletionFunction)
)

;;; ============================================================
;;; Minibuffer completion

(et-declare
 (@def minibuffer-message (message: String &rest args: &List) String|Nil)
 (@def set-minibuffer-message (message: String) Boolean)
 (@def set-message-functions (message: String) Any)
 (@def inhibit-message (message: String) Integer|String)
 (@def set-multi-message (message: String?) String)
 (@def clear-minibuffer-message () @dont-clear-message|Nil)
 (@def minibuffer-completion-contents () String)
 (@def delete-minibuffer-contents () Nil)
 (@def completion-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer
        &optional metadata: &Alist<Symbol~Any>?)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer
        &optional metadata: &Alist<Symbol~Any>?)
       Todo)
 (@def minibuffer-complete () Boolean)
 (@def minibuffer-sort-alphabetically (completions: List<String>) List<String>)
 (@def minibuffer-sort-by-history (completions: List<String>) List<String>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-all-sorted-completions (&optional start: IntOrMarker? end: IntOrMarker?) Todo)
 (@def minibuffer-force-complete-and-exit () Any)
 (@def minibuffer-force-complete (&optional start: IntOrMarker? end: IntOrMarker? dont-cycle: Bool) Any)
 (@def minibuffer-complete-and-exit (&optional no-exit: Bool) Any)
 (@def completion-complete-and-exit (beg: IntOrMarker end: IntOrMarker exit-function: AnyFn) Any)
 (@def minibuffer-complete-word () Boolean)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-hilit-commonality
       (completions: List<String> prefix-len: Integer &optional base-size: Integer?)
       Todo)
 (@def display-completion-list
       (completions: &List<String|Symbol|Tuple<String~String>|Tuple<String~String~String>>
        &optional common-substring: String? group-fun: fn2<String~Boolean~String?>)
       Nil)
 (@def minibuffer-completion-help (&optional start: IntOrMarker? end: IntOrMarker?) Nil)
 (@def minibuffer-hide-completions () Any)
 (@def exit-minibuffer () Never)
 (@def minibuffer-restore-windows () Any)
 (@def minibuffer-quit-recursive-edit (&optional levels: Integer?) Never)
 (@def self-insert-and-exit () Never)
 (@def completion-in-region
       (start: IntOrMarker end: IntOrMarker collection: CompletionTable &optional predicate: CompletionPredicate?)
       Boolean)
 ;; `completion-in-region-mode' is defined via `define-minor-mode', which
 ;; expands into a variable definition, keymap wiring, and a toggle function.
 ;; There is no checker shortcut for a minor-mode definition form.
 (@check completion-in-region-mode ($todo))
 (@def completion-at-point () Any)
 (@def completion-help-at-point (&optional only-if-eager: Bool) String|Nil)
)

;;; ============================================================
;;; Key bindings.

(et-declare
 (@def minibuffer-completion-exit (&optional no-exit: Bool) Nil)
 (@def read-no-blanks-input (prompt: String &optional initial: String|Cons<String~Integer>? inherit-input-method: Bool)
       String)
)

;;; ============================================================
;;; Major modes for the minibuffer

(et-declare
 (@def minibuffer-inactive-mode () Nil)
 (@def minibuffer-mode () Nil)
)

;;; ============================================================
;;; Completion tables.

(et-declare
 (@def minibuffer-maybe-quote-filename (filename: String) String)
 (@def completion-file-name-table
       ([(<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>)]
        string: String pred: CompletionPredicate? action: A)
       (switch A
               [Nil Boolean|String]
               [True &List<String>]
               [@lambda Bool]
               [@metadata Cons<@metadata~Alist<Symbol~Any>>]
               [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>]
               Any))
 (@def read-file-name
       (prompt: String &optional dir: String? default-filename: String|List<String>?
        mustmatch: Any initial: String? predicate: CompletionPredicate?)
       String)
 (@def read-file-name-default
       (prompt: String &optional dir: String? default-filename: String|List<String>?
        mustmatch: Any initial: String? predicate: CompletionPredicate?)
       String)
 (@def internal-complete-buffer-except (&optional buffer: String|Buffer?) CompletionFunction)
)

;;; ============================================================
;;; Old-style completion, used in Emacs-21 and Emacs-22.

(et-declare
 (@def completion-emacs21-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? _point: Integer)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-emacs21-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? _point: Integer)
       Todo)
 (@def completion-emacs22-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-emacs22-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Todo)
)

;;; ============================================================
;;; Basic completion.

(et-declare
 (@def completion-basic-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-basic-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Todo)
)

;;; ============================================================
;;; Partial-completion-mode style completion.

(et-declare
 (@def completion-lazy-hilit (str: String) String)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-pcm-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Todo)
 (@def completion-pcm-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|True|Cons<String~Integer>)
)

;;; ============================================================
;;; Substring completion

(et-declare
 (@def completion-substring-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-substring-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Todo)
)

;;; ============================================================
;;; "flex" completion, also known as flx/fuzzy/scatter completion

(et-declare
 (@def completion-flex-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|True|Cons<String~Integer>)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-flex-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Todo)
 (@def completion-initials-expand (str: String table: CompletionTable pred: CompletionPredicate?)
       String|Nil)
 ;; The result is a list of completion candidates whose final cdr may be an
 ;; integer base-size instead of nil, i.e. an improper list. The type
 ;; language has no alias for a list-with-non-nil-tail shape, and authoring
 ;; may not define new aliases to express it.
 (@def completion-initials-all-completions
       (string: String table: CompletionTable pred: CompletionPredicate? _point: Integer)
       Todo)
 (@def completion-initials-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? _point: Integer)
       Nil|True|Cons<String~Integer>)
 (@def completion-shorthand-try-completion
       (string: String table: CompletionTable pred: CompletionPredicate? point: Integer)
       Nil|Cons<String~Integer>)
 (@def completion-shorthand-all-completions
       (_string: String _table: CompletionTable _pred: CompletionPredicate? _point: Integer)
       Nil)
 (@def completing-read-default
       (prompt: String collection: CompletionTable &optional predicate: CompletionPredicate?
        require-match: Any initial-input: String|Cons<String~Integer>?
        hist: Symbol|Cons<Symbol~Integer>? def: String|List<String>? inherit-input-method: Bool)
       String)
 (@def minibuffer-insert-file-name-at-point () Nil)
 (@def minibuffer-beginning-of-buffer (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Nil|Integer)
 ;; The macro only executes its body when a live selected-window exists
 ;; before the minibuffer was entered; otherwise it returns nil. `$body'
 ;; cannot express this extra nil branch alongside the body's own type.
 (@check with-minibuffer-selected-window ($todo))
 (@def minibuffer-recenter-top-bottom (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Any)
 (@def minibuffer-scroll-up-command (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Any)
 (@def minibuffer-scroll-down-command (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Any)
 (@def minibuffer-scroll-other-window (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Any)
 (@def minibuffer-scroll-other-window-down (&optional arg: Integer|Cons<Integer~Any>|@-|Nil) Any)
 ;; The macro first ensures a completions window exists via a side-effecting
 ;; call, then conditionally executes body in that window. `$body' cannot
 ;; express this precondition-establishing step before the body runs.
 (@check with-minibuffer-completions-window ($todo))
 (@def minibuffer-next-completion (&optional n: Integer? vertical: Bool) Any)
 (@def minibuffer-previous-completion (&optional n: Integer?) Any)
 (@def minibuffer-next-line-completion (&optional n: Integer?) Any)
 (@def minibuffer-previous-line-completion (&optional n: Integer?) Any)
 (@def minibuffer-next-column-completion (&optional n: Integer?) Any)
 (@def minibuffer-previous-column-completion (&optional n: Integer?) Any)
 (@def minibuffer-choose-completion (&optional no-exit: Bool no-quit: Bool) Any)
 (@def minibuffer-choose-completion-or-exit (&optional no-exit: Bool no-quit: Bool) Any)
 (@def minibuffer-complete-history () Boolean)
 (@def minibuffer-complete-defaults () Boolean)
 (@def format-prompt (prompt: String default: Any &rest format-args: &List) String)
)

;;; ============================================================
;;; On screen keyboard support.

(et-declare
 (@def minibuffer-setup-on-screen-keyboard () Any)
 (@def minibuffer-exit-on-screen-keyboard () Any)
 ;; `minibuffer-regexp-mode' is defined via `define-minor-mode', which
 ;; expands into a variable definition, keymap wiring, and a toggle function.
 ;; There is no checker shortcut for a minor-mode definition form.
 (@check minibuffer-regexp-mode ($todo))
 ;; `minibuffer-nonselected-mode' is defined via `define-minor-mode', which
 ;; expands into a variable definition, keymap wiring, and a toggle function.
 ;; There is no checker shortcut for a minor-mode definition form.
 (@check minibuffer-nonselected-mode ($todo))
)

;;; ============================================================
