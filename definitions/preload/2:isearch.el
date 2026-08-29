;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Configuration variables

(et-declare
 (@variable search-exit-option Boolean|@edit|@append)
 (@variable search-slow-window-lines Integer)
 (@variable search-slow-speed Integer)
 (@variable search-upper-case Bool)
 (@variable search-nonincremental-instead Boolean)
 (@variable search-whitespace-regexp String?)
 (@variable search-invisible Boolean|@open)
 (@variable isearch-hide-immediately Boolean)
 (@variable isearch-resume-in-command-history Boolean)
 (@variable isearch-wrap-pause Boolean|@no|@no-ding)
 (@variable isearch-repeat-on-direction-change Boolean)
 (@variable isearch-mode-hook List<AnyFn>)
 (@variable isearch-update-post-hook List<AnyFn>)
 (@variable isearch-mode-end-hook List<AnyFn>)
 (@variable isearch-mode-end-hook-quit Boolean)
 (@variable isearch-message-function fn?)
 (@variable isearch-wrap-function fn?)
 (@variable isearch-push-state-function fn?)
 (@variable isearch-filter-predicate (fn (Args IntOrMarker IntOrMarker) Boolean))
 (@variable isearch-text-conversion-style Any)
 (@variable search-ring List<String>)
 (@variable regexp-search-ring List<String>)
 (@variable search-ring-max Integer)
 (@variable regexp-search-ring-max Integer)
 (@variable search-ring-yank-pointer List<String>?)
 (@variable regexp-search-ring-yank-pointer List<String>?)
 (@variable search-ring-update Boolean)
 (@variable search-default-mode Boolean|(fn (Args String Bool?) String)))

;;; ============================================================
;;; Highlighting customization

(et-declare
 (@variable search-highlight Boolean)
 (@variable search-highlight-submatches Boolean)
 (@variable isearch-face Symbol)
 (@variable isearch-lazy-highlight Boolean|@all-windows)
 (@variable isearch-lazy-count Boolean)
 (@variable lazy-highlight-cleanup Boolean)
 (@variable lazy-highlight-initial-delay Number)
 (@variable lazy-highlight-no-delay-length Integer)
 (@variable lazy-highlight-interval Number)
 (@variable lazy-highlight-max-at-a-time Integer?)
 (@variable lazy-highlight-buffer-max-at-a-time Integer?)
 (@variable lazy-highlight-buffer Boolean)
 (@variable lazy-count-prefix-format String?)
 (@variable lazy-count-suffix-format String?)
 (@variable lazy-count-invisible-format String?))

;;; ============================================================
;;; Help and keymaps

(et-declare
 (@variable isearch-help-map Cons<@keymap~Any>)
 (@def isearch-help-for-help () Any)
 (@def isearch-describe-bindings () Any)
 (@def isearch-describe-key () Any)
 (@def isearch-describe-mode () Any)
 (@def isearch-tmm-menubar () Any)
 (@variable isearch-menu-bar-commands List<Symbol>)
 (@variable isearch-mode-map Cons<@keymap~Any>)
 (@variable isearch-menu-bar-map Cons<@keymap~Any>)
 (@variable isearch-tool-bar-old-map Cons<@keymap~Any>?)
 (@def isearch-tool-bar-image (image-name: String) Any)
 (@variable isearch-tool-bar-map Cons<@keymap~Any>)
 (@variable minibuffer-local-isearch-map Cons<@keymap~Any>))

;;; ============================================================
;;; Mode state variables

(et-declare
 (@variable isearch-forward Boolean)
 (@variable isearch-regexp Boolean)
 (@variable isearch-regexp-function Boolean|(fn (Args String Bool?) String))
 (@variable isearch-lax-whitespace Boolean)
 (@variable isearch-regexp-lax-whitespace Boolean)
 (@variable isearch-cmds List<Any>)
 (@variable isearch-string String)
 (@variable isearch-message String)
 (@variable isearch-message-prefix-add String?)
 (@variable isearch-message-suffix-add String?)
 (@variable isearch-success Boolean)
 (@variable isearch-error String?)
 (@variable isearch-other-end Integer?)
 (@variable isearch-wrapped Boolean)
 (@variable isearch-barrier Integer)
 (@variable isearch-just-started Boolean)
 (@variable isearch-start-hscroll Integer)
 (@variable isearch-match-data List<Integer|Marker?>)
 (@variable isearch-case-fold-search Boolean|@yes)
 (@variable isearch-invisible Boolean|@open)
 (@variable isearch-last-case-fold-search Boolean|@yes)
 (@variable isearch-original-minibuffer-message-timeout Any)
 (@variable isearch-adjusted Boolean)
 (@variable isearch-slow-terminal-mode Boolean)
 (@variable isearch-small-window Boolean)
 (@variable isearch-opoint Integer)
 (@variable isearch-window-configuration WindowConfiguration?)
 (@variable isearch-yank-flag Boolean)
 (@variable isearch-op-fun fn?)
 (@variable isearch-recursive-edit Boolean)
 (@variable isearch-nonincremental Boolean)
 (@variable isearch-new-nonincremental Boolean)
 (@variable isearch-new-forward Boolean)
 (@variable isearch-opened-overlays List<Overlay>)
 (@variable isearch-hidden Boolean)
 (@variable isearch-input-method-function Any)
 (@variable isearch-mode String?))

;;; ============================================================
;;; Entry points

(et-declare
 (@def isearch-forward (&optional regexp-p: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-forward-regexp (&optional not-regexp: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-forward-word (&optional not-word: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-forward-symbol (&optional _not-symbol: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-backward (&optional regexp-p: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-backward-regexp (&optional not-regexp: Bool no-recursive-edit: Bool) Boolean)
 (@def isearch-forward-symbol-at-point (&optional arg: Any) Any)
 (@variable isearch-forward-thing-at-point List<Symbol>)
 (@def isearch-forward-thing-at-point () Any))

;;; ============================================================
;;; Mode setup and updating

(et-declare
 (@def isearch-mode (forward: Bool &optional regexp: Bool op-fun: fn? recursive-edit: Bool regexp-function: Boolean|(fn (Args String Bool?) String)) Boolean)
 (@def isearch-update () Any)
 (@def isearch-done (&optional nopush: Bool edit: Bool) Any)
 (@variable isearch-mouse-commands List<Symbol>)
 (@def isearch-mouse-leave-buffer () Any)
 (@def isearch-update-ring (string: String &optional regexp: Bool) Any)
 (@def isearch-string-propertize (string: String &optional properties: PlistOf<Symbol~Any>) String)
 (@def isearch-update-from-string-properties (string: String) Any))

;;; ============================================================
;;; Search status stack

(et-declare
 (@def isearch-pop-state () Any)
 (@def isearch-push-state () List<Any>))

;;; ============================================================
;;; Isearch commands

(et-declare
 (@def isearch-exit () Any)
 (@def isearch-fail-pos (&optional msg: Bool) Integer?)
 (@variable isearch-new-regexp-function Boolean|(fn (Args String Bool?) String))
 (@variable isearch-suspended Boolean)
 ;; The body forms must type-check under the macro's dynamic rebinding of
 ;; many isearch-* globals, but the macro's return value comes from later
 ;; isearch-search/isearch-push-state/isearch-update calls made after an
 ;; unwind-protect, not from the body's final expression. Neither $body nor
 ;; $fn can express "check body, then return a value unrelated to it."
 (@check with-isearch-suspended ($todo))
 (@def isearch-edit-string () Any)
 (@def isearch-nonincremental-exit-minibuffer () Any)
 (@def isearch-forward-exit-minibuffer () Any)
 (@def isearch-reverse-exit-minibuffer () Any)
 (@def isearch-cancel () Any)
 (@def isearch-abort () Any)
 (@def isearch-repeat (direction: @forward|@backward &optional count: Integer?) Any)
 (@def isearch-repeat-forward (&optional arg: Any) Any)
 (@def isearch-repeat-backward (&optional arg: Any) Any)
 (@def isearch-beginning-of-buffer (&optional arg: Integer?) Any)
 (@def isearch-end-of-buffer (&optional arg: Integer?) Any))

;;; ============================================================
;;; Toggles and word/symbol search

(et-declare
 ;; MODE, KEY, and FUNCTION are used as compile-time code-generation data
 ;; (interned into a new command name, spliced into a key string, and
 ;; conditionally quoted), not evaluated as ordinary runtime operands, and
 ;; BODY is spliced into a newly-defined command rather than checked at the
 ;; macro's own call site. Neither $body nor $fn can express this
 ;; code-generation pattern.
 (@check isearch-define-mode-toggle ($todo))
 (@def [isearch-toggle-word isearch-toggle-symbol isearch-toggle-char-fold
        isearch-toggle-regexp isearch-toggle-lax-whitespace
        isearch-toggle-case-fold isearch-toggle-invisible]
       () Any)
 (@variable isearch-message-properties PlistOf<Symbol~Any>)
 (@def word-search-regexp (string: String &optional lax: Bool) String)
 (@def word-search-backward (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def word-search-forward (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def word-search-backward-lax (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def word-search-forward-lax (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def isearch-symbol-regexp (string: String &optional lax: Bool) String)
 (@def search-forward-lax-whitespace (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def search-backward-lax-whitespace (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def re-search-forward-lax-whitespace (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?)
 (@def re-search-backward-lax-whitespace (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?) Integer?))

;;; ============================================================
;;; Replace and occur commands

(et-declare
 (@def isearch-query-replace (&optional arg: Any regexp-flag: Bool) Any)
 (@def isearch-query-replace-regexp (&optional arg: Any) Any)
 (@def isearch-occur (regexp: String &optional nlines: Integer?) Any)
 (@def isearch-highlight-regexp () Any)
 (@def isearch-highlight-lines-matching-regexp () Any))

;;; ============================================================
;;; Editing the search string

(et-declare
 (@def isearch-delete-char () Any)
 (@def isearch-del-char (&optional arg: Integer?) Any)
 (@def isearch-yank-string (string: String) Any)
 (@def isearch-yank-kill () Any)
 (@def isearch-yank-from-kill-ring () Any)
 (@def isearch-yank-pop () Any)
 (@def isearch-yank-pop-only (&optional arg: Any) Any)
 (@def isearch-yank-x-selection () Any)
 (@def isearch-mouse-2 (click: Any) Any)
 (@def isearch-xterm-paste (event: Any) Any)
 (@def isearch-yank-internal (jumpform: (fn Nil IntOrMarker)) Any)
 (@def isearch-yank-char-in-minibuffer (&optional arg: Integer) Any)
 (@def isearch-yank-char (&optional arg: Integer) Any)
 (@def isearch-yank-word-or-char (&optional arg: Integer) Any)
 (@def isearch-yank-symbol-or-char (&optional arg: Integer) Any)
 (@def isearch-yank-word (&optional arg: Integer) Any)
 (@def isearch-yank-until-char (char: Integer &optional arg: Integer?) Any)
 (@def isearch-yank-line (&optional arg: Integer) Any))

;;; ============================================================
;;; Character input

(et-declare
 (@def isearch-char-by-name (&optional count: Integer?) Any)
 (@def isearch-emoji-by-name (&optional count: Integer?) Any)
 (@def isearch-search-and-update () Any)
 (@def isearch-backslash (str: String) Boolean)
 (@def isearch-fallback (want-backslash: Bool &optional allow-invalid: Bool to-barrier: Bool) Any))

;;; ============================================================
;;; Scrolling during search

(et-declare
 (@variable isearch-allow-scroll Boolean|@unlimited)
 (@variable isearch-allow-motion Boolean)
 (@variable isearch-motion-changes-direction Boolean)
 (@variable isearch-allow-prefix Boolean)
 (@def isearch-string-out-of-window (isearch-point: IntOrMarker) Nil|@above|@below)
 (@def isearch-back-into-window (above: Bool isearch-point: IntOrMarker) Integer)
 (@variable isearch-pre-scroll-point Integer?)
 (@variable isearch-pre-move-point Integer?)
 (@variable isearch-yank-on-move Boolean|@shift))

;;; ============================================================
;;; Command processing

(et-declare
 (@def isearch-pre-command-hook () Any)
 (@def isearch-post-command-hook () Any)
 (@def isearch-quote-char (&optional count: Integer?) Any)
 (@def isearch-printing-char (&optional char: Integer? count: Integer?) Any)
 (@def isearch-process-search-char (char: Integer &optional count: Integer?) Any)
 (@def isearch-process-search-string (string: String message: String) Any))

;;; ============================================================
;;; Search ring commands

(et-declare
 (@def isearch-ring-adjust1 (advance: Bool) Any)
 (@def isearch-ring-adjust (advance: Bool) Any)
 (@def isearch-ring-advance () Any)
 (@def isearch-ring-retreat () Any)
 (@def isearch-complete1 () Boolean|String)
 (@def isearch-complete () Any)
 (@def isearch-complete-edit () Any))

;;; ============================================================
;;; Message display

(et-declare
 (@def isearch-message (&optional c-q-hack: Bool ellipsis: Bool) String)
 (@def isearch-message-prefix (&optional ellipsis: Bool nonincremental: Bool) String)
 (@def isearch-message-suffix (&optional c-q-hack: Bool) String)
 (@def isearch-lazy-count-format (&optional suffix-p: Bool) String))

;;; ============================================================
;;; Search execution

(et-declare
 (@variable isearch-search-fun-function (fn Nil (fn (Args String IntOrMarker? Bool Integer?) Integer?)))
 (@def isearch-search-fun () (fn (Args String IntOrMarker? Bool Integer?) Integer?))
 (@def isearch-search-fun-default () (fn (Args String IntOrMarker? Bool Integer?) Integer?))
 (@def isearch-search-string (string: String bound: IntOrMarker? noerror: Bool) Integer?)
 (@def isearch-search () Any))

;;; ============================================================
;;; Overlay and visibility management

(et-declare
 (@def isearch-open-overlay-temporary (ov: Overlay) Any)
 (@def isearch-open-necessary-overlays (ov: Overlay) Any)
 (@def isearch-clean-overlays () Any)
 (@def isearch-intersects-p (start0: IntOrMarker end0: IntOrMarker start1: IntOrMarker end1: IntOrMarker) Boolean)
 (@def isearch-close-unnecessary-overlays (beg: IntOrMarker end: IntOrMarker) Nil)
 (@def isearch-range-invisible (beg: IntOrMarker end: IntOrMarker) Boolean)
 (@def isearch-filter-visible (beg: IntOrMarker end: IntOrMarker) Boolean))

;;; ============================================================
;;; Utility functions

(et-declare
 (@def isearch-no-upper-case-p (string: String regexp-flag: Bool) Boolean)
 (@def isearch-text-char-description (c: Integer) String)
 (@def isearch-unread (&rest char-or-events: &List) List<Any>))

;;; ============================================================
;;; Highlighting the match

(et-declare
 (@variable isearch-overlay Overlay?)
 (@variable isearch-submatches-overlays List<Overlay>)
 (@def isearch-highlight (beg: IntOrMarker end: IntOrMarker &optional match-data: &List<Integer|Marker?>?) Any)
 (@def isearch-dehighlight () Any))

;;; ============================================================
;;; Lazy highlighting

(et-declare
 (@variable isearch-lazy-highlight-overlays List<Overlay>)
 (@variable isearch-lazy-highlight-wrapped Boolean)
 (@variable isearch-lazy-highlight-start-limit Integer?)
 (@variable isearch-lazy-highlight-end-limit Integer?)
 (@variable isearch-lazy-highlight-start Integer?)
 (@variable isearch-lazy-highlight-end Integer?)
 (@variable isearch-lazy-highlight-timer Any)
 (@variable isearch-lazy-highlight-last-string String?)
 (@variable isearch-lazy-highlight-window Window?)
 (@variable isearch-lazy-highlight-window-group List<Window>?)
 (@variable isearch-lazy-highlight-window-start Integer?)
 (@variable isearch-lazy-highlight-window-end Integer?)
 (@variable isearch-lazy-highlight-window-start-changed Boolean)
 (@variable isearch-lazy-highlight-window-end-changed Boolean)
 (@variable isearch-lazy-highlight-point-min Integer?)
 (@variable isearch-lazy-highlight-point-max Integer?)
 (@variable isearch-lazy-highlight-buffer Boolean)
 (@variable isearch-lazy-highlight-case-fold-search Boolean|@yes)
 (@variable isearch-lazy-highlight-invisible Boolean|@open)
 (@variable isearch-lazy-highlight-regexp Boolean)
 (@variable isearch-lazy-highlight-lax-whitespace Boolean)
 (@variable isearch-lazy-highlight-regexp-lax-whitespace Boolean)
 (@variable isearch-lazy-highlight-regexp-function Boolean|(fn (Args String Bool?) String))
 (@variable isearch-lazy-highlight-forward Boolean)
 (@variable isearch-lazy-highlight-error String?)
 (@variable isearch-lazy-count-current Integer?)
 (@variable isearch-lazy-count-total Integer?)
 (@variable isearch-lazy-count-invisible Integer?)
 (@variable isearch-lazy-count-hash HashTable<Integer~Integer>)
 (@variable lazy-count-update-hook List<AnyFn>)
 (@def lazy-highlight-cleanup (&optional force: Bool procrastinate: Bool) Any)
 (@def isearch-lazy-highlight-new-loop (&optional beg: IntOrMarker? end: IntOrMarker?) Any)
 (@def isearch-lazy-highlight-search (string: String bound: IntOrMarker?) Integer?)
 (@def isearch-lazy-highlight-match (mb: Integer me: Integer) Any)
 (@def isearch-lazy-highlight-start () Any)
 (@def isearch-lazy-highlight-update () Any)
 (@def isearch-lazy-highlight-buffer-update () Any))

;;; ============================================================
;;; Misc search helpers

(et-declare
 (@variable minibuffer-lazy-count-format String?)
 (@def minibuffer-lazy-highlight-setup
       (&key highlight: Boolean|@all-windows cleanup: Boolean
             transform: (fn (Args String) String)
             filter: (or Nil (fn (Args IntOrMarker IntOrMarker) Boolean))
             regexp: Boolean
             regexp-function: Boolean|(fn (Args String Bool?) String)
             case-fold: Boolean|@yes
             lax-whitespace: Boolean)
       fn)
 (@def isearch-search-fun-in-noncontiguous-region
       (search-fun: (or Nil (fn (Args String IntOrMarker? Bool Integer?) Integer?))
        bounds: &List<&Cons<Integer~Integer>>)
       (fn (Args String IntOrMarker? Bool Integer?) Integer?))
 (@def isearch-search-fun-in-text-property
       (search-fun: (or Nil (fn (Args String IntOrMarker? Bool Integer?) Integer?))
        properties: Symbol|&List<Symbol>)
       (fn (Args String IntOrMarker? Bool Integer?) Integer?))
 (@def search-within-boundaries
       (search-fun: (or Nil (fn (Args String IntOrMarker? Bool Integer?) Integer?))
        get-fun: (fn (Args Integer) Bool)
        next-fun: (fn (Args Integer) Integer?)
        string: String
        &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def isearch-resume (string: String regexp: Bool word: Boolean|(fn (Args String Bool?) String) forward: Bool message: String case-fold: Boolean|@yes) Any)
 (@variable isearch-fold-quotes-mode Boolean)
 (@def isearch-fold-quotes-mode (&optional arg: Any) Any)
 (@def isearch-mode-help () Any))

;;; ============================================================
