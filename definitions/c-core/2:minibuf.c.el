;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Minibuffer state

(et-declare
 (@def active-minibuffer-window () Window?)
 (@def set-minibuffer-window (window: Window) Window)
 (@def minibufferp (&optional buffer: Buffer|String? live: Bool) Boolean)
 (@def innermost-minibuffer-p (&optional buffer: Buffer?) Boolean)
 (@def minibuffer-innermost-command-loop-p (&optional buffer: Buffer?) Boolean)
 (@def abort-minibuffers () Nil)
 (@def minibuffer-prompt-end () Integer)
 (@def minibuffer-contents () String)
 (@def minibuffer-contents-no-properties () String))

;;; ============================================================
;;; Minibuffer input

(et-declare
 ;; The return type depends on whether READ is nil (String) or non-nil (the
 ;; Lisp object read from the input, which can be any type). The type
 ;; language cannot yet express a return type conditioned on a parameter's
 ;; runtime value.
 (@def read-from-minibuffer
       (prompt: String
        &optional initial-contents: String|Cons<String~Integer?>?
        keymap: (or Nil Cons<@keymap~Any> Symbol) read: Bool
        hist: Symbol|Cons<Symbol~Integer?>?
        default-value: String|List<String>? inherit-input-method: Bool)
       Todo)
 (@def read-string
       (prompt: String
        &optional initial-input: String|Cons<String~Integer?>?
        history: Symbol|Cons<Symbol~Integer?>?
        default-value: String|List<String>? inherit-input-method: Bool)
       String)
 (@def read-command (prompt: String &optional default-value: Symbol|String|List<String>?) Symbol?)
 (@def read-variable (prompt: String &optional default-value: Symbol|String|List<String>?) Symbol?)
 (@def read-buffer
       (prompt: String &optional def: Buffer|String|List<Buffer|String>?
        require-match: Any predicate: AnyFn?)
       String))

;;; ============================================================
;;; Completion

(et-declare
 (@def try-completion
       (string: String
        collection: (or (Emacs hash-table) Obarray Vector
                        List<Cons<String|Symbol~Any>|String|Symbol>
                        AnyFn)
        &optional predicate: AnyFn?)
       String|Boolean)
 (@def all-completions
       (string: String
        collection: (or (Emacs hash-table) Obarray Vector
                        List<Cons<String|Symbol~Any>|String|Symbol>
                        AnyFn)
        &optional predicate: AnyFn?)
       ListFresh<String>)
 (@def completing-read
       (prompt: String
        collection: (or (Emacs hash-table) Obarray Vector
                        List<Cons<String|Symbol~Any>|String|Symbol>
                        AnyFn)
        &optional predicate: AnyFn? require-match: Any
        initial-input: String|Cons<String~Integer?>?
        hist: Symbol|Cons<Symbol~Integer?>?
        def: String|List<String>? inherit-input-method: Bool)
       String)
 ;; A non-nil return is documented to mean "valid completion", but when
 ;; PREDICATE is supplied, the value returned is whatever PREDICATE itself
 ;; returns. The type language cannot yet express a callback whose
 ;; application result determines the caller's own return type.
 (@def test-completion
       (string: String
        collection: (or (Emacs hash-table) Obarray Vector
                        List<Cons<String|Symbol~Any>|String|Symbol>
                        AnyFn)
        &optional predicate: AnyFn?)
       Bool)
 ;; The shape of the result depends on the value of FLAG (nil, t, any other
 ;; value, or `metadata'), mirroring the completion-table function protocol.
 ;; The type language cannot yet express a return type conditioned on a
 ;; parameter's runtime value.
 (@def internal-complete-buffer (string: String predicate: AnyFn? flag: Any) Todo)
 (@def assoc-string
       (key: String|Symbol list: List<Cons<String|Symbol~Any>|String|Symbol>
        &optional case-fold: Bool)
       Cons<String|Symbol~Any>|String|Symbol|Nil))

;;; ============================================================
;;; Minibuffer information

(et-declare
 (@def minibuffer-depth () Integer)
 (@def minibuffer-prompt () String?))

;;; ============================================================
