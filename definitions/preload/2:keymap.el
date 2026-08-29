;; -*- lexical-binding: t; -*-

(et-declare
 (@def keymap-set (keymap: Cons<@keymap~Any>|Symbol key: String definition: Any) Any)
 (@def keymap-global-set (key: String command: Any &optional interactive: Bool) Any)
 (@def keymap-local-set (key: String command: Any &optional interactive: Bool) Any)
 (@def keymap-global-unset (key: String &optional remove: Bool) Nil)
 (@def keymap-local-unset (key: String &optional remove: Bool) Nil)
 (@def keymap-unset (keymap: Cons<@keymap~Any>|Symbol key: String &optional remove: Bool) Nil)
 (@def keymap-substitute
       (keymap: Cons<@keymap~Any>|Symbol olddef: Any newdef: Any
        &optional oldmap: Cons<@keymap~Any>|Symbol? prefix: String?)
       Any)
 (@def keymap-set-after
       (keymap: Cons<@keymap~Any>|Symbol key: String definition: Any
        &optional after: String|Symbol|Integer)
       Any)
 (@def key-parse (keys: String) Vector<Integer|Symbol>)
 (@def key-valid-p (keys: Any) Boolean)
 (@def key-translate (from: String to: String?) Integer|Nil)
 (@def key-translate-select () String)
 (@def key-translate-remove (from: String) Integer|Nil)
 (@def keymap-lookup
       (keymap: &Cons<@keymap~Any>|Symbol|&List<&Cons<@keymap~Any>|Symbol>?
        key: String &optional accept-default: Bool no-remap: Bool position: Any)
       Any)
 (@def keymap-local-lookup (keys: String &optional accept-default: Bool) Any)
 (@def keymap-global-lookup (keys: String &optional accept-default: Bool message: Bool) Any)
 (@def define-keymap (&rest definitions: &List<Any>) Cons<@keymap~Any>|Symbol)
 ;; `defvar-keymap' is a macro whose body mixes non-evaluated leading
 ;; keyword options (some rewritten specially, like `:prefix t'), quoted
 ;; key/definition pairs, and a `:repeat' plist that generates extra `put'
 ;; forms at expansion time. None of this fits `$body' (a plain sequence
 ;; of evaluated expressions) or `$fn' (fixed positional operand types
 ;; with a fixed return). A future checker needs macro-expansion-aware
 ;; handling of plist-shaped operands.
 (@check defvar-keymap ($todo))
 (@def make-non-key-event ([(<= T Symbol)] symbol: T) T)
 (@def keymap-read-only-bind (binding: [T]) (Tuple @menu-item (Literal "") T :filter Closure)))
