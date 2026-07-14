;; Type definitions for builtins defined in Emacs' src/keymap.c.

;;; Keymap data

;; A keymap is a cons whose car is the literal symbol `keymap'.  Its cdr is
;; intentionally loose: real keymaps may contain alist entries, char-tables,
;; vectors, menu items, parent links, and nested keymaps.

(et-declare
 (@alias Keymap (ConsR @keymap ListR<Any>))
 (@alias KeymapOrList (or Keymap ListR<Keymap>))
 (@alias KeySequence (or String VectorR<Any>))
 (@alias KeymapDefinition (or Nil Symbol String Keymap Cons VectorR<Any>))
 (@alias MinorModeMapAlist AList<Symbol~Keymap>)
 (@alias EmulationModeMapAlists ListR<Symbol>))


;;; Keymap variables

(et-declare
 (@variable minor-mode-map-alist MinorModeMapAlist)
 (@variable minor-mode-overriding-map-alist MinorModeMapAlist|Nil)
 ;; Each symbol names a variable whose value is a `MinorModeMapAlist'.
 (@variable emulation-mode-map-alists EmulationModeMapAlists)

 (@variable overriding-local-map Keymap|Nil)
 (@variable overriding-terminal-local-map Keymap|Nil)
 (@variable overriding-local-map-menu-flag Boolean)
 (@variable special-event-map Keymap)

 (@variable function-key-map Keymap)
 (@variable key-translation-map Keymap)
 (@variable input-decode-map Keymap)
 (@variable local-function-key-map Keymap)

 (@variable current-key-remap-sequence KeySequence|Nil)
 (@variable where-is-preferred-modifier Symbol|Nil)
 (@variable system-key-alist AList<Symbol~KeySequence>|Nil)
 (@variable minibuffer-local-map Keymap)
 (@variable menu-prompting Boolean))


;;; Constructors and predicates

(et-declare
 (@function keymapp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Keymap)))
                         (and Nil (bindsof (subtract T Keymap))))))
 (@function make-keymap (&optional string)
            (string String|Nil) (@return Keymap))
 (@function make-sparse-keymap (&optional string)
            (string String|Nil) (@return Keymap)))


;;; Lookup and binding

(et-declare
 (@function define-key (keymap key def &optional remove)
            (keymap Keymap) (key KeySequence) (def KeymapDefinition) (remove Any)
            (@return KeymapDefinition))
 (@function lookup-key (keymap key &optional accept-default)
            (keymap KeymapOrList) (key KeySequence) (accept-default Any)
            (@return KeymapDefinition|Integer))
 (@function key-binding (key &optional accept-default no-remap position)
            (key KeySequence) (accept-default Any) (no-remap Any)
            (position Any)
            (@return KeymapDefinition))
 (@function current-active-maps (&optional olp position)
            (olp Any) (position Any)
            (@return ListFresh<Keymap>))
 (@function keymap-lookup (keymap key &optional accept-default no-remap position)
            (keymap KeymapOrList|Nil) (key String) (accept-default Any)
            (no-remap Any) (position Any)
            (@return KeymapDefinition))
 (@function keymap-set (keymap key definition)
            (keymap Keymap) (key String) (definition KeymapDefinition)
            (@return KeymapDefinition))
 (@function keymap-global-set (key command &optional interactive)
            (key String) (command KeymapDefinition) (interactive Any)
            (@return KeymapDefinition))
 (@function keymap-local-set (key command &optional interactive)
            (key String) (command KeymapDefinition) (interactive Any)
            (@return KeymapDefinition)))


;;; Tests

(et-test
 (et-assert-call Keymap make-sparse-keymap)
 (et-assert-call Keymap make-keymap String)
 (et-assert-call (or True Nil) keymapp Any)
 (et-assert-call KeymapDefinition|Integer lookup-key Keymap String)
 (et-assert-call KeymapDefinition key-binding String)
 (et-assert-call ListFresh<Keymap> current-active-maps)
 (et-assert-call KeymapDefinition keymap-lookup Keymap String)
 (et-assert-call-errors make-keymap Integer))
