;; Type definitions for builtins defined in Emacs' src/keymap.c.

;;; Keymap data

;; A keymap is a cons whose car is the literal symbol `keymap'.  Its cdr is
;; intentionally loose: real keymaps may contain alist entries, char-tables,
;; vectors, menu items, parent links, and nested keymaps.

(et-declare
 (@alias Keymap (ConsR @keymap ListR<Any>))
 (@alias KeymapOrList (or Keymap ListR<Keymap>))
 (@alias KeySequence (or String VectorR<Any>))
 (@alias KeymapDefinition (or Nil Symbol String Keymap Cons VectorR<Any> Function<Never~Any>))
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
 (@function define-key-after (keymap key definition &optional after)
            (keymap Keymap) (key KeySequence) (definition KeymapDefinition)
            (after Symbol|Integer|True|Nil)
            (@return Nil))
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
 (@function keymap-unset (keymap key &optional remove)
            (keymap Keymap) (key String) (remove Any)
            (@return Nil))
 (@function keymap-set-after (keymap key definition &optional after)
            (keymap Keymap) (key String) (definition KeymapDefinition)
            (after Symbol|Integer|True|Nil)
            (@return Nil))
 (@function keymap-substitute (keymap olddef newdef &optional oldmap)
            (keymap Keymap) (olddef KeymapDefinition) (newdef KeymapDefinition)
            (oldmap Keymap|Nil)
            (@return Nil))
 (@function keymap-global-set (key command &optional interactive)
            (key String) (command KeymapDefinition) (interactive Any)
            (@return KeymapDefinition))
 (@function keymap-global-unset (key &optional remove)
            (key String) (remove Any)
            (@return Nil))
 (@function keymap-local-set (key command &optional interactive)
            (key String) (command KeymapDefinition) (interactive Any)
            (@return KeymapDefinition))
 (@function keymap-local-unset (key &optional remove)
            (key String) (remove Any)
            (@return Nil))
 (@function global-set-key (key command)
            (key KeySequence) (command KeymapDefinition)
            (@return KeymapDefinition))
 (@function local-set-key (key command)
            (key KeySequence) (command KeymapDefinition)
            (@return KeymapDefinition))
 (@function substitute-key-definition (olddef newdef keymap &optional oldmap)
            (olddef KeymapDefinition) (newdef KeymapDefinition)
            (keymap Keymap) (oldmap Keymap|Nil)
            (@return Nil))
 (@function suppress-keymap (map &optional nodigits)
            (map Keymap) (nodigits Any)
            (@return Nil))
 (@function define-prefix-command (command &optional mapvar name)
            (command Symbol) (mapvar Symbol|Nil) (name String|Nil)
            (@return Symbol))
 (@function use-global-map (keymap)
            (keymap Keymap)
            (@return Nil))
 (@function use-local-map (keymap)
            (keymap Keymap|Nil)
            (@return Nil)))


;;; Tests

(et-test
 (et-assert-call Keymap make-sparse-keymap)
 (et-assert-call Keymap make-keymap String)
 (et-assert-call (or True Nil) keymapp Any)
 (et-assert-call KeymapDefinition define-key Keymap KeySequence KeymapDefinition)
 (et-assert-call Nil define-key-after Keymap KeySequence KeymapDefinition)
 (et-assert-call KeymapDefinition|Integer lookup-key Keymap String)
 (et-assert-call KeymapDefinition key-binding String)
 (et-assert-call ListFresh<Keymap> current-active-maps)
 (et-assert-call KeymapDefinition keymap-lookup Keymap String)
 (et-assert-call KeymapDefinition keymap-set Keymap String KeymapDefinition)
 (et-assert-call Nil keymap-unset Keymap String)
 (et-assert-call Nil keymap-set-after Keymap String KeymapDefinition)
 (et-assert-call Nil keymap-substitute Keymap KeymapDefinition KeymapDefinition)
 (et-assert-call KeymapDefinition keymap-global-set String KeymapDefinition)
 (et-assert-call Nil keymap-global-unset String)
 (et-assert-call KeymapDefinition keymap-local-set String KeymapDefinition)
 (et-assert-call Nil keymap-local-unset String)
 (et-assert-call KeymapDefinition global-set-key KeySequence KeymapDefinition)
 (et-assert-call KeymapDefinition local-set-key KeySequence KeymapDefinition)
 (et-assert-call Nil substitute-key-definition KeymapDefinition KeymapDefinition Keymap)
 (et-assert-call Nil suppress-keymap Keymap)
 (et-assert-call Symbol define-prefix-command Symbol)
 (et-assert-call Nil use-global-map Keymap)
 (et-assert-call Nil use-local-map Keymap|Nil)
 (et-assert-call-errors make-keymap Integer))
