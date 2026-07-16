;; Type definitions for builtins defined in Emacs' src/font.c.
;;
;; There are three font datatypes, and they are disjoint: internally they
;; are all the same pseudovector tag, distinguished only by an exact
;; length, so no value is ever two of them at once. `Font' is the union of
;; all three (what `fontp' tests for).
;;
;; They form a pipeline:
;;
;;   FontSpec    a *query*: font properties, some left unspecified.
;;      |          `list-fonts'/`find-font' match it against the system
;;      v
;;   FontEntity  a *match*: fully instantiated properties, not yet opened.
;;      |          `open-font' opens it
;;      v
;;   FontObject  an *opened* font, ready for display.


;;; Predicates

;; With no EXTRA-TYPE, `fontp' tests for any of the three, so it narrows
;; to `Font'.  EXTRA-TYPE (`font-spec', `font-entity' or `font-object')
;; asks about one specific kind instead, which the return type cannot
;; express, so it is only accurate when EXTRA-TYPE is omitted.

(et-declare
 (@function fontp (object &optional extra-type)
            (@generics [T]) (object T) (extra-type Symbol|Nil)
            (@return (or (and True (bindsof (and T Font)))
                         (and Nil (bindsof (subtract T Font)))))))

(et-test
 (et-assert-resolve True&{$a::Font}
  (fontp (:type Font&{::$a})))
 (et-assert-resolve (or True&{$a::FontObject} Nil&{$a::String})
  (fontp (:type {::$a}&{FontObject|String}))))


;;; Font specs

;; PROPS are KEY VALUE pairs (`:family', `:weight', `:size', ...).

(et-declare
 (@function font-spec (&rest props)
            (props KVPList<Symbol~Any>) (@return FontSpec))

 ;; Properties can be read from any of the three, and `font-put' hands
 ;; back the value it stored.
 (@function font-get (font key)
            (font Font) (key Symbol) (@return Any))
 (@function font-put (font key val)
            (@generics [V]) (font Font) (key Symbol) (val V) (@return V)))

(et-test
 (et-assert-resolve FontSpec (font-spec :family "Monospace" :size 12))
 (et-assert-resolve Any
  (font-get (:type FontSpec) (:type Symbol)))
 (et-assert-resolve String
  (font-put (:type FontObject) (:type Symbol) (:type String)))
 (et-assert-resolve-errors
 (font-get (:type String) (:type Symbol))))


;;; Matching a spec against the system

;; FRAME defaults to the selected frame.  `list-fonts' returns nil when
;; nothing matches, and `find-font' is `list-fonts' limited to one result.

(et-declare
 (@function list-fonts (font-spec &optional frame num prefer)
            (font-spec FontSpec) (frame Frame|Nil) (num Integer|Nil) (prefer FontSpec|Nil)
            (@return List<FontEntity>))
 (@function find-font (font-spec &optional frame)
            (font-spec FontSpec) (frame Frame|Nil)
            (@return FontEntity|Nil))
 (@function font-match-p (spec font)
            (spec FontSpec) (font Font) (@return Boolean))
 (@function font-family-list (&optional frame)
            (frame Frame|Nil) (@return List<String>)))

(et-test
 (et-assert-resolve List<FontEntity>
  (list-fonts (:type FontSpec)))
 (et-assert-resolve FontEntity|Nil
  (find-font (:type FontSpec)))
 (et-assert-resolve Boolean
  (font-match-p (:type FontSpec) (:type FontObject)))
 (et-assert-resolve List<String>
  (font-family-list))
 ;; Only a spec can be matched against; an entity is already a match.
 (et-assert-resolve-errors
 (find-font (:type FontEntity))))


;;; Opening a font

(et-declare
 (@function open-font (font-entity &optional size frame)
            (font-entity FontEntity) (size Integer|Nil) (frame Frame|Nil)
            (@return FontObject|Nil))
 (@function close-font (font-object &optional frame)
            (font-object FontObject) (frame Frame|Nil) (@return Nil))
 ;; The font used to display the character at POSITION, in WINDOW's buffer
 ;; or -- when STRING is given -- at that index into STRING.
 (@function font-at (position &optional window string)
            (position IntOrMarker) (window Window|Nil) (string String|Nil)
            (@return FontObject|Nil)))

(et-test
 (et-assert-resolve FontObject|Nil
  (open-font (:type FontEntity)))
 (et-assert-resolve Nil
  (close-font (:type FontObject)))
 (et-assert-resolve FontObject|Nil
  (font-at (:type Integer)))
 (et-assert-resolve FontObject|Nil (font-at (point-marker)))
 ;; Only an entity can be opened, and only an object can be closed.
 (et-assert-resolve-errors
 (open-font (:type FontSpec)))
 (et-assert-resolve-errors
 (close-font (:type FontEntity))))


;;; Names and caches

(et-declare
 ;; Nil when the name is too long to be represented as an XLFD.
 (@function font-xlfd-name (font &optional fold-wildcards long-xlfds)
            (font Font) (fold-wildcards Any) (long-xlfds Any)
            (@return String|Nil))
 (@function clear-font-cache () (@return Nil)))

(et-test
 (et-assert-resolve String|Nil
  (font-xlfd-name (:type FontSpec)))
 (et-assert-resolve Nil (clear-font-cache)))
