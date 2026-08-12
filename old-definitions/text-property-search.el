;; Type definitions for lisp/emacs-lisp/text-property-search.el.

;;; prop-match

;; Redeclaring the struct is what gives `make-prop-match', `prop-match-p',
;; the copier, and the accessors their types. The slots mirror the real
;; definition: a matched region and the value the property had in it.

(cl-defstruct (prop-match)
  (beginning nil :et Integer)
  (end nil :et Integer)
  (value nil :et Any))


;;; Searching

;; PREDICATE decides whether the property's value at a position matches
;; VALUE: nil means "not `equal', and non-nil", t means `equal', and a
;; function is called with VALUE and the property value.

(et-declare
 (@alias PropMatchPredicate (or Boolean (Function (Args Any Any) Any)))

 (@function text-property-search-forward (property &optional value predicate not-current)
            (property Symbol) (value Any)
            (predicate PropMatchPredicate) (not-current Any)
            (@return *prop-match|Nil))
 (@function text-property-search-backward (property &optional value predicate not-current)
            (property Symbol) (value Any)
            (predicate PropMatchPredicate) (not-current Any)
            (@return *prop-match|Nil)))

(et-test
 (et-assert-resolve *prop-match|Nil (text-property-search-forward 'face))
 (et-assert-resolve *prop-match|Nil
   (text-property-search-backward 'face "x" #'equal t))
 (et-assert-resolve-errors (text-property-search-forward "face"))
 (et-assert-resolve-errors (text-property-search-forward 'face nil 5))

 ;; Accessors come from the struct definition above
 (et-assert-resolve Integer
   (prop-match-beginning (make-prop-match :beginning 1 :end 2)))
 (et-assert-resolve-errors (make-prop-match :beginning "1")))
