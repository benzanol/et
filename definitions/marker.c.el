;; Type definitions for builtins defined in Emacs' src/marker.c.
;;
;; A marker either points into a buffer or points nowhere, and there is no
;; type-level distinction between the two. So `marker-buffer' and
;; `marker-position' are nil-able, and anywhere a position is accepted, nil
;; means "point nowhere".

;;; Marker accessors

(et-declare
 (@function marker-buffer (marker)
            (marker Marker) (@return Buffer|Nil))
 (@function marker-position (marker)
            (marker Marker) (@return Integer|Nil))
 (@function marker-last-position (marker)
            (marker Marker) (@return Integer|Nil))
 (@function marker-insertion-type (marker)
            (marker Marker) (@return Boolean)))

(et-test
 (et-assert-resolve Buffer|Nil (marker-buffer (make-marker)))
 (et-assert-resolve Integer|Nil (marker-position (make-marker)))
 (et-assert-resolve Integer|Nil (marker-last-position (make-marker)))
 (et-assert-resolve Boolean (marker-insertion-type (make-marker)))
 (et-assert-resolve-errors (marker-position 5)))


;;; Creating and moving markers

;; `set-marker' returns the marker it was given, and
;; `set-marker-insertion-type' returns the type it was given.

(et-declare
 (@function set-marker (marker position &optional buffer)
            (marker Marker) (position IntOrMarker|Nil) (buffer Buffer|Nil)
            (@return Marker))
 (@function copy-marker (&optional position type)
            (position IntOrMarker|Nil) (type Any)
            (@return Marker))
 (@function set-marker-insertion-type (marker type)
            (@generics [T])
            (marker Marker) (type T)
            (@return T)))

(et-test
 (et-assert-resolve Marker (set-marker (make-marker) 1))
 (et-assert-resolve Marker (set-marker (make-marker) nil))
 (et-assert-resolve Marker
  (set-marker (:type Marker) (:type Marker) (:type Buffer)))
 (et-assert-resolve Marker (copy-marker))
 (et-assert-resolve Marker (copy-marker 1 t))
 (et-assert-resolve True (set-marker-insertion-type (make-marker) t))
 (et-assert-resolve-errors (set-marker (make-marker) "1")))


;;; Markers in the current buffer

(et-declare
 (@function buffer-has-markers-at (position)
            (position IntOrMarker) (@return Boolean)))

(et-test
 (et-assert-resolve Boolean (buffer-has-markers-at 1))
 (et-assert-resolve-errors (buffer-has-markers-at nil)))
