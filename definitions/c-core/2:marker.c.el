;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Operations on markers

(et-declare
 (@def marker-buffer (marker: Marker) Buffer?)
 (@def marker-position (marker: Marker) Integer?)
 (@def marker-last-position (marker: Marker) Integer)
 (@def set-marker ([(<= M Marker)] marker: M position: IntOrMarker? &optional buffer: Buffer?) M)
 (@def copy-marker (&optional marker: IntOrMarker? type: Bool) Marker)
 (@def marker-insertion-type (marker: Marker) Boolean)
 (@def set-marker-insertion-type (marker: Marker type: [T]) T))

;;; ============================================================
