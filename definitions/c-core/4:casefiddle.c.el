;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Case conversion

(et-declare
 (@def upcase ([(<= T Integer|String)] obj: T) T)
 (@def downcase ([(<= T Integer|String)] obj: T) T)
 (@def capitalize ([(<= T Integer|String)] obj: T) T)
 (@def upcase-initials ([(<= T Integer|String)] obj: T) T))

;;; ============================================================
;;; Region case conversion

(et-declare
 (@def upcase-region
       (beg: IntOrMarker end: IntOrMarker &optional region-noncontiguous-p: Bool)
       Nil)
 (@def downcase-region
       (beg: IntOrMarker end: IntOrMarker &optional region-noncontiguous-p: Bool)
       Nil)
 (@def capitalize-region
       (beg: IntOrMarker end: IntOrMarker &optional region-noncontiguous-p: Bool)
       Nil)
 (@def upcase-initials-region
       (beg: IntOrMarker end: IntOrMarker &optional region-noncontiguous-p: Bool)
       Nil))

;;; ============================================================
;;; Word case conversion

(et-declare
 (@def upcase-word (arg: Integer) Nil)
 (@def downcase-word (arg: Integer) Nil)
 (@def capitalize-word (arg: Integer) Nil))

;;; ============================================================
