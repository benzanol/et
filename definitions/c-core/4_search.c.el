;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Matching

(et-declare
 (@def looking-at (regexp: String &optional inhibit-modify: Bool) Boolean)
 (@def posix-looking-at (regexp: String &optional inhibit-modify: Bool) Boolean)
 (@def string-match
       (regexp: String string: String &optional start: Integer? inhibit-modify: Bool)
       Integer?)
 (@def posix-string-match
       (regexp: String string: String &optional start: Integer? inhibit-modify: Bool)
       Integer?))

;;; ============================================================
;;; Search commands

(et-declare
 (@def search-backward
       (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def search-forward
       (string: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def re-search-backward
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def re-search-forward
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def posix-search-backward
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?)
 (@def posix-search-forward
       (regexp: String &optional bound: IntOrMarker? noerror: Bool count: Integer?)
       Integer?))

;;; ============================================================
;;; Match replacement and data

(et-declare
 (@def replace-match
       (newtext: String &optional fixedcase: Bool literal: Bool string: String? subexp: Integer?)
       String?)
 (@def match-beginning (subexp: Integer) Integer?)
 (@def match-end (subexp: Integer) Integer?)
 (@def match-data (&optional integers: Bool reuse: List? reseat: Bool) List<Integer|Marker|Buffer>?)
 (@def set-match-data (list: List &optional reseat: Bool) Nil))

;;; ============================================================
;;; Regexp utilities

(et-declare
 (@def regexp-quote (string: String) String))

;;; ============================================================
;;; Newline cache

(et-declare
 (@def newline-cache-check (&optional buffer: Buffer?) Vector<Vector<Integer>>?))

;;; ============================================================
