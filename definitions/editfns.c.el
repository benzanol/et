;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Character and position basics

(et-declare
 (@def char-to-string (char: Integer) String)
 (@def byte-to-string (byte: Integer) String)
 (@def string-to-char (string: String) Integer)
 (@def point () Integer)
 (@def point-marker () Marker)
 (@def goto-char ([(<= P IntOrMarker)] position: P) P)
 (@def region-beginning () Integer)
 (@def region-end () Integer)
 (@def mark-marker () Marker)
 (@def get-pos-property
       (position: IntOrMarker prop: Symbol
                  &optional object: Nil|String|Buffer|Window)
       Any))

;;; ============================================================
;;; Fields and line boundaries

(et-declare
 (@def delete-field (&optional pos: IntOrMarker|Nil) Nil)
 (@def field-string (&optional pos: IntOrMarker|Nil) String)
 (@def field-string-no-properties (&optional pos: IntOrMarker|Nil) String)
 (@def field-beginning
       (&optional pos: IntOrMarker|Nil escape-from-edge: Boolean
                  limit: IntOrMarker|Nil)
       Integer)
 (@def field-end
       (&optional pos: IntOrMarker|Nil escape-from-edge: Boolean
                  limit: IntOrMarker|Nil)
       Integer)
 (@def constrain-to-field
       (new-pos: IntOrMarker|Nil old-pos: IntOrMarker
                &optional escape-from-edge: Boolean only-in-line: Boolean
                inhibit-capture-property: Symbol|Nil)
       Integer)
 (@def pos-bol (&optional n: Integer|Nil) Integer)
 (@def line-beginning-position (&optional n: Integer|Nil) Integer)
 (@def pos-eol (&optional n: Integer|Nil) Integer)
 (@def line-end-position (&optional n: Integer|Nil) Integer))

;;; ============================================================
;;; Excursion and buffer position

(et-declare
 (@check save-excursion ($body))
 (@check save-current-buffer ($body))
 (@def buffer-size (&optional buffer: Buffer|Nil) Integer)
 (@def point-min () Integer)
 (@def point-min-marker () Marker)
 (@def point-max () Integer)
 (@def point-max-marker () Marker)
 (@def gap-position () Integer)
 (@def gap-size () Integer)
 (@def position-bytes (position: IntOrMarker) Integer|Nil)
 (@def byte-to-position (bytepos: Integer) Integer|Nil)
 (@def following-char () Integer)
 (@def preceding-char () Integer)
 (@def bobp () Boolean)
 (@def eobp () Boolean)
 (@def bolp () Boolean)
 (@def eolp () Boolean)
 (@def char-after (&optional pos: IntOrMarker|Nil) Integer|Nil)
 (@def char-before (&optional pos: IntOrMarker|Nil) Integer|Nil))

;;; ============================================================
;;; User and system information

(et-declare
 (@def user-login-name (&optional uid: Integer|Nil) String|Nil)
 (@def user-real-login-name () String)
 (@def user-uid () Integer)
 (@def user-real-uid () Integer)
 (@def group-name (gid: Number) String|Nil)
 (@def group-gid () Integer)
 (@def group-real-gid () Integer)
 (@def user-full-name (&optional uid: Number|String|Nil) String|Nil)
 (@def system-name () String)
 (@def emacs-pid () Integer))

;;; ============================================================
;;; Insertion

(et-declare
 (@def insert (&rest args: &List<String|Integer>) Nil)
 (@def insert-and-inherit (&rest args: &List<String|Integer>) Nil)
 (@def insert-before-markers (&rest args: &List<String|Integer>) Nil)
 (@def insert-before-markers-and-inherit
       (&rest args: &List<String|Integer>)
       Nil)
 (@def insert-char
       (character: Integer &optional count: Integer|Nil inherit: Boolean)
       Nil)
 (@def insert-byte (byte: Integer count: Integer &optional inherit: Boolean) Nil))

;;; ============================================================
;;; Buffer contents and editing

(et-declare
 (@def buffer-substring (start: IntOrMarker end: IntOrMarker) String)
 (@def buffer-substring-no-properties (start: IntOrMarker end: IntOrMarker) String)
 (@def buffer-string () String)
 (@def insert-buffer-substring
       (buffer: Buffer|String &optional start: IntOrMarker|Nil end: IntOrMarker|Nil)
       Nil)
 (@def compare-buffer-substrings
       (buffer1: Buffer|String|Nil start1: IntOrMarker|Nil end1: IntOrMarker|Nil
                buffer2: Buffer|String|Nil start2: IntOrMarker|Nil end2: IntOrMarker|Nil)
       Integer)
 (@def replace-buffer-contents
       (source: Buffer|String &optional max-secs: Number|Nil max-costs: Integer|Nil)
       Boolean)
 (@def subst-char-in-region
       (start: IntOrMarker end: IntOrMarker fromchar: Integer tochar: Integer
               &optional noundo: Boolean)
       Nil)
 (@def translate-region-internal
       (start: IntOrMarker end: IntOrMarker table: String|CharTable)
       Integer)
 (@def delete-region (start: IntOrMarker end: IntOrMarker) Nil)
 (@def delete-and-extract-region (start: IntOrMarker end: IntOrMarker) String))

;;; ============================================================
;;; Buffer restrictions

(et-declare
 (@def widen () Nil)
 (@def narrow-to-region (start: IntOrMarker end: IntOrMarker) Nil)
 (@def internal--labeled-narrow-to-region
       (start: IntOrMarker end: IntOrMarker label: Any)
       Nil)
 (@def internal--labeled-widen (label: Any) Nil)
 (@check save-restriction ($body)))

;;; ============================================================
;;; Messages and formatting

(et-declare
 (@def ngettext
       ([(<= S String) (<= P String)] msgid: S msgid-plural: P n: Integer)
       S|P)
 (@def message (format-string: String|Nil &rest args: &List) String|Nil)
 (@def message-box (format-string: String|Nil &rest args: &List) String|Nil)
 (@def message-or-box (format-string: String|Nil &rest args: &List) String|Nil)
 (@def current-message () String|Nil)
 (@def propertize (string: String &rest properties: &PlistOf<Symbol~Any>) String)
 (@def format (string: String &rest objects: &List) String)
 (@def format-message (string: String &rest objects: &List) String))

;;; ============================================================
;;; Character and region operations

(et-declare
 (@def char-equal (c1: Integer c2: Integer) Boolean)
 (@def transpose-regions
       (startr1: IntOrMarker endr1: IntOrMarker
                startr2: IntOrMarker endr2: IntOrMarker
                &optional leave-markers: Boolean)
       Nil))

;;; ============================================================
