;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Coding system predicates and input

(et-declare
 (@def coding-system-p (object: Any) Boolean)
 (@def read-non-nil-coding-system (prompt: String) Symbol)
 (@def read-coding-system
       (prompt: String &optional default-coding-system: Symbol|String?)
       Symbol?)
 (@def check-coding-system ([(<= T Symbol?)] coding-system: T) T))

;;; ============================================================
;;; Coding system detection

(et-declare
 (@def detect-coding-region
       (start: IntOrMarker end: IntOrMarker &optional highest: Bool)
       Symbol|ListFresh<Symbol>?)
 (@def detect-coding-string (string: String &optional highest: Bool)
       Symbol|ListFresh<Symbol>?)
 (@def find-coding-systems-region-internal
       (start: String|IntOrMarker end: IntOrMarker &optional exclude: List<Symbol>?)
       True|ListFresh<Symbol>)
 (@def unencodable-char-position
       (start: IntOrMarker end: IntOrMarker coding-system: Symbol?
               &optional count: Integer? string: String?)
       Integer|ListFresh<Integer>?)
 (@def check-coding-systems-region
       (start: String|IntOrMarker end: IntOrMarker coding-system-list: List<Symbol>)
       ListFresh<ConsFresh<Symbol~ListFresh<Integer>>>))

;;; ============================================================
;;; Coding conversion

(et-declare
 (@def decode-coding-region
       (start: IntOrMarker end: IntOrMarker coding-system: Symbol?
               &optional destination: Buffer|True?)
       Integer|String)
 (@def encode-coding-region
       (start: IntOrMarker end: IntOrMarker coding-system: Symbol?
               &optional destination: Buffer|True?)
       Integer|String)
 (@def decode-coding-string
       (string: String coding-system: Symbol? &optional nocopy: Bool buffer: Buffer|True?)
       Integer|String)
 (@def encode-coding-string
       (string: String coding-system: Symbol? &optional nocopy: Bool buffer: Buffer|True?)
       Integer|String))

;;; ============================================================
;;; Character coding conversion

(et-declare
 (@def decode-sjis-char (code: Integer) Integer)
 (@def encode-sjis-char (ch: Integer) Integer)
 (@def decode-big5-char (code: Integer) Integer)
 (@def encode-big5-char (ch: Integer) Integer))

;;; ============================================================
;;; Terminal coding systems

(et-declare
 (@def set-terminal-coding-system-internal
       (coding-system: Symbol &optional terminal: Terminal|Frame?)
       Nil)
 (@def set-safe-terminal-coding-system-internal (coding-system: Symbol) Nil)
 (@def terminal-coding-system (&optional terminal: Terminal|Frame?) Symbol?)
 (@def set-keyboard-coding-system-internal
       (coding-system: Symbol &optional terminal: Terminal|Frame?)
       Nil)
 (@def keyboard-coding-system (&optional terminal: Terminal|Frame?) Symbol))

;;; ============================================================
;;; Operation coding systems

(et-declare
 ;; The result is a cons from an internal alist, a fresh cons built here, or
 ;; whatever an arbitrary looked-up function returns, so only the general
 ;; cons shape is accurate.
 (@def find-operation-coding-system (operation: Symbol &rest args: &List) Cons<Any~Any>?))

;;; ============================================================
;;; Coding system priorities

(et-declare
 (@def set-coding-system-priority (&rest coding-systems: &List<Symbol>) Nil)
 (@def coding-system-priority-list (&optional highestp: Bool) Symbol|ListFresh<Symbol>?))

;;; ============================================================
;;; Coding system definitions

(et-declare
 (@def define-coding-system-internal (&rest args: &List) Nil)
 (@def coding-system-put (coding-system: Symbol? prop: Symbol val: Any) Any)
 (@def define-coding-system-alias (alias: Symbol coding-system: Symbol?) Nil)
 (@def coding-system-base (coding-system: Symbol?) Symbol)
 (@def coding-system-plist (coding-system: Symbol?) &PlistOf<Symbol~Any>)
 (@def coding-system-aliases (coding-system: Symbol?) &List<Symbol>)
 (@def coding-system-eol-type (coding-system: Symbol?) Nil|Integer|VectorFresh<Symbol>))

;;; ============================================================
