;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Recursive editing and errors

(et-declare
 (@def recursive-edit () Nil)
 (@def command-error-default-function (data: &Cons<Symbol~List> context: String signal: Symbol?) Nil)
 (@def top-level () Never)
 (@def exit-recursive-edit () Never)
 (@def abort-recursive-edit () Never))

;;; ============================================================
;;; Idle time

(et-declare
 (@def current-idle-time () (or Nil (Cons Integer Integer) (Tuple Integer Integer Integer Integer))))

;;; ============================================================
;;; Event processing

(et-declare
 (@def internal-event-symbol-parse-modifiers (symbol: Symbol) Cons<Integer|Symbol~List<Symbol>>)
 (@def event-convert-list (event-desc: &List<Symbol|Integer>) Integer|Symbol)
 (@def internal-handle-focus-in (event: &Cons<@focus-in~&Cons<Frame~Any>>) Nil))

;;; ============================================================
;;; Keyboard input

(et-declare
 (@def read-char (&optional prompt: String? inherit-input-method: Bool seconds: Number?) Integer?)
 (@def read-event (&optional prompt: String? inherit-input-method: Bool seconds: Number?) Any)
 (@def read-char-exclusive (&optional prompt: String? inherit-input-method: Bool seconds: Number?) Integer?)
 (@def read-key-sequence
       (prompt: String? &optional continue-echo: Bool dont-downcase-last: Bool
                can-return-switch-frame: Bool cmd-loop: Bool disable-text-conversion: Bool)
       String|Vector<Any>)
 (@def read-key-sequence-vector
       (prompt: String? &optional continue-echo: Bool dont-downcase-last: Bool
                can-return-switch-frame: Bool cmd-loop: Bool disable-text-conversion: Bool)
       Vector<Any>)
 (@def input-pending-p (&optional check-timers: Bool) Boolean)
 (@def insert-special-event (event: &Cons<Symbol~Any>) Nil)
 (@def lossage-size (&optional arg: Integer?) Integer)
 (@def recent-keys (&optional include-cmds: Bool) Vector<Any>)
 (@def this-command-keys () String|Vector<Any>)
 (@def this-command-keys-vector () Vector<Any>)
 (@def this-single-command-keys () Vector<Any>)
 (@def this-single-command-raw-keys () Vector<Any>)
 (@def clear-this-command-keys (&optional keep-record: Bool) Nil)
 (@def recursion-depth () Integer)
 (@def open-dribble-file (file: String?) Nil)
 (@def discard-input () Nil)
 (@def suspend-emacs (&optional stuffstring: String?) Nil))

;;; ============================================================
;;; Terminal input modes

(et-declare
 (@def set-input-interrupt-mode (interrupt: Bool) Nil)
 (@def set-output-flow-control (flow: Bool &optional terminal: Terminal|Frame?) Nil)
 (@def set-input-meta-mode (meta: Any &optional terminal: Terminal|Frame?) Nil)
 (@def set-quit-char (quit: Integer) Nil)
 (@def set-input-mode (interrupt: Bool flow: Bool meta: Any &optional quit: Integer?) Nil)
 (@def current-input-mode () (Tuple Boolean Boolean Integer|@encoded|Boolean Integer)))

;;; ============================================================
;;; Position queries

(et-declare
 ;; The result is a mouse-click-style position list whose tail (OBJECT POS
 ;; (COL . ROW) IMAGE (DX . DY) (WIDTH . HEIGHT)) has a length and shape
 ;; that depends on which part of the window or frame was clicked (text
 ;; area, mode line, margin, fringe, tab bar, and so on). The type language
 ;; cannot yet express a value-dependent result structure.
 (@def posn-at-x-y (x: Integer y: Integer &optional frame-or-window: Frame|Window? whole: Bool) Todo)
 ;; See `posn-at-x-y': the result has the same value-dependent structure,
 ;; or is nil if POS is not visible in WINDOW.
 (@def posn-at-point (&optional pos: IntOrMarker? window: Window?) Todo))

;;; ============================================================
