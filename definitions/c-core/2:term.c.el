;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; TTY capabilities

(et-declare
 (@def tty-display-color-p (&optional terminal: Terminal|Frame?) Boolean)
 (@def tty-display-color-cells (&optional terminal: Terminal|Frame?) Integer)
 (@def tty-type (&optional terminal: Terminal|Frame?) String?)
 (@def controlling-tty-p (&optional terminal: Terminal|Frame?) Boolean)
 (@def tty-no-underline (&optional terminal: Terminal|Frame?) Nil)
 (@def tty-top-frame (&optional terminal: Terminal|Frame?) Frame?))


;;; ============================================================
;;; TTY suspension

(et-declare
 (@def suspend-tty (&optional tty: Terminal|Frame?) Nil)
 (@def resume-tty (&optional tty: Terminal|Frame?) Nil))


;;; ============================================================
;;; Mouse and frame lookup

(et-declare
 (@def tty-frame-at (x: Integer y: Integer) (or Nil (Tuple Frame Integer Integer)))
 (@def gpm-mouse-start () Nil)
 (@def gpm-mouse-stop () Nil))


;;; ============================================================
;;; Frame geometry

(et-declare
 (@def tty-frame-geometry (&optional frame: Frame?) Alist<Symbol~Any>?)
 (@def tty-frame-edges (&optional frame: Frame? type: Any) (or Nil (Tuple Integer Integer Integer Integer)))
 (@def tty-frame-list-z-order (&optional frame: Frame?) List<Frame>)
 (@def tty-frame-restack (frame1: Frame frame2: Frame &optional above: Bool) Never)
 (@def tty-display-pixel-width (&optional display: Frame?) Integer)
 (@def tty-display-pixel-height (&optional display: Frame?) Integer))


;;; ============================================================
