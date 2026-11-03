;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Redisplay history

(et-declare
 (@def dump-redisplay-history () Nil))

;;; ============================================================
;;; Frame redraw

(et-declare
 (@def redraw-frame (&optional frame: Frame?) Nil)
 (@def redraw-display () Nil))

;;; ============================================================
;;; Terminal output

(et-declare
 (@def open-termscript (file: String?) Nil)
 (@def send-string-to-terminal (string: String &optional terminal: Terminal|Frame?) Nil)
 (@def ding (&optional arg: Bool) Nil))

;;; ============================================================
;;; Sleeping

(et-declare
 (@def sleep-for (seconds: Number &optional milliseconds: Integer?) Nil))

;;; ============================================================
;;; Redisplay

(et-declare
 (@def redisplay (&optional force: Any) Boolean))

;;; ============================================================
;;; Frame and buffer state

(et-declare
 (@def frame-or-buffer-changed-p (&optional variable: Var?) Boolean))

;;; ============================================================
;;; Cursor visibility

(et-declare
 (@def internal-show-cursor (window: Window? show: Bool) Nil)
 (@def internal-show-cursor-p (&optional window: Window?) Boolean))

;;; ============================================================
