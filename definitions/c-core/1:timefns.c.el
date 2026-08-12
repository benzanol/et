;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Time arithmetic

(et-declare
;; AUTHORING STUB: not yet classified.
(@def time-add (a: Todo b: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def time-subtract (a: Todo b: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def time-less-p (a: Todo b: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def time-equal-p (a: Todo b: Todo) Todo))

;;; ============================================================
;;; Time conversion and formatting

(et-declare
;; AUTHORING STUB: not yet classified.
(@def float-time (&optional specified-time: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def format-time-string (format-string: Todo &optional time: Todo zone: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def decode-time (&optional time: Todo zone: Todo form: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def encode-time (time: Todo &rest obsolescent-arguments: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def time-convert (time: Todo &optional form: Todo) Todo))

;;; ============================================================
;;; Current time

(et-declare
;; AUTHORING STUB: not yet classified.
(@def current-time () Todo)
;; AUTHORING STUB: not yet classified.
(@def current-cpu-time () Todo)
;; AUTHORING STUB: not yet classified.
(@def current-time-string (&optional specified-time: Todo zone: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def current-time-zone (&optional specified-time: Todo zone: Todo) Todo))

;;; ============================================================
;;; Time zone rules

(et-declare
;; AUTHORING STUB: not yet classified.
(@def set-time-zone-rule (tz: Todo) Todo))

;;; ============================================================
