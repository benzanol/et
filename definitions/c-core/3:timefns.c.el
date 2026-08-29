;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Time arithmetic

(et-declare
 (@def time-add (a: Timestamp b: Timestamp) TimeOutput)
 (@def time-subtract (a: Timestamp b: Timestamp) TimeOutput)
 (@def time-less-p (a: Timestamp b: Timestamp) Boolean)
 (@def time-equal-p (a: Timestamp b: Timestamp) Boolean))


;;; ============================================================
;;; Time conversion and formatting

(et-declare
 (@def float-time
       (&optional specified-time: Timestamp)
       Number)
 (@def format-time-string
       (format-string: String
                       &optional time: Timestamp
                       zone: Timezone)
       String)
 (@def decode-time
       (&optional time: Timestamp zone: Timezone form: (or Nil @integer True))
       (Tuple (or Integer (Cons Integer Integer))
              Integer Integer Integer Integer Integer Integer
              (or Integer Nil True)
              (or Integer Nil)))
 ;; TIME may be a single decoded-time list, or, as an obsolescent calling
 ;; convention, 6 or more separate positional arguments where any argument
 ;; beyond the 6th is treated as ZONE. The type language cannot express this
 ;; arity-dependent parameter structure.
 (@def encode-time
       (time: Todo &rest obsolescent-arguments: &List<Todo>)
       (or Integer (Tuple Integer Integer) (Cons Integer Integer)))
 (@def time-convert (time: Timestamp &optional form: (or Nil True @integer @list Integer)) TimeOutput))


;;; ============================================================
;;; Current time

(et-declare
 (@def current-time () (or (Cons Integer Integer) (Tuple Integer Integer Integer Integer)))
 (@def current-cpu-time () (Cons Integer Integer))
 (@def current-time-string (&optional specified-time: Timestamp zone: Timezone) String)
 (@def current-time-zone (&optional specified-time: Timestamp zone: Timezone)
       (Tuple (or Integer Nil) String)))


;;; ============================================================
;;; Time zone rules

(et-declare
 (@def set-time-zone-rule (tz: Timezone) Nil))


;;; ============================================================
