;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Time arithmetic

(et-declare
 (@def time-add
       (a: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer))
        b: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer)))
       (or Integer (Cons Integer Integer) (Tuple Integer Integer Integer Integer)))
 (@def time-subtract
       (a: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer))
        b: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer)))
       (or Integer (Cons Integer Integer) (Tuple Integer Integer Integer Integer)))
 (@def time-less-p
       (a: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer))
        b: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer)))
       Boolean)
 (@def time-equal-p
       (a: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer))
        b: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
               (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
               (&Tuple Integer Integer Integer Integer)))
       Boolean))

;;; ============================================================
;;; Time conversion and formatting

(et-declare
 (@def float-time
       (&optional specified-time: (or Nil Number (&Cons Integer Integer)
                                      (&Cons Integer (&Cons Integer Integer))
                                      (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                                      (&Tuple Integer Integer Integer Integer)))
       Number)
 (@def format-time-string
       (format-string: String
        &optional time: (or Nil Number (&Cons Integer Integer)
                            (&Cons Integer (&Cons Integer Integer))
                            (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                            (&Tuple Integer Integer Integer Integer))
        zone: (or Nil True @wall String Integer (&Tuple Integer String)))
       String)
 (@def decode-time
       (&optional time: (or Nil Number (&Cons Integer Integer)
                            (&Cons Integer (&Cons Integer Integer))
                            (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                            (&Tuple Integer Integer Integer Integer))
        zone: (or Nil True @wall String Integer (&Tuple Integer String))
        form: (or Nil @integer True))
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
 (@def time-convert
       (time: (or Nil Number (&Cons Integer Integer) (&Cons Integer (&Cons Integer Integer))
                  (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                  (&Tuple Integer Integer Integer Integer))
        &optional form: (or Nil True @integer @list Integer))
       (or Integer (Cons Integer Integer) (Tuple Integer Integer Integer Integer))))

;;; ============================================================
;;; Current time

(et-declare
 (@def current-time () (or (Cons Integer Integer) (Tuple Integer Integer Integer Integer)))
 (@def current-cpu-time () (Cons Integer Integer))
 (@def current-time-string
       (&optional specified-time: (or Nil Number (&Cons Integer Integer)
                                      (&Cons Integer (&Cons Integer Integer))
                                      (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                                      (&Tuple Integer Integer Integer Integer))
        zone: (or Nil True @wall String Integer (&Tuple Integer String)))
       String)
 (@def current-time-zone
       (&optional specified-time: (or Nil Number (&Cons Integer Integer)
                                      (&Cons Integer (&Cons Integer Integer))
                                      (&Tuple Integer Integer) (&Tuple Integer Integer Integer)
                                      (&Tuple Integer Integer Integer Integer))
        zone: (or Nil True @wall String Integer (&Tuple Integer String)))
       (Tuple (or Integer Nil) String)))

;;; ============================================================
;;; Time zone rules

(et-declare
 (@def set-time-zone-rule (tz: (or Nil True @wall String Integer (&Tuple Integer String))) Nil))

;;; ============================================================
