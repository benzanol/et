;; -*- lexical-binding: t; -*-

(et-declare
 (@def lock-file (file: String) Nil)
 (@def unlock-file (file: String) Nil)
 (@def lock-buffer (&optional file: String?) Nil)
 (@def unlock-buffer () Nil)
 (@def file-locked-p (filename: String) String|Boolean))
