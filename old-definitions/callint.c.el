;;; Type definitions for builtins defined in Emacs' src/callint.c.

(defun et--interactive-code-type (code)
  (pcase code
    ((or ?a ?C ?S ?v ?z) (list (et Symbol)))
    ((or ?b ?B ?D ?f ?F ?G ?M ?s) (list (et String)))
    ((or ?c ?d ?m ?p) (list (et Integer)))
    ((or ?n ?N) (list (et Number)))
    (?P (list (et Any)))
    (?r (list (et Integer) (et Integer)))
    (?Z (list (et Symbol|Nil)))
    ((or ?e ?k ?K ?U ?x ?X) (list (et Any)))
    (?i (list (et Nil)))
    (_ (error "Unknown interactive code: %c" code))))

(defun et--interactive-line-types (line)
  (cl-loop for char across line
           unless (memq char '(?* ?@ ?^))
           return (et--interactive-code-type char)))

(defun et--interactive-string-args-type (descriptor)
  (et--tuple
   'ConsR
   (cl-loop for line in (split-string descriptor "\n")
            append (et--interactive-line-types line))))

(defun et--interactive-checker ()
  (let* ((current-defun (or et-checking-defun
                            (error "`interactive' checked outside a defun")))
         (func-type (or (et-function-type current-defun)
                        (error "No function type for `%s'" current-defun)))
         (args-type
          (pcase (cdr et--checker-expr)
            ('() (et Nil))
            (`(,(and descriptor (pred stringp)) . ,_modes)
             (et--interactive-string-args-type descriptor))
            (`(,_descriptor . ,_modes)
             (et-checker-sub 1))
            (_ (error "Expected an optional arg descriptor"))))
         (match (et-funcall func-type args-type)))
    (unless (et-match-result-success match)
      (et-err 0 "Invalid arg descriptor"))
    (et Nil)))

(et-declare
 (@function interactive (&optional arg-descriptor &rest modes)
            (arg-descriptor Any)
            (modes ListR<Symbol>)
            (@return Nil)
            (@checker et--interactive-checker)))
