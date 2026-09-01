(et-declare
 (@check setf
         ($pcase
          `(,a ,b ,c ,d . ,rest)
          ($progn ($recurse `(,a ,b)) ($recurse `(,c ,d ,@rest)))
          `(,place ,_val)
          ($eval (funcall (cdr (et:flow-check-place place)) (et-chk ($at 2)))))))
