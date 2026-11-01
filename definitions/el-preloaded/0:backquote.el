(et-declare
 (@check [backquote \`] ($expand))
 (@check backquote-list*
         ($pcase
          `(,_last) ($at 1)
          `(,first . ,rest)
          ($exp `(cons ,first (cl-list* ,@rest))))))
