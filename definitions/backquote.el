;;;; Constant detection

(defun et--backquote-constant-p (form)
  "Return non-nil if FORM has no active `,' or `,@' anywhere below it,
i.e. it can be type-checked as a plain literal.

A nested backquote form (FORM headed by \\=`) resets the unquote depth,
so everything inside one is inert at THIS level and counts as constant
regardless of what it contains -- see the Limitation in the commentary
above."
  (cond
   ((and (consp form) (memq (car form) '(\, \,@))) nil)
   ((and (consp form) (eq (car form) '\`)) t)
   ((consp form) (and (et--backquote-constant-p (car form))
                      (et--backquote-constant-p (cdr form))))
   ((vectorp form) (cl-every #'et--backquote-constant-p form))
   (t t)))


;;;; Checking unquoted expressions

(defun et--backquote-check-expr (expr path)
  "Type check EXPR, an unquoted form addressed at buffer path PATH.

Like `et-checker-sub', but takes the expression directly: the buffer
paths used here (through quotes and dotted tails) do not follow plain
tree indexing, so the expression cannot be recovered from the path."
  (let ((checked (et-at path (et--check expr et--checker-narrows nil))))
    (setq et--checker-narrows (et--check-result-narrows checked))
    (et--check-result-type checked)))


;;;; Main dispatch

(defun et--backquote-check (form path)
  "Type-check backquote sub-template FORM, addressed at PATH.

PATH also serves as FORM's own indexing prefix if FORM is itself a
bracketed (cons/vector) structure to recurse into."
  (cond
   ;; `,EXPR'/`,@EXPR' found as a whole position (e.g. the dotted tail of
   ;; `(a . ,b)) just checks EXPR -- there is nothing for a dotted `,@'
   ;; to splice onto, so it degenerates to a plain unquote too.
   ((and (consp form) (memq (car form) '(\, \,@)))
    (et--backquote-check-expr (cadr form) (append path '(1))))
   ;; No active unquote anywhere below: the whole subtree is just data.
   ((et--backquote-constant-p form) (et-literal form))
   ((vectorp form) (et--backquote-check-vector form path))
   (t (et--backquote-check-list form path 0))))


;;;; Cons cells

(defun et--backquote-check-list (form prefix index)
  "Walk one active cons cell of a backquote sub-template.

FORM is the tail beginning at element INDEX of the bracket addressed by
PREFIX; per the convention above, (PREFIX . (INDEX)) doubles as that
element's own indexing prefix.

Recognizes a `,@SPLICE' in the car as a list splice; otherwise builds a
fresh cons cell out of the (recursively checked) car and cdr."
  (let* ((here (append prefix (list index)))
         (qcar (car form))
         (qcdr (cdr form)))
    (if (and (consp qcar) (eq (car qcar) '\,@))
        (let* ((elem-path (append here '(1)))
               (elems-type (et--backquote-check-expr (cadr qcar) elem-path))
               (rest-type (et--backquote-check-tail qcdr prefix (1+ index))))
          (et--backquote-splice elems-type rest-type elem-path))
      (et-dt 'ConsFresh
             (et--backquote-check qcar here)
             (et--backquote-check-tail qcdr prefix (1+ index))))))

(defun et--backquote-check-tail (form prefix index)
  "Type-check FORM as the flat continuation of the bracket addressed by
PREFIX, starting at element INDEX -- i.e. FORM occupies that very
position, rather than a freshly-entered nested bracket.

A plain active cons here is simply more of the same spine, so it keeps
walking via `et--backquote-check-list'. Anything else (an escape, inert
data, a nested backquote, a vector, or the end of the list) has no flat
continuation of its own, so it is handled uniformly by
`et--backquote-check' at that position."
  (if (and (consp form) (not (memq (car form) '(\, \,@)))
           (not (et--backquote-constant-p form)))
      (et--backquote-check-list form prefix index)
    (et--backquote-check form (append prefix (list index)))))


;;;; Splicing

;; `,@EXPR' contributes EXPR's elements, followed by whatever the rest of
;; the template checks to. That "followed by" is exactly what `append'
;; already types (see et--append-return-type and the `AppendFresh' alias
;; in the main definitions), but this file stays self-contained (like
;; pcase.el) rather than reaching into that checker's private alias, so
;; it declares its own equivalent two-generic version.
(et-defalias SpliceFresh [E R] (or R (ConsFresh E (SpliceFresh E R))))

(defun et--backquote-splice (elems-type rest-type path)
  "Return the type of splicing ELEMS-TYPE's elements onto REST-TYPE, for
a `,@' escape whose spliced expression is addressed at PATH.

ELEMS-TYPE must be list-shaped; on failure, reports an error at PATH and
returns nil."
  (if-let* ((elem-type (et-checker-infer elems-type [E] ListR<E> E)))
      (et-alias 'SpliceFresh elem-type rest-type)
    (et-err path "Expected a list to splice, found %s" elems-type)))


;;;; Vectors

(defun et--backquote-check-vector (form path)
  "Type-check an active vector sub-template FORM addressed at PATH.

Vectors are homogeneous (see `VectorFresh'), so every slot's
contribution is unioned into one element type."
  (et-dt 'VectorFresh
         (apply #'et--or
                (cl-loop for elem across form
                         for i upfrom 0
                         collect (et--backquote-check-vector-elem elem (append path (list i)))))))

(defun et--backquote-check-vector-elem (elem path)
  "Return the element type ELEM (one vector slot, addressed at PATH)
contributes to its enclosing vector.

A plain slot contributes its own type; a `,@SPLICE' slot contributes the
element type of SPLICE, since splicing a list into a vector still yields
one flat (homogeneous) vector rather than nested structure."
  (if (and (consp elem) (eq (car elem) '\,@))
      (let* ((elem-path (append path '(1)))
             (list-type (et--backquote-check-expr (cadr elem) elem-path)))
        (or (et-checker-infer list-type [E] ListR<E> E)
            (et-err elem-path "Expected a list to splice, found %s" list-type)))
    (et--backquote-check elem path)))


;;;; Checker

(et-define-pcase-checker \` `(,template)
  (et--backquote-check template '(1)))


;;;; Tests

(defun et--test-backquote-literal (expr expected)
  "Assert that EXPR's checked type is exactly `(et-literal EXPECTED)',
not merely a subtype of it -- used to confirm the constant-collapse
optimization actually fires instead of building unnecessary `ConsFresh'
structure."
  (et-result-boundary
   (let ((got (et--check-result-type (et-at 1 (et--check expr nil nil)))))
     (or (equal got (et-literal expected))
         (et-err 0 "Expected exact literal %s, found %s" (et-literal expected) got)))))

(et-test
 ;; --- fully constant templates collapse to one literal, no ConsFresh ---
 (et--test-backquote-literal '`(a b c) '(a b c))
 (et--test-backquote-literal '`() nil)
 (et--test-backquote-literal '`a 'a)

 ;; --- a single unquote produces ConsFresh around it, literal tail ---
 (et-assert-resolve ConsR<Symbol~ConsR<Integer~Tuple<Symbol>>>
   `(a ,(:type Integer) c))
 ;; ... and the literal head/tail are exact, not just "some symbol"
 (et-assert-no-resolve ConsR<1~Any> `(a ,(:type Integer) c))

 ;; --- the unquoted expression is checked normally (errors propagate) ---
 (et-assert-resolve-errors `(a ,(error "boom") c))
 (et-assert-resolve-errors `(a ,this-var-does-not-exist c))

 ;; --- dotted unquote: `(a . ,b) needs no trailing Nil ---
 (et-assert-resolve ConsR<Symbol~String> `(a . ,(:type String)))

 ;; --- the whole template can itself be a single unquote ---
 (et-assert-resolve Integer `,(:type Integer))

 ;; --- nested backquote is opaque: inert even with a free variable inside ---
 (et--test-backquote-literal '`(a `(b ,undefined-free-var))
                             '(a `(b ,undefined-free-var)))

 ;; --- ,@ splices a list's elements in, fresh-consed, tail preserved ---
 (et-assert-resolve ListR<Symbol|Integer>
   `(a ,@(:type ListR<Integer>) c))
 (et-assert-no-resolve ListR<Symbol>
   `(a ,@(:type ListR<Integer>) c))
 ;; splicing nothing onto nothing collapses appropriately
 (et-assert-resolve ListR<Integer> `(,@(:type ListR<Integer>)))

 ;; --- splicing a non-list is a type error ---
 (et-assert-resolve-errors `(a ,@(:type Integer) c))

 ;; --- vectors: homogeneous union of slot types ---
 (et-assert-resolve VectorR<Symbol|Integer> `[a ,(:type Integer) c])
 (et--test-backquote-literal '`[a b c] [a b c])

 ;; --- ,@ inside a vector contributes the spliced list's element type ---
 (et-assert-resolve VectorR<Symbol|Integer>
   `[a ,@(:type ListR<Integer>) c]))


;;; Extra checkers

(et-declare
 (@macro backquote-list* :expand t))
