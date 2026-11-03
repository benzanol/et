;; -*- lexical-binding: t; -*-


;;; Syntax

(defconst et:std--loop-accumulation-keywords
  '(collect collecting append appending nconc nconcing
    concat concating vconcat vconcating count counting sum summing
    maximize maximizing minimize minimizing))

(cl-defstruct (et:std--loop-parser
               (:constructor et:std--loop-parser-create))
  args
  (position 0))

(cl-defstruct (et:std--loop-clause
               (:constructor et:std--loop-clause-create))
  kind
  path
  data)

(defun et:std--loop-parser-end-p (parser)
  (>= (et:std--loop-parser-position parser)
      (length (et:std--loop-parser-args parser))))

(defun et:std--loop-parser-peek (parser &optional offset)
  (nth (+ (et:std--loop-parser-position parser) (or offset 0))
       (et:std--loop-parser-args parser)))

(defun et:std--loop-parser-path (parser &optional offset)
  (list (1+ (+ (et:std--loop-parser-position parser) (or offset 0)))))

(defun et:std--loop-parser-pop (parser)
  (if (et:std--loop-parser-end-p parser)
      (et-fatal nil "Incomplete `cl-loop' clause")
    (prog1 (et:std--loop-parser-peek parser)
      (cl-incf (et:std--loop-parser-position parser)))))

(defun et:std--loop-parser-expression (parser)
  (let ((path (et:std--loop-parser-path parser)))
    (et:std--loop-parser-pop parser)
    path))

(defun et:std--loop-parser-eat (parser &rest words)
  (when (memq (et:std--loop-parser-peek parser) words)
    (et:std--loop-parser-pop parser)
    t))

(defun et:std--loop-parser-type (parser)
  (when (et:std--loop-parser-eat parser 'of-type)
    (et:std--loop-parser-pop parser)))

(defun et:std--loop-parser-body (parser)
  (let (paths)
    (while (consp (et:std--loop-parser-peek parser))
      (push (et:std--loop-parser-expression parser) paths))
    (unless paths
      (et-fatal (et:std--loop-parser-path parser)
                "Expected a non-atomic loop body form"))
    (nreverse paths)))

(defun et:std--loop-parser-using (parser)
  (when (et:std--loop-parser-eat parser 'using)
    (let ((path (et:std--loop-parser-path parser))
          (spec (et:std--loop-parser-pop parser)))
      (unless (and (listp spec) (= (length spec) 2)
                   (symbolp (car spec)))
        (et-fatal path "Invalid `using' clause: %s" spec))
      spec)))

(defun et:std--loop-parser-for-being (parser)
  (et:std--loop-parser-eat parser 'the 'each)
  (let ((kind (et:std--loop-parser-pop parser)))
    (cond
     ((memq kind '(element elements))
      (unless (et:std--loop-parser-eat parser 'in 'of 'in-ref 'of-ref)
        (et-fatal (et:std--loop-parser-path parser) "Expected `of'"))
      (list :mode kind :source (et:std--loop-parser-expression parser)
            :using (et:std--loop-parser-using parser)))
     ((memq kind '(hash-key hash-keys hash-value hash-values
                            key-code key-codes key-seq key-seqs
                            key-binding key-bindings))
      (unless (et:std--loop-parser-eat parser 'in 'of)
        (et-fatal (et:std--loop-parser-path parser) "Expected `of'"))
      (list :mode kind :source (et:std--loop-parser-expression parser)
            :using (et:std--loop-parser-using parser)))
     ((memq kind '(symbol symbols present-symbol present-symbols
                          external-symbol external-symbols))
      (list :mode kind
            :source (when (et:std--loop-parser-eat parser 'in 'of)
                      (et:std--loop-parser-expression parser))))
     ((memq kind '(overlay overlays extent extents interval intervals))
      (let (source property from to)
        (while (memq (et:std--loop-parser-peek parser)
                     '(in of property from to))
          (pcase (et:std--loop-parser-pop parser)
            ((or 'in 'of) (setq source (et:std--loop-parser-expression parser)))
            ('property (setq property (et:std--loop-parser-expression parser)))
            ('from (setq from (et:std--loop-parser-expression parser)))
            ('to (setq to (et:std--loop-parser-expression parser)))))
        (list :mode kind :source source :property property :from from :to to)))
     ((memq kind '(frame frames screen screens buffer buffers))
      (list :mode kind))
     ((memq kind '(window windows))
      (list :mode kind
            :source (when (et:std--loop-parser-eat parser 'in 'of)
                      (et:std--loop-parser-expression parser))))
     (t
      (et-fatal (et:std--loop-parser-path parser -1)
                "Unsupported `cl-loop' iteration kind: %s" kind)))))

(defun et:std--loop-parser-for-spec (parser variable declared-type)
  (let ((word (et:std--loop-parser-pop parser)))
    (cond
     ((memq word '(from upfrom downfrom to upto downto above below by))
      (let (start end step)
        (when (memq word '(downto above))
          (et-fatal (et:std--loop-parser-path parser -1)
                    "A downward loop requires an explicit `from' value"))
        (if (eq word 'by)
            (setq step (et:std--loop-parser-expression parser))
          (when (memq word '(from upfrom downfrom))
            (setq start (et:std--loop-parser-expression parser)
                  word (et:std--loop-parser-peek parser)))
          (when (memq word '(to upto downto above below))
            ;; After a FROM clause WORD was only peeked; with an implicit
            ;; start it was already consumed by the caller.
            (when (eq word (et:std--loop-parser-peek parser))
              (et:std--loop-parser-pop parser))
            (setq end (et:std--loop-parser-expression parser)))
          (when (et:std--loop-parser-eat parser 'by)
            (setq step (et:std--loop-parser-expression parser))))
        (list :variable variable :declared-type declared-type :mode 'arithmetic
              :start start :end end :step step)))
     ((memq word '(in in-ref on))
      (list :variable variable :declared-type declared-type :mode word
            :source (et:std--loop-parser-expression parser)
            :step (when (et:std--loop-parser-eat parser 'by)
                    (et:std--loop-parser-expression parser))))
     ((eq word '=)
      (list :variable variable :declared-type declared-type :mode 'equals
            :start (et:std--loop-parser-expression parser)
            :then (when (et:std--loop-parser-eat parser 'then)
                    (et:std--loop-parser-expression parser))))
     ((memq word '(across across-ref))
      (list :variable variable :declared-type declared-type :mode word
            :source (et:std--loop-parser-expression parser)))
     ((eq word 'being)
      (append (list :variable variable :declared-type declared-type)
              (et:std--loop-parser-for-being parser)))
     (t
      (et-fatal (et:std--loop-parser-path parser -1)
                "Expected a `for' preposition, found %s" word)))))

(defun et:std--loop-parser-for (parser path)
  (let (specs)
    (while
        (let* ((variable (et:std--loop-parser-pop parser))
               (declared-type (et:std--loop-parser-type parser)))
          (push (et:std--loop-parser-for-spec parser variable declared-type)
                specs)
          (et:std--loop-parser-eat parser 'and)))
    (et:std--loop-clause-create :kind 'for :path path
                                :data (nreverse specs))))

(defun et:std--loop-parser-with (parser path)
  (let (bindings more)
    (setq more t)
    (while more
      (let ((variable (et:std--loop-parser-pop parser))
            (declared-type (et:std--loop-parser-type parser)))
        (push (list :variable variable :declared-type declared-type
                    :initial (when (et:std--loop-parser-eat parser '=)
                               (et:std--loop-parser-expression parser)))
              bindings))
      (setq more (et:std--loop-parser-eat parser 'and)))
    (et:std--loop-clause-create :kind 'with :path path
                                :data (nreverse bindings))))

(defun et:std--loop-parser-conditional-arm (parser)
  (let ((clauses (list (et:std--loop-parser-clause parser))))
    (while (et:std--loop-parser-eat parser 'and)
      (push (et:std--loop-parser-clause parser) clauses))
    (nreverse clauses)))

(defun et:std--loop-parser-conditional (parser path kind)
  (let* ((test (et:std--loop-parser-expression parser))
         (then (et:std--loop-parser-conditional-arm parser))
         (else (when (et:std--loop-parser-eat parser 'else)
                 (et:std--loop-parser-conditional-arm parser))))
    (et:std--loop-parser-eat parser 'end)
    (et:std--loop-clause-create
     :kind kind :path path :data (list :test test :then then :else else))))

(defun et:std--loop-parser-clause (parser)
  (let* ((path (et:std--loop-parser-path parser))
         (word (et:std--loop-parser-pop parser)))
    (cond
     ((memq word '(for as)) (et:std--loop-parser-for parser path))
     ((eq word 'with) (et:std--loop-parser-with parser path))
     ((memq word '(if when unless))
      (et:std--loop-parser-conditional parser path word))
     ((memq word et:std--loop-accumulation-keywords)
      (et:std--loop-clause-create
       :kind 'accumulate :path path
       :data (list :operation word
                   :expression (et:std--loop-parser-expression parser)
                   :into (when (et:std--loop-parser-eat parser 'into)
                           (et:std--loop-parser-pop parser))
                   :declared-type (et:std--loop-parser-type parser))))
     ((memq word '(do doing initially))
      (when (eq word 'initially)
        (et:std--loop-parser-eat parser 'do 'doing))
      (et:std--loop-clause-create
       :kind (if (eq word 'initially) 'initially 'do) :path path
       :data (et:std--loop-parser-body parser)))
     ((eq word 'finally)
      (if (et:std--loop-parser-eat parser 'return)
          (et:std--loop-clause-create
           :kind 'finally-return :path path
           :data (et:std--loop-parser-expression parser))
        (et:std--loop-parser-eat parser 'do 'doing)
        (let ((body (et:std--loop-parser-body parser)))
          (if (eq (car-safe
                   (nth (1- (caar body))
                        (et:std--loop-parser-args parser)))
                  'return)
              (et:std--loop-clause-create
               :kind 'finally-return :path path
               :data (append (car body) '(1)))
            (et:std--loop-clause-create :kind 'finally :path path :data body)))))
     ((memq word '(repeat while until always never thereis return))
      (et:std--loop-clause-create :kind word :path path
                                  :data (et:std--loop-parser-expression parser)))
     ((eq word 'named)
      (et:std--loop-clause-create :kind 'named :path path
                                  :data (et:std--loop-parser-pop parser)))
     (t
      (et-fatal path "Expected a `cl-loop' keyword, found %s" word)))))

(defun et:std--loop-parse (expression)
  (let ((arguments (cdr expression)))
    ;; `cl-loop' retains the old `(loop FORM...)' spelling when there is no
    ;; non-boolean symbol at the top level.
    (if (not (memq t (mapcar #'symbolp
                             (delq nil (delq t (copy-sequence arguments))))))
        (when arguments
          (list
           (et:std--loop-clause-create
            :kind 'do :path '(0)
            :data (cl-loop for index from 1 to (length arguments)
                           collect (list index)))))
      (let ((parser (et:std--loop-parser-create :args arguments))
            clauses)
        (while (not (et:std--loop-parser-end-p parser))
          (push (et:std--loop-parser-clause parser) clauses)
          ;; `and' can join ordinary body clauses as well as appearing inside
          ;; conditional, for, and with clauses.
          (et:std--loop-parser-eat parser 'and))
        (nreverse clauses)))))


;;; Checking state

(cl-defstruct (et:std--loop-accumulator
               (:constructor et:std--loop-accumulator-create))
  name
  category
  type
  variable
  nonfresh)

(cl-defstruct (et:std--loop-flow
               (:constructor et:std--loop-flow-create))
  live
  narrows)

(cl-defstruct (et:std--loop-state
               (:constructor et:std--loop-state-create))
  variables
  accumulators
  bare-accumulator
  return-types
  return-narrows
  normal-exits
  steps
  bare-selected
  result-mode)

(defun et:std--loop-category (operation)
  (cond
   ((memq operation '(collect collecting append appending nconc nconcing)) 'list)
   ((memq operation '(count counting sum summing maximize maximizing
                            minimize minimizing)) 'number)
   ((memq operation '(concat concating)) 'string)
   ((memq operation '(vconcat vconcating)) 'vector)))

(defun et:std--loop-initial-accumulator-type (operation)
  (pcase (et:std--loop-category operation)
    ('list (et Nil))
    ('number (if (memq operation '(maximize maximizing minimize minimizing))
                 (et Nil)
               (et Integer)))
    ('string (et String))
    ('vector (et-dt 'VectorFresh (et-never)))
    (_ (et-any))))

(defun et:std--loop-accumulator (state name operation)
  (let* ((category (et:std--loop-category operation))
         (accumulator
          (if name
              (alist-get name (et:std--loop-state-accumulators state))
            (et:std--loop-state-bare-accumulator state))))
    (unless accumulator
      (setq accumulator
            (et:std--loop-accumulator-create
             :name name :category category
             :type (et:std--loop-initial-accumulator-type operation)))
      (if name
          (let ((variable (et:type-var-new :name name :type (et-any))))
            (setf (et:std--loop-accumulator-variable accumulator) variable)
            (push variable (et:std--loop-state-variables state))
            (et-update-var-narrow
             variable (et:std--loop-accumulator-type accumulator))
            (push (cons name accumulator)
                  (et:std--loop-state-accumulators state)))
        (setf (et:std--loop-state-bare-accumulator state) accumulator)))
    accumulator))

(defun et:std--loop-discover-accumulators (clauses state)
  (dolist (clause clauses)
    (pcase (et:std--loop-clause-kind clause)
      ('accumulate
       (let ((data (et:std--loop-clause-data clause)))
         (et:std--loop-accumulator
          state (plist-get data :into) (plist-get data :operation))))
      ((or 'if 'when 'unless)
       (let ((data (et:std--loop-clause-data clause)))
         (et:std--loop-discover-accumulators (plist-get data :then) state)
         (et:std--loop-discover-accumulators (plist-get data :else) state))))))

(defun et:std--loop-check-at (state path &optional recommendation)
  (et-with-vars (et:std--loop-state-variables state)
    (et-check-at recommendation path)))

(defun et:std--loop-expect (type expected path)
  (unless (et-subtype? (et:algebra-remove-binds type) expected)
    (et-err path "Expected %s, found %s" expected type))
  type)

(defun et:std--loop-element-type (type matcher)
  (if (et-subtype? (et:algebra-remove-binds type) (et Nil))
      (et-never)
    (or (pcase matcher
          ('list (et-infer type [E] &List<E> E))
          ('ord-sequence (et-infer type [E] &OrdSeq<E> E))
          ('elt-sequence (et-infer type [E] &EltSeq<E> E))
          ('hash-key (et-infer type [K V] &HashTable<K_V> K))
          ('hash-value (et-infer type [K V] &HashTable<K_V> V)))
        (et-any))))

(defun et:std--loop-cl-type (spec)
  "Translate the useful, non-dependent part of a CL loop type spec."
  (pcase spec
    ((or 't '* 'nil) (et-any))
    ((or 'fixnum 'integer 'unsigned-byte 'signed-byte) (et Integer))
    ((or 'float 'short-float 'single-float 'double-float 'long-float
         'real 'rational 'ratio 'number)
     (et Number))
    ('string (et String))
    ('symbol (et Symbol))
    ('list (et List))
    ('vector (et Vector))
    (`(integer . ,_) (et Integer))
    (`(or . ,types) (apply #'et-union (mapcar #'et:std--loop-cl-type types)))
    (`(and . ,types) (apply #'et-supersect (mapcar #'et:std--loop-cl-type types)))
    (_ (et-any))))

(defun et:std--loop-declared-type (type spec path)
  (if (null spec) type
    (let ((declared (et:std--loop-cl-type spec)))
      (unless (et-subtype? (et:algebra-remove-binds type) declared)
        (et-err path "Inferred loop variable type %s is not %s" type spec))
      (et-supersect type declared))))

(defun et:std--loop-pattern-bindings (pattern type)
  (condition-case nil
      (et:helpers--cl-destructure pattern type)
    (error
     (cl-loop for symbol in (delete-dups (flatten-tree pattern))
              when (and symbol (symbolp symbol)
                        (not (memq symbol '(&optional &rest))))
              collect (cons symbol (et-any))))))

(defun et:std--loop-bind-pattern (state pattern type)
  (dolist (binding (et:std--loop-pattern-bindings pattern type))
    (let ((variable (et:type-var-new
                     :name (car binding)
                     :type (et-any))))
      (push variable (et:std--loop-state-variables state))
      (et-update-var-narrow variable (cdr binding)))))

(defun et:std--loop-update-pattern (state pattern type)
  (dolist (binding (et:std--loop-pattern-bindings pattern type))
    (when-let ((variable
                (cl-find (car binding) (et:std--loop-state-variables state)
                         :key #'et:type-var->name)))
      (et-update-var-narrow
       variable
       (et-union (et:check-var-type variable) (cdr binding))))))

(defun et:std--loop-for-source-type (state spec)
  (let* ((mode (plist-get spec :mode))
         (source-path (plist-get spec :source))
         (source (when source-path
                   (et:std--loop-check-at state source-path))))
    (setf (plist-get spec :checked-source) source)
    (pcase mode
      ('arithmetic
       (let ((types
              (delq nil
                    (mapcar
                     (lambda (key)
                       (when-let ((path (plist-get spec key)))
                         (et:std--loop-check-at state path (et Number))))
                     '(:start :end :step)))))
         (dolist (type types)
           (et:std--loop-expect type (et Number)
                                (or (plist-get spec :start)
                                    (plist-get spec :end))))
         (if (cl-every (lambda (type) (et-subtype? type (et Integer))) types)
             (et Integer) (et Number))))
      ('equals
       (et:std--loop-check-at state (plist-get spec :start)))
      ((or 'in 'in-ref)
       (et:std--loop-expect source (et &List) source-path)
       (et:std--loop-element-type source 'list))
      ('on
       (et:std--loop-expect source (et &List) source-path)
       source)
      ((or 'across 'across-ref)
       (et:std--loop-expect source (et &OrdSeq) source-path)
       (et:std--loop-element-type source 'ord-sequence))
      ((or 'element 'elements)
       (et:std--loop-expect source (et &EltSeq) source-path)
       (et:std--loop-element-type source 'elt-sequence))
      ((or 'hash-key 'hash-keys)
       (et:std--loop-expect source (et &HashTable) source-path)
       (et:std--loop-element-type source 'hash-key))
      ((or 'hash-value 'hash-values)
       (et:std--loop-expect source (et &HashTable) source-path)
       (et:std--loop-element-type source 'hash-value))
      ((or 'symbol 'symbols 'present-symbol 'present-symbols
           'external-symbol 'external-symbols) (et Symbol))
      ((or 'frame 'frames 'screen 'screens) (et Frame))
      ((or 'buffer 'buffers) (et Buffer))
      ((or 'window 'windows) (et Window))
      ((or 'overlay 'overlays 'extent 'extents) (et Overlay))
      ((or 'interval 'intervals) (et Cons<Integer_Integer>))
      ((or 'key-code 'key-codes) (et Integer))
      ((or 'key-seq 'key-seqs) (et Vector<Integer>))
      ((or 'key-binding 'key-bindings) (et Any))
      (_ (et-any)))))

(defun et:std--loop-check-for-modifiers (state spec primary-type)
  (dolist (key '(:property :from :to))
    (when-let ((path (plist-get spec key)))
      (et:std--loop-check-at state path)))
  (when-let ((path (plist-get spec :step)))
    (push (list path (et AnyFn)) (et:std--loop-state-steps state)))
  (when-let ((using (plist-get spec :using)))
    (let* ((kind (car using))
           (name (cadr using))
           (type
            (pcase kind
              ('index (et Integer))
              ((or 'hash-key 'hash-keys)
               (let ((source (plist-get spec :checked-source)))
                 (et:std--loop-element-type source 'hash-key)))
              ((or 'hash-value 'hash-values)
               (let ((source (plist-get spec :checked-source)))
                 (et:std--loop-element-type source 'hash-value)))
              ((or 'key-code 'key-codes) (et Integer))
              ((or 'key-seq 'key-seqs) (et Vector<Integer>))
              (_ (et-any)))))
      (unless (equal name (plist-get spec :variable))
        (et:std--loop-bind-pattern state name type))))
  primary-type)

(defun et:std--loop-setup-for (state specs)
  ;; Every `and' group is parallel: calculate its initial values before
  ;; installing any of its bindings.
  (let ((types
         (mapcar
          (lambda (spec)
            (let* ((type (et:std--loop-for-source-type state spec))
                   (type (et:std--loop-declared-type
                          type (plist-get spec :declared-type)
                          (plist-get spec :source))))
              (cons spec type)))
          specs)))
    (dolist (entry types)
      (et:std--loop-bind-pattern
       state (plist-get (car entry) :variable) (cdr entry)))
    (dolist (entry types)
      (let ((spec (car entry)))
        (et:std--loop-check-for-modifiers state spec (cdr entry))
        (when-let ((then (plist-get spec :then)))
          (push (list then (cdr entry) (plist-get spec :variable))
                (et:std--loop-state-steps state)))))))

(defun et:std--loop-setup-with (state bindings)
  (let ((types
         (mapcar
          (lambda (binding)
            (let* ((path (plist-get binding :initial))
                   (type (if path (et:std--loop-check-at state path) (et Nil))))
              (cons binding
                    (et:std--loop-declared-type
                     type (plist-get binding :declared-type) path))))
          bindings)))
    (dolist (entry types)
      (et:std--loop-bind-pattern
       state (plist-get (car entry) :variable) (cdr entry)))))

(defun et:std--loop-check-paths (state paths)
  (let ((type (et Nil)))
    (dolist (path paths type)
      (setq type (et:std--loop-check-at state path)))))

(defun et:std--loop-flow-merge (flows)
  (let ((live (cl-remove-if-not #'et:std--loop-flow-live flows)))
    (et:std--loop-flow-create
     :live (not (null live))
     :narrows (when live
                (cl-reduce #'et:flow--narrows-or
                           (mapcar #'et:std--loop-flow-narrows live))))))

(defun et:std--loop-condition-flow (type base truthy)
  (let* ((part (et-supersect type (if truthy (et NonNil) (et Nil))))
         (narrows (et:check--narrows-and base (et-type-binds part))))
    (et:std--loop-flow-create :live (not (et-never-p part)) :narrows narrows)))

(defun et:std--loop-record-return (state type narrows)
  (push (et:algebra-remove-binds type) (et:std--loop-state-return-types state))
  (push narrows (et:std--loop-state-return-narrows state)))

(defun et:std--loop-check-accumulation (state data)
  (let* ((operation (plist-get data :operation))
         (path (plist-get data :expression))
         (category (et:std--loop-category operation))
         (recommendation
          (pcase category
            ('number (unless (memq operation '(count counting)) (et Number)))
            ('string (et &ConcatSeq<Integer>))
            ('vector (et &OrdSeq))
            (_ nil)))
         (form-type (et:std--loop-check-at state path recommendation))
         (accumulator
          (if-let ((name (plist-get data :into)))
              (alist-get name (et:std--loop-state-accumulators state))
            (et:std--loop-state-bare-accumulator state)))
         contribution)
    (unless (eq category (et:std--loop-accumulator-category accumulator))
      (et-err path "Incompatible accumulation operations share `%s'"
              (or (plist-get data :into) "the implicit result")))
    (when (and (null (plist-get data :into))
               (not (et:std--loop-state-bare-selected state)))
      (setf (et:std--loop-state-bare-selected state) t
            (et:std--loop-state-result-mode state) 'accumulator))
    (pcase category
      ('list
       (if (memq operation '(collect collecting))
           (setq contribution form-type)
         (et:std--loop-expect form-type (et &List) path)
         (setq contribution (et:std--loop-element-type form-type 'list))
         (when (memq operation '(nconc nconcing))
           (setf (et:std--loop-accumulator-nonfresh accumulator) t))))
      ('number
       (setq contribution
             (if (memq operation '(count counting)) (et Integer)
               (et:std--loop-expect form-type (et Number) path))))
      ('string
       (et:std--loop-expect form-type (et &ConcatSeq<Integer>) path)
       (setq contribution (et Integer)))
      ('vector
       (et:std--loop-expect form-type (et &OrdSeq) path)
       (setq contribution
             (et:std--loop-element-type form-type 'ord-sequence))))
    (let ((old (et:std--loop-accumulator-type accumulator)))
      (setf (et:std--loop-accumulator-type accumulator)
            (pcase category
              ('list
               (et-alias
                (if (et:std--loop-accumulator-nonfresh accumulator)
                    'List 'ListFresh)
                (et-union
                 (et:std--loop-element-type old 'list)
                 (et:algebra-remove-binds contribution))))
              ('number (et-union old (et:algebra-remove-binds contribution)))
              ('string (et String))
              ('vector
               (et-dt 'VectorFresh
                      (et-union
                       (et:std--loop-element-type old 'ord-sequence)
                       (et:algebra-remove-binds contribution)))))))
    (when-let ((variable (et:std--loop-accumulator-variable accumulator)))
      (et-update-var-narrow variable
                            (et:std--loop-accumulator-type accumulator)))))


;;; Flow interpreter

(defun et:std--loop-check-conditional (state clause flow)
  (let* ((data (et:std--loop-clause-data clause))
         (_ (et-set-narrows (et:std--loop-flow-narrows flow)))
         (test (et:std--loop-check-at state (plist-get data :test)))
         (base (et-cur-narrows))
         (positive (et:std--loop-condition-flow test base t))
         (negative (et:std--loop-condition-flow test base nil))
         (then-flow (if (eq (et:std--loop-clause-kind clause) 'unless)
                        negative positive))
         (else-flow (if (eq (et:std--loop-clause-kind clause) 'unless)
                        positive negative))
         (it-variable (et:type-var-new :name 'it :type (et-any)))
         (old-vars (et:std--loop-state-variables state))
         then-result else-result)
    (setf (et:std--loop-state-variables state) (cons it-variable old-vars))
    (unwind-protect
        (progn
          (when (et:std--loop-flow-live then-flow)
            (et-set-narrows (cons (cons it-variable
                                        (if (eq (et:std--loop-clause-kind clause) 'unless)
                                            (et-supersect test (et Nil))
                                          (et-supersect test (et NonNil))))
                                  (et:std--loop-flow-narrows then-flow))))
          (setq then-result
                (et:std--loop-process-sequence
                 state (plist-get data :then)
                 (et:std--loop-flow-create
                  :live (et:std--loop-flow-live then-flow)
                  :narrows (et-cur-narrows))))
          (if (plist-get data :else)
              (progn
                (when (et:std--loop-flow-live else-flow)
                  (et-set-narrows
                   (cons (cons it-variable
                               (if (eq (et:std--loop-clause-kind clause) 'unless)
                                   (et-supersect test (et NonNil))
                                 (et-supersect test (et Nil))))
                         (et:std--loop-flow-narrows else-flow))))
                (setq else-result
                      (et:std--loop-process-sequence
                       state (plist-get data :else)
                       (et:std--loop-flow-create
                        :live (et:std--loop-flow-live else-flow)
                        :narrows (et-cur-narrows)))))
            (setq else-result else-flow)))
      (setf (et:std--loop-state-variables state) old-vars))
    (dolist (result (list then-result else-result))
      (setf (et:std--loop-flow-narrows result)
            (cl-remove it-variable (et:std--loop-flow-narrows result)
                       :key #'car)))
    (et:std--loop-flow-merge (list then-result else-result))))

(defun et:std--loop-process-live-clause (state clause flow)
  (et-set-narrows (et:std--loop-flow-narrows flow))
  (let ((kind (et:std--loop-clause-kind clause))
        (data (et:std--loop-clause-data clause)))
    (pcase kind
      ('do
       (et:std--loop-check-paths state data)
       (et:std--loop-flow-create :live t :narrows (et-cur-narrows)))
      ('accumulate
       (et:std--loop-check-accumulation state data)
       (et:std--loop-flow-create :live t :narrows (et-cur-narrows)))
      ((or 'if 'when 'unless)
       (et:std--loop-check-conditional state clause flow))
      ((or 'while 'until)
       (let* ((type (et:std--loop-check-at state data))
              (base (et-cur-narrows))
              (positive (et:std--loop-condition-flow type base t))
              (negative (et:std--loop-condition-flow type base nil))
              (continue (if (eq kind 'until) negative positive))
              (exit (if (eq kind 'until) positive negative)))
         (when (et:std--loop-flow-live exit)
           (push (et:std--loop-flow-narrows exit)
                 (et:std--loop-state-normal-exits state)))
         continue))
      ((or 'always 'never)
       (setf (et:std--loop-state-result-mode state) 'boolean)
       (let* ((type (et:std--loop-check-at state data))
              (base (et-cur-narrows))
              (positive (et:std--loop-condition-flow type base t))
              (negative (et:std--loop-condition-flow type base nil))
              (continue (if (eq kind 'never) negative positive))
              (exit (if (eq kind 'never) positive negative)))
         (when (et:std--loop-flow-live exit)
           (et:std--loop-record-return
            state (et Nil) (et:std--loop-flow-narrows exit)))
         continue))
      ('thereis
       (let* ((type (et:std--loop-check-at state data))
              (base (et-cur-narrows))
              (positive (et:std--loop-condition-flow type base t))
              (negative (et:std--loop-condition-flow type base nil)))
         (when (et:std--loop-flow-live positive)
           (et:std--loop-record-return
            state (et-supersect type (et NonNil))
            (et:std--loop-flow-narrows positive)))
         negative))
      ('return
       (let ((type (et:std--loop-check-at
                    state data (et-cur-recommendation))))
         (et:std--loop-record-return state type (et-cur-narrows))
         (et:std--loop-flow-create :live nil :narrows (et-cur-narrows))))
      (_ flow))))

(defun et:std--loop-process-sequence (state clauses flow)
  (dolist (clause clauses flow)
    (if (et:std--loop-flow-live flow)
        (setq flow (et:std--loop-process-live-clause state clause flow))
      ;; Keep the source warning, but do not let dead code contribute return
      ;; or accumulator types, nor let its narrows reanimate the path.
      (let ((et:check--unreachable t))
        (pcase (et:std--loop-clause-kind clause)
          ('do (dolist (path (et:std--loop-clause-data clause))
                 (et:std--loop-check-at state path)))
          ('accumulate
           (et:std--loop-check-at
            state (plist-get (et:std--loop-clause-data clause) :expression)))
          ((or 'if 'when 'unless 'while 'until 'always 'never 'thereis 'return)
           (et:std--loop-check-at
            state (if (memq (et:std--loop-clause-kind clause) '(if when unless))
                      (plist-get (et:std--loop-clause-data clause) :test)
                    (et:std--loop-clause-data clause)))))))))

(defun et:std--loop-filter-local-narrows (narrows variables)
  (cl-remove-if (lambda (entry) (memq (car entry) variables)) narrows))

(defun et:std--loop-remove-local-variables (type variables)
  (et:algebra--transform-type
   type nil
   (lambda (_ case value)
     (et-copy-with
      case
      :value value
      :binds
      (cl-loop for (variable . bound) in (et:type-case->binds case)
               unless (memq variable variables)
               collect
               (cons variable
                     (et:std--loop-remove-local-variables bound variables)))
      :typeofs
      (cl-remove-if (lambda (variable) (memq variable variables))
                    (et:type-case->typeofs case))))))

(defun et:std--loop-normal-result (state)
  (pcase (et:std--loop-state-result-mode state)
    ('boolean (et True))
    ('accumulator
     (if-let ((accumulator (et:std--loop-state-bare-accumulator state)))
           (et:std--loop-accumulator-type accumulator)
       (et Nil)))
    (_ (et Nil))))

(defun et:std--loop-may-terminate-after-iteration-p (clauses)
  (cl-some
   (lambda (clause)
     (pcase (et:std--loop-clause-kind clause)
       ('repeat t)
       ('for
        (cl-some
         (lambda (spec)
           (let ((mode (plist-get spec :mode)))
             (or (not (memq mode '(equals arithmetic)))
                 (and (eq mode 'arithmetic) (plist-get spec :end)))))
         (et:std--loop-clause-data clause)))
       (_ nil)))
   clauses))

(defun et:std--loop-run (clauses entry-narrows)
  (let* ((state (et:std--loop-state-create))
         (setup (cl-remove-if-not
                 (lambda (clause)
                   (memq (et:std--loop-clause-kind clause)
                         '(for with repeat named)))
                 clauses))
         (initially (cl-remove-if-not
                     (lambda (clause)
                       (eq (et:std--loop-clause-kind clause) 'initially))
                     clauses))
         (finally (cl-remove-if-not
                   (lambda (clause)
                     (memq (et:std--loop-clause-kind clause)
                           '(finally finally-return)))
                   clauses))
         (body (cl-remove-if
                (lambda (clause)
                  (memq (et:std--loop-clause-kind clause)
                        '(for with repeat named initially finally finally-return)))
                clauses)))
    (et-set-narrows entry-narrows)
    (et:std--loop-discover-accumulators clauses state)
    (dolist (clause setup)
      (pcase (et:std--loop-clause-kind clause)
        ('for (et:std--loop-setup-for state (et:std--loop-clause-data clause)))
        ('with (et:std--loop-setup-with state (et:std--loop-clause-data clause)))
        ('repeat
         (et:std--loop-expect
          (et:std--loop-check-at state (et:std--loop-clause-data clause)
                                 (et Integer))
          (et Integer) (et:std--loop-clause-data clause)))))
    (dolist (clause initially)
      (et:std--loop-check-paths state (et:std--loop-clause-data clause)))
    (let* ((iteration-entry (et-cur-narrows))
           (iteration-terminates
            (et:std--loop-may-terminate-after-iteration-p setup))
           (flow (et:std--loop-process-sequence
                  state body
                  (et:std--loop-flow-create :live t
                                            :narrows iteration-entry))))
      (when (et:std--loop-flow-live flow)
        (et-set-narrows (et:std--loop-flow-narrows flow))
        (dolist (step (nreverse (et:std--loop-state-steps state)))
          (let ((step-type
                 (et:std--loop-check-at state (car step) (cadr step))))
            (when (nth 2 step)
              (et:std--loop-update-pattern
               state (nth 2 step) step-type))))
        (when iteration-terminates
          (push (et-cur-narrows) (et:std--loop-state-normal-exits state))))
      ;; A terminating iterator can reject its very first iteration.
      (when iteration-terminates
        (push iteration-entry (et:std--loop-state-normal-exits state)))
      (let* ((normal-paths (et:std--loop-state-normal-exits state))
             (normal-narrows
              (when normal-paths
                (cl-reduce #'et:flow--narrows-or normal-paths)))
             (normal-type
              (if normal-paths (et:std--loop-normal-result state) (et-never))))
        (if normal-paths
            (progn
              (et-set-narrows normal-narrows)
              (dolist (clause finally)
                (pcase (et:std--loop-clause-kind clause)
                  ('finally
                   (et:std--loop-check-paths
                    state (et:std--loop-clause-data clause)))
                  ('finally-return
                   (setq normal-type
                         (et:std--loop-check-at
                          state (et:std--loop-clause-data clause)
                          (et-cur-recommendation)))))))
          (let ((et:check--unreachable t))
            (dolist (clause finally)
              (pcase (et:std--loop-clause-kind clause)
                ('finally
                 (et:std--loop-check-paths
                  state (et:std--loop-clause-data clause)))
                ('finally-return
                 (et:std--loop-check-at
                  state (et:std--loop-clause-data clause)))))))
        (let* ((all-narrows
                (append (and normal-paths (list (et-cur-narrows)))
                        (et:std--loop-state-return-narrows state)))
               (result
                (apply #'et-union
                       normal-type
                       (et:std--loop-state-return-types state))))
          (et-set-narrows
           (if all-narrows
               (et:std--loop-filter-local-narrows
                (cl-reduce #'et:flow--narrows-or all-narrows)
                (et:std--loop-state-variables state))
             entry-narrows))
          (et:std--loop-remove-local-variables
           result (et:std--loop-state-variables state)))))))


;;; Checker

(et-set-checker #'cl-loop #'et:std--check-cl-loop)

(defun et:std--check-cl-loop ()
  "Parse and type-check `cl-loop' without declarative checker macros."
  (let* ((clauses (et:std--loop-parse (et-cur-expr)))
         (entry (et-cur-narrows))
         (trial
          (et-with-narrows entry
            (let ((type (et:check-preliminary-pass
                         (et:std--loop-run clauses entry))))
              (cons type (et-cur-narrows)))))
         (after-preliminary (cdr trial))
         (dirty (et:flow--narrows-changed-vars entry after-preliminary))
         (stable-entry
          (cl-remove-if (lambda (entry) (memq (car entry) dirty)) entry)))
    (ignore (car trial))
    (et-set-narrows stable-entry)
    (et:std--loop-run clauses stable-entry)))
