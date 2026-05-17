;;; et.el --- Typesystem for emacs lisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou;; -*- lexical-binding: t; -*- <benzanol@nixos>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'et-macros)
(require 'seq)


(defvar et-debug nil
  "Perform extra debug checks.")


;;; ============================================================
;;; Results
;;;; Struct

;; When type-checking an expression, it is important to know where
;; each error occurred in the expression. Since emacs lisp code is
;; made up of nested lists, we can express positions of the code as
;; paths to the correct expression, where each element of the path is
;; the index in the next expression.
;;
;; An `et-result' struct represents the result of performing some
;; action on an expression. It contains the output value, whether it
;; succeeded or failed, and a list of diagnostics which occurred, and
;; where they occurred in the expression.
;;
;; To collect an et-result, wrap the corresponding code in an
;; `et-result-boundary'. This will declare a collection of dynamically
;; scoped variables for collecting information about checking. If an
;; error is thrown, the result boundary will catch an error, and
;; observe the current value of `et--path' to see where the error
;; occurred.
;;
;; The `et-at' macro should be used whenever processing a
;; sub-expression of the current expression. The `et-error-boundary'
;; function should also be used to continue from a certain location in
;; the event of an error.

[et
 (@alias EtPath List<Integer>)
 (@alias EtSeverity (or @error @warning @hint))
 (@alias EtDiagnostic (Tuple EtPath EtSeverity String))]

(defvar et--in-result? nil)

(cl-defstruct et-result
  (value nil :et-generics [T] :et T|Nil)
  (failed nil :et Boolean)
  (diagnostics nil :et List<EtDiagnostic>))

(cl-defmethod cl-print-object ((result et-result) stream)
  (cl-flet* ((count-str (count) (if (eq count 0) "" (format " (+%s)" count))))

    (if (et-result-failed result)
        (cl-loop with others = 0
                 for (_path severity msg) in (et-result-diagnostics result)
                 when (eq 'error severity) collect (format "[%s]" msg) into strs
                 else do (cl-callf 1+ others)
                 finally do (princ (format "#<FAIL: %s%s>" (string-join strs " ") (count-str others)) stream))

      (princ (format "#<SUCCESS: %s%s>" (cl-prin1-to-string (et-result-value result))
                     (count-str (length (et-result-diagnostics result))))
             stream))))


;;;; Paths

(defvar et--path nil
  "The path to the current expression being processed.")

(defvar et--path-offset 0
  "An offset for future appends to `et--path'.

This is relevant if the current expression being processed is actually
the cdr of a larger expression. For example, when type checking a
function body, the body of the function is the `cddr' (offset=2) of a
lambda expression, or the `cdddr' (offset=3) of a defun.")

(defvar et--sticky-path nil
  "Whether to inhibit modifications to `et--path'.

`et--path' should only be changed when the expression being evaluated
corresponds to an expression present in the buffer. When calling a
function which thinks it is operating on a buffer expression, with an
expression that is not actually in the buffer, ensure that
`et--sticky-path' is non-nil to avoid creating an invalid path.")

(defun et--resolve-path (rel)
  (if et--sticky-path et--path
    (if-let* ((flat (flatten-tree (list rel))))
        (append et--path (list (+ et--path-offset (car flat))) (cdr flat))
      et--path)))

(defmacro et-at (rel &rest body)
  (declare (indent 1))
  (let* ((orig-var (gensym 'orig)))
    ;; On error, we want the path to stay where it is, hence using setq instead of let
    `(let ((,orig-var et--path))
       (setq et--path (et--resolve-path ,rel))
       (prog1 (let ((et--path-offset 0)) ,@body)
         (setq et--path ,orig-var)))))

(defmacro et-at-offset (offset &rest body)
  (declare (indent 1))
  ;; On error, we want the path to stay where it is, hence using setq instead of let
  `(let ((et--path-offset (+ et--path-offset ,offset)))
     ,@body))


(defmacro et-with-sticky-path (&rest body)
  "Evaluate BODY with a sticky path. See `et--sticky-path'."
  `(let* ((et--sticky-path t)) ,@body))


;;;; Diagnostics

(defvar et--result-diagnostics nil
  "Diagnostics collected for the current result.")

(defvar et--result-failed nil)


(defun et--diagnostic (rel severity fmt &rest args)
  (unless et--in-result? (error "Not in a result boundary"))
  (push (list (et--resolve-path rel) severity
              (if args (apply #'format fmt (mapcar #'et-pp args))
                (et-pp fmt)))
        et--result-diagnostics)
  ;; Intentionally return nil
  nil)

(defmacro et--define-diagnostics-function (name severity &optional failed)
  `(defun ,name (relative fmt &rest args)
     ,(format "Create a diagnostic with severity `%s'." severity)
     (apply #'et--diagnostic relative ',severity fmt args)
     ,@(when failed (list '(setq et--result-failed t)))
     nil))

(et--define-diagnostics-function et-err error t)
(et--define-diagnostics-function et-warn warning)
(et--define-diagnostics-function et-hint hint)

(defun et-fatal (relative fmt &rest args)
  (et-at relative (error "%s" (if args (apply #'format fmt args) fmt))))


;;;; Boundaries

(defmacro et-error-boundary (relative &rest body)
  (declare (indent 1))
  `(et-at ,relative
     (condition-case-unless-debug err (progn . ,body)
       (error (et-err nil (error-message-string err))))))

(defmacro et-result-boundary (&rest body)
  (declare (et (@generics [T])
               (@return *et-result<T>)))

  `(let* ((et--in-result? t)
          (et--path nil)
          (et--path-offset 0)
          (et--sticky-path nil)
          (et--result-diagnostics nil)
          (et--result-failed nil))
     (make-et-result
      :value (et-error-boundary nil ,@body)
      :failed et--result-failed
      :diagnostics et--result-diagnostics)))

(defun et-propagate-result (result)
  (cl-assert et--in-result?)
  (cl-loop for (path severity msg) in (et-result-diagnostics result)
           do (et--diagnostic path severity msg))
  (when (et-result-failed result) (setq et--result-failed t)))

;; I think that this is a bad idea: either be a result boundary or don't, not both
;; (defmacro et-subresult-boundary (&rest body)
;;   `(let* ((result (et-result-boundary ,@body)))
;;      (when et--in-result? (et-propagate-result result))
;;      result))

(defmacro et-failed-boundary (&rest body)
  "Evaluate BODY with `et--result-failed' temporarily bound to nil.

Sometimes, we care whether a particular function call failed. Checking
`et--result-failed' normally isn't sufficient, because it already might
be non-nil."
  `(let* ((value-and-failed
           (let* ((et--result-failed nil))
             (cons (progn ,@body) et--result-failed))))
     (setq et--result-failed (or et--result-failed (cdr value-and-failed)))
     (car value-and-failed)))


;;;; Utils

(defun et-result-map (func exprs)
  (cl-loop for expr in exprs
           for idx upfrom 0
           collect (et-at idx (funcall func expr))))


;;; ============================================================
;;; Testing macros
;;;; Flycheck rebasing

(defvar et--error-path nil)

(defmacro et-with-error-path (path &rest body)
  (declare (indent 1))
  `(let* ((et--error-path (append et--error-path ,path nil)))
     (condition-case err (progn ,@body)
       (error (et-error nil (error-message-string err)) nil))))

(defun et-error (path string &rest args)
  (setq string (concat string (et--error-message-suffix path)))
  (byte-compile-warn "%s" (if args (apply #'format string args) string))
  nil)

(defun et--error-message-suffix (path)
  (format "\0;;error-path:%s" (append et--error-path path)))

(defun et--traverse-buffer-expr (path)
  (goto-char (point-min))
  (dotimes (_ (or (car path) 0)) (forward-sexp))
  (forward-comment (buffer-size))

  (dolist (idx (cdr path))
    ;; Skip whitespace and comments before looking at the next form
    (cond
     ((looking-at-p ",\\|`\\|#?']")
      (if (eq idx 1)
          (goto-char (match-end 0))
        (error "Only valid subexpr of quote is 1")))

     ((looking-at-p "[([]")
      (forward-char 1)
      (dotimes (_ idx) (forward-sexp))
      (forward-comment (buffer-size)))

     (t (error "Invalid expression container: %s" (thing-at-point 'char))))

    (forward-comment (buffer-size))))


;;;; Repeat

(defmacro et-repeat (var repls &rest body)
  (declare (indent 2))
  (cl-assert (vectorp repls))
  (cl-loop for repl across repls
           collect (cl-subst repl var body) into all
           finally return (cons #'ignore all)))


;;;; Testing

(eval-and-compile
  (defvar et-run-tests (if noninteractive t nil)
    "Whether to run et tests when compiling source files.")

  (defvar et-running-tests nil
    "Whether tests are currently being run."))

(defmacro et-test (&rest body)
  "BODY can start with a series of VAR VECTOR... forms."

  (when (and et-run-tests (null load-file-name)
             (stringp (car command-line-args-left)))
    ;; Require the file without tests
    (condition-case _
        (let ((et-run-tests nil))
          (load-file (car command-line-args-left)))
      (:success nil)
      (t (setq et-run-tests nil))))

  (when (and et-run-tests (null load-file-name))
    ;; Repeat var
    (let* ((et-running-tests t)
           (evaller
            (lambda (body start-idx)
              (cl-loop for expr in body
                       for idx upfrom 0
                       do (et-with-error-path (list (+ idx start-idx))
                            (or (eval expr) (error "Returned nil"))))))
           repeat-var repeat-forms)

      (when (symbolp (car body))
        (setq repeat-var (pop body)
              repeat-forms (pop body)))

      (if repeat-var
          (cl-loop for form across repeat-forms
                   do (funcall evaller (cl-subst form repeat-var body) 3))
        (funcall evaller body 1)))))

(defmacro et-assert-error (expr)
  (declare (indent 1))
  `(et-with-error-path (list 1)
     (condition-case val ,expr
       (error t)
       (:success (error "=> %s" val)))))


;;; ============================================================
;;; Utils
;;;; Recursive copy

(defun et--recursive-copy (object func)
  "Return a copy of OBJECT by applying FUNC to every object.

This will start by applying FUNC to OBJECT. If the returned
result is an atom (not a cons cell,) it is returned. Otherwise,
what will be returned is the result of repeating this process for
both sides of the cons cell."
  (declare (et (object Any)
               (func Abcd)
               (@return Any)))

  (if (not (eq object (setq object (funcall func object))))
      object

    (cond
     ;; Handle structs
     ((recordp object)
      (let* ((entries nil))
        (dotimes (i (length object))
          (push (et--recursive-copy (aref object i) func) entries))
        (apply #'record (nreverse entries))))

     ((atom object) object)
     ;; We could just do (cons (copy (car object) func) (copy (cdr object) func)),
     ;; but this would hit the recursion limit for long lists.
     ;; The current solution is equivalent, but does all elements of a list in the same call.
     ((prog1 (setq object (cons (et--recursive-copy (car object) func)
                                (funcall func (cdr object))))
        (while (consp (cdr object))
          (setcdr object (cons (et--recursive-copy (cadr object) func)
                               (funcall func (cddr object))))
          (setq object (cdr object))))))))


;;;; Substitute with placeholder

(defun et--subst-placeholder (idx)
  (declare (et (idx Abcd)
               (@return Symbol)))
  (intern (format "@@et-ph-%s@@" idx)))

(defun et--subst-to-placeholders (object pred)
  "Substitute certain values in OBJECT with placeholders.

This will return (NEW-OBJECT REPL-LIST) where NEW-OBJECT is OBJECT with
all values matching PRED replaced by a placeholder. Values that are `eq'
will get replaced by the same placeholder. REPL-ALIST is a list
of (VALUE . REPLACEMENT) that were replaced by placeholders."
  (let* ((repl-alist nil))
    (cons
     (et--recursive-copy
      object
      (lambda (x)
        (if (not (funcall pred x)) x
          (or (alist-get x repl-alist)
              (cdar (push (cons x (et--subst-placeholder (length repl-alist))) repl-alist))))))
     repl-alist)))

(defun et--subst-with-placeholders (object repl-alist)
  (et--recursive-copy object (lambda (x) (or (alist-get x repl-alist) x))))

(defun et--subst-from-placeholders (object repl-alist)
  (et--recursive-copy
   object
   (lambda (x) (if-let* ((entry (rassq x repl-alist)))
                   (car entry)
                 x))))

(et-test
 (pcase-let* ((`(,new-obj . ,repls)
               (et--subst-to-placeholders
                '((1 2 3) (4 5 6))
                (lambda (x) (and (numberp x) (= 0 (mod x 2)))))))
   (and (pcase new-obj (`((1 ,(pred symbolp) 3) (,(pred symbolp) 5 ,(pred symbolp))) t))
        (= 3 (length repls))
        (equal (et--subst-from-placeholders (list (cadr new-obj) (car new-obj)) repls)
               '((4 5 6) (1 2 3))))))


;;;; Caching

(defvar et-cache-file "~/.emacs.d/.cache/et-cache.el"
  "File to use to store the cache.")

(defvar et-cache nil
  "Hash table containing the cache.")

(defvar et--caching nil
  "Non-nil when the expression currently being evaluated will be cached.")

(defvar et-cache-hits 'NO
  "When this is a list, cache hits will be placed here for debugging.")

(defvar et-caching-enabled t
  "Whether to enable caching.")

(defun et--load-cache ()
  (let* ((hash-table (make-hash-table :test #'equal)))
    ;; Read the file into a hashtable
    (when (and (stringp et-cache-file) (file-exists-p et-cache-file))
      (with-temp-buffer
        (insert-file-contents et-cache-file)
        (goto-char (point-min))
        (while (let* ((pair (ignore-errors (read (current-buffer)))))
                 (when (consp pair)
                   (puthash (car pair) (cdr pair) hash-table)
                   t)))))
    hash-table))

(defun et--cache-retrieve (key)
  (ignore-errors
    (when-let* ((val (gethash key (if (hash-table-p et-cache) et-cache
                                    (setq et-cache (et--load-cache))))))
      ;; For debugging
      (when (listp et-cache-hits)
        (push (cons key val) et-cache-hits))

      ;; Return a copy
      (et--recursive-copy val #'identity))))

(defun et--cache-store (key value)
  (ignore-errors
    (unless (hash-table-p et-cache) (setq et-cache (et--load-cache)))

    (puthash key value et-cache)
    (write-region (format "%s" (prin1-to-string (cons key value))) nil et-cache-file
                  (file-exists-p et-cache-file)))
  value)

(defmacro et-cache (key ph-pred &rest body)
  (declare (indent 2))
  `(pcase-let* ((et--caching t)
                (`(,subst-key . ,repls) (et--subst-to-placeholders ,key ,ph-pred)))
     (if-let* ((_ et-caching-enabled)
               (retrieved (et--cache-retrieve subst-key)))
         (et--subst-from-placeholders retrieved repls) ; Insert placeholders

       (let* ((value (progn . ,body)))
         (et--cache-store subst-key (et--subst-with-placeholders value repls))
         value))))

(defun et-clear-cache ()
  (interactive)
  (setq et-cache (make-hash-table))
  (write-region "" nil et-cache-file))

(defun et-refresh-cache ()
  (interactive)
  (setq et-cache (et--load-cache)))


;;;; Quote macro

(eval-and-compile
  (defun et--copy-quotes (expr)
    (cond ((and (eq (car-safe expr) #'quote) (consp (cdr-safe expr)))
           (list #'copy-tree expr))
          ((consp expr) (cons (et--copy-quotes (car expr)) (et--copy-quotes (cdr expr))))
          (t expr))))

(et-test
 (equal '(a (copy-tree '(b)) c (copy-tree ''(((1 2 'hi)))))
        (et--copy-quotes '(a '(b) c ''(((1 2 'hi)))))))


(defmacro et-q (expr)
  "Like `backquote', but return copies of all list literals.

This avoids the undefined behavior caused by mutating list literals by
applying `copy-tree' to all list ltierals before they are returned. This
could be improved in the future by replacing all list literals with
instances of `list' and `cons', but this is not currently a high
priority."
  (et--copy-quotes (cdr (backquote-process expr))))

(defmacro et-ql (&rest exprs)
  (et--copy-quotes (cdr (backquote-process exprs))))


;;;; Dnf And

(defun et--dnf-and (&rest dnfs)
  "Return the DNF of intersecting DNFS."

  (pcase dnfs
    ('() (list (list)))
    (`(,a) a)
    (`(,a ,b ,c . ,rest) (et--dnf-and a (apply #'et--dnf-and b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in a
              nconc
              (cl-loop for b-case in b
                       collect (append a-case b-case))))))


;;;; Stop recursion

(defmacro et--stop-recursion (var elem default &rest body)
  "This allows defining recursive algorithms that loop.

A function implementing this kind of algorithm should define a stack
variable, which holds the current call stack. Each call to this macro
will add ELEM to the call stack. If ELEM already existed in the call
stack, then DEFAULT will be evaluated, stored, and returned. If ELEM is
ever encountered again, this stored value will be returned.

When execution returns to the original stack frame, the frame will have
access to the default value that was created, as the cdar of the stack
variable."
  (declare (indent 3))
  `(let ((elem ,elem))
     (if-let* ((entry (assoc elem ,var)))
         (setcdr entry (or (cdr entry) ,default))
       (let ((,var (cons (cons elem nil) ,var)))
         ,@body))))


;;; ============================================================
;;; Types
;;;; Struct

(cl-defstruct et-var
  "A variable currently in scope."
  name type)

(cl-defstruct et-datatype
  "A datatype factor of an `et-type'."
  (name nil :et-generics [A B] :et Symbol)
  (args nil :et List<*et-type|Any>))

(cl-defstruct et-alias "A type alias factor of an `et-type'." name args)

(cl-defstruct et-type-case
  "Struct representing a case of an `et-type'.

BINDS is a list of (`et-var' . `et-type').

TYPEOFS is a list of `et-var'.

VALUE is an instance of either `et-datatype' or `et-alias'."
  (value nil :et *et-datatype|*et-alias)
  (binds nil :et AList<Symbol~*et-var>)
  (typeofs nil :et List<*et-var>))

(cl-defstruct et-type
  "Struct representing a root-level et type.

  CASES is a list of `et-type-case' instances being unioned."
  (cases nil :et List<*et-type-case>)
  (label nil :et Nil|EtTypeLabel))

(defun et--verify-type (type)
  "Check that a matcher is valid."
  (unless (et-type-p type)
    (error "Not a type: %s" type))

  (when et-debug
    (dolist (case (et-type-cases type))
      (let ((val (et-type-case-value case)))
        (cond
         ((et-datatype-p val)
          ;; Check that all of the arguments have the correct role
          (et--datatype-map-type-args (et-datatype-name val) (et-datatype-args val) #'et--verify-type))
         ((et-alias-p val) (mapc #'et--verify-type (et-alias-args val)))
         (t (error "Expected datatype or alias, found %s" val))))

      (dolist (x (et-type-case-binds case))
        (or (and (consp x) (et-var-p (car x)) (et--verify-type (cdr x)))
            (error "Expected bind, found %s" x)))
      (dolist (x (et-type-case-typeofs case))
        (or (et-var-p x) (error "Expected typeof var, found %s" x)))))

  type)

(advice-add #'make-et-type :filter-return #'et--verify-type)


;;;; Datatypes

[et (@alias EtDatatypeRole (or @CONST @CO @CONTRA @ISO @IGNORE))
    (@alias EtDatatypeProps
            (PList :args List<EtDatatypeRole>
                   :overlap True|List<Symbol>
                   :predicate (Function Any True|List<Any>)))
    (@alias EtDatatypeName (or @Any @Literal @NonNil
                               @Symbol @NonNilSymbol @Number @Integer @Positive @Negative @String
                               @ConsFull @ConsFresh @VectorFull @VectorFresh @PList
                               @Function @DynFunction
                               @Struct @Scoped))
    (@variable et--datatypes AList<EtDatatypeName~EtDatatypeProps>)]

(defvar et--datatypes
  '((Any :args nil :overlap t :predicate (lambda (v) t))
    ;; Literal<VALUE> is a type matching only the value VALUE
    (Literal :args (CONST) :overlap nil :predicate (lambda (v me) (equal v me)))
    (NonNil :args nil :overlap t :predicate (lambda (v) v))
    (Symbol :args nil :overlap (Function DynFunction) :predicate symbolp)
    (NonNilSymbol :args nil :overlap (Function DynFunction)
                  :predicate (lambda (v) (and v (symbolp v))))
    (Number :args nil :overlap nil :predicate numberp)
    (Integer :args nil :overlap (Positive Negative) :predicate integerp)
    (Positive :args nil :overlap nil :predicate (lambda (v) (and (numberp v) (> v 0))))
    (Negative :args nil :overlap nil :predicate (lambda (v) (and (numberp v) (< v 0))))
    (String :args nil :overlap nil :predicate stringp)

    ;; ConsFull<CAR-READ CAR-WRITE CDR-READ CDR-WRITE> is a cons cell.
    ;; CAR-READ/CDR-READ are the output types of calling car/cdr on
    ;; the cons cell. CAR-WRITE/CDR-WRITE are the types that are valid
    ;; to write to the cons cell. The most general cons cell is thus
    ;; ConsFull<Any Never Any Never>.
    (ConsFull :args (CO CONTRA CO CONTRA) :overlap (ConsFresh Function DynFunction PList) :intersect t
              :predicate (lambda (v l _1 r _2) (when (consp v) `((,(car v) . ,l) (,(cdr v) . ,r)))))
    ;; When you create a new cons cell with cons/list/quote/etc, you
    ;; get a ConsFresh. This can be thought of as an "undetermined"
    ;; cons cell: in that it knows what it contains, but it has not
    ;; yet decided what can be written to it. A ConsFresh can be
    ;; converted to a ConsFull as long as the read types of the
    ;; ConsFull are supertypes of the arg types of the ConsFresh.
    (ConsFresh :args (CO CO) :overlap (Function DynFunction PList) :intersect t
               :predicate (lambda (v l r) (when (consp v) `((,(car v) . ,l) (,(cdr v) . ,r)))))

    ;; VectorFull<ELEM-READ ELEM-WRITE>: same idea as ConsFull
    (VectorFull :args (CO CONTRA) :overlap (VectorFresh) :intersect t
                :predicate (lambda (v e _) (when (vectorp v) (or (cl-loop for x across v collect (cons x e)) t))))
    ;; Same idea as ConsFresh
    (VectorFresh :args (CO) :overlap nil :intersect t
                 :predicate (lambda (v e) (when (vectorp v) (or (cl-loop for x across v collect (cons x e)) t))))

    ;; Function<ARGLIST-TYPE OUTPUT-TYPE> is a function with a fixed
    ;; input and output type.
    (Function :args (CONTRA CO) :overlap (DynFunction) :intersect t)
    ;; DynFunction<ARGLIST-MATCHER OUTPUT-STRUCTURE> is a
    ;; function whose output depends on the input. For a given
    ;; ARGLIST-TYPE, the output of the function is determined by
    ;; inferring ARGLIST-TYPE against ARGLIST-MATCHER, with
    ;; OUTPUT-STRUCTURE as the output.
    (DynFunction :args (CONST CONST) :overlap nil)

    ;; PList<PROP1 VAL1 PROP2 VAL2 ...> is a covariant, unordered
    ;; plist.
    (PList
     :args (lambda (args)
             (cl-loop for (_prop _val) on args by #'cddr
                      nconc (list 'CONST 'ISO)))
     :overlap nil
     :intersect et--plist-intersect-args)

    ;; Struct<NAME~GENERCIC-PARAMS...>
    (Struct :args (lambda (args)
                    (if-let* ((name (car args))
                              (plist (get name 'et-struct))
                              (arg-count (length (plist-get plist :generics))))
                        (if (eq (length (cdr args)) arg-count)
                            (cons 'CONST (make-list (length (cdr args)) 'ISO))
                          (error "Struct %s takes %s arguments" name arg-count))
                      (error "Not a struct: %s" name)))
            :overlap nil :intersect nil)

    ;; Scoped datatypes occur when you have a function with generics.
    ;; Then, inside of that function you can use the generics provided
    ;; in the function as types. How a scoped datatype interacts with
    ;; other datatypes is determined entirely by what constraints were
    ;; placed upon it in its definition. Its arguments are (NAME
    ;; UNIQUE CONSTRAINTS), where UNIQUE is a unique symbol for this
    ;; scoped datatype, and CONSTRAINTS is a list of type constraints.
    (Scoped
     :args (CONST CONST CONST)
     :overlap t
     :intersect nil))
  "Datatypes")

(defvar et--scoped-datatypes nil
  "A list of (NAME SCOPED CONSTRAINTS).")

(defun et--make-scoped-datatypes (matcher)
  (declare (et (matcher Nil|*et-matcher)
               (@return (List (Tuple NonNilSymbol NonNilSymbol List<EtConstraint>)))))

  (cl-loop for name in (when matcher (et-matcher-generics matcher))
           for qs = (cl-loop for q in (when matcher (et-matcher-constraints matcher))
                             when (eq (cadr q) name)
                             collect q)
           collect (list name (gensym (format "scoped-%s@" name)) qs)))

(defmacro et--with-scoped-datatypes (scoped &rest body)
  (declare (indent 1))
  `(let* ((et--scoped-datatypes (append ,scoped et--scoped-datatypes)))
     ,@body))

(defun et--plist-intersect-args (args1 args2 intersect _union)
  (let* ((all-props (cl-loop for (p) on (append args1 args2) by #'cddr collect p)))
    (cl-loop for prop in (delete-dups all-props)
             for val1 = (plist-get args1 prop)
             for val2 = (plist-get args2 prop)
             for intersection = (if val1 (if val2 (funcall intersect val1 val2) val1) val2)
             when (et-never-p intersection) return 'INVALID
             nconc (list prop intersection))))


;;;; Datatype helpers

(defun et--datatype-name? (name)
  "Check if NAME is a datatype name."
  (not (not (assq name et--datatypes))))

(defun et--scoped-datatype-from-name (name)
  "Check if NAME is a datatype name."
  (assq name et--scoped-datatypes))

(defun et--datatype-arg-roles (dt-name dt-args)
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'.

The resulting list must be the exact length of DT-ARGS, and each element
corresponds to the role of each argument in `dt-args'. `CONST' indicates
an argument which is a literal Lisp value. `CO'/`CONTRA'/`ISO' indicate
that the argument is a type argument, and whether the type argument is
covariant, contravariant, or isovariant."
  (pcase (plist-get (or (alist-get dt-name et--datatypes)
                        (error "Invalid datatype: %s %s" dt-name dt-args))
                    :args)
    ((and (pred functionp) func) (funcall func dt-args))
    (other (copy-tree other))))

(defun et--datatype-might-overlap-nontrivial? (a-dt b-dt)
  "Return whether datatypes A and B might overlap.

This function is designed for `nontrivial' cases, in that it assumes
that A and B are not subtypes of each other."
  (let* ((a (et-datatype-name a-dt))
         (b (et-datatype-name b-dt)))

    (when (< (cl-position b et--datatypes :key #'car) (cl-position a et--datatypes :key #'car))
      (cl-rotatef a b))
    (pcase (plist-get (alist-get a et--datatypes) :overlap)
      ('t t)
      (overlap (not (not (memq b overlap)))))))

(defun et--datatype-intersect-args-nontrivial (name args1 args2 intersect union)
  "Return a list of arguments intersecting ARGS1 and ARGS2.

The goal of this function is to determine a list of arguments
INTERSECTION-ARGS such that (NAME INTERSECTION-ARGS) is a subtype of
both (NAME ARGS1) and (NAME ARGS2).

If no such list is found, then return the symbol `INVALID'.

This function is designed for `nontrivial' cases in that it assumes that
neither datatype is already a subset of the other, in which case the
subset args would be a trivial solution to this function. This is so
that this function can focus on the non-trivial cases where neither is a
subset of the other.

INTERSECT and UNION are functions which each take 2 elements from
ARGS1/ARGS2 and return a new arg, either the intersection or union of
the two args respectively."

  (pcase (plist-get (alist-get name et--datatypes) :intersect)
    ((and func (pred functionp)) (funcall func args1 args2 intersect union))
    ('t (cl-loop for role in (et--datatype-arg-roles name args1)
                 for arg1 in args1
                 for arg2 in args2
                 for new-arg = (pcase role
                                 ('CO (funcall intersect arg1 arg2))
                                 ('CONTRA (funcall union arg1 arg2))
                                 ('ISO (if (equal arg1 arg2) arg1 'INVALID))
                                 (_ (error "Unexpected arg role: %s" role)))
                 when (or (eq new-arg 'INVALID) (et-never-p new-arg)) return 'INVALID
                 collect new-arg))
    (_ 'INVALID)))

(defun et--datatype-constraints (sub-name sub-args super-name super-args co contra iso co-literal)
  "Determine when one datatype to be a subtype of another.

Returns the list of constraints required for (SUB-NAME SUB-ARGS) to be a
subtype of (SUPER-NAME SUPER-ARGS), using provided functions provided
for checking the sub-arguments.

Specifically, (funcall CO/CONTRA/ISO sub-arg super-arg) will return the
constraints necessary for sub-arg/super-arg to be a
subtype/supertype/equal (respectively) of super-arg. The reason these
must be provided as different functions is that the sub and super
datatypes may have different arg types. For example one might be a
matcher and another might be a type.

Also, (funcall CO-LITERAL val super-arg) checks if the literal val is a
subtype of super-arg."

  (cl-flet ((valid-if (valid) (if valid nil (list (et--never-constraint)))))

    (pcase (list sub-name super-name)
      (`(,_ Any) nil)

      ('(Integer Number) nil)
      ('(Positive Number) nil)
      ('(Negative Number) nil)

      ('(ConsFresh ConsFull) (append (funcall co (car sub-args) (car super-args))
                                     (funcall co (cadr sub-args) (caddr super-args))))
      ('(VectorFresh VectorFull) (funcall co (car sub-args) (car super-args)))

      (`(DynFunction Function)
       (if-let* ((func-input (car super-args))
                 (func-output (cadr super-args))
                 ((and (et-type-p func-input) (et-type-p func-output)))
                 (dyn-output (et--funcall (apply #'et-dt 'DynFunction sub-args)
                                          (car super-args)))
                 ((et-type-p dyn-output))
                 ((et-subtype? dyn-output func-output)))
           (valid-if t)
         (valid-if nil)))

      ('(,_ NonNil)
       (pcase super-name
         ('Literal (valid-if (cadr super-args)))
         ('Symbol (valid-if nil))
         (_ (valid-if t))))
      ('(,_ NonNilSymbol)
       (pcase super-name
         ('Literal (valid-if (cadr super-args)))
         (_ (valid-if nil))))

      ('(ConsFull PList)
       (et--cons-is-plist sub-args super-args co))

      ('(Plist Plist)
       (cl-loop for (prop super-val) on super-args by #'cddr
                for sub-val = (plist-get sub-args prop)
                unless sub-val return (list (et--never-constraint))
                nconc (funcall co sub-val super-val)))

      ((guard (eq sub-name super-name))
       ;; Datatypes of the same type (except PList) should have the same number of arguments
       (cl-assert (eq (length sub-args) (length super-args)))
       (cl-loop for sub-arg in sub-args
                for super-arg in super-args
                for role in (et--datatype-arg-roles super-name super-args)
                nconc (pcase role
                        ;; Const args must be equal to match
                        ('CONST (valid-if (equal sub-arg super-arg)))
                        ('CO (funcall co sub-arg super-arg))
                        ('CONTRA (funcall contra sub-arg super-arg))
                        ('ISO (funcall iso sub-arg super-arg))
                        (_ (error "Unknown argument role: %s" role)))))

      (`(Literal ,_)
       (let* ((pred (plist-get (alist-get super-name et--datatypes) :predicate)))
         (pcase (apply (or pred #'ignore) (car sub-args) super-args)
           ('nil (valid-if nil))
           ('t (valid-if t))
           (sub (cl-loop for (sub-val . arg) in sub nconc (funcall co-literal sub-val arg))))))

      (_ (valid-if nil)))))

(defun et--cons-is-plist (cons-args plist-args co)
  "Constraints for ConsFull to be a subtype of PList.
A plist is a flat list (K1 V1 K2 V2 ...).  The ConsFull car is a key.
If it matches a required PList key, the cdr must be a cons whose car
satisfies that key's value type and whose cdr covers the remaining
keys.  If it does not match, the cdr must be a cons (skipping the
value) whose cdr still covers all required keys.  Extra keys are
allowed and order does not matter."
  (let ((car-read (et-expand-all-aliases (nth 0 cons-args)))
        (cdr-read (nth 2 cons-args)))
    (pcase (et-type-cases car-read)
      (`(,(cl-struct et-type-case
                     (value (cl-struct et-datatype (name 'Literal) (args `(,prop))))))
       (let ((pval (plist-get plist-args prop))
             (rest-plist (copy-tree plist-args)))
         (when pval (cl-remf rest-plist prop))
         (let ((tail (if rest-plist
                         (apply #'et-dt 'PList rest-plist)
                       (et-any))))
           (funcall co cdr-read
                    (if pval
                        (et-alias 'ConsR pval tail)
                      (et-alias 'ConsR (et-any) tail))))))
      (_ (list (et--never-constraint))))))


;;;; Datatype mappers

(defun et--datatype-map-args (dt-name dt-args func)
  "Apply FUNC to each argument, returning the resulting list.

FUNC is called with two arguments, ARG and ROLE, where role is one of
`CONST', `CO', `CONTRA', or `ISO'."
  (cl-loop for arg in dt-args
           for role in (et--datatype-arg-roles dt-name dt-args)
           collect (funcall func arg role)))

(defun et--datatype-map-type-args (dt-name dt-args func)
  "Like `et--datatype-map-args', but the identify for CONST args.

FUNC is called with one argument, the current argument"
  (cl-loop for arg in dt-args
           for role in (et--datatype-arg-roles dt-name dt-args)
           if (eq role 'CONST) collect arg
           else collect (funcall func arg)))


;;;; Defining aliases

[et (@alias EtTypeSpec Any)
    (@alias EtGeneric NonNilSymbol)
    (@alias EtGenVec (VectorR (or EtGeneric (TupleR (or @= @<= @>=) EtGeneric Any))))
    (@alias EtAliasName NonNilSymbol)
    (@alias EtAliasDefinitionPlist [(<= R (or @TYPE @MATCHER @BOTH))]
            (PList :restrict R
                   :custom (or Nil (Function Args<List<Any>> EtRestrictedStructure<R>))
                   :generics List<NonNilSymbol>
                   :constraints List<EtConstraint>
                   :structure (or Nil EtRestrictedStructure<R>)
                   :type (or Nil *et-type)))]

(defmacro et-define-custom-alias (name arglist &rest body)
  (declare (indent 2))
  (let* ((props (cl-loop while (keywordp (car body))
                         nconc (list (pop body) (pop body)) into props
                         finally return props))
         (to (plist-get props :type-only))
         (mo (plist-get props :matcher-only))
         (_ (and to mo (error "Alias cannot be both type-only and matcher-only")))
         (restrict (if to 'TYPE (if mo 'MATCHER 'BOTH))))

    `(put ',name 'et-alias
          (list
           :custom (lambda ,arglist ,@body)
           :restrict ',restrict
           ,@props))))

(defun et--gen-vec-generics (gen-vec)
  (declare (et (gen-vec EtGenVec) (@return List<EtGeneric>) (@skip)))

  (when gen-vec
    (cl-loop for gen-spec across gen-vec
             for gen =
             (pcase gen-spec
               (`(,(or '= '<= '>=) ,(and gen (pred symbolp)) ,_type-spec) gen)
               ((pred symbolp) gen-spec)
               (_ (error "Invalid generic entry: %s" gen-spec)))
             unless (memq gen generics) collect gen into generics
             finally return generics)))

(defun et--gen-vec-constraints (gen-vec)
  "Parse a generic vector to a list of generics and constraints."
  (declare (et (gen-vec EtGenVec) (@return List<EtConstraint>) (@skip)))

  (when gen-vec
    (cl-loop for gen-spec across gen-vec
             for constraint =
             (pcase gen-spec
               (`(,(or (and '= (let op 'Q:EQ))
                       (and '<= (let op 'Q:LEQ))
                       (and '>= (let op 'Q:GEQ)))
                  ,(and gen (pred symbolp)) ,type-spec)
                (list op gen (et-parse-type type-spec))))
             when constraint collect constraint)))

(defun et--define-alias (name gen-vec spec &rest props)
  (apply #'et--declare-alias name gen-vec spec props)
  (et--initialize-alias name))

(defun et--initialize-alias (name)
  (if-let* ((props (get name 'et-alias))
            (spec (plist-get props :spec))
            (restrict (plist-get props :restrict)))
      (progn (plist-put props :structure (et--parse-struct spec (plist-get props :generics) restrict))
             (plist-put props :constraints (et--gen-vec-constraints (plist-get props :gen-vec))))
    (error "Alias `%s' not declared" name)))

(defun et--declare-alias (name gen-vec spec &rest props)
  (declare (et (name EtAliasName)
               (gen-vec EtGenVec)
               (spec EtTypeSpec)
               (@return Nil)
               (@skip)))

  (when (plist-get (get name 'et-alias) :read-only)
    (error "Alias %s is already defined, and is read-only" name))

  (pcase-let* ((gens (et--gen-vec-generics gen-vec))
               (restrict (or (plist-get props :restrict) 'BOTH))
               (_ (or (memq restrict '(TYPE MATCHER BOTH))
                      (error ":restrict must be one of `TYPE', `MATCHER', or `BOTH'")))
               (plist (cl-list*
                       :restrict restrict
                       :gen-vec gen-vec
                       :generics gens
                       :spec spec
                       props)))
    (put name 'et-alias plist)
    nil))

(defun et--props-and-body (body)
  (let* ((props nil))
    (while (keywordp (car body))
      (setq props (nconc props (list (pop body) (pop body)))))
    (or body (error "Empty body"))
    (or (eq 1 (length body)) (error "Body can only contain one expression"))
    (cons props (car body))))

(defmacro et-defalias (name &rest body)
  "Alias NAME types to return the specific type.

\(fn NAME [GENERIC-VECTOR] [PROPS...] BODY...)"
  (declare (indent 2))

  (let* ((gen-vec (when (vectorp (car body)) (pop body)))
         (pb (et--props-and-body body)))
    `(et--define-alias ',name ,gen-vec ',(cdr pb) ,@(car pb))))


;;;; Expanding aliases

(defun et-alias-expand (alias)
  "Expand an alias to a type."
  (declare (et (alias *et-alias)
               (@return *et-type)
               (@skip)))

  (et--alias-call (et-alias-name alias) (et-alias-args alias) 'TYPE))

(defun et--expand-alias-as-matcher-dnf (name args _generics)
  "Expand an alias within a matcher."
  (et--alias-call name args 'MATCHER))

(defun et-expand-all-aliases (type)
  (et--verify-type type)

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           append (if (et-alias-p val)
                      (apply #'list (et-type-cases (et-expand-all-aliases (et-alias-expand val))))
                    (list case))
           into new-cases
           finally return (make-et-type :cases new-cases)))

(defun et--alias-call (name args target)
  "Expand the alias with name NAME, passing arguments ARGS."
  (declare (et (@generics [(<= T @TYPE|@MATCHER)])
               (name EtAliasName)
               (args (ListR (and (extends? @TYPE T *et-type Never)
                                 (extends? @MATCHER T EtMatcherStructure Never))))
               (target T)
               (@return (or (extends? @TYPE T *et-type Never)
                            (extends? @MATCHER T EtMatcherStructure Never)))
               (@skip)))

  (let* ((plist (or (get name 'et-alias) (error "Alias %s is not defined" name)))
         (restrict (or (plist-get plist :restrict) (error "No target defined for alias")))
         (custom (plist-get plist :custom))
         (generics (plist-get plist :generics)))

    ;; Ensure the alias is valid for the intended target
    (when (or (and (eq restrict 'TYPE) (eq target 'MATCHER))
              (and (eq restrict 'MATCHER) (eq target 'TYPE)))
      (error "Alias %s not defined for %s" name (downcase (format "%s" restrict))))

    (cond
     (custom
      (let* ((structure (apply custom args)))
        (if (eq target 'MATCHER) structure
          (et-structure-to-type structure nil))))

     ;; Ensure there are the right number of arguments
     ((not (eq (length generics) (length args)))
      (error "Alias %s expected %s arguments, but %s were provided"
             name (length generics) (length args)))

     ((let* ((structure (plist-get plist :structure))
             (gen-repls (cl-loop for gen in generics
                                 for arg in args
                                 collect (cons gen arg))))

        (unless structure (error "Alias %s defined incorrectly: Missing structure" name))

        ;; Replace S:GENERIC with the specified arg
        (if (eq target 'MATCHER)
            (if (null generics) structure
              (et--structure-substitute-generics structure gen-repls))
          (et-structure-to-type structure gen-repls)))))))


;;;; Constructors

(defun et-never-p (type)
  (et--verify-type type)
  (null (et-type-cases type)))

(defun et-type (&rest cases)
  "Construct a new `et-type' out of CASES.

Each of CASES should be an instance of `et-type-case', or alternatively
a valid `et-type-case-value'."
  (cl-loop for c in cases
           collect (if (et-type-case-p c) c
                     ;; Checking is done inside of `make-et-type'
                     (make-et-type-case :value c))
           into cases
           finally return (make-et-type :cases cases)))

(defun et-dt (name &rest args)
  (cl-assert (et--datatype-name? name))
  (et-type (make-et-datatype :name name :args args)))

(defun et-alias (name &rest args)
  (cl-assert (symbolp name))
  (cl-assert (string-match-p "^[A-Z]" (symbol-name name)))
  (et-type (make-et-alias :name name :args args)))

(defun et-any () (et-dt 'Any))
(defun et-never () (make-et-type :cases nil))
(defun et-literal (val) (et-dt 'Literal val))


;;; ============================================================
;;; Matching
;;;; Struct

(cl-defstruct et-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the struct representing the matcher."
  (generics nil :et List<EtGeneric>)
  (dnf nil :et EtMatcherStructure)
  (constraints nil :et List<EtConstraint>))

(defun et--verify-matcher (matcher)
  "Check that a matcher is valid."
  (or (et-matcher-p matcher)
      (error "Not a matcher: %s" matcher))

  (when et-debug
    (let* ((generics (et-matcher-generics matcher)))
      (dolist (generic generics)
        (or (symbolp generic) (error "Generics must be a list of symbols")))

      (cl-loop for q in (et-matcher-constraints matcher)
               do (pcase q
                    (`(Q:NEVER ,_))
                    (`(,(or 'Q:EQ 'Q:LEQ 'Q:GEQ)
                       ,(and gen (guard (memq gen generics)))
                       (pred et-type-p)))
                    (_ (error "Invalid constraint: %s" q))))

      (cl-flet ((genericp (var) (or (and (symbolp var) (memq var generics))
                                    (error "Not a generic: %s" var))))
        (dolist (case (et-matcher-dnf matcher))
          (dolist (factor case)
            (pcase factor
              (`(S:DT ,(and name (pred symbolp)) . ,args)
               (et--datatype-map-args
                name args
                (lambda (arg role)
                  (pcase role
                    ('CONST nil)
                    ((or 'CO 'CONTRA 'ISO) (make-et-matcher :generics generics :dnf arg))
                    (_ (error "Unknown role type: %s" role))))))
              (`(S:ALIAS ,(pred symbolp) . ,args)
               (dolist (arg args) (make-et-matcher :generics generics :dnf arg)))
              (`(S:GENERIC ,(pred genericp)))
              (`(S:SET ,_ ,(pred et-type-p)))
              (_ (error "Invalid match factor: %s" factor))))))))

  matcher)

(advice-add #'make-et-matcher :filter-return #'et--verify-matcher)


;;;; Expand matcher aliaes

(defun et--matcher-expand-aliases (matcher)
  (let ((generics (et-matcher-generics matcher))
        (dnf (et-matcher-dnf matcher)))
    (make-et-matcher :dnf (et--matcher-dnf-expand-aliases dnf generics)
                     :generics generics
                     :constraints (et-matcher-constraints matcher))))

(defun et--matcher-dnf-expand-aliases (dnf generics)
  (cl-loop for case in dnf
           nconc
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(S:ALIAS ,name . ,args)
                       (et--matcher-dnf-expand-aliases
                        (et--expand-alias-as-matcher-dnf name args generics)
                        generics))
                      (other (list (list other))))
                    into and-terms
                    finally return (apply #'et--dnf-and and-terms))))


;;;; Iso match

(defun et-iso-match (matcher type)
  (delete-dups (nconc (et--sub-constraints matcher type)
                      (et--super-constraints matcher type))))


;;;; Sub match

(defvar et--sub-constraints-stack nil
  "Stack of calls to `et--sub-constraints' with the form (MATCHER . TYPE).")

(defvar et--constraint-label nil
  "A cons cell (MATCHER-LABEL . TYPE-LABEL).

When evaluating `et--sub-constraints', when encountering a matcher with
a (S:LABEL) field, or a type with a non-nil :label, the corresponding
car/cdr of this variable will be set. Then, when a constraint is later
created, its label will be set to the value of this variable.")

(defun et--simplify-constraints (qs)
  (if (null (cdr qs)) qs
    (if-let* ((never (assq 'Q:NEVER qs)))
        (list never)
      (delete-dups qs))))

(defun et--never-constraint ()
  (list 'Q:NEVER et--constraint-label))

(defun et--sub-constraints (matcher type)
  (et--verify-matcher matcher)
  (et--verify-type type)

  (let* ((et--constraint-label (cons (car et--constraint-label)
                                     (or (et-type-label type) (cdr et--constraint-label)))))

    (et--stop-recursion et--sub-constraints-stack (cons matcher type) nil
      (setq matcher (et--matcher-expand-aliases matcher))

      (cl-loop for case in (et-type-cases type)
               for binds = (append (et-type-case-binds case)
                                   (cl-loop for var in (et-type-case-typeofs case)
                                            collect (cons var (et--remove-type-binds (et-type case)))))
               for qs-raw = (et--sub-constraints-2 matcher case)
               for qs-with-binds =
               (if (not binds) qs-raw
                 (cl-loop for q in qs-raw
                          if (eq (car q) 'Q:GEQ)
                          collect (list 'Q:GEQ (cadr q)
                                        (et--supersect (caddr q) (et--replace-type-binds (et-any) binds)))
                          else collect q))
               nconc qs-with-binds into result
               finally return (et--simplify-constraints result)))))

(defun et--sub-constraints-2 (matcher case)
  (cl-loop for match-case in (et-matcher-dnf matcher)
           for label = (cadr (assq 'S:MATCHER-LABEL match-case))
           for result-1 =
           (let* ((et--constraint-label (cons (or label (car et--constraint-label))
                                              (cdr et--constraint-label))))
             (cl-loop for match-factor in match-case
                      for gens = (et-matcher-generics matcher)
                      nconc (et--sub-or-super-constraints-3 match-factor case gens)))
           unless (assq 'Q:NEVER result-1)
           return result-1
           ;; If all cases failed, fallback to 2.2 or 2.3
           finally return
           (let ((val (et-type-case-value case)))
             (if (et-alias-p val)
                 (et--sub-constraints
                  matcher
                  (cl-loop with exp = (et-alias-expand val)
                           for c in (et-type-cases exp)
                           collect (make-et-type-case
                                    :value (et-type-case-value c)
                                    :binds (et-type-case-binds case)
                                    :typeofs (et-type-case-typeofs case))
                           into cases finally return (make-et-type :label (et-type-label exp) :cases cases)))
               ;; result-1 has the correct labels
               result-1))))

(defun et--sub-or-super-constraints-3 (match-factor case generics &optional is-super)
  (pcase match-factor
    (`(S:MATCHER-LABEL ,_) nil)
    (`(S:GENERIC ,var) (et-q ((,(if is-super 'Q:LEQ 'Q:GEQ) ,var ,(et-type case)))))
    (`(S:SET ,dnf ,type)
     (funcall (if is-super #'et--super-constraints #'et--sub-constraints)
              (make-et-matcher :dnf dnf :generics generics) type))
    (`(S:DT ,mdt-name . ,mdt-args)
     (pcase (et-type-case-value case)
       ((and alias (pred et-alias-p))
        (if (not is-super) (list (et--never-constraint))
          (et--super-constraints (make-et-matcher :generics generics :dnf (list (list match-factor)))
                                 (et-alias-expand alias))))
       ((and dt (pred et-datatype-p))
        (et--sub-or-super-constraints-4
         mdt-name mdt-args (et-datatype-name dt) (et-datatype-args dt)
         generics is-super))
       (_ (error "Unsupported matching datatype"))))
    (_ (error "Invalid match factor"))))

(defun et--sub-or-super-constraints-4 (m-name m-args t-name t-args generics &optional is-super)
  (cl-flet ((make-matcher (dnf) (make-et-matcher :dnf dnf :generics generics)))
    (if (not is-super)
        ;; subtype matching (super=MATCHER > sub=TYPE)
        (et--datatype-constraints
         t-name t-args m-name m-args
         (lambda (type dnf) (et--sub-constraints (make-matcher dnf) type))
         (lambda (type dnf) (et--super-constraints (make-matcher dnf) type))
         (lambda (type dnf) (et-iso-match (make-matcher dnf) type))
         (lambda (literal dnf) (et--sub-constraints (make-matcher dnf) (et-literal literal))))
      ;; supertype matching (sub=MATCHER < super=TYPE)
      (et--datatype-constraints
       m-name m-args t-name t-args
       (lambda (dnf type) (et--super-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et--sub-constraints (make-matcher dnf) type))
       (lambda (dnf type) (et-iso-match (make-matcher dnf) type))
       (lambda (literal type)
         (let ((literal-m (make-matcher (et-q (((S:DT Literal ,literal)))))))
           (et--super-constraints literal-m type)))))))


;;;; Super match

(defvar et--super-constraints-stack nil
  "Stack of calls to `et--super-constraints' with form (MATCHER . TYPE).")

(defun et--super-constraints (matcher type)
  (et--stop-recursion et--super-constraints-stack (cons matcher type) nil
    (et--verify-matcher matcher)
    (et--verify-type type)

    (setq matcher (et--matcher-expand-aliases matcher))

    (cl-loop for m-case in (et-matcher-dnf matcher)
             nconc (et--super-constraints-2 m-case type (et-matcher-generics matcher))
             into result
             finally return (et--simplify-constraints result))))

(defun et--super-constraints-2 (match-case type generics)
  (make-et-matcher :dnf (list match-case) :generics generics)
  (et--verify-type type)

  (or
   (pcase match-case
     ;; A single match factor, at that is a S:GENERIC match factor
     (`((S:GENERIC ,var)) (et-q ((Q:LEQ ,var ,type)))))

   (cl-loop for case in (et-type-cases type)
            for result =
            (cl-loop for match-factor in match-case
                     nconc (et--sub-or-super-constraints-3 match-factor case generics 'SUPER))
            unless (assq 'Q:NEVER result)
            return result
            ;; If all cases failed, return never
            finally return (list (et--never-constraint)))))


;;; ============================================================
;;; Structure
;;;; Documentation

[et
 (@alias EtTypeLabel Integer)
 (@alias EtMatcherLabel Integer)
 (@alias EtConstraintLabel (Cons EtMatcherLabel EtTypeLabel))
 (@alias EtConstraint
         (or (Tuple @Q:NEVER EtConstraintLabel)
             (Tuple @Q:EQ EtGeneric *et-type)
             (Tuple @Q:GEQ EtGeneric *et-type)
             (Tuple @Q:LEQ EtGeneric *et-type)))
 (@alias EtBothStructureFactor
         (or (TupleStar @S:DT EtDatatypeName List<Any>)
             (TupleStar @S:ALIAS EtAliasName List<*et-type>)
             (Tuple @S:GENERIC EtGeneric)))
 (@alias EtTypeStructureFactor
         (or (Tuple @S:TYPE-LABEL EtTypeLabel)
             (Tuple @S:TYPE *et-type)
             (Tuple @S:BIND NonNilSymbol EtTypeStructure)
             (Tuple @S:TYPEOF EtTypeStructure)
             (Tuple @S:BINDS-OF EtTypeStructure)
             (Tuple @S:SUBTRACT EtTypeStructure EtTypeStructure)
             (Tuple @S:INFER List<EtGeneric> EtMatcherStructure EtTypeStructure EtTypeStructure EtTypeStructure)
             (Tuple @S:EXTENDS EtTypeStructure EtTypeStructure EtTypeStructure EtTypeStructure)
             (TupleStar @S:EVAL Function<List<*et-type>~*et-type> List<*et-type>)))
 (@alias EtMatcherStructureFactor
         (or (Tuple @S:MATCHER-LABEL EtMatcherLabel)
             (Tuple @S:SET EtMatcherStructure EtTypeStructure)))
 (@alias EtTypeStructure
         (List (List (or EtBothStructureFactor EtMatcherStructureFactor))))
 (@alias EtMatcherStructure
         (List (List (or EtBothStructureFactor EtMatcherStructureFactor))))
 (@alias EtBothStructure (List (List EtBothStructureFactor)))
 (@alias EtRestrictedStructure [(<= R (or @TYPE @MATCHER @BOTH))]
         (and (extends? @TYPE R EtTypeStructure Never)
              (extends? @MATCHER R EtMatcherStructure Never)
              (extends? @BOTH R EtBothStructure Never)))
 ]

;; A structure is a general format that can be parsed to either a matcher
;; or a type. RESTRICT can be either `type' or `matcher' to ensure the
;; resulting structure is one or the other, and `both' to ensure that the
;; resulting structure is a valid type AND matcher. It is a list of cases,
;; each of which is a list of factors. A factor is one of the following:
;;
;; \(`S:DT' NAME ARGS...)
;; \(`S:ALIAS' NAME ARGS...)
;; \(`S:GENERIC' VAR) - In matchers, this compiles to a matcher generic. In
;;   types, you must provide replacements for each generic when parsing the
;;   structure to a type.
;;
;; Types only:
;; \(`S:TYPE' TYPE) - an already compiled type
;; \(`S:BIND' VAR TYPE)
;; \(`S:TYPEOF' VAR)
;; \(`S:BINDS-OF' TYPE)
;; \(`S:SUBTRACT' TYPE1 TYPE2)
;; \(`S:INFER' GENERICS MATCHER TYPE Y-RESULT N-RESULT)
;; \(`S:EXTENDS' SUB SUPER Y-RESULT N-RESULT)
;; \(`S:EVAL' FUNC TYPES...)
;;
;; Matchers only:
;; \(`S:MATCHER-DNF' LITERAL-MATCHER-DNF) - an already compiled matcher dnf
;; \(`S:SET' MATCHER TYPE)


;;;; Parsing

(defmacro et-struct (&rest args)
  `(et-parse-structure (et-q ,(if (eq (length args) 1) (car args) args))))

(defvar et--parsing-generics nil)
(defvar et--parsing-restriction nil)

(defun et--parse-struct (spec generics restrict)
  (let* ((et--parsing-generics generics)
         (et--parsing-restriction restrict))
    (et--parse-sub spec)))

(defun et--parse-sub (spec &optional extra-generics)
  (cl-assert (memq et--parsing-restriction '(TYPE MATCHER BOTH)))
  (let* ((et--parsing-generics (append extra-generics et--parsing-generics)))
    (cond
     ((and (consp spec) (symbolp (car spec)))
      (et--parse-spec-factor (car spec) (cdr spec)))
     ((et-type-p spec) spec)
     ((symbolp spec) (et--parse-string (symbol-name spec)))
     ((stringp spec) (et--parse-string spec))
     ((numberp spec) (et--parse-sub (list 'literal spec)))
     (t (error "Invalid spec: %s" spec)))))

(defun et--parse-spec-factor (name args)
  (if-let* ((handler (get name 'et-spec-parse)))
      (if (eq :full (car handler))
          (apply (cdr handler) args)
        ;; Check that it is a valid factor for the current restriction
        (pcase et--parsing-restriction
          ('TYPE (unless (get (car handler) 'et-struct-to-type)
                   (error "Invalid type structure: %s" name)))
          ('MATCHER (unless (memq (car handler) '(S:DT S:ALIAS S:GENERIC S:SET))
                      (error "Invalid matcher structure: %s" name))))
        ;; Base case: wrap a factor in a list of lists
        (list (list (cons (car handler) (apply (cdr handler) args)))))

    (if (memq name et--parsing-generics)
        (list (list (list 'S:GENERIC name)))

      (if (et--datatype-name? name)
          ;; Parse a datatype
          (et--parse-spec-factor 'dt (cons name args))

        (let* ((alias (or (get name 'et-alias) (error "Invalid type name: %s" name)))
               (alias-restrict (plist-get alias :restrict))
               (restrict et--parsing-restriction))
          ;; Ensure the restriction is valid
          (when (or (and (eq restrict 'TYPE) (eq alias-restrict 'MATCHER))
                    (and (eq restrict 'MATCHER) (eq alias-restrict 'TYPE)))
            (error "Parsing %s, but alias %s only supports %s"
                   (downcase (symbol-name restrict)) name (downcase (symbol-name alias-restrict))))
          ;; Parse an alias
          (et--parse-spec-factor 'alias (cons name args)))))))


;;;; Parse string

(defvar et--test-variables
  (list (cons '$a (make-et-var :name '$a :type (et-dt 'Any)))
        (cons '$b (make-et-var :name '$b :type (et-dt 'Any)))
        (cons '$c (make-et-var :name '$c :type (et-dt 'Any)))))

(defun et--parse-string (s)
  (when (string-empty-p s) (error "Empty type expression"))

  (cl-loop for or-seg in (et--split-at-depth s ?|)
           when (string-empty-p or-seg)
           do (error "Empty segment in union type: %s" s)
           collect
           (cl-loop for and-seg in (et--split-at-depth or-seg ?&)
                    when (string-empty-p and-seg)
                    do (error "Empty segment in intersection type: %s" s)
                    collect (et--parse-atom and-seg) into and-parts
                    finally return (cl-reduce #'et--dnf-and and-parts))
           into or-parts
           finally return (apply #'nconc or-parts)))

(defun et--parse-atom (s)
  "Parse a single type atom into an `et-type'."
  (cond
   ;; Literal number
   ((string-match "^[0-9]+\\(\\.[0-9]+\\)?$" s)
    (et--parse-sub (list 'literal (string-to-number s)) nil))

   ;; Parenthesized expression
   ((string-match "^{\\(.*\\)}$" s)
    (et--parse-string (substring s 1 -1)))

   ;; @symbol  ->  Literal symbol
   ((string-match "^@\\(.*\\)$" s)
    (et--parse-sub (list 'literal (intern (match-string 1 s)))))

   ;; %string  ->  Literal string
   ((string-match "^%\\(.*\\)$" s)
    (et--parse-sub (list 'literal (match-string 1 s))))

   ;; $TestVar=Type  ->  Bind to TestVar
   ((string-match "^\\(\\$[a-z]\\)::\\(.*\\)$" s)
    (et--parse-sub
     (list 'bind (or (alist-get (intern (match-string 1 s)) et--test-variables)
                     (error "Invalid test variable: %s" (match-string 1 s)))
           (match-string 2 s))))

   ;; ::$TestVar  ->  Typeof TestVar
   ((string-match "^::\\(\\$[a-z]\\)$" s)
    (et--parse-sub
     (list 'typeof (or (alist-get (intern (match-string 1 s)) et--test-variables)
                       (error "Invalid test variable: %s" (match-string 1 s))))))

   ;; Var=Type  ->  Matcher set
   ((string-match "^\\([-a-zA-Z0-9]*\\)=\\(.*\\)$" s)
    (et--parse-sub (list 'set (intern (match-string 1 s)) (match-string 2 s))))

   ;; Name or Name<...> or *struct or *struct<...>
   ((string-match "^\\*?\\([-a-zA-Z0-9:]+\\)\\(?:<\\(.*\\)>\\)?$" s)
    (let* ((is-struct (string-match-p "^\\*" s))
           (name (intern (match-string 1 s)))
           (inner (match-string 2 s))
           (arg-strs (when inner (et--split-at-depth inner ?~))))

      ;; Force the arg name to get parsed as a constant symbol
      (when is-struct
        (push name arg-strs)
        (setq name 'Struct))

      (cl-loop for s in arg-strs
               for role in (if (et--datatype-name? name)
                               (et--datatype-arg-roles name arg-strs)
                             (make-list (length arg-strs) nil))
               collect (if (not (eq role 'CONST)) s
                         (cond ((symbolp s) s) ; struct name
                               ((string-match-p ":.*" s) (intern s))
                               ((string-match-p "@.*" s) (intern (substring s 1)))
                               ((string-match-p "%.*" s) (substring s 1))
                               ((string-match-p "[0-9]+\\(\\.[0-9]+\\)?" s)
                                (string-to-number s))
                               (t (error "Invalid constant format: %s" s))))
               into args
               finally return (et--parse-sub (cons name args)))))

   (t (error "Invalid parse syntax: %s" s))))


(defun et--split-at-depth (s delim)
  "Split string S on character DELIM at depth 0 only.
Depth tracks < > and { } nesting."
  (let ((depth 0) (start 0) (result '()))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (cond ((memq c '(?< ?{)) (cl-incf depth))
              ((memq c '(?> ?})) (cl-decf depth))
              ((and (eq c delim) (= depth 0))
               (push (substring s start i) result)
               (setq start (1+ i))))))
    (push (substring s start) result)
    (nreverse result)))


;;;; Struct to type

(defvar et--totype-gen-repls nil)

(defun et--totype-sub (structure)
  (if (et-type-p structure) structure
    (cl-loop for factors in structure
             collect
             (cl-loop for (name . args) in factors
                      for totype = (or (get name 'et-struct-to-type)
                                       (error "Invalid type structure: %s" name))
                      for out = (apply totype args)
                      for type = (cond ((et-type-p out) out)
                                       ((et-type-case-p out) (make-et-type :cases (list out)))
                                       (t (make-et-type :cases out)))
                      collect type into and-types
                      finally return (apply #'et--supersect and-types))
             into or-types
             finally return (apply #'et--or or-types))))

(defun et-structure-to-type (structure &optional gen-repls)
  "Convert STRUCTURE to an `et-type'.

GEN-REPLS is an alist of symbols to `et-type's. Each time S:GENERIC
appears in STRUCTURE, it will be replaced with the corresponding value
in GEN-REPLS, if it exists.

This function will not succeed for all structures. Structures can
represent multiple types in a single case, but types do not support
this, so it will fail in this case.

Also, there are certain structure types that are designed for matchers,
which are invalid for types."
  (let* ((et--totype-gen-repls gen-repls))
    (et--totype-sub structure)))


;;;; Replacement for matchers

(defun et--structure-substitute-generics (structure gen-repls)
  (declare (et (structure EtMatcherStructure)
               (gen-repls AList<EtGeneric~EtMatcherStructure>)))

  (cl-loop with sub = (lambda (s) (et--structure-substitute-generics s gen-repls))
           for case in structure
           nconc
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(S:GENERIC ,var)
                       ;; Don't use alist-get, because the value of the replacement can be nil
                       (if-let* ((entry (assq var gen-repls))) (cdr entry)
                         (error "Replacement for %s not provided" var)))
                      (`(S:DT ,name . ,args)
                       (et-q (((S:DT ,name . ,(et--datatype-map-type-args name args sub))))))
                      (`(S:ALIAS ,name . ,args)
                       (et-q (((S:ALIAS ,name . ,(mapcar sub args))))))
                      ;; Maybe at some point this should also be called on type?
                      ;; But that means every type matcher needs a substitution function
                      (`(S:SET ,matcher ,type)
                       (et-q (((S:SET ,(funcall sub matcher) ,type)))))
                      (_ (error "Invalid matcher structure factor: %s" factor)))
                    into and-structs
                    finally return (apply #'et--dnf-and and-structs))))


;;;; Printing

(defun et--print-sub (structure)
  (cl-loop for factors in structure
           collect
           (cl-loop for (name . args) in factors
                    for print = (or (get name 'et-struct-print)
                                    (error "Invalid structure: %s" name))
                    collect (apply print args) into and-strings
                    finally return
                    (if and-strings (string-join and-strings " & ") "Any"))
           into or-strings
           finally return (if or-strings (string-join or-strings " | ") "Never")))

(defun et--print-sub-named (name args)
  (pcase (cons name args)
    (`(Literal ,val)
     (format "`%s'" (prin1-to-string val)))

    (`(Struct ,name . ,args)
     (if (null args) (format "*%s" name)
       (format "*%s<%s>" name (string-join (mapcar #'et--print-sub args) " "))))

    ((or `(ConsFull ,left-sub ,_1 ,right-sub ,_2)
         `(,(or 'ConsR 'ConsW 'ConsRW 'ConsWR) ,left-sub ,right-sub))
     (let ((elems (list (et--print-sub left-sub))))
       (while (pcase right-sub
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     ((or `(S:DT ConsFull ,car-sub ,_1 ,cdr-sub ,_2)
                          `(S:ALIAS ,(or 'ConsR 'ConsW 'ConsRW 'ConsWR) ,car-sub ,cdr-sub))
                      (nconc elems (list (et--print-sub car-sub)))
                      (setq right-sub cdr-sub)
                      t))))))
       (let ((tail-nil-p
              (and (= (length right-sub) 1)
                   (= (length (car right-sub)) 1)
                   (equal (car (car right-sub)) '(S:DT Literal nil)))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et--print-sub right-sub))))))

    (`(DynFunction ,matcher ,output-type)
     (format "(%s) -> %s" (et-pp-matcher matcher) (et--print-sub output-type)))

    (_
     (let* ((name-str (symbol-name name))
            (strs (if (not (et--datatype-name? name))
                      (mapcar #'et--print-sub args)
                    (et--datatype-map-args
                     name args
                     (lambda (arg role)
                       (if (eq role 'CONST) (format "%s" arg)
                         (et--print-sub arg)))))))
       (if (null args) name-str
         (format "%s<%s>" name-str (string-join strs ", ")))))))


;;;; Segment Macros

(defmacro et--define-struct-segment (struct-sym spec-sym arglist &rest plist)
  (declare (indent 3))
  (let* ((parse (or (plist-get plist :parse) (error "No :parse field provided")))
         (print (or (plist-get plist :print) (error "No :print field provided")))
         (totype (plist-get plist :to-type))
         (ignore (cons #'ignore (remq '&optional (remq '&rest arglist)))))
    `(progn
       (put ',spec-sym 'et-spec-parse (cons ',struct-sym (lambda ,arglist ,ignore ,parse)))
       (put ',struct-sym 'et-struct-print (lambda ,arglist ,ignore ,print))
       ,@(when totype `((put ',struct-sym 'et-struct-to-type (lambda ,arglist ,ignore ,totype)))))))

(defmacro et--define-spec-segment (spec-sym struct-sym arglist body)
  "Define how a spec form is parsed into a structure."
  (declare (indent 3))
  `(put ',spec-sym 'et-spec-parse
        (cons ',struct-sym (lambda ,arglist ,body))))


;;;; Spec segments

(et--define-spec-segment literal S:DT (val) (list 'Literal val))
(et--define-spec-segment Any S:DT () (list 'Any))
(et--define-spec-segment Nil S:DT () (list 'Literal nil))
(et--define-spec-segment True S:DT () (list 'Literal t))

(et--define-spec-segment Never :full () nil)
(et--define-spec-segment or :full (&rest args)
  (apply #'nconc (mapcar #'et--parse-sub args)))
(et--define-spec-segment and :full (&rest args)
  (apply #'et--dnf-and (mapcar #'et--parse-sub args)))

(et--define-struct-segment S:TYPE-LABEL type-label (label)
  :parse (list label)
  :to-type (list)
  :print (format "t@%s" label))

(et--define-struct-segment S:MATCHER-LABEL matcher-label (label)
  :parse (list label)
  :print (format "m@%s" label))

(et--define-struct-segment S:BIND bind (var type)
  :parse (list var (et--parse-sub type))
  :to-type (list (make-et-type-case
                  :value (make-et-datatype :name 'Any)
                  :binds (list (cons var (et--totype-sub type)))))
  :print (format "{%s : %s}" (et-var-name var) (et--print-sub type)))

(et--define-struct-segment S:TYPEOF typeof (var)
  :parse (list var)
  :to-type (list (make-et-type-case :value (make-et-datatype :name 'Any) :typeofs (list var)))
  :print (format "{typeof %s}" (et-var-name var)))

(et--define-struct-segment S:BINDS-OF bindsof (type)
  :parse (list (et--parse-sub type))
  :to-type (let* ((type (et--totype-sub type)))
             (if (et-never-p type) nil
               (list (make-et-type-case
                      :value (make-et-datatype :name 'Any)
                      :binds (et--type-binds type)))))
  :print (format "{bindsof %s}" (et--print-sub type)))

(et--define-struct-segment S:SUBTRACT subtract (a b)
  :parse (list (et--parse-sub a) (et--parse-sub b))
  :to-type (et-type-cases (et--subtract (et--totype-sub a) (et--totype-sub b)))
  :print (format "{%s - %s}" (et--print-sub a) (et--print-sub b)))

(et--define-struct-segment S:INFER infer (type gen-vec matcher yes no)
  :parse
  (let* ((generics (if (vectorp gen-vec) (append gen-vec nil)
                     (error "Generics must be a vector: %s" gen-vec))))
    (list (et--parse-sub type)
          generics
          (make-et-matcher
           :generics generics
           :dnf (et--parse-struct matcher generics 'MATCHER))
          (et--parse-sub yes generics)
          (et--parse-sub no)))
  :to-type
  (et-type-cases (or (et--infer matcher (et--totype-sub type) yes
                                et--totype-gen-repls)
                     (et--totype-sub no)))
  :print
  (format "{if %s matches %s then %s else %s}"
          (et--print-sub type) (et-pp-matcher matcher)
          (et--print-sub yes) (et--print-sub no)))

(et--define-struct-segment S:EXTENDS extends? (sub super yes no)
  :parse (list (et--parse-sub sub) (et--parse-sub super) (et--parse-sub yes) (et--parse-sub no))
  :to-type (et-type-cases
            (if (et-subtype? (et--totype-sub sub) (et--totype-sub super))
                (et--totype-sub yes) (et--totype-sub no)))
  :print (format "{if %s extends %s then %s else %s}"
                 (et--print-sub sub) (et--print-sub super)
                 (et--print-sub yes) (et--print-sub no)))

(et--define-struct-segment S:EVAL eval (func &rest args)
  :parse (cons func (mapcar #'et--parse-sub args))
  :to-type (et-type-cases (apply func (mapcar #'et--totype-sub args)))
  :print (format "{eval %s on %s}" func (mapconcat #'et--print-sub args " ")))

(et--define-struct-segment S:GENERIC generic (var)
  :parse (if (memq var et--parsing-generics) (list var) (error "Generic %s not defined" var))
  :to-type (or (alist-get var et--totype-gen-repls) (error "Generic %s not defined" var))
  :print (format "@%s" var))

(et--define-struct-segment S:SET set (dnf type)
  :parse (list (et--parse-sub dnf) (et-parse-type type))
  :print (format "{match %s to %s}" (et--print-sub dnf) (et--print-sub type)))

(et--define-struct-segment S:DT dt (name &rest args)
  :parse (cons name (et--datatype-map-type-args name args #'et--parse-sub))
  :to-type
  (let* ((new-args (et--datatype-map-type-args name args #'et--totype-sub)))
    (list (make-et-type-case :value (make-et-datatype :name name :args new-args))))
  :print (et--print-sub-named name args))

(et--define-struct-segment S:ALIAS alias (name &rest args)
  :parse
  (let* ((plist (get name 'et-alias))
         (_ (or plist (error "Not an alias: %s" name)))
         (gen-ct (length (plist-get plist :generics)) )
         (_ (or (plist-get plist :custom)
                (eq (length args) gen-ct)
                (error "Alias %s requires %s arguments, got %s" name gen-ct (length args)))))
    (cons name (mapcar #'et--parse-sub args)))
  :to-type
  (let* ((new-args (mapcar #'et--totype-sub args)))
    (list (make-et-type-case :value (make-et-alias :name name :args new-args))))
  :print (et--print-sub-named name args))


;;;; Type to struct

(defun et-type-to-structure (type)
  "Convert an `et-type' to a structure DNF."
  (et--verify-type type)
  (cl-loop for case in (et-type-cases type)
           for value = (et-type-case-value case)
           collect
           (cons
            (pcase value
              ((cl-struct et-datatype name args)
               (et-ql S:DT ,(et-datatype-name value)
                      ,@(et--datatype-map-type-args name args #'et-type-to-structure)))
              ((cl-struct et-alias name args)
               (et-ql S:ALIAS ,name ,@(mapcar #'et-type-to-structure args)))
              (_ (error "Unsupported type case value: %s" value)))

            (nconc
             (cl-loop for (var . type) in (et-type-case-binds case)
                      collect (et-ql S:BIND ,var ,(et-type-to-structure type)))
             (cl-loop for var in (et-type-case-typeofs case)
                      collect (et-ql S:TYPEOF ,var))
             (when (et-type-label type)
               (list (et-ql S:TYPE-LABEL ,(et-type-label type))))))))


;;;; Parse/print type

(defun et-parse-type (spec)
  "Parse SPEC as an `et-type'."
  (declare (et (spec Any)
               (@return *et-type)
               (@skip)))

  (et-structure-to-type (et--parse-struct spec nil 'TYPE)))

(defmacro et (&rest args)
  `(et-parse-type (et-q ,(if (eq (length args) 1) (car args) args))))

(defun et-pp-type (type)
  (or (ignore-errors
        (format "%s%s" (if (et-type-label type) (format "%s:" (et-type-label type)) "")
                (et--print-sub (et-type-to-structure type))))
      (format "%s" type)))

(cl-defmethod cl-print-object ((type et-type) stream)
  (princ (et-pp-type type) stream))

(defun et-pp (arg)
  (if (stringp arg) arg (cl-prin1-to-string arg)))



(et-test
 (equal (et Cons<1~@abc>)
        (et-type (make-et-alias :name 'Cons :args (list (et-literal 1) (et-literal 'abc)))))

 (equal (et Number)
        (et-type (make-et-datatype :name 'Number)))

 (equal (et {$a::5}&4)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Literal :args (list 4))
                  :binds (list (cons (alist-get '$a et--test-variables) (et-literal 5))))))

 (equal (et {::$a}&{$b::6}&Integer)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Integer)
                  :binds (list (cons (alist-get '$b et--test-variables) (et-literal 6)))
                  :typeofs (list (alist-get '$a et--test-variables)))))

 (equal (et bindsof<{::$a}&{$a::2|3}&{1|2}>)
        (et-type (make-et-type-case
                  :value (make-et-datatype :name 'Any)
                  :binds (list (cons (alist-get '$a et--test-variables) (et-literal 2))))))

 ;; Test that bindsof never is never
 (equal (et Never) (et bindsof (and Integer&{::$a} String)))

 ;; Test when a predicate is redundant (both directions)
 (et-subtype? (et or (and True (bindsof (and Integer&{::$a} String)))
                  (and Nil (bindsof (subtract Integer&{::$a} String))))
              (et Nil))
 (et-subtype? (et or (and True (bindsof (and Integer&{::$a} Number)))
                  (and Nil (bindsof (subtract Integer&{::$a} Number))))
              (et True))

 ;; Test infer
 (equal (et Never)
        (et infer ConsR<%hi~%hi> [T] ConsR<T&Integer~String> VectorR<T> Never))
 (equal (et VectorR<12>)
        (et infer ConsR<12~%hi> [T] ConsR<T&Integer~String> VectorR<T> Never))
 (equal (et VectorR<Integer>)
        (et infer ConsR<Integer~%hi> [T] ConsR<T&Integer~String> VectorR<T> Never))
 (equal (et Never)
        (et infer ConsR<Number~%hi> [T] ConsR<T&Integer~String> VectorR<T> Never))
 (equal (et VectorR<12>)
        (et infer ListR<12> [T] ListR<T&Integer> VectorR<T> Never))
 (equal (et VectorR<1|2>)
        (et infer ConsR<1~ConsR<2~Nil>> [T] ListR<T&Integer> VectorR<T> Never))
 (equal (et VectorR<1|2|3>)
        (et infer ConsR<1~ConsR<2~ListR<3>>> [T] ListR<T&Integer> VectorR<T> Never))
 (equal (et Never)
        (et infer ListR<Number> [T] ListR<T&Integer> VectorR<T> Never))
 (equal (et VectorR<Never>)
        (et infer Nil [T] ListR<T&Integer> VectorR<T> Never))

 ;; Test constant args
 (equal (et PList<%hello~%hi~@hello~@hi~:hello~@:hi~123~4.56>)
        (et-dt 'PList
               "hello" (et-literal "hi")
               'hello (et-literal 'hi)
               :hello (et-literal :hi)
               123 (et-literal 4.56))))


;;;; Parse/print matcher

(defmacro et-matcher (gen-vec &rest args)
  (declare (indent 1))
  (or (vectorp gen-vec) (error "Write the generics as a vector"))
  `(et-parse-matcher (et-q ,(if (eq (length args) 1) (car args) args))
                     ,gen-vec))

(defun et-parse-matcher (spec gen-vec)
  "Parse SPEC as an `et-matcher' with GEN-VEC.

GEN-VEC is a list of symbols and constraint specs. A constraint
spec is one of:
  (= GEN TYPE-SPEC)
  (>= GEN TYPE-SPEC)
  (<= GEN TYPE-SPEC)

A generic can be implicitly defined by just providing a constraint
involving that generic. For example, the spec [(<= T Number)] is the
same as [T (<= T Number)]."
  (let* ((generics (et--gen-vec-generics gen-vec)))
    (make-et-matcher
     :generics generics
     :constraints (et--gen-vec-constraints gen-vec)
     :dnf (et--parse-struct spec generics 'MATCHER))))

(defun et-pp-matcher (matcher)
  "Format an `et-matcher' into a human-readable string."
  (let* ((generics (et-matcher-generics matcher))
         (body (et--print-sub (et-matcher-dnf matcher))))
    (format "[%s] %s" (mapconcat #'symbol-name generics " ") body)))

(cl-defmethod cl-print-object ((matcher et-matcher) stream)
  (princ (format "#<%s>" (et-pp-matcher matcher)) stream))


;;; ============================================================
;;; Type features
;;;; Union

(defun et--or (&rest types)
  "Return the exact type union of TYPES."
  (mapc #'et--verify-type types)

  (cl-loop with label = nil
           for type in types
           when (et-type-label type) do (setq label (et-type-label type))
           nconc (apply #'list (et-type-cases type)) into cases
           finally return (make-et-type :label label :cases cases)))


;;;; Subtype

(defun et-datatype-subtype? (sub super)
  (cl-assert (et-datatype-p sub))
  (cl-assert (et-datatype-p super))

  (cl-flet ((valid-if (valid) (if valid nil (list (et--never-constraint)))))
    (let ((constraints
           (et--datatype-constraints
            (et-datatype-name sub) (et-datatype-args sub)
            (et-datatype-name super) (et-datatype-args super)
            (lambda (a b) (valid-if (et-subtype? a b)))
            (lambda (a b) (valid-if (et-subtype? b a)))
            (lambda (a b) (valid-if (and (et-subtype? a b) (et-subtype? b a))))
            (lambda (literal b) (valid-if (and (et-subtype? (et-literal literal) b)))))))

      (not (assq 'Q:NEVER constraints)))))

(defun et--binds-subtype? (sub-binds super-binds)
  (cl-loop for (var . super-type) in super-binds
           for sub-type = (alist-get var sub-binds)
           always (and sub-type (et-subtype? sub-type super-type))))

(defun et--case-subtype? (sub super)
  (cl-assert (et-type-case-p sub))
  (cl-assert (et-type-case-p super))

  (and (cl-loop with sub-vars = (et-type-case-typeofs sub)
                for super-var in (et-type-case-typeofs super)
                always (memq super-var sub-vars))
       ;; Macro expansion in `et-subtype?' means that the value should always be a datatype
       (et-datatype-subtype? (et-type-case-value sub) (et-type-case-value super))
       (et--binds-subtype? (et-type-case-binds sub) (et-type-case-binds super))))

;; This function could just use `et-sub-match', but it needs to take
;; into account binds.

(defvar et--subtype-stack nil
  "Stack of calls to `et-subtype?' with the form (SUB-TYPE . SUPER-TYPE).")

(defun et-subtype? (sub super)
  (et--verify-type sub)
  (et--verify-type super)

  (if (equal sub super) t ; Not strictly necessary, but improves efficiency

    (et--stop-recursion et--subtype-stack (cons sub super) t

      (setq sub (et-expand-all-aliases sub))
      (setq super (et-expand-all-aliases super))

      (cl-loop for sub-case in (et-type-cases sub)
               always
               (cl-loop for super-case in (et-type-cases super)
                        thereis
                        (et--case-subtype? sub-case super-case))))))

(et-test
 (et-subtype? (et Integer) (et Number))
 (et-subtype? (et Integer) (et Any))

 ;; Check that ConsR has covariant arguments
 (et-subtype? (et ConsR<Integer~Integer>) (et ConsR<Number~Number>))
 (not (et-subtype? (et ConsR<Number~Number>) (et ConsR<Integer~Integer>)))
 ;; Check that ConsW has contravariant arguments
 (et-subtype? (et ConsW<Number~Number>) (et ConsW<Integer~Integer>))
 (not (et-subtype? (et ConsW<Integer~Integer>) (et ConsW<Number~Number>)))
 ;; Check that ConsWR/RW have alternating arguments
 (et-subtype? (et ConsRW<Integer~Number>) (et ConsRW<Number~Integer>))
 (not (et-subtype? (et ConsRW<Integer~Number>) (et ConsRW<1~Integer>)))
 (et-subtype? (et ConsWR<Number~Integer>) (et ConsWR<Integer~Number>))
 (not (et-subtype? (et ConsWR<Number~Integer>) (et ConsWR<Integer~1>)))
 ;; Cons (no arguments) is a supertype of everything
 (et-subtype? (et ConsR<1~2>) (et Cons))
 (et-subtype? (et ConsW<1~2>) (et Cons))
 (et-subtype? (et ConsRW<1~2>) (et Cons))
 (et-subtype? (et ConsWR<1~2>) (et Cons))
 ;; Cons with arguments is a subtype of all
 (et-subtype? (et Cons<1~2>) (et ConsR<Integer~Integer>))
 (not (et-subtype? (et Cons<Integer~Integer>) (et ConsR<1~2>)))
 (et-subtype? (et Cons<Integer~Integer>) (et ConsW<1~2>))
 (not (et-subtype? (et Cons<1~2>) (et ConsW<Integer~Integer>)))

 (et-subtype? (et Literal (4 . 5)) (et ConsR<Integer~Integer>))
 (et-subtype? (et Literal (4 . 5)) (et Cons<Integer~Integer>))
 (not (et-subtype? (et Literal (4 . 5.5)) (et ConsR<Integer~Integer>)))
 (et-subtype? (et Literal (4 . 5)) (et Cons))

 (et-subtype? (et Literal [4 5 6]) (et VectorR<Integer>))
 (et-subtype? (et Literal [4 5 6]) (et Vector<Integer>))
 (not (et-subtype? (et Literal [4 5 6.6]) (et VectorR<Integer>)))
 (et-subtype? (et Literal [4 5 6]) (et Vector))

 (et-subtype? (et ListR<Integer>)
              (et Nil|ConsR<Number~ListR<Integer>>))

 ;; Check function subtypes
 (et-subtype? (et Function Integer Integer) (et Function Integer Integer))
 (et-subtype? (et Function Number Integer) (et Function Integer Number))

 ;; Check recursive subtypes
 (et-subtype? (et ListR<Integer>) (et ListR<Number>))
 (et-subtype? (et Cons<1~Cons<2~List<3>>>) (et ListR<Number>))
 (not (et-subtype? (et ListR<Number>) (et ListR<Integer>)))

 ;; Check cons is plist
 ;; ConsFull representing (:a 1 :b 2) is a subtype of PList<:a Integer :b Integer>
 (et-subtype? (et TupleR @:b %hi @:c 5 @:a 1 @:b 4)
              (et PList<:a~Integer~:b~String>))

 ;; Wrong value type: (:a "hi" :b 2) is NOT a subtype of PList<:a Integer :b Integer>
 (not (et-subtype? (et ConsR<@:a~ConsR<%hi~ConsR<@:b~ConsR<2~Nil>>>>)
                   (et PList<:a~Integer~:b~Integer>))))


;;;; Simplify

(defun et-simplify-type (type)
  (et--verify-type type)

  (cl-loop for (case . rest) on (et-type-cases type)
           ;; Check if this case is redundant
           unless (cl-loop for c in (append new-cases rest)
                           ;; case is a subtype of c, so case is redundant
                           thereis
                           (and (et-subtype? (make-et-type :cases (list case))
                                             (make-et-type :cases (list c)))))

           collect case into new-cases
           finally return (make-et-type :label (et-type-label type) :cases new-cases)))


;;;; Intersection

;; `et--subsect' generates a type which is a subset of ALL of the
;; provided types. In other words, it will approximate a smaller type
;; (i.e. never)

;; Thus, if v is (et--subsect A B), then v is both A and B, but but v
;; could be A and B but NOT (et--subsect A B).

;; This makes `et--subsect' useful for satisfying a list of subtype
;; constraints.

;; `et--supersect' generates a type which is a SUPERSET of the
;; intersection of the provided types. It will approximate by choosing
;; one of the types to use as the innacurate intersection.

;; Thus, if v is both A and B, then it is (et--supersect A B), but v
;; could be (et--supersect A B) but NOT be A or NOT be B.

;; This makes `et--supersect' useful for type narrowing, because unlike
;; `et--subsect', it will never make the type TOO narrow.

;; For example,
;; (et--subsect Integer Positive) -> Never
;; (et--supersect Integer Positive) -> Integer

(defun et--subsect (&rest types) (apply #'et--intersect t types))
(defun et--supersect (&rest types) (apply #'et--intersect nil types))
(defun et--non-nil (type) (et--supersect type (et NonNil)))

(et-test
 (equal (et-never) (et--subsect (et Integer) (et Positive)))
 (equal (et Integer) (et--supersect (et Integer) (et Positive))))

(defun et--intersect (subsect? &rest types)
  "Return the type intersection of TYPES."
  (mapc #'et--verify-type types)

  (pcase types
    ('nil (et-any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et--intersect subsect? a (apply #'et--intersect subsect? b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in (et-type-cases a)
              nconc
              (cl-loop for b-case in (et-type-cases b)
                       nconc (et--intersect-cases subsect? a-case b-case))
              into all-cases
              finally return
              (let ((result (make-et-type :cases all-cases)))
                ;; This assertion should pass if there are no bugs
                (when (and et-debug subsect?)
                  (or (and (et-subtype? result a) (et-subtype? result b))
                      (error "`et--subsect' determined incorrect intersection")))
                result)))))

(defun et--intersect-binds (subsect? a-binds b-binds)
  "Create a list of binds which are a subtype of both A-BINDS and B-BINDS."

  (cl-loop for (var . binds) in (seq-group-by #'car (append a-binds b-binds))
           collect (cons var (apply #'et--intersect subsect? (mapcar #'cdr binds)))))

(defun et--intersect-cases (subsect? a-case b-case)
  "Return a list of cases resulting from intersecting A-CASE and B-CASE."
  (cl-assert (et-type-case-p a-case))
  (cl-assert (et-type-case-p b-case))

  (let* ((a (et-type-case-value a-case))
         (b (et-type-case-value b-case))
         (a-case-raw (make-et-type-case :value a))
         (b-case-raw (make-et-type-case :value b))
         ;; Check if one of the values is a subtype of the other
         (sub-val (cond ((et-subtype? (et-type a-case-raw) (et-type b-case-raw)) a)
                        ((et-subtype? (et-type b-case-raw) (et-type a-case-raw)) b)))
         (make-case
          (lambda (val)
            (make-et-type-case
             :value val
             :binds (et--intersect-binds subsect? (et-type-case-binds a-case) (et-type-case-binds b-case))
             :typeofs (delete-dups (append (et-type-case-typeofs a-case)
                                           (et-type-case-typeofs b-case)
                                           nil))))))

    (cond
     (sub-val (list (funcall make-case sub-val)))

     ((et-alias-p a)
      (cl-loop for exp-case in (et-type-cases (et-alias-expand a))
               ;; Carry forward binds/typeofs from the original a-case
               for merged-case = (make-et-type-case
                                  :value (et-type-case-value exp-case)
                                  :binds (append (et-type-case-binds a-case)
                                                 (et-type-case-binds exp-case))
                                  :typeofs (append (et-type-case-typeofs a-case)
                                                   (et-type-case-typeofs exp-case)))
               nconc (et--intersect-cases subsect? merged-case b-case)))
     ((et-alias-p b)
      (cl-loop for exp-case in (et-type-cases (et-alias-expand b))
               for merged-case = (make-et-type-case
                                  :value (et-type-case-value exp-case)
                                  :binds (append (et-type-case-binds b-case)
                                                 (et-type-case-binds exp-case))
                                  :typeofs (append (et-type-case-typeofs b-case)
                                                   (et-type-case-typeofs exp-case)))
               nconc (et--intersect-cases subsect? a-case merged-case)))

     ((and (et-datatype-p a) (et-datatype-p b))
      (let ((dt (et--intersect-datatypes subsect? a b)))
        (if (not (eq dt 'INVALID)) (list (funcall make-case dt))
          ;; This is where subsect and supersect differ
          (if subsect? nil
            (if (et--datatype-might-overlap-nontrivial? a b)
                (list (funcall make-case a))
              nil)))))

     (t (error "Invalid type values")))))

(defun et--intersect-datatypes (subsect? a b)
  "Returns the datatype resulting from intersecting A and B, or `INVALID'."

  (cl-assert (et-datatype-p a))
  (cl-assert (et-datatype-p b))

  (let* ((a-name (et-datatype-name a))
         (b-name (et-datatype-name b))
         (a-args (et-datatype-args a))
         (b-args (et-datatype-args b)))
    (cond
     ((et-datatype-subtype? a b) a)
     ((et-datatype-subtype? b a) b)

     ((eq a-name b-name)
      (let ((arg-intersection
             (et--datatype-intersect-args-nontrivial
              a-name a-args b-args
              (lambda (a b) (et--intersect subsect? a b)) #'et--or)))

        (if (eq arg-intersection 'INVALID) 'INVALID
          (make-et-datatype :name a-name :args arg-intersection))))

     (t 'INVALID))))


;;;; Subtract

(defun et--subtract (a b)
  "Subtract type A from B.

This function errs on the side of subtracting less. In other words,
returning A itself is a valid approximation."
  (et--verify-type a)
  (et--verify-type b)

  (cl-loop for a-case in (et-type-cases a)
           nconc
           (cl-loop for b-case in (et-type-cases b)
                    nconc (et--subtract-cases a-case b-case))
           into all-cases
           finally return
           (let ((result (make-et-type :cases all-cases)))
             result)))

(defun et--subtract-binds (a-binds b-binds)
  (cl-loop for (var . a-type) in a-binds
           for b-type = (alist-get var b-binds)
           collect (cons var (if b-type (et--subtract a-type b-type) a-type))))

(defun et--subtract-cases (a-case b-case)
  (cl-assert (et-type-case-p a-case))
  (cl-assert (et-type-case-p b-case))

  (let* ((a (et-type-case-value a-case))
         (b (et-type-case-value b-case))
         (a-case-raw (make-et-type-case :value a))
         (b-case-raw (make-et-type-case :value b))
         (make-case
          (lambda (val)
            (make-et-type-case
             :value val
             :binds (et--subtract-binds (et-type-case-binds a-case) (et-type-case-binds b-case))
             :typeofs (et-type-case-typeofs a-case)))))

    (cond
     ;; Subtracting gives never
     ((et-subtype? (et-type a-case-raw) (et-type b-case-raw)) nil)

     ((et-alias-p a) (cl-loop for exp-case in (et-type-cases (et-alias-expand a))
                              nconc (et--subtract-cases exp-case b-case)))
     ((et-alias-p b) (cl-loop for exp-case in (et-type-cases (et-alias-expand b))
                              nconc (et--subtract-cases a-case exp-case)))

     ;; Todo: Handle more complex cases
     (t (list (funcall make-case a))))))

(et-test
 (equal (et 1|3) (et--subtract (et 1|2|3) (et 2)))
 (equal (et-never) (et--subtract (et Integer) (et Number)))
 (equal (et Number) (et--subtract (et Number) (et Integer)))
 (equal (et String|Number) (et--subtract (et String|Number) (et Integer)))
 (equal (et NonNil) (et--subtract (et NonNil) (et Integer))))


;;;; Satisfy constraints

(defun et--type-contains-binds (type)
  (cl-loop for c in (et-type-cases type)
           thereis (or (et-type-case-binds c) (et-type-case-typeofs c)
                       (pcase (et-type-case-value c)
                         ((cl-struct et-alias args)
                          (cl-loop for arg in args thereis (et--type-contains-binds arg)))
                         ((cl-struct et-datatype args)
                          (cl-loop for arg in args
                                   thereis (and (et-type-p arg) (et--type-contains-binds arg))))))))

(defun et--sub-match (matcher type)
  (et-cache (list #'et--sub-match matcher type) #'et-var-p
    (et--sub-match-logic matcher type)))

(defun et--super-match (matcher type)
  (et-cache (list #'et--super-match matcher type) #'et-var-p
    (et--super-match-logic matcher type)))

(defun et--sub-match-logic (matcher type)
  (let ((constraints (append (et--sub-constraints matcher type)
                             (et-matcher-constraints matcher))))
    (et--match-satisfy-constraints-smallest
     (et-matcher-generics matcher) constraints)))

(defun et--super-match-logic (matcher type)
  (let ((constraints (append (et--super-constraints matcher type)
                             (et-matcher-constraints matcher))))
    (et--match-satisfy-constraints-biggest
     (et-matcher-generics matcher) constraints)))

(defun et--match-satisfy-constraints-biggest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns (`INVALID' . REASON) if invalid. REASON is a cons
cell (MATCHER-LABEL . TYPE-LABEL), or nil."
  (if-let* ((never (assq 'Q:NEVER constraints))) (cons 'INVALID (cadr never))
    (cl-loop
     for gen in generics
     for gen-result =
     (let ((guess
            (cl-loop for (fact g type) in constraints
                     when (and (eq g gen) (memq fact '(Q:EQ Q:LEQ)))
                     collect type into types
                     finally return (et-simplify-type (apply #'et--subsect types)))))
       (if (cl-loop for (fact g type) in constraints
                    always
                    (or (not (eq g gen))
                        (not (memq fact '(Q:EQ Q:GEQ)))
                        (et-subtype? type guess)))
           guess (et-never)))
     when (equal gen-result (et-never))
     do (cl-return (cons 'INVALID nil))
     collect gen-result)))

(defun et--match-satisfy-constraints-smallest (generics constraints)
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns (`INVALID' . REASON) if invalid. REASON is a cons
cell (MATCHER-LABEL . TYPE-LABEL), or nil.

However, unlike `et--match-satisfy-constraints-biggest', this allows
values to be the never type."
  (if-let* ((never (assq 'Q:NEVER constraints))) (cons 'INVALID (cadr never))
    (cl-loop
     for gen in generics
     for gen-result =
     (let ((guess
            (cl-loop for (fact g type) in constraints
                     when (and (eq g gen) (memq fact '(Q:EQ Q:GEQ)))
                     collect type into types
                     finally return (et-simplify-type (apply #'et--or types)))))
       (if (cl-loop for (fact g type) in constraints
                    always
                    (or (not (eq g gen))
                        (not (memq fact '(Q:EQ Q:LEQ)))
                        (et-subtype? guess type)))
           guess 'INVALID))
     ;; Unlike biggest, the never type actually represents a valid possible answer
     when (equal gen-result 'INVALID)
     do (cl-return (cons 'INVALID nil))
     collect gen-result)))


;;;; Binds utils

(defun et--remove-type-binds (type)
  "Recursively remove bindings/typeofs from TYPE."
  (cl-assert (et-type-p type))

  (cl-loop for case in (et-type-cases type)
           for val = (et-type-case-value case)
           collect
           (make-et-type-case
            :value
            (pcase val
              ((cl-struct et-alias name args)
               (make-et-alias :name name :args (mapcar #'et--remove-type-binds args)))
              ((cl-struct et-datatype name args)
               (let ((new-args (et--datatype-map-type-args name args #'et--remove-type-binds)))
                 (make-et-datatype :name name :args new-args)))
              (_ (error "Invalid case val: %s" val))))
           into cases
           finally return (make-et-type :label (et-type-label type) :cases cases)))

(defun et--type-binds (type)
  ;; binds is an alist of `et-var' to a list of types (which will be `et--or'ed)
  (let ((binds-alist nil))
    (dolist (case (et-type-cases type))
      (let* ((binds (et-type-case-binds case))
             (typeofs (et-type-case-typeofs case)))
        (dolist (var (delete-dups (append (mapcar #'car binds) typeofs nil)))
          (let* ((b-type (alist-get var binds))
                 (ti-type (when (memq var typeofs)
                            (et--remove-type-binds (et-type case)))))
            (push (if (and b-type ti-type) (et--supersect b-type ti-type)
                    (or b-type ti-type))
                  (alist-get var binds-alist))))))
    (cl-loop for (var . types) in (nreverse binds-alist)
             collect (cons var (et-simplify-type (apply #'et--or (nreverse types)))))))

(et-test
 (equal (list (cons (alist-get '$a et--test-variables) (et 2|3)))
        (et--type-binds (et $a::{2|3}&{1|2})))
 (equal (list (cons (alist-get '$a et--test-variables) (et 1|2)))
        (et--type-binds (et {::$a}&{1|2})))
 (equal (list (cons (alist-get '$a et--test-variables) (et 2)))
        (et--type-binds (et {::$a}&{$a::{2|3}}&{1|2}))))


(defun et--replace-type-binds (type binds)
  (cl-loop for case in (et-type-cases (et--remove-type-binds type))
           collect (make-et-type-case :value (et-type-case-value case) :binds binds)
           into cases
           finally return (make-et-type :label (et-type-label type) :cases cases)))


;;;; Infer

(defun et--infer (matcher type output-struct &optional extra-repls)
  "Infer TYPE against MATCHER, then convert OUTPUT-STRUCT to a type.

Specifically, this will first call `et--sub-match' on MATCHER and TYPE
to determine values for the generics defined in MATCHER. Then, if this
succeeds, it will convert OUTPUT-STRUCT to a type, replacing each
generic with the value determined by `et--sub-match', as well as
EXTRA-REPLS if provided.

If matching fails, return REASON (see `et--sub-match')."

  (let* ((gen-results (et--sub-match matcher type)))
    (if (eq (car gen-results) 'INVALID) (cdr gen-results)
      (cl-loop for gen in (et-matcher-generics matcher)
               for gen-type in gen-results
               collect (cons gen gen-type) into new-repls
               finally return
               (et-structure-to-type
                output-struct
                (nconc new-repls extra-repls))))))


;;;; Funcall

(defun et--funcall (func-type arglist-type)
  "Determine the return type of calling FUNC-TYPE with ARGLIST-TYPE.

Returns the output type, or REASON if the call is invalid, where REASON
is either (MATCHER-LABEL . TYPE-LABEL), or nil."

  (et--verify-type func-type)
  (et--verify-type arglist-type)

  (setq func-type (et-expand-all-aliases func-type))

  (cl-loop for case in (et-type-cases func-type)
           for val = (et-type-case-value case)
           for result =
           (pcase val
             ((cl-struct et-datatype (name 'Function) (args `(,param-type ,return-type)))
              (and (et-subtype? arglist-type param-type) return-type))
             ((cl-struct et-datatype (name 'DynFunction) (args `(,matcher ,output-struct)))
              (et--infer matcher arglist-type output-struct))
             (_ nil))
           unless (et-type-p result) return result
           collect result into results
           finally return (apply #'et--supersect results)))


;;;; Tuple

(defun et--tuple (cons types)
  (if (null types) (et-literal nil)
    (et-alias cons (car types) (et--tuple cons (cdr types)))))

(defun et--tailed-tuple (cons types)
  (pcase types
    (`(,last) last)
    (`(,next . ,rest) (et-alias cons next (et--tailed-tuple cons rest)))
    (_ (error "No tail provided"))))


;;;; Freshen/Unfreshen

(defvar et--rec-transform-stack nil)

(defun et--rec-transform-datatypes (type transform)
  "Recursively transform datatypes in a type.

TRANSFORM is a function which takes (dt-name dt-args) and returns a new
`et-datatype' or `et-alias' (type-case value)."
  (et--stop-recursion et--rec-transform-stack (list type)
                      (et-type (make-et-alias :name (gensym "@Loop") :args nil))

    (cl-loop for case in (et-type-cases type)
             for val = (et-type-case-value case)
             nconc
             (pcase val
               ((cl-struct et-alias)
                (let* ((binds (et-type-case-binds case))
                       (typeofs (et-type-case-binds case))
                       (expanded (et-alias-expand val))
                       (expanded-transformed (et--rec-transform-datatypes expanded transform)))
                  (if (equal expanded expanded-transformed) (list case)
                    (if (not (or binds typeofs)) (et-type-cases expanded-transformed)
                      ;; Add the binds from TYPE
                      (cl-loop for exp-case in (et-type-cases expanded-transformed)
                               collect (make-et-type-case
                                        :value (et-type-case-value exp-case)
                                        :binds (et-type-case-binds case)
                                        :typeofs (et-type-case-typeofs case)))))))

               ((cl-struct et-datatype name args)
                (list (make-et-type-case
                       :value (funcall transform name args)
                       :binds (et-type-case-binds case)
                       :typeofs (et-type-case-typeofs case)))))
             into new-cases
             finally return
             (let* ((type (make-et-type :label (et-type-label type) :cases new-cases))
                    (no-binds (et--remove-type-binds type))
                    (alias-type (cdar et--rec-transform-stack)))
               (when alias-type
                 (let* ((alias (et-type-case-value (car (et-type-cases alias-type)))))
                   (et--define-alias (et-alias-name alias) [] no-binds :restrict 'TYPE)))
               type))))


(defun et--unfreshen-type (type)
  (et--remove-type-binds
   (et--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFresh (make-et-alias :name 'Cons :args (mapcar #'et--unfreshen-type args)))
        ('VectorFresh (make-et-alias :name 'Vector :args (mapcar #'et--unfreshen-type args)))
        (_ (make-et-datatype :name name :args args)))))))

(defun et--freshen-type (type)
  (et--remove-type-binds
   (et--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFull
         (let* ((new-args (list (et--freshen-type (car args)) (et--freshen-type (caddr args)))))
           (make-et-datatype :name 'ConsFresh :args new-args)))
        ('VectorFull (make-et-alias :name 'VectorFresh :args (list (et--freshen-type (car args)))))
        (_ (make-et-datatype :name name :args args)))))))

(defun et--freshen-type-shallow (type)
  (et--remove-type-binds
   (et--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFull
         (let* ((new-args (list (car args) (et--freshen-type-shallow (caddr args)))))
           (make-et-datatype :name 'ConsFresh :args new-args)))
        (_ (make-et-datatype :name name :args args)))))))


;;;; Deep expand aliases

(defun et-expand-aliases-at-depth (type depth)
  (if (<= depth 0) type
    (cl-loop for case in (et-type-cases (et-expand-all-aliases type))
             for dt = (et-type-case-value case)
             for new-dt =
             (make-et-datatype
              :name (et-datatype-name dt)
              :args (et--datatype-map-type-args
                     (et-datatype-name dt) (et-datatype-args dt)
                     (lambda (type) (et-expand-aliases-at-depth type (1- depth)))))
             collect (make-et-type-case :value new-dt) into new-cases
             finally return (make-et-type :label (et-type-label type) :cases new-cases))))


;;; ============================================================
;;; Define common aliases

(et-define-custom-alias Cons (&optional a b)
  (if a (et-q (((S:DT ConsFull ,a ,a ,(or b a) ,(or b a)))))
    (et-q (((S:DT ConsFull (((S:DT Any))) nil (((S:DT Any))) nil))))))

(et-defalias ConsR [L R] (ConsFull L Never R Never))
(et-defalias ConsW [L R] (ConsFull Any L Any R))
(et-defalias ConsRW [L R] (ConsFull L Never Any R))
(et-defalias ConsWR [L R] (ConsFull Any L R Never))

(et-define-custom-alias Vector (&optional a)
  (if a (et-q (((S:DT VectorFull ,a ,a))))
    (et-q (((S:DT VectorFull (((S:DT Any))) nil))))))

(et-defalias VectorR [E] VectorFull<E~Never>)
(et-defalias VectorW [E] VectorFull<Any~E>)

(et-defalias Nil [] (Literal nil))
(et-defalias True [] (Literal t))
(et-defalias Boolean [] (or (Literal nil) (Literal t)))

;; ConsR/ListR/*R can be thought of as "read only references" to a
;; type. It is merely a shortcut for "[(T <= Number)] List<T>" for
;; function parameters. Actual data, such as return values from
;; functions, should usually not have the ListR/ConsR/*R type.
(et-defalias ListFresh [E] (or Nil (ConsFresh E (ListFresh E))))
(et-defalias ListR [E] (or Nil (ConsR E (ListR E))))
(et-defalias List [E] (or Nil (Cons E (List E))))
(et-defalias NonNilListR [E] (ConsR E (ListR E)))

(et-defalias Tree [E] (or E (List (Tree E))))
(et-defalias TreeR [E] (or E (ListR (TreeR E))))

(et-defalias AList [K V] (List (Cons K V)))
(et-defalias AListR [K V] (ListR (ConsR K V)))

(et-defalias KVPList [K V] (or Nil (Cons K (Cons V (KVPList K V)))))

(defun et--expand-tuple-struct (cons args)
  (if (null args) (et-q (((S:DT Literal nil))))
    (et-q (((S:ALIAS ,cons ,(car args) ,(et--expand-tuple-struct cons (cdr args))))))))

(defun et--expand-tailed-tuple-struct (cons types)
  (pcase types
    (`(,last) last)
    (`(,next . ,rest)
     (et-q (((S:ALIAS ,cons ,next ,(et--expand-tailed-tuple-struct cons rest))))))
    (_ (error "No tail provided"))))

(et-define-custom-alias TupleR (&rest args) (et--expand-tuple-struct 'ConsR args))
(et-define-custom-alias Tuple (&rest args) (et--expand-tuple-struct 'Cons args))
(et-define-custom-alias TupleStar (&rest args) (et--expand-tuple-struct 'Cons args))
(et-define-custom-alias Args (&rest args) (et--expand-tuple-struct 'ConsR args))
(et-define-custom-alias ArgsWithTail (&rest args) (et--expand-tailed-tuple-struct 'ConsR args))
(et-define-custom-alias TupleWithTail (&rest args) (et--expand-tailed-tuple-struct 'Cons args))


;;; ============================================================
;;; Provide

(provide 'et)


;;; et.el ends here
