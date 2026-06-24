;;; et-cache.el --- Caching for et.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
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

(require 'et-check)
(require 'subr-x)


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
               (func Function<Any~Any>)
               (@return Any)
               (@skip)))

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
  (declare (et (idx Integer)
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


;;; ============================================================
;;; Hashing
;;;; Documentation

;; `et-hash-type' turns an `et-type' into a digest string based purely
;; on structure, never on naming. Two structurally identical types hash
;; identically; an alias never hashes the same as its definition.
;;
;; Hashing accumulates values onto `et--hashing-state-acc' (pushed in
;; reverse, then reversed and digested at the end). To "hash" a value
;; just means to push it. Lengths are pushed before every
;; variable-length sequence so that distinct structures cannot flatten
;; to the same accumulator.
;;
;; Variables and scoped datatypes are ephemeral (fresh symbols every
;; session), so hashing them by identity would be useless. Instead each
;; is replaced by its index in `vars'/`scoped' -- two types differing
;; only in which ephemeral objects they contain hash identically. The
;; encountered objects are returned alongside the hash, so the caller
;; can restore the correct ephemeral objects into a cached result.
;;
;; The alias-definition stack is threaded as a plain parameter (newest
;; first, extended with `cons') rather than living on the state: a
;; recursive alias hashes its index in the stack instead of looping.


;;;; State

(cl-defstruct (et--hashing-state (:constructor et--make-hashing-state))
  (acc nil)            ; Accumulated values, in reverse order
  (vars nil)           ; `et-var's encountered so far, in order (index = position)
  (scoped nil)         ; Scoped arg-tuples encountered so far (compared by `equal')
  (name-dependent nil)) ; Whether to fold alias names into the hash (see `et--hash-alias-def')

(defun et--hash-push (state value)
  "Accumulate VALUE onto STATE's hash."
  (push value (et--hashing-state-acc state)))

(defun et--hash-var-index (state var)
  "Return the index of VAR in STATE's variable list, appending if new."
  (let ((vars (et--hashing-state-vars state)))
    (or (cl-position var vars :test #'eq)
        (prog1 (length vars)
          (setf (et--hashing-state-vars state) (append vars (list var)))))))

(defun et--hash-scoped-index (state tuple)
  "Return the index of scoped TUPLE in STATE, appending if new.
TUPLE is the (NAME UNIQUE CONSTRAINTS) arg list of a `Scoped' datatype."
  (let ((scoped (et--hashing-state-scoped state)))
    (or (cl-position tuple scoped :test #'equal)
        (prog1 (length scoped)
          (setf (et--hashing-state-scoped state) (append scoped (list tuple)))))))

(defconst et--hash-repr-factor-types
  '(S:DT S:ALIAS S:GENERIC S:TYPE S:BIND S:TYPEOF
         S:BINDS-OF S:SUBTRACT S:INFER S:EXTENDS S:EVAL S:SET S:NOINFER)
  "Fixed ordering of repr factor types, used to hash a factor's type.")


;;;; Types

(defun et--hash-type (state type alias-stack)
  (et--hash-push state (et-type-label type))
  (let ((cases (et-type-cases type)))
    (et--hash-push state (length cases))
    (dolist (case cases)
      (et--hash-type-case state case alias-stack))))

(defun et--hash-type-case (state case alias-stack)
  (let ((binds (et-type-case-binds case))
        (typeofs (et-type-case-typeofs case))
        (value (et-type-case-value case)))
    ;; Binds: (et-var . et-type)
    (et--hash-push state (length binds))
    (dolist (bind binds)
      (et--hash-push state (et--hash-var-index state (car bind)))
      (et--hash-type state (cdr bind) alias-stack))
    ;; Typeofs: et-var
    (et--hash-push state (length typeofs))
    (dolist (var typeofs)
      (et--hash-push state (et--hash-var-index state var)))
    ;; Value: datatype or alias
    (cond
     ((et-datatype-p value)
      (et--hash-push state "datatype")
      (et--hash-datatype state (et-datatype-name value) (et-datatype-args value)
                         alias-stack nil))
     ((et-alias-p value)
      (et--hash-push state "alias")
      (et--hash-alias state (et-alias-name value) (et-alias-args value)
                      alias-stack nil))
     (t (error "Invalid type-case value: %s" value)))))


;;;; Datatypes

(defun et--hash-datatype (state name args alias-stack as-repr)
  "Hash datatype NAME applied to ARGS.
When AS-REPR is non-nil the variant arguments are reprs; otherwise they
are types.  The argument count is fixed per datatype, so it is not
hashed."
  (et--hash-push state (or (cl-position name et--datatypes :key #'car)
                           (error "Invalid datatype: %s" name)))
  (cond
   ;; Scoped: the (NAME UNIQUE CONSTRAINTS) tuple is ephemeral, so it is
   ;; treated like a variable -- hash its index -- plus its constraints
   ;; (the generic name is irrelevant).
   ((eq name 'Scoped)
    (et--hash-push state (et--hash-scoped-index state args))
    (et--hash-constraints state (nth 2 args) alias-stack))
   ;; DynFunction: arg 0 is a matcher, arg 1 an output repr.
   ((eq name 'DynFunction)
    (et--hash-matcher state (nth 0 args) alias-stack)
    (et--hash-repr state (nth 1 args) alias-stack))
   ;; Otherwise: CONST args are literal values, variant args are types
   ;; (or reprs, when hashing a repr factor).
   (t
    (cl-loop for arg in args
             for role in (et--datatype-arg-roles name args)
             do (pcase role
                  ('CONST (et--hash-push state arg))
                  ((or 'CO 'CONTRA 'ISO)
                   (if as-repr
                       (et--hash-repr state arg alias-stack)
                     (et--hash-type state arg alias-stack)))
                  (_ (error "Unknown datatype arg role: %s" role)))))))


;;;; Aliases

(defun et--hash-alias (state name args alias-stack as-repr)
  "Hash alias reference NAME applied to ARGS.
When AS-REPR is non-nil ARGS are reprs; otherwise they are types."
  (et--hash-alias-def state name alias-stack)
  (et--hash-push state (length args))
  (dolist (arg args)
    (if as-repr
        (et--hash-repr state arg alias-stack)
      (et--hash-type state arg alias-stack))))

(defun et--hash-alias-def (state name alias-stack)
  "Hash the definition of alias NAME under the current ALIAS-STACK."
  (if-let* ((idx (cl-position name alias-stack :test #'eq)))
      ;; Recursive reference: hash its depth in the stack and stop.
      (et--hash-push state idx)
    (let ((plist (or (get name 'et-alias) (error "Alias %s not defined" name))))
      (if (plist-get plist :custom)
          ;; Custom aliases are assumed immutable, so the name suffices.
          (et--hash-push state name)
        ;; Repr aliases: descend into the definition, guarding recursion.
        ;; When name-dependent, also fold in the name, so a structurally
        ;; identical alias under a different name hashes differently.
        (progn
          (when (et--hashing-state-name-dependent state)
            (et--hash-push state name))
          (et--hash-repr state
                         (or (plist-get plist :repr)
                             (error "Alias %s missing repr" name))
                         (cons name alias-stack)))))))


;;;; Constraints

(defun et--hash-constraints (state constraints alias-stack)
  (et--hash-push state (length constraints))
  (dolist (q constraints)
    (et--hash-type-constraint state q alias-stack)))

(defun et--hash-type-constraint (state q alias-stack)
  (pcase q
    (`(,op ,gen ,type)
     (et--hash-push state (pcase op
                            ('Q:EQ 0) ('Q:GEQ 1) ('Q:LEQ 2)
                            (_ (error "Invalid constraint operator: %s" op))))
     (et--hash-push state gen)
     (et--hash-type state type alias-stack))
    (_ (error "Invalid type constraint: %s" q))))


;;;; Matchers

(defun et--hash-matcher (state matcher alias-stack)
  (let ((generics (et-matcher-generics matcher)))
    (et--hash-push state (length generics))
    (dolist (g generics) (et--hash-push state g)))
  (et--hash-constraints state (et-matcher-constraints matcher) alias-stack)
  (et--hash-repr state (et-matcher-repr matcher) alias-stack))


;;;; Reprs

(defun et--hash-repr (state repr alias-stack)
  (et--hash-push state (et-repr-target repr))
  (et--hash-push state (et-repr-label repr))
  (let ((cases (et-repr-dnf repr)))
    (et--hash-push state (length cases))
    (dolist (case cases)
      (et--hash-push state (length case))
      (dolist (factor case)
        (et--hash-repr-factor state factor alias-stack)))))

(defun et--hash-repr-factor (state factor alias-stack)
  (et--hash-push state (or (cl-position (car factor) et--hash-repr-factor-types)
                           (error "Invalid repr factor: %s" factor)))
  (pcase factor
    ;; Datatype/alias args are reprs here, hence the AS-REPR flag.
    (`(S:DT ,name . ,args) (et--hash-datatype state name args alias-stack t))
    (`(S:ALIAS ,name . ,args) (et--hash-alias state name args alias-stack t))
    (`(S:GENERIC ,var) (et--hash-push state var))
    (`(S:TYPE ,type) (et--hash-type state type alias-stack))
    (`(S:BIND ,var ,repr)
     (et--hash-push state var)
     (et--hash-repr state repr alias-stack))
    (`(S:TYPEOF ,var) (et--hash-push state var))
    (`(S:BINDS-OF ,repr) (et--hash-repr state repr alias-stack))
    (`(S:SUBTRACT ,a ,b)
     (et--hash-repr state a alias-stack)
     (et--hash-repr state b alias-stack))
    (`(S:INFER ,type ,generics ,matcher ,yes ,no)
     (et--hash-push state (length generics))
     (dolist (g generics) (et--hash-push state g))
     (et--hash-repr state type alias-stack)
     (et--hash-matcher state matcher alias-stack)
     (et--hash-repr state yes alias-stack)
     (et--hash-repr state no alias-stack))
    (`(S:EXTENDS ,sub ,super ,yes ,no)
     (et--hash-repr state sub alias-stack)
     (et--hash-repr state super alias-stack)
     (et--hash-repr state yes alias-stack)
     (et--hash-repr state no alias-stack))
    (`(S:EVAL ,func . ,reprs)
     (et--hash-push state func)
     (et--hash-push state (length reprs))
     (dolist (r reprs) (et--hash-repr state r alias-stack)))
    ;; The second element of S:SET is a fully parsed type, not a repr.
    (`(S:SET ,matcher-repr ,type)
     (et--hash-repr state matcher-repr alias-stack)
     (et--hash-type state type alias-stack))
    ;; S:NOINFER wraps a single type-target repr.
    (`(S:NOINFER ,tr) (et--hash-repr state tr alias-stack))
    (_ (error "Invalid repr factor: %s" factor))))


;;;; Entry point

(defun et--hash-finalize (state)
  "Digest STATE's accumulated values into a string."
  (let* ((print-level nil)
         (print-length nil))
    (secure-hash 'md5 (prin1-to-string (nreverse (et--hashing-state-acc state))))))

(cl-defstruct et-hash
  "The result of hashing a list of items.

VALUE is the digest string. VARS and SCOPED are the `et-var's and
`Scoped' datatype arg-tuples encountered (in the order their indices
were assigned), which let a caller restore the ephemeral objects of a
cached result."
  (value nil :et String)
  (vars nil :et List<*et-var>)
  (scoped nil :et List<Any>))

(cl-defun et-hash-items (items &key name-dependent)
  "Hash ITEMS into a structural `et-hash'.

Each item must be an `et-type', `et-matcher', `et-repr', or a lisp
object; anything else signals an error. Items are hashed in order into a
single digest, each tagged by its kind so that, e.g., a type never
collides with an atom of the same shape.

By default hashing is purely structural: aliases are identified by their
definition, never their name. With NAME-DEPENDENT non-nil, alias names
are also folded into the hash, so two structurally identical types that
reference differently named aliases hash differently. This is required
whenever the cached value can contain alias names reachable from the
input (see the call cache)."
  (let ((state (et--make-hashing-state :name-dependent name-dependent)))
    (dolist (item items)
      (cond
       ((et-type-p item)
        (et--hash-push state 'type)
        (et--hash-type state item nil))
       ((et-matcher-p item)
        (et--hash-push state 'matcher)
        (et--hash-matcher state item nil))
       ((et-repr-p item)
        (et--hash-push state 'repr)
        (et--hash-repr state item nil))
       ((or (numberp item) (stringp item) (symbolp item) (consp item))
        (et--hash-push state 'object)
        (et--hash-push state item))
       (t (error "Cannot hash item: %s" item))))
    (make-et-hash
     :value (et--hash-finalize state)
     :vars (et--hashing-state-vars state)
     :scoped (et--hashing-state-scoped state))))


;;;; Tests

(defun et-same-hash? (t1 &rest rest)
  (cl-loop with t1-hash = (et-hash-value (et-hash-items (list t1)))
           for type in rest
           always (equal t1-hash (et-hash-value (et-hash-items (list type))))))

(et-test
 ;; Basic comparisons
 (not (et-same-hash? (et ConsR 1 2) (et ConsR 1 2)))
 (not (et-same-hash? (et ConsR 1 2) (et ConsR 1 3)))
 (not (et-same-hash? (et ConsR 1 2) (et ConsW 1 2)))

 ;; Variable binds
 (not (et-same-hash? (et ConsR 1 2) (et ConsR 1&{$a::Number} 2)))
 (et-same-hash? (et ConsR 1&{$a::Number} 2) (et ConsR 1&{$a::Number} 2))
 (et-same-hash? (et ConsR 1&{$b::Number} 2) (et ConsR 1&{$a::Number} 2))
 (et-same-hash? (et ConsR 1&{$a::3} 2&{$b::4}) (et ConsR 1&{$a::3} 2&{$b::4}))
 ;; Which variable is which doesn't matter
 (et-same-hash? (et ConsR 1&{$a::3} 2&{$b::4}) (et ConsR 1&{$b::3} 2&{$a::4}))
 ;; What the variable is narrowed to matters
 (not (et-same-hash? (et ConsR 1&{$a::3} 2) (et ConsR 1&{$a::4} 2)))
 ;; Relative variable order matters
 (et-same-hash? (et Tuple 1&{$a::1} 2&{$b::2} 3&{$b::2})
                (et Tuple 1&{$b::1} 2&{$a::2} 3&{$a::2}))
 (not (et-same-hash? (et Tuple 1&{$a::1} 2&{$a::2} 3&{$b::2})
                     (et Tuple 1&{$b::1} 2&{$a::2} 3&{$a::2})))

 ;; Same but for typeofs
 (not (et-same-hash? (et ConsR 1 2) (et ConsR 1&{::$a} 2)))
 (et-same-hash? (et ConsR 1&{::$a} 2) (et ConsR 1&{::$b} 2))
 (not (et-same-hash? (et ConsR 1&{$a::Any} 2) (et ConsR 1&{::$b} 2)))
 (et-same-hash? (et ConsR 1&{::$a} 2&{::$b}) (et ConsR 1&{::$b} 2&{::$a}))
 (et-same-hash? (et Tuple 1&{::$a} 2&{::$b} 3&{::$b})
                (et Tuple 1&{::$b} 2&{::$a} 3&{::$a}))
 (not (et-same-hash? (et Tuple 1&{::$a} 2&{::$a} 3&{::$b})
                     (et Tuple 1&{::$b} 2&{::$a} 3&{::$a})))

 ;; Structural alias equality and circular aliases
 (progn
   (et--process-exprs
    '((et-declare
       (@alias ConsR2 [L R] (ConsFull L Never R Never))
       (@alias CircA [] (ConsR CircB Integer))
       (@alias CircB [] (ConsR CircA Integer))
       (@alias Circ2A [] (ConsR2 Circ2B Integer))
       (@alias Circ2B [] (ConsR2 Circ2A Integer))
       (@alias CircSolo [] (ConsR CircSolo Integer))

       (@alias Circ3A [] (ConsR Circ3B Integer))
       (@alias Circ3B [] (ConsR Circ3A Number)))))

   (and
    (et-same-hash? (et ConsR 1 2) (et ConsR2 1 2))
    (not (et-same-hash? (et ConsR 1 2) (et ConsR2 1 3)))

    (et-same-hash? (et CircA) (et CircB) (et Circ2A) (et Circ2B))
    (not (et-same-hash? (et CircA) (et CircSolo)))
    (not (et-same-hash? (et Circ3A) (et Circ3B)))))

 ;; Hash of alias depends on alias structure, not name
 (progn
   (et--process-exprs
    '((et-declare (@alias Test1 [] (ConsR Integer String)))
      (et-declare (@alias Test2 [] (ConsR 1 2)))))
   (let* ((h1 (et-hash-value (et-hash-items (list (et Test1)))))
          (h2 (et-hash-value (et-hash-items (list (et Test2))))))
     ;; Swap the definitions
     (et--process-exprs
      '((et-declare (@alias Test1 [] (ConsR 1 2)))
        (et-declare (@alias Test2 [] (ConsR Integer String)))))
     (let* ((h3 (et-hash-value (et-hash-items (list (et Test1)))))
            (h4 (et-hash-value (et-hash-items (list (et Test2))))))
       (and (equal h1 h4)
            (equal h2 h3)
            (not (equal h1 h3))
            (not (equal h2 h4)))))))


;;; ============================================================
;;; Caching
;;;; Documentation

;; The cache is partitioned per source file: a single run only ever
;; touches the cached results for the exact file being checked. Each
;; source's `et-cache' lives in its own file under `et-cache-directory'
;; (named after the escaped source path), or in `et-cache-nonfile-file'
;; for a buffer with no source. There is no persistent in-memory cache;
;; disk is the only store.
;;
;; `et-with-cache-source' loads a source's `et-cache' from disk (or a
;; fresh one) into the dynamically scoped `et--current-cache', runs the
;; body, and writes `et--current-cache' back to disk afterwards. A run
;; enters through `et-with-cache-source-if-enabled', which does this
;; only when `et-cache-enabled' -- the single place that switch is read.
;; When caching is disabled `et--current-cache' stays nil, so the defun
;; and call caches below simply see no cache and fall back to the
;; uncached path; they key off `et--current-cache', never the switch.
;;
;; The cached source is deliberately distinct from `et--checking-file':
;; under flycheck we actually check a temp copy, so `et--checking-file'
;; (the temp path) is the thing being checked, while the source passed
;; to `et-with-cache-source' (the real path) is what the cache is keyed
;; on.


;;;; Variables

(defvar et-cache-enabled t
  "Master switch for et's caching.  When nil, no caching happens at all.")

(defvar et-cache-defuns t
  "Whether to cache the result of checking a defun body.
Has no effect unless `et-cache-enabled' is non-nil.")

(defvar et-cache-calls t
  "Whether to cache subtyping and constraint-solving calls.
Has no effect unless `et-cache-enabled' is non-nil.")

(defvar et-cache-directory
  (expand-file-name ".cache/et-cache/" user-emacs-directory)
  "Directory holding one cache file per checked source file.")

(defvar et-cache-nonfile-file
  (expand-file-name "et-cache-nonfile.eld" temporary-file-directory)
  "Cache file used when type checking a buffer with no source file.")

(defvar et--current-cache nil
  "The `et-cache' for the source currently being checked, or nil.
Bound to a loaded `et-cache' by `et-with-cache-source' for the duration
of a run, and left nil when caching is disabled.  The defun and call
caches read and fill it in place when it is non-nil, and fall back to
the uncached path otherwise; they never consult `et-cache-enabled'.")


;;;; Structs

(cl-defstruct et-cache
  "The whole in-memory cache for one source file, serialized to disk.

DEFUN-CACHE maps a function name to its `et--cached-defun'.
CALL-CACHE maps a call hash (see `et--call-cached') to a cached result,
with ephemeral variables and scoped datatypes replaced by placeholders."
  (defun-cache (make-hash-table :test 'eq))
  (call-cache (make-hash-table :test 'equal)))

(cl-defstruct et--cached-defun
  "A cached defun check: its FINGERPRINT and the `et-result' VALUE."
  fingerprint value)


;;;; Cache files

(defun et--cache-file-for (source)
  "Return the cache file path for SOURCE (nil means the non-file cache)."
  (if (null source)
      et-cache-nonfile-file
    (expand-file-name
     (concat (replace-regexp-in-string
              "[^A-Za-z0-9_.-]" (lambda (s) (format "%%%02x" (aref s 0))) source)
             ".eld")
     et-cache-directory)))

(defun et--read-cache-file (file)
  "Read an `et-cache' from FILE, or nil if it is absent or unreadable."
  (and (file-exists-p file)
       (ignore-errors
         (with-temp-buffer
           (insert-file-contents file)
           (let ((obj (read (current-buffer))))
             (and (et-cache-p obj) obj))))))

(defun et--write-cache-file (cache file)
  "Write CACHE to FILE, creating its parent directory if needed."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-level nil) (print-length nil) (print-circle t))
      (prin1 cache (current-buffer)))))

(defun et--load-cache-file (file)
  "Read an `et-cache' from FILE, or return a fresh empty one."
  (or (et--read-cache-file file) (make-et-cache)))

(defmacro et--with-cache-file (file &rest body)
  "Run BODY with FILE's `et-cache' loaded into `et--current-cache'.
Load FILE's cache from disk (or a fresh one) into `et--current-cache',
run BODY, then write `et--current-cache' back to FILE."
  (declare (indent 1))
  (let ((f (gensym "file")))
    `(let* ((,f ,file)
            (et--current-cache (et--load-cache-file ,f)))
       (unwind-protect (progn ,@body)
         (et--write-cache-file et--current-cache ,f)))))

(defmacro et-with-cache-source (source &rest body)
  "Run BODY with SOURCE's `et-cache' loaded into `et--current-cache'.
SOURCE is the real source file path, or nil for the non-file cache.
See `et--with-cache-file'."
  (declare (indent 1))
  `(et--with-cache-file (et--cache-file-for ,source) ,@body))

(defmacro et-with-cache-source-if-enabled (source &rest body)
  "Like `et-with-cache-source', but a no-op wrapper when caching is off.
When `et-cache-enabled' is non-nil, load SOURCE's cache as
`et-with-cache-source' does; otherwise just run BODY with
`et--current-cache' left nil.  This is the only place `et-cache-enabled'
is consulted: every consumer keys off whether `et--current-cache' is
non-nil instead."
  (declare (indent 1))
  `(if et-cache-enabled
       (et-with-cache-source ,source ,@body)
     ,@body))

(defun et--all-cache-files ()
  "Return the list of all existing on-disk cache files."
  (append (when (file-directory-p et-cache-directory)
            (directory-files et-cache-directory t "\\.eld\\'"))
          (when (file-exists-p et-cache-nonfile-file)
            (list et-cache-nonfile-file))))

(defun et-clear-source-cache (source)
  "Empty SOURCE's cached results, rewriting its cache file in place.
SOURCE is the original source file path, or nil for the non-file cache.
Interactively, clears the cache for the current buffer's file."
  (interactive (list (buffer-file-name)))
  (et-with-cache-source source
    (setq et--current-cache (make-et-cache))))

(defun et-clear-cache ()
  "Empty every source's cached results, rewriting the cache files in place."
  (interactive)
  (dolist (file (et--all-cache-files))
    (et--with-cache-file file
      (setq et--current-cache (make-et-cache)))))


;;;; Clearing the defun cache

(defun et-clear-defun-cache (source &optional all-loaded)
  "Clear cached defun check results, leaving the call cache intact.
Clear SOURCE's defun cache, or every on-disk source's when ALL-LOADED
is non-nil, rewriting the affected cache files in place.  Called
interactively, clears every source's defun cache."
  (interactive (list nil t))
  (dolist (file (if all-loaded (et--all-cache-files)
                  (list (et--cache-file-for source))))
    (et--with-cache-file file
      (clrhash (et-cache-defun-cache et--current-cache)))))


;;;; Error guard

;; The cache is supposed to be a transparent optimization: it slots in
;; alongside the real logic without ever changing behavior, so its own
;; bookkeeping (hashing, lookups, stores, possibly-stale on-disk structs)
;; must never let an error escape into the type checker. `et--cache-try'
;; runs a piece of cache bookkeeping and, if it signals, yields the
;; sentinel `et--cache-bail' so the caller can fall back to the uncached
;; path. `debug-on-error' is honored, so a real bug still surfaces while
;; debugging.

(defvar et--cache-bail (make-symbol "et--cache-bail")
  "Sentinel returned by `et--cache-try' when cache bookkeeping signals.")

(defmacro et--cache-try (&rest body)
  "Evaluate BODY, shielding the checker from cache-machinery errors.
Return BODY's value, or `et--cache-bail' if BODY signals."
  (declare (indent 0) (debug t))
  `(condition-case-unless-debug _ (progn ,@body) (error et--cache-bail)))


;;; ============================================================
;;; Defun Caching
;;;; Documentation

;; A defun's body check is cached keyed on the defun name. The cached
;; payload is the full `et-result' (its value is just `(et-literal
;; name)', but it also carries the diagnostics and the failed flag,
;; which are the real output of checking a body).
;;
;; An `et-defun-fingerprint' records everything whose change should
;; invalidate that result:
;;   SOURCE    - hash of the defun's own source (from its `et-func-sig')
;;   SIGNATURE - hash of the defun's own function type
;;   TYPES     - alist of (SPEC . HASH) for every type spec the body
;;               parsed, so an alias changing under an unchanged body
;;               is caught by re-parsing
;;   FUNCTIONS - alist of (NAME . HASH) for every typed function the
;;               body called, hashed by that function's *signature*
;;               (the body never depends on a callee's body)
;; A nil HASH means "not defined when cached"; if it later resolves,
;; the recomputed hash differs and the entry is invalidated.
;;
;; The dependency identities are captured as side effects of the
;; original check (advice on `et-parse-type' and `et--check'), so on a
;; miss the check runs once and the fingerprint falls out of it. On a
;; hit, validation re-hashes those stored identities without ever
;; re-checking the body.

(cl-defstruct et-defun-fingerprint
  "What must be unchanged for a cached defun result to stay valid."
  source signature types functions)


;;;; Dependency capture

(defvar et--fp-active nil
  "Non-nil while capturing a defun's referenced types and functions.")
(defvar et--fp-specs nil
  "Type specs parsed while `et--fp-active', newest first.")
(defvar et--fp-funcs nil
  "Typed function names checked while `et--fp-active', newest first.")

;; These are `:before' advices: a signal here would abort the advised
;; call itself, so their bodies are fully guarded. Their results are
;; discarded, so swallowing is safe -- a failed capture just means the
;; fingerprint omits a dependency, which only forgoes a future cache hit.

(defun et--cache-capture-type (spec &rest _)
  "Advice on `et-parse-type': record SPEC as a body type dependency."
  (et--cache-try
    (when et--fp-active
      (cl-pushnew spec et--fp-specs :test #'equal))))

(defun et--cache-capture-function (expr)
  "Advice on `et--check': record a called typed function as a dependency."
  (et--cache-try
    (when (and et--fp-active (consp expr)
               (symbolp (car expr)) (car expr))
      (cl-pushnew (car expr) et--fp-funcs :test #'eq))))


;;;; Fingerprint computation

(defun et--hash-name-func-type (name)
  "Hash NAME's function type, or nil if it has none."
  (when-let* ((ft (get name 'et-function-type)))
    (et-hash-value (et-hash-items (list ft)))))

(defun et--hash-type-spec (spec)
  "Hash the type SPEC parses to, or nil if it does not resolve."
  (ignore-errors (et-hash-value (et-hash-items (list (et-parse-type spec))))))

(defun et--hash-defun-source (name)
  "Hash the source of the defun NAME, or nil if it has no signature."
  (when-let* ((sig (get name 'et-function-signature)))
    (secure-hash 'md5 (let ((print-level nil) (print-length nil))
                        (prin1-to-string (et-func-sig-source sig))))))

(defun et--compute-defun-fingerprint (name specs funcs)
  "Build an `et-defun-fingerprint' for NAME from its captured deps."
  (make-et-defun-fingerprint
   :source (et--hash-defun-source name)
   :signature (et--hash-name-func-type name)
   :types (mapcar (lambda (s) (cons s (et--hash-type-spec s))) specs)
   :functions (mapcar (lambda (f) (cons f (et--hash-name-func-type f))) funcs)))

(defun et--defun-fingerprint-current-p (name fp)
  "Return non-nil if FP still matches the current state for NAME."
  (and (equal (et-defun-fingerprint-source fp) (et--hash-defun-source name))
       (equal (et-defun-fingerprint-signature fp) (et--hash-name-func-type name))
       (cl-every (lambda (c) (equal (cdr c) (et--hash-type-spec (car c))))
                 (et-defun-fingerprint-types fp))
       (cl-every (lambda (c) (equal (cdr c) (et--hash-name-func-type (car c))))
                 (et-defun-fingerprint-functions fp))))


;;;; Cached check

(defun et--check-cached (orig expr)
  "Around advice on `et--check' implementing the defun cache.

For a root defun with a parsed signature, return its cached result when
the fingerprint still holds; otherwise run ORIG once, fingerprint the
dependencies it touched, and store the result. All other expressions
pass straight through to ORIG."
  (if (not (and et--current-cache et-cache-defuns
                (eq (car-safe expr) 'defun)
                (symbolp (cadr expr))
                (get (cadr expr) 'et-function-signature)))
      (funcall orig expr)

    ;; Guarded lookup: a fresh, fingerprint-valid entry is wrapped in a
    ;; list so a hit (any value, even nil) is distinguishable from a miss
    ;; (nil) or a bail (`et--cache-bail'). No side effects here, so a
    ;; bail safely falls through to recompute.
    (let* ((name (cadr expr))
           (hit (et--cache-try
                  (let* ((table (et-cache-defun-cache et--current-cache))
                         (cached (gethash name table)))
                    (when (and cached
                               (et--defun-fingerprint-current-p
                                name (et--cached-defun-fingerprint cached)))
                      (list (et--cached-defun-value cached)))))))
      (pcase hit
        ;; Hit: replay the cached diagnostics into the current context
        ;; (re-anchoring their relative paths) and return the value.
        (`(,result)
         (et-propagate-result result)
         (et-result-value result))

        ;; Miss/bail: check in an isolated boundary so the diagnostics are
        ;; captured relative to the defun, fingerprint what it touched,
        ;; store (guarded), then propagate as usual. ORIG runs exactly once.
        (_
         (let* ((et--fp-active t)
                (et--fp-specs nil)
                (et--fp-funcs nil)
                (result (et-result-boundary (funcall orig expr))))
           (et--cache-try
             (puthash name
                      (make-et--cached-defun
                       :fingerprint (et--compute-defun-fingerprint
                                     name et--fp-specs et--fp-funcs)
                       :value result)
                      (et-cache-defun-cache et--current-cache)))
           (et-propagate-result result)
           (et-result-value result)))))))


;;; ============================================================
;;; Call Caching
;;;; Documentation

;; Subtyping (`et-subtype?') and constraint solving (`et-sub-constraints'
;; and `et--super-constraints') are pure functions of their arguments --
;; all of which are hashable -- so their results are memoized in the
;; per-file `call-cache', keyed on the hash of (FUNCNAME ARGS...). The
;; advice sits on the inner recursive bodies (`et--subtype?-1',
;; `et--sub-constraints-1', `et--super-constraints-1'); see "The recursion
;; stack" below for why. Two wrinkles make this more than a plain memo
;; table.
;;
;; Ephemerals. A constraint result can contain `et-var's and scoped
;; datatypes, fresh identity tokens with no meaning across sessions.
;; Their identities in a result are always a subset of the input's, and
;; hashing the input already enumerates them: an `et-hash' carries the
;; `vars' and `scoped' it encountered, in index order. On a store we swap
;; each ephemeral for a positional placeholder; on a hit we swap the
;; placeholders back for the *current* call's ephemerals, reconstituting
;; an answer with the right fresh tokens.
;;
;; The recursion stack. Each function wraps its body in
;; `et--stop-recursion', which breaks cycles by optimistically assuming a
;; result and recording it on the stack frame. The advice sits on the
;; inner `-1' function, so a frame is already pushed when it runs -- the
;; car of the stack is this very call. Hence:
;;
;;   * A result is safe to cache only if no *strict ancestor* frame was
;;     touched while computing it. A touched ancestor (its mark flipped
;;     away from `et--stop-recursion-unset-marker') means the result
;;     leaned on a parent's optimistic assumption and is context-bound. A
;;     touched car is fine -- that is mere self-recursion, reproduced
;;     identically every time.
;;
;;   * A failing constraint result carries a `stack' field built from the
;;     live frames, so it only means anything in the context it was born
;;     in. We therefore cache a failure only at the root (no ancestors,
;;     so the field spans this call's own subtree) and only *serve* a
;;     cached failure at the root. Successful results carry no stack and
;;     are served at any depth.


;;;; Placeholders

(defun et--call-ph (kind idx)
  "Return the placeholder symbol for an ephemeral of KIND at index IDX."
  (intern (format "@@et-ph-%s-%d@@" kind idx)))

(defun et--call-ephemeral-alist (hash store)
  "Substitution alist between HASH's ephemerals and their placeholders.
With STORE non-nil, map each ephemeral to its placeholder; otherwise map
each placeholder back to the ephemeral. A variable is keyed by its
`et-var', a scoped datatype by its unique symbol."
  (cl-loop for kind in '(var scoped)
           for ephemerals in (list (et-hash-vars hash)
                                   (mapcar #'cadr (et-hash-scoped hash)))
           nconc (cl-loop for e in ephemerals for i from 0
                          for ph = (et--call-ph kind i)
                          collect (if store (cons e ph) (cons ph e)))))


;;;; Cache policy

(defun et--call-uncompromised-p (stack)
  "Non-nil if no strict ancestor frame on STACK was touched by a cutoff.
STACK's car is this call's own frame; self-recursion there is harmless."
  (cl-every (lambda (frame) (eq (cdr frame) et--stop-recursion-unset-marker))
            (cdr stack)))

(cl-defun et--call-cached (name orig args stack success-p &key name-dependent)
  "Around-advice body memoizing a recursive type function.

NAME identifies the function in the cache key; ORIG and ARGS are the
advised call. STACK is the function's `et--stop-recursion' stack, whose
car is this call's frame. SUCCESS-P tests whether a result may be served
at any depth; a non-success result is confined to the root level.

NAME-DEPENDENT is forwarded to `et-hash-items' for the cache key: pass
non-nil when the cached value can contain alias names (constraint
solving), and nil when it cannot (subtyping returns a bare boolean)."
  ;; Guarded lookup: resolve to a ready-to-return hit, or the data the
  ;; store phase needs on a miss. No side effects, so a bail safely falls
  ;; through to running ORIG uncached. With no cache loaded the subject
  ;; is nil, which matches no clause and falls through likewise.
  (pcase (and et--current-cache et-cache-calls
              (et--cache-try
                (let* ((cache (et-cache-call-cache et--current-cache))
                       (hash (et-hash-items (cons name args) :name-dependent name-dependent))
                       (key (et-hash-value hash))
                       (root (null (cdr stack)))
                       (cached (gethash key cache 'et--call-miss)))
                  (if (and (not (eq cached 'et--call-miss))
                           (or (funcall success-p cached) root))
                      (cons 'hit (et--subst-with-placeholders
                                  cached (et--call-ephemeral-alist hash nil)))
                    (list 'miss cache key hash root)))))

    (`(hit . ,value) value)

    ;; Miss: run ORIG exactly once, then store (guarded and discarded).
    (`(miss ,cache ,key ,hash ,root)
     (let ((result (apply orig args)))
       (when (and (or (funcall success-p result) root)
                  (et--call-uncompromised-p stack))
         (et--cache-try
           (puthash key (et--subst-with-placeholders
                         result (et--call-ephemeral-alist hash t))
                    cache)))
       result))

    ;; Disabled or bailed: behave as the unadvised function.
    (_ (apply orig args))))


;;;; Advice

(defun et--sub-constraints-cached (orig &rest args)
  (et--call-cached 'et--sub-constraints-1 orig args
                   et--constraints-stack #'et-match-result-success
                   :name-dependent t))

(defun et--super-constraints-cached (orig &rest args)
  (et--call-cached 'et--super-constraints-1 orig args
                   et--constraints-stack #'et-match-result-success
                   :name-dependent t))

(defun et--subtype?-cached (orig &rest args)
  (et--call-cached 'et--subtype?-1 orig args
                   et--subtype-stack (lambda (&rest _) t)))


;;; ============================================================
;;; Extra
;;;; Process directory

(defun et-process-directory (dir &rest args)
  ;; Ignores all diagnostics
  (et-result-boundary
   (dolist (file (directory-files dir t))
     (when (file-regular-p file)
       (et-error-boundary nil
         (apply #'et--process-exprs (et--file-exprs file) args))))))


;;;; Process in the current buffer

(defun et-buffer-cache-source ()
  (when buffer-file-name
    (expand-file-name buffer-file-name)))

(defun et-process-buffer (&rest args)
  (interactive (if current-prefix-arg '(:check t :test t) nil))
  (et-result-boundary
   (et-with-cache-source-if-enabled (et-buffer-cache-source)
     (apply #'et--process-exprs (et--buffer-exprs) args))))

(defun et-process-form (expr &optional cache-source &rest args)
  (interactive (list (save-excursion (beginning-of-defun) (read (current-buffer)))
                     (et-buffer-cache-source)))
  (et-result-boundary
   (et-with-cache-source-if-enabled cache-source
     (apply #'et--process-exprs (list expr) args))))


;;;; Flycheck entry point

(defun et-flycheck-check-file (true-file &optional content-file &rest args)
  "Entry point for batch-mode type checking."
  (et-with-cache-source-if-enabled true-file
    (with-temp-buffer
      (insert-file-contents (or content-file true-file (error "No file provided")))
      (emacs-lisp-mode)
      (cl-loop for (path severity msg)
               in (et-result-diagnostics
                   (et-result-boundary
                    (apply #'et--process-exprs (et--buffer-exprs) args)))
               do (ignore-errors
                    (ignore-errors (et--traverse-buffer-expr path))
                    (let* ((start-line (line-number-at-pos))
                           (start-col (1+ (current-column))))
                      (ignore-errors (forward-sexp))
                      (princ (format "%s:%d:%d:%s:%s: %s: %s path=%s\n"
                                     true-file
                                     start-line start-col
                                     (line-number-at-pos) (1+ (current-column))
                                     severity msg path))))))))

(defun et--traverse-buffer-expr (path)
  (goto-char (point-min))
  (dotimes (_ (or (car path) 0)) (forward-sexp))
  (forward-comment (buffer-size))

  (dolist (idx (cdr path))
    ;; Skip whitespace and comments before looking at the next form
    (cond
     ((looking-at ",@?\\|`\\|#?']")
      (if (eq idx 1)
          (goto-char (match-end 0))
        (error "Only valid subexpr of quote is 1")))

     ((looking-at-p "[([]")
      (forward-char 1)
      (dotimes (_ idx) (forward-sexp))
      (forward-comment (buffer-size)))

     (t (error "Invalid expression container: %s" (thing-at-point 'char))))

    (forward-comment (buffer-size))))


;;;; Advice installation

(advice-add 'et-parse-type :before #'et--cache-capture-type)
(advice-add 'et--check :before #'et--cache-capture-function)
(advice-add 'et--check :around #'et--check-cached)

(advice-add 'et--sub-constraints-1 :around #'et--sub-constraints-cached)
(advice-add 'et--super-constraints-1 :around #'et--super-constraints-cached)
(advice-add 'et--subtype?-1 :around #'et--subtype?-cached)


;;; ============================================================
;;; Provide

(provide 'et-cache)


;;; et-cache.el ends here
