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
  (acc nil)     ; Accumulated values, in reverse order
  (vars nil)    ; `et-var's encountered so far, in order (index = position)
  (scoped nil)) ; Scoped arg-tuples encountered so far (compared by `equal')

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
         S:BINDS-OF S:SUBTRACT S:INFER S:EXTENDS S:EVAL S:SET)
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
        (et--hash-repr state
                       (or (plist-get plist :repr)
                           (error "Alias %s missing repr" name))
                       (cons name alias-stack))))))


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

(defun et-hash-items (items)
  "Hash ITEMS into a structural `et-hash'.

Each item must be an `et-type', `et-matcher', `et-repr', or a lisp
object; anything else signals an error. Items are hashed in order into a
single digest, each tagged by its kind so that, e.g., a type never
collides with an atom of the same shape."
  (let ((state (et--make-hashing-state)))
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
    (not (et-same-hash? (et Circ3A) (et Circ3B))))))


;;; ============================================================
;;; Caching
;;;; Variables

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


;;;; Disk operations

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


;;;; Macros

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

(defmacro et-cache-if (bool key ph-pred &rest body)
  (declare (indent 3))
  `(if ,bool
       (et-cache ,key ,ph-pred ,@body)
     ,@body))


;;;; Bonus functions

(defun et-clear-cache ()
  (interactive)
  (setq et-cache nil)
  (write-region "" nil et-cache-file))

(defun et-refresh-cache ()
  (interactive)
  (setq et-cache (et--load-cache)))


;;; ============================================================
;;; Provide

(provide 'et)


;;; et.el ends here
