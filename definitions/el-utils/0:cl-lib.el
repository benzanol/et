;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Cl-loop

(et-define-check-macro $clloop-1 (body)
  `(et:def--clloop-parse-1 ,body))

(et-defun et:def--clloop-parse-clauses (exprs: Sexps) Cons<Sexps_Sexps>|Nil
  (when-let* ((result (et:def--clloop-parse-2 exprs)))
    (let* ((rest (car result))
           (clause (take (- (length exprs) (length rest)) exprs)))
      (if (eq 'and (car rest))
          (when-let* ((rec (et:def--clloop-parse-clauses (cdr rest))))
            (cons (cons clause (car rec)) (cdr rec)))
        (cons (list clause) rest)))))

(pcase-defmacro et:def--clloop-clauses (clauses rest)
  (list 'app #'et:def--clloop-parse-clauses
        (list '\` (cons (list '\, clauses) (list '\, rest)))))

(defmacro et:def--clloop-pcase (body &rest pats)
  (declare (indent 1))
  (cl-loop for (pat chk) in pats
           for expanded = (macroexpand-all `(et-chk ,chk))
           collect `(,pat (cons rest (lambda () ,expanded))) into exprs
           finally return `(pcase ,body ,@exprs)))

(et-defvar et:def--clloop-vars Alist<Var_EtType> nil)

(et-defun et:def--clloop-parse-2 (body: Sexps) Cons<Sexps_EtCheckFn>|Nil
  (let* ((rest nil))
    (et:def--clloop-pcase body
      ;; Empty
      ('nil ($nil))
      ;; do
      (`(do ,expr . ,rest)
       ($progn ($exp expr) ($clloop-1 rest)))
      (`(return ,expr . ,rest)
       ($progn ($exp expr) ($clloop-1 rest))) ;todo
      ;; Assignment
      (`(for ,pat = ,expr1 . ,(or `(then . ,rest) rest))
       ($cl-bind pat ($exp expr1) ($clloop-1 rest)))
      ;; Looping
      (`(for ,var ,(or 'from 'upfrom 'downfrom) ,start
             ,(or 'to 'upto 'downto 'above 'below) ,end
             . ,(or `(by ,by . ,rest) rest))
       ;; todo: start/by can be non-Integers, but if so, neither is var
       ($progn ($expect ($type Integer) ($exp start))
               ($expect ($type Number) ($exp end))
               ($if-eval by ($expect ($type Integer) ($exp by)) ($nil))
               ($bind [_ var ($type Integer)] ($clloop-1 rest))))
      (`(for ,pat ,(and in (or 'in 'on 'in-ref)) ,list
             . ,(or `(by ,by . ,rest) rest))
       ($cl-bind pat ($infer [T] &List<T> T ($exp list)) ($clloop-1 rest)))
      (`(for ,pat ,(and across (or 'across 'across-ref)) ,arr . ,rest)
       ($cl-bind pat ($infer [T] ArefSeq<T> T ($exp arr)) ($clloop-1 rest)))
      (`(repeat ,times . ,rest)
       ($progn ($expect ($type Integer) ($exp times)) ($clloop-1 rest)))
      (`(while ,cond . ,rest) ($loop ($if ($exp cond) ($exp rest) ($nil)))) ;todo
      (`(until ,cond . ,rest) ($loop ($if ($exp cond) ($nil) ($exp rest)))) ;todo
      ;; Existence
      (`(,(or 'always 'never 'thereis) ,expr) ($progn ($exp expr) ($type Boolean)))
      ;; Conditional: todo
      (`(if ,cond . ,(et:def--clloop-clauses
                      thens
                      (or `(else . ,(et:def--clloop-clauses elses rest))
                          rest)))
       ($if ($exp cond) ($nil) ($nil)))
      (`(when ,cond . ,(et:def--clloop-clauses thens rest))
       ($nil))
      (`(unless ,cond . ,(et:def--clloop-clauses elses rest))
       ($nil))
      ;; Collecting
      (`(collect ,expr into ,(and (pred symbolp) var) . ,rest)
       ($eval (if-let* ((exist (alist-get var et:def--clloop-vars)))
                  (et-chk ($expect ($infer [T] List<T> T ($eval exist)) ($exp expr))
                          ($clloop-1 rest))
                (let* ((vartype (et-reify-type (et List ,(et-chk ($exp expr))))))
                  (setf (alist-get var et:def--clloop-vars) vartype)
                  (et-chk ($bind [_ var ($eval vartype)]
                                 ($clloop-1 rest)))))))
      (`(collect ,expr . ,rest)
       ($infer [T] T List<T> ($exp expr)))
      ;; Appending
      (`(,(or 'append 'nconc) ,expr into ,(and (pred symbolp) var) . ,rest)
       ($eval (if-let* ((exist (alist-get var et:def--clloop-vars)))
                  (et-chk ($expect ($infer [T] List<T> List<T> ($eval exist)) ($exp expr))
                          ($clloop-1 rest))
                (let* ((vartype (et-reify-type ,(et-chk ($infer [T] List<T> List<T> ($exp expr))))))
                  (setf (alist-get var et:def--clloop-vars) vartype)
                  (et-chk ($bind [_ var ($eval vartype)]
                                 ($clloop-1 rest)))))))
      (`(,(or 'append 'nconc) ,expr . ,rest)
       ($infer [T] T List<T> ($exp expr)))
      ;; Final
      (`(finally return ,expr)
       ($exp expr))
      (`(finally . ,(or `(do . ,exprs) exprs))
       ($prog1 ($nil) ($exps exprs))))))


(et-defun et:def--clloop-parse-1 (body: Sexps) EtType
  (funcall (cdr (or (et:def--clloop-parse-2 body)
                    (error "Failed to parse cl-loop: %s"
                           (if (length< body 4) body
                             (append (take 3 body) (list '...))))))))

(et-defun et:def--clloop-parse (body: Sexps) EtType
  (let* ((et:def--clloop-vars nil))
    (et:def--clloop-parse-1 body)))

;; (et-declare
;;  (@check cl-loop
;;          ($eval (et:def--clloop-parse (cdr (et-cur-expr))))))


;;; ============================================================
;;; Local functions

(defconst et:def--cl-flet-properties
  '(et-symbol-func-type et-function-parameters et-function-props et-checker)
  "Symbol properties temporarily rebound by the `cl-flet' checker.")

(et-defun et:def--cl-flet-remprop (name: Symbol prop: Symbol) Nil
  "Remove PROP from NAME without disturbing its other properties."
  (setplist name
            (cl-loop for (key value) on (symbol-plist name) by #'cddr
                     unless (eq key prop)
                     append (list key value)))
  nil)

(et-defun et:def--cl-flet-snapshot (name: Symbol) List
  "Return NAME's current local-function properties, including absence."
  ;; Resolve a deferred global declaration before shadowing it so restoring
  ;; the snapshot cannot accidentally discard that declaration.
  (et-run-deferred-declare name)
  (let* ((plist (symbol-plist name)))
    (cl-loop for prop in et:def--cl-flet-properties
             collect (list prop (not (null (plist-member plist prop)))
                           (get name prop)))))

(et-defun et:def--cl-flet-restore (name: Symbol snapshot: List) Nil
  "Restore NAME's function properties from SNAPSHOT."
  (dolist (entry snapshot)
    (pcase-let ((`(,prop ,present ,value) entry))
      (if present (put name prop value)
        (et:def--cl-flet-remprop name prop)))))

(et-defun et:def--cl-flet-install (name: Symbol decls: EtFuncDeclarations) Nil
  "Install DECLS on NAME for the dynamic extent of a `cl-flet' check."
  ;; A symbol can have only one call strategy.  In particular, an outer
  ;; custom checker must not win over a local ordinary function type.
  (et:def--cl-flet-remprop name 'et-symbol-func-type)
  (et:def--cl-flet-remprop name 'et-checker)
  (et-symbol-set-func-decls name decls))

(et-defun et:def--cl-flet-inferred-decls
    (params: EtFuncParameters<Var> props: PlistOf<Var_Any> body-path: EtPath)
    EtFuncDeclarations
  "Infer an unannotated local function at BODY-PATH."
  (let* ((input (et-untyped-func-input params))
         (provisional (et-dt 'Function input (et-any)))
         (ret (et-check-function-body params provisional body-path)))
    (list :parameters params :props props
          :definition (et-dt 'Function input ret))))

(et-defun et:def--cl-flet-binding-decls (binding: Sexp rel: EtPath)
          Cons<Symbol_EtFuncDeclarations>
  "Parse and check one `cl-flet' BINDING located at REL."
  (pcase binding
    (`(,(and name (pred symbolp)) ,_function)
     (let* ((type (et-check-at (et AnyFn) rel 1)))
       (cons name (list :parameters nil :props nil :definition type))))
    (`(,(and name (pred symbolp)) ,(and arglist (pred listp)) . ,body)
     (let* ((params (et-at (append rel '(1)) (et-parse-arglist arglist)))
            (declare-path (et:func--declares-path body))
            (decls (et-at rel
                     (et-at-offset 2
                       (et-find-and-parse-func-decls params body))))
            (props (or (plist-get decls :props) nil))
            (definition (plist-get decls :definition))
            (body-offset (if declare-path (+ 3 (car declare-path)) 2))
            (body-path (append rel (list body-offset))))
       (cond
        ((et:type-p definition)
         (unless (plist-get props :skip)
           (et-check-function-body params definition body-path)))
        ((functionp definition))
        ((null definition)
         (setq decls (et:def--cl-flet-inferred-decls params props body-path)))
        (t (et-fatal rel "Invalid local function declaration")))
       (cons name (or decls
                      (list :parameters params :props props
                            :definition definition)))))
    (_ (et-fatal rel "Expected (NAME ARGLIST BODY...) or (NAME FUNCTION)"))))

(et-defun et:def--check-cl-flet () EtType
  "Check the current `cl-flet' form with temporary symbol properties."
  (pcase (cdr (et-cur-expr))
    (`(,(and bindings (pred listp)) . ,_body)
     (let* ((names
             (cl-loop for binding in bindings
                      for name = (car-safe binding)
                      unless (symbolp name)
                      do (et-fatal (list 1 (cl-position binding bindings))
                                   "Local function name must be a symbol")
                      collect name))
            (_ (unless (eq (length names) (length (delete-dups (copy-sequence names))))
                 (et-fatal 1 "Duplicate local function name")))
            (snapshots
             (cl-loop for name in names
                      collect (cons name (et:def--cl-flet-snapshot name)))))
       (unwind-protect
           (let* ((decls
                   ;; `cl-flet' definitions do not see one another: derive
                   ;; every definition before installing any of them.
                   (cl-loop for binding in bindings
                            for idx upfrom 0
                            collect (et:def--cl-flet-binding-decls
                                     binding (list 1 idx)))))
             (dolist (entry decls)
               (et:def--cl-flet-install (car entry) (cdr entry)))
             (et-chk ($tail 2)))
         (dolist (entry snapshots)
           (et:def--cl-flet-restore (car entry) (cdr entry))))))
    (_ (et-fatal nil "Expected (cl-flet (BINDINGS...) BODY...)"))))

(et-declare
 (@check cl-flet ($eval (et:def--check-cl-flet))))

(et-defun et:def--cl-labels-binding-plan (binding: Sexp rel: EtPath) List
  "Parse one `cl-labels' BINDING and return its checking plan."
  (pcase binding
    (`(,(and name (pred symbolp)) ,_function)
     (list :name name :rel rel :kind 'expression
           :decls (list :parameters nil :props nil
                        :definition (et AnyFn))))
    (`(,(and name (pred symbolp)) ,(and arglist (pred listp)) . ,body)
     (let* ((params (et-at (append rel '(1)) (et-parse-arglist arglist)))
            (declare-path (et:func--declares-path body))
            (decls (et-at rel
                     (et-at-offset 2
                       (et-find-and-parse-func-decls params body))))
            (props (or (plist-get decls :props) nil))
            (definition (plist-get decls :definition))
            (body-offset (if declare-path (+ 3 (car declare-path)) 2))
            (body-path (append rel (list body-offset)))
            (kind (cond ((et:type-p definition) 'typed)
                        ((functionp definition) 'checker)
                        ((null definition) 'inferred)
                        (t (et-fatal rel "Invalid local function declaration"))))
            (provisional
             (if (eq kind 'inferred)
                 (list :parameters params :props props
                       :definition
                       (et-dt 'Function (et-untyped-func-input params) (et Any)))
               (or decls
                   (list :parameters params :props props
                         :definition definition)))))
       (list :name name :rel rel :kind kind :params params :props props
             :body-path body-path :decls provisional)))
    (_ (et-fatal rel "Expected (NAME ARGLIST BODY...) or (NAME FUNCTION)"))))

(et-defun et:def--cl-labels-final-decls (plan: List) EtFuncDeclarations
  "Check the definition described by a `cl-labels' PLAN."
  (let* ((kind (plist-get plan :kind))
         (decls (plist-get plan :decls)))
    (pcase kind
      ('expression
       (list :parameters nil :props nil
             :definition
             (et-check-at (et AnyFn) (plist-get plan :rel) 1)))
      ('inferred
       (et:def--cl-flet-inferred-decls
        (plist-get plan :params) (plist-get plan :props)
        (plist-get plan :body-path)))
      ('typed
       (unless (plist-get (plist-get plan :props) :skip)
         (et-check-function-body
          (plist-get plan :params) (plist-get decls :definition)
          (plist-get plan :body-path)))
       decls)
      ('checker decls)
      (_ (error "Invalid cl-labels checking plan")))))

(et-defun et:def--check-cl-labels () EtType
  "Check `cl-labels', making all local functions mutually recursive."
  (pcase (cdr (et-cur-expr))
    (`(,(and bindings (pred listp)) . ,_body)
     (let* ((names
             (cl-loop for binding in bindings
                      for name = (car-safe binding)
                      unless (symbolp name)
                      do (et-fatal (list 1 (cl-position binding bindings))
                                   "Local function name must be a symbol")
                      collect name))
            (_ (unless (eq (length names)
                           (length (delete-dups (copy-sequence names))))
                 (et-fatal 1 "Duplicate local function name")))
            (snapshots
             (cl-loop for name in names
                      collect (cons name (et:def--cl-flet-snapshot name)))))
       (unwind-protect
           (let* ((plans
                   (cl-loop for binding in bindings
                            for idx upfrom 0
                            collect (et:def--cl-labels-binding-plan
                                     binding (list 1 idx)))))
             ;; Install every signature before checking any definition.  This
             ;; is the essential difference from `cl-flet': each local body
             ;; can call itself and every other local function.
             (dolist (plan plans)
               (et:def--cl-flet-install
                (plist-get plan :name) (plist-get plan :decls)))
             ;; Derive all final signatures against the same provisional
             ;; environment, then publish them together for the outer body.
             (let* ((finals
                     (mapcar #'et:def--cl-labels-final-decls plans)))
               (cl-mapc (lambda (plan decls)
                          (et:def--cl-flet-install
                           (plist-get plan :name) decls))
                        plans finals))
             (et-chk ($tail 2)))
         (dolist (entry snapshots)
           (et:def--cl-flet-restore (car entry) (cdr entry))))))
    (_ (et-fatal nil "Expected (cl-labels (BINDINGS...) BODY...)"))))

(et-declare
 (@check cl-labels ($eval (et:def--check-cl-labels))))


;;; ============================================================
;;; Generalized variables

(et-declare
 (@check cl-incf
         ($pcase
          `(,(and var (pred symbolp)))
          ($exp `(setq ,var (1+ ,var)))))
 (@check cl-decf
         ($pcase
          `(,(and var (pred symbolp)))
          ($exp `(setq ,var (1- ,var)))))
 (@check cl-callf
         ($pcase
          `(,(and fn (pred symbolp)) ,(and var (pred symbolp)) . ,args)
          ($exp `(setq ,var (,fn ,var ,@args)))))
 (@check cl-pushnew))

(et-declare
 (@check cl-rotatef ($expand))
 (@check cl-psetf ($expand)))


;;; ============================================================
;;; Multiple values

(et-declare
 (@def cl-values (&rest values: [T]) (freshen-shallow T))
 (@def cl-values-list ([(L <: &List)] list: L) L)
 (@def cl-multiple-value-list (expression: [T]) T)
 (@def cl-multiple-value-apply ([A R] function: (fn A R) expression: A) R)
 (@def cl-multiple-value-call
       ([A R] function: (fn (eval et-tuple-to-tailed A) R)
        &rest arguments: A)
       R)
 (@def cl-nth-value ([E] n: Integer list: &List<E>) E?))


;;; ============================================================
;;; Declarations

(et-declare
 (@def cl-proclaim (spec: Any) Nil)
 (@check cl-declaim))


;;; ============================================================
;;; Assertions

(et-declare
 (@check cl-assert
         ($pcase
          `(,_condition . ,rest)
          ($let [condition ($at 1)]
                [success ($eval (et-supersect condition (et NonNil)))]
                ($progn
                 ($if-eval
                  (and (cdr rest)
                       (not (et-never-p
                             (et-supersect condition (et Nil)))))
                  ($ignore
                   ($when-is ($eval condition) ($type Nil)
                             ($progn
                              ($expect ($type String?) ($at 3))
                              ($ignore ($tail 4))
                              ($type Never))))
                  ($type Nil))
                 ($if-eval
                  (et-never-p success) ($eval success)
                  ($eval
                   (et-apply-type-narrows success)
                   (et Nil))))))))


;;; ============================================================
;;; Numbers

(et-declare
 (@def cl-floatp-safe ([T] object: T) (is-a? T Number))
 (@def cl-plusp (number: Number) Boolean)
 (@def cl-minusp (number: Number) Boolean)
 (@def cl-oddp (integer: Integer) Boolean)
 (@def cl-evenp (integer: Integer) Boolean)
 (@def cl-digit-char-p (char: Integer &optional radix: Integer?) Integer?))


;;; ============================================================
;;; Sequence functions

(et-declare
 (@def cl-copy-seq (arg: [S <: &LenSeq]) (freshen-shallow S))
 (@def cl-mapcar)
 (@def cl-svref ([T] array: ArefSeq<T> idx: Integer) T))


;;; ============================================================
;;; List functions

(et-declare
 (@def cl-first ([T] list: (or Nil^{T:>:Nil} (&Cons T Any))) T)
 (@def cl-second ([T] list: &List<T>) T?)
 (@def cl-rest ([T] list: (or Nil^{T:>:Nil} &Cons<Any_T>)) T)
 (@def cl-third ([T] list: &List<T>) T?)
 (@def cl-fourth ([T] list: &List<T>) T?)
 (@def cl-fifth ([T] x: &List<T>) T?)
 (@def cl-sixth ([T] x: &List<T>) T?)
 (@def cl-seventh ([T] x: &List<T>) T?)
 (@def cl-eighth ([T] x: &List<T>) T?)
 (@def cl-ninth ([T] x: &List<T>) T?)
 (@def cl-tenth ([T] x: &List<T>) T?)
 (@def cl-caaar)
 (@def cl-caadr ([T] list: &List<&Cons<T_Any>>) T?)
 (@def cl-cadar)
 (@def cl-caddr ([T] list: &List<T>) T?)
 (@def cl-cdaar)
 (@def cl-cdadr)
 (@def cl-cddar)
 (@def cl-cdddr)
 (@def cl-caaaar)
 (@def cl-caaadr)
 (@def cl-caadar)
 (@def cl-caaddr)
 (@def cl-cadaar)
 (@def cl-cadadr)
 (@def cl-caddar)
 (@def cl-cadddr)
 (@def cl-cdaaar)
 (@def cl-cdaadr)
 (@def cl-cdadar)
 (@def cl-cdaddr)
 (@def cl-cddaar)
 (@def cl-cddadr)
 (@def cl-cdddar)
 (@def cl-cddddr)
 (@check cl-list*
         ($pcase
          `(,_last) ($at 1)
          `(,first . ,rest)
          ($exp `(cons ,first (cl-list* ,@rest)))))
 (@def cl-ldiff ([T] list: &List<T> sublist: &List<T>) ListFresh<T>)
 (@def cl-copy-list)
 (@def cl-adjoin)
 (@def cl-subst)
 (@def cl-acons ([K V A] key: K value: V alist: A)
       ConsFresh<ConsFresh<K_V>_A>)
 (@def cl-pairlis))


;;; ============================================================
;;; Miscellaneous

(et-declare
 (@def cl-old-struct-compat-mode (&optional arg: Any) Boolean)
 (@def cl-constantly ([T] value: T) (fn &List T)))


;;; ============================================================
