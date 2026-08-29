;; -*- lexical-binding: t; -*-

(et-declare
 (@def cl-generic-make-generalizer (name: String priority: Integer tagcode-function: AnyFn specializers-function: AnyFn) *cl--generic-generalizer)
 ;; cl-generic-define-generalizer is a declarative macro: NAME is never
 ;; evaluated -- it is used unevaluated as the symbol for the defconst
 ;; the macro creates. There is no checker shortcut for a form that
 ;; quotes one operand while defining a new global binding named by it.
 (@check cl-generic-define-generalizer ($todo))
 (@def cl-generic-function-options (generic: *cl--generic) List<Any>)
 (@def cl-generic-p (f: Any) Bool)
 (@def cl-generic-ensure-function (name: Symbol &optional noerror: Bool) *cl--generic)
 ;; cl-defgeneric is a declarative macro: NAME (possibly a (setf ...)
 ;; form) and ARGS are never evaluated -- they specify the name and
 ;; lambda-list of a new generic function. OPTIONS-AND-METHODS mixes
 ;; keyword options, an optional docstring, declare forms, embedded
 ;; :method clauses that expand into further cl-defmethod calls, and a
 ;; trailing default method body. There is no checker shortcut for a
 ;; form whose operands are interpreted structurally rather than
 ;; evaluated and checked as a simple leading-types-then-body sequence.
 (@check cl-defgeneric ($todo))
 ;; The result is a compiled dispatcher function whose calling
 ;; convention (its arity and per-argument dispatch behavior) is
 ;; determined by the runtime value of ARGS, a plain list of
 ;; argument-list syntax. The type language cannot yet express a
 ;; function type derived from an arbitrary runtime list value.
 (@def cl-generic-define (name: Symbol args: &List options: &List) Todo)
 ;; cl-generic-current-method-specializers only has meaning when
 ;; macroexpanded inside the lexical body of a cl-defmethod, where
 ;; cl--generic-lambda intercepts it via macroexpand-all-environment
 ;; and substitutes the method's specializer list; standalone it always
 ;; signals an error. There is no checker shortcut for a form whose
 ;; expansion depends on an ambient macro-expansion environment set up
 ;; by an enclosing form.
 (@check cl-generic-current-method-specializers ($todo))
 ;; cl-generic-define-context-rewriter is a declarative macro: NAME is
 ;; never evaluated (it is used as a property-list key), and ARGS/BODY
 ;; describe a lambda that is stored as a property and invoked later by
 ;; the dispatch machinery instead of being evaluated in place. There
 ;; is no checker shortcut for a form that stores a deferred lambda
 ;; under an unevaluated name.
 (@check cl-generic-define-context-rewriter ($todo))
 ;; cl-defmethod is a declarative macro: NAME (possibly a (setf ...)
 ;; form), an open-ended sequence of QUALIFIER atoms, and ARGS (a
 ;; lambda-list whose elements can be plain variables or (VAR TYPE)
 ;; dispatch pairs, plus &context specializers) are never evaluated --
 ;; they describe how to dispatch, not code to run. Checking BODY
 ;; correctly would require binding each dispatch variable to the type
 ;; given by its specializer, which the current shortcuts cannot do for
 ;; an open-ended, optionally qualified arglist.
 (@check cl-defmethod ($todo))
 (@def cl-generic-define-method (name: Symbol qualifiers: &List args: &List call-con: Nil|@curried|True function: Closure) Symbol)
 (@def cl-generic-call-method (generic: *cl--generic method: *cl--generic-method &optional fun: AnyFn?) AnyFn)
 (@def cl-method-qualifiers (method: *cl--generic-method) List<Any>)
 (@def cl-generic-apply (generic: *cl--generic args: &List) Any)
 (@def cl-generic-generalizers (specializer: Any) List<*cl--generic-generalizer>)
 (@def cl-generic-combine-methods (generic: *cl--generic methods: &List<*cl--generic-method>) AnyFn)
 (@def cl-no-next-method (generic: *cl--generic method: *cl--generic-method &rest args: &List) Never)
 (@def cl-no-applicable-method (generic: *cl--generic &rest args: &List) Never)
 (@def cl-no-primary-method (generic: *cl--generic &rest args: &List) Never)
 ;; cl-call-next-method only works when shadowed by a local cl-flet
 ;; binding that cl--generic-lambda installs inside the expansion of
 ;; cl-defmethod; the global function itself always signals an error.
 ;; The real argument and return types come from the enclosing method's
 ;; own dispatch signature, which the current shortcuts cannot look up.
 (@check cl-call-next-method ($todo))
 ;; cl-next-method-p only works when shadowed by a local cl-flet
 ;; binding that cl--generic-lambda installs inside the expansion of
 ;; cl-defmethod; the global function itself always signals an error.
 ;; The current shortcuts cannot express a callable whose behavior is
 ;; supplied entirely by an enclosing method definition.
 (@check cl-next-method-p ($todo))
 (@def cl-find-method (generic: Symbol qualifiers: &List specializers: &List) *cl--generic-method?)
 (@def cl-generic-all-functions (&optional type: Any) List<Symbol>))
