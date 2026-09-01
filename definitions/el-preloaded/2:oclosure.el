;; -*- lexical-binding: t; -*-

(et-declare
 ;; NAME, DOCSTRING, and SLOTS are unevaluated structural specifications
 ;; that generate a new OClosure type together with its accessor,
 ;; predicate, and copier functions as global bindings named by their
 ;; (unevaluated) symbols. There is no checker shortcut for a form that
 ;; defines new global function bindings from unevaluated structural specs.
 (@check oclosure-define ($todo))
 ;; TYPE-AND-SLOTS names an OClosure type and binds its slots via
 ;; evaluated value forms; ARGS and BODY then define a new function
 ;; closed over those slots. The result is a value of a struct-like
 ;; OClosure type that the current type language has no way to declare
 ;; or represent, and neither $body nor $fn can check a lambda-like form
 ;; that also binds named slots from a type-specific list.
 (@check oclosure-lambda ($todo))
 (@def oclosure-type (oclosure: Any) Symbol))
