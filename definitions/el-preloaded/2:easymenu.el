;; -*- lexical-binding: t; -*-

(et-declare
 (@def easy-menu-intern (s: [<= T String|Symbol]) (extends? T String Symbol T))
 ;; SYMBOL is never evaluated; it is used as a name to `defvar' and later
 ;; `defalias' as both a variable and a command. The checker shortcuts only
 ;; support checking evaluated operands against types, not binding a name
 ;; from an unevaluated operand.
 (@check easy-menu-define ($todo))
 (@def easy-menu-binding (menu: Any &optional item-name: String?) Cons<@menu-item~Cons<Any~Cons<Any~Any>>>)
 (@def easy-menu-do-define (symbol: Symbol maps: Any doc: String? menu: Cons<String~List<Sexp>>) Nil)
 (@def easy-menu-filter-return (menu: Any &optional name: String?) Any)
 (@def easy-menu-create-menu (menu-name: String menu-items: &List<Sexp>) Any)
 (@def easy-menu-convert-item (item: Sexp) Cons<Symbol~Any>)
 (@def easy-menu-convert-item-1 (item: Sexp) Cons<Symbol~Any>)
 (@def easy-menu-define-key (menu: Symbol|Cons<Any~Any> key: Any item: Any &optional before: String|Symbol?) Nil)
 (@def easy-menu-name-match (name: String|Symbol item: Any) Bool)
 (@def easy-menu-always-true-p (x: Any) Bool)
 (@def easy-menu-make-symbol (callback: Any &optional noexp: Bool) NonNilSymbol)
 (@def easy-menu-change (path: &List<String> name: String items: &List<Sexp> &optional before: String|Symbol? map: Any) Nil)
 (@def easy-menu-remove (&rest _ignore: &List) Nil)
 (@def easy-menu-add (&rest _ignore: &List) Nil)
 (@def add-submenu (menu-path: &List<String> submenu: Sexp &optional before: String|Symbol? in-menu: Any) Nil)
 (@def easy-menu-add-item (map: Any path: &List<String>? item: Any &optional before: String|Symbol?) Nil)
 (@def easy-menu-item-present-p (map: Any path: &List<String>? name: String) Nil|Cons<String~Any>)
 (@def easy-menu-remove-item (map: Any path: &List<String>? name: String) Nil|Cons<String~Any>)
 (@def easy-menu-return-item (menu: Any name: [<= T String|Symbol]) Nil|Cons<T~Any>)
 (@def easy-menu-lookup-name (map: Any name: String|Symbol) Any)
 (@def easy-menu-get-map (map: Any path: &List<String>? &optional to-modify: String|Symbol?) Any))
