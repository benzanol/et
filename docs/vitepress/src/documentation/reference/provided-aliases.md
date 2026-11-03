---
title: "Provided aliases"
---

# Provided aliases

This is a collection of some of the more useful aliases built into the typesystem.  

```emacs-lisp
(et-defalias Boolean [] (or Nil True))
;; Most places that take a boolean can actually take anything.
;; Use Bool for these instead of Any to make it more clear
(et-defalias Bool [] Any)

;; All functions are a subtype of AnyFn
(et-defalias AnyFn [] (Function Never Any))
(et-defalias IdFn [T] (Function T T))
(et-defalias Sink [T] (Function (args T) Nil))

(et-defalias IntOrMarker [] (or Integer Marker))
(et-defalias NumOrMarker [] (or Number Marker))

(et-defalias List [(E = Any)] (or Nil (Cons E (List E))))
(et-defalias ListFresh [(E = Any)] (or Nil (ConsFresh E (ListFresh E))))

(et-defalias ListWithLast [E Last]
  (or (Cons Last Nil)
      (Cons E ListWithLast<E_Last>)))
(et-defalias AppendLast [T]
  (map-cases T
             [[X] &Cons<X_Nil> X]
             [[A B] &Cons<A_B> &Cons<A_AppendLast<B>>]))

(et-defalias Tree [(E = Any)] (or (List (Tree E)) E))
(et-defalias ConsTree [(E = Any)] (or (Cons (ConsTree E) (ConsTree E)) E))

(et-defalias Alist [K V] (List (Cons K V)))

(et-defalias PlistOf [K V] (or Nil (Cons K (Cons V (PlistOf K V)))))

(et-defalias PlistLike [K V]
  (or PlistOf<K_V>{V:>:Nil}
      Plist<K_V>))

(et-defalias PlistGet [P K]
  (map-cases P
             [[V] (&PlistOf K V) V|Nil]
             [[P] P (plist-get-r P K)]))

(et-defalias PlistPut [P K]
  (map-cases P
             [[V] (PlistOf K V) V|Nil]
             [[P] P (plist-get-w P K)]))

(et-defalias AlistPut [PL PK PV]
  (infer PL [K V] Alist<K_V> Alist<K|PK_V|PV>
         Never))

(et-defalias Sexp []
  (or Symbol String Number
      (Cons Sexp Sexp)
      (Vector Sexp)))
(et-defalias Sexps [] List<Sexp>)
```
