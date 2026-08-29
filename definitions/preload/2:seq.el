;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Macros

(et-declare
 ;; seq-doseq binds a variable to each element of a sequence and
 ;; evaluates the body. Requires a checker that handles binding,
 ;; looping, and body evaluation.
 (@check seq-doseq ($todo))
 ;; seq-let destructures a sequence into bindings using pcase patterns.
 ;; Requires a checker that handles destructuring bind and body evaluation.
 (@check seq-let ($todo))
 ;; seq-setq destructures a sequence into assignments using pcase patterns.
 ;; Requires a checker that handles destructuring assignment.
 (@check seq-setq ($todo)))


;;; ============================================================
;;; Basic sequence functions

(et-declare
 (@def seq-elt ([E] sequence: EltSeq<E> n: Integer) E)
 (@def seq-length (sequence: LenSeq) Integer)
 (@def seq-first ([E] sequence: EltSeq<E>) E)
 (@def seq-rest (sequence: [<= S &MapSeq]) S)
 (@def seq-do ([E (<= S MapSeq<E>)] function: fn1<E> sequence: S) S)
 (@def seq-each ([E (<= S MapSeq<E>)] function: fn1<E> sequence: S) S)
 (@def seq-do-indexed ([E] function: fn2<E~Integer> sequence: MapSeq<E>) Nil)
 (@def seqp (object: Any) Boolean)
 (@def seq-copy (sequence: [<= S &MapSeq]) (freshen-shallow S))
 (@def seq-subseq (sequence: [<= S &MapSeq] start: Integer &optional end: Integer?) (freshen-shallow S)))


;;; ============================================================
;;; Sequence operations

(et-declare
 (@def seq-map ([E R] function: fn1<E~R> sequence: MapSeq<E>) List<R>)
 (@def seq-map-indexed ([E R] function: fn2<E~Integer~R> sequence: MapSeq<E>) List<R>)
 (@def seq-mapn (function: AnyFn sequence: MapSeq<Any> &rest sequences: &List<MapSeq<Any>>) List)
 ;; todo: This is incorrect. seq-drop(Cons<1~Nil>) => Cons<1~Nil>
 (@def seq-drop (sequence: [<= S &MapSeq] n: Integer) S)
 (@def seq-take (sequence: [<= S MapSeq] n: Integer) (freshen-shallow S))
 (@def seq-drop-while ([E] pred: fn1<E> sequence: [<= S &MapSeq<E>]) S)
 (@def seq-take-while ([E] pred: fn1<E> sequence: [<= S &MapSeq<E>]) (freshen-shallow S))
 (@def seq-empty-p (sequence: LenSeq) Boolean)
 (@def seq-sort ([E] pred: fn2<E~E> sequence: [<= S MapSeq<E>]) S)
 ;; Returns a sequence of the same type as SEQUENCE. The type language
 ;; cannot express value-dependent preservation of the input container type.
 (@def seq-sort-by ([E] function: fn1<E> pred: fn2<Any~Any> sequence: MapSeq<E>) Todo)
 ;; Returns a sequence of the same type as SEQUENCE. The type language
 ;; cannot express value-dependent preservation of the input container type.
 (@def seq-reverse ([E] sequence: MapSeq<E>) Todo)
 ;; The return type depends on the TYPE symbol argument. The type language
 ;; cannot express value-dependent return types based on a symbol argument.
 (@def seq-concatenate (type: @vector|@string|@list &rest sequences: &List<MapSeq<Any>>) Todo)
 (@def seq-into-sequence (sequence: [S]) S)
 ;; The return type depends on the TYPE symbol argument. The type language
 ;; cannot express value-dependent return types based on a symbol argument.
 (@def seq-into (sequence: MapSeq<Any> type: @vector|@string|@list) Todo)
 (@def seq-filter ([E] pred: fn1<E> sequence: MapSeq<E>) List<E>)
 (@def seq-remove ([E] pred: fn1<E> sequence: MapSeq<E>) List<E>)
 ;; Returns a sequence of the same type as SEQUENCE. The type language
 ;; cannot express value-dependent preservation of the input container type.
 (@def seq-remove-at-position ([E] sequence: MapSeq<E> n: Integer) Todo)
 (@def seq-reduce ([E A] function: fn2<A~E~A> sequence: MapSeq<E> initial-value: A) A)
 (@def seq-every-p ([E] pred: fn1<E> sequence: MapSeq<E>) Boolean)
 (@def seq-some ([E R] pred: fn1<E~R> sequence: MapSeq<E>) R?)
 (@def seq-find ([E] pred: fn1<E> sequence: MapSeq<E> &optional default: [D]) E|D)
 (@def seq-count ([E] pred: fn1<E> sequence: MapSeq<E>) Integer)
 (@def seq-contains ([E] sequence: MapSeq<E> elt: E &optional testfn: fn2<E~E>?) E?)
 (@def seq-contains-p ([E] sequence: MapSeq<E> elt: E &optional testfn: fn2<E~E>?) Bool)
 (@def seq-set-equal-p ([E] sequence1: MapSeq<E> sequence2: MapSeq<E> &optional testfn: fn2<E~E>?) Boolean)
 (@def seq-position ([E] sequence: MapSeq<E> elt: E &optional testfn: fn2<E~E>?) Integer?)
 (@def seq-positions ([E] sequence: MapSeq<E> elt: E &optional testfn: fn2<E~E>?) List<Integer>)
 (@def seq-uniq ([E] sequence: MapSeq<E> &optional testfn: fn2<E~E>?) List<E>)
 (@def seq-mapcat ([E R] function: fn1<E~MapSeq<R>> sequence: MapSeq<E> &optional type: @vector|@string|@list?) List<R>)
 ;; Returns a list of sub-sequences whose type matches SEQUENCE. The
 ;; element type of the outer list is value-dependent on the input container type.
 (@def seq-partition ([E] sequence: MapSeq<E> n: Integer) Todo)
 (@def seq-union ([E] sequence1: MapSeq<E> sequence2: MapSeq<E> &optional testfn: fn2<E~E>?) List<E>)
 (@def seq-intersection ([E] sequence1: MapSeq<E> sequence2: MapSeq<E> &optional testfn: fn2<E~E>?) List<E>)
 (@def seq-difference ([E] sequence1: MapSeq<E> sequence2: MapSeq<E> &optional testfn: fn2<E~E>?) List<E>)
 (@def seq-group-by ([E R] function: fn1<E~R> sequence: MapSeq<E>) Alist<R~List<E>>)
 (@def seq-min ([(<= E NumOrMarker)] sequence: MapSeq<E>) E)
 (@def seq-max ([(<= E NumOrMarker)] sequence: MapSeq<E>) E)
 (@def seq-random-elt ([E] sequence: EltSeq<E>) E)
 ;; Returns a list of sub-sequences whose type matches SEQUENCE. The
 ;; element type of the outer list is value-dependent on the input container type.
 (@def seq-split ([E] sequence: MapSeq<E> length: Integer) Todo)
 (@def seq-keep ([E R] function: fn1<E~R> sequence: MapSeq<E>) List<R>))


;;; ============================================================
