;; -*- lexical-binding: t; -*-

(et-declare
 (@def cl-endp (x: Nil|&Cons) Boolean)
 (@def cl-reduce)
 (@def cl-fill
       ([E (<= S List<E>|FillableArray<E>)] cl-seq: S cl-item: E
        &key start: Integer? end: Integer?)
       S)
 (@def cl-replace
       ([E (<= S List<E>|AsetSeq<E>)] cl-seq1: S cl-seq2: &LenSeq<E>
        &key start1: Integer? end1: Integer?
        start2: Integer? end2: Integer?)
       S)
 (@def cl-remove
       ([E K (<= S ConcatSeq<E>)] cl-item: Any cl-seq: S
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-remove-if
       ([E K (<= S ConcatSeq<E>)] cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-remove-if-not
       ([E K (<= S ConcatSeq<E>)] cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-delete
       ([E K (<= S ConcatSeq<E>)] cl-item: Any cl-seq: S
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-delete-if
       ([E K (<= S ConcatSeq<E>)] cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-delete-if-not
       ([E K (<= S ConcatSeq<E>)] cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-remove-duplicates)
 (@def cl-delete-duplicates
       ([E K (<= S ConcatSeq<E>)] cl-seq: S
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-substitute)
 (@def cl-substitute-if)
 (@def cl-substitute-if-not)
 (@def cl-nsubstitute
       ([E K (<= S List<E>|AsetSeq<E>)] cl-new: E cl-old: Any seq: S
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-nsubstitute-if
       ([E K (<= S List<E>|AsetSeq<E>)] cl-new: E
        cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-nsubstitute-if-not
       ([E K (<= S List<E>|AsetSeq<E>)] cl-new: E
        cl-pred: (fn (Args K) Bool) cl-list: S
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        count: Integer? start: Integer? end: Integer? from-end: Bool)
       S)
 (@def cl-find
       ([E K] cl-item: Any cl-seq: &LenSeq<E>
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       E?)
 (@def cl-find-if
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       E?)
 (@def cl-find-if-not
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       E?)
 (@def cl-position
       ([E K] cl-item: Any cl-seq: &LenSeq<E>
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       Integer?)
 (@def cl-position-if
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       Integer?)
 (@def cl-position-if-not
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer? from-end: Bool)
       Integer?)
 (@def cl-count
       ([E K] cl-item: Any cl-seq: &LenSeq<E>
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer?)
       Integer)
 (@def cl-count-if
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer?)
       Integer)
 (@def cl-count-if-not
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &LenSeq<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E})
        start: Integer? end: Integer?)
       Integer)
 (@def cl-mismatch
       ([E K] cl-seq1: &LenSeq<E> cl-seq2: &LenSeq<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start1: Integer? end1: Integer?
        start2: Integer? end2: Integer? from-end: Bool)
       Integer?)
 (@def cl-search
       ([E K] cl-seq1: &LenSeq<E> cl-seq2: &LenSeq<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E})
        start1: Integer? end1: Integer?
        start2: Integer? end2: Integer? from-end: Bool)
       Integer?)
 (@def cl-sort
       ([E K (<= S OrdSeq<E>)] cl-seq: S cl-pred: (fn (Args K K) Bool)
        &key key: (or (fn (Args E) K) Nil&{K:=:E}))
       S)
 (@def cl-stable-sort
       ([E K (<= S OrdSeq<E>)] cl-seq: S cl-pred: (fn (Args K K) Bool)
        &key key: (or (fn (Args E) K) Nil&{K:=:E}))
       S)
 (@def cl-merge)
 (@def cl-member
       ([E K] cl-item: Any cl-list: &List<E>
        &key test: (or Nil (fn (Args Any K) Bool))
        test-not: (or Nil (fn (Args Any K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-member-if
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &List<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-member-if-not
       ([E K] cl-pred: (fn (Args K) Bool) cl-list: &List<E>
        &key key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-assoc
       ([K V X] cl-item: Any cl-alist: &Alist<K~V>
        &key test: (or Nil (fn (Args Any X) Bool))
        test-not: (or Nil (fn (Args Any X) Bool))
        key: (or (fn (Args K) X) Nil&{X:=:K}))
       &Cons<K~V>?)
 (@def cl-assoc-if
       ([K V X] cl-pred: (fn (Args X) Bool) cl-list: &Alist<K~V>
        &key key: (or (fn (Args K) X) Nil&{X:=:K}))
       &Cons<K~V>?)
 (@def cl-assoc-if-not
       ([K V X] cl-pred: (fn (Args X) Bool) cl-list: &Alist<K~V>
        &key key: (or (fn (Args K) X) Nil&{X:=:K}))
       &Cons<K~V>?)
 (@def cl-rassoc
       ([K V X] cl-item: Any cl-alist: &Alist<K~V>
        &key test: (or Nil (fn (Args Any X) Bool))
        test-not: (or Nil (fn (Args Any X) Bool))
        key: (or (fn (Args V) X) Nil&{X:=:V}))
       &Cons<K~V>?)
 (@def cl-rassoc-if
       ([K V X] cl-pred: (fn (Args X) Bool) cl-list: &Alist<K~V>
        &key key: (or (fn (Args V) X) Nil&{X:=:V}))
       &Cons<K~V>?)
 (@def cl-rassoc-if-not
       ([K V X] cl-pred: (fn (Args X) Bool) cl-list: &Alist<K~V>
        &key key: (or (fn (Args V) X) Nil&{X:=:V}))
       &Cons<K~V>?)
 (@def cl-union
       ([E K] cl-list1: &List<E> cl-list2: &List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-nunion
       ([E K] cl-list1: List<E> cl-list2: List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       List<E>)
 (@def cl-intersection
       ([E K] cl-list1: &List<E> cl-list2: &List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-nintersection
       ([E K] cl-list1: List<E> cl-list2: List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       List<E>)
 (@def cl-set-difference
       ([E K] cl-list1: &List<E> cl-list2: &List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-nset-difference
       ([E K] cl-list1: List<E> cl-list2: List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       List<E>)
 (@def cl-set-exclusive-or
       ([E K] cl-list1: &List<E> cl-list2: &List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       &List<E>)
 (@def cl-nset-exclusive-or
       ([E K] cl-list1: List<E> cl-list2: List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       List<E>)
 (@def cl-subsetp
       ([E K] cl-list1: &List<E> cl-list2: &List<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       Boolean)
 (@def cl-subst-if)
 (@def cl-subst-if-not)
 (@def cl-nsubst)
 (@def cl-nsubst-if)
 (@def cl-nsubst-if-not)
 (@def cl-sublis)
 (@def cl-nsublis)
 (@def cl-tree-equal
       ([E K] cl-x: &ConsTree<E> cl-y: &ConsTree<E>
        &key test: (or Nil (fn (Args K K) Bool))
        test-not: (or Nil (fn (Args K K) Bool))
        key: (or (fn (Args E) K) Nil&{K:=:E}))
       Boolean))
