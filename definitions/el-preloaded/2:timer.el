;; -*- lexical-binding: t; -*-

(et-declare
 ;; Timer objects are cl-defstruct instances declared with (:type vector)
 ;; and no distinguishing tag, so `type-of' reports them as plain vectors.
 ;; The type language has no opaque tagged-vector type to represent a
 ;; timer precisely without also matching arbitrary vectors.
 (@def timer-create () Todo)
 (@def timerp (object: Any) Boolean)
 (@def timer-set-time (timer: [T] time: Timestamp &optional delta: Number?) T)
 (@def timer-set-idle-time (timer: [T] secs: Timestamp &optional repeat: Bool) T)
 (@def timer-next-integral-multiple-of-time (time: Timestamp secs: Number) TimeOutput)
 (@def timer-relative-time (time: Timestamp secs: Number &optional usecs: Integer? psecs: Integer?) TimeOutput)
 ;; The timer parameter is only used to read and overwrite its time field
 ;; here; it is not the value returned by this function, so the identity
 ;; generic used elsewhere in this file does not apply. See timer-create
 ;; for why the timer type itself cannot be expressed.
 (@def timer-inc-time (timer: Todo secs: Number &optional usecs: Integer? psecs: Integer?) TimeOutput)
 (@def timer-set-function (timer: [T] function: fn<&List~Any> &optional args: &List?) T)
 ;; The timer and reuse-cell parameters both embed the unrepresentable
 ;; timer type: reuse-cell is a mutable cons cell spliced directly into
 ;; `timer-list' or `timer-idle-list', whose car holds a timer. See
 ;; timer-create for why the timer type itself cannot be expressed.
 (@def timer-activate (timer: Todo &optional triggered-p: Bool reuse-cell: Cons<Todo~List<Todo>>?) Nil)
 ;; Same limitation as timer-activate: the timer type cannot be expressed,
 ;; and reuse-cell is a mutable cons cell holding a timer.
 (@def timer-activate-when-idle (timer: Todo &optional dont-wait: Bool reuse-cell: Cons<Todo~List<Todo>>?) Nil)
 ;; The timer parameter embeds the unrepresentable timer type. See
 ;; timer-create for why the timer type itself cannot be expressed.
 (@def cancel-timer (timer: Todo) Nil)
 ;; Same limitation as cancel-timer for the parameter; the return value is
 ;; the same kind of mutable cons cell described for timer-activate's
 ;; reuse-cell, or nil if the timer was not found in either list.
 (@def cancel-timer-internal (timer: Todo) Cons<Todo~List<Todo>>?)
 (@def cancel-function-timers (function: fn<&List~Any>) Nil)
 ;; The timer parameter embeds the unrepresentable timer type. See
 ;; timer-create for why the timer type itself cannot be expressed.
 (@def timer-until (timer: Todo time: Timestamp) Number)
 ;; The timer parameter embeds the unrepresentable timer type. This
 ;; function is invoked directly by the C runtime and its return value is
 ;; not part of any documented contract.
 (@def timer-event-handler (timer: Todo) Any)
 (@def timeout-event-p (event: Any) Boolean)
 ;; The return value is a newly created timer. See timer-create for why
 ;; the timer type itself cannot be expressed.
 (@def run-at-time (time: Timestamp|String|True repeat: Number? function: fn<&List~Any> &rest args: &List) Todo)
 ;; Same limitation as run-at-time: the return value is a newly created
 ;; timer whose type cannot be expressed.
 (@def run-with-timer (secs: Number repeat: Number? function: fn<&List~Any> &rest args: &List) Todo)
 ;; Same limitation as run-at-time: the return value is a newly created
 ;; timer whose type cannot be expressed.
 (@def add-timeout (secs: Number function: fn1<Any> object: Any &optional repeat: Number?) Todo)
 ;; Same limitation as run-at-time: the return value is a newly created
 ;; timer whose type cannot be expressed.
 (@def run-with-idle-timer (secs: Timestamp repeat: Bool function: fn<&List~Any> &rest args: &List) Todo)
 ;; with-timeout destructures its first operand into a SECONDS expression
 ;; and TIMEOUT-FORMS, evaluates SECONDS and BODY, and returns either
 ;; BODY's value or TIMEOUT-FORMS' value depending on whether a timeout
 ;; occurred before BODY finished. This value-dependent choice between two
 ;; independently-checked forms is not expressible by $body or $fn.
 (@check with-timeout ($todo))
 ;; The result pairs each pending with-timeout timer with a time-delta
 ;; value; the timer element embeds the unrepresentable timer type. See
 ;; timer-create for why the timer type itself cannot be expressed.
 (@def with-timeout-suspend () (ListFresh (Tuple Todo TimeOutput)))
 ;; The argument pairs a timer with a time-delta value; the timer element
 ;; embeds the unrepresentable timer type. See timer-create for why the
 ;; timer type itself cannot be expressed.
 (@def with-timeout-unsuspend (timer-spec-list: (read-only (List (Tuple Todo TimeOutput)))) Nil)
 (@def y-or-n-p-with-timeout (prompt: String seconds: Number default-value: [T]) T|Boolean)
 (@def timer-duration (string: String) Number?)
 (@def internal-timer-start-idle () Nil))
