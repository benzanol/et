;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Process management

(et-declare
 (@def processp (object: Any) Boolean)
 (@def get-process (name: Process|String) Process?)
 (@def delete-process (&optional process: Process|Buffer|String?) Nil)
 (@def process-status (process: Process|Buffer|String?) Symbol?)
 (@def process-exit-status (process: Process) Integer)
 (@def process-id (process: Process) Integer?)
 (@def process-name (process: Process) String)
 (@def process-command (process: Process) List<String>|True)
 (@def process-tty-name (process: Process &optional stream: @stdin|@stdout|@stderr?) String?)
 (@def set-process-buffer (process: Process buffer: Buffer?) Buffer?)
 (@def process-buffer (process: Process) Buffer?)
 (@def process-mark (process: Process) Marker)
 (@def set-process-filter (process: Process filter: (or Nil True fn2<Process~String>))
       (or Nil True fn2<Process~String>))
 (@def process-filter (process: Process) (or Nil True fn2<Process~String>))
 (@def set-process-sentinel (process: Process sentinel: (or Nil True fn2<Process~String>))
       (or Nil True fn2<Process~String>))
 (@def process-sentinel (process: Process) (or Nil True fn2<Process~String>))
 (@def set-process-thread ([(<= T Thread?)] process: Process thread: T) T)
 (@def process-thread (process: Process) Thread?)
 (@def set-process-window-size (process: Process height: Integer width: Integer) Boolean)
 (@def set-process-inherit-coding-system-flag (process: Process flag: Bool) Bool)
 (@def set-process-query-on-exit-flag (process: Process flag: Bool) Bool)
 (@def process-query-on-exit-flag (process: Process) Boolean)
 (@def process-contact (process: Process &optional key: Any no_block: Bool) Any)
 ;; The plist is only required to be a proper list (CHECK_LIST); it is not
 ;; validated to alternate keys and values, so a strict PlistOf would claim
 ;; unsupported precision.
 (@def process-plist (process: Process) List<Any>)
 (@def set-process-plist ([(<= L List<Any>)] process: Process plist: L) L)
 (@def process-type (process: Process|Buffer|String?) @real|@network|@serial|@pipe)
 (@def format-network-address
       (address: String|Vector<Integer>|Cons<Integer~Vector<Integer>>?
                 &optional omit_port: Bool)
       String?)
 (@def process-list () List<Process>))


;;; ============================================================
;;; Process creation and networking

(et-declare
 ;; ARGS is a keyword/value plist (see the docstring for the full set of
 ;; keywords and their per-keyword value types); left as an unconstrained
 ;; rest list rather than typed key-by-key, following the `make-hash-table'
 ;; precedent for keyword-argument primitives.
 (@def make-process (&rest args: &List) Process?)
 ;; ARGS is a keyword/value plist (see the docstring); left as an
 ;; unconstrained rest list, following the `make-hash-table' precedent.
 (@def make-pipe-process (&rest args: &List) Process?)
 (@def process-datagram-address (process: Process)
       String|Vector<Integer>|Cons<Integer~Vector<Integer>>?)
 (@def set-process-datagram-address
       ([(<= A String|Vector<Integer>|Cons<Integer~Vector<Integer>>)]
        process: Process address: A)
       A?)
 (@def set-network-process-option
       (process: Process option: Symbol value: Any &optional no_error: Bool)
       Boolean)
 ;; ARGS is a keyword/value plist (see the docstring); left as an
 ;; unconstrained rest list, following the `make-hash-table' precedent.
 (@def serial-process-configure (&rest args: &List) Nil)
 ;; ARGS is a keyword/value plist (see the docstring); left as an
 ;; unconstrained rest list, following the `make-hash-table' precedent.
 (@def make-serial-process (&rest args: &List) Process?)
 ;; ARGS is a keyword/value plist (see the docstring); left as an
 ;; unconstrained rest list, following the `make-hash-table' precedent.
 (@def make-network-process (&rest args: &List) Process?)
 ;; Each element is a 2-element cons when FULL is nil, or a 4-element list
 ;; when FULL is non-nil. The type language cannot express result structure
 ;; that branches on a parameter's runtime value.
 (@def network-interface-list (&optional full: Bool family: @ipv4|@ipv6?) List<Todo>?)
 (@def network-interface-info (ifname: String) (or Nil (Tuple Any Any Any Any Any)))
 (@def network-lookup-address-info
       (name: String &optional family: @ipv4|@ipv6? hint: @numeric?)
       List<Vector<Integer>>?)
 (@def accept-process-output
       (&optional process: Process? seconds: Number? millisec: Integer? just_this_one: Bool)
       Boolean))


;;; ============================================================
;;; Process input and output

(et-declare
 (@def internal-default-process-filter (proc: Process text: String) Nil)
 (@def process-send-region (process: Process|Buffer|String? start: IntOrMarker end: IntOrMarker) Nil)
 (@def process-send-string (process: Process|Buffer|String? string: String) Nil))


;;; ============================================================
;;; Process control

(et-declare
 (@def process-running-child-p (&optional process: Process|Buffer|String?) Integer|Boolean)
 (@def internal-default-interrupt-process
       ([(<= P Process|Buffer|String?)] &optional process: P current_group: Bool)
       P)
 (@def interrupt-process (&optional process: Process|Buffer|String? current_group: Bool) Any)
 (@def kill-process
       ([(<= P Process|Buffer|String?)] &optional process: P current_group: Bool)
       P)
 (@def quit-process
       ([(<= P Process|Buffer|String?)] &optional process: P current_group: Bool)
       P)
 (@def stop-process
       ([(<= P Process|Buffer|String?)] &optional process: P current_group: Bool)
       P)
 (@def continue-process
       ([(<= P Process|Buffer|String?)] &optional process: P current_group: Bool)
       P)
 (@def internal-default-signal-process
       (process: Process|Buffer|Number|String? sigcode: Integer|Symbol &optional remote: String?)
       Nil|Integer)
 (@def signal-process
       (process: Process|Buffer|Number|String? sigcode: Integer|Symbol &optional remote: String?)
       Any)
 (@def process-send-eof ([(<= P Process|Buffer|String?)] &optional process: P) P))


;;; ============================================================
;;; Coding systems and system information

(et-declare
 (@def internal-default-process-sentinel (proc: Process msg: String) Nil)
 (@def set-process-coding-system
       (process: Process &optional decoding: Symbol? encoding: Symbol?)
       Nil)
 (@def process-coding-system (process: Process) Cons<Symbol?~Symbol?>)
 (@def get-buffer-process (buffer: Buffer|String?) Process?)
 (@def process-inherit-coding-system-flag (process: Process) Boolean)
 (@def waiting-for-user-input-p () Boolean)
 (@def list-system-processes () List<Integer>?)
 (@def process-attributes (pid: Number) Alist<Symbol~Any>?)
 (@def num-processors (&optional query: @current|@all?) Integer)
 (@def signal-names () List<String>?))


;;; ============================================================
