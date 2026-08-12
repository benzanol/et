;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Process management

(et-declare
;; AUTHORING STUB: not yet classified.
(@def processp (object: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-process (name: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def delete-process (&optional process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-status (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-exit-status (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-id (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-name (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-command (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-tty-name (process: Todo &optional stream: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-buffer (process: Todo buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-buffer (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-mark (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-filter (process: Todo filter: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-filter (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-sentinel (process: Todo sentinel: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-sentinel (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-thread (process: Todo thread: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-thread (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-window-size (process: Todo height: Todo width: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-inherit-coding-system-flag (process: Todo flag: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-query-on-exit-flag (process: Todo flag: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-query-on-exit-flag (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-contact (process: Todo &optional key: Todo no_block: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-plist (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-plist (process: Todo plist: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-type (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def format-network-address (address: Todo &optional omit_port: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-list () Todo))

;;; ============================================================
;;; Process creation and networking

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-process (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-pipe-process (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-datagram-address (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-datagram-address (process: Todo address: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-network-process-option (process: Todo option: Todo value: Todo &optional no_error: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def serial-process-configure (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-serial-process (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-network-process (&rest args: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def network-interface-list (&optional full: Todo family: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def network-interface-info (ifname: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def network-lookup-address-info (name: Todo &optional family: Todo hint: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def accept-process-output (&optional process: Todo seconds: Todo millisec: Todo just_this_one: Todo) Todo))

;;; ============================================================
;;; Process input and output

(et-declare
;; AUTHORING STUB: not yet classified.
(@def internal-default-process-filter (proc: Todo text: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-send-region (process: Todo start: Todo end: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-send-string (process: Todo string: Todo) Todo))

;;; ============================================================
;;; Process control

(et-declare
;; AUTHORING STUB: not yet classified.
(@def process-running-child-p (&optional process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-default-interrupt-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def interrupt-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def kill-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def quit-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def stop-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def continue-process (&optional process: Todo current_group: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def internal-default-signal-process (process: Todo sigcode: Todo &optional remote: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def signal-process (process: Todo sigcode: Todo &optional remote: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-send-eof (&optional process: Todo) Todo))

;;; ============================================================
;;; Coding systems and system information

(et-declare
;; AUTHORING STUB: not yet classified.
(@def internal-default-process-sentinel (proc: Todo msg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-process-coding-system (process: Todo &optional decoding: Todo encoding: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-coding-system (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-buffer-process (buffer: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def process-inherit-coding-system-flag (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def waiting-for-user-input-p () Todo)
;; AUTHORING STUB: not yet classified.
(@def list-system-processes () Todo)
;; AUTHORING STUB: not yet classified.
(@def process-attributes (process: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def num-processors (&optional query: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def signal-names () Todo))

;;; ============================================================
