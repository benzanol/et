;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Selected window and temporary buffer windows

(et-declare
 (@check save-selected-window ($body))
 (@def temp-buffer-window-setup (buffer-or-name: Buffer|String) Buffer)
 (@def temp-buffer-window-show
       (buffer: Buffer &optional action: True|Cons<fn2<Buffer~Alist<Symbol~Any>~Window?>|List<fn2<Buffer~Alist<Symbol~Any>~Window?>>~Alist<Symbol~Any>>?)
       Window?)
 ;; The macro's return value depends on whether QUIT-FUNCTION is a
 ;; function at run time: it is either the last body form's value or the
 ;; result of calling QUIT-FUNCTION on the window and that value. The
 ;; type language cannot yet express a checker whose result type
 ;; branches on the runtime funcallability of an argument.
 (@check with-temp-buffer-window ($todo))
 ;; Same value-dependent return as `with-temp-buffer-window'.
 (@check with-current-buffer-window ($todo))
 ;; Same value-dependent return as `with-temp-buffer-window'.
 (@check with-displayed-buffer-window ($todo))
 (@check with-window-non-dedicated ($body Window?)))

;;; ============================================================
;;; Window relationships and tree structure

(et-declare
 (@def window-right (window: Window?) Window?)
 (@def window-left (window: Window?) Window?)
 (@def window-child (window: Window?) Window?)
 (@def window-child-count (window: Window?) Integer)
 (@def window-last-child (window: Window?) Window?)
 (@def window-normalize-buffer (buffer-or-name: Buffer|String?) Buffer)
 (@def window-normalize-frame (frame: Frame?) Frame)
 (@def window-normalize-window (window: Window? &optional live-only: Bool) Window)
 (@def frame-char-size (&optional window-or-frame: Window|Frame? horizontal: Bool) Integer)
 (@def window-safe-min-pixel-height (&optional window: Window?) Integer)
 (@def window-min-pixel-height (&optional window: Window?) Integer)
 (@def window-safe-min-pixel-width (&optional window: Window?) Integer)
 (@def window-min-pixel-width (&optional window: Window?) Integer)
 (@def window-safe-min-pixel-size (&optional window: Window? horizontal: Bool) Integer)
 (@def window-min-pixel-size (&optional window: Window? horizontal: Bool) Integer)
 (@def window-combined-p (&optional window: Window? horizontal: Bool) Window?)
 (@def window-combination-p (&optional window: Window? horizontal: Bool) Window?)
 (@def window-combinations (window: Window? &optional horizontal: Bool ignore-fixed: Bool) Integer)
 (@def walk-window-tree-1 (fun: fn1<Window> walk-window-tree-window: Window? any: Bool &optional sub-only: Bool) Any)
 (@def walk-window-tree (fun: fn1<Window> &optional frame: Frame? any: Bool minibuf: Bool) Any)
 (@def walk-window-subtree (fun: fn1<Window> &optional window: Window? any: Bool) Any)
 (@def window-with-parameter (parameter: Any &optional value: Any frame: Frame? any: Bool minibuf: Bool) Window?))

;;; ============================================================
;;; Atomic windows

(et-declare
 (@def window-atom-root (&optional window: Window?) Window?)
 (@def window-make-atom (window: Window) Window)
 (@def display-buffer-in-atom-window (buffer: Buffer alist: Alist<Symbol~Any>) Window?))

;;; ============================================================
;;; Side windows

(et-declare
 (@def window-main-window (&optional frame: Frame?) Window)
 (@def display-buffer-in-side-window (buffer: Buffer alist: Alist<Symbol~Any>) Window?)
 (@def window-toggle-side-windows (&optional frame: Frame?) Any))

;;; ============================================================
;;; Window sizes

(et-declare
 (@def window-total-size (&optional window: Window? horizontal: Bool round: Any) Integer)
 (@def window-size (&optional window: Window? horizontal: Bool pixelwise: Bool round: Any) Integer)
 (@def window-preserve-size (&optional window: Window? horizontal: Bool preserve: Bool) Tuple<Buffer~Integer?~Integer?>)
 (@def window-preserved-size (&optional window: Window? horizontal: Bool) Integer?)
 (@def window-safe-min-size (&optional window: Window? horizontal: Bool pixelwise: Bool) Integer)
 (@def window-min-size (&optional window: Window? horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Integer)
 (@def window-sizable (window: Window? delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Integer)
 (@def window-sizable-p (window: Window? delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Boolean)
 (@def window-size-fixed-p (&optional window: Window? horizontal: Bool ignore: @safe|@preserved|Window|Boolean) Boolean)
 (@def window-min-delta
       (&optional window: Window? horizontal: Bool ignore: @safe|@preserved|Window|Boolean
        trail: @before|@after? noup: Bool nodown: Bool pixelwise: Bool)
       Integer)
 (@def frame-windows-min-size (&optional frame: Frame? horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Integer)
 (@def window-max-delta
       (&optional window: Window? horizontal: Bool ignore: @safe|@preserved|Window|Boolean
        trail: @before|@after? noup: Bool nodown: Bool pixelwise: Bool)
       Integer)
 (@def window-resizable (window: Window? delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Integer)
 (@def window-resizable-p (window: Window? delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Boolean)
 ;; Alias of a function defined in window.c.
 (@def window-height (&optional window: Window? round: Any) Integer)
 ;; Alias of a function defined in window.c.
 (@def window-width (&optional window: Window? pixelwise: Any) Integer)
 ;; Alias of a function defined in window.c.
 (@def window-pixel-width-before-size-change (&optional window: Window?) Integer)
 ;; Alias of a function defined in window.c.
 (@def window-pixel-height-before-size-change (&optional window: Window?) Integer)
 (@def window-full-height-p (&optional window: Window?) Boolean)
 (@def window-full-width-p (&optional window: Window?) Boolean)
 (@def window-body-size (&optional window: Window? horizontal: Bool pixelwise: Bool) Integer)
 (@def window-font-width (&optional window: Window? face: Symbol?) Integer)
 (@def window-font-height (&optional window: Window? face: Symbol?) Integer)
 (@def window-max-chars-per-line (&optional window: Window? face: Symbol?) Integer)
 (@def window-current-scroll-bars (&optional window: Window?) Cons<@left|@right|Nil~@bottom|Nil>)
 (@def walk-windows (fun: fn1<Window> &optional minibuf: Bool all-frames: Boolean|@visible|0|Frame) Any)
 (@def window-at-side-p (&optional window: Window? side: @left|@top|@right|@bottom?) Boolean)
 (@def window-at-side-list (&optional frame: Frame? side: @left|@top|@right|@bottom?) List<Window>)
 (@def window-in-direction
       (direction: @up|@down|@above|@below|@left|@right &optional window: Window?
        ignore: Bool sign: Number? wrap: Bool minibuf: Bool)
       Window?)
 (@def get-window-with-predicate (predicate: fn1<Window> &optional minibuf: Bool all-frames: Boolean|@visible|0|Frame default: [D]) Window|D)
 ;; Alias of `get-window-with-predicate'.
 (@def some-window (predicate: fn1<Window> &optional minibuf: Bool all-frames: Boolean|@visible|0|Frame default: [D]) Window|D)
 (@def get-lru-window (&optional all-frames: Boolean|@visible|0|Frame dedicated: Bool not-selected: Bool no-other: Bool) Window?)
 (@def get-mru-window (&optional all-frames: Boolean|@visible|0|Frame dedicated: Bool not-selected: Bool no-other: Bool) Window?)
 (@def get-largest-window (&optional all-frames: Boolean|@visible|0|Frame dedicated: Bool not-selected: Bool no-other: Bool) Window?)
 (@def get-buffer-window-list (&optional buffer-or-name: Buffer|String? minibuf: Bool all-frames: Boolean|@visible|0|Frame) List<Window>)
 (@def minibuffer-window-active-p (window: Window) Boolean)
 (@def count-windows (&optional minibuf: Bool all-frames: Boolean|@visible|0|Frame) Integer))

;;; ============================================================
;;; Resizing windows

(et-declare
 (@def window-resize (window: Window? delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Any)
 (@def window-resize-no-error (window: Window delta: Integer &optional horizontal: Bool ignore: @safe|@preserved|Window|Boolean pixelwise: Bool) Any)
 (@def adjust-window-trailing-edge (window: Window? delta: Integer &optional horizontal: Bool pixelwise: Bool) Any)
 (@def enlarge-window (delta: Integer &optional horizontal: Bool) Any)
 (@def shrink-window (delta: Integer &optional horizontal: Bool) Any)
 (@def maximize-window (&optional window: Window?) Any)
 (@def minimize-window (&optional window: Window?) Any))

;;; ============================================================
;;; Window edges

(et-declare
 (@def window-edges (&optional window: Window? body: Bool absolute: Bool pixelwise: Bool) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-body-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 ;; Alias of `window-body-edges'.
 (@def window-inside-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-body-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 ;; Alias of `window-body-pixel-edges'.
 (@def window-inside-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-absolute-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-absolute-body-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 ;; Alias of `window-absolute-body-pixel-edges'.
 (@def window-inside-absolute-pixel-edges (&optional window: Window?) Tuple<Integer~Integer~Integer~Integer>)
 (@def window-absolute-pixel-position (&optional position: IntOrMarker? window: Window?) Cons<Integer~Integer>?)
 (@def frame-root-window-p (window: Window) Boolean)
 ;; Same recursive window-tree structure as the internal subtree
 ;; builder: a list whose elements are windows or nested cons trees of
 ;; split-direction, edges, and child subtrees. Expressing this
 ;; requires a named recursive type alias, which authoring is not
 ;; permitted to define.
 (@def window-tree (&optional frame: Frame?) Todo)
 (@def other-window (count: Integer &optional all-frames: Boolean|@visible|0|Frame interactive: Bool) Any)
 (@def other-window-prefix () Any)
 (@def same-window-prefix () Any)
 (@def one-window-p (&optional nomini: Bool all-frames: Boolean|@visible|0|Frame) Boolean))

;;; ============================================================
;;; Deleting windows

(et-declare
 (@def window-deletable-p (&optional window: Window?) @frame|Boolean)
 (@def window-at-x-y (x: Integer y: Integer &optional frame: Frame? no-other: Bool) Window?)
 (@def delete-window (&optional window: Window?) Any)
 (@def delete-other-windows (&optional window: Window? interactive: Bool) Any)
 (@def delete-other-windows-vertically (&optional window: Window?) Any))

;;; ============================================================
;;; Windows and buffers

(et-declare
 (@def push-window-buffer-onto-prev (&optional window: Window?) Any)
 (@def record-window-buffer (&optional window: Window?) Any)
 (@def unrecord-window-buffer (&optional window: Window? buffer: Buffer?) List<Buffer>)
 (@def set-window-buffer-start-and-point
       (window: Window? buffer: Buffer|String &optional start: IntOrMarker? point: IntOrMarker?)
       Any)
 (@def switch-to-prev-buffer-skip-p
       (skip: fn3<Window~Buffer~Any>|True|@visible|0|Frame? window: Window buffer: Buffer &optional bury-or-kill: Bool|@append)
       Any)
 (@def switch-to-prev-buffer (&optional window: Window? bury-or-kill: Bool|@append) Buffer?)
 (@def switch-to-next-buffer (&optional window: Window?) Buffer?)
 (@def get-next-valid-buffer (list: &List<Buffer> &optional buffer: Buffer? visible-ok: Bool frame: Frame?) Buffer?)
 (@def last-buffer (&optional buffer: Buffer? visible-ok: Bool frame: Frame?) Buffer)
 (@def bury-buffer (&optional buffer-or-name: Buffer|String?) Nil)
 (@def unbury-buffer () Any)
 (@def next-buffer (&optional arg: Integer? interactive: Bool) Any)
 (@def previous-buffer (&optional arg: Integer? interactive: Bool) Any)
 (@def delete-windows-on (&optional buffer-or-name: Buffer|String? frame: Boolean|@visible|0|Frame) Any)
 (@def replace-buffer-in-windows (&optional buffer-or-name: Buffer|String?) Any)
 (@def quit-restore-window (&optional window: Window? bury-or-kill: Bool|@append|@bury|@kill) Any)
 (@def quit-window (&optional kill: Bool window: Window?) Any)
 (@def quit-windows-on (&optional buffer-or-name: Buffer|String? kill: Bool frame: Boolean|@visible|0|Frame) Any)
 (@def split-window (&optional window: Window? size: Number? side: Any pixelwise: Bool) Window)
 (@def split-window-no-error (&optional window: Window? size: Number? side: Any pixelwise: Bool) Window?)
 (@def split-window-below (&optional size: Integer? window-to-split: Window?) Window)
 ;; Alias of `split-window-below'.
 (@def split-window-vertically (&optional size: Integer? window-to-split: Window?) Window)
 (@def split-root-window-below (&optional size: Integer?) Window)
 (@def split-window-right (&optional size: Integer? window-to-split: Window?) Window)
 ;; Alias of `split-window-right'.
 (@def split-window-horizontally (&optional size: Integer? window-to-split: Window?) Window)
 (@def split-root-window-right (&optional size: Integer?) Window))

;;; ============================================================
;;; Balancing windows

(et-declare
 (@def balance-windows-2 (window: Window horizontal: Bool) Any)
 (@def balance-windows-1 (window: Window &optional horizontal: Bool) Any)
 (@def balance-windows (&optional window-or-frame: Window|Frame?) Any)
 (@def window-fixed-size-p (&optional window: Window? direction: @height|@width?) Boolean))

;;; ============================================================
;;; A different solution to balance-windows

(et-declare
 (@def balance-windows-area-adjust (window: Window delta: Integer horizontal: Bool pixelwise: Bool) Any)
 (@def balance-windows-area () Any))

;;; ============================================================
;;; Window states

(et-declare
 ;; The result is a heterogeneous nested alist whose shape (leaf vs.
 ;; vc/hc combination, which keys are present) depends on the kind of
 ;; window and the WRITABLE flag. Expressing this requires a named
 ;; recursive type alias, which authoring is not permitted to define.
 (@def window-state-get (&optional window: Window? writable: Bool) Todo)
 ;; STATE has the same window-state structure produced by
 ;; `window-state-get', which cannot currently be typed.
 (@def window-state-put (state: Todo &optional window: Window? ignore: @safe|@preserved|Window|Boolean) Any)
 ;; STATE has the same window-state structure produced by
 ;; `window-state-get', which cannot currently be typed.
 (@def window-state-buffers (state: Todo) List<Buffer>)
 (@def window-swap-states (&optional window-1: Window? window-2: Window? size: @height|@width|True?) Any)
 (@def display-buffer-record-window (type: @reuse|@window|@frame|@tab window: Window buffer: Buffer) Any))

;;; ============================================================
;;; Special and same-window display

(et-declare
 (@def special-display-p (buffer-name: String) Any)
 ;; Alias of a function scheduled for removal; superseded by
 ;; `display-buffer-pop-up-frame'.
 (@def special-display-popup-frame (buffer: Buffer &optional args: &List?) Window?)
 (@def same-window-p (buffer-name: Any) Any))

;;; ============================================================
;;; Pop-up window and frame policy

(et-declare
 (@def window-splittable-p (window: Window &optional horizontal: Bool) Boolean)
 (@def split-window-sensibly (&optional window: Window?) Window?)
 (@def toggle-window-dedicated (&optional window: Window? flag: Any interactive: Bool) Any)
 (@def display-buffer-assq-regexp (buffer-name: String alist: Alist<Any~Any> action: Any) Any)
 (@def display-buffer
       (buffer-or-name: Buffer|String
        &optional action: True|Cons<fn2<Buffer~Alist<Symbol~Any>~Window?>|List<fn2<Buffer~Alist<Symbol~Any>~Window?>>~Alist<Symbol~Any>>?
        frame: Frame?)
       Window?)
 (@def display-buffer-other-frame (buffer: Buffer|String) Window?))

;;; ============================================================
;;; Display-buffer action functions

(et-declare
 (@def [display-buffer-use-some-frame
        display-buffer-same-window
        display-buffer-full-frame
        display-buffer-reuse-window
        display-buffer-reuse-mode-window
        display-buffer-pop-up-frame
        display-buffer-pop-up-window
        display-buffer-in-child-frame
        display-buffer-in-direction
        display-buffer-below-selected
        display-buffer-at-bottom
        display-buffer-in-previous-window
        display-buffer-use-some-window
        display-buffer-use-least-recent-window]
       (buffer: Buffer alist: Alist<Symbol~Any>)
       Window?)
 (@def windows-sharing-edge (&optional window: Window? edge: @left|@above|@right|@below? within: Bool) List<Window>)
 (@def display-buffer-no-window (_buffer: Buffer alist: Alist<Symbol~Any>) @fail?))

;;; ============================================================
;;; Display and selection commands

(et-declare
 (@def pop-to-buffer
       (buffer-or-name: Buffer|String?
        &optional action: True|Cons<fn2<Buffer~Alist<Symbol~Any>~Window?>|List<fn2<Buffer~Alist<Symbol~Any>~Window?>>~Alist<Symbol~Any>>?
        norecord: Bool)
       Buffer)
 (@def pop-to-buffer-same-window (buffer: Buffer|String? &optional norecord: Bool) Buffer)
 (@def read-buffer-to-switch (prompt: String) String)
 (@def window-normalize-buffer-to-switch-to (buffer-or-name: Buffer|String?) Buffer)
 (@def switch-to-buffer (buffer-or-name: Buffer|String? &optional norecord: Bool force-same-window: Bool) Buffer)
 (@def switch-to-buffer-other-window (buffer-or-name: Buffer|String? &optional norecord: Bool) Buffer)
 (@def switch-to-buffer-other-frame (buffer-or-name: Buffer|String? &optional norecord: Bool) Buffer)
 (@def display-buffer-override-next-command
       (pre-function: fn2<Buffer~Alist<Symbol~Any>~Cons<Window~@reuse|@window|@frame|@tab>>
        &optional post-function: fn2<Window~Window>? echo: String?)
       fn)
 (@def set-window-text-height (window: Window? height: Integer) Any)
 (@def enlarge-window-horizontally (delta: Integer) Any)
 (@def shrink-window-horizontally (delta: Integer) Any)
 (@def count-screen-lines (&optional beg: IntOrMarker? end: IntOrMarker? count-final-newline: Bool window: Window?) Integer)
 (@def window-buffer-height (window: Window?) Integer)
 (@def window-default-font-height (&optional window: Window?) Integer)
 (@def window-default-line-height (&optional window: Window?) Integer))

;;; ============================================================
;;; Resizing windows and frames to fit their contents exactly

(et-declare
 (@def fit-mini-frame-to-buffer (&optional frame: Frame?) Any)
 (@def fit-frame-to-buffer
       (&optional frame: Frame? max-height: Integer? min-height: Integer? max-width: Integer? min-width: Integer?
        only: @vertically|@horizontally?)
       Any)
 (@def fit-frame-to-buffer-1
       (&optional frame: Frame? max-height: Integer? min-height: Integer? max-width: Integer? min-width: Integer?
        only: @vertically|@horizontally? from: IntOrMarker? to: IntOrMarker?)
       Any)
 (@def fit-window-to-buffer
       (&optional window: Window? max-height: Integer? min-height: Integer? max-width: Integer? min-width: Integer?
        preserve-size: Bool)
       Any)
 (@def window-safely-shrinkable-p (&optional window: Window?) Boolean)
 (@def shrink-window-if-larger-than-buffer (&optional window: Window?) Any)
 (@def window-largest-empty-rectangle
       (&optional window: Window? count: Integer|Cons<Integer~Bool>? min-width: Integer? min-height: Integer?
        positions: Cons<Integer~Integer>? left: Bool)
       (is-non-nil? count List<Tuple<Integer~Integer~Integer~Integer>>? Tuple<Integer~Integer~Integer>?))
 (@def kill-buffer-and-window () Any))

;;; ============================================================
;;; Groups of windows (Follow Mode)

(et-declare
 ;; Each function tries a user-installed hook function before falling
 ;; back to the corresponding built-in. The hook branch calls an
 ;; arbitrary stored function, so the result cannot be narrowed beyond
 ;; Any.
 (@def window-group-start (&optional window: Window?) Any)
 (@def window-group-end (&optional window: Window? update: Bool) Any)
 (@def set-window-group-start (window: Window? pos: IntOrMarker &optional noforce: Bool) Any)
 (@def recenter-window-group (&optional arg: Any) Any)
 (@def pos-visible-in-window-group-p (&optional pos: IntOrMarker|@t? window: Window? partially: Bool) Any)
 (@def selected-window-group () Any)
 (@def move-to-window-group-line (arg: Any) Any))

;;; ============================================================
;;; Recentering and scrolling commands

(et-declare
 (@def recenter-top-bottom (&optional arg: Any) Any)
 (@def recenter-other-window (&optional arg: Any) Any)
 (@def move-to-window-line-top-bottom (&optional arg: Any) Any)
 (@def scroll-up-command (&optional arg: Integer|@-?) Any)
 (@def scroll-down-command (&optional arg: Integer|@-?) Any)
 (@def scroll-other-window (&optional lines: Integer|@-?) Any)
 (@def scroll-other-window-down (&optional lines: Integer|@-?) Any)
 (@def scroll-up-line (&optional arg: Integer?) Any)
 (@def scroll-down-line (&optional arg: Integer?) Any)
 (@def beginning-of-buffer-other-window (arg: Any) Any)
 (@def end-of-buffer-other-window (arg: Any) Any))

;;; ============================================================
;;; Mouse autoselection and process window utilities

(et-declare
 (@def mouse-autoselect-window-cancel (&optional force: Bool) Any)
 (@def mouse-autoselect-window-start (mouse-position: Any &optional window: Window? suspend: Bool) Any)
 (@def mouse-autoselect-window-select () Any)
 (@def handle-select-window (event: Any) Any)
 (@def truncated-partial-width-window-p (&optional window: Window?) Bool)
 (@def window-adjust-process-window-size (reducer: fn2<Integer~Integer~Integer> windows: &List<Window>) Cons<Integer~Integer>?)
 (@def window-adjust-process-window-size-smallest (_process: Process windows: &List<Window>) Cons<Integer~Integer>?)
 (@def window-adjust-process-window-size-largest (_process: Process windows: &List<Window>) Cons<Integer~Integer>?))

;;; ============================================================
;;; Window point context

(et-declare
 (@def window-point-context-set () Any)
 (@def window-point-context-use () Any)
 (@def window-point-context-set-default-function (w: Window) Alist<@front-context-string|@rear-context-string~String>?)
 (@def window-point-context-use-default-function (w: Window context: Alist<Any~Any>) Any))

;;; ============================================================
