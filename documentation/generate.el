#!/usr/bin/env -S emacs --script
;;; generate.el --- Generate VitePress content from Org files -*- lexical-binding: t; -*-

;; Usage:
;;   documentation/generate.el DIR
;; or:
;;   emacs --batch -Q -l documentation/generate.el \
;;     --eval '(et:vitepress-generate "DIR")'
;;
;; DIR must contain an existing VitePress project in site/.  This script replaces
;; site/src/ on every run.  The rest of site/ is persistent and owns VitePress
;; configuration, dependencies, themes, styles, and static assets.
;;
;; Every .org file directly inside DIR becomes a collection.  Each top-level
;; headline becomes a page by default, with all of its descendant headlines on
;; that page.  Give a headline the Org tag `section' to make it a sidebar group:
;; each of its direct child headlines then becomes a separate page.  A section
;; that also has its own body content is both a clickable page and a group.
;; Give any headline the tag `page' to make that particular headline a separate
;; page, even when its parent is not a section.  Untagged sibling headlines stay
;; on their parent's page.
;;
;;   * Language reference                                          :section:
;;   ** Syntax
;;   ** Semantics
;;
;; Here, "Language reference" is a group, while "Syntax" and "Semantics" are
;; pages.  If text appeared between the "Language reference" headline and its
;; first child, the group would also link to a page containing that text.
;; Without the `section' tag, "Language reference" alone would be a page and
;; the two child headlines would appear in its right-hand TOC.  A child can
;; itself be tagged `section' to create a nested group.
;; A file can set its tab position with either of these top-level keywords:
;;
;;   #+VITEPRESS_TAB_ORDER: 1
;;   #+PROPERTY: VITEPRESS_TAB_ORDER 1
;;

(require 'json)
(require 'org)
(require 'org-element)
(require 'ox-md)
(require 'seq)
(require 'subr-x)

(setq org-export-preserve-breaks t)

(cl-defstruct (et:vitepress--page (:constructor et:vitepress--page-create))
  title slug path level begin end contents-begin contents-end children home)

(cl-defstruct (et:vitepress--target (:constructor et:vitepress--target-create))
  page anchor title)

(cl-defstruct (et:vitepress--tab (:constructor et:vitepress--tab-create))
  title slug order file pages nav targets)

(defvar et:vitepress--link-targets nil
  "Headline targets available while exporting the current Org file.")

(defvar et:vitepress--current-page nil
  "Page currently being exported by the VitePress backend.")

(defun et:vitepress--slugify (text)
  "Turn TEXT into a safe URL/file-name component."
  (let ((slug (downcase (string-trim text))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    (if (string-empty-p slug) "page" slug)))

(defun et:vitepress--file-property (name)
  "Return file-wide Org property NAME from the current buffer."
  (org--property-global-or-keyword-value name t))

(defun et:vitepress--parse-order (value file)
  (when value
    (unless (string-match-p "\\`[+-]?[0-9]+\\'" (string-trim value))
      (error "%s: VITEPRESS_TAB_ORDER must be an integer" file))
    (string-to-number value)))

(defun et:vitepress--section-has-content-p (headline)
  "Return non-nil when HEADLINE has visible content before its children.

Org represents that direct body as a `section' element.  Planning metadata,
property drawers, and comments do not make a navigation group into a page."
  (when-let* ((section
               (seq-find (lambda (element)
                           (eq (org-element-type element) 'section))
                         (org-element-contents headline))))
    (seq-some
     (lambda (element)
       (not (memq (org-element-type element)
                  '(planning property-drawer comment comment-block))))
     (org-element-contents section))))

(defun et:vitepress--headline-pages (ast tab-slug file)
  "Read selected pages from AST and return their page-tree roots."
  (let (used-paths)
    (cl-labels
        ((walk (elements children-are-pages-p path-prefix)
           (let (pages)
             (dolist (element elements)
               (when (eq (org-element-type element) 'headline)
                 (let* ((level (org-element-property :level element))
                        ;; Read tags from Org's parsed headline node.  Do not
                        ;; inspect or parse the headline text ourselves.
                        (section
                         (member "section"
                                 (org-element-property :tags element)))
                        (explicit-page
                         (member "page"
                                 (org-element-property :tags element)))
                        (selected (or explicit-page
                                      (and children-are-pages-p
                                           (not section))))
                        (title (org-element-property :raw-value element))
                        (heading-slug
                         (et:vitepress--slugify title)))
                   (cond
                    ;; A section's direct children are entries/pages in its
                    ;; navigation group.  When it has its own body, also make
                    ;; the section itself a page which owns those children.  An
                    ;; explicit `page' tag makes even an empty section a page.
                    (section
                     (if (or explicit-page
                             (et:vitepress--section-has-content-p element))
                         (let* ((requested-slug
                                 (org-element-property :VITEPRESS_SLUG element))
                                (slug (et:vitepress--slugify
                                       (or requested-slug title)))
                                (path (string-join
                                       (append path-prefix (list slug)) "/"))
                                (page
                                 (et:vitepress--page-create
                                  :title title :slug slug :path path :level level
                                  :begin (org-element-property :begin element)
                                  :end (org-element-property :end element)
                                  :contents-begin
                                  (org-element-property :contents-begin element)
                                  :contents-end
                                  (org-element-property :contents-end element))))
                           (when (member path used-paths)
                             (error "%s: duplicate generated page path %s"
                                    file path))
                           (push path used-paths)
                           (setf (et:vitepress--page-children page)
                                 (walk (org-element-contents element) t
                                       (append path-prefix (list slug))))
                           (setq pages (append pages (list page))))
                       (setq pages
                             (append pages
                                     (walk
                                      (org-element-contents element) t
                                      (append path-prefix
                                              (list heading-slug)))))))
                    ((not selected)
                       (setq pages
                             (append pages
                                     (walk
                                      (org-element-contents element)
                                      nil
                                      (append
                                       path-prefix
                                       (list heading-slug))))))
                    (t
                     (let* (
                            (requested-slug
                             (org-element-property :VITEPRESS_SLUG element))
                            (slug (et:vitepress--slugify
                                   (or requested-slug title)))
                            (path
                             (string-join (append path-prefix (list slug)) "/"))
                            (page
                             (et:vitepress--page-create
                              :title title :slug slug :path path :level level
                              :begin (org-element-property :begin element)
                              :end (org-element-property :end element)
                              :contents-begin
                              (org-element-property :contents-begin element)
                              :contents-end
                              (org-element-property :contents-end element))))
                       (when (member path used-paths)
                         (error "%s: duplicate generated page path %s"
                                file path))
                       (push path used-paths)
                       (setf (et:vitepress--page-children page)
                             ;; A page absorbs ordinary descendants.  Walking
                             ;; them still discovers any nested `section'.
                             (walk (org-element-contents element) nil
                                   (append path-prefix (list slug))))
                       (setq pages (append pages (list page)))))))))
             pages)))
      ;; Headlines directly below the document root are always pages.
      (walk (org-element-contents ast) t (list tab-slug)))))

(defun et:vitepress--page-navigation (page children)
  "Build a VitePress sidebar item for PAGE with CHILDREN."
  (append
   `(("text" . ,(et:vitepress--page-title page))
     ("link" . ,(concat "/" (et:vitepress--page-path page))))
   (when children
      (append
      (when (> (et:vitepress--page-level page) 1)
        '(("collapsed" . :json-false)))
      `(("items" . ,(vconcat children)))))))

(defun et:vitepress--heading-navigation-entry (element page-table)
  "Build navigation entries for one Org ELEMENT.

PAGE-TABLE maps headline positions to generated pages."
  (when (eq (org-element-type element) 'headline)
    (let* ((page (gethash (org-element-property :begin element) page-table))
           (children
            (et:vitepress--heading-navigation
             (org-element-contents element) page-table))
           (section
            (member "section" (org-element-property :tags element))))
      (cond (page
             (list (et:vitepress--page-navigation page children)))
            ((and section children)
             (list
              (append
               `(("text" . ,(org-element-property :raw-value element)))
               (when (> (org-element-property :level element) 1)
                 '(("collapsed" . :json-false)))
               `(("items" . ,(vconcat children))))))
            (children children)))))

(defun et:vitepress--heading-navigation (elements page-table)
  "Build nested VitePress navigation from Org ELEMENTS and PAGE-TABLE."
  (cl-mapcan
   (lambda (element)
     (et:vitepress--heading-navigation-entry element page-table))
   elements))

(defun et:vitepress--build-navigation (ast pages)
  "Build sidebar navigation from AST and generated PAGES."
  (let ((page-table (make-hash-table :test #'eql)))
    (dolist (page (et:vitepress--all-pages pages))
      (puthash (et:vitepress--page-begin page) page page-table))
    (et:vitepress--heading-navigation (org-element-contents ast) page-table)))

(defun et:vitepress--headline-targets (ast pages)
  "Build a link-target table for all headlines in AST.

PAGES identifies which generated page owns each headline.  Targets retain page
objects rather than paths so making a page the site home later is reflected in
all links to it."
  (let ((page-table (make-hash-table :test #'eql))
        (targets (make-hash-table :test #'equal)))
    (dolist (page (et:vitepress--all-pages pages))
      (puthash (et:vitepress--page-begin page) page page-table))
    (cl-labels
        ((add (type name page anchor title)
           (when (and name (not (gethash (cons type name) targets)))
             (puthash (cons type name)
                      (et:vitepress--target-create
                       :page page :anchor anchor :title title)
                      targets)))
         (walk (elements containing-page)
           (dolist (element elements)
             (when (eq (org-element-type element) 'headline)
               (let* ((page (or (gethash (org-element-property :begin element)
                                         page-table)
                                containing-page))
                      (page-root-p
                       (and page
                            (= (org-element-property :begin element)
                               (et:vitepress--page-begin page))))
                      (title (org-element-property :raw-value element))
                      (custom-id (org-element-property :CUSTOM_ID element))
                      (id (org-element-property :ID element)))
                 (when page
                   (add "custom-id" custom-id page
                        (unless page-root-p custom-id) title)
                   (add "id" id page (unless page-root-p id) title)
                   ;; Org represents both [[Heading]] and [[*Heading]] as fuzzy
                   ;; links (the latter retains its leading star in :path).
                   (let ((anchor (unless page-root-p
                                   (or custom-id id
                                       (et:vitepress--slugify title)))))
                     (add "fuzzy" title page anchor title)
                     (add "fuzzy" (concat "*" title) page anchor title)))
                 (walk (org-element-contents element) page))))))
      (walk (org-element-contents ast) nil))
    targets))

(defun et:vitepress--read-tab (file)
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let* ((title (or (et:vitepress--file-property "TITLE")
                      (file-name-base file)))
           (slug (et:vitepress--slugify (file-name-base file)))
           (order (et:vitepress--parse-order
                   (et:vitepress--file-property "VITEPRESS_TAB_ORDER") file))
           (ast (org-element-parse-buffer))
           (pages (et:vitepress--headline-pages ast slug file)))
      (unless pages
        (error "%s: no page headings found" file))
      (et:vitepress--tab-create :title title :slug slug :order order
                               :file file :pages pages
                               :nav
                               (et:vitepress--build-navigation ast pages)
                               :targets
                               (et:vitepress--headline-targets ast pages)))))

(defun et:vitepress--all-pages (pages)
  (cl-mapcan
   (lambda (page)
     (cons page (et:vitepress--all-pages (et:vitepress--page-children page))))
   pages))

(defun et:vitepress--page-org (page)
  "Extract PAGE body, excluding nested child pages, from current buffer."
  (let ((start (or (et:vitepress--page-contents-begin page)
                   (et:vitepress--page-contents-end page)))
        (end (et:vitepress--page-contents-end page)))
    (if (not (and start end))
        ""
      (let ((body (buffer-substring-no-properties start end))
            (removed nil))
        ;; Remove every nearest child page subtree.  Removing a parent removes its
        ;; marked descendants too.
        (dolist (child (et:vitepress--page-children page))
          (push (cons (- (et:vitepress--page-begin child) start)
                      (- (et:vitepress--page-end child) start))
                removed))
        (with-temp-buffer
          (insert body)
          (dolist (range (sort removed (lambda (a b) (> (car a) (car b)))))
            (delete-region (1+ (car range)) (1+ (cdr range))))
          ;; Direct children of a page should be H2 in the resulting MDX.  Edit
          ;; only parsed headlines, so stars inside source blocks are untouched.
          (let ((ast (org-element-parse-buffer))
                replacements)
            (org-element-map ast 'headline
              (lambda (headline)
                (push (cons (org-element-property :begin headline)
                            (max 2 (1+ (- (org-element-property :level headline)
                                          (et:vitepress--page-level page)))))
                      replacements)))
            (dolist (replacement
                     (sort replacements (lambda (a b) (> (car a) (car b)))))
              (goto-char (car replacement))
              (when (looking-at "\\*+")
                (replace-match (make-string (cdr replacement) ?*) t t))))
          (buffer-string))))))

(defun et:vitepress--table-cell (cell info)
  "Export Org table CELL as one Markdown cell using INFO."
  (let ((text (string-trim
               (org-export-data (org-element-contents cell) info))))
    (setq text (replace-regexp-in-string "[\n\r]+" "<br />" text))
    (replace-regexp-in-string "|" "\\\\|" text t t)))

(defun et:vitepress--table (table _contents info)
  "Export an Org TABLE as a GitHub-style Markdown table.

The stock Markdown exporter emits HTML tables.  Raw HTML is parsed as JSX in
MDX, and attributes such as `class' then make VitePress compilation fail."
  (let (rows)
    (dolist (row (org-element-contents table))
      (when (and (eq (org-element-type row) 'table-row)
                 (eq (org-element-property :type row) 'standard))
        (push (mapcar (lambda (cell)
                        (et:vitepress--table-cell cell info))
                      (seq-filter
                       (lambda (element)
                         (eq (org-element-type element) 'table-cell))
                       (org-element-contents row)))
              rows)))
    (setq rows (nreverse rows))
    (when rows
      (let* ((width (apply #'max (mapcar #'length rows)))
             (padded
              (mapcar (lambda (row)
                        (append row (make-list (- width (length row)) "")))
                      rows))
             (render
              (lambda (row)
                (concat "| " (string-join row " | ") " |\n"))))
        (concat (funcall render (car padded))
                (funcall render (make-list width "---"))
                (mapconcat render (cdr padded) ""))))))

(defun et:vitepress--src-block (src-block _contents _info)
  "Export SRC-BLOCK as fenced Markdown suitable for MDX.

The stock Markdown exporter uses four-space-indented code.  MDX can parse
angle brackets inside those blocks as JSX, so use explicit fences instead."
  (let* ((language (or (org-element-property :language src-block) ""))
         (value (org-element-property :value src-block))
         (max-run 0)
         (start 0))
    (while (string-match "`+" value start)
      (setq max-run (max max-run (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (let ((fence (make-string (max 3 (1+ max-run)) ?`)))
      (format "%s%s\n%s%s\n" fence language value fence))))

(defun et:vitepress--footnote-reference (footnote-reference _contents info)
  "Export FOOTNOTE-REFERENCE using GFM footnote syntax."
  (format "[^%d]"
          (org-export-get-footnote-number footnote-reference info)))

(defun et:vitepress--footnote-definition (number definition info)
  "Export footnote NUMBER with Org DEFINITION using INFO."
  (let* ((text (string-trim (org-export-data definition info)))
         (lines (split-string text "\n" nil))
         (first (or (car lines) ""))
         (rest (cdr lines)))
    (concat (format "[^%d]: %s" number first)
            (when rest
              (concat "\n"
                      (mapconcat
                       (lambda (line) (concat "    " line))
                       rest "\n"))))))

(defun et:vitepress--footnote-section (info)
  "Export all footnote definitions from INFO using GFM syntax."
  (when-let* ((definitions (org-export-collect-footnote-definitions info)))
    (mapconcat
     (pcase-lambda (`(,number ,_type ,definition))
       (et:vitepress--footnote-definition number definition info))
     definitions "\n\n")))

(defun et:vitepress--inner-template (contents info)
  "Append GFM footnote definitions to exported CONTENTS."
  (let ((footnotes (et:vitepress--footnote-section info)))
    (concat contents
            (when footnotes (concat "\n\n" footnotes "\n")))))

(defun et:vitepress--target-url (target)
  "Return the site URL for TARGET."
  (let* ((page (et:vitepress--target-page target))
         (anchor (et:vitepress--target-anchor target))
         (page-url (if (et:vitepress--page-home page)
                       "/"
                     (concat "/" (et:vitepress--page-path page)))))
    (cond
     ((and anchor (eq page et:vitepress--current-page))
      (concat "#" anchor))
     (anchor (concat page-url "#" anchor))
     (t page-url))))

(defun et:vitepress--link (link description info)
  "Export LINK, rewriting document headline targets to generated page URLs."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (target (and et:vitepress--link-targets
                      (gethash (cons type path)
                               et:vitepress--link-targets))))
    (if (not target)
        (org-md-link link description info)
      (format "[%s](%s)"
              (if (or (null description) (string-empty-p description))
                  (et:vitepress--target-title target)
                description)
              (et:vitepress--target-url target)))))

(org-export-define-derived-backend 'et:vitepress-md 'md
  :translate-alist '((footnote-reference . et:vitepress--footnote-reference)
                     (inner-template . et:vitepress--inner-template)
                     (link . et:vitepress--link)
                     (src-block . et:vitepress--src-block)
                     (table . et:vitepress--table)))

(defun et:vitepress--org-to-md (org-text page targets)
  (let ((org-export-with-author nil)
        (org-export-with-broken-links 'mark)
        (org-export-with-creator nil)
        (org-export-with-date nil)
        (org-export-with-properties nil)
        (org-export-with-section-numbers nil)
        (org-export-with-tags nil)
        (org-export-with-toc nil)
        (org-md-headline-style 'atx)
        (et:vitepress--current-page page)
        (et:vitepress--link-targets targets))
    ;; A synthetic root keeps page-local headings at H2 and below.  Org's
    ;; body-only exporter would otherwise promote the shallowest heading to H1.
    (let ((markdown
           (org-export-string-as
            (concat "* VitePressPageRoot\n" org-text) 'et:vitepress-md t)))
      (setq markdown
            (replace-regexp-in-string
             "\\`[ \t\n\r]*# VitePressPageRoot[ \t]*\n*" "" markdown))
      (string-trim markdown))))

(defun et:vitepress--yaml-string (string)
  "Encode STRING in a YAML-compatible double-quoted form."
  (json-serialize string))

(defun et:vitepress--write-page (page source-file src-dir targets)
  (with-temp-buffer
    (insert-file-contents source-file)
    (org-mode)
    (let* ((org-text (et:vitepress--page-org page))
           (markdown (et:vitepress--org-to-md org-text page targets))
           (output (expand-file-name
                    (if (et:vitepress--page-home page)
                        "index.md"
                      (concat (et:vitepress--page-path page) ".md"))
                    src-dir)))
      (make-directory (file-name-directory output) t)
      (with-temp-file output
        (insert "---\n"
                "title: " (et:vitepress--yaml-string
                           (et:vitepress--page-title page)) "\n"
                "---\n\n"
                "# " (et:vitepress--page-title page) "\n\n")
        (unless (string-empty-p markdown)
          (insert markdown "\n"))))))

(defun et:vitepress--json-string (value)
  "Return VALUE as pretty-printed JSON, which is also valid JavaScript."
  (with-temp-buffer
    (insert (json-encode value))
    (json-pretty-print-buffer)
    (string-trim-right (buffer-string))))

(defun et:vitepress--first-page (tab)
  "Return the first generated page in TAB."
  (car (et:vitepress--all-pages (et:vitepress--tab-pages tab))))

(defun et:vitepress--rewrite-link (tree old new)
  "Destructively replace links equal to OLD with NEW in TREE."
  (cond
   ((vectorp tree)
    (dotimes (index (length tree))
      (et:vitepress--rewrite-link (aref tree index) old new)))
   ((consp tree)
    (if (and (equal (car tree) "link") (equal (cdr tree) old))
        (setcdr tree new)
      (et:vitepress--rewrite-link (car tree) old new)
      (et:vitepress--rewrite-link (cdr tree) old new)))))

(defun et:vitepress--set-home-page (tabs)
  "Make the first generated page in TABS the site root."
  (let* ((page (et:vitepress--first-page (car tabs)))
         (old-link (concat "/" (et:vitepress--page-path page))))
    (setf (et:vitepress--page-home page) t)
    (dolist (tab tabs)
      (et:vitepress--rewrite-link (et:vitepress--tab-nav tab)
                                  old-link "/"))))

(defun et:vitepress--navigation (tabs)
  "Return (NAV . SIDEBAR) for TABS."
  (if (= (length tabs) 1)
      (cons [] (vconcat (et:vitepress--tab-nav (car tabs))))
    (let (nav sidebar)
      (dolist (tab tabs)
        (let ((page (et:vitepress--first-page tab)))
          (push `(("text" . ,(et:vitepress--tab-title tab))
                  ("link" . ,(if (et:vitepress--page-home page)
                                  "/"
                                (concat "/"
                                        (et:vitepress--page-path page)))))
                nav)
          (push (cons (concat "/" (et:vitepress--tab-slug tab) "/")
                      (vconcat (et:vitepress--tab-nav tab)))
                sidebar)))
      ;; The first page is emitted at `/`, outside its collection prefix.  Add
      ;; a root-path alias so that page still receives its collection sidebar.
      ;; VitePress prefers the more-specific collection prefixes elsewhere.
      (cons (vconcat (nreverse nav))
            (append (nreverse sidebar)
                    (list
                     (cons "/"
                           (vconcat
                            (et:vitepress--tab-nav (car tabs))))))))))

(defun et:vitepress--write-navigation (tabs src-dir)
  "Write generated VitePress navigation for TABS into SRC-DIR."
  (pcase-let ((`(,nav . ,sidebar) (et:vitepress--navigation tabs)))
    (with-temp-file (expand-file-name "navigation.mjs" src-dir)
      (insert "// Generated by documentation/generate.el.\n"
              "export const nav = "
              (et:vitepress--json-string nav) ";\n\n"
              "export const sidebar = "
              (et:vitepress--json-string sidebar) ";\n"))))

(defun et:vitepress--tab-less-p (a b)
  (let ((ao (et:vitepress--tab-order a))
        (bo (et:vitepress--tab-order b)))
    (cond ((and ao bo)
           (if (= ao bo)
               (string-lessp (et:vitepress--tab-title a)
                             (et:vitepress--tab-title b))
             (< ao bo)))
          (ao t)
          (bo nil)
          (t (string-lessp (et:vitepress--tab-title a)
                           (et:vitepress--tab-title b))))))

(defun et:vitepress--empty-directory (dir)
  "Remove all generated content inside DIR."
  (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
    (cond ((file-symlink-p entry) (delete-file entry))
          ((file-directory-p entry) (delete-directory entry t))
          (t (delete-file entry)))))


;;;###autoload

(defun et:vitepress-generate (dir)
  "Generate VitePress content in the existing project at DIR/site/."
  (interactive "DOrg documentation directory: ")
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (unless (file-directory-p dir)
    (error "Not a directory: %s" dir))
  (let* ((files (directory-files dir t "\\.org\\'" t))
         (site (expand-file-name "site" dir))
         (src (expand-file-name "src" site))
         (tabs (sort (mapcar #'et:vitepress--read-tab files)
                     #'et:vitepress--tab-less-p)))
    (unless tabs
      (error "No .org files found directly inside %s" dir))
    (unless (file-directory-p site)
      (error "VitePress project does not exist: %s" site))
    (et:vitepress--set-home-page tabs)
    (if (file-directory-p src)
        (et:vitepress--empty-directory src)
      (make-directory src t))
    (dolist (tab tabs)
      (let ((all-pages (et:vitepress--all-pages
                        (et:vitepress--tab-pages tab))))
        (dolist (page all-pages)
          (et:vitepress--write-page page (et:vitepress--tab-file tab) src
                                    (et:vitepress--tab-targets tab)))))
    (et:vitepress--write-navigation tabs src)
    (message "Generated %d collection(s) in %s" (length tabs) src)
    src))

(defun et:vitepress-generate-batch ()
  "Command-line entry point for `et:vitepress-generate'."
  (let ((dir (pop command-line-args-left)))
    (unless dir
      (error "Usage: generate.el DIR"))
    (when command-line-args-left
      (error "Unexpected argument(s): %s"
             (string-join command-line-args-left " ")))
    (et:vitepress-generate dir)))

(when (member "-scriptload" command-line-args)
  (condition-case err
      (et:vitepress-generate-batch)
    (error
     (message "generate.el: %s" (error-message-string err))
     (kill-emacs 1))))

(provide 'generate)
;;; generate.el ends here
