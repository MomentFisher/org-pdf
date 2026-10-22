;;; org-pdf-layout.el --- Lay out an org AST into PDF draw operations  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;;; Commentary:

;; This turns an `org-element' parse tree into a flat list of draw
;; operations, one list per page.  A draw operation is one of:
;;
;;   (:text X Y STRING FONT SIZE RGB)   -- STRING is raw (pre-encoding) text
;;   (:rect X Y W H RGB)                -- filled rectangle
;;   (:line X Y1 X2 Y2 WIDTH RGB)       -- stroked line
;;
;; Coordinates are top-left origin (y grows downward), matching how one
;; naturally lays out a page; org-pdf.el flips them when writing the content
;; stream.  Text is kept as Lisp strings here and only encoded to bytes once
;; the font subset is known, so the layout engine never touches glyph ids.

;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'org-src)
(require 'url-util)
(require 'org-pdf-font)
(require 'org-pdf-image)

(defcustom org-pdf-page-size 'a4
  "Default page size.  A symbol from `org-pdf-page-sizes' or (WIDTH . HEIGHT)
in PDF points."
  :type '(choice symbol (cons number number))
  :group 'org-pdf)

(defconst org-pdf-page-sizes
  '((a4     . (595.28 . 841.89))
    (letter . (612.0 . 792.0))
    (a3     . (841.89 . 1190.55))
    (legal  . (612.0 . 1008.0)))
  "Named page sizes in PDF points (1 pt = 1/72 inch).")

(defconst org-pdf--color-black '(0 0 0))
(defconst org-pdf--color-gray  '(0.35 0.35 0.35))
(defconst org-pdf--color-link  '(0.0 0.25 0.65))
(defconst org-pdf--color-code-bg '(0.955 0.955 0.94))
(defconst org-pdf--color-rule '(0.8 0.8 0.8))

(defun org-pdf-page-dimensions (size)
  "Return (WIDTH . HEIGHT) in points for SIZE, a symbol or a cons."
  (cond
   ((consp size) size)
   ((cdr (assq size org-pdf-page-sizes)))
   (t (cdr (assq 'a4 org-pdf-page-sizes)))))

;;;; Layout state --------------------------------------------------------

(defvar org-pdf--include-directory nil
  "Directory against which relative #+include: paths are resolved.
Bound to the directory of the org file currently being expanded.")

(defvar org-pdf--include-stack nil
  "List of already-expanded include file names, to break include cycles.")

(defvar org-pdf--in-include nil
  "Non-nil while rendering content pulled in by #+include:.
Suppresses a second title block coming from an included file.")

(defvar org-pdf--current-level 0
  "Level of the section currently being rendered.
Headlines pulled in by #+include: are shifted so they start one level below
this, i.e. nested inside the section that contains the include keyword.")

(cl-defstruct (org-pdf--layout
               (:constructor org-pdf--make-layout))
  page-w page-h margin-left margin-right margin-top margin-bottom
  base-size
  cjk-font            ; parsed font plist or nil
  head-font           ; symbol: :helvetica or :times
  has-cjk             ; t once any CJK glyph has been requested
  used-cids           ; hash of codepoints needing embedding
  pages               ; list of pages, each a list of ops (page order reversed)
  cur                 ; ops of the page being built (reverse order)
  x y                 ; pen position (top-left origin)
  avail               ; current right edge (x limit)
  indent              ; current left indent
  bookmarks           ; list of (LEVEL TITLE PAGE-INDEX PDF-Y), reading order
  counters            ; vector: section number counter per headline level
  number-headings)    ; non-nil to prefix headlines with "1.2.3" numbers

(defun org-pdf--new-page (st)
  "Finish the current page in ST and start a fresh one."
  (when (org-pdf--layout-cur st)
    (push (nreverse (org-pdf--layout-cur st)) (org-pdf--layout-pages st)))
  (setf (org-pdf--layout-cur st) nil
        (org-pdf--layout-x st) (+ (org-pdf--layout-margin-left st)
                                  (org-pdf--layout-indent st))
        (org-pdf--layout-y st) (org-pdf--layout-margin-top st)
        (org-pdf--layout-avail st) (- (org-pdf--layout-page-w st)
                                      (org-pdf--layout-margin-right st))))

(defun org-pdf--page-bottom (st)
  (- (org-pdf--layout-page-h st) (org-pdf--layout-margin-bottom st)))

(defun org-pdf--ensure (st height)
  "Start a new page in ST if HEIGHT points will not fit on the current page."
  (when (> (+ (org-pdf--layout-y st) height) (org-pdf--page-bottom st))
    (org-pdf--new-page st)))

(defun org-pdf--emit (st op)
  "Add draw OP to the current page of ST."
  (push op (org-pdf--layout-cur st)))

;;;; Inline text extraction ----------------------------------------------

(defun org-pdf--inline-text (node)
  "Return the plain text of org NODE, a string, a list of nodes, or an element.
Trailing blanks recorded by org-element in each inline object's :post-blank
are restored as spaces."
  (cond
   ((null node) "")
   ((stringp node) (substring-no-properties node))
   ((not (listp node)) (format "%s" node))
   ;; A list: either a genuine org element (org-element-type returns a symbol)
   ;; or a plain list of child nodes/strings (e.g. a headline's :title).
   ((org-element-type node)
    (let ((blank (or (org-element-property :post-blank node) 0)))
      (concat
       (pcase (org-element-type node)
         ((or 'bold 'italic 'underline 'strike-through 'subscript 'superscript)
          (org-pdf--inline-text (org-element-contents node)))
         ((or 'code 'verbatim) (or (org-element-property :value node) ""))
         ('plain-text (substring-no-properties node))
         ('entity
          (concat (or (org-element-property :utf-8 node)
                      (org-element-property :name node) "")
                  ;; entities keep a trailing space via post-blank too
                  ""))
         ('link
          (let ((desc (org-element-contents node)))
            (if desc (org-pdf--inline-text desc)
              (or (org-element-property :raw-link node) ""))))
         ('line-break "\n")
         ('latex-fragment (or (org-element-property :value node) ""))
         (_ (org-pdf--inline-text (org-element-contents node))))
       (if (> blank 0) (make-string blank ?\s) ""))))
   (t
    (mapconcat #'org-pdf--inline-text node ""))))

(defun org-pdf--url-hexify-path (path)
  "Percent-encode the characters of PATH that would break a URL."
  (let ((out (list)) (i 0))
    (while (< i (length path))
      (let* ((c (aref path i))
             (s (substring path i (1+ i))))
        (push (if (or (and (>= c ?a) (<= c ?z))
                      (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?0) (<= c ?9))
                      (memq c '(?- ?_ ?. ?~ ?/ ?:)))
                  s
                (mapconcat (lambda (b) (format "%%%02X" b))
                           (string-to-list (encode-coding-string s 'utf-8))
                           ""))
              out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun org-pdf--link-target (node)
  "Return a PDF link target for org link NODE, or nil.
Only hyperlinks that can be made clickable are reported: web and mail
links map straight to their URI; file links become file:// URLs.  A fuzzy
link whose text is recognisably a URL (org only keeps it as a plain link)
is promoted to an https URL.  Internal links (id:, custom ids, other fuzzy
targets) have no destination in the generated PDF and yield nil, so their
text is drawn without an annotation."
  (let ((type (org-element-property :type node))
        (path (org-element-property :path node)))
    (pcase type
      ((or "http" "https" "ftp" "mailto" "news")
       (org-element-property :raw-link node))
      ("file"
       (let* ((dir (or org-pdf--include-directory default-directory))
              (abs (if (file-name-absolute-p path)
                       (expand-file-name path)
                     (expand-file-name path dir))))
         (concat "file://" (org-pdf--url-hexify-path abs))))
      ("coderef"
       ;; "[[(ref:NAME)][desc]]": org keeps only NAME; rebuild the anchor
       ;; name that the code block's "(ref:NAME)" annotation records.
       (concat "ref:" path))
      ("fuzzy"
       ;; org treats a bare [[www.example.com]] as a fuzzy (internal) link.
       ;; When it looks like a host name, make it a clickable web link.
       ;; A "(ref:NAME)" target points at a "ref:NAME" annotation in a code
       ;; block; keep it as-is so the writer can resolve it to a destination.
       (cond
        ((and path (string-match-p "\\`(ref:[^)]+)\\'" path)) path)
        ((and path
              (string-match-p "\\`\\(?:[A-Za-z0-9-]+\\.\\)+[A-Za-z]\\{2,\\}\\(?:[/?#].*\\)?\\'" path))
         (concat "https://" path))
        (t nil)))
      (_ nil))))

(defun org-pdf--inline-segments (node &optional link)
  "Return NODE's text as a list of (TEXT . LINK) segments.
LINK is the target URL that TEXT should carry, or nil for plain text.
Links nest their description's runs, so emphasis inside a link keeps the
link target, and `:post-blank' spaces stay outside it."
  (cond
   ((null node) nil)
   ((stringp node) (list (cons (substring-no-properties node) link)))
   ((not (listp node)) (list (cons (format "%s" node) link)))
   ;; A plain list of child nodes/strings (not an element): keep its order.
   ((null (org-element-type node))
    (apply #'append (mapcar (lambda (n) (org-pdf--inline-segments n link))
                            node)))
   (t
    (let ((blank (or (org-element-property :post-blank node) 0))
          (segs nil))
      (setq segs
            (pcase (org-element-type node)
              ((or 'bold 'italic 'underline 'strike-through
                   'subscript 'superscript)
               (org-pdf--inline-segments (org-element-contents node) link))
              ((or 'code 'verbatim)
               (list (cons (or (org-element-property :value node) "") link)))
              ('plain-text (list (cons (substring-no-properties node) link)))
              ('entity
               (list (cons (or (org-element-property :utf-8 node)
                               (org-element-property :name node) "")
                           link)))
              ('link
               (let ((desc (org-element-contents node)))
                 (if desc
                     (org-pdf--inline-segments
                      desc (or (org-pdf--link-target node) link))
                   (list (cons (or (org-element-property :raw-link node) "")
                               link)))))
              ('line-break (list (cons "\n" link)))
              ('latex-fragment
               (list (cons (or (org-element-property :value node) "") link)))
              (_ (org-pdf--inline-segments (org-element-contents node) link))))
      (if (> blank 0)
          (append segs (list (cons (make-string blank ?\s) nil)))
        segs)))))

(defun org-pdf--emphasis-font (node font)
  "Return the font variant for NODE derived from base FONT symbol.
FONT is one of :helvetica / :times / :courier; returns the styled variant."
  (let ((bold nil) (italic nil))
    (org-element-map (if (listp node) node (list node))
        '(bold italic)
      (lambda (n)
        (if (eq (org-element-type n) 'italic) (setq italic t) (setq bold t))))
    (pcase font
      (:courier (cond ((and bold italic) 'courier-bold-oblique)
                      (bold 'courier-bold) (italic 'courier-oblique) (t 'courier)))
      (:times (cond ((and bold italic) 'times-bold-italic)
                    (bold 'times-bold) (italic 'times-italic) (t 'times)))
      (_ (cond ((and bold italic) 'helvetica-bold-oblique)
               (bold 'helvetica-bold) (italic 'helvetica-oblique) (t 'helvetica))))))

;;;; Measurement ----------------------------------------------------------

(defun org-pdf--char-width (st ch size font)
  "Return the advance width in points of CH at SIZE using FONT in state ST."
  (let ((c (aref (if (stringp ch) ch (char-to-string ch)) 0)))
    (cond
     ((org-pdf-cjk-p c)
      (setf (org-pdf--layout-has-cjk st) t)
      (puthash c t (org-pdf--layout-used-cids st))
      ;; advance from the font if available, else full em (typical for CJK)
      (let* ((ft (org-pdf--layout-cjk-font st))
             (cm (and ft (plist-get ft :cmap)))
             (gid (and cm (gethash c cm))))
        (if (and ft gid (< gid (length (plist-get ft :widths))))
            (* size (/ (float (aref (plist-get ft :widths) gid))
                       (plist-get ft :units)))
          size)))
     ((org-pdf--base14-p c)
      (* size (/ (org-pdf-base14-width font c) 1000.0)))
     (t
      ;; other non-Latin: fall back to the CJK font too
      (setf (org-pdf--layout-has-cjk st) t)
      (puthash c t (org-pdf--layout-used-cids st))
      size))))

(defun org-pdf--string-width (st s size font)
  "Return the rendered width in points of string S at SIZE in FONT."
  (let ((w 0.0))
    (dotimes (i (length s))
      (setq w (+ w (org-pdf--char-width st (substring s i (1+ i)) size font))))
    w))

;;;; Wrapping ------------------------------------------------------------

(defun org-pdf--split-tokens (s)
  "Split S into break-eligible tokens: CJK chars singletons, Latin words.
Every CJK character is its own token so text may break at any CJK boundary;
runs of non-space Latin are grouped into words.  Any whitespace (space, tab,
newline) collapses to a single separatory space token."
  (let ((tokens nil) (cur nil) (i 0) (n (length s)))
    (cl-flet ((flush () (when cur (push (apply #'concat (nreverse cur)) tokens)
                          (setq cur nil))))
      (while (< i n)
        (let ((ch (substring s i (1+ i))))
          (cond
           ((memq (aref ch 0) '(?\s ?\t ?\n ?\r ?\f)) (flush) (push " " tokens))
           ((org-pdf-cjk-p (aref ch 0)) (flush) (push ch tokens))
           (t (push ch cur))))
        (setq i (1+ i)))
      (flush))
    (nreverse tokens)))

(defun org-pdf--tokenize-segments (segs)
  "Split the (TEXT . LINK) SEGS into (TOKEN . LINK) units for wrapping.
Whitespace collapses to single space tokens, as in `org-pdf--split-tokens'."
  (let ((out nil))
    (dolist (seg segs)
      (dolist (tok (org-pdf--split-tokens (car seg)))
        (push (cons tok (cdr seg)) out)))
    (nreverse out)))

(defun org-pdf--push-run (runs link str)
  "Append STR with LINK to RUNS, merging it into the last run when possible.
Returns the new RUNS (a reversed list of (LINK . STRING))."
  (if (and runs (equal (caar runs) link))
      (cons (cons link (concat (cdar runs) str)) (cdr runs))
    (cons (cons link str) runs)))

(defun org-pdf--wrap-segments (st segs size font max-width)
  "Wrap SEGS into a list of (STRING WIDTH RUNS) lines fitting MAX-WIDTH.
SEGS is a list of (TEXT . LINK); every line's RUNS is a list of
(LINK . STRING) covering the line in order, so the caller can colour and
annotate each link's span.  State ST provides metrics; SIZE and FONT select
the font."
  (let ((lines nil) (line "") (lw 0.0) (runs nil) (runw 0.0))
    (cl-flet ((flush-line ()
                (when (> (length line) 0)
                  (push (list line lw (nreverse runs)) lines))
                (setq line "" lw 0.0 runs nil runw 0.0)))
      (dolist (unit (org-pdf--tokenize-segments segs))
        (let* ((tok (car unit))
               (link (cdr unit))
               (tw (org-pdf--string-width st tok size font)))
          (cond
           ((and (not (equal tok " "))
                 (> (+ lw tw) max-width)
                 (> (length line) 0))
            ;; flush current line, start new one with TOK
            (flush-line)
            (unless (equal tok " ")
              (setq line tok lw tw
                    runs (list (cons link tok)) runw tw)))
           (t
            (setq line (concat line tok)
                  lw (+ lw tw)
                  runs (org-pdf--push-run runs link tok)
                  runw (+ runw tw))))
          ;; hard-break a single token wider than the line
          (while (> lw max-width)
            (let ((accw 0.0) (j 0) (len (length line)))
              (while (and (< j len)
                          (<= (+ accw (org-pdf--string-width
                                       st (substring line j (1+ j)) size font))
                              max-width))
                (setq accw (+ accw (org-pdf--string-width
                                    st (substring line j (1+ j)) size font)))
                (setq j (1+ j)))
              (when (> j 0)
                (push (list (substring line 0 j) accw
                            (list (cons link (substring line 0 j))))
                      lines)
                (setq line (substring line j)
                      lw (- lw accw)
                      runs (and (> (length line) 0) (list (cons link line)))
                      runw lw))))))
      (flush-line))
    (or (nreverse lines) (list (list "" 0.0 nil)))))

(defun org-pdf--wrap (st text size font max-width)
  "Wrap TEXT into a list of (STRING . WIDTH) lines fitting MAX-WIDTH.
Uses state ST for metrics; SIZE and FONT select the font."
  (mapcar (lambda (ln) (cons (nth 0 ln) (nth 1 ln)))
          (org-pdf--wrap-segments st (list (cons text nil)) size font max-width)))

(defun org-pdf--wrap-hanging (st text size font max-width hang)
  "Wrap TEXT into a list of (STRING . WIDTH) lines, honouring hard newlines.
The first line may use the full MAX-WIDTH; every following line (whether it
follows an embedded newline or a wrap) has MAX-WIDTH - HANG available and is
meant to be drawn HANG to the right of the first line.  Used for list items,
where the marker starts the first line and the text hangs past it."
  (let ((out nil) (first t))
    (dolist (seg (split-string text "\n" t))
      (let ((w (if first max-width (max 1.0 (- max-width hang)))))
        (setq out (append out (org-pdf--wrap st (string-trim seg) size font w))))
      (setq first nil))
    (or out (list (cons "" 0.0)))))

(defun org-pdf--segments-append (segs text link)
  "Append TEXT carrying LINK to SEGS, merging into the last segment.
SEGS is a list of (TEXT . LINK); returns the extended list."
  (if (and segs (equal (cdar segs) link))
      (append (butlast segs) (list (cons (concat (caar (last segs)) text) link)))
    (append segs (list (cons text link)))))

(defun org-pdf--trim-segments (segs)
  "Remove leading and trailing whitespace from SEGS, keeping each link.
Returns a fresh list; SEGS itself is not modified."
  (let ((copy (mapcar (lambda (s) (cons (car s) (cdr s))) segs)))
    ;; leading whitespace
    (while (and copy (string-match-p "\\`[ \t\r\n]+\\'" (caar copy)))
      (setq copy (cdr copy)))
    (when copy
      (setcar (car copy) (replace-regexp-in-string "\\`[ \t\r\n]+" "" (caar copy)))
      (when (string-empty-p (caar copy)) (setq copy (cdr copy))))
    ;; trailing whitespace
    (while (and copy (string-match-p "\\`[ \t\r\n]+\\'" (car (car (last copy)))))
      (setq copy (butlast copy)))
    (when copy
      (setcar (car (last copy))
              (replace-regexp-in-string "[ \t\r\n]+\\'" "" (car (car (last copy))))))
    copy))

(defun org-pdf--join-hanging-lines (segs)
  "Join the list of (TEXT . LINK) SEGS into a list of hard lines.
Each hard line is itself a segment list.  Newlines inside a segment start a
new line, and no artificial break is added between segments -- the list
marker and the item's text must share a line."
  (let ((lines nil) (cur nil))
    (dolist (seg segs)
      (let ((parts (split-string (car seg) "\n"))
            (link (cdr seg)))
        (while parts
          (setq cur (append cur (list (cons (car parts) link)))
                parts (cdr parts))
          (when parts
            (push (append cur nil) lines)
            (setq cur nil)))))
    (push (append cur nil) lines)
    (nreverse lines)))

(defun org-pdf--wrap-hanging-segments (st segs size font max-width hang)
  "Wrap SEGS like `org-pdf--wrap-hanging', keeping link runs per line.
SEGS is a list of (TEXT . LINK) and is flowed as one paragraph: only the
embedded newlines start new lines, so a list marker segment and the item
text stay on the same line.  Returns (STRING WIDTH RUNS) lines."
  (let ((out nil) (first t))
    (dolist (hard (org-pdf--join-hanging-lines segs))
      (let ((seg (org-pdf--trim-segments hard)))
        (when seg
          (setq out (append out (org-pdf--wrap-segments
                                 st seg size font
                                 (if first max-width
                                   (max 1.0 (- max-width hang)))))
                first nil))))
    (or out (list (list "" 0.0 nil)))))

(defun org-pdf--draw-segments (st runs x y size font rgb)
  "Emit text and link annotations for RUNS at baseline (X,Y).
RUNS is the line's list of (LINK . STRING) runs; plain text is drawn in RGB,
while text carrying a link is drawn in `org-pdf--color-link' and underlined,
and gets a clickable `:link' op spanning its box.  Returns the pen X after
the last run."
  (let ((x x))
    (dolist (run runs)
      (let* ((link (car run))
             (str (cdr run))
             (w (org-pdf--string-width st str size font))
             (rgb (if link org-pdf--color-link rgb)))
        (when (> (length str) 0)
          (org-pdf--draw-line-of-text st str x y size font rgb)
          (when (and link (> (length (string-trim str)) 0))
            ;; Underline a hair above the baseline, then annotate the run.
            (org-pdf--emit st (list :line x (+ y 1.5) (+ x w) (+ y 1.5) 0.6 rgb))
            (org-pdf--emit st (list :link link x (- y size) w (* size 1.3))))
          (setq x (+ x w)))))
    x))

;;;; Primitive drawing ---------------------------------------------------

(defun org-pdf--draw-line-of-text (st str x y size font rgb)
  "Emit a text op for STR at (X,Y) baseline, SIZE, FONT, colour RGB."
  (org-pdf--emit st (list :text x y str font size rgb)))

(defun org-pdf--flow (st text size font rgb &optional indent-extra)
  "Lay out a paragraph TEXT at the current pen, wrapping and paginating in ST.
Advances ST's pen downward.  INDENT-EXTRA adds to the current indent.
TEXT is either a string or a list of (TEXT . LINK) segments, in which case
linked stretches are drawn in the link colour, underlined and clickable."
  (let* ((indent (+ (org-pdf--layout-indent st) (or indent-extra 0)))
         (left (+ (org-pdf--layout-margin-left st) indent))
         (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st)))
         (maxw (- right left))
         (lh (* size 1.35))
         (lines (org-pdf--wrap-segments st (if (stringp text)
                                               (list (cons text nil))
                                             text)
                                        size font maxw)))
    (setf (org-pdf--layout-x st) left
          (org-pdf--layout-avail st) right
          (org-pdf--layout-indent st) indent)
    (dolist (line lines)
      (org-pdf--ensure st lh)
      (org-pdf--draw-segments st (nth 2 line) left
                              (org-pdf--layout-y st) size font rgb)
      (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) lh)))
    (setf (org-pdf--layout-indent st) (- indent (or indent-extra 0)))))

;;;; Block-level rendering -----------------------------------------------

;;;; Syntax highlighting --------------------------------------------------

(defcustom org-pdf-syntax-highlight t
  "Whether to syntax-highlight src blocks.
Colouring uses Emacs's own font-lock for the block's language, so it works
for any language with a major mode installed.  Faces are mapped to colours by
`org-pdf--face-colors'."
  :type 'boolean
  :group 'org-pdf)

(defconst org-pdf--face-colors
  '((font-lock-keyword-face          . (0.60 0.10 0.60))
    (font-lock-builtin-face          . (0.20 0.35 0.70))
    (font-lock-function-name-face    . (0.10 0.35 0.75))
    (font-lock-variable-name-face    . (0.55 0.30 0.05))
    (font-lock-type-face             . (0.15 0.45 0.35))
    (font-lock-constant-face         . (0.10 0.45 0.55))
    (font-lock-string-face           . (0.55 0.20 0.10))
    (font-lock-doc-face              . (0.45 0.40 0.20))
    (font-lock-comment-face          . (0.45 0.50 0.45))
    (font-lock-comment-delimiter-face . (0.45 0.50 0.45))
    (font-lock-preprocessor-face     . (0.50 0.35 0.60))
    (font-lock-negation-char-face    . (0.10 0.45 0.55))
    (font-lock-warning-face          . (0.75 0.10 0.10))
    (font-lock-regexp-grouping-backslash . (0.55 0.20 0.10))
    (font-lock-regexp-grouping-construct . (0.55 0.20 0.10)))
  "Map of font-lock faces to RGB (each component 0..1) for src highlighting.")

(defun org-pdf--face-rgb (face)
  "Return an RGB list for FACE, or nil if FACE has no mapped colour."
  (cond
   ((null face) nil)
   ((listp face)
    ;; A face spec may be a plist (:foreground ...) or a list of faces/modes.
    (or (and (keywordp (car face))
             (let ((fg (plist-get face :foreground)))
               (and fg (org-pdf--color-rgb fg))))
        (cl-some #'org-pdf--face-rgb (if (listp face) face (list face)))))
   ((symbolp face)
    (or (cdr (assq face org-pdf--face-colors))
        ;; Fall back to a colour the face itself carries.
        (and (facep face)
             (let ((fg (face-attribute face :foreground nil 'default)))
               (and fg (not (eq fg 'unspecified)) (org-pdf--color-rgb fg))))))
   (t nil)))

(defun org-pdf--color-rgb (color)
  "Convert COLOUR (a #RRGGBB string or a colour name) to an RGB list, or nil."
  (cond
   ((and (stringp color) (string-match-p "\\`#[0-9a-fA-F]\\{6\\}\\'" color))
    (list (/ (string-to-number (substring color 1 3) 16) 255.0)
          (/ (string-to-number (substring color 3 5) 16) 255.0)
          (/ (string-to-number (substring color 5 7) 16) 255.0)))
   ((and (stringp color) (color-defined-p color))
    (let ((rgb (color-values color)))
      (when rgb
        (list (/ (nth 0 rgb) 65535.0)
              (/ (nth 1 rgb) 65535.0)
              (/ (nth 2 rgb) 65535.0)))))
   (t nil)))

(defun org-pdf--lang-mode (lang)
  "Return the major mode symbol to fontify LANG with, or nil.
Honours `org-src-lang-modes', including entries whose value is a function
name (e.g. \"C\" -> `c-mode' via a lambda list), then falls back to
\"LANG-mode\"."
  (when (and lang (not (string-empty-p lang)))
    (let* ((key (intern (downcase lang)))
           (mapped (cdr (assq key org-src-lang-modes))))
      (cond
       ((null mapped) (intern (concat (downcase lang) "-mode")))
       ((symbolp mapped)
        (if (fboundp mapped)
            (funcall mapped)
          (intern (concat (symbol-name mapped) "-mode"))))
       (t mapped)))))

(defun org-pdf--fontify-spans (lang value)
  "Fontify code VALUE using LANG's major mode.
Returns a list of lines, each line a list of (TEXT . RGB) spans; consecutive
characters sharing a colour are merged into one span.  When syntax
highlighting is disabled or the language mode is unavailable, every line is a
single black span."
  (let ((mode (and org-pdf-syntax-highlight (org-pdf--lang-mode lang))))
    (if (not (and mode (fboundp mode)))
        (mapcar (lambda (ln) (list (cons ln org-pdf--color-black)))
                (split-string value "\n"))
      (with-temp-buffer
        (insert value)
        (delay-mode-hooks (funcall mode))
        (ignore-errors (font-lock-ensure))
        (let ((lines nil) (spans nil) (text "") (col nil) (started nil)
              (i (point-min)) (n (point-max)))
          (while (< i n)
            (let* ((ch (char-to-string (char-after i)))
                   (c (org-pdf--face-rgb (get-text-property i 'face))))
              (if (string= ch "\n")
                  (progn
                    ;; finish current span and line
                    (when started
                      (push (cons text (or col org-pdf--color-black)) spans))
                    (push (nreverse spans) lines)
                    (setq spans nil text "" col nil started nil))
                (if (and started (not (equal c col)))
                    (progn
                      (push (cons text (or col org-pdf--color-black)) spans)
                      (setq text ch col c))
                  (setq text (concat text ch))
                  (unless started (setq col c started t)))))
            (setq i (1+ i)))
          ;; last line (no trailing newline)
          (when (or started spans)
            (when started
              (push (cons text (or col org-pdf--color-black)) spans))
            (push (nreverse spans) lines))
          (nreverse lines))))))

(defun org-pdf--heading-sizes (level base)
  "Return the font size for headline LEVEL relative to BASE."
  (let ((factor (pcase level
                  (1 1.7) (2 1.45) (3 1.25) (4 1.12) (_ 1.0))))
    (* base factor)))

(defun org-pdf--headline-number (st level)
  "Advance the section counters in ST for a headline at LEVEL.
Returns the number string (e.g. \"2.3\"), or nil when numbering is off."
  (when (org-pdf--layout-number-headings st)
    (let* ((counters (org-pdf--layout-counters st))
           (n (length counters)))
      ;; grow the counter vector if the document nests deeper than seen so far
      (when (> level n)
        (setq counters (vconcat counters (make-vector (- level n) 0)))
        (setf (org-pdf--layout-counters st) counters))
      (aset counters (1- level) (1+ (aref counters (1- level))))
      ;; reset all deeper levels
      (let ((i level))
        (while (< i (length counters))
          (aset counters i 0)
          (setq i (1+ i))))
      (mapconcat (lambda (k) (number-to-string (aref counters (1- k))))
                 (number-sequence 1 level) "."))))

(defun org-pdf--render-headline (st node)
  "Render headline NODE and everything under it into ST."
  (let* ((level (org-element-property :level node))
         (size (org-pdf--heading-sizes level (org-pdf--layout-base-size st)))
         (title (org-pdf--inline-text (org-element-property :title node)))
         (number (org-pdf--headline-number st level))
         (displayed (if number (concat number " " title) title))
         (font (if (<= level 3) (org-pdf--emph-headline-font st level) 'helvetica-bold)))
    (org-pdf--ensure st (* size 2.2))
    (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.7)))
    ;; Record a bookmark at the headline's top before drawing it.  The bookmark
    ;; title carries the section number so it matches the printed heading.
    (push (list level displayed
                (length (org-pdf--layout-pages st))
                (- (org-pdf--layout-page-h st) (org-pdf--layout-y st)))
          (org-pdf--layout-bookmarks st))
    (org-pdf--flow st displayed size font org-pdf--color-black)
    (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.35)))
    ;; Render the headline's body (its section and any nested headlines).  The
    ;; dynamic level lets a contained #+include: nest one level below it.
    (let ((org-pdf--current-level level))
      (dolist (child (org-element-contents node))
        (org-pdf--render st child)))))

(defun org-pdf--emph-headline-font (st _level)
  "Font symbol for a headline at _LEVEL in state ST."
  (if (eq (org-pdf--layout-head-font st) :times) 'times-bold 'helvetica-bold))

(defun org-pdf--image-link-path (link)
  "Return the absolute file name LINK embeds, or nil.
LINK must be a description-less `file:' link whose target names an existing
PNG/JPEG.  Relative paths resolve against `org-pdf--include-directory' (the
directory of the org file being exported), matching how #+include: paths
resolve."
  (when (and (eq (org-element-type link) 'link)
             (equal (org-element-property :type link) "file")
             (null (org-element-contents link)))
    (let* ((raw (org-element-property :path link))
           (path (cond ((file-name-absolute-p raw) (expand-file-name raw))
                       (org-pdf--include-directory
                        (expand-file-name raw org-pdf--include-directory))
                       (t (expand-file-name raw)))))
      (and (org-pdf--image-file-p path) path))))

(defun org-pdf--line-start-p (str)
  "Return non-nil if STR is at the start of a line, indentation aside.
That is, everything after its last newline is whitespace, or STR holds no
text at all.  List items and other contexts indent their content, so a
leading blank run before an image must not make it look inline."
  (and (stringp str)
       (string-match-p "\\(?:\\`\\|[\n\r]\\)[ \t]*\\'" str)))

(defun org-pdf--image-block-p (kids link)
  "Return non-nil if LINK is an image drawn as a block within KIDS.
An image is a block when it occupies its own line: no text follows it on the
same line (its `:post-blank' is 0, so any following text is separated by a
newline) and, unless it is the very first thing in the paragraph, the text
before it ends with a newline or only indentation.  This is what tells

    [[file:fig.png]]
    正文……

apart from the inline form `[[file:fig.png]] 说明', which stays text."
  (when (org-pdf--image-link-path link)
    (let* ((before (cl-loop for c in kids
                            until (eq c link)
                            when (stringp c)
                            collect (substring-no-properties c) into ss
                            finally return (mapconcat #'identity ss "")))
           (next (cadr (memq link kids)))
           (next-str (and (stringp next) (substring-no-properties next))))
      (and (org-pdf--line-start-p before)
           (zerop (or (org-element-property :post-blank link) 0))
           (or (null next)
               (and next-str (string-match-p "\\`[ \t]*\n" next-str)))))))

(defun org-pdf--render-image (st path)
  "Render the image at PATH into ST at the current pen.
The image is scaled to fit the text column, centred in it, and page-broken
first if it does not fit on the current page.  Advances the pen below it."
  (let* ((px (org-pdf--image-pixel-size path))
         (left (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st)))
         (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st)))
         (maxw (- right left))
         (maxh (- (org-pdf--page-bottom st) (org-pdf--layout-margin-top st))))
    (if (null px)
        (display-warning 'org-pdf (format "Could not read image %s; skipping." path) :warning)
      (let* ((w (* (car px) (/ 72.0 org-pdf--image-dpi)))
             (h (* (cdr px) (/ 72.0 org-pdf--image-dpi)))
             (scale (min 1.0 (/ maxw w) (/ maxh h)))
             (w (* w scale))
             (h (* h scale))
             (x (+ left (/ (- maxw w) 2.0)))
             (size (org-pdf--layout-base-size st)))
        ;; A little breathing room above the figure, then the figure itself.
        (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.4)))
        (org-pdf--ensure st (+ h (* size 1.2)))
        (org-pdf--emit st (list :image path x (org-pdf--layout-y st) w h))
        ;; Advance well past the image so a following paragraph's ascent (which
        ;; rises above its baseline) cannot overlap the figure.
        (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) h (* size 1.4)))))))

(defun org-pdf--leading-block-image (node)
  "Return the path of a block image that opens paragraph NODE, or nil.
Used to decide whether a list item's marker stands alone on its line."
  (let ((kids (org-element-contents node)))
    (catch 'found
      (dolist (c kids)
        (unless (and (stringp c) (string-empty-p (string-trim c)))
          (throw 'found (and (not (stringp c))
                             (org-pdf--image-block-p kids c)
                             (org-pdf--image-link-path c))))))))

(defun org-pdf--render-children (st kids size font rgb &optional indent-extra)
  "Render the paragraph children KIDS into ST.
Text runs are flowed at SIZE/FONT/RGB; a link that is a block image (see
`org-pdf--image-block-p') is embedded as a figure instead of drawn as its
link text.  INDENT-EXTRA is forwarded to `org-pdf--flow' (list items)."
  (let ((segs nil))
    (cl-flet ((flush ()
                (when (and segs (not (string-empty-p
                                      (string-trim (mapconcat #'car segs "")))))
                  (org-pdf--flow st segs size font rgb indent-extra)
                  (setf (org-pdf--layout-y st)
                        (+ (org-pdf--layout-y st) (* size 0.5))))
                (setq segs nil)))
      (dolist (child kids)
        (if (and (not (stringp child))
                 (org-pdf--image-block-p kids child))
            (progn (flush)
                   (org-pdf--render-image st (org-pdf--image-link-path child)))
          (setq segs (append segs (org-pdf--inline-segments child)))))
      (flush))))

(defun org-pdf--render-paragraph (st node)
  "Render paragraph NODE into ST.
A block image link inside the paragraph (one that occupies its own line) is
drawn as an embedded figure rather than as its link text."
  (let* ((font (if (eq (org-pdf--layout-head-font st) :times) 'times 'helvetica))
         (size (org-pdf--layout-base-size st)))
    (org-pdf--render-children st (org-element-contents node) size font
                              org-pdf--color-black)))

(defun org-pdf--item-font (st)
  "Return the body font symbol for list items in state ST."
  (if (eq (org-pdf--layout-head-font st) :times) 'times 'helvetica))

(defun org-pdf--render-item-body (st hang node)
  "Render list item NODE's body below its marker line, hanging past HANG.
The first paragraph is flowed so its continuation lines line up at HANG past
the item indent; remaining children are rendered recursively.  A first
paragraph that opens with a block image has the image embedded below the
marker."
  (let ((first t))
    (dolist (child (org-element-contents node))
      (if (and first (eq (org-element-type child) 'paragraph))
          (org-pdf--render-children st (org-element-contents child)
                                    (org-pdf--layout-base-size st)
                                    (org-pdf--item-font st)
                                    org-pdf--color-black hang)
        (org-pdf--render st child))
      (setq first nil))))

(defun org-pdf--render-list (st node)
  "Render item or plain-list NODE into ST, recursing into sub-items.
The bullet starts the item's first line, on which its title (or, for a
description list, its term) sits; the description follows on the next line,
indented past the bullet so it lines up under the title."
  (let* ((type (org-element-type node))
         (size (org-pdf--layout-base-size st)))
    (cond
     ((eq type 'item)
      (let* ((raw-bullet (or (org-element-property :bullet node) "•"))
             (checkbox (org-element-property :checkbox node))
             (tag (org-element-property :tag node))
             (indent-step (* size 1.4))
             (font (org-pdf--item-font st))
             (prefix (cond ((eq checkbox 'on) (concat (string-trim raw-bullet) " [X] "))
                           ((eq checkbox 'off) (concat (string-trim raw-bullet) " [ ] "))
                           (t (concat (string-trim raw-bullet) " ")))))
        ;; Every item shifts its body one step to the right of the list level.
        (setf (org-pdf--layout-indent st) (+ (org-pdf--layout-indent st) indent-step))
        (let* ((hang (org-pdf--string-width st prefix size font))
               ;; The item's own text: a description item's term, else the
               ;; first paragraph's text (embedded newlines preserved).  When
               ;; that paragraph opens with a block image, the marker is drawn
               ;; on its own and the image (plus any following text) follows
               ;; below.
               (para (car (org-element-map node 'paragraph #'identity)))
               (img (and para (not tag) (org-pdf--leading-block-image para)))
               (lead (cond (tag (org-pdf--inline-segments tag))
                           (img nil)
                           (para (org-pdf--inline-segments
                                  (org-element-contents para)))))
               (left (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st)))
               (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st)))
               (maxw (- right left))
               (lh (* size 1.35))
               (lines (org-pdf--wrap-hanging-segments
                       st (cons (cons prefix nil) lead) size font maxw hang))
               (first-line t))
          ;; Lay the marker plus the item's text out as one hanging paragraph:
          ;; the marker opens the first line, later lines indent to HANG.
          (dolist (line lines)
            (org-pdf--ensure st lh)
            (org-pdf--draw-segments st (nth 2 line)
                                    (if first-line left (+ left hang))
                                    (org-pdf--layout-y st) size font
                                    org-pdf--color-black)
            (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) lh))
            (setq first-line nil))
          ;; A description list's definition (the first paragraph) then flows
          ;; below the term, hanging past the marker; an image item draws the
          ;; image (and any text after it) there.  A plain item's text has
          ;; already been drawn as the lead, so only its remaining children are
          ;; rendered.
          (cond
           (tag (org-pdf--render-item-body st hang node))
           (img (org-pdf--render-children st (org-element-contents para) size font
                                          org-pdf--color-black hang)
                (let ((first t))
                  (dolist (child (org-element-contents node))
                    (unless (and first (eq (org-element-type child) 'paragraph))
                      (org-pdf--render st child))
                    (setq first nil))))
           (t (let ((first t))
                (dolist (child (org-element-contents node))
                  (unless (and first (eq (org-element-type child) 'paragraph))
                    (org-pdf--render st child))
                  (setq first nil))))))
        (setf (org-pdf--layout-indent st) (- (org-pdf--layout-indent st) indent-step))))
     (t (dolist (child (org-element-contents node))
          (org-pdf--render st child))))))

(defun org-pdf--wrap-code-spans (st spans size max-width)
  "Split SPANS (a list of (TEXT . RGB)) into visual lines fitting MAX-WIDTH.
ST and SIZE give the metrics.  Returns a list of span lists, one per visual
line; colours are preserved across the break.  A single character wider than
MAX-WIDTH still goes on a line of its own (never dropped)."
  (let ((lines nil) (cur nil) (curw 0.0))
    (dolist (span spans)
      (let* ((txt (car span))
             (rgb (cdr span))
             (len (length txt)))
        ;; accumulate character by character, breaking when the line is full
        (let ((i 0) (run ""))
          (cl-flet ((emit-run ()
                      (when (> (length run) 0)
                        (push (cons run rgb) cur)
                        (setq run ""))))
            (while (< i len)
              (let* ((ch (substring txt i (1+ i)))
                     (cw (org-pdf--string-width st ch size 'courier)))
                (when (and (> curw 0) (> (+ curw cw) max-width))
                  ;; flush the current run and line, start a new visual line
                  (emit-run)
                  (push (nreverse cur) lines)
                  (setq cur nil curw 0.0))
                (setq run (concat run ch)
                      curw (+ curw cw)))
              (setq i (1+ i)))
            (emit-run)))))
    (if cur
        (push (nreverse cur) lines)
      ;; a line was just flushed on a break: ensure at least one (possibly empty)
      (unless lines (push nil lines)))
    (nreverse lines)))

(defconst org-pdf--ref-regexp "(\\(ref:[^)[:space:]]+\\))"
  "Regexp matching a \"(ref:NAME)\" annotation in a code block.
Group 1 is the anchor name the matching \"[[(ref:NAME)][...]]\" links open.")
  ;; `:dest' ops record group 1, i.e. the bare \"ref:NAME\" anchor.

(defun org-pdf--split-refs (text)
  "Split TEXT into a list of (TEXT . IS-REF) pieces at ref annotations."
  (let ((out nil) (pos 0))
    (while (string-match org-pdf--ref-regexp text pos)
      (when (> (match-beginning 0) pos)
        (push (cons (substring text pos (match-beginning 0)) nil) out))
      (push (cons (match-string 0 text) t) out)      (setq pos (match-end 0)))
    (when (< pos (length text))
      (push (cons (substring text pos) nil) out))
    (nreverse out)))

(defun org-pdf--src-hides-refs-p (node)
  "Return non-nil if src block NODE carries the -r switch."
  (let ((switches (org-element-property :switches node)))
    (and switches (string-match-p "\\(?:\\`\\| \\)-r\\(?: \\|\\'\\)" switches))))

(defun org-pdf--render-src (st node)
  "Render a src/example/verse block NODE, monospaced on a tinted background.
Src blocks are syntax-highlighted (see `org-pdf-syntax-highlight'); lines
wider than the text column are wrapped (never clipped).  A src block with
the -r switch hides its \"(ref:NAME)\" annotations, but still records their
positions so coderef links keep jumping to the right line."
  (let* ((size (* (org-pdf--layout-base-size st) 0.92))
         (lang (org-element-property :language node))
         (value (or (org-element-property :value node) ""))
         (hide-refs (org-pdf--src-hides-refs-p node))
         (left (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st) (* size 0.5)))
         (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st)))
         (maxw (- right left))
         (lh (* size 1.3))
         (lines (org-pdf--fontify-spans lang value)))
    (setf (org-pdf--layout-indent st) (+ (org-pdf--layout-indent st) (* size 1.0)))
    (dolist (spans lines)
      ;; Wrap each logical line into as many visual lines as it needs.
      (dolist (vline (org-pdf--wrap-code-spans st spans size maxw))
        (org-pdf--ensure st lh)
        (let ((y (org-pdf--layout-y st))
              (x left))
          (org-pdf--emit st (list :rect (- left (* size 0.4)) (- y (* size 0.9))
                                  (+ (- right left) (* size 0.8)) lh
                                  org-pdf--color-code-bg))
          (dolist (span vline)
            (dolist (piece (org-pdf--split-refs (car span)))
              (let* ((txt (car piece))
                     (is-ref (cdr piece))
                     (w (org-pdf--string-width st txt size 'courier)))
                (if is-ref
                    (progn
                      ;; Record the destination so "[[(ref:NAME)][...]]" can
                      ;; jump here; the label itself is only drawn unless the
                      ;; block's -r switch hides it.
                      (let ((name (substring txt 1 -1)))
                        (unless hide-refs
                          (org-pdf--draw-line-of-text st txt x y size 'courier
                                                      org-pdf--color-link)
                          (setq x (+ x w)))
                        (org-pdf--emit st (list :dest name x y))))
                  (org-pdf--draw-line-of-text st txt x y size 'courier (cdr span))
                  (setq x (+ x w)))))))
        (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) lh))))
    (setf (org-pdf--layout-indent st) (- (org-pdf--layout-indent st) (* size 1.0)))
    (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.5)))))

(defun org-pdf--render-quote (st node)
  "Render a quote/center block NODE into ST with an indent and a left rule."
  (let ((size (org-pdf--layout-base-size st)))
    (setf (org-pdf--layout-indent st) (+ (org-pdf--layout-indent st) (* size 1.2)))
    (let ((x (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st) (- (* size 0.8))))
          (y0 (org-pdf--layout-y st)))
      (dolist (child (org-element-contents node))
        (org-pdf--render st child))
      (org-pdf--emit st (list :line x y0 x (- (org-pdf--layout-y st) (* size 0.4)) 2
                              org-pdf--color-rule)))
    (setf (org-pdf--layout-indent st) (- (org-pdf--layout-indent st) (* size 1.2)))))

(defun org-pdf--cell-text (cell)
  "Return the plain text of a table CELL element."
  (org-pdf--inline-text (org-element-contents cell)))

(defun org-pdf--render-table (st node)
  "Render a table NODE into ST as a grid with wrapping cells."
  (let* ((size (* (org-pdf--layout-base-size st) 0.92))
         (rows (org-element-map node 'table-row
                 (lambda (r) (org-element-map r 'table-cell #'org-pdf--cell-text))))
         (ncols (apply #'max (mapcar #'length rows)))
         (left (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st)))
         (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st)))
         (total (- right left))
         (pad (* size 0.4))
         ;; distribute: measure natural widths, then clamp to table width
         (colw (make-vector ncols (/ total ncols))))
    ;; naive natural width per column, then normalise
    (dotimes (c ncols)
      (let ((mx 0.0))
        (dolist (row rows)
          (when (> (length row) c)
            (setq mx (max mx (org-pdf--string-width st (nth c row) size 'helvetica)))))
        (aset colw c (+ (* 2 pad) mx))))
    (let ((sum (cl-reduce #'+ colw)))
      (when (> sum total)
        (dotimes (c ncols) (aset colw c (* (aref colw c) (/ total sum))))))
    (let ((lh (* size 1.4)))
      (dolist (row rows)
        ;; compute wrapped height of the tallest cell in this row
        (let ((cell-lines (mapcar (lambda (c)
                                    (org-pdf--wrap st (or (nth c row) "")
                                                   size 'helvetica (- (aref colw c) (* 2 pad))))
                                  (number-sequence 0 (1- ncols))))
              (rowh 0))
          (dolist (cl cell-lines) (setq rowh (max rowh (* (length cl) lh))))
          (org-pdf--ensure st rowh)
          (let ((y0 (org-pdf--layout-y st))
                (x left))
            ;; vertical column rules
            (dotimes (c ncols)
              (org-pdf--emit st (list :line x y0 x (+ y0 rowh) 0.5 org-pdf--color-rule))
              (setq x (+ x (aref colw c))))
            (org-pdf--emit st (list :line x y0 x (+ y0 rowh) 0.5 org-pdf--color-rule))
            (org-pdf--emit st (list :line left y0 (+ left (cl-reduce #'+ colw)) y0 0.5 org-pdf--color-rule))
            ;; cell text
            (setq x left)
            (dotimes (c ncols)
              (let ((yy (+ y0 (* size 0.95))))
                (dolist (pair (nth c cell-lines))
                  (org-pdf--draw-line-of-text st (car pair) (+ x pad) yy size 'helvetica org-pdf--color-black)
                  (setq yy (+ yy lh))))
              (setq x (+ x (aref colw c))))
            (setf (org-pdf--layout-y st) (+ y0 rowh))
            (org-pdf--emit st (list :line left (org-pdf--layout-y st)
                                    (+ left (cl-reduce #'+ colw)) (org-pdf--layout-y st)
                                    0.5 org-pdf--color-rule))))
        )
      (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.6))))))

(defun org-pdf--render-hr (st _node)
  "Render a horizontal rule into ST."
  (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* (org-pdf--layout-base-size st) 0.6)))
  (let ((left (+ (org-pdf--layout-margin-left st) (org-pdf--layout-indent st)))
        (right (- (org-pdf--layout-page-w st) (org-pdf--layout-margin-right st))))
    (org-pdf--emit st (list :line left (org-pdf--layout-y st) right (org-pdf--layout-y st)
                            0.75 org-pdf--color-rule)))
  (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* (org-pdf--layout-base-size st) 0.6))))

(defun org-pdf--render (st node)
  "Render a single org-element NODE into state ST."
  (pcase (org-element-type node)
    ('headline (org-pdf--render-headline st node))
    ('paragraph (org-pdf--render-paragraph st node))
    ('plain-list (org-pdf--render-list st node))
    ('item (org-pdf--render-list st node))
    ((or 'src-block 'example-block 'export-block 'fixed-width) (org-pdf--render-src st node))
    ((or 'quote-block 'center-block 'verse-block) (org-pdf--render-quote st node))
    ('table (org-pdf--render-table st node))
    ('horizontal-rule (org-pdf--render-hr st node))
    ;; A property drawer holds metadata (EXPORT_FILE_NAME, CUSTOM_ID, ...),
    ;; not document text -- drawing it would leak ":KEY: value" into the PDF.
    ('property-drawer nil)
    ('keyword (if (equal (downcase (or (org-element-property :key node) "")) "include")
                  (org-pdf--render-include st node)
                (org-pdf--render-keyword st node)))
    ;; sections and any other containers: recurse into all children
    (_ (dolist (child (org-element-contents node))
         (org-pdf--render st child)))))

(defun org-pdf--render-keyword (st node)
  "Render a keyword NODE (only titles/captions are shown) into ST.
Inside an #+include: expansion the document-level title block is suppressed."
  (let ((key (downcase (or (org-element-property :key node) "")))
        (value (org-element-property :value node)))
    (when (and (not (and org-pdf--in-include
                         (member key '("title" "subtitle" "author" "date"))))
               (member key '("title" "subtitle" "author" "date" "caption")))
      (let* ((size (if (member key '("title")) (* (org-pdf--layout-base-size st) 1.9)
                     (* (org-pdf--layout-base-size st) 1.05)))
             (font (if (equal key "title") 'helvetica-bold 'helvetica))
             (rgb (if (equal key "title") org-pdf--color-black org-pdf--color-gray)))
        (org-pdf--ensure st (* size 1.6))
        (org-pdf--flow st (string-trim value) size font rgb)
        (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.4)))))))

;;;; #+include: expansion -------------------------------------------------

(defun org-pdf--parse-include-args (value)
  "Parse the argument string VALUE of an #+include keyword.
Returns a plist with :file, :lines (a (START . END) cons or nil), :minlevel
\(an integer or nil) and :src (a language string or nil)."
  (let ((rest value) (res nil))
    ;; leading double-quoted file name
    (when (string-match "\\`[ \t]*\"\\([^\"]*\\)\"" rest)
      (setq res (list :file (match-string 1 rest))
            rest (substring rest (match-end 0))))
    (while (string-match "[^ \t]" rest)
      (setq rest (substring rest (match-beginning 0)))
      (cond
       ;; :lines N-M  or  :lines N
       ((string-match "\\`\\(:lines?\\)[ \t]+\"?\\([0-9]+\\)\\(-\\([0-9]+\\)\\)?\"?" rest)
        (setq res (plist-put res :lines
                             (cons (string-to-number (match-string 2 rest))
                                   (and (match-string 4 rest)
                                        (string-to-number (match-string 4 rest)))))
              rest (substring rest (match-end 0))))
       ;; :minlevel N
       ((string-match "\\`\\(:minlevel\\)[ \t]+\\([0-9]+\\)" rest)
        (setq res (plist-put res :minlevel (string-to-number (match-string 2 rest)))
              rest (substring rest (match-end 0))))
       ;; src LANG
       ((string-match "\\`src[ \t]+\\([A-Za-z0-9_+.-]+\\)" rest)
        (setq res (plist-put res :src (match-string 1 rest))
              rest (substring rest (match-end 0))))
       ;; anything else: skip one whitespace-delimited token
       ((string-match "\\`\\(:[A-Za-z0-9_+-]+\\|[^ \t]+\\)" rest)
        (setq rest (substring rest (match-end 0))))))
    res))

(defun org-pdf--slice-lines (text lines)
  "Return LINES (a (START . END) cons) of TEXT as a string, 1-based inclusive."
  (if (null lines)
      text
    (let ((ls (split-string text "\n"))
          (start (car lines))
          (end (or (cdr lines) (length (split-string text "\n")))))
      (string-join (cl-subseq ls (max 0 (1- start)) (min (length ls) end)) "\n"))))

(defun org-pdf--render-include (st node)
  "Expand an #+include: keyword NODE into ST.
Supports plain includes, :lines N[-M], :minlevel N and the src LANG form,
with nested includes resolved relative to the included file."
  (let* ((value (or (org-element-property :value node) ""))
         (args (org-pdf--parse-include-args value))
         (file (plist-get args :file)))
    (when (and file org-pdf--include-directory)
      (let ((path (expand-file-name file org-pdf--include-directory)))
        (cond
         ((not (file-readable-p path))
          (message "org-pdf: include file not found: %s" path))
         ((member path org-pdf--include-stack)
          (message "org-pdf: skipping recursive include of %s" path))
         (t
          (let ((org-pdf--include-stack (cons path org-pdf--include-stack))
                (next-dir (file-name-directory path))
                (text (org-pdf--slice-lines
                       (with-temp-buffer (insert-file-contents path) (buffer-string))
                       (plist-get args :lines))))
            ;; src LANG form: wrap the file content in a src block.
            (when (plist-get args :src)
              (setq text (concat "#+begin_src " (plist-get args :src) "\n"
                                 text "\n#+end_src\n")))
            (let ((org-pdf--include-directory next-dir))
              (with-temp-buffer
                (delay-mode-hooks (org-mode))
                (insert text)
                (let* ((tree (org-element-parse-buffer))
                       ;; By default an included file nests one level below the
                       ;; section that contains the include keyword; an explicit
                       ;; :minlevel overrides that.
                       (target (or (plist-get args :minlevel)
                                   (1+ org-pdf--current-level)))
                       (shallowest
                        (let ((m nil))
                          (org-element-map tree 'headline
                            (lambda (h)
                              (let ((lv (org-element-property :level h)))
                                (when (or (null m) (< lv m)) (setq m lv)))))
                          (or m target)))
                       (delta (- target shallowest)))
                  (unless (and (zerop delta) (null (plist-get args :minlevel)))
                    (org-element-map tree 'headline
                      (lambda (h)
                        (org-element-put-property
                         h :level (max 1 (+ delta (org-element-property :level h)))))))
                  (let ((org-pdf--in-include t))
                    (dolist (child (org-element-contents tree))
                      (when (memq (org-element-type child) '(section headline keyword))
                        (org-pdf--render st child))))))))))))))

;;;; Driver --------------------------------------------------------------

(defun org-pdf--render-tree (st tree &optional document-title)
  "Walk the top-level org TREE and populate ST's pages.
When DOCUMENT-TITLE is non-nil, render it as the document title and treat the
sole top-level headline as the document root: its body is emitted at the top
level and its children keep their (already normalised) levels, so the first
child is section 1."
  (if document-title
      (progn
        (org-pdf--render-document-title st document-title)
        (let ((root (cl-find-if (lambda (n) (eq (org-element-type n) 'headline))
                                (org-element-contents tree))))
          (dolist (child (org-element-contents tree))
            (unless (eq child root)
              (org-pdf--render st child)))
          (when root
            (dolist (child (org-element-contents root))
              (org-pdf--render st child)))))
    (dolist (child (org-element-contents tree))
      (org-pdf--render st child))))

(defun org-pdf--render-document-title (st title)
  "Render TITLE as the document title in state ST.
Uses the same style as a `#+title:' keyword: 1.9x base size, bold, black."
  (unless (string-empty-p (string-trim title))
    (let ((size (* (org-pdf--layout-base-size st) 1.9)))
      (org-pdf--ensure st (* size 1.6))
      (org-pdf--flow st (string-trim title) size 'helvetica-bold org-pdf--color-black)
      (setf (org-pdf--layout-y st) (+ (org-pdf--layout-y st) (* size 0.4))))))

(cl-defun org-pdf--build-layout (tree &key page-size base-size head-font cjk-font
                                      (number-headings t) document-title)
  "Return the finished layout state for org TREE.
PAGE-SIZE is a symbol or cons; BASE-SIZE is the body font size in points;
HEAD-FONT is :helvetica or :times; CJK-FONT is a parsed font plist or nil;
NUMBER-HEADINGS non-nil prefixes headlines with hierarchical numbers;
DOCUMENT-TITLE, when non-nil, is rendered as the document title and the sole
top-level headline is treated as the root (see `org-pdf--render-tree')."
  (let* ((dim (org-pdf-page-dimensions page-size))
         (st (org-pdf--make-layout
              :page-w (car dim) :page-h (cdr dim)
              :margin-left 56.7 :margin-right 56.7 :margin-top 56.7 :margin-bottom 56.7
              :base-size base-size :head-font head-font :cjk-font cjk-font
              :used-cids (make-hash-table :test #'eql)
              :pages nil :cur nil :indent 0
              :number-headings number-headings
              :counters (make-vector 0 0))))
    (org-pdf--new-page st)
    (org-pdf--render-tree st tree document-title)
    ;; flush the last page
    (when (org-pdf--layout-cur st)
      (push (nreverse (org-pdf--layout-cur st)) (org-pdf--layout-pages st)))
    (setf (org-pdf--layout-pages st) (nreverse (org-pdf--layout-pages st)))
    (setf (org-pdf--layout-bookmarks st) (nreverse (org-pdf--layout-bookmarks st)))
    st))

(provide 'org-pdf-layout)
;;; org-pdf-layout.el ends here
