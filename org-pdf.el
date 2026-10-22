;;; org-pdf.el --- Export Org documents to PDF with no external tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: org-pdf contributors
;; Version: 0.1.0
;; Keywords: wp, pdf, org, hypermedia
;; Package-Requires: ((emacs "30.1"))
;; URL: https://example.org/org-pdf

;;; Commentary:

;; org-pdf exports an Org document to PDF using nothing but Emacs itself.
;; There is no LaTeX, no pandoc, no headless browser, and no external
;; program of any kind: the PDF is written byte by byte from Elisp.
;;
;; Latin text is set in the PDF "base 14" fonts (Helvetica/Times/Courier),
;; which every reader provides, so nothing has to be embedded for a
;; Latin-only document.  CJK text is embedded from a TrueType font found on
;; the system (see `org-pdf-cjk-font-file'), subset to just the glyphs used.
;;
;; Usage:
;;   M-x org-pdf-export           ; export current buffer
;;   M-x org-pdf-export-to-file   ; choose the output path
;;
;; From the command line:
;;   emacs --batch -l org-pdf.el --eval '(org-pdf-export-file "in.org" "out.pdf")'

;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'org-pdf-pdf)
(require 'org-pdf-font)
(require 'org-pdf-layout)

(defgroup org-pdf nil
  "Export Org documents to PDF without external tools."
  :group 'org-export
  :prefix "org-pdf-")

(defcustom org-pdf-page-size 'a4
  "Page size for exported PDFs.
A symbol from `org-pdf-page-sizes' (see `org-pdf-page-dimensions') or a cons
\(WIDTH . HEIGHT) in PDF points."
  :type '(choice (symbol :tag "Named" a4 letter a3 legal)
                 (cons (number :tag "Width") (number :tag "Height")))
  :group 'org-pdf)

(defcustom org-pdf-base-font-size 11.0
  "Body font size in points."
  :type 'number
  :group 'org-pdf)

(defcustom org-pdf-head-font 'helvetica
  "Font family for body text: `helvetica' or `times'."
  :type '(choice (const :tag "Helvetica (sans)" helvetica)
                 (const :tag "Times (serif)" times))
  :group 'org-pdf)

(defcustom org-pdf-cjk-font-file nil
  "Path to a TrueType/OpenType font used for CJK text.
If nil, org-pdf searches a list of common system fonts.  The file may be a
.ttf/.otf, or a .ttc/.otc collection (the first face is used)."
  :type '(choice (const :tag "Auto-detect" nil) file)
  :group 'org-pdf)

(defcustom org-pdf-open-after-export t
  "Whether to open the produced PDF after export."
  :type 'boolean
  :group 'org-pdf)

(defcustom org-pdf-number-headings t
  "Whether to number headlines with hierarchical numbers (1, 1.1, 1.1.2...).
Headline numbers also appear in the PDF outline (bookmarks)."
  :type 'boolean
  :group 'org-pdf)

;;;; Font discovery ------------------------------------------------------

(defconst org-pdf--cjk-font-candidates
  '(;; macOS -- TrueType-outline fonts first (glyf-based subsetting only)
    "/System/Library/Fonts/Supplemental/Songti.ttc"
    "/Library/Fonts/Arial Unicode.ttf"
    "/System/Library/Fonts/STHeiti Light.ttc"
    "/System/Library/Fonts/STHeiti Medium.ttc"
    "/System/Library/Fonts/Supplemental/Kai.ttf"
    ;; Windows
    "C:/Windows/Fonts/simsun.ttc"
    "C:/Windows/Fonts/simhei.ttf"
    "C:/Windows/Fonts/msyh.ttc"
    "C:/Windows/Fonts/msjh.ttc"
    ;; Linux -- Debian/Ubuntu, Fedora, openSUSE
    "/usr/share/fonts/truetype/arphic/uming.ttc"
    "/usr/share/fonts/truetype/arphic/ukai.ttc"
    "/usr/share/fonts/truetype/wqy/wqy-zenhei.ttc"
    "/usr/share/fonts/truetype/wqy/wqy-microhei.ttc"
    "/usr/share/fonts/wqy-zenhei/wqy-zenhei.ttc"
    "/usr/share/fonts/wqy-microhei/wqy-microhei.ttc"
    "/usr/share/fonts/chinese/TrueType/uming.ttf"
    "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc"
    ;; 银河麒麟 / 中标麒麟 -- 自带中文字体多在此目录下
    "/usr/share/fonts/kylin/simsun.ttc"
    "/usr/share/fonts/kylin/simhei.ttf"
    "/usr/share/fonts/truetype/simsun.ttc"
    "/usr/share/fonts/truetype/simhei.ttf")
  "System fonts searched, in order, when `org-pdf-cjk-font-file' is nil.
Only TrueType-outline (glyf-based) fonts can be subset by org-pdf; CFF/OTF
fonts (e.g. Noto CJK, PingFang, 方正兰亭) are skipped automatically at parse
time.  When none of these paths yields a usable font, the system font
directories are scanned recursively -- see `org-pdf--scan-cjk-font'.")

(defconst org-pdf--font-search-dirs
  (list "/usr/share/fonts"
        "/usr/local/share/fonts"
        (expand-file-name "~/.local/share/fonts")
        (expand-file-name "~/.fonts"))
  "Directories scanned recursively for a CJK font when the candidates miss.")

(defconst org-pdf--cjk-font-name-re
  (regexp-opt
   '("cjk" "noto" "wqy" "wenquanyi" "zenhei" "microhei" "uming" "ukai"
     "arphic" "simsun" "simhei" "msyh" "msjh" "songti" "droidsansfallback"
     "sourcehan" "source-han" "kylin" "zf" "song" "hei" "kai" "ming"
     "fang" "yuan" "黑" "宋" "楷" "仿" "雅" "圆")
   t)
  "Regexp matching font filenames likely to carry Han glyphs.
Used only to try the most promising files first during the directory scan.")

(defun org-pdf--font-has-cjk-p (font)
  "Return non-nil if parsed FONT's cmap covers a common Han character.
FONT is a font plist as returned by `org-pdf--read-font', or nil."
  (let ((cm (and font (plist-get font :cmap))))
    (and cm (or (gethash #x4E2D cm)      ; 中
                (gethash #x6587 cm)      ; 文
                (gethash #x56FD cm)))))  ; 国

(defun org-pdf--usable-cjk-font-p (path)
  "Return non-nil if PATH parses as glyf-based TrueType with Han coverage.
This is what actually guarantees the embedded font can be subset; a readable
but CFF/OTTO font would parse to nil and silently drop all CJK text."
  (and (file-readable-p path)
       (org-pdf--font-has-cjk-p (org-pdf--read-font path))))

(defun org-pdf--scan-cjk-font ()
  "Scan the system font directories for a usable embedded CJK font.
Returns the path of the first file that parses as glyf-based TrueType and
whose cmap covers Han characters, or nil.  Files whose names suggest CJK are
tried first so the common case stays fast."
  (let ((named nil) (other nil))
    (dolist (dir org-pdf--font-search-dirs)
      (when (file-directory-p dir)
        (dolist (f (ignore-errors
                     (directory-files-recursively
                      dir "\\.\\(?:ttf\\|ttc\\|otc\\|otf\\)\\'" nil nil)))
          (if (string-match-p org-pdf--cjk-font-name-re f)
              (push f named)
            (push f other)))))
    (or (cl-find-if #'org-pdf--usable-cjk-font-p (nreverse named))
        (cl-find-if #'org-pdf--usable-cjk-font-p (nreverse other)))))

(defun org-pdf--find-cjk-font ()
  "Return the path of a usable CJK font, or nil.
An explicit `org-pdf-cjk-font-file' wins.  Otherwise the built-in candidate
list is tried, then the system font directories are scanned; only fonts that
parse as glyf-based TrueType with Han coverage are accepted."
  (or (and org-pdf-cjk-font-file
           (file-readable-p org-pdf-cjk-font-file)
           org-pdf-cjk-font-file)
      (cl-find-if #'org-pdf--usable-cjk-font-p org-pdf--cjk-font-candidates)
      (org-pdf--scan-cjk-font)))

(defun org-pdf--read-font (path)
  "Read and parse the TrueType font at PATH, or return nil."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (org-pdf--parse-font (buffer-string)))))

;;;; Resources and fonts -------------------------------------------------

(defconst org-pdf--base14-names
  '((helvetica            . "Helvetica")
    (helvetica-bold       . "Helvetica-Bold")
    (helvetica-oblique    . "Helvetica-Oblique")
    (helvetica-bold-oblique . "Helvetica-BoldOblique")
    (times                . "Times-Roman")
    (times-bold           . "Times-Bold")
    (times-italic         . "Times-Italic")
    (times-bold-italic    . "Times-BoldItalic")
    (courier              . "Courier")
    (courier-bold         . "Courier-Bold")
    (courier-oblique      . "Courier-Oblique")
    (courier-bold-oblique . "Courier-BoldOblique"))
  "Map of org-pdf font symbols to PDF base-14 font names.")

(defun org-pdf--build-font-resources (w used-fonts cjk-font cids used-codes)
  "Add font objects to writer W.
USED-FONTS is a list of base-14 symbols actually used.  CJK-FONT is a parsed
font plist or nil; CIDS is the list of original glyph ids needed; USED-CODES
is a hash of code points (for the ToUnicode CMap).  Returns
\(FONT-RESOURCES-ALIST . CJK-GID-MAP) where the alist maps font symbol to the
PDF resource name, and CJK-GID-MAP maps original gid to subset gid.
The CJK font's ToUnicode CMap is attached inside `org-pdf--add-truetype'."
  (let ((alist nil) (n 0) (cjk-gid-map nil))
    (dolist (sym used-fonts)
      (let* ((name (format "F%d" n))
             (base (cdr (assq sym org-pdf--base14-names)))
             (obj (org-pdf--w-add
                   w
                   (format (concat "<< /Type /Font /Subtype /Type1 /BaseFont /%s"
                                   " /Encoding /WinAnsiEncoding >>")
                           base))))
        (push (cons sym (cons name obj)) alist)
        (setq n (1+ n))))
    (when (and cjk-font cids)
      (let* ((res (org-pdf--add-truetype w cjk-font cids used-codes))
             (obj (car res))
             (gmap (cdr res))
             (name (format "F%d" n)))
        (push (cons :cjk (cons name obj)) alist)
        (setq cjk-gid-map gmap)))
    (cons (nreverse alist) cjk-gid-map)))

;;;; Content-stream writing ---------------------------------------------

(defun org-pdf--pdf-str (s)
  "Escape S as a PDF literal string body for a base-14 font."
  (let ((out (list)) (i 0) (n (length s)))
    (while (< i n)
      (let ((c (aref s i)))
        (push (cond ((= c ?\\) "\\\\")
                    ((= c ?\() "\\(")
                    ((= c ?\)) "\\)")
                    ((or (< c 32) (>= c 127)) (format "\\%03o" (logand c 255)))
                    (t (char-to-string c)))
              out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun org-pdf--run-class (ch)
  "Classify CH: `base' for base-14 text, `cjk' for everything else."
  (let ((c (aref ch 0)))
    (if (and (org-pdf--base14-p c) (<= c 255)) 'base 'cjk)))

(defun org-pdf--emit-text-op (buf x y str font size rgb
                                  cjk-font cjk-res-name gid-map font-alist)
  "Append a text draw to BUF at baseline (X,Y).
STR is the raw text; FONT is a base-14 symbol; SIZE is in points; RGB is a
colour list.  CJK-FONT / CJK-RES-NAME / GID-MAP describe the embedded font;
FONT-ALIST maps font symbols to resource names.  X and Y are already in PDF
coordinates (bottom-left origin)."
  (let ((cm (and cjk-font (plist-get cjk-font :cmap))))
    (with-current-buffer buf
      (insert "BT\n")
      (insert (format "%s %s %s rg\n"
                      (org-pdf--num (nth 0 rgb))
                      (org-pdf--num (nth 1 rgb))
                      (org-pdf--num (nth 2 rgb))))
      (insert (format "%s %s Td\n" (org-pdf--num x) (org-pdf--num y)))
      (let ((i 0) (n (length str)))
        (while (< i n)
          (let* ((class (org-pdf--run-class (substring str i (1+ i))))
                 (j (1+ i)))
            (while (and (< j n)
                        (eq (org-pdf--run-class (substring str j (1+ j))) class))
              (setq j (1+ j)))
            (let ((run (substring str i j)))
              (if (eq class 'base)
                  (progn
                    (insert (format "/%s %s Tf\n"
                                    (org-pdf--base-res font font-alist)
                                    (org-pdf--num size)))
                    (insert (format "(%s) Tj\n" (org-pdf--pdf-str run))))
                (when cjk-res-name
                  (insert (format "/%s %s Tf\n" cjk-res-name (org-pdf--num size)))
                  (insert (format "<%s> Tj\n"
                                  (org-pdf--encode-cjk run cm gid-map))))))
            (setq i j))))
      (insert "ET\n"))))

(defun org-pdf--base-res (font font-alist)
  "Return the PDF resource name for base-14 FONT symbol in FONT-ALIST.
Falls back to the first available base font if FONT is not present."
  (or (cadr (assq font font-alist))
      (let ((cell (cl-find-if (lambda (c) (string-prefix-p "F" (cadr c)))
                              font-alist)))
        (cadr cell))
      "F0"))

(defun org-pdf--encode-cjk (run cmap gid-map)
  "Encode RUN (a string) as a hex string of 2-byte subset glyph ids."
  (let ((out (list)) (i 0) (n (length run)))
    (while (< i n)
      (let* ((ch (aref run i))
             (old (and cmap (gethash ch cmap)))
             (new (and old gid-map (gethash old gid-map))))
        (push (format "%04x" (or new 0)) out))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun org-pdf--encode-op (buf op cjk cjk-res-name gid-map font-alist page-h
                               &optional image-alist)
  "Append draw OP to BUF, flipping Y for page height PAGE-H.
CJK, CJK-RES-NAME, GID-MAP and FONT-ALIST are passed to the text encoder;
IMAGE-ALIST maps an image key to its resource name for `:image' ops."
  (pcase (car op)
    (:text
     (pcase-let ((`(:text ,x ,y ,str ,font ,size ,rgb) op))
       (org-pdf--emit-text-op buf x (- page-h y) str font size rgb
                              cjk cjk-res-name gid-map font-alist)))
    (:rect
     (pcase-let ((`(:rect ,x ,y ,w ,h ,rgb) op))
       (with-current-buffer buf
         (insert (format "%s %s %s rg\n"
                         (org-pdf--num (nth 0 rgb))
                         (org-pdf--num (nth 1 rgb))
                         (org-pdf--num (nth 2 rgb))))
         (insert (format "%s %s %s %s re f\n"
                         (org-pdf--num x) (org-pdf--num (- page-h (+ y h)))
                         (org-pdf--num w) (org-pdf--num h))))))
    (:image
     (pcase-let ((`(:image ,key ,x ,y ,w ,h) op))
       (let ((name (cadr (assoc key image-alist))))
         (when name
           (with-current-buffer buf
             ;; Place the unit image into the W x H box with the cm operator;
             ;; the box's top-left (x, y) becomes the bottom-left origin.
             (insert "q\n")
             (insert (format "%s 0 0 %s %s %s cm\n"
                             (org-pdf--num w) (org-pdf--num h)
                             (org-pdf--num x) (org-pdf--num (- page-h (+ y h)))))
             (insert (format "/%s Do\n" name))
             (insert "Q\n"))))))
    ((or :link :dest)
     ;; Link points only feed page annotations (see `org-pdf--build-link-annots')
     ;; and ref destinations only name a jump target; neither draws anything.
     nil)
    (:line
     (pcase-let ((`(:line ,x1 ,y1 ,x2 ,y2 ,lw ,rgb) op))
       (with-current-buffer buf
         (insert (format "%s %s %s RG\n"
                         (org-pdf--num (nth 0 rgb))
                         (org-pdf--num (nth 1 rgb))
                         (org-pdf--num (nth 2 rgb))))
         (insert (format "%s w\n" (org-pdf--num lw)))
         (insert (format "%s %s m %s %s l S\n"
                         (org-pdf--num x1) (org-pdf--num (- page-h y1))
                         (org-pdf--num x2) (org-pdf--num (- page-h y2)))))))))

;;;; Main export ---------------------------------------------------------

(defun org-pdf--collect-used-fonts (st)
  "Return a list of base-14 font symbols referenced by the ops in ST.
Always includes at least `helvetica'."
  (let ((seen nil))
    (dolist (page (org-pdf--layout-pages st))
      (dolist (op page)
        (when (eq (car op) :text)
          (push (nth 4 op) seen))))
    (cl-remove-duplicates (cons 'helvetica seen))))

(defun org-pdf--collect-used-images (st)
  "Return the distinct image keys referenced by the ops in ST, in order."
  (let ((seen nil) (order nil))
    (dolist (page (org-pdf--layout-pages st))
      (dolist (op page)
        (when (eq (car op) :image)
          (let ((key (nth 1 op)))
            (unless (member key seen)
              (push key seen)
              (push key order))))))
    (nreverse order)))

(defun org-pdf--page-images (ops image-alist)
  "Return the image resource names used by OPS, in first-use order."
  (let ((seen nil) (names nil))
    (dolist (op ops)
      (when (eq (car op) :image)
        (let* ((cell (assoc (nth 1 op) image-alist))
               (name (cadr cell)))
          (when (and name (not (member name seen)))
            (push name seen)
            (push name names)))))
    (nreverse names)))

(defun org-pdf--svg-renumber (dict map)
  "Rewrite `@N' references in DICT to real object numbers using MAP.
MAP is a hash of bundle id to object number.  Only the dictionary text is
touched; stream bytes are appended after it and may legitimately contain
anything that looks like `@N'."
  (replace-regexp-in-string
   "@\\([0-9]+\\)"
   (lambda (m)
     (let ((n (string-to-number (match-string 1 m))))
       (if-let ((obj (gethash n map)))
           (format "%d 0 R" obj)
         "null")))
   dict t t))

(defun org-pdf--build-svg-objects (w spec)
  "Write the SVG bundle SPEC as objects in writer W; return the root's number.
SPEC comes from `org-pdf--svg-entries'.  Every object is reserved first so
the cross-references can be resolved before any dictionary text is fixed up."
  (let* ((objects (plist-get spec :objects))
         (root (plist-get spec :root))
         (map (make-hash-table :test #'eql))
         (nums nil))
    ;; Reserve one object number per bundle object, in bundle order.
    (dolist (pair objects)
      (let ((num (org-pdf--w-reserve w)))
        (puthash (car pair) num map)
        (push (cons pair num) nums)))
    (dolist (entry (nreverse nums))
      (let* ((pair (car entry))
             (num (cdr entry))
             (body (cdr pair))
             (dict (org-pdf--svg-renumber (car body) map))
             (data (cdr body)))
        (org-pdf--w-put
         w num
         (if data
             ;; The bundle's header already carries /Length, so only the
             ;; stream wrapper is added here.
             (org-pdf--img-concat "<< " dict " >>\nstream\n" data "\nendstream")
           (concat "<< " dict " >>")))))
    (gethash root map)))

(defun org-pdf--build-image-resources (w keys)
  "Add an image XObject per key in KEYS to writer W.
Returns an alist (KEY NAME . OBJ).  Each key is a file name; an image is
embedded once no matter how many pages reference it.  An alpha image adds a
second object for its soft mask, whose number replaces
`org-pdf--smask-token' in the base image's dictionary.  An SVG adds however
many objects its bundle needs."
  (let ((alist nil) (n 0))
    (dolist (key keys)
      (let* ((name (format "Im%d" n))
             (emb (org-pdf--image-embed key)))
        (when emb
          (let* ((entries (plist-get emb :entries))
                 (base (car entries))
                 (mask-entry (cadr entries))
                 (spec (plist-get emb :svg-spec))
                 (base-dict (car base))
                 (base-data (cdr base)))
            (when mask-entry
              ;; The mask is a separate object; number it before the base so
              ;; the base dictionary can point at it.
              (let ((mask-obj (org-pdf--w-add-stream w (car mask-entry) (cdr mask-entry))))
                (setq base-dict (replace-regexp-in-string
                                 (regexp-quote org-pdf--smask-token)
                                 (format "%d 0 R" mask-obj)
                                 base-dict t t))))
            (let ((obj (if spec
                           (org-pdf--build-svg-objects w spec)
                         (org-pdf--w-add-stream w base-dict base-data))))
              (push (cons key (cons name obj)) alist))))
        (setq n (1+ n))))
    (nreverse alist)))

(defun org-pdf--pdf-string-utf16 (s)
  "Encode S as a hex string of UTF-16BE with a BOM, for a PDF /Title."
  (concat "FEFF" (mapconcat #'org-pdf--utf16be s "")))

(defun org-pdf--build-dest-map (page-objs pages page-h)
  "Return a hash mapping ref annotations to (PAGE-OBJECT . (X . Y)).
OPS live in top-left coordinates; the stored Y is flipped to PDF space."
  (let ((map (make-hash-table :test #'equal)))
    (cl-mapc
     (lambda (obj ops)
       (dolist (op ops)
         (when (eq (car op) :dest)
           (pcase-let ((`(:dest ,name ,x ,y) op))
             (puthash name (cons obj (cons x (- page-h y))) map)))))
     page-objs pages)
    map))

(defun org-pdf--build-link-annots (w ops page-h dests)
  "Add a Link annotation object to W for every `:link' op in OPS.
PAGE-H is the page height, used to flip the top-left box to PDF
coordinates.  DESTs maps internal targets to their destination (see
`org-pdf--build-dest-map'); links found there get a /Dest instead of a
/URI action.  Returns the annotation object numbers in op order."
  (let (objs)
    (dolist (op ops)
      (when (eq (car op) :link)
        (pcase-let ((`(:link ,url ,x ,y ,wd ,ht) op))
          (let ((dest (gethash url dests)))
            (push (org-pdf--w-add
                   w
                   (format (concat "<< /Type /Annot /Subtype /Link"
                                   " /Rect [%s %s %s %s]"
                                   " /Border [0 0 0]"
                                   " /H /I"
                                   "%s >>")
                           (org-pdf--num x)
                           (org-pdf--num (- page-h (+ y ht)))
                           (org-pdf--num (+ x wd))
                           (org-pdf--num (- page-h y))
                           (if dest
                               (format " /Dest [%d 0 R /XYZ %s %s null]"
                                       (car dest)
                                       (org-pdf--num (cadr dest))
                                       (org-pdf--num (cddr dest)))
                             (format " /A << /S /URI /URI (%s) >>"
                                     (org-pdf--pdf-str url)))))
                  objs)))))
    (nreverse objs)))

(defun org-pdf--outline-link-siblings (kids parent-obj)
  "Link KIDS (outline node plists) as siblings under PARENT-OBJ.
Sets :parent, :prev, :next on each.  Returns the kid list."
  (let ((prev nil))
    (dolist (k kids)
      (plist-put k :parent parent-obj)
      (plist-put k :prev (and prev (plist-get prev :obj)))
      (when prev (plist-put prev :next (plist-get k :obj)))
      (setq prev k)))
  kids)

(defun org-pdf--outline-descendants (node)
  "Return the number of descendant outline items of NODE."
  (let ((n 0))
    (dolist (k (plist-get node :children))
      (setq n (+ n 1 (org-pdf--outline-descendants k))))
    n))

(defun org-pdf--outline-emit (w node page-objs root)
  "Write outline object for NODE (and its subtree) into writer W.
PAGE-OBJS maps page index to page object number; ROOT is the /Outlines root."
  (let* ((obj (plist-get node :obj))
         (kids (plist-get node :children))
         (page-obj (nth (plist-get node :page) page-objs))
         (parent (or (plist-get node :parent) root)))
    (when kids
      (org-pdf--outline-link-siblings kids obj)
      (dolist (k kids) (org-pdf--outline-emit w k page-objs root)))
    (org-pdf--w-put
     w obj
     (format (concat "<< /Title <%s> /Parent %d 0 R"
                     " /Prev %s /Next %s /First %s /Last %s"
                     " /Dest [%d 0 R /XYZ 0 %s null] /Count %d >>")
             (org-pdf--pdf-string-utf16 (plist-get node :title))
             parent
             (if (plist-get node :prev)
                 (format "%d 0 R" (plist-get node :prev)) "null")
             (if (plist-get node :next)
                 (format "%d 0 R" (plist-get node :next)) "null")
             (if kids (format "%d 0 R" (plist-get (car kids) :obj)) "null")
             (if kids (format "%d 0 R" (plist-get (car (last kids)) :obj)) "null")
             page-obj
             (org-pdf--num (plist-get node :y))
             (org-pdf--outline-descendants node)))))

(defun org-pdf--build-outline (w st page-objs)
  "Build a PDF outline (bookmark tree) from ST's headlines into writer W.
PAGE-OBJS maps page index to page object number.  Returns the /Outlines root
object number, or nil when the document has no headlines."
  (let ((bookmarks (org-pdf--layout-bookmarks st)))
    (when bookmarks
      (let* ((root (org-pdf--w-reserve w))
             (roots nil)
             (stack nil))
        ;; Build the tree: each headline is a child of the nearest preceding
        ;; headline with a smaller level.
        (dolist (b bookmarks)
          (let* ((level (nth 0 b))
                 (node (list :level level :title (nth 1 b)
                             :page (nth 2 b) :y (nth 3 b)
                             :obj (org-pdf--w-reserve w) :children nil)))
            (while (and stack (>= (plist-get (car stack) :level) level))
              (pop stack))
            (if stack
                (plist-put (car stack) :children
                           (append (plist-get (car stack) :children) (list node)))
              (setq roots (append roots (list node))))
            (push node stack)))
        (org-pdf--outline-link-siblings roots root)
        (dolist (r roots) (org-pdf--outline-emit w r page-objs root))
        (org-pdf--w-put
         w root
         (format "<< /Type /Outlines /First %d 0 R /Last %d 0 R /Count %d >>"
                 (plist-get (car roots) :obj)
                 (plist-get (car (last roots)) :obj)
                 (length bookmarks)))
        root))))

(cl-defun org-pdf--write (tree path &key page-size base-size head-font cjk-path
                               (number-headings org-pdf-number-headings)
                               base-dir document-title)
  "Render org TREE and write a PDF to PATH.  Returns PATH.
BASE-DIR is the directory against which #+include: paths resolve; it defaults
to `default-directory'.  DOCUMENT-TITLE, when non-nil, is rendered as the
document title and the sole top-level headline is treated as the root."
  (let* ((org-pdf--include-directory (or base-dir default-directory))
         (cjk (org-pdf--read-font cjk-path))
         (st (org-pdf--build-layout tree
                                    :page-size (or page-size org-pdf-page-size)
                                    :base-size (or base-size org-pdf-base-font-size)
                                    :head-font (or head-font org-pdf-head-font)
                                    :cjk-font cjk
                                    :number-headings number-headings
                                    :document-title document-title))
         (pages (org-pdf--layout-pages st))
         (page-w (org-pdf--layout-page-w st))
         (page-h (org-pdf--layout-page-h st))
         (used-fonts (org-pdf--collect-used-fonts st))
         (w (org-pdf--make-writer))
         (catalog (org-pdf--w-reserve w))
         (pages-obj (org-pdf--w-reserve w))
         ;; If the document needs CJK but no usable font was found, every
         ;; CJK run would be silently dropped -- warn loudly instead.
         (_ (when (and (org-pdf--layout-has-cjk st) (not cjk))
              (display-warning
               'org-pdf
               (format (concat "No usable CJK font found; CJK text will be "
                               "blank in %s.\nSet `org-pdf-cjk-font-file' to a "
                               "TrueType (glyf-outline) font, e.g. wqy-zenhei.ttc "
                               "or simsun.ttc.  CFF/OTF fonts such as Noto CJK "
                               "cannot be embedded by org-pdf.")
                       path)
               :error)))
         ;; Collect the glyph ids that must be embedded for CJK text.
         (cids (when (and cjk (org-pdf--layout-has-cjk st))
                 (let ((cm (plist-get cjk :cmap)) (res nil))
                   (maphash (lambda (code _)
                              (let ((gid (and cm (gethash code cm))))
                                (when gid (push gid res))))
                            (org-pdf--layout-used-cids st))
                   (cl-remove-duplicates res))))
         ;; Fonts first, so the resource dictionary is known.
         (fontres (org-pdf--build-font-resources
                   w used-fonts cjk cids (org-pdf--layout-used-cids st)))
         (font-alist (car fontres))
         (gid-map (cdr fontres))
         (cjk-res (cadr (assq :cjk font-alist)))
         (font-dict (mapconcat (lambda (c) (format "/%s %d 0 R" (cadr c) (cddr c)))
                               font-alist " "))
         ;; Embed every referenced image once, before the pages so their
         ;; resource names can be listed per page.
         (image-alist (org-pdf--build-image-resources
                       w (org-pdf--collect-used-images st)))
         ;; NAME -> object number, for building each page's /XObject dict.
         (image-objs (mapcar (lambda (c) (cons (cadr c) (cddr c))) image-alist)))
    ;; Reserve one Page object per layout page (so /Kids can be written once).
    (let* ((page-objs (mapcar (lambda (_) (org-pdf--w-reserve w)) pages))
           ;; ref annotation name -> destination, for internal links.
           (dests (org-pdf--build-dest-map page-objs pages page-h))
           (outline nil))
      (org-pdf--w-put
       w pages-obj
       (format "<< /Type /Pages /Kids [%s] /Count %d >>"
               (mapconcat (lambda (n) (format "%d 0 R" n)) page-objs " ")
               (length page-objs)))
      ;; Fill in each Page object with a freshly built content stream.
      (cl-mapc
       (lambda (obj ops)
         (let* ((link-objs (org-pdf--build-link-annots w ops page-h dests))
                (content-obj (org-pdf--make-content w ops cjk cjk-res gid-map
                                                    font-alist page-h image-alist))
                (img-names (org-pdf--page-images ops image-alist))
                (xobj-dict (if img-names
                               (format " /XObject << %s >>"
                                       (mapconcat
                                        (lambda (nm)
                                          (format "/%s %d 0 R" nm
                                                  (cdr (assoc nm image-objs))))
                                        img-names " "))
                             ""))
                (annot-dict (if link-objs
                                (format " /Annots [%s]"
                                        (mapconcat (lambda (n) (format "%d 0 R" n))
                                                   link-objs " "))
                              "")))
           (org-pdf--w-put
            w obj
            (format (concat "<< /Type /Page /Parent %d 0 R"
                            " /MediaBox [0 0 %s %s]"
                            " /Resources << /Font << %s >>%s >>"
                            "%s"
                            " /Contents %d 0 R >>")
                    pages-obj (org-pdf--num page-w) (org-pdf--num page-h)
                    font-dict xobj-dict annot-dict content-obj))))
       page-objs pages)
      ;; Bookmarks (outline), now that page object numbers are known.
      (setq outline (org-pdf--build-outline w st page-objs))
      ;; Catalog last; it may point at the (already reserved) Pages object.
      (org-pdf--w-put
       w catalog
       (format "<< /Type /Catalog /Pages %d 0 R%s >>"
               pages-obj
               (if outline
                   (format " /Outlines %d 0 R /PageMode /UseOutlines" outline)
                 ""))))
    (org-pdf--w-write w path)))

(defun org-pdf--make-content (w ops cjk cjk-res-name gid-map font-alist page-h
                                &optional image-alist)
  "Build a content stream for OPS in writer W; return its object number.
IMAGE-ALIST maps image keys to resource names, for `:image' ops."
  (let ((content (generate-new-buffer " *org-pdf-content*")))
    (unwind-protect
        (with-current-buffer content
          (set-buffer-multibyte nil)
          (dolist (op ops)
            (org-pdf--encode-op content op cjk cjk-res-name gid-map
                                font-alist page-h image-alist))
          (org-pdf--w-add-stream w "" (buffer-string)))
      (kill-buffer content))))

;;;; Public commands -----------------------------------------------------

(defun org-pdf--normalize-levels (tree)
  "Shift headline levels in TREE so the shallowest becomes level 1.
A no-op for a normal buffer (whose headlines already start at level 1), but
needed for a subtree export, where the root headline may be level 2 or deeper
\(which would otherwise be numbered \"0.1\")."
  (let ((min nil))
    (org-element-map tree 'headline
      (lambda (h)
        (let ((lv (org-element-property :level h)))
          (when (or (null min) (< lv min)) (setq min lv)))))
    (when (and min (> min 1))
      (let ((delta (- min 1)))
        (org-element-map tree 'headline
          (lambda (h)
            (org-element-put-property
             h :level (- (org-element-property :level h) delta))))))
    tree))

(defun org-pdf--subtree-root (tree)
  "Return the top-level headline of TREE, or nil.
The shallowest headline is chosen; normally there is exactly one under a
narrowed subtree."
  (let ((min nil) (root nil))
    (dolist (n (org-element-contents tree))
      (when (eq (org-element-type n) 'headline)
        (let ((lv (org-element-property :level n)))
          (when (or (null min) (< lv min))
            (setq min lv root n)))))
    root))

(defun org-pdf--parse-subtree ()
  "Parse the narrowed buffer as a subtree-to-document export.
Returns (TREE . TITLE): TREE is the parsed buffer with every headline level
shifted down by one (so the subtree root's children become the top level and
are numbered from 1), and TITLE is the subtree root's title as plain text.
The caller renders TITLE as the document title and skips the root headline."
  (let* ((tree (org-element-parse-buffer))
         (root (org-pdf--subtree-root tree))
         (title (and root (org-pdf--inline-text (org-element-property :title root)))))
    ;; Drop one level so the root's children start at level 1.
    (org-element-map tree 'headline
      (lambda (h)
        (org-element-put-property
         h :level (max 1 (1- (org-element-property :level h))))))
    (cons tree (or title ""))))

(defun org-pdf--parse-current-buffer (&optional subtreep)
  "Parse the current Org buffer, honouring narrowing, and normalise levels.
Always returns a cons (TREE . TITLE).  With SUBTREEP non-nil, TITLE is the
subtree root's title (to be rendered as the document title) and TREE's
headlines are shifted so the root's children are numbered from 1.  Without
SUBTREEP, TITLE is nil and the shallowest headline is normalised to level 1."
  (if subtreep
      (org-pdf--parse-subtree)
    (cons (org-pdf--normalize-levels (org-element-parse-buffer)) nil)))


(defun org-pdf-export-file (org-file pdf-file &optional _arg)
  "Export ORG-FILE to PDF-FILE.  Intended for batch use."
  (interactive "fOrg file: \nFPDF file: ")
  (with-current-buffer (find-file-noselect org-file)
    (org-mode)
    (pcase-let* ((`(,tree . ,doc-title) (org-pdf--parse-current-buffer))
                 (cjk-path (org-pdf--find-cjk-font))
                 (base-dir (file-name-directory (expand-file-name org-file))))
      (org-pdf--write tree pdf-file :cjk-path cjk-path :base-dir base-dir
                      :document-title doc-title)
      (message "Wrote %s" pdf-file)))
  pdf-file)

(defun org-pdf-export (&optional subtreep)
  "Export the current Org buffer to a PDF beside the Org file.
With a prefix argument (SUBTREEP), export only the subtree at point,
naming the file after its :EXPORT_FILE_NAME: property if present, else
after the subtree title."
  (interactive "P")
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
  (org-pdf-export-to-file (org-pdf--default-output-file subtreep) subtreep))

(defun org-pdf-export-to-file (pdf-file &optional subtreep)
  "Export the current Org buffer to PDF-FILE.
With SUBTREEP non-nil, export only the subtree at point.
Interactively, prompt for the destination writable file; a prefix argument
exports the current subtree."
  (interactive "FPDF file: \nP")
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
  (when (and subtreep (not (org-at-heading-p)))
    (org-back-to-heading t))
  (let* ((cjk-path (org-pdf--find-cjk-font))
         (base-dir (and (buffer-file-name) (file-name-directory (buffer-file-name)))))
    (save-restriction
      (when subtreep (org-narrow-to-subtree))
      (pcase-let* ((`(,tree . ,doc-title) (org-pdf--parse-current-buffer subtreep)))
        (org-pdf--write tree pdf-file :cjk-path cjk-path :base-dir base-dir
                        :document-title doc-title)))
    (when (and org-pdf-open-after-export (not (equal system-type 'windows-nt)))
      (ignore-errors (start-process "org-pdf-open" nil
                                    (pcase system-type
                                      ('darwin "open")
                                      ('gnu/linux "xdg-open")
                                      (_ "open"))
                                    pdf-file)))
    (message "Wrote %s" pdf-file)
    pdf-file))

;;;; Org export dispatcher integration (C-c C-e) -------------------------

(require 'ox)

(defun org-pdf-export-to-pdf (&optional _async subtreep _visible-only _body-only)
  "Export the current Org buffer to PDF using the org-pdf engine.
SUBTREEP, when non-nil, restricts export to the subtree at point (as when
`org-export-initial-scope' is subtree or `C-c C-e C-s' is used).  The output
file is named after the subtree title.  Registered as an Org export backend
action, so it is reachable from the dispatcher (\\[org-export-dispatch])."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-pdf: not an Org buffer"))
  (org-pdf-export-to-file (org-pdf--default-output-file subtreep) subtreep))

(defun org-pdf-export-to-pdf-and-open (&optional _async subtreep _visible-only _body-only)
  "Export the current Org buffer to PDF using the org-pdf engine.
SUBTREEP, when non-nil, restricts export to the subtree at point (as when
`org-export-initial-scope' is subtree or `C-c C-e C-s' is used).  The output
file is named after the subtree title.  Registered as an Org export backend
action, so it is reachable from the dispatcher (\\[org-export-dispatch])."
  (interactive)
  (let ((org-pdf-open-after-export t))
                                       (org-pdf-export-to-pdf _async subtreep _visible-only _body-only))
  )

(defun org-pdf--subtree-slug ()
  "Return a filesystem-safe slug of the headline at point, or \"subtree\"."
  (let (title)
    (condition-case nil
        (save-excursion
          (org-back-to-heading t)
          (setq title (nth 4 (org-heading-components))))
      (error nil))
    (let ((slug (and title (replace-regexp-in-string
                            "[ \t/\\\\:*?\"<>|]+" "_" (string-trim title)))))
      (if (and slug (> (length slug) 0)) slug "subtree"))))

(defun org-pdf--default-output-file (subtreep)
  "Return the default output PDF path for the current buffer.
When SUBTREEP is non-nil, honour a headline's :EXPORT_FILE_NAME: property
\(as org does), resolved relative to the buffer's file; otherwise fall back to
\"<buffer base>_<subtree title>.pdf\".  Without SUBTREEP the whole-buffer name
\"<buffer base>.pdf\" is used."
  (let* ((buffer-file (buffer-file-name))
         (base-dir (and buffer-file (file-name-directory buffer-file)))
         (export-name (and subtreep (org-pdf--export-file-name-property))))
    (cond
     ;; 1. Subtree with an explicit :EXPORT_FILE_NAME:
     ((and subtreep export-name base-dir)
      (org-pdf--append-ext (expand-file-name export-name base-dir)))
     ;; 2. Subtree without a property: base + "_" + title
     (subtreep
      (concat (file-name-sans-extension
               (or buffer-file
                   (expand-file-name (make-temp-name "org-pdf-") default-directory)))
              "_" (org-pdf--subtree-slug) ".pdf"))
     ;; 3. Whole buffer
     (t
      (concat (file-name-sans-extension
               (or buffer-file
                   (expand-file-name (make-temp-name "org-pdf-") default-directory)))
              ".pdf")))))

(defun org-pdf--export-file-name-property ()
  "Return the EXPORT_FILE_NAME property of the headline at point, or nil."
  (condition-case nil
      (save-excursion
        (org-back-to-heading t)
        (let ((v (org-entry-get nil "EXPORT_FILE_NAME" t)))
          (and v (not (string-empty-p (string-trim v))) (string-trim v))))
    (error nil)))

(defun org-pdf--append-ext (path)
  "Return PATH with a .pdf extension if it does not already have one."
  (if (string-suffix-p ".pdf" path) path (concat path ".pdf")))

(org-export-define-derived-backend 'org-pdf 'ascii
  :menu-entry
  '(?S "Export to PDF (org-pdf)"
       ((?P "As PDF file" org-pdf-export-to-pdf)
        (?o "As PDF file and open" org-pdf-export-to-pdf-and-open)))
  ;; The backend delegates all real work to org-pdf; transcoders are unused.
  :translate-alist
  '((plain-text . (lambda (_text _backend _info) ""))
    (bold . (lambda (_b _c _i) ""))
    (italic . (lambda (_b _c _i) ""))
    (headline . (lambda (_h _c _i) ""))
    (paragraph . (lambda (_p _c _i) ""))
    (section . (lambda (_s _c _i) ""))))

(defun org-pdf-register-backend ()
  "Add `org-pdf' to `org-export-backends' and register its backend.
Call this once (e.g. from your init file) if `C-c C-e' does not show the
\"Export to PDF (org-pdf)\" entry."
  (interactive)
  (require 'ox)
  (add-to-list 'org-export-backends 'org-pdf)
  (org-export-register-backend (org-export-get-backend 'org-pdf)))

;; Register automatically on load.
(org-pdf-register-backend)

(provide 'org-pdf)
;;; org-pdf.el ends here
