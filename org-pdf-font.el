;;; org-pdf-font.el --- Font metrics and TrueType embedding  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;;; Commentary:

;; Two kinds of fonts are used by org-pdf:
;;
;;  * The PDF "base 14" fonts (Helvetica / Times / Courier families).  These
;;    are never embedded -- every PDF reader ships them -- and their glyph
;;    widths are known constants, hard-coded below.  They cover Latin text.
;;
;;  * An embedded TrueType/OpenType font for CJK (and anything the base-14
;;    fonts cannot encode).  Fonts are parsed and *subset* entirely in Elisp:
;;    we read the table directory, pull advances from `hmtx', the code point
;;    -> glyph id map from `cmap', and outlines from `loca'/`glyf', then
;;    rebuild a small font containing only the glyphs actually used.  The
;;    subset is embedded as a Type0 / CIDFontType2 font with Identity-H
;;    encoding, so content streams address glyphs by 2-byte glyph id.

;;; Code:

(require 'cl-lib)
(require 'org-pdf-pdf)

;;;; Byte helpers ---------------------------------------------------------

(defsubst org-pdf--u8  (b o) (aref b o))
(defsubst org-pdf--u16 (b o) (+ (ash (aref b o) 8) (aref b (1+ o))))
(defsubst org-pdf--s16 (b o) (let ((v (org-pdf--u16 b o))) (if (>= v #x8000) (- v #x10000) v)))
(defsubst org-pdf--u32 (b o) (+ (ash (aref b o) 24) (ash (aref b (1+ o)) 16)
                                (ash (aref b (+ o 2)) 8) (aref b (+ o 3))))
(defun org-pdf--be16 (n) (unibyte-string (logand (ash n -8) #xFF) (logand n #xFF)))
(defun org-pdf--be32 (n)
  (unibyte-string (logand (ash n -24) #xFF) (logand (ash n -16) #xFF)
                  (logand (ash n -8) #xFF) (logand n #xFF)))
(defun org-pdf--pad4 (n) (let ((r (* 4 (ceiling n 4)))) r))

;;;; Base-14 width tables -------------------------------------------------

;; Widths are in 1/1000 em for character codes 32..126, in order -- the
;; standard Adobe values used by every PDF producer.

(defconst org-pdf--helvetica-widths
  [278 278 355 556 556 889 667 191 333 333 389 584 278 333 278 278
   556 556 556 556 556 556 556 556 556 556 278 278 584 584 584 556
   1015 667 667 722 722 667 611 778 722 278 500 667 556 833 722 778
   667 778 722 667 611 722 667 944 667 667 611 278 278 278 469 556
   333 556 556 500 556 556 278 556 556 222 222 500 222 833 556 556
   556 556 333 500 278 556 500 722 500 500 500 334 260 334 584])

(defconst org-pdf--helvetica-bold-widths
  [278 333 474 556 556 889 722 238 333 333 389 584 278 333 278 278
   556 556 556 556 556 556 556 556 556 556 333 333 584 584 584 611
   975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778
   667 778 722 667 611 722 667 944 667 667 611 333 278 333 584 556
   333 556 611 556 611 556 333 611 611 278 278 556 278 889 611 611
   611 611 389 556 333 611 556 778 556 556 500 389 280 389 584])

(defconst org-pdf--times-widths
  [250 333 408 500 500 833 778 180 333 333 500 564 250 333 250 278
   500 500 500 500 500 500 500 500 500 500 278 278 564 564 564 444
   921 722 667 667 722 611 556 722 722 333 389 722 611 889 722 722
   556 722 667 556 611 722 722 944 722 722 611 333 278 333 469 500
   333 444 500 444 500 444 333 500 500 278 278 500 278 778 500 500
   500 500 333 389 278 500 500 722 500 500 444 480 200 480 541])

(defconst org-pdf--times-bold-widths
  [250 333 555 500 500 1000 833 278 333 333 500 570 250 333 250 278
   500 500 500 500 500 500 500 500 500 500 333 333 570 570 570 500
   930 722 667 722 722 667 611 778 778 389 500 778 667 944 722 778
   611 778 722 556 667 722 722 1000 722 722 667 333 278 333 581 500
   333 500 556 444 556 444 333 500 556 278 333 556 278 833 556 500
   556 556 444 389 333 556 500 722 500 500 444 394 220 394 520])

(defun org-pdf-base14-width (font-code code)
  "Return the advance width (1/1000 em) of CODE in base-14 FONT-CODE.
FONT-CODE is a symbol such as `helvetica' or `courier'; CODE is a character
code 0..255.  Covers the bold/oblique/italic variants used by org-pdf."
  (cond
   ((memq font-code '(courier courier-bold courier-oblique courier-bold-oblique)) 600)
   ((>= code 256) 500)
   (t (let ((tbl (pcase font-code
                   ((or 'helvetica 'helvetica-oblique) org-pdf--helvetica-widths)
                   ('helvetica-bold org-pdf--helvetica-bold-widths)
                   ((or 'times 'times-italic) org-pdf--times-widths)
                   ((or 'times-bold 'times-bold-italic) org-pdf--times-bold-widths)
                   (_ org-pdf--helvetica-widths))))
        (if (and (>= code 32) (<= code 126))
            (aref tbl (- code 32))
          500)))))

;;;; Character classification --------------------------------------------

(defun org-pdf-cjk-p (ch)
  "Return non-nil if CH is a wide (CJK) character needing an embedded font."
  (let ((c (cond ((characterp ch) ch) ((stringp ch) (aref ch 0)) (t ch))))
    (or (and (>= c #x1100) (<= c #x11FF))   ; Hangul Jamo
        (and (>= c #x2E80) (<= c #x303F))   ; CJK radicals, punctuation
        (and (>= c #x3040) (<= c #x30FF))   ; Hiragana, Katakana
        (and (>= c #x3130) (<= c #x318F))   ; Hangul compat jamo
        (and (>= c #x3400) (<= c #x4DBF))   ; CJK ext A
        (and (>= c #x4E00) (<= c #x9FFF))   ; CJK unified
        (and (>= c #xA960) (<= c #xA97F))   ; Hangul jamo ext A
        (and (>= c #xAC00) (<= c #xD7AF))   ; Hangul syllables
        (and (>= c #xF900) (<= c #xFAFF))   ; CJK compat ideographs
        (and (>= c #xFE30) (<= c #xFE4F))   ; CJK compat forms
        (and (>= c #xFF00) (<= c #xFFEF))   ; Fullwidth forms
        (and (>= c #x20000) (<= c #x3FFFF)) ; CJK ext B..F
        (and (>= c #xE000) (<= c #xF8FF))))) ; private use (icon fonts etc.)

(defun org-pdf--base14-p (ch)
  "Return non-nil if CH can be encoded in a base-14 font (Latin-1 range)."
  (let ((c (cond ((characterp ch) ch) ((stringp ch) (aref ch 0)) (t ch))))
    (and (>= c 32) (< c 256))))

;;;; TrueType parsing ----------------------------------------------------

;; A parsed font is a plist:
;;   :units :num-glyphs :widths (vector) :cmap (hash code->gid)
;;   :loca (vector offsets) :glyf (offset) :sfnt (full bytes)
;;   :ascent :descent :bbox (xMin yMin xMax yMax) :loca-long

(defun org-pdf--table-dir (bytes num-offset)
  "Parse the sfnt table directory at NUM-OFFSET in BYTES.
Returns an alist (TAG . (OFFSET . LENGTH))."
  (let* ((num-tables (org-pdf--u16 bytes (+ num-offset 4)))
         (res nil))
    (dotimes (i num-tables)
      (let* ((rec (+ num-offset 12 (* i 16)))
             (tag (substring bytes rec (+ rec 4)))
             (off (org-pdf--u32 bytes (+ rec 8)))
             (len (org-pdf--u32 bytes (+ rec 12))))
        (push (cons (intern tag) (cons off len)) res)))
    res))

(defun org-pdf--parse-cmap (bytes off)
  "Parse the cmap table at OFF in BYTES.  Returns hash CODE->GID.
Handles format 4 (BMP) and format 12 (full Unicode) subtables."
  (let* ((num-tables (org-pdf--u16 bytes (+ off 2)))
         (best nil) (best-score 0) (hash (make-hash-table :test #'eql)))
    (dotimes (i num-tables)
      (let* ((rec (+ off 4 (* i 8)))
             (plat (org-pdf--u16 bytes rec))
             (enc (org-pdf--u16 bytes (+ rec 2)))
             (sub (+ off (org-pdf--u32 bytes (+ rec 4))))
             (fmt (org-pdf--u16 bytes sub)))
        (when (memq fmt '(4 12))
          (let ((score (if (= fmt 12)
                           (if (= plat 3) 3 2)
                         1)))
            (when (and (or (and (= plat 3) (memq enc '(1 10)))
                           (= plat 0))
                       (> score best-score))
              (setq best sub best-score score))))))
    (when best
      (pcase (org-pdf--u16 bytes best)
        (4
         (let* ((seg-x2 (org-pdf--u16 bytes (+ best 6)))  ; segCount * 2 (bytes)
                (end-base (+ best 14))
                (start-base (+ end-base seg-x2 2))       ; + reservedPad (2)
                (delta-base (+ start-base seg-x2))
                (range-base (+ delta-base seg-x2)))
           (dotimes (i (ash seg-x2 -1))
             (let* ((end (org-pdf--u16 bytes (+ end-base (* i 2))))
                    (start (org-pdf--u16 bytes (+ start-base (* i 2))))
                    (delta (org-pdf--s16 bytes (+ delta-base (* i 2))))
                    (ro (org-pdf--u16 bytes (+ range-base (* i 2))))
                    (c start))
               ;; The final segment is the sentinel start=end=0xFFFF; skip it.
               (unless (and (= start #xFFFF) (= end #xFFFF))
                 (while (<= c end)
                   (let ((gid (if (= 0 ro)
                                  (logand (+ c delta) #xFFFF)
                                (let* ((gi (+ range-base (* i 2) ro (* 2 (- c start))))
                                       (g (org-pdf--u16 bytes gi)))
                                  (if (= 0 g) 0 (logand (+ g delta) #xFFFF))))))
                     (when (/= 0 gid) (puthash c gid hash)))
                   (setq c (1+ c))))))))
        (12
         (let ((n (org-pdf--u32 bytes (+ best 12))))
           (dotimes (i n)
             (let* ((rec (+ best 16 (* i 12)))
                    (sc (org-pdf--u32 bytes rec))
                    (ec (org-pdf--u32 bytes (+ rec 4)))
                    (sg (org-pdf--u32 bytes (+ rec 8)))
                    (c sc))
               (while (<= c ec)
                 (puthash c (+ sg (- c sc)) hash)
                 (setq c (1+ c)))))))))
    hash))

(defun org-pdf--parse-font (bytes)
  "Parse a TrueType font in BYTES.  Returns a font plist, or nil.
If BYTES is a TTC (TrueType Collection) the first face is used."
  (let ((num-offset 0))
    (when (equal (substring bytes 0 4) "ttcf")
      (setq num-offset (org-pdf--u32 bytes 12)))
    (let* ((dir (org-pdf--table-dir bytes num-offset))
           (get (lambda (tag) (cdr (assoc tag dir))))
           (head (funcall get 'head))
           (hhea (funcall get 'hhea))
           (maxp (funcall get 'maxp))
           (hmtx (funcall get 'hmtx))
           (cmap (funcall get 'cmap))
           (loca (funcall get 'loca))
           (glyf (funcall get 'glyf)))
      (when (and head hhea maxp hmtx cmap loca glyf)
        (let* ((ho (car head))          ; (OFFSET . LENGTH)
               (units (org-pdf--u16 bytes (+ ho 18)))
               (loca-long (> (org-pdf--s16 bytes (+ ho 50)) 0))
               (num-glyphs (org-pdf--u16 bytes (+ (car maxp) 4)))
               (num-hmetrics (org-pdf--u16 bytes (+ (car hhea) 34)))
               (ho2 (car hhea))
               (ascent (org-pdf--s16 bytes (+ ho2 4)))
               (descent (org-pdf--s16 bytes (+ ho2 6)))
               (widths (make-vector num-glyphs 0))
               (last 0)
               (hmo (car hmtx)))
          (dotimes (i num-glyphs)
            (when (< i num-hmetrics)
              (setq last (org-pdf--u16 bytes (+ hmo (* i 4)))))
            (aset widths i last))
          (let* ((lo (car loca))
                 (offs (make-vector (1+ num-glyphs) 0)))
            (dotimes (i (1+ num-glyphs))
              (aset offs i (if loca-long
                               (org-pdf--u32 bytes (+ lo (* i 4)))
                             (* 2 (org-pdf--u16 bytes (+ lo (* i 2)))))))
            (list :units units :num-glyphs num-glyphs :widths widths
                  :cmap (org-pdf--parse-cmap bytes (car cmap))
                  :loca offs :glyf (car glyf) :sfnt bytes
                  :ascent ascent :descent descent
                  :bbox (list (org-pdf--s16 bytes (+ ho 36))
                              (org-pdf--s16 bytes (+ ho 38))
                              (org-pdf--s16 bytes (+ ho 40))
                              (org-pdf--s16 bytes (+ ho 42)))
                  :loca-long loca-long)))))))

;;;; TrueType subsetting -------------------------------------------------

(defun org-pdf--glyf-range (font gid)
  "Return (START . END) byte range of glyph GID in the source font."
  (let ((offs (plist-get font :loca)))
    (when (and (>= gid 0) (< gid (1- (length offs))))
      (let ((a (aref offs gid)) (b (aref offs (1+ gid))))
        (when (< a b) (cons a b))))))

(defun org-pdf--composite-components (font gid)
  "Return the component glyph ids referenced by composite glyph GID."
  (let ((rng (org-pdf--glyf-range font gid)))
    (when rng
      (let* ((bytes (plist-get font :sfnt))
             (base (+ (plist-get font :glyf) (car rng)))
             (nc (org-pdf--s16 bytes base))
             (res nil))
        (when (< nc 0)
          (let ((p (+ base 10)) (more t) flags)
            (while more
              (setq flags (org-pdf--u16 bytes p))
              (push (org-pdf--u16 bytes (+ p 2)) res)
              (setq p (+ p 4))
              (setq p (if (zerop (logand flags #x0001)) (+ p 2) (+ p 4)))
              (cond ((not (zerop (logand flags #x0008))) (setq p (+ p 2)))
                    ((not (zerop (logand flags #x0040))) (setq p (+ p 4)))
                    ((not (zerop (logand flags #x0080))) (setq p (+ p 8))))
              (setq more (not (zerop (logand flags #x0020)))))))
        (nreverse res)))))

(defun org-pdf--close-glyphs (font gids)
  "Return GIDS plus every composite glyph they depend on, as a sorted list."
  (let ((seen (make-hash-table :test #'eql)) (stack (copy-sequence gids)))
    (while stack
      (let ((g (pop stack)))
        (when (and (>= g 0) (not (gethash g seen)))
          (puthash g t seen)
          (dolist (c (org-pdf--composite-components font g))
            (push c stack)))))
    (sort (let (l) (maphash (lambda (k _) (push k l)) seen) l) #'<)))

(defun org-pdf--rewrite-composite (font gid old->new)
  "Return GID's glyph data with composite component indices remapped.
Returns a fresh unibyte string identical to the source glyph except that any
embedded component glyph ids are replaced by their new ids in OLD->NEW."
  (let ((rng (org-pdf--glyf-range font gid)))
    (if (null rng) ""
      (let* ((bytes (plist-get font :sfnt))
             (start (+ (plist-get font :glyf) (car rng)))
             (data (copy-sequence (substring bytes start (+ (plist-get font :glyf) (cdr rng)))))
             (nc (org-pdf--s16 data 0)))
        (when (< nc 0)
          (let ((p 10) (more t) flags)
            (while more
              (setq flags (org-pdf--u16 data p))
              (let ((new (gethash (org-pdf--u16 data (+ p 2)) old->new)))
                (when new
                  (aset data (+ p 2) (logand (ash new -8) #xFF))
                  (aset data (+ p 3) (logand new #xFF))))
              (setq p (+ p 4))
              (setq p (if (zerop (logand flags #x0001)) (+ p 2) (+ p 4)))
              (cond ((not (zerop (logand flags #x0008))) (setq p (+ p 2)))
                    ((not (zerop (logand flags #x0040))) (setq p (+ p 4)))
                    ((not (zerop (logand flags #x0080))) (setq p (+ p 8))))
              (setq more (not (zerop (logand flags #x0020)))))))
        data))))

(defun org-pdf--ttf-subset (font cids)
  "Build a subset TrueType from FONT containing glyphs CIDS and dependencies.
Returns (SUBSET-BYTES . GID-MAP), where GID-MAP maps original glyph id to new
glyph id (glyphs mapped to .notdef map to 0)."
  (let* ((closure (org-pdf--close-glyphs font cids))
         (old->new (make-hash-table :test #'eql))
         (n 1))
    (dolist (g (sort (copy-sequence closure) #'<))
      (when (/= g 0) (puthash g n old->new) (setq n (1+ n))))
    (let* ((num-new n)
           (loca (make-vector (1+ num-new) 0))
           (pieces nil)
           (pos 0)
           (k 1))
      ;; Emit glyphs in new-gid order (== ascending original gid, skipping 0).
      ;; loca[k] is the START offset of glyph k, so record it before appending.
      (dolist (g (sort (copy-sequence closure) #'<))
        (when (/= g 0)
          (let* ((data (org-pdf--rewrite-composite font g old->new))
                 (len (length data))
                 (pad (- (org-pdf--pad4 len) len)))
            (aset loca k pos)
            (push data pieces)
            (setq pos (+ pos len))
            (when (> pad 0)
              (push (make-string pad 0) pieces)
              (setq pos (+ pos pad)))
            (setq k (1+ k)))))
      ;; loca[num-new] is the end offset (== glyph data length).
      (aset loca num-new pos)
      (let* ((glyf-data (apply #'concat (nreverse pieces)))
             (loca-long-p (> pos #x1FFFF))
             (loca-data (org-pdf--build-loca loca loca-long-p))
             (tables (list (org-pdf--sfnt-table "head" (org-pdf--build-head font loca-long-p))
                           (org-pdf--sfnt-table "loca" loca-data)
                           (org-pdf--sfnt-table "hhea" (org-pdf--build-hhea font num-new))
                           (org-pdf--sfnt-table "hmtx" (org-pdf--build-hmtx font closure))
                           (org-pdf--sfnt-table "maxp" (org-pdf--build-maxp num-new))
                           (org-pdf--sfnt-table "glyf" glyf-data)))
             (sfnt (org-pdf--assemble-sfnt tables)))
        (cons sfnt old->new)))))

(defun org-pdf--build-loca (loca long-p)
  "Serialise a loca table (vector LOCA) using 4- or 2-byte entries."
  (let ((out (list)))
    (dotimes (i (length loca))
      (push (if long-p
                (org-pdf--be32 (aref loca i))
              (org-pdf--be16 (/ (aref loca i) 2)))
            out))
    (apply #'concat (nreverse out))))

(defun org-pdf--table-checksum (data)
  "Sum DATA (unibyte) as big-endian 32-bit words, zero-padded."
  (let* ((len (length data))
         (padded (concat data (make-string (- (org-pdf--pad4 len) len) 0)))
         (sum 0) (i 0))
    (while (< i (length padded))
      (setq sum (logand (+ sum (org-pdf--u32 padded i)) #xFFFFFFFF))
      (setq i (+ i 4)))
    sum))

(defun org-pdf--build-head (font loca-long-p)
  "Serialise a head table for the subset of FONT.
The checksum adjustment is left at 0; readers accept this."
  (let* ((units (plist-get font :units))
         (bbox (plist-get font :bbox))
         (out (make-string 54 0)))
    (org-pdf--put-u32 out 0 #x00010000)          ; version
    (org-pdf--put-u32 out 4 #x00010000)          ; fontRevision
    (org-pdf--put-u32 out 12 #x5F0F3CF5)         ; magicNumber
    (org-pdf--put-u16 out 16 #x000B)             ; flags
    (org-pdf--put-u16 out 18 units)              ; unitsPerEm
    (org-pdf--put-s16 out 36 (nth 0 bbox))
    (org-pdf--put-s16 out 38 (nth 1 bbox))
    (org-pdf--put-s16 out 40 (nth 2 bbox))
    (org-pdf--put-s16 out 42 (nth 3 bbox))
    (org-pdf--put-u16 out 46 0)                  ; macStyle
    (org-pdf--put-u16 out 48 8)                  ; lowestRecPPEM
    (org-pdf--put-s16 out 50 (if loca-long-p 1 0))
    out))

(defun org-pdf--build-hhea (font num-glyphs)
  "Serialise an hhea table for a subset with NUM-GLYPHS glyphs."
  (let* ((asc (plist-get font :ascent))
         (desc (plist-get font :descent))
         (out (make-string 36 0)))
    (org-pdf--put-u32 out 0 #x00010000)
    (org-pdf--put-s16 out 4 asc)
    (org-pdf--put-s16 out 6 desc)
    (org-pdf--put-u16 out 10 (plist-get font :units)) ; advanceWidthMax
    (org-pdf--put-s16 out 20 1)                  ; caretSlopeRise
    (org-pdf--put-u16 out 34 num-glyphs)         ; numberOfHMetrics
    out))

(defun org-pdf--build-hmtx (font closure)
  "Serialise an hmtx table with one metric per glyph, in new-gid order.
New gid 0 is .notdef; new gid k>=1 is the k-th non-zero glyph of CLOSURE in
ascending original order (the same order used for loca/glyf and old->new).
The table must hold exactly (1 + number of non-zero glyphs) entries, matching
the numGlyphs / numberOfHMetrics written to maxp and hhea."
  (let ((widths (plist-get font :widths))
        (out (list)))
    ;; gid 0 (.notdef)
    (push (org-pdf--be16 (if (> (length widths) 0) (aref widths 0) 0)) out)
    (push (org-pdf--be16 0) out)
    ;; gid 1..K
    (dolist (g (sort (copy-sequence closure) #'<))
      (unless (zerop g)
        (let ((w (if (< g (length widths)) (aref widths g) 0)))
          (push (org-pdf--be16 w) out)
          (push (org-pdf--be16 0) out))))       ; lsb = 0 (unused by viewer)
    (apply #'concat (nreverse out))))

(defun org-pdf--build-maxp (num-glyphs)
  "Serialise a minimal maxp table for NUM-GLYPHS glyphs."
  (let ((out (make-string 32 0)))
    (org-pdf--put-u32 out 0 #x00010000)
    (org-pdf--put-u16 out 4 num-glyphs)
    out))

(defun org-pdf--put-u16 (str off val)
  (aset str off (logand (ash val -8) #xFF))
  (aset str (1+ off) (logand val #xFF)))
(defun org-pdf--put-s16 (str off val) (org-pdf--put-u16 str off (logand val #xFFFF)))
(defun org-pdf--put-u32 (str off val)
  (aset str off (logand (ash val -24) #xFF))
  (aset str (1+ off) (logand (ash val -16) #xFF))
  (aset str (+ off 2) (logand (ash val -8) #xFF))
  (aset str (+ off 3) (logand val #xFF)))

(defun org-pdf--sfnt-table (tag data) (cons tag data))

(defun org-pdf--assemble-sfnt (tables)
  "Assemble TABLES (list of (TAG . DATA)) into a complete sfnt font."
  (let* ((sorted (sort (copy-sequence tables)
                       (lambda (a b) (string< (car a) (car b)))))
         (n (length sorted))
         (dir-len (+ 12 (* 16 n)))
         (off dir-len)
         (dir (list)) (body (list)))
    (let ((p2 1))
      (while (<= (* 2 p2) n) (setq p2 (* 2 p2)))
      (push (org-pdf--be32 #x00010000) dir)
      (push (org-pdf--be16 n) dir)
      (push (org-pdf--be16 (* 16 p2)) dir)                     ; searchRange
      (push (org-pdf--be16 (org-pdf--ilog2 p2)) dir)           ; entrySelector
      (push (org-pdf--be16 (- (* 16 n) (* 16 p2))) dir))       ; rangeShift
    (dolist (tbl sorted)
      (let* ((tag (car tbl))
             (data (cdr tbl))
             (len (length data))
             (padded (concat data (make-string (- (org-pdf--pad4 len) len) 0))))
        (push (concat (if (= (length tag) 4) tag (concat tag (make-string (- 4 (length tag)) 32)))
                      (org-pdf--be32 (org-pdf--table-checksum data))
                      (org-pdf--be32 off)
                      (org-pdf--be32 len))
              dir)
        (push padded body)
        (setq off (+ off (length padded)))))
    (concat (apply #'concat (nreverse dir))
            (apply #'concat (nreverse body)))))

(defun org-pdf--ilog2 (n)
  "Return floor(log2 N) for N >= 1."
  (let ((k 0)) (while (> n 1) (setq n (ash n -1) k (1+ k))) k))

;;;; PDF font objects ----------------------------------------------------

(defun org-pdf--pdf-object (bytes)
  "Wrap BYTES as a PDF stream dictionary + body for a FontFile2."
  (concat "<< /Length1 " (number-to-string (length bytes))
          " /Length " (number-to-string (length bytes))
          " >>\nstream\n" bytes "\nendstream"))

(defun org-pdf--add-truetype (w font cids used-codes)
  "Embed the subset of FONT containing glyphs CIDS into writer W.
USED-CODES is a hash of the code points in the document, used to build a
ToUnicode CMap so the text stays copyable.
Returns (FONT-OBJECT-NUMBER . GID-MAP)."
  (let* ((res (org-pdf--ttf-subset font cids))
         (subset (car res))
         (gid-map (cdr res))
         (units (plist-get font :units))
         (ascent (plist-get font :ascent))
         (descent (plist-get font :descent))
         (bbox (plist-get font :bbox))
         (to1000 (lambda (v) (round (/ (* 1000.0 v) units))))
         (fontfile (org-pdf--w-add w (org-pdf--pdf-object subset)))
         (tounicode (org-pdf--build-tounicode w font used-codes gid-map))
         ;; CIDFontType2: CIDs equal glyph ids; Identity-H maps 2-byte codes
         ;; to CIDs 1:1, and CIDToGIDMap /Identity maps CID to the same gid.
         (desc (org-pdf--w-add
                w
                (format (concat "<< /Type /FontDescriptor /FontName /OrgPdfCJK"
                                " /Flags 4 /FontBBox [%d %d %d %d] /ItalicAngle 0"
                                " /Ascent %d /Descent %d /CapHeight %d /StemV 80"
                                " /FontFile2 %d 0 R >>")
                        (nth 0 bbox) (nth 1 bbox) (nth 2 bbox) (nth 3 bbox)
                        (funcall to1000 ascent) (funcall to1000 descent)
                        (funcall to1000 ascent) fontfile)))
         (warray (org-pdf--font-w-array font gid-map to1000))
         (descendant (org-pdf--w-add
                      w
                      (format (concat "<< /Type /Font /Subtype /CIDFontType2"
                                      " /BaseFont /OrgPdfCJK"
                                      " /CIDSystemInfo << /Registry (Adobe)"
                                      " /Ordering (Identity) /Supplement 0 >>"
                                      " /FontDescriptor %d 0 R /DW 1000 /W %s"
                                      " /CIDToGIDMap /Identity >>")
                              desc warray)))
         (fontobj (org-pdf--w-add
                   w
                   (format (concat "<< /Type /Font /Subtype /Type0"
                                   " /BaseFont /OrgPdfCJK /Encoding /Identity-H"
                                   " /DescendantFonts [%d 0 R]"
                                   " /ToUnicode %d 0 R >>")
                           descendant tounicode))))
    (cons fontobj gid-map)))

(defun org-pdf--utf16be (code)
  "Return CODE as a big-endian UTF-16 hex string (surrogate pair if needed)."
  (if (< code #x10000)
      (format "%04X" code)
    (let* ((c (- code #x10000))
           (hi (+ #xD800 (ash c -10)))
           (lo (+ #xDC00 (logand c #x3FF))))
      (format "%04X%04X" hi lo))))

(defun org-pdf--build-tounicode (w font used-codes gid-map)
  "Add a /ToUnicode CMap object to W and return its object number.
The CMap maps each 2-byte subset glyph id (as written by Identity-H) back to
the Unicode code point it came from, so text can be copied, searched and
extracted.  FONT is the parsed source font; USED-CODES is a hash of code
points; GID-MAP maps original gid to subset gid."
  (let* ((cmap (plist-get font :cmap))
         (entries nil))
    (maphash
     (lambda (code _)
       (let* ((orig (and cmap (gethash code cmap)))
              (sub (and orig gid-map (gethash orig gid-map))))
         (when sub (push (cons sub code) entries))))
     used-codes)
    (setq entries (sort entries (lambda (a b) (< (car a) (car b)))))
    (let ((body (list)))
      (push (concat "/CIDInit /ProcSet findresource begin\n"
                    "12 dict begin\nbegincmap\n"
                    "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n"
                    "/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n"
                    "1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n")
            body)
      ;; beginbfchar blocks are limited to 100 entries each.
      (let ((chunks nil) (cur nil))
        (dolist (e entries)
          (push (format "<%04X> <%s>\n" (car e) (org-pdf--utf16be (cdr e))) cur)
          (when (>= (length cur) 100)
            (push (nreverse cur) chunks) (setq cur nil)))
        (when cur (push (nreverse cur) chunks))
        (dolist (chunk (nreverse chunks))
          (push (format "%d beginbfchar\n%s endbfchar\n" (length chunk)
                        (apply #'concat chunk))
                body)))
      (push "endcmap\nCMapName currentdict /CMap defineresource pop\nend\nend\n" body)
      (org-pdf--w-add-stream w "" (apply #'concat (nreverse body))))))

(defun org-pdf--font-w-array (font gid-map to1000)
  "Build a PDF /W array string from FONT advances and GID-MAP.
TO1000 scales font units to 1/1000 em.  The default width is 1000, so only
glyphs whose advance differs are listed; consecutive equal widths are
collapsed into `first last width' runs."
  (let ((widths (make-hash-table :test #'eql))
        (maxgid 0))
    (maphash
     (lambda (old new)
       (let ((w (funcall to1000
                         (if (< old (length (plist-get font :widths)))
                             (aref (plist-get font :widths) old) 0))))
         (unless (= w 1000) (puthash new w widths))
         (setq maxgid (max maxgid new))))
     gid-map)
    (let ((parts nil) (g 1))
      (while (<= g maxgid)
        (let ((w (gethash g widths)))
          (if (null w)
              (setq g (1+ g))
            ;; find run [g, h] of identical width
            (let ((h g))
              (while (and (<= (1+ h) maxgid)
                          (equal (gethash (1+ h) widths) w))
                (setq h (1+ h)))
              (push (if (= g h)
                        (format "%d [%d]" g w)
                      (format "%d %d %d" g h w))
                    parts)
              (setq g (1+ h))))))
      (if parts
          (concat "[" (mapconcat #'identity (nreverse parts) " ") "]")
        "[]"))))

(provide 'org-pdf-font)
;;; org-pdf-font.el ends here
