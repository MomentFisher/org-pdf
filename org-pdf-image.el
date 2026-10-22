;;; org-pdf-image.el --- PNG/JPEG image embedding for org-pdf  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;;; Commentary:

;; Org `[[file:foo.png]]' links are embedded as PDF image XObjects.  Two
;; strategies keep the output small without any external program:
;;
;;  * JPEG files are copied in verbatim and tagged /Filter /DCTDecode -- the
;;    PDF reader decodes the JPEG itself, so nothing is re-encoded.
;;
;;  * A PNG whose samples are 8- or 16-bit (i.e. byte-aligned rows) that is
;;    *not* interlaced and carries no alpha can reuse its compressed IDAT
;;    stream as a /FlateDecode image with /Predictor 15 -- PDF's PNG
;;    predictor is byte-identical to PNG's own row filters, so the bytes pass
;;    through untouched.
;;
;;  * A PNG that *does* carry alpha (grey+alpha or RGBA) cannot be passed
;;    through: PDF has no RGBA colour space, so it must be split into an RGB
;;    (or grey) image plus a separate soft mask (/SMask).  That requires
;;    inflating the IDAT stream and undoing the per-row filters.  Emacs can
;;    decompress with `zlib-decompress-region' but cannot *compress*, so the
;;    split samples are written uncompressed.  To keep such files from
;;    ballooning, the decoded samples are box-downsampled so the longest side
;;    is at most `org-pdf-image-max-pixels'.

;;; Code:

(require 'cl-lib)
(require 'org-pdf-pdf)

(defcustom org-pdf-image-max-pixels 1600
  "Longest side, in pixels, that an alpha PNG is downsampled to.
Only affects images that must be decoded (those with an alpha channel);
JPEGs and opaque 8/16-bit PNGs are embedded losslessly at full resolution.
Because org-pdf cannot recompress image data, a decoded image is stored
uncompressed, so a smaller limit means a smaller PDF.  Set to nil to keep
the original resolution."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'org-pdf)

(defconst org-pdf--image-extensions '("png" "jpg" "jpeg" "svg")
  "File-name extensions treated as embeddable images.")

(defcustom org-pdf-svg-java-jar
  (expand-file-name "java/target/org-pdf-svg.jar"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Path to the helper jar that renders SVG files to PDF form XObjects.
Build it once with `mvn package' in the java/ subdirectory of this package.
SVG images are skipped with a warning when the jar is missing."
  :type 'file
  :group 'org-pdf)

(defcustom org-pdf-svg-java-program "java"
  "Java launcher used to run `org-pdf-svg-java-jar'."
  :type 'string
  :group 'org-pdf)

(defvar org-pdf--svg-cache (make-hash-table :test #'equal)
  "Cache of rendered SVG bundles, keyed by (FILE . MODTIME).
Rendering starts a JVM, so the same image is only converted once per run.")

(defconst org-pdf--image-dpi 96.0
  "Assumed screen resolution of raster images, for sizing them in points.")

(defconst org-pdf--smask-token "@@SMASK@@"
  "Placeholder in an image dictionary for the soft-mask object number.")

;;;; Byte helpers ---------------------------------------------------------

(defsubst org-pdf--img-u8 (b o) (aref b o))
(defsubst org-pdf--img-u16 (b o) (+ (ash (aref b o) 8) (aref b (1+ o))))
(defsubst org-pdf--img-u32 (b o) (+ (ash (aref b o) 24) (ash (aref b (1+ o)) 16)
                                    (ash (aref b (+ o 2)) 8) (aref b (+ o 3))))

;;;; Reading --------------------------------------------------------------

(defun org-pdf--image-read-bytes (path)
  "Return the raw bytes of file PATH as a unibyte string, or nil."
  (when (file-readable-p path)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (buffer-string))))

(defun org-pdf--image-png-p (bytes)
  "Return non-nil if BYTES begin with the PNG signature."
  (and (>= (length bytes) 8)
       (equal (substring bytes 0 8) (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a))))

(defun org-pdf--image-jpeg-p (bytes)
  "Return non-nil if BYTES begin with the JPEG start-of-image marker."
  (and (>= (length bytes) 3)
       (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8)))

(defun org-pdf--image-file-p (path)
  "Return non-nil if PATH names a file org-pdf can embed as an image.
Only true for directories-less existing PNG/JPEG files; the check is by
extension so it stays cheap when deciding how to lay a paragraph out."
  (and (stringp path)
       (let ((ext (downcase (or (file-name-extension path) ""))))
         (member ext org-pdf--image-extensions))
       (file-readable-p path)))

;;;; Header parsing (cheap; used for layout sizing) -----------------------

(defun org-pdf--png-chunks (bytes)
  "Return a list of (TYPE . DATA) for the chunks in PNG BYTES.
Stops at IEND, and stops early rather than reading past the end: PNG files
sometimes carry trailing junk after IEND, whose bytes are not a chunk header
at all and would otherwise yield a preposterous length."
  (let ((i 8) (n (length bytes)) (out nil) (done nil))
    (while (and (not done) (<= (+ i 12) n))
      (let* ((len (org-pdf--img-u32 bytes i))
             (end (+ i 12 len))
             (type (substring bytes (+ i 4) (+ i 8))))
        (when (> end n)
          (setq done t))
        (unless done
          (push (cons type (substring bytes (+ i 8) (+ i 8 len))) out)
          (when (equal type "IEND")
            (setq done t))
          (setq i end))))
    (nreverse out)))

(defun org-pdf--png-idat (chunks)
  "Concatenate the IDAT payloads from CHUNKS into one string."
  (let ((parts nil))
    (dolist (c chunks)
      (when (equal (car c) "IDAT") (push (cdr c) parts)))
    (apply #'concat (nreverse parts))))

(defun org-pdf--image-jpeg-info (bytes)
  "Return a plist describing JPEG BYTES, or nil.
Fields: :type :width :height :components.  Only markers up to the first
start-of-frame are read -- enough to size the image."
  (let ((i 2) (n (length bytes)) (res nil))
    (while (and (< i n) (not res))
      (if (/= (aref bytes i) #xff)
          (setq i (1+ i))
        (let ((marker (aref bytes (1+ i))))
          (cond
           ;; start-of-frame markers carry the dimensions
           ((memq marker '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7 #xc9 #xca #xcb #xcd #xce #xcf))
            (let ((h (org-pdf--img-u16 bytes (+ i 5)))
                  (w (org-pdf--img-u16 bytes (+ i 7)))
                  (comp (aref bytes (+ i 9))))
              (setq res (list :type 'jpeg :width w :height h :components comp))))
           ;; markers with no payload
           ((memq marker '(#xd8 #xd9 #x01 #xd0 #xd1 #xd2 #xd3 #xd4 #xd5 #xd6 #xd7))
            (setq i (+ i 2)))
           (t (setq i (+ i 2 (org-pdf--img-u16 bytes (+ i 2)))))))))
    res))

(defun org-pdf--image-png-info (bytes)
  "Return a plist describing PNG BYTES, or nil.
Fields: :type :width :height :bit-depth :color-type :interlace :palette
:trns.  :palette is a list of (R G B); :trns is the tRNS payload or nil."
  (let* ((chunks (org-pdf--png-chunks bytes))
         (ihdr (cdr (assoc "IHDR" chunks))))
    (when (and ihdr (>= (length ihdr) 13))
      (let* ((w (org-pdf--img-u32 ihdr 0))
             (h (org-pdf--img-u32 ihdr 4))
             (bd (aref ihdr 8))
             (ct (aref ihdr 9))
             (il (aref ihdr 12))
             (plte (cdr (assoc "PLTE" chunks)))
             (trns (cdr (assoc "tRNS" chunks)))
             (pal (and plte
                       (let ((i 0) (out nil))
                         (while (< i (length plte))
                           (push (list (aref plte i) (aref plte (1+ i)) (aref plte (+ i 2))) out)
                           (setq i (+ i 3)))
                         (nreverse out)))))
        (list :type 'png :width w :height h :bit-depth bd :color-type ct
              :interlace il :palette pal :trns trns)))))

(defun org-pdf--svg-length-pt (s)
  "Convert SVG length S to points, or nil.
A unitless length is an SVG user unit, which is a CSS pixel (1/96 inch).
Percentage and unknown units are rejected so the caller can fall back."
  (when (and (stringp s) (string-match
                          "\\`[ \t]*\\([-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?\\)[ \t]*\\([a-z%]*\\)"
                          s))
    (let ((n (string-to-number (match-string 1 s)))
          (unit (downcase (match-string 2 s))))
      (pcase unit
        ("pt" n) ("in" (* n 72.0)) ("pc" (* n 12.0))
        ("px" (* n (/ 72.0 96.0)))
        ("mm" (* n (/ 72.0 25.4))) ("cm" (* n (/ 72.0 2.54)))
        (""   (* n (/ 72.0 96.0)))
        (_ nil)))))

(defun org-pdf--svg-info (path)
  "Return a plist describing the SVG at PATH, or nil.
The width and height are in *pixels at 96dpi*, matching what raster images
report, so the layout code treats both alike.  Reads the root element only."
  (when (and (fboundp 'libxml-parse-xml-region) (file-readable-p path))
    (condition-case nil
        (with-temp-buffer
          (set-buffer-multibyte t)
          (insert-file-contents path)
          (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
                 (attrs (and dom (cadr dom)))
                 ;; libxml returns attribute names as symbols.
                 (get (lambda (k) (cdr (assq (intern k) attrs))))
                 (w (funcall get "width"))
                 (h (funcall get "height"))
                 (vb (funcall get "viewBox"))
                 (nw (org-pdf--svg-length-pt w))
                 (nh (org-pdf--svg-length-pt h))
                 (nvb (when vb
                        (mapcar #'string-to-number
                                (split-string (string-trim vb) "[ \t\n\r,]+" t)))))
            (cond
             ((and nw nh (>= nw 0) (>= nh 0))
              (list :type 'svg :width (* nw (/ 96.0 72.0)) :height (* nh (/ 96.0 72.0))))
             ((and nvb (= (length nvb) 4) (> (nth 2 nvb) 0) (> (nth 3 nvb) 0))
              (list :type 'svg :width (nth 2 nvb) :height (nth 3 nvb)))
             (t nil))))
      (error nil))))

(defun org-pdf--image-info (path)
  "Return a plist describing the image at PATH, or nil.
Reads only the file header, so it is cheap enough for the layout pass."
  (if (equal (downcase (or (file-name-extension path) "")) "svg")
      (org-pdf--svg-info path)
    (let ((bytes (org-pdf--image-read-bytes path)))
      (when bytes
        (cond
         ((org-pdf--image-png-p bytes) (org-pdf--image-png-info bytes))
         ((org-pdf--image-jpeg-p bytes) (org-pdf--image-jpeg-info bytes))
         (t nil))))))

(defun org-pdf--image-pixel-size (path)
  "Return (WIDTH . HEIGHT) in pixels of the image at PATH, or nil."
  (let ((info (org-pdf--image-info path)))
    (when info
      (cons (plist-get info :width) (plist-get info :height)))))

;;;; PNG sample decoding (only the alpha-colour path) --------------------

(defun org-pdf--png-inflate (data)
  "Return the inflated bytes of the zlib stream DATA, or nil on error."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert data)
    (condition-case nil
        (progn (zlib-decompress-region (point-min) (point-max)) (buffer-string))
      (error nil))))

(defun org-pdf--paeth (a b c)
  "Paeth predictor for the three neighbouring bytes A, B and C."
  (let* ((p (- (+ a b) c))
         (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(defun org-pdf--png-unfilter-row (raw foff rowbytes bpp ftype prev out)
  "Undo one PNG row filter.
RAW is the inflated IDAT payload; the row's filter byte is at FOFF - 1 and
its ROWBYTES sample bytes start at FOFF.  BPP is the byte distance to the
left neighbour; FTYPE is the PNG filter type; PREV is the previous
reconstructed row (ROWBYTES bytes); OUT is the destination row buffer, which
is filled in place.  Sub/Average/Paeth index the first BPP bytes
separately so the main loop needs no per-byte bound test."
  (let ((x 0))
    (while (< x bpp)
      (let* ((xraw (aref raw (+ foff x)))
             (b (aref prev x))
             (v (pcase ftype
                  (1 xraw) (2 (+ xraw b)) (3 (+ xraw (ash b -1)))
                  (4 (+ xraw (org-pdf--paeth 0 b 0))) (_ xraw))))
        (aset out x (logand v 255))
        (setq x (1+ x))))
    (while (< x rowbytes)
      (let* ((xraw (aref raw (+ foff x)))
             (a (aref out (- x bpp)))
             (b (aref prev x))
             (v (pcase ftype
                  (0 xraw) (1 (+ xraw a)) (2 (+ xraw b))
                  (3 (+ xraw (ash (+ a b) -1)))
                  (4 (+ xraw (org-pdf--paeth a b (aref prev (- x bpp)))))
                  (_ xraw))))
        (aset out x (logand v 255))
        (setq x (1+ x))))))

(defun org-pdf--png-decode-stream (raw src-w src-h color-comp dw dh)
  "Decode a non-interlaced 8-bit PNG and box-downsample it in a single pass.
RAW is the inflated IDAT payload (one filter byte then the samples of each
row).  Each source pixel is COLOR-COMP colour bytes plus one alpha byte, so
the row stride is (COLOR-COMP + 1) bytes per pixel.  Source rows are
reconstructed one at a time and accumulated straight into the destination
rows, so no full-resolution buffer is ever materialised.  Returns (COLOR .
ALPHA): COLOR is DW * DH * COLOR-COMP bytes, ALPHA is DW * DH bytes.  Averages
truncate toward zero, matching a plain box filter."
  (let* ((nplanes (1+ color-comp))
         (rowbytes (* src-w nplanes))
         (colors (make-string (* dw dh color-comp) 0))
         (alpha (make-string (* dw dh) 0))
         (prev (make-string rowbytes 0))
         (row (make-string rowbytes 0))
         (acc (make-vector (* dw nplanes) 0))
         (counts (make-vector dw 0))
         (total 0)
         (dy 0)
         (by (/ (float src-h) dh))
         ;; Per-destination-column source spans and destination byte offsets,
         ;; computed once so the hot loops are free of floating-point maths.
         (spans (make-vector dw nil))
         (abses (make-vector dw 0))
         (cbs (make-vector dw 0)))
    (dotimes (dx dw)
      ;; Source columns for destination column DX span
      ;; [floor(dx*src-w/dw), max(x0+1, ceil((dx+1)*src-w/dw))), capped at
      ;; src-w.  Note the two-argument `floor'/`ceiling' -- the division must
      ;; not be truncated to an integer first, or ceil becomes floor.
      (let* ((x0 (min (1- src-w) (floor (* dx src-w) dw)))
             (x1 (min src-w (max (1+ x0) (ceiling (* (1+ dx) src-w) dw)))))
        (aset spans dx (cons (* x0 nplanes) (- x1 x0)))
        (aset abses dx (* dx nplanes))
        (aset cbs dx (* dx color-comp))))
    (cl-flet ((flush ()
                (when (> total 0)
                  (dotimes (dx dw)
                    (let ((abase (aref abses dx))
                          (cbase (aref cbs dx))
                          (py (* dy dw))
                          (cnt (aref counts dx)))
                      (when (> cnt 0)
                        (dotimes (p color-comp)
                          (aset colors (+ (* py color-comp) cbase p)
                                (/ (aref acc (+ abase p)) cnt)))
                        (aset alpha (+ py dx)
                              (/ (aref acc (+ abase color-comp)) cnt)))))
                  (fillarray acc 0)
                  (fillarray counts 0)
                  (setq total 0))))
      (dotimes (y src-h)
        ;; Reconstruct this source row into ROW, reading filter type and data
        ;; from RAW and using the previous row as the up-reference.
        (org-pdf--png-unfilter-row raw (1+ (* y (1+ rowbytes))) rowbytes
                                   nplanes (aref raw (* y (1+ rowbytes))) prev row)
        ;; Advance to the destination row this source row belongs to, flushing
        ;; any destination rows we have finished with.
        (let ((target (min (1- dh) (floor (/ (* (+ y 0.5) dh) src-h)))))
          (while (< dy target)
            (flush)
            (setq dy (1+ dy))))
        ;; Add every source pixel's contribution to its destination pixel.
        (if (= nplanes 4)
            ;; Fast path for RGBA/grey+alpha: NPLANES is 4, so the four
            ;; samples are added without a per-plane loop.
            (dotimes (dx dw)
              (let* ((sp (aref spans dx))
                     (rowpx (car sp))
                     (n (cdr sp))
                     (abase (aref abses dx)))
                (dotimes (_ n)
                  (aset acc abase (+ (aref acc abase) (aref row rowpx)))
                  (aset acc (1+ abase) (+ (aref acc (1+ abase)) (aref row (1+ rowpx))))
                  (aset acc (+ abase 2) (+ (aref acc (+ abase 2)) (aref row (+ rowpx 2))))
                  (aset acc (+ abase 3) (+ (aref acc (+ abase 3)) (aref row (+ rowpx 3))))
                  (setq rowpx (+ rowpx 4)
                        total (1+ total)))
                (aset counts dx (+ (aref counts dx) n))))
          (dotimes (dx dw)
            (let* ((sp (aref spans dx))
                   (rowpx (car sp))
                   (n (cdr sp))
                   (abase (aref abses dx)))
              (dotimes (_ n)
                (let ((r rowpx))
                  (dotimes (p nplanes)
                    (aset acc (+ abase p)
                          (+ (aref acc (+ abase p)) (aref row (+ r p)))))
                  (setq rowpx (+ rowpx nplanes)
                        total (1+ total))))
              (aset counts dx (+ (aref counts dx) n)))))
        ;; ROW is now the previous row for the next iteration; recycle the
        ;; buffer that just became free.
        (let ((tmp prev)) (setq prev row row tmp)))
      ;; Flush the final destination row.
      (flush))
    (cons colors alpha)))

;;;; Building the PDF image XObject(s) ------------------------------------

(defun org-pdf--png-passthrough-dict (info)
  "Return a PDF image dict string for PNG INFO whose IDAT can pass through.
INFO must describe a non-interlaced, byte-aligned (8/16-bit) image with no
alpha channel.  The IDAT stream itself is supplied separately as the image
data; PDF's Predictor 15 reproduces PNG's row filters exactly."
  (let* ((w (plist-get info :width))
         (bd (plist-get info :bit-depth))
         (ct (plist-get info :color-type))
         (pal (plist-get info :palette))
         (cs (pcase ct
               (0 "/DeviceGray")
               (2 "/DeviceRGB")
               (3 "/Indexed /DeviceRGB")))
         (colors (if (eq ct 2) 3 1)))
    (concat (format " /Type /XObject /Subtype /Image /Width %d /Height %d" w (plist-get info :height))
            (if (eq ct 3)
                (format " /ColorSpace [/Indexed /DeviceRGB %d <%s>] /BitsPerComponent %d"
                        (1- (length pal)) (org-pdf--hex (org-pdf--plte-bytes pal)) bd)
              (format " /ColorSpace %s /BitsPerComponent %d" cs bd))
            " /Filter /FlateDecode"
            (format " /DecodeParms << /Predictor 15 /Colors %d /BitsPerComponent %d /Columns %d >>"
                    colors bd w))))

(defun org-pdf--plte-bytes (pal)
  "Return PAL (a list of (R G B)) as a raw byte string, for /Indexed."
  (apply #'unibyte-string (apply #'append pal)))

(defun org-pdf--hex (bytes)
  "Return BYTES (a unibyte string) as lowercase hex."
  (let ((out (list)) (i 0) (n (length bytes)))
    (while (< i n)
      (push (format "%02x" (aref bytes i)) out)
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun org-pdf--png-alpha-entries (info)
  "Decode PNG INFO (an 8-bit alpha PNG) into PDF image entries.
Returns a plist (:width :height :entries ENTRIES).  ENTRIES is a list of
two (DICT . DATA) pairs: the base RGB/grey image whose dictionary ends in a
literal `SMASK' token, and the soft mask.  The caller substitutes the real
object number for that token.  Returns nil if the data cannot be decoded."
  (let* ((bytes (org-pdf--image-read-bytes (plist-get info :path)))
         (chunks (and bytes (org-pdf--png-chunks bytes)))
         (raw (and chunks (org-pdf--png-inflate (org-pdf--png-idat chunks))))
         (src-w (plist-get info :width))
         (src-h (plist-get info :height))
         (ct (plist-get info :color-type))
         (color-comp (if (eq ct 4) 1 3)))
    (when (and raw (= (plist-get info :bit-depth) 8))
      (let* ((limit org-pdf-image-max-pixels)
             (scale (if (and limit (> (max src-w src-h) limit))
                        (/ (float limit) (max src-w src-h))
                      1.0))
             (dw (max 1 (round (* src-w scale))))
             (dh (max 1 (round (* src-h scale))))
             (split (org-pdf--png-decode-stream raw src-w src-h color-comp dw dh))
             (colors (car split))
             (alpha (cdr split)))
        (list :width dw :height dh
              :entries
              (list
               (cons (format (concat " /Type /XObject /Subtype /Image /Width %d /Height %d"
                                     " /ColorSpace %s /BitsPerComponent 8 /SMask %s")
                             dw dh (if (eq color-comp 1) "/DeviceGray" "/DeviceRGB")
                             org-pdf--smask-token)
                     colors)
               (cons (format (concat " /Type /XObject /Subtype /Image /Width %d /Height %d"
                                     " /ColorSpace /DeviceGray /BitsPerComponent 8")
                             dw dh)
                     alpha)))))))

(defconst org-pdf--smask-token "@@SMASK@@"
  "Placeholder replaced by the soft-mask object number when assembling.")

(defun org-pdf--image-embed (path)
  "Return embeddable PDF image data for the file at PATH, or nil.
The value is a plist (:width :height :entries ENTRIES), where ENTRIES is a
list of (DICT . DATA) pairs: for an opaque image a single entry, for an
alpha image the base image and its /SMask.  DICT is the dictionary body
\(without `<<' `>>'); DATA is the raw stream bytes.  The base image's
dictionary may contain `org-pdf--smask-token' where the soft-mask object
number belongs, and its :width/:height are the *raster* dimensions."
  (let ((bytes (org-pdf--image-read-bytes path)))
    (cond
     ((null bytes) nil)
     ;; SVG: rendered to a form XObject by the Java helper.
     ((equal (downcase (or (file-name-extension path) "")) "svg")
      (org-pdf--svg-entries path))
     ;; JPEG: embed the original bytes, no decoding.
     ((org-pdf--image-jpeg-p bytes)
      (let ((info (org-pdf--image-jpeg-info bytes)))
        (when info
          (let ((cs (pcase (plist-get info :components) (1 "/DeviceGray") (_ "/DeviceRGB"))))
            (list :width (plist-get info :width)
                  :height (plist-get info :height)
                  :entries
                  (list (cons (format (concat " /Type /XObject /Subtype /Image /Width %d"
                                              " /Height %d /ColorSpace %s /BitsPerComponent 8"
                                              " /Filter /DCTDecode")
                                      (plist-get info :width) (plist-get info :height) cs)
                              bytes)))))))
     ;; PNG.
     ((org-pdf--image-png-p bytes)
      (let* ((raw-info (org-pdf--image-png-info bytes))
             (info (and raw-info (plist-put raw-info :path path)))
             (ct (plist-get info :color-type))
             (bd (plist-get info :bit-depth))
             (trns (plist-get info :trns))
             (alpha (memq ct '(4 6)))
             (il (plist-get info :interlace))
             (passthrough (and (zerop il) (memq bd '(8 16))
                               (not alpha)
                               (not (and (eq ct 3) trns)))))
        (cond
         (passthrough
          (let* ((chunks (org-pdf--png-chunks bytes))
                 (idat (org-pdf--png-idat chunks)))
            (list :width (plist-get info :width)
                  :height (plist-get info :height)
                  :entries
                  (list (cons (org-pdf--png-passthrough-dict info) idat)))))
         ((and alpha (= bd 8) (zerop il))
          (org-pdf--png-alpha-entries info))
         (t nil))))
     (t nil))))

;;;; SVG (rendered to a PDF form XObject by the Java helper) --------------

(defun org-pdf--img-concat (&rest parts)
  "Concatenate PARTS byte-for-byte into a unibyte string.
Plain `concat' refuses to mix multibyte and unibyte strings; bundle
dictionaries are text but stream payloads are arbitrary bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (dolist (p parts)
      (insert (if (multibyte-string-p p) (string-to-unibyte p) p)))
    (buffer-string)))

(defun org-pdf--svg-call-java (path outdir)
  "Run the SVG helper on PATH, writing a bundle to OUTDIR.
Returns non-nil on success; signals an error with the helper's message on
failure."
  (let ((jar (expand-file-name org-pdf-svg-java-jar)))
    (unless (file-readable-p jar)
      (error "SVG helper jar not found (%s); build it with `mvn package' in %s"
             jar (expand-file-name "java/" (file-name-directory jar))))
    (with-temp-buffer
      (let ((status (call-process org-pdf-svg-java-program nil t nil
                                  "-Djava.awt.headless=true" "-jar" jar
                                  (expand-file-name path) outdir)))
        (cond
         ((eq status 0) t)
         (t (error "SVG rendering failed for %s: %s"
                   (file-name-nondirectory path)
                   (string-trim (buffer-string)))))))))

(defun org-pdf--svg-bundle (path)
  "Return the cached bundle plist for the SVG at PATH, rendering if needed.
The cache key includes the file's modification time, so an edited image is
re-rendered.  A failed render is remembered for the rest of the session, so a
broken image is not retried once per page."
  (let* ((real (file-truename path))
         (key (cons real (or (nth 5 (file-attributes real)) 0)))
         (hit (gethash key org-pdf--svg-cache 'miss)))
    (cond
     ((not (eq hit 'miss)) hit)
     (t
      (let* ((dir (make-temp-file "org-pdf-svg-" t))
             (result (condition-case err
                         (progn
                           (org-pdf--svg-call-java real dir)
                           (org-pdf--svg-read-bundle dir))
                       (error
                        (display-warning 'org-pdf (error-message-string err) :warning)
                        (delete-directory dir t)
                        nil))))
        (unless result (delete-directory dir t))
        (puthash key result org-pdf--svg-cache)
        result)))))

(defun org-pdf--svg-read-bundle (dir)
  "Parse the bundle in DIR into a plist (:width :height :root :objects).
:objects is a list of (ID . (DICT . DATA)) in id order, where DICT is a
dictionary body and DATA the stream bytes, or nil for a dictionary-only
object.  Indirect references inside DICT appear as `@N'; the assembler turns
them into real object numbers."
  (let* ((meta (with-temp-buffer
                 (insert-file-contents (expand-file-name "meta" dir))
                 (split-string (buffer-string) "[\n\r]+" t)))
         (width (string-to-number (nth 0 meta)))
         (height (string-to-number (nth 1 meta)))
         (root (string-to-number (nth 2 meta)))
         (count (string-to-number (nth 3 meta)))
         (objects nil))
    (dotimes (i count)
      (let* ((id (1+ i))
             (file (expand-file-name (format "obj-%d" id) dir))
             (bytes (org-pdf--image-read-bytes file))
             (split (and bytes (string-match "\n\n" bytes))))
        (when bytes
          (if split
              (push (cons id (cons (substring bytes 0 split)
                                   (substring bytes (match-end 0))))
                    objects)
            (push (cons id (cons bytes nil)) objects)))))
    (when (and (> width 0) (> height 0) (>= root 1))
      (list :width width :height height :root root
            :objects (nreverse objects)))))

(defun org-pdf--svg-entries (path)
  "Return embeddable data for the SVG at PATH, or nil.
The value is a plist (:width :height :entries ENTRIES :svg-spec SPEC), where
ENTRIES holds a single (DICT . DATA) pair for the form XObject and SPEC is
the parsed bundle the assembler needs to renumber its objects.  :width and
:height are given in 96dpi pixels, like the raster paths, so the layout code
scales every image alike."
  (let* ((bundle (org-pdf--svg-bundle path))
         (objects (plist-get bundle :objects))
         (root (plist-get bundle :root))
         (form (cdr (assq root objects)))
         (info (org-pdf--svg-info path))
         ;; The form's BBox is expressed in SVG user units, which are already
         ;; points here; reporting width times 72/96 pixels makes the layout
         ;; code's px->pt conversion land back on the exact point size.
         (w (plist-get bundle :width))
         (h (plist-get bundle :height)))
    (when (and bundle form info)
      (list :width (* w (/ 96.0 72.0))
            :height (* h (/ 96.0 72.0))
            :svg-spec bundle
            :root root
            :entries (list (cons (concat " " (car form)) (cdr form)))))))

(provide 'org-pdf-image)

;;; org-pdf-image.el ends here
