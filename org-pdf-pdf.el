;;; org-pdf-pdf.el --- Minimal dependency-free PDF writer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Free Software Foundation, Inc.

;; Author: org-pdf contributors
;; Keywords: wp, pdf, org
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; A tiny PDF 1.7 writer, sufficient for org-pdf.  No external programs and
;; no external Emacs libraries are used.  The writer collects numbered
;; indrect objects (which may be reserved before their body is known, so
;; objects can forward-reference each other), then serialises them with a
;; classic cross-reference table and trailer.
;;
;; All output is written as unibyte data so that embedded font programs and
;; page-content streams survive byte-for-byte on every platform.

;;; Code:

(require 'cl-lib)

(cl-defstruct (org-pdf--writer
               (:constructor org-pdf--make-writer ())
               (:copier nil))
  "Mutable state for building one PDF file."
  (objects nil)   ; alist (NUMBER . BODY-STRING), body unibyte
  (next 1))       ; next free object number

(defun org-pdf--w-reserve (w)
  "Reserve and return a fresh object number in writer W."
  (let ((n (org-pdf--writer-next w)))
    (setf (org-pdf--writer-next w) (1+ n))
    n))

(defun org-pdf--w-put (w num body)
  "Attach BODY (a string) to object NUM in writer W."
  (push (cons num body) (org-pdf--writer-objects w))
  num)

(defun org-pdf--w-add (w body)
  "Add a new object with BODY to writer W, returning its number."
  (org-pdf--w-put w (org-pdf--w-reserve w) body))

(defun org-pdf--w-add-stream (w dict-entries data)
  "Add a stream object to W.
DICT-ENTRIES is a string of already-formatted dictionary key/value pairs
(without the surrounding << >>).  DATA is the raw (possibly binary) stream
payload; it is coerced to unibyte before measuring."
  (let* ((bytes (if (multibyte-string-p data)
                    (string-to-unibyte data)
                  data))
         (len (length bytes))
         (dict (concat "<< " dict-entries " /Length " (number-to-string len) " >>\n"))
         (body (concat dict "stream\n" bytes "\nendstream")))
    (org-pdf--w-add w body)))

(defun org-pdf--num (x)
  "Format number X for a PDF content stream or dictionary."
  (let ((s (format "%.2f" (float x))))
    (if (string-match-p "\\." s) s (concat s ".0"))))

(defun org-pdf--join (strings)
  "Concatenate STRINGS into a single unibyte string."
  (let ((buf (generate-new-buffer " *org-pdf-join*")))
    (unwind-protect
        (progn
          (set-buffer-multibyte nil)
          (dolist (s strings) (insert s))
          (buffer-string))
      (kill-buffer buf))))

(defun org-pdf--w-write (w path)
  "Serialise W and write the PDF to PATH (a file name).
Returns PATH."
  (let ((buf (generate-new-buffer " *org-pdf-out*"))
        (offsets (make-hash-table :test #'eql)))
    (unwind-protect
        (with-current-buffer buf
          (set-buffer-multibyte nil)
          (erase-buffer)
          ;; Header: the four high-bit bytes after %PDF are a convention that
          ;; tells tools the file is binary.
          (insert "%PDF-1.7\n%\303\274\303\274\303\274\303\274\n")
          ;; Objects, in ascending number order.
          (dolist (cell (sort (copy-sequence (org-pdf--writer-objects w))
                              (lambda (a b) (< (car a) (car b)))))
            (puthash (car cell) (point) offsets)
            (insert (number-to-string (car cell)) " 0 obj\n")
            (insert (cdr cell))
            (insert "\nendobj\n"))
          ;; Cross-reference table and trailer.
          (let ((xref-pos (point))
                (count (org-pdf--writer-next w)))
            (insert "xref\n"
                    "0 " (number-to-string count) "\n"
                    "0000000000 65535 f \n")
            (dotimes (i (1- count))
              (insert (format "%010d 00000 n \n"
                              (or (gethash (1+ i) offsets) 0))))
            (insert "trailer\n<< /Size " (number-to-string count)
                    " /Root 1 0 R >>\n"
                    "startxref\n" (number-to-string xref-pos) "\n%%EOF\n"))
          (let ((coding-system-for-write 'no-conversion))
            (write-region (point-min) (point-max) path nil 'silent))
          path)
      (kill-buffer buf))))

(provide 'org-pdf-pdf)
;;; org-pdf-pdf.el ends here
