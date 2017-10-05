;;; archive-rpm.el --- RPM support for archive-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: files
;; Package-Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module adds support for RPM archives to archive-mode.

;; RPM files consist of metadata plus a compressed CPIO archive, so
;; this module relies on `archive-cpio'.

;;; Code:

(require 'archive-cpio)
(require 'bindat)

;;;###autoload
(defun archive-rpm-find-type ()
  "Return `rpm' if the current buffer contains an RPM archive.
Otherwise, return nil.

This function is meant to be used as a :before-until advice for
`archive-find-type'."
  (widen)
  (goto-char (point-min))
  ;; Look for RPM "lead" magic number, and version number 3.0
  (when (looking-at "\xed\xab\xee\xdb\x03\x00")
    'rpm))

;;;###autoload
(with-eval-after-load "arc-mode"
  (advice-add 'archive-find-type :before-until 'archive-rpm-find-type))

;;;###autoload
(add-to-list 'magic-mode-alist '("\xed\xab\xee\xdb\x03\x00" . archive-mode))

(defconst archive-rpm--header-bindat-spec
  '((:magic              u24)
    (:version            u8)
    (:reserved           u32)
    (:n-index-entries    u32)
    (:data-len           u32))
  "Bindat spec for RPM header.

As per <http://ftp.rpm.org/max-rpm/s1-rpm-file-format-rpm-file-format.html>:

The header structure header always starts with a three-byte magic
number: 8e ad e8. Following this is a one-byte version
number.  Next are four bytes that are reserved for future
expansion.  After the reserved bytes, there is a four-byte number
that indicates how many index entries exist in this header
structure, followed by another four-byte number indicating how
many bytes of data are part of the header structure.")

(defconst archive-rpm--index-entry-bindat-spec
  '((:tag        u32)
    (:type       u32)
    (:offset     u32)
    (:count      u32))
  "Bindat spec for index entry in RPM header.")

(defun archive-rpm-summarize ()
  "Summarize files in an RPM archive.
This function is meant to be called by `archive-summarize'."
  ;; Skip past "lead"
  (goto-char (+ 96 (point-min)))
  ;; Skip past the first header, the "signature", which is not
  ;; particularly interesting.
  (archive-rpm--parse-header t)
  ;; The second header contains data we're interested in.
  (let* ((header-entries (archive-rpm--parse-header nil))
         (payload (buffer-substring (point) (point-max)))
         (archive-buffer (current-buffer)))
    ;; Let's insert some interesting information, but in a way that it
    ;; won't get clobbered...
    (save-restriction
      (goto-char (point-min))
      (archive-rpm--insert-interesting-information header-entries)
      (narrow-to-region (point) (point-max))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (archive-rpm--decompress-payload payload header-entries)
        ;; Finally ready to do something with the cpio archive
        (archive-cpio-summarize archive-buffer)))))

(defun archive-rpm-extract (archive name)
  "Extract, from the RPM file ARCHIVE, the file named NAME."
  ;; Based on archive-ar-extract
  (let ((destbuf (current-buffer))
        (archivebuf (find-file-noselect archive)))
    (with-current-buffer archivebuf
      (save-restriction
        ;; We may be in archive-mode or not, so either with or without
        ;; narrowing and with or without a prepended summary.
        (save-excursion
          (widen)
          ;; Find and skip past "lead"
          (search-forward "\xed\xab\xee\xdb\x03\x00")
          (goto-char (+ 96 (match-beginning 0)))
          (archive-rpm--parse-header t)
          (let* ((header-entries (archive-rpm--parse-header nil))
                 (payload (buffer-substring (point) (point-max))))
            (with-temp-buffer
              (archive-rpm--decompress-payload payload header-entries)
              (goto-char (point-min))
              (archive-cpio-extract-from-buffer name (current-buffer) destbuf))))))))

(defun archive-rpm--decompress-payload (payload header-entries)
  "Decompress PAYLOAD, given the information in HEADER-ENTRIES.
Insert the decompressed data into the current buffer, which is
assumed to be empty."
  (let ((payload-format (archive-rpm--get-header-data 1124 header-entries))
        (payload-compressor (archive-rpm--get-header-data 1125 header-entries)))
    (unless (equal "cpio" payload-format)
      (error "RPM payload is in `%s', not cpio format" payload-format))
    (set-buffer-multibyte nil)
    (cond
     ((equal "gzip" payload-compressor)
      (insert payload)
      (unless (zlib-decompress-region (point-min) (point-max))
        (error "Zlib decompression failed")))
     ;; Not sure why it's either "xz" or "lzma", but xz seems to understand both
     ((member payload-compressor '("xz" "lzma"))
      (insert payload)
      (let* ((coding-system-for-write 'no-conversion)
             (coding-system-for-read 'no-conversion)
             (exit-code (call-process-region
                         (point-min) (point-max)
                         "xz" t t nil "-q" "-c" "-d")))
        (unless (zerop exit-code)
          (error "xz decompression failed: %s" (buffer-string)))))
     (t
      (error "Unknown RPM payload compressor `%s'" payload-compressor)))))

(defconst archive-rpm--interesting-fields
  '((1000 . "Name")
    (1001 . "Version")
    (1002 . "Release")
    (1004 . "Summary")
    (1010 . "Distribution")
    (1011 . "Vendor")
    (1014 . "License")
    (1016 . "Group")
    (1020 . "URL")
    (1021 . "OS")
    (1022 . "Architecture")
    (1124 . "Format")
    (1125 . "Compression"))
  "Fields to output at top of RPM archive buffer.")

(defun archive-rpm--insert-interesting-information (header-entries)
  "Insert relevant and human-readable parts of HEADER-ENTRIES."
  (let ((max-field-name-len (apply 'max
                                   (mapcar (lambda (entry) (length (cdr entry)))
                                           archive-rpm--interesting-fields))))
    (dolist (field archive-rpm--interesting-fields)
      (let ((data (archive-rpm--get-header-data (car field) header-entries)))
        (when (stringp data)
          (insert (cdr field)
                  ":"
                  (make-string (+ 1 (- max-field-name-len (length (cdr field)))) ?\s)
                  data
                  "\n"))))
    (insert "\n")))

(defun archive-rpm--get-header-data (tag header-entries)
  "Return data from the header with tag TAG from HEADER-ENTRIES."
  (bindat-get-field
   (cl-find tag header-entries
            :key (lambda (e) (bindat-get-field e :tag)))
   :data))

(defun archive-rpm--find-index-entry-data (index-entry data-starts-at data-len)
  "Extract data for INDEX-ENTRY from RPM header data store.
DATA-STARTS-AT is the position where the data store starts.
DATA-LEN is the length of the data store."
  (let ((entry-data-starts-at (+ data-starts-at (bindat-get-field index-entry :offset))))
    (cl-case (bindat-get-field index-entry :type)
      (6
       ;; STRING (null-terminated)
       (save-excursion
         (goto-char entry-data-starts-at)
         (when (search-forward "\0" (+ data-starts-at data-len) t)
           (cons
            (cons :data (buffer-substring entry-data-starts-at (match-beginning 0)))
            index-entry))))
      (otherwise
       ;; Any other type.  Not parsing it, not adding :data.
       index-entry))))

(defun archive-rpm--parse-header (align-after)
  "Parse RPM header, and return list of index entries.
If ALIGN-AFTER is non-nil, leave point on an 8-byte alignment afterwards."
  (unless (looking-at "\x8e\xad\xe8")
    (error "Incorrect header magic"))
  (let* ((header (bindat-unpack archive-rpm--header-bindat-spec
                                (string-to-unibyte
                                 (buffer-substring (point) (+ 16 (point))))))
         (n-index-entries (bindat-get-field header :n-index-entries))
         (data-len (bindat-get-field header :data-len))
         (data-starts-at (+ (point) 16 (* 16 n-index-entries)))
         index-entries)
    ;; Move past header
    (forward-char 16)
    ;; Parse each index entry
    (dotimes (_n n-index-entries)
      (let ((index-entry (bindat-unpack archive-rpm--index-entry-bindat-spec
                                        (string-to-unibyte
                                         (buffer-substring (point) (+ 16 (point)))))))
        (setq index-entry (archive-rpm--find-index-entry-data
                           index-entry data-starts-at data-len))
        (push index-entry index-entries)
        (forward-char 16)))
    ;; Skip past data store
    (forward-char data-len)
    ;; If we're preparing to read a second header, check alignment
    (when align-after
      (unless (zerop (% data-len 8))
        (forward-char (- 8 (% data-len 8)))))
    ;; Return list of index entries
    index-entries))

(provide 'archive-rpm)
;;; archive-rpm.el ends here
