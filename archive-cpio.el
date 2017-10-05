;;; archive-cpio.el --- CPIO support for archive-mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Stefan Monnier
;; Copyright (C) 2017  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: files

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

;; This module adds support for CPIO archives to archive-mode.

;; The format supported is the "New ASCII Format" as documented in
;; http://people.freebsd.org/~kientzle/libarchive/man/cpio.5.txt.

;; Based on cpio-mode.el by Stefan Monnier, posted here:
;; https://lists.gnu.org/archive/html/emacs-devel/2015-04/msg00824.html

;;; Code:

(defconst archive-cpio-entry-header-re
  "07070[12]\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)\\([[:xdigit:]]\\{8\\}\\)[[:xdigit:]]\\{8\\}\\|\0+\\'"
  "Regular expression matching a CPIO entry.
The matched groups are:

1. ino
2. mode
3. uid
4. gid
5. nlink
6. mtime
7. filesize
8. devmajor
9. devminor
10. rdevmajor
11. rdevminor
12. namesize

The name starts at the end of the match, and goes on for namesize
bytes.  It is padded with NUL bytes so that the start of file
data is aligned to four bytes.  File data is also padded, so that
the next header is aligned to four bytes.")

;;;###autoload
(defun archive-cpio-find-type ()
  "Return `cpio' if the current buffer contains a CPIO archive.
Otherwise, return nil.

This function is meant to be used as a :before-until advice for
`archive-find-type'."
  (widen)
  (goto-char (point-min))
  (when (looking-at archive-cpio-entry-header-re)
    'cpio))

;;;###autoload
(with-eval-after-load "arc-mode"
  (advice-add 'archive-find-type :before-until 'archive-cpio-find-type))

(defun archive-cpio--parse-mode (mode)
  "Parse MODE, an integer, and return a permissions string (10 characters)."
  (string
   (case (logand #o170000 mode)
     (#o140000 ?s)
     (#o120000 ?l)
     (#o100000 ?-)
     (#o060000 ?b)
     (#o040000 ?d)
     (#o020000 ?c)
     (#o010000 ?p)
     (t (error "Unknown mode %S" mode)))
   (if (zerop (logand #o400 mode)) ?- ?r)
   (if (zerop (logand #o200 mode)) ?- ?w)
   (if (zerop (logand #o100 mode))
       (if (zerop (logand #o4000 mode)) ?- ?S)
     (if (zerop (logand #o4000 mode)) ?x ?s))
   (if (zerop (logand #o040 mode)) ?- ?r)
   (if (zerop (logand #o020 mode)) ?- ?w)
   (if (zerop (logand #o010 mode))
       (if (zerop (logand #o2000 mode)) ?- ?S)
     (if (zerop (logand #o2000 mode)) ?x ?s))
   (if (zerop (logand #o004 mode)) ?- ?r)
   (if (zerop (logand #o002 mode)) ?- ?w)
   (if (zerop (logand #o001 mode)) ?- ?x)))

(defun archive-cpio-summarize (&optional archive-buffer)
  "Summarize files in a cpio archive.
Insert file list in ARCHIVE-BUFFER, or the current buffer if
ARCHIVE-BUFFER is nil."
  (let (visual files)
    (goto-char (point-min))
    (while (not (eobp))
      (assert (zerop (mod (- (point) (point-min)) 4)))
      (cond
       ((not (looking-at archive-cpio-entry-header-re))
        (error "Unrecognized cpio header format"))
       ((not (match-beginning 1))
        ;; Reached the trailing padding, just skip it.
        ;; (put-text-property (point) (point-max) 'invisible t)
        (goto-char (match-end 0)))
       (t
        (let* ((ino (string-to-number (match-string 1) 16))
               (mode (string-to-number (match-string 2) 16))
               (uid (string-to-number (match-string 3) 16))
               (gid (string-to-number (match-string 4) 16))
               ;; (nlink (string-to-number (match-string 5) 16))
               ;; (mtime (string-to-number (match-string 6) 16))
               (filesize (string-to-number (match-string 7) 16))
               ;; (devmajor (string-to-number (match-string 8) 16))
               ;; (devminor (string-to-number (match-string 9) 16))
               ;; (rdevmajor (string-to-number (match-string 10) 16))
               ;; (rdevminor (string-to-number (match-string 11) 16))
               (namesize (string-to-number (match-string 12) 16))
               (namebeg (match-end 0))
               (name (buffer-substring namebeg (+ namebeg namesize -1)))
               (filebeg (+ (match-end 0) 2 (* (/ (+ namesize 1) 4) 4)))
               (next (+ filebeg (* (/ (+ filesize 3) 4) 4))))
          (if (and (zerop ino) (zerop mode) (zerop filesize)
                   (equal name "TRAILER!!!"))
              ;; Last entry in archive: go to end
              (goto-char (point-max))
            ;; Building this in two parts, since we need to know in
            ;; which column the file name starts.
            (let* ((text-a
                    (format "  %s %8.0f %10d/%-10d "
                            (archive-cpio--parse-mode mode)
                            filesize
                            uid
                            gid))
                   (text-b
                    (format "%s%s"
                            name
                            (if (= (logand #o170000 mode) #o120000) ;Symlink
                                (concat " -> " (buffer-substring filebeg (+ filebeg filesize)))
                              "")))
                   (text (concat text-a text-b)))
              (push (vector text (length text-a) (length text)) visual)
              (push (vector name name nil mode filebeg) files)))
          (goto-char next)))))
    (with-current-buffer (or archive-buffer (current-buffer))
      (goto-char (point-min))
      (insert "M   Filemode   Length        UID/GID        File\n")
      (insert "- ---------- -------- ---------- ---------- -----\n")
      (archive-summarize-files (nreverse visual))
      (apply #'vector (nreverse files)))))

(defun archive-cpio-extract (archive name)
  "Open the CPIO file ARCHIVE and extract NAME into the current buffer.
This function is meant to be called from `archive-extract'."
  ;; Based on archive-ar-extract
  (let ((destbuf (current-buffer))
        (archivebuf (find-file-noselect archive)))
    (archive-cpio-extract-from-buffer name archivebuf destbuf)))

(defun archive-cpio-extract-from-buffer (name archivebuf destbuf)
  "Extract the file named NAME from the archive in ARCHIVEBUF.
Insert the file contents into DESTBUF."
  (with-current-buffer archivebuf
    (let ((from nil) size)
      (save-restriction
        ;; We may be in archive-mode or not, so either with or without
        ;; narrowing and with or without a prepended summary.
        (save-excursion
          (widen)
          (search-forward-regexp archive-cpio-entry-header-re)
          (goto-char (match-beginning 0))
          (while (and (not from) (looking-at archive-cpio-entry-header-re))
            (let* ((namesize (string-to-number (match-string 12) 16))
                   (filesize (string-to-number (match-string 7) 16))
                   (namebeg (match-end 0))
                   (entry-name (buffer-substring namebeg (+ namebeg namesize -1)))
                   (filebeg (+ (match-end 0) 2 (* (/ (+ namesize 1) 4) 4)))
                   (next (+ filebeg (* (/ (+ filesize 3) 4) 4))))
              (if (equal entry-name name)
                  (setq from filebeg
                        size filesize)
                ;; Move to the end of the data.
                (goto-char next)))))
        (when from
          (set-buffer-multibyte nil)
          (with-current-buffer destbuf
            ;; Do it within the `widen'.
            (insert-buffer-substring archivebuf from (+ from size)))
          (set-buffer-multibyte 'to)
          ;; Inform the caller that the call succeeded.
          t)))))

(provide 'archive-cpio)
;;; archive-cpio.el ends here
