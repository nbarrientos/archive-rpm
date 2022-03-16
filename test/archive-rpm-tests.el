;;; archive-rpm-tests.el --- Tests for archive-rpm   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nacho Barrientos

;; Author: Nacho Barrientos <nacho.barrientos@cern.ch>
;; URL: https://github.com/nbarrientos/archive-rpm
;; Package-Requires: ((emacs "24.1"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; These are tests for archive-rpm.el

;;; Code:

(require 'ert)

(require 'archive-rpm)

(defun archive-rpm-tests--find-regexp-in-buffer (regexp)
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp
     regexp
     nil
     t)))

(defun archive-rpm-tests--find-field-in-buffer (field value)
  (archive-rpm-tests--find-regexp-in-buffer (format "^%s:[ ]*%s$" field value)))

(defmacro archive-rpm-tests--with-rpm (rpm &rest body)
  `(let ((buffer (find-file-noselect ,rpm)))
     (with-current-buffer buffer
       ,@body)))

(ert-deftest archive-rpm-tests--expected-buffer-mode ()
  "Test that the buffer has archive-mode enabled."
  (archive-rpm-tests--with-rpm
   "RPMS/gz/package-1-1.noarch.rpm"
   (should (equal 'archive-mode major-mode))))

(ert-deftest archive-rpm-tests--expected-buffer-size ()
  "Test that the buffer contains all the expected lines."
  (archive-rpm-tests--with-rpm
   "RPMS/gz/package-1-1.noarch.rpm"
   (should (equal 15 (count-lines (point-min) (point-max))))))

(ert-deftest archive-rpm-tests--expected-number-of-files ()
  "Test that the number of files listed is correct."
  (archive-rpm-tests--with-rpm
   "RPMS/gz/package-1-1.noarch.rpm"
   (should (equal 2 (count-lines archive-file-list-start archive-file-list-end)))))

(ert-deftest archive-rpm-tests--expected-fields ()
  "Test that all the fields that are RPM metadata are correctly displayed."
  (archive-rpm-tests--with-rpm
   "RPMS/gz/package-1-1.noarch.rpm"
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Name" "package"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Version" "1"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Release" "1"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Architecture" "noarch"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Format" "cpio"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Vendor" "ACME Inc\\."))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "URL" "https://example\\.org"))))
   (should (not (null (archive-rpm-tests--find-field-in-buffer "License" "BSD"))))))

(ert-deftest archive-rpm-tests--expected-compression-gzip ()
  "Test that RPMs compressed using gzip can be visited."
  (archive-rpm-tests--with-rpm
   "RPMS/gz/package-1-1.noarch.rpm"
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Compression" "gzip"))))
   (should (not (null (archive-rpm-tests--find-regexp-in-buffer "\\./usr/share/doc/package-1/README\\.md$"))))))

(ert-deftest archive-rpm-tests--expected-compression-xz ()
  "Test that RPMs compressed using xz can be visited."
  (archive-rpm-tests--with-rpm
   "RPMS/xz/package-1-1.noarch.rpm"
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Compression" "xz"))))
   (should (not (null (archive-rpm-tests--find-regexp-in-buffer "\\./usr/share/doc/package-1/README\\.md$"))))))

(ert-deftest archive-rpm-tests--expected-compression-zstd ()
  "Test that RPMs compressed using zstd can be visited."
  (archive-rpm-tests--with-rpm
   "RPMS/zstd/package-1-1.noarch.rpm"
   (should (not (null (archive-rpm-tests--find-field-in-buffer "Compression" "zstd"))))
   (should (not (null (archive-rpm-tests--find-regexp-in-buffer "\\./usr/share/doc/package-1/README\\.md$"))))))

(provide 'archive-rpm-tests)
;;; archive-rpm-tests.el ends here
