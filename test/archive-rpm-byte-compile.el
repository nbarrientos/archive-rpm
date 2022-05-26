(require 'find-lisp)

;; The byte-compiler throws warnings on <28 because the structs
;; declared below do not exist. Define them to mask the "false
;; positive".
(when (< emacs-major-version 28)
  (cl-defstruct (archive--file-summary
                 (:constructor nil)
                 (:constructor archive--file-summary (text name-start name-end)))
    text name-start name-end)

  (cl-defstruct (archive--file-desc
                 (:constructor nil)
                 (:constructor archive--file-desc
                               ;; ext-file-name and int-file-name are usually `eq'
                               ;; except when int-file-name is the downcased
                               ;; ext-file-name.
                               (ext-file-name int-file-name mode size time
                               &key pos ratio uid gid)))
    ext-file-name int-file-name
    (mode nil  :type integer)
    (size nil  :type integer)
    (time nil  :type string)
    (ratio nil :type string)
    uid gid
    pos))

;; We skip 26 and older due to a warning when using
;; `string-to-unibyte'. In 26 the byte compiler throws a deprecation
;; warning however 27 onwards reverted this.
;; From NEWS.27:
;;   ** The functions 'string-to-unibyte' and 'string-to-multibyte' are no
;;   longer declared obsolete.  We have found that there are legitimate use
;;   cases for these functions, where there's no better alternative.  We
;;   believe that the incorrect uses of these functions all but disappeared
;;   by now, so we are un-obsoleting them.
(when (>= emacs-major-version 27)
  (let ((files (find-lisp-find-files-internal
                ".."
                (lambda (file dir)
                  (and (not (file-directory-p (expand-file-name file dir)))
                       (not (string-match "pkg\\.el$" file))
                       (string-match "\\.el$" file)
                       (not (string-match "\\.dir-locals\\.el" file))))
                (lambda (dir parent)
                  (not (or (string= dir ".")
                           (string= dir "..")
                           (string= dir ".git")
                           (string= dir "test")))))))
    (dolist (file files)
      (byte-compile-file file))))
