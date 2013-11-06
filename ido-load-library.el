;;; ido-load-library.el --- Load-library alternative using ido-completing-read
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/ido-load-library
;; URL: http://raw.github.com/rolandwalker/ido-load-library/master/ido-load-library.el
;; Version: 0.2.0
;; Last-Updated: 28 Oct 2013
;; EmacsWiki: IdoLoadLibrary
;; Package-Requires: ((persistent-soft "0.8.8") (pcache "0.2.3"))
;; Keywords: maint, completion
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'ido-load-library)
;;     M-x ido-load-library RET
;;
;; Explanation
;;
;; Ido-load-library is an alternative to `load-library' which
;; uses `ido-completing-read' for completion against all
;; available libraries in your `load-path'.
;;
;; To use ido-load-library, place the ido-load-library.el file
;; somewhere Emacs can find it, and add the following to your
;; ~/.emacs file:
;;
;;     (require 'ido-load-library)
;;
;; The interactive command `ido-load-library' is provided, though
;; not bound to any key.  It can be executed via
;;
;;     M-x ido-load-library
;;
;; or bound via something like
;;
;;     (define-key global-map (kbd "C-c l") 'ido-load-library)
;;
;; or safely aliased to `load-library'
;;
;;     (defalias 'load-library 'ido-load-library)
;;
;; The interactive command `ido-load-library-find' is also
;; provided.  Like `ido-load-library', it searches your
;; `load-path', but instead of loading the selected library,
;; it visits the file in a buffer.
;;
;; See Also
;;
;;     M-x customize-group RET ido-load-library RET
;;     M-x customize-group RET ido RET
;;
;; Notes
;;
;;     Defines the variable `library-name-history' outside of the
;;     `ido-load-library-' namespace.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Uses if present: persistent-soft.el (Recommended)
;;
;; Bugs
;;
;;     When invalidating the disk cache, `ido-load-library' only checks
;;     whether `load-path' has changed, not whether new files were added
;;     to existing paths.  Workarounds:
;;
;;         1. Install libraries using ELPA/package.el, in which case this
;;            assumption always works.
;;         2. Wait for the cache to expire (7 days).
;;         3. Give universal prefix argument to `ido-load-library'
;;            to force invalidation of the cache.
;;
;;     Should not remove -autoloads and -pkg library names unless there
;;     is another name found with the same base.
;;
;; TODO
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requirements

;; for callf, incf, assert, remove-if, remove-if-not
(require 'cl)

(autoload 'persistent-soft-store             "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."    )
(autoload 'persistent-soft-fetch             "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store."  )
(autoload 'persistent-soft-exists-p          "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."    )
(autoload 'persistent-soft-flush             "persistent-soft" "Flush data for the LOCATION data store to disk."                     )
(autoload 'persistent-soft-location-readable "persistent-soft" "Return non-nil if LOCATION is a readable persistent-soft data store.")
(autoload 'persistent-soft-location-destroy  "persistent-soft" "Destroy LOCATION (a persistent-soft data store)."                    )
(autoload 'thing-at-point                    "thingatpt"       "Return the THING at point."                                       nil)

;;; customizable variables

;;;###autoload
(defgroup ido-load-library nil
  "Load-library alternative using `ido-completing-read'."
  :version "0.2.0"
  :link '(emacs-commentary-link :tag "Commentary" "ido-load-library")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/ido-load-library")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/IdoLoadLibrary")
  :prefix "ido-load-library-"
  :group 'abbreviations
  :group 'convenience)

(defcustom ido-load-library-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'ido-load-library)

(defcustom ido-load-library-use-persistent-storage "ido-load-library"
  "Use persistent disk storage when available.

This speeds some operations between sessions.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "ido-load-library")
          (const :tag "No"   nil))
  :group 'ido-load-library)

(defcustom ido-load-library-persistent-storage-expiration-days 7
  "Number of days to keep on-disk library-name completion cache."
  :type 'integer
  :group 'ido-load-library)

;;; variables

(defvar ido-load-library-suffix-regexp       (concat (regexp-opt (get-load-suffixes) 'paren) "\\'")
                                                       "Regexp matching `get-load-suffixes'.")
(defvar ido-load-library-all-library-names   nil       "All library names available to `load-library'.")
(defvar ido-load-library-all-library-paths   nil       "All library paths available to `load-library'.")
(defvar ido-load-library-load-path-saved     load-path "A saved copy of `load-path' used to detect changes.")

;; note: outside of ido-load-library- namespace
(defvar library-name-history nil "History of library names entered in the minibuffer.")

;;; compatibility functions

(unless (fboundp 'string-match-p)
  ;; added in 23.x
  (defun string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(defun persistent-softest-store (symbol value location &optional expiration)
  "Call `persistent-soft-store' but don't fail when library not present."
  (ignore-errors (persistent-soft-store symbol value location expiration)))
(defun persistent-softest-fetch (symbol location)
  "Call `persistent-soft-fetch' but don't fail when library not present."
  (ignore-errors (persistent-soft-fetch symbol location)))
(defun persistent-softest-exists-p (symbol location)
  "Call `persistent-soft-exists-p' but don't fail when library not present."
  (ignore-errors (persistent-soft-exists-p symbol location)))
(defun persistent-softest-flush (location)
  "Call `persistent-soft-flush' but don't fail when library not present."
  (ignore-errors (persistent-soft-flush location)))
(defun persistent-softest-location-readable (location)
  "Call `persistent-soft-location-readable' but don't fail when library not present."
  (ignore-errors (persistent-soft-location-readable location)))
(defun persistent-softest-location-destroy (location)
  "Call `persistent-soft-location-destroy' but don't fail when library not present."
  (ignore-errors (persistent-soft-location-destroy location)))

;;; utility functions

(defun ido-load-library-all-library-names (&optional progress regenerate)
  "All library names available to load.

Names are cached in the list `ido-load-library-all-library-names'.

With optional PROGRESS, report progress building cache.

With optional REGENERATE, force rebuilding the cache."
  (when (not (equal load-path ido-load-library-load-path-saved))
    (setq regenerate t))
  (when (and ido-load-library-use-persistent-storage
             (not (stringp (persistent-softest-fetch 'ido-load-library-data-version ido-load-library-use-persistent-storage))))
    (setq regenerate t))
  (when (and ido-load-library-use-persistent-storage
             (stringp (persistent-softest-fetch 'ido-load-library-data-version ido-load-library-use-persistent-storage))
             (version<
              (persistent-softest-fetch 'ido-load-library-data-version ido-load-library-use-persistent-storage)
              (get 'ido-load-library 'custom-version)))
    (setq regenerate t))
  (when regenerate
    (setq ido-load-library-all-library-names nil)
    (persistent-softest-store 'ido-load-library-all-library-names nil
                              ido-load-library-use-persistent-storage
                              (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days)))
    (persistent-softest-store 'ido-load-library-all-library-paths nil
                              ido-load-library-use-persistent-storage
                              (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days))))
  (cond
    (ido-load-library-all-library-names
     t)
    ((and (not regenerate)
          (persistent-softest-exists-p 'ido-load-library-all-library-names ido-load-library-use-persistent-storage)
          (equal load-path (persistent-softest-fetch 'ido-load-library-load-path-saved ido-load-library-use-persistent-storage))
          (consp (setq ido-load-library-all-library-names (persistent-softest-fetch 'ido-load-library-all-library-names ido-load-library-use-persistent-storage)))
          (consp (setq ido-load-library-all-library-paths (persistent-softest-fetch 'ido-load-library-all-library-paths ido-load-library-use-persistent-storage))))
     (setq ido-load-library-load-path-saved load-path))
    (t
     (let ((reporter (when progress (make-progress-reporter "Caching library names..." 0 (length load-path))))
           (counter 0)
           (names nil))
       (setq ido-load-library-suffix-regexp (concat (regexp-opt (get-load-suffixes) 'paren) "\\'"))
       (dolist (elt load-path)
         (when progress
           (progress-reporter-update reporter (incf counter)))
         (setq names
              (remove-if #'(lambda (x) (string-match-p "-\\(autoloads\\|pkg\\)\\'" x))
                         (mapcar #'(lambda (x)
                                     (replace-regexp-in-string ido-load-library-suffix-regexp "" x))
                                 (locate-file-completion-table (list elt) nil ""
                                                               #'(lambda (x)
                                                                   (and (string-match-p ido-load-library-suffix-regexp x)
                                                                        (not (string-match-p "\\`[.#]" x))))
                                                               t))))
         (setq ido-load-library-all-library-names (append ido-load-library-all-library-names names))
         (setq ido-load-library-all-library-paths (append ido-load-library-all-library-paths
                                                          (mapcar #'(lambda (x)
                                                                      (expand-file-name x elt)) names))))
       (callf sort ido-load-library-all-library-names 'string<)
       (delete-dups ido-load-library-all-library-names)
       (callf sort ido-load-library-all-library-paths 'string<)
       (delete-dups ido-load-library-all-library-paths)

       (setq ido-load-library-load-path-saved load-path)
       (let ((persistent-soft-inhibit-sanity-checks t))
         (persistent-softest-store 'ido-load-library-all-library-names ido-load-library-all-library-names
                                   ido-load-library-use-persistent-storage
                                   (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days)))
         (persistent-softest-store 'ido-load-library-all-library-paths ido-load-library-all-library-paths
                                   ido-load-library-use-persistent-storage
                                   (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days)))
         (persistent-softest-store 'ido-load-library-load-path-saved   ido-load-library-load-path-saved
                                   ido-load-library-use-persistent-storage
                                   (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days)))
         (persistent-softest-store 'ido-load-library-data-version (get 'ido-load-library 'custom-version)
                                   ido-load-library-use-persistent-storage
                                   (round (* 60 60 24 ido-load-library-persistent-storage-expiration-days))))
       (persistent-softest-flush ido-load-library-use-persistent-storage)
       (when progress
         (progress-reporter-done reporter)))))
  ido-load-library-all-library-names)

;;; interactive commands

;;;###autoload
(defun ido-load-library (library &optional regenerate)
  "Load the Emacs Lisp library named LIBRARY.

This is identical to `load-library' except that is uses
`ido-completing-read' and a specialized history.

To set REGENERATE and reload the cache of library names, use a
universal prefix argument."
  (interactive
   (list (ido-completing-read "Load library: "
                          (ido-load-library-all-library-names
                           (not ido-load-library-less-feedback)
                           (consp current-prefix-arg))
                          nil
                          nil
                          nil
                          library-name-history)
         (consp current-prefix-arg)))
  (load library))

;;;###autoload
(defun ido-load-library-find (file &optional regenerate)
  "Open the Emacs Lisp library named FILE for editing.

Uses `ido-completing-read' to find any library on `load-path' for
visiting in a buffer.  Text around the point will be used for the
default value, making this something of an alternative to
`find-file-at-point'.

To set REGENERATE and reload the cache of library names, use a
universal prefix argument."
  (interactive
   (list (ido-completing-read "Find library: "
                          (progn
                            (ido-load-library-all-library-names
                             (not ido-load-library-less-feedback)
                             (consp current-prefix-arg))
                            ido-load-library-all-library-paths)
                          nil
                          nil
                          (substring-no-properties (or (thing-at-point 'filename) ""))
                          nil) ; todo a history variable
         (consp current-prefix-arg)))
  (setq file (car
              (remove-if 'file-directory-p
                 (remove-if-not 'file-readable-p
                    (remove-if-not 'file-exists-p
                       (mapcar #'(lambda (ext)
                                   (concat file ext)) '("" ".el" ".el.gz")))))))
  (assert file nil "File is not readable")
  (find-file file))

(provide 'ido-load-library)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: IdoLoadLibrary
;;

;;; ido-load-library.el ends here
