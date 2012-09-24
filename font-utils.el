;;; font-utils.el --- Utility functions for working with fonts
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/font-utils
;; URL: http://raw.github.com/rolandwalker/font-utils/master/font-utils.el
;; Version: 0.6.1
;; Last-Updated: 24 Sep 2012
;; EmacsWiki: FontUtils
;; Package-Requires: ((persistent-soft "0.8.0") (pcache "0.2.3"))
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'font-utils)
;;
;;     (font-utils-exists-p "Courier")
;;
;; Explanation
;;
;; Font-utils is a collection of functions for working with fonts.
;; This library has no user-level interface; it is only useful
;; for programming in Emacs Lisp.
;;
;; The following functions are provided, most of which deal with
;; font names rather than font objects:
;;
;;     `font-utils-exists-p'
;;     `font-utils-first-existing-font'
;;     `font-utils-lenient-name-equal'
;;     `font-utils-list-names'
;;     `font-utils-name-from-xlfd'
;;     `font-utils-read-name'
;;
;; The most generally useful of these is `font-utils-exists-p', which
;; tests whether a font matching the given name is currently available
;; for use.
;;
;; To use font-utils, place the font-utils.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'font-utils)
;;
;; See Also
;;
;;     M-x customize-group RET font-utils RET
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;;     Requires persistent-soft.el
;;
;; Bugs
;;
;;     Checking for font availability is slow on most systems.
;;     Workaround: where supported, font information will be cached
;;     to disk.  See customize for more.
;;
;;     font-utils-exists-p only supports two styles of font
;;     name.  This page
;;
;;         http://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
;;
;;     describes four styles of font name.
;;
;; TODO
;;
;;     Possibly return a font object instead of font-info vector
;;     from font-utils-exists-p.
;;
;;     Test whether (find-font (font-spec :name "Name")) is faster
;;     than font-info.
;;
;;     font-utils-create-fuzzy-matches is not exhaustive enough
;;     to catch many typos.
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

;;; requires

;; for callf, callf2, intersection, remove-if-not
(require 'cl)

(autoload 'persistent-soft-store     "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-fetch     "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store." t)
(autoload 'persistent-soft-exists-p  "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-flush     "persistent-soft" "Flush data for the LOCATION data store to disk."                    t)

;;; customizable variables

;;;###autoload
(defgroup font-utils nil
  "Utility functions for working with fonts."
  :version "0.6.1"
  :link '(emacs-commentary-link "font-utils")
  :prefix "font-utils-"
  :group 'extensions)

(defcustom font-utils-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'font-utils)

(defcustom font-utils-use-persistent-storage "font-utils"
  "Use persistent disk storage when available.

This speeds some operations between sessions.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "font-utils")
          (const :tag "No"   nil))
  :group 'font-utils)

(defcustom font-utils-use-memory-cache (not (or (eq window-system 'x)
                                                (eq window-system 'w32)))
  "Run `font-family-list' at first call of `font-utils-exists-p'.

Take as canonical the list of family names produced.

This is generally a speed benefit.  However, some font names
will later be missed by `font-utils-exists-p', as the font
backend usually has the ability to recognize some alternate
names.

Disabled on X11 and MS Windows by default, because `font-family-list'
often gives truncated results before Emacs is fully initialized.

The MS Windows Emacs port responds to `font-info' requests
quickly, so the cache is less needed, leaving the X11 port as
the pathological case with regard to startup time."
  :type 'boolean
  :group 'font-utils)

;;; variables

;; note: variable outside font-utils- namespace
(defvar font-name-history nil "History of font names entered in the minibuffer.")

(defvar font-utils-all-names nil
  "Hash of all font names.")

(defvar font-utils-list-names-mem nil
  "Memoization data for `font-utils-list-names'.")

(defvar font-utils-exists-p-mem (make-hash-table :test 'equal)
  "Memoization data for `font-utils-exists-p'.")

;;; utility functions

;;;###autoload
(defun font-utils-name-from-xlfd (xlfd)
  "Return the font-family name from XLFD, a string.

This function accounts for the fact that the XLFD
delimiter, \"-\", is a legal character within fields."
  (let ((elts (split-string
               (replace-regexp-in-string
                "\\-\\(semi\\|demi\\|half\\|double\\|ultra\\|extra\\)-" "-\\1_" xlfd) "-")))
    (if (>= (length elts) 15)
        (mapconcat 'identity
                   (nreverse
                    (nthcdr
                     12
                     (nreverse
                      (nthcdr 2 elts)))) "-")
      (nth 2 elts))))

(defun font-utils-create-fuzzy-matches (font-name &optional keep-size)
  "Return a list of approximate matches to FONT-NAME.

If KEEP-SIZE is set, do not strip point sizes in the form

   Font Name-pointsize"
  (let ((case-fold-search  nil)
        (font-name-uncamel nil)
        (fuzzy-match-list (list font-name)))
    (setq font-name (replace-regexp-in-string "\\`[ \t_]+" ""
                       (replace-regexp-in-string "[ \t_]+\\'" ""
                          (replace-regexp-in-string ":[^:]*\\'" ""
                                font-name))))
    (unless keep-size
      (setq font-name (replace-regexp-in-string "[ \t]*-[0-9.]+\\'" "" font-name)))
    (setq font-name-uncamel (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" font-name))
    (push font-name                                                   fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_]+"  ""  font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]"    "_" font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_]+"  " " font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_]+"  "_" font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]"    "-" font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]+"   "-" font-name        ) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_-]+" "-" font-name        ) fuzzy-match-list)
    (push font-name-uncamel                                           fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]"    "_" font-name-uncamel) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_]+"  " " font-name-uncamel) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_]+"  "_" font-name-uncamel) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]"    "-" font-name-uncamel) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t]+"   "-" font-name-uncamel) fuzzy-match-list)
    (push (replace-regexp-in-string "[ \t_-]+" "-" font-name-uncamel) fuzzy-match-list)
    (setq fuzzy-match-list (nreverse fuzzy-match-list))
    (delete-dups fuzzy-match-list)
    (remove-if-not #'(lambda (x) (string-match-p "[^ \t]" x)) fuzzy-match-list)))

;;;###autoload
(defun font-utils-list-names ()
  "Return a list of all font names on the current system."
  (when (display-multi-font-p)
    (if font-utils-list-names-mem
        font-utils-list-names-mem
      (setq font-utils-list-names-mem
            (delete-dups (remove "nil" (remq nil (font-family-list))))))))

(defun font-utils-load-names (&optional progress regenerate)
  "Populate the hash `font-utils-all-names'.

When optional PROGRESS is true, show progress feedback in the
echo area.

When optional REGENERATE is true, always rebuild from
scratch."
  (when (display-multi-font-p)
    (when regenerate
      (setq font-utils-all-names nil)
      (persistent-soft-store (intern (format "checksum-%s" window-system))
                             nil font-utils-use-persistent-storage)
      (persistent-soft-store (intern (format "font-names-%s" window-system))
                             nil font-utils-use-persistent-storage)
      (persistent-soft-flush font-utils-use-persistent-storage))
    (unless (or (hash-table-p font-utils-all-names)
                (not font-utils-use-memory-cache))
      (when progress
        (message "Font cache ... checking"))
      (let* ((old-checksum (persistent-soft-fetch (intern (format "checksum-%s" window-system)) font-utils-use-persistent-storage))
             (listing (font-utils-list-names))
             (new-checksum (md5 (mapconcat 'identity (sort listing 'string<) "") nil nil 'utf-8))
             (dupes nil))
        (when (equal old-checksum new-checksum)
          (setq font-utils-all-names (persistent-soft-fetch
                                          (intern (format "font-names-%s" window-system))
                                          font-utils-use-persistent-storage)))
        (unless (hash-table-p font-utils-all-names)
          (when progress
            (message "Font cache ... rebuilding"))
          (setq font-utils-all-names (make-hash-table :size (* 5 (length listing)) :test 'equal))
          (dolist (font-name listing)
            (dolist (fuzzy-name (font-utils-create-fuzzy-matches font-name))
              (callf upcase fuzzy-name)
              (when (and (gethash fuzzy-name font-utils-all-names)
                         (not (equal (gethash fuzzy-name font-utils-all-names) font-name)))
                (push fuzzy-name dupes))
              (puthash (upcase fuzzy-name) font-name font-utils-all-names)))
          (delete-dups dupes)
          (dolist (fuzzy-name dupes)
            (remhash fuzzy-name font-utils-all-names))
          (persistent-soft-store (intern (format "checksum-%s" window-system))
                                 new-checksum font-utils-use-persistent-storage)
          (persistent-soft-store (intern (format "font-names-%s" window-system))
                                 font-utils-all-names font-utils-use-persistent-storage)
          (persistent-soft-flush font-utils-use-persistent-storage)))
      (when progress
        (message "Font cache ... done")))))

;;;###autoload
(defun font-utils-read-name (&optional ido)
  "Read a font name using `completing-read'.

Underscores are removed from the return value.

Uses `ido-completing-read' if optional IDO is set."
  (save-match-data
    (let ((prompt "Font: ")
          (reader (if ido 'ido-completing-read 'completing-read))
          (font-names (mapcar #'(lambda (x)
                                   (replace-regexp-in-string " " "_" x))
                              (font-utils-list-names))))
      (replace-regexp-in-string "_" " "
         (funcall reader prompt font-names nil nil nil font-name-history)))))

;;;###autoload
(defun font-utils-lenient-name-equal (font-name-a font-name-b)
  "Leniently match two strings, FONT-NAME-A and FONT-NAME-B."
  (setq font-name-a (replace-regexp-in-string ":[^:]*\\'"   "" font-name-a))
  (setq font-name-b (replace-regexp-in-string ":[^:]*\\'"   "" font-name-b))
  (setq font-name-a (replace-regexp-in-string "-[0-9.]+\\'" "" font-name-a))
  (setq font-name-b (replace-regexp-in-string "-[0-9.]+\\'" "" font-name-b))
  (setq font-name-a (replace-regexp-in-string "[ \t_'\"-]+" "" font-name-a))
  (setq font-name-b (replace-regexp-in-string "[ \t_'\"-]+" "" font-name-b))
  (string-equal (downcase font-name-a) (downcase font-name-b)))

;;;###autoload
(defun font-utils-exists-p (font-name &optional point-size strict scope)
  "Test whether FONT-NAME (a string or font object) exists.

FONT-NAME is a string, typically in Fontconfig font-name format.
A font-spec, font-vector, or font-object are accepted, though
the behavior for the latter two is not well defined.

Returns a matching font vector.

When POINT-SIZE is set, check for a specific font size.  Size may
also be given at the end of a string FONT-NAME, eg \"Monaco-12\".

When optional STRICT is given, FONT-NAME must will not be
leniently modified before passing to `font-info'.

Optional SCOPE is a list of font names, within which FONT-NAME
must \(leniently\) match."
  (when (display-multi-font-p)
    (let ((args (list font-name point-size strict scope)))
      (if (gethash args font-utils-exists-p-mem)
          (gethash args font-utils-exists-p-mem)
        ;; else
        (save-match-data
          (when (fontp font-name 'font-spec)
            (when (and (floatp (font-get font-name :size))
                       (not point-size))
              (setq point-size (font-get font-name :size)))
            (setq font-name (or (font-get font-name :name) (font-get font-name :family))))
          (puthash args
                   (cond
                     ((fontp font-name 'font-entity)
                      (font-info font-name))
                     ((vectorp font-name)
                      font-name)
                     (t
                      (let ((font-name-list        nil)
                            (fontconfig-params     ""))

                        ;; read all fonts if possible
                        (font-utils-load-names (not font-utils-less-feedback))

                        ;; clean up name and set point-size.  Priority
                        ;;    argument to function
                        ;;    font-spec property
                        ;;    fontconfig-style parameter
                        ;;    fontconfig-style trailing size
                        (when (string-match "\\(:.*\\)\\'" font-name)
                          (setq fontconfig-params (match-string 1 font-name))
                          (setq font-name (replace-match "" t t font-name))
                          (when (string-match "\\<size=\\([0-9.]+\\)" fontconfig-params)
                            (callf or point-size (string-to-number (match-string 1 fontconfig-params)))
                            (setq fontconfig-params (replace-match "" t t fontconfig-params))))
                        (when (string-match "-\\([0-9.]+\\)\\'" font-name)
                          (callf or point-size (string-to-number (match-string 1 font-name)))
                          (setq font-name (replace-match "" t t font-name)))
                        (when (stringp point-size)
                          (callf string-to-number point-size))
                        (when (numberp point-size)
                          (callf concat fontconfig-params (format ":size=%s" (round point-size))))
                        (setq fontconfig-params (replace-regexp-in-string "::+" ":" fontconfig-params))

                        ;; generate list of font names to try
                        (setq font-name-list (if strict
                                                 (list font-name)
                                               (font-utils-create-fuzzy-matches font-name)))

                        ;; constrain font list to scope requested
                        (when scope
                          (callf2 intersection scope font-name-list :test 'font-utils-lenient-name-equal))

                        ;; constrain font list by font cache if possible
                        (when (and font-utils-use-memory-cache
                                   (hash-table-p font-utils-all-names))
                          (setq font-name-list (remove-if-not #'(lambda (key)
                                                                  (gethash (upcase
                                                                            (replace-regexp-in-string "-[0-9.]+\\'" "" key))
                                                                           font-utils-all-names))
                                                              font-name-list)))
                        ;; find the font
                        (catch 'font
                          (dolist (name font-name-list)
                            (let* ((query-name (concat name fontconfig-params))
                                   (font-vec (with-local-quit (ignore-errors (font-info query-name)))))
                              (when (and font-vec
                                         (or (find-font (font-spec :name name))    ; verify - some systems return the
                                             (find-font (font-spec :family name))) ; default face on font-info failure
                                         (or (not (numberp point-size))
                                             (= point-size (aref font-vec 2))))
                                (throw 'font font-vec))))))))
                   font-utils-exists-p-mem))))))

;;;###autoload
(defun font-utils-first-existing-font (font-names &optional no-normalize)
  "Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked.

If NO-NORMALIZE is given, the return value is exactly as the
member of FONT-NAMES.  Otherwise, the family name is extracted
from the XLFD returned by `font-info'."
  (when (display-multi-font-p)
    (catch 'name
      (dolist (name font-names)
        (let ((font (font-utils-exists-p name)))
          (when font
            (when no-normalize
              (throw 'name name))
            (cond
              ((vectorp font)
               (throw 'name (font-utils-name-from-xlfd (aref font 0))))
              ((fontp font)
               (throw 'name (or (font-get font :name) (font-get font :family))))
              (t
               (error "Unknown type")))))))))

(provide 'font-utils)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;
;; LocalWords: FontUtils ARGS alist utils pcache XLFD demi fontconfig callf
;;

;;; font-utils.el ends here
