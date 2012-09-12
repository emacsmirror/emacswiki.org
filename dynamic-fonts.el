;;; dynamic-fonts.el --- Set faces based on available fonts
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/dynamic-fonts
;; URL: http://raw.github.com/rolandwalker/dynamic-fonts/master/dynamic-fonts.el
;; Version: 0.6.0
;; Last-Updated: 7 Sep 2012
;; EmacsWiki: DynamicFonts
;; Keywords: faces, frames
;; Package-Requires: ((persistent-soft "0.8.0") (pcache "0.2.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'dynamic-fonts)
;;
;;     (dynamic-fonts-setup)     ; finds "best" fonts and sets faces:
;;                               ; default, fixed-pitch, variable-pitch
;;
;; Explanation
;;
;; Dynamic-fonts.el makes font configuration more portable between
;; machines.  When Emacs is starting up, dynamic-fonts chooses fonts
;; for your basic faces based on which fonts are actually available
;; on your system.
;;
;; You may set a list of fonts in order of preference using customize.
;;
;; See Also
;;
;;     M-x customize-group RET dynamic-fonts RET
;;
;; Notes
;;
;; The secondary purpose of this library is to provide some font
;; utility functions to be called from Lisp.  See in particular
;; `dynamic-fonts-font-exists-p', which tests font availability.
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;;     Requires persistent-soft.el
;;
;;     Uses if present: memoize.el
;;
;; Bugs
;;
;;     Checking for font availability is slow on most systems.  This
;;     library can add up to several seconds to startup time.  Workaround:
;;     where supported, font information can be cached to disk.
;;
;;     dynamic-fonts-font-exists-p only supports two styles of font
;;     name. This page
;;
;;         http://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
;;
;;     describes four styles of font name.
;;
;; TODO
;;
;;     test whether (find-font (font-spec :name "Name")) is faster
;;     than font-info
;;
;;     dynamic-fonts-create-fuzzy-matches not exhaustive enough to
;;     catch all typos
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

;; for callf, callf2, intersection, remove-if-not, let*
(eval-when-compile
  (require 'cl))

(require 'memoize nil t)

(autoload 'persistent-soft-store     "persistent-soft" "Under SYMBOL, store VALUE in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-fetch     "persistent-soft" "Return the value for SYMBOL in the LOCATION persistent data store." t)
(autoload 'persistent-soft-exists-p  "persistent-soft" "Return t if SYMBOL exists in the LOCATION persistent data store."   t)
(autoload 'persistent-soft-flush     "persistent-soft" "Flush data for the LOCATION data store to disk."                    t)

(declare-function remove-if-not "cl-seq.el")
(declare-function intersection  "cl-seq.el")
(declare-function gensym        "cl-macs.el")
(declare-function memoize       "memoize.el")
(declare-function memoize-wrap  "memoize.el")

;;; customizable variables

;;;###autoload
(defgroup dynamic-fonts nil
  "Set faces based on available fonts."
  :version "0.6.0"
  :link '(emacs-commentary-link "dynamic-fonts")
  :prefix "dynamic-fonts-"
  :group 'faces)

(defcustom dynamic-fonts-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-use-persistent-storage "dynamic-fonts"
  "Use persistent disk storage when available.

This speeds some operations between sessions.

Internally, this value is a string which is used for the filename
of the persistent data store."
  :type '(choice
          (const :tag "Yes"  "dynamic-fonts")
          (const :tag "No"   nil))
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-use-memory-cache (if (or (eq window-system 'x)
                                                  (eq window-system 'w32)) nil t)
  "Run `font-family-list' at first call of `dynamic-fonts-font-exists-p'.

Take as canonical the list of family names produced.

This is generally a speed benefit.  However, some font names
will later be missed by `dynamic-fonts-font-exists-p', as the font
backend usually has the ability to recognize some alternate
names.

Disabled on X11 and MS Windows by default, because `font-family-list'
often gives truncated results before Emacs is fully initialized.

The MS Windows Emacs port responds to `font-info' requests
quickly, so the cache is less needed, leaving the X11 port as
the pathological case with regard to startup time."
  :type 'boolean
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-proportional-faces '(variable-pitch)
  "List of faces to set with the best-found proportional font.

It is best to keep this list small, and let other proportional
faces inherit from these faces."
  :type '(repeat face)
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-monospace-faces '(fixed-pitch default)
  "List of faces to set with the best-found monospace font.

It is best to keep this list small, and let other monospace
faces inherit from these faces."
  :type '(repeat face)
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-alternatives t
  "Whether to set `face-font-family-alternatives'.

If non-nil, the default entries in `face-font-family-alternatives'
will be supplemented with the preferred monospace and proportional
fonts set in customize."
  :type 'boolean
  :group 'dynamic-fonts)

;;;###autoload
(defgroup dynamic-fonts-preferred-fonts nil
  "Preferred font choices"
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-preferred-proportional-fonts '(
                                                        "Lucida Grande"
                                                        "Segoe UI"
                                                        "DejaVu Sans"
                                                        "Bitstream Vera"
                                                        "Tahoma"
                                                        "Verdana"
                                                        "Helvetica"
                                                        "Arial Unicode MS"
                                                        "Arial"
                                                        )
"A list of proportional fonts in order of preference.

The first font which is present on the system will be used as the
default for variable-width faces."
  :type '(repeat string)
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-proportional-point-size 12
  "Basic proportional fonts will be set at this size if possible."
  :type 'number
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-monospace-fonts '(
                                                     "Monaco"
                                                     "Consolas"
                                                     "Menlo"                        ; based on Bitstream Vera
                                                     "DejaVu Sans Mono"             ; based on Bitstream Vera
                                                     "Droid Sans Mono Pro"
                                                     "Droid Sans Mono"
                                                     "Inconsolata"
                                                     "Lucida Console"
                                                     "Envy Code R"
                                                     "Andale Mono"
                                                     "Lucida Sans Typewriter"
                                                     "Lucida Typewriter"
                                                     "Panic Sans"                   ; based on Bitstream Vera
                                                     "Bitstream Vera Sans Mono"
                                                     "Excalibur Monospace"
                                                     "Courier New"
                                                     "Courier"
                                                     "Cousine"
                                                     "Lekton"
                                                     "Ubuntu Mono"
                                                     "Liberation Mono"
                                                     "BPmono"
                                                     "Anonymous Pro"
                                                     "ProFontWindows"
                                                     )
"A list of monospace fonts in order of preference.

The first font which is present on the system will be used as the
default for fixed-width faces."
  :type '(repeat string)
  :group 'dynamic-fonts-preferred-fonts)

(defcustom dynamic-fonts-preferred-monospace-point-size 12
  "Basic monospace fonts will be set to this size if possible."
  :type 'number
  :group 'dynamic-fonts-preferred-fonts)

;;; variables

;; note: variable outside dynamic-fonts- namespace
(defvar font-name-history nil "History of font names entered in the minibuffer.")

(defvar dynamic-fonts-font-names nil "Hash of all font names.")

;;; compatibility functions

(unless (fboundp 'memoize)
  ;; by Christopher Wellons <mosquitopsu@gmail.com>
  (defun memoize (func)
    "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
    (typecase func
      (symbol (fset func (memoize-wrap (symbol-function func))) func)
      (function (memoize-wrap func))))
  (defun memoize-wrap (func)
    "Return the memoized version of the given function."
    (let ((table-sym (gensym))
          (val-sym (gensym))
          (args-sym (gensym)))
      (set table-sym (make-hash-table :test 'equal))
      `(lambda (&rest ,args-sym)
         ,(concat (documentation func) "\n(memoized function)")
         (let ((,val-sym (gethash ,args-sym ,table-sym)))
           (if ,val-sym
               ,val-sym
             (puthash ,args-sym (apply ,func ,args-sym) ,table-sym)))))))

;;; utility functions

;;;###autoload
(defun dynamic-fonts-font-name-from-xlfd (xlfd)
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

(defun dynamic-fonts-create-fuzzy-matches (font-name &optional keep-size)
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
(defun dynamic-fonts-list-font-names ()
  "Return a list of all font names on the current system."
  (when (display-multi-font-p)
    (delete-dups (remove "nil" (remq nil (font-family-list))))))
(memoize 'dynamic-fonts-list-font-names)

(defun dynamic-fonts-load-font-names (&optional progress regenerate)
  "Populate the hash `dynamic-fonts-font-names'.

When optional PROGRESS is true, show progress feedback in the
echo area.

When optional REGENERATE is true, always rebuild from
scratch."
  (when (display-multi-font-p)
    (when regenerate
      (setq dynamic-fonts-font-names nil)
      (persistent-soft-store (intern (format "checksum-%s" window-system))
                             nil dynamic-fonts-use-persistent-storage)
      (persistent-soft-store (intern (format "font-names-%s" window-system))
                             nil dynamic-fonts-use-persistent-storage)
      (persistent-soft-flush dynamic-fonts-use-persistent-storage))
    (unless (or (hash-table-p dynamic-fonts-font-names)
                (not dynamic-fonts-use-memory-cache))
      (when progress
        (message "Font cache ... checking"))
      (let* ((old-checksum (persistent-soft-fetch (intern (format "checksum-%s" window-system)) dynamic-fonts-use-persistent-storage))
             (listing (dynamic-fonts-list-font-names))
             (new-checksum (md5 (mapconcat 'identity (sort listing 'string<) "") nil nil 'utf-8))
             (dupes nil))
        (when (equal old-checksum new-checksum)
          (setq dynamic-fonts-font-names (persistent-soft-fetch
                                          (intern (format "font-names-%s" window-system))
                                          dynamic-fonts-use-persistent-storage)))
        (unless (hash-table-p dynamic-fonts-font-names)
          (when progress
            (message "Font cache ... rebuilding"))
          (setq dynamic-fonts-font-names (make-hash-table :size (* 5 (length listing)) :test 'equal))
          (dolist (font-name listing)
            (dolist (fuzzy-name (dynamic-fonts-create-fuzzy-matches font-name))
              (callf upcase fuzzy-name)
              (when (and (gethash fuzzy-name dynamic-fonts-font-names)
                         (not (equal (gethash fuzzy-name dynamic-fonts-font-names) font-name)))
                (push fuzzy-name dupes))
              (puthash (upcase fuzzy-name) font-name dynamic-fonts-font-names)))
          (delete-dups dupes)
          (dolist (fuzzy-name dupes)
            (remhash fuzzy-name dynamic-fonts-font-names))
          (persistent-soft-store (intern (format "checksum-%s" window-system))
                                 new-checksum dynamic-fonts-use-persistent-storage)
          (persistent-soft-store (intern (format "font-names-%s" window-system))
                                 dynamic-fonts-font-names dynamic-fonts-use-persistent-storage)
          (persistent-soft-flush dynamic-fonts-use-persistent-storage)))
      (when progress
        (message "Font cache ... done")))))

;;;###autoload
(defun dynamic-fonts-read-font-name (&optional ido)
  "Read a font name using `completing-read'.

Underscores are removed from the return value.

Uses `ido-completing-read' if optional IDO is set."
  (save-match-data
    (let ((prompt "Font: ")
          (reader (if ido 'ido-completing-read 'completing-read))
          (font-names (mapcar #'(lambda (x)
                                   (replace-regexp-in-string " " "_" x))
                              (dynamic-fonts-list-font-names))))
      (replace-regexp-in-string "_" " "
         (funcall reader prompt font-names nil nil nil font-name-history)))))

;;;###autoload
(defun dynamic-fonts-lenient-font-name-equal (font-name-a font-name-b)
  "Leniently match two strings, FONT-NAME-A and FONT-NAME-B."
  (setq font-name-a (replace-regexp-in-string ":[^:]*\\'"   "" font-name-a))
  (setq font-name-b (replace-regexp-in-string ":[^:]*\\'"   "" font-name-b))
  (setq font-name-a (replace-regexp-in-string "-[0-9.]+\\'" "" font-name-a))
  (setq font-name-b (replace-regexp-in-string "-[0-9.]+\\'" "" font-name-b))
  (setq font-name-a (replace-regexp-in-string "[ \t_'\"-]+" "" font-name-a))
  (setq font-name-b (replace-regexp-in-string "[ \t_'\"-]+" "" font-name-b))
  (string-equal (downcase font-name-a) (downcase font-name-b)))

;;;###autoload
(defun dynamic-fonts-font-exists-p (font-name &optional point-size strict scope)
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
    (save-match-data
      (when (fontp font-name 'font-spec)
        (when (and (floatp (font-get font-name :size))
                   (not point-size))
          (setq point-size (font-get font-name :size)))
        (setq font-name (or (font-get font-name :name) (font-get font-name :family))))
      (cond
        ((fontp font-name 'font-entity)
         (font-info font-name))
        ((vectorp font-name)
          font-name)
        (t
         (let ((font-name-list        nil)
               (fontconfig-params     ""))

           ;; read all fonts if possible
           (dynamic-fonts-load-font-names (not dynamic-fonts-less-feedback))

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
                                  (dynamic-fonts-create-fuzzy-matches font-name)))

           ;; constrain font list to scope requested
           (when scope
             (callf2 intersection scope font-name-list :test 'dynamic-fonts-lenient-font-name-equal))

           ;; constrain font list by font cache if possible
           (when (and dynamic-fonts-use-memory-cache
                      (hash-table-p dynamic-fonts-font-names))
             (setq font-name-list (remove-if-not #'(lambda (key)
                                                     (gethash (upcase (replace-regexp-in-string "-[0-9.]+\\'" "" key)) dynamic-fonts-font-names))
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
                   (throw 'font font-vec)))))))))))
(memoize 'dynamic-fonts-font-exists-p)

;;;###autoload
(defun dynamic-fonts-first-existing-font (font-names &optional no-normalize)
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
        (let ((font (dynamic-fonts-font-exists-p name)))
          (when font
            (when no-normalize
              (throw 'name name))
            (cond
              ((vectorp font)
               (throw 'name (dynamic-fonts-font-name-from-xlfd (aref font 0))))
              ((fontp font)
               (throw 'name (or (font-get font :name) (font-get font :family))))
              (t
               (error "Unknown type")))))))))

(defun dynamic-fonts-set-face (face families &optional point-size)
  "Set FACE to the first existing font name in FAMILIES.

Point size may be specified by the optional variable POINT-SIZE.
If POINT-SIZE is not present, size may be specified by a
fontconfig-style specification on the members of FAMILIES.  In
order of preference:

    Font Name:size=<points>
    Font Name-<points>"
  (when (display-multi-font-p)
    (save-match-data
      (let ((font-name (dynamic-fonts-first-existing-font families 'no-normalize)))
        (when font-name
          (when (and (not point-size)
                     (or (string-match ":.*\\<size=\\([0-9.]+\\)" font-name)
                         (string-match "-\\([0-9.]+\\)\\(?::\\|\\'\\)" font-name)))
            (setq point-size (string-to-number
                              (match-string-no-properties 1 font-name))))
          (setq font-name (dynamic-fonts-first-existing-font (list font-name))) ; normalize
          (when (eq face 'default)
            (if (< emacs-major-version 24)
                (set-frame-font font-name t)
              (set-frame-font font-name t t)))
          (set-face-attribute face nil :family font-name)
          (when point-size
            (set-face-attribute face nil :height (round (* 10 point-size)))))))))

;;; interactive commands

;;;###autoload
(defun dynamic-fonts-setup ()
  "Set up `fixed-pitch', `variable-pitch', and `default' faces.

The font face and size is determined dynamically, by comparing
the following values

   `dynamic-fonts-preferred-monospace-fonts'
   `dynamic-fonts-preferred-monospace-point-size'
   `dynamic-fonts-preferred-proportional-fonts'
   `dynamic-fonts-preferred-proportional-point-size'

with the fonts available on your system.

When `dynamic-fonts-set-alternatives' is set, also amends
the standard value of `face-font-family-alternatives', providing
the values above as alternatives."
  (interactive)
  (when (display-multi-font-p)

    (dolist (face dynamic-fonts-set-monospace-faces)
      (dynamic-fonts-set-face face dynamic-fonts-preferred-monospace-fonts dynamic-fonts-preferred-monospace-point-size))

    (dolist (face dynamic-fonts-set-proportional-faces)
      (dynamic-fonts-set-face face dynamic-fonts-preferred-proportional-fonts  dynamic-fonts-preferred-proportional-point-size))

    (when dynamic-fonts-set-alternatives
      (setq face-font-family-alternatives
            (list (append '("Monospace") dynamic-fonts-preferred-monospace-fonts '("courier" "fixed"))
                  (append '("courier" "Courier New" "Lucida Sans Typewriter" "CMU Typewriter Text") dynamic-fonts-preferred-monospace-fonts '("fixed"))
                  (append '("Sans Serif" "helv" "helvetica" "arial") dynamic-fonts-preferred-proportional-fonts dynamic-fonts-preferred-monospace-fonts '("fixed"))
                  (append '("helv" "helvetica" "arial") dynamic-fonts-preferred-proportional-fonts dynamic-fonts-preferred-monospace-fonts '("fixed")))))))

(provide 'dynamic-fonts)

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
;; LocalWords:  DynamicFonts XQuartz Lucida callf Segoe DejaVu Arial
;; LocalWords:  Bitstream Tahoma Verdana Helvetica Consolas Menlo
;; LocalWords:  Inconsolata Andale Cousine Lekton BPmono Garamond
;; LocalWords:  ProFontWindows Hiragino XLFD demi fontconfig Grande
;; LocalWords:  helvetica
;;

;;; dynamic-fonts.el ends here
