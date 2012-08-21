;;; dynamic-fonts.el --- Set faces based on available fonts
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker walker@pobox.com
;; URL: https://github.com/rolandwalker/dynamic-fonts.el
;; Version: 0.5.0
;; Last-Updated: 21 Aug 2012
;; EmacsWiki: DynamicFonts
;; Keywords:
;; Package-Requires: ((persistent-soft "0.8.0"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Dynamic-fonts.el makes font configuration more portable between
;; machines.  When Emacs is starting up, dynamic-fonts chooses fonts
;; for your basic faces based on which fonts are actually available.
;;
;; You may set a list of fonts in order of preference using customize.
;;
;; To use dynamic-fonts, place the dynamic-fonts.el file somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;    (require 'dynamic-fonts)
;;    (dynamic-fonts-setup)
;;
;; See Also
;;
;;    M-x customize-group RET dynamic-fonts RET
;;
;; Notes
;;
;; The secondary purpose of this library is to provide some font
;; utility functions to be called from Lisp.  See in particular
;; `dynamic-fonts-font-exists-p', which tests font availability.
;;
;; Compatibility
;;
;;     Tested only on GNU Emacs version 24.1
;;
;; Bugs
;;
;;    Checking for font availability is slow on most systems.  This
;;    library can add up to several seconds to startup time.  Workaround:
;;    where supported, font information can be cached to disk.
;;
;;    dynamic-fonts-font-exists-p only supports two styles of font
;;    name.   This page
;;
;;       http://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html#Fonts
;;
;;    describes four styles of font name.
;;
;;    Passing point size to dynamic-fonts-font-exists-p not working
;;    well under XQuartz.  When requesting with a size appended to
;;    name ("Lucida Typewriter-12"), the size returned is always
;;    larger.  Needs testing on other X11 servers.
;;
;;    A bogus short name such as "M" may be allowed to match the default
;;    font in dynamic-fonts-font-exists-p if the system returns the
;;    default font on failure of font-info (eg Cocoa).
;;
;; TODO
;;
;;    test whether (find-font (font-spec :name "Name")) is faster
;;    than font-info
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

;;; customizable variables

;;;###autoload
(defgroup dynamic-fonts nil
  "Set faces based on available fonts."
  :version "0.5.0"
  :link '(emacs-commentary-link "dynamic-fonts")
  :prefix "dynamic-fonts-"
  :group 'extensions)

(defcustom dynamic-fonts-less-feedback nil
  "Give less echo area feedback."
  :group 'dynamic-fonts
  :type 'boolean)

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
  :type '(repeat symbol)
  :group 'dynamic-fonts)

(defcustom dynamic-fonts-set-monospace-faces '(fixed-pitch default)
  "List of faces to set with the best-found monospace font.

It is best to keep this list small, and let other monospace
faces inherit from these faces."
  :type '(repeat symbol)
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

(defvar dynamic-fonts-font-info-workaround '(                                       ;
                                             ;; exact FULL-NAME     . regexp matching arg to font-info
                                             ("CooperBlackStd"      . "\\`#?Cooper[ \t_-]*Std\\'")
                                             ("DFKaiShu-SB-Estd-BF" . "\\`#?Biau[ \t_-]*Kai\\'")
                                             ("GaramondPremrPro"    . "\\`#?Garamond[ \t_-]*Premier[ \t_-]*Pro\\'")
                                             ("HiraKakuPro-W3"      . "\\`#?Hiragino[ \t_-]*Kaku[ \t_-]*Gothic[ \t_-]*Pro\\'")
                                             ("HiraKakuProN-W3"     . "\\`#?Hiragino[ \t_-]*Kaku[ \t_-]*Gothic[ \t_-]*Pro[ \t_-]*N\\'")
                                             ("HiraKakuStd-W8"      . "\\`#?Hiragino[ \t_-]*Kaku[ \t_-]*Gothic[ \t_-]*Std\\'")
                                             ("HiraKakuStdN-W8"     . "\\`#?Hiragino[ \t_-]*Kaku[ \t_-]*Gothic[ \t_-]*Std[ \t_-]*N\\'")
                                             ("HiraMaruPro-W4"      . "\\`#?Hiragino[ \t_-]*Maru[ \t_-]*Gothic[ \t_-]*Pro\\'")
                                             ("HiraMaruProN-W4"     . "\\`#?Hiragino[ \t_-]*Maru[ \t_-]*Gothic[ \t_-]*Pro[ \t_-]*N\\'")
                                             ("HiraMinPro-W3"       . "\\`#?Hiragino[ \t_-]*Mincho[ \t_-]*Pro\\'")
                                             ("HiraMinProN-W3"      . "\\`#?Hiragino[ \t_-]*Mincho[ \t_-]*Pro[ \t_-]*N\\'")
                                             ("JCHEadA"             . "\\`#?head[ \t_-]*line[ \t_-]*a\\'")
                                             ("JCfg"                . "\\`#?Pilgi")
                                             ("JCkg"                . "\\`#?gung[ \t_-]*seo")
                                             ("JCsmPC"              . "\\`#?P[ \t_-]*C[ \t_-]*Myung[ \t_-]*jo\\'")
                                             ("JCsmPC"              . "\\`#?P[ \t_-]*C[ \t_-]*Myung[ \t_-]*jo\\'")
                                             ("LiGothicMed"         . "\\`#?Apple[ \t_-]*Li[ \t_-]*Gothic\\'")
                                             ("LiSungLight"         . "\\`#?Apple[ \t_-]*Li[ \t_-]*Sung\\'")
                                             ("NanumBrush"          . "\\`#?Nanum[ \t_-]*Brush[ \t_-]*Script\\'")
                                             ("NanumPen"            . "\\`#?Nanum[ \t_-]*Pen[ \t_-]*Script\\'")
                                             ("SIL-Hei-Med-Jian"    . "\\`#?Hei\\'")
                                             ("SIL-Kai-Reg-Jian"    . "\\`#?Kai\\'")
                                             ("STKaiti-SC-Regular"  . "\\`#?Kaiti[ \t_-]*S[ \t_-]*C\\'")
                                             ("STXihei"             . "\\`#?S[ \t_-]*T[ \t_-]*Heiti\\'")
                                             ) "Overrides for fonts that return a non-matching FULL-NAME in `font-info'.")

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

;;;###autoload
(defun dynamic-fonts-list-font-names ()
  "Return a list of all font names on the current system."
  (when (display-multi-font-p)
    (delete-dups (remove "nil" (remq nil (font-family-list))))))
(memoize 'dynamic-fonts-list-font-names)

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

Returns a matching font vector.

When POINT-SIZE is set, check for a specific font size.  Size may
also be given at the end of a string FONT-NAME, eg \"Monaco-12\".

When optional STRICT is given, FONT-NAME must will not be
leniently modified before passing to `font-info', and must
exactly match the string at the FULL-NAME element in the
returned font vector, or the family name in the XLFD string.

Optional SCOPE is a list of font names, within which FONT-NAME
must \(leniently\) match."
  (when (display-multi-font-p)
    (save-match-data
      (if (fontp font-name 'font-object)
          font-name
        ;; else
        (let* ((font-name-list        nil)
               (font-name-list-nosize nil)
               (font-name-params      nil)              ; fontconfig-style parameters
               (font-name-regexp      nil))
          (dynamic-fonts-load-font-names (not dynamic-fonts-less-feedback))
          (when (string-match ":\\(.*\\)\\'" font-name)
            (setq font-name-params (match-string 1 font-name))
            (setq font-name (replace-match "" t t font-name)))
          (if strict
              (progn
                (setq font-name-list (list font-name))
                (setq font-name-list-nosize (list font-name))
                (setq font-name-regexp (concat "\\`" (regexp-quote font-name) "\\'")))
            ;; else
            (setq font-name-list (dynamic-fonts-create-fuzzy-matches font-name 'keep-size))
            (when (string-match "-\\([0-9.]+\\)\\'" font-name)
              (callf or point-size (string-to-number (match-string-no-properties 1 font-name))))
            (setq font-name-list-nosize (mapcar #'(lambda (x) (replace-regexp-in-string "-[0-9.]+\\'" "" x)) font-name-list))
            (setq font-name-regexp (concat "\\`" (regexp-opt font-name-list-nosize 'paren))))
          (when (and (not point-size)
                     font-name-params
                     (string-match "\\<size=\\([0-9.]+\\)" font-name-params))
            (setq point-size (match-string 1 font-name-params)))
          (when point-size
            (setq font-name-list (mapcar #'(lambda (x) (format "%s:size=%s" x point-size)) font-name-list-nosize)))
          (unless strict
            (setq case-fold-search t))
          (when scope
            (callf2 intersection scope font-name-list-nosize :test 'dynamic-fonts-lenient-font-name-equal)
            (callf2 intersection scope font-name-list        :test 'dynamic-fonts-lenient-font-name-equal))
          (when (and dynamic-fonts-use-memory-cache
                     (hash-table-p dynamic-fonts-font-names))
            (setq font-name-list-nosize (remove-if-not #'(lambda (key)
                                                           (gethash (upcase key) dynamic-fonts-font-names))
                                                       font-name-list-nosize))
            (setq font-name-list        (remove-if-not #'(lambda (key)
                                                           (gethash (upcase (replace-regexp-in-string "-[0-9.]+\\'" "" key)) dynamic-fonts-font-names))
                                                       font-name-list)))
          ;; using find-font to return a font object makes more sense, but
          ;; font-info does a better job of normalizing the family name - why?
          ;; (catch 'font
          ;;   (dolist (name font-name-list)
          ;;     (let* ((font-obj (or (find-font (font-spec :name name))
          ;;                          (find-font (font-spec :family name))))
          ;;            (font-name nil)
          ;;            (alt-font-name nil))
          ;;       (when font-obj
          ;;         (setq font-name (font-get font-obj :name))
          ;;         (setq alt-font-name (font-get font-obj :family))
          ;;         (when (or (dynamic-fonts-lenient-font-name-equal name font-name)
          ;;                   (dynamic-fonts-lenient-font-name-equal name alt-font-name))
          ;;           (when point-size
          ;;             (font-put :size (float point-size)))
          ;;             (throw 'font font-obj)))))))))))
          (catch 'font
            (dolist (name font-name-list)
              (let* ((font-vec (with-local-quit (ignore-errors (font-info name))))
                     (font-name nil)
                     (alt-font-name nil))
                (when font-vec
                  ;; maybe matching the name field of the vector was a mistake, everything
                  ;; else is working off the XLFD at this point
                  (setq font-name (car (split-string (aref font-vec 1) ":")))
                  (setq alt-font-name (dynamic-fonts-font-name-from-xlfd (aref font-vec 0)))
                  (when (or (string-match-p font-name-regexp font-name)
                            (string-match-p font-name-regexp alt-font-name)
                            (and (not strict)
                                 (assoc font-name dynamic-fonts-font-info-workaround)
                                 (remove-if-not #'(lambda (x)
                                                    (let ((case-fold-search t))
                                                      (string-match-p (cdr (assoc font-name dynamic-fonts-font-info-workaround)) x)))
                                                font-name-list)))
                    (when (or (not point-size)
                              (eq point-size (aref font-vec 2)))
                      (throw 'font font-vec))))))))))))
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
            (set-frame-font font-name t t))
          (set-face-attribute face nil :family font-name)
          (when point-size
            (set-face-attribute face nil :height (round (* 10 point-size)))))))))

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
;; End:
;;
;; LocalWords:  DynamicFonts XQuartz Lucida callf Segoe DejaVu Arial
;; LocalWords:  Bitstream Tahoma Verdana Helvetica Consolas Menlo
;; LocalWords:  Inconsolata Andale Cousine Lekton BPmono Garamond
;; LocalWords:  ProFontWindows Hiragino XLFD demi fontconfig Grande
;; LocalWords:  helvetica
;;

;;; dynamic-fonts.el ends here
