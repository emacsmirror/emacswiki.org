;;; mon-css-check.el --- optimize CSS attributes and properties with Emacs interface to csstidy


;; Copyright (C) 2010  niels giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Website: http://pft.github.com
;; Keywords: css, web, html


;;; ==============================
;; <Timestamp: #{2010-03-31T19:00:44-04:00Z}#{10133} - by MON>
;; :NOTE This is Niels Giesen's css-check.el with MON minor changes.
;; URL: http://github.com/pft/elisp-assorted/blob/master/css-check.el
;;
;; Changes and modifications include:
;; RENAMED: 
;; `css-check-file'         -> `*css-check-file*'
;; `css-check-map'          -> `*css-check-map*'
;; `css-check-follow-mode'  -> `*css-check-follow-mode*'
;; `css-check-csstidy-path' -> `*css-check-csstidy-path*'
;; 
;; :ADDED docstring xrefs
;; :CHANGED `css-check-toggle-follow' now buffer-local
;; :ADDED :FUNCTION `css-check-kill-buffer-hook'
;; :ADDED :FUNCTION `css-check-buffer-offer-save'
;; :ADDED :FUNCTION `css-check-buffer-mode-css-p'
;; :ADDED :VARIABLE `*CSS-CHECK*'
;; :ADDED docstring keybinding xrefs to `*css-check-map*'
;;
;; :NOTE csstidy release 1.3 from source won't compile on my system.
;; I get this:
;; ,----
;; | g++ -o release/csstidy/misc.o -c -O2 csstidy/misc.cpp
;; | csstidy/misc.cpp: In function 'bool in_char_arr(const char*, char)':
;; | csstidy/misc.cpp:124: error: 'strlen' was not declared in this scope
;; | scons: *** [release/csstidy/misc.o] Error 1
;; | scons: building terminated because of errors.
;; `----
;; Proplem is `in_char_arr' in the file: mics.cpp
;; A quick grep on the sources shows the symbol `in_char_arr' appears in the two
;; files:
;;
;; misc.cpp
;; misc.hpp
;; `in_char_arr' has no callers.
;;
;; If I comment out the code block of `in_char_arr' scons will compile the
;; executable.
;; :SEE (URL `http://sourceforge.net/tracker/?func=detail&aid=2996207&group_id=148404&atid=771415')
;; ==============================
;; ==============================
;; :TODO <Timestamp: #{2010-06-02T18:49:50-04:00Z}#{10223} - by MON>
;;
;; Add defun* that parses these keywords for to augment the flags passed to
;; `call-process-shell-command' in `css-check'.
;;
;; ,----
;; | :allow-html-in-templates (nil t)
;; | :discard-invalid-properties (nil t)
;; | :lowercase-s (nil t)
;; | :preserve-css (nil t)
;; | :remove-last-semic (nil t)
;; | :silent= (nil t)
;; | :sort-properties (nil t)
;; | :sort-selectors (nil t)
;; | :timestamp (nil t)
;; | :merge-selectors (2 1 0)
;; | :case-properties (0 1 2)
;; | :optimise-shorthands (1 2 0)
;; | :template (default filename low high highest)
;; | :NOTE that default to t should be inverted.
;; | :compress-colors (t nil)      ;; :no-compress-colors (nil t)
;; | :compress-font-weight (t nil) ;; :no-compress-font-weight (nil t)
;; | :remove-bslash (t nil)        ;; :no-remove-bslash (nil t)
;; `----
;; :SEE `mon-help-css-check' for some details as to what these flags do.
;; 
;;
;; :NOTE <Timestamp: #{2010-08-07T18:25:14-04:00Z}#{10316} - by MON KEY>
;; I'm seeing occurences of csstidy lines in the *CSS-CHECK* buffer like this:
;; ,----
;; | 139: Optimised font-weight: Changed "normal" to "400"
;; | 180: Optimised font-weight: Changed "normal" to "400"
;; | 438: Optimised font-weight: Changed "bold" to "700"
;; | 723: Optimised font-weight: Changed "bold" to "700"
;; `----
;; Where the corresponding CSS file has these line:
;; ,----
;; | 	font-weight: bold;
;; | 	font-weight: bold;
;; | 	font-weight: normal;
;; | 	font-weight: normal;
;; `----
;; Which causes `css-check-apply-line-at-p' to miss on the line/text replacement
;; correspondences from *CSS-CHECK* -> *css-check-file* and not so nothing gets
;; replace.  Its not clear to me what is happening or why, including whether my
;; shit CSS skills are to blame. This said, my current guess is that `css-check'
;; isn't passing any flags to csstidy at the `call-process-shell-command' too
;; bad the csstidy project is mostly-dead.
;;
;; ==============================
;; 
;; :TODO Build a function that adds text-props Xrefing the optimized/changed thing to
;;       the relevant section of `mon-help-css-properties' in mon-doc-help-css.el
;;       Doing this will first require an enumeration of _what_ csstidy can optimize/change
;;
;; :CSS-CHECK-OPTIMISED
;; "shorthand notation (padding):"
;; "color:"
;; "number:"
;; "font-weight:"
;;
;; :CSS-CHECK-CHANGED
;; long-hex -> short-hex
;; "normal" to "400"
;; "bold" to "700"
;; "1px 1px" to "1px"
;;
;; URL: http://www.emacswiki.org/emacs/mon-doc-help-css.el
;;
;; FIRST-PUBLISHED:
;; <Timestamp: #{2010-08-07T19:37:01-04:00Z}#{10316} - by MON KEY>
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-03-31T19:00:44-04:00Z}#{10133} - by MON>
;; ==============================


;;; ==============================

;;; css-check.el --- optimize CSS attributes and properties with Emacs interface to csstidy

;; Copyright (C) 2010  niels giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Website: http://pft.github.com
;; Keywords: css, web, html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Quick program. Use csstidy to check buffer for improper css. 

;;; Usage:

;; M-x css-check to check buffer using the minifying program csstidy.

;;; Code:

(defgroup css-check nil
  "Customization group for CSS-check.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*', `*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  :group 'css)

(defface css-check-unapplied-face
  '((default)
    (((background light)) (:foreground "#ab5736"))
    (((background dark)) (:foreground "#ab5736")))
  "Face for unapplied line.\n
:SEE-ALSO `mon-help-css-check', `css-check', `*css-check-csstidy-path*',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p'.\n►►►"
  :group 'css-check)

(defface css-check-applied-face
  '((default)
    (((background light)) (:foreground "#aa9b37"))
    (((background dark)) (:foreground "#aa9b37")))
  "Face for applied line.
:SEE-ALSO `mon-help-css-check', `css-check', `*css-check-csstidy-path*',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p'.\n►►►"
  :group 'css-check)

(defvar *css-check-map* nil  
  "Keymap for `*css-check-follow-mode*'.\n
 C-m `css-check-goto-line-at-p'
 a   `css-check-apply-line-at-p'
 u   `css-check-undo-application-at-p'
 p   `css-check-prev-line'
 n   `css-check-next-line'
 f   `css-check-toggle-follow'\n
:SEE-ALSO `mon-help-css-check', `css-check', `*css-check-csstidy-path*',
`*css-check-file*', `*css-check-map*', `*css-check-follow-mode*',
`css-check-applied-face', `css-check-unapplied-face'.\n►►►")
(unless (bound-and-true-p *css-check-map*)
  (setq *css-check-map*
        (let ((map (make-sparse-keymap)))
          (define-key map "\C-m" 'css-check-goto-line-at-p)
          (define-key map "f"    'css-check-toggle-follow)
          (define-key map "n"    'css-check-next-line)
          (define-key map "p"    'css-check-prev-line)
          (define-key map "a"    'css-check-apply-line-at-p)
          (define-key map "u"    'css-check-undo-application-at-p)
          (define-key map [(mouse-1)] 'css-check-goto-line-at-p)
          map)))

(defvar *css-check-follow-mode* nil
  "Whether `css-check' follows changes to *css-check-file* in other-window.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*', `*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►")

(defvar *css-check-csstidy-path* nil
  "Whether the csstidy executable is in path.\n
When non-nil return value is the path to local csstidy.\n
:SEE (URL `http://csstidy.sourceforge.net/index.php')
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*', `*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►")
;;
(unless (bound-and-true-p *css-check-csstidy-path*)
  (let ((csstidy-path (or (executable-find "csstidy") 
                          (executable-find "csstidy.exe"))))
    (when csstidy-path (setq *css-check-csstidy-path* csstidy-path))))

(defvar *css-check-file* nil
  "The current file `css-check'is frobbing.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-csstidy-path',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p', `*css-check-csstidy-path*',
`*css-check-file*', `*css-check-map*', `*css-check-follow-mode*',
`css-check-applied-face', `css-check-unapplied-face'.\n►►►" )

;;; ==============================
;;; :CHANGESET 1827
;;; :CREATED <Timestamp: #{2010-06-02T20:51:35-04:00Z}#{10223} - by MON>
(defvar *CSS-CHECK* nil
  "A buffer name to display the results displayed from `css-check'.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-csstidy-path',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p', `*css-check-csstidy-path*',
`*css-check-file*', `*css-check-map*', `*css-check-follow-mode*',
`css-check-applied-face', `css-check-unapplied-face'.\n►►►")
;;
(unless (bound-and-true-p *CSS-CHECK*)
  (setq *CSS-CHECK*  (symbol-name '*CSS-CHECK*)))

;;; ==============================
;;; :CHANGESET 1827
;;; :CREATED <Timestamp: #{2010-06-02T20:49:27-04:00Z}#{10223} - by MON>
(defun css-check-kill-buffer-hook ()
  "Clear `*css-check-file*' buffer-local-variable on the `*CSS-CHECK*' buffer.\n
Run on the `kill-buffer-hook'.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-csstidy-path',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p', `*css-check-csstidy-path*',
`*css-check-file*', `*css-check-map*', `*css-check-follow-mode*',
`css-check-applied-face', `css-check-unapplied-face'.\n►►►"
  (when (get-buffer *CSS-CHECK*)
    (with-current-buffer (buffer-name (get-buffer *CSS-CHECK*))
      (setq *css-check-file* nil))))
;;
;; (remove-hook 'kill-buffer-hook 'css-check-kill-buffer-hook)

(defun css-check-toggle-follow ()
  "Toggle `*css-check-follow-mode*'.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (interactive)
  (with-current-buffer (get-buffer *CSS-CHECK*)
    (set (make-local-variable '*css-check-follow-mode*)
         ;; (setq *css-check-follow-mode* 
         (not (buffer-local-value *css-check-follow-mode* (current-buffer)))))
  (message (concat ":FUNCTION `css-check-toggle-follow' "
                   "-- local variable `*css-check-follow-mode*' set to: %S")
           (buffer-local-value *css-check-follow-mode* (get-buffer *CSS-CHECK*))))

;; *css-check-follow-mode*

(defun css-check-next-line ()
  "When `*css-check-follow-mode*' do `css-check-goto-line-at-p' for next line.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (interactive)
  (forward-line 1)
  (when (and (get-buffer *CSS-CHECK*)
             (buffer-local-value *css-check-follow-mode* (get-buffer *CSS-CHECK*)))
    (let ((ccnl-buffer (current-buffer)))
      (css-check-goto-line-at-p)
      (switch-to-buffer-other-window ccnl-buffer))))

(defun css-check-prev-line ()
  "When `*css-check-follow-mode*' do `css-check-goto-line-at-p' for previous line.\n
:SEE-ALSO `mon-help-css-check', `css-check',
`css-check-toggle-follow', `css-check-next-line', `css-check-prev-line',
`css-check-goto-line-at-p', `css-check-apply-line-at-p',
`css-check-undo-application-at-p'.\n►►►"
  (interactive)
  (forward-line -1)
  (when (and (get-buffer *CSS-CHECK*)
             (buffer-local-value *css-check-follow-mode* (get-buffer *CSS-CHECK*)))
    ;; (when *css-check-follow-mode*
    (let ((ccpl-buffer (current-buffer)))
      (css-check-goto-line-at-p)
      (switch-to-buffer-other-window ccpl-buffer))))

(defun css-check-goto-line-at-p ()
  "Find css-check file `*css-check-file*' other window.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (interactive)
  ;; (and *css-check-file*
  (and *css-check-file* 
       (get-buffer *CSS-CHECK*)
       (buffer-local-value '*css-check-file* (get-buffer *CSS-CHECK*))
       (let ((ccglap-line (save-excursion 
                            (beginning-of-line)
                            (save-match-data 
                              (and (search-forward-regexp "^[[:digit:]]+" (line-end-position) t)
                                   (string-to-number (match-string 0)))))))
         (if (null *css-check-file*)
             (error 
              (concat ":FUNCTION `css-check-goto-line-at-p' "
                      " -- `*css-check-file*' not currently bound to file name" ))
           (find-file-other-window *css-check-file*))
         ;; <Timestamp: #{2010-06-02T19:54:52-04:00Z}#{10223} - by MON>
         ;; :ADDED `with-no-warnings', `save-match-data'
         (and ccglap-line (with-no-warnings (save-match-data (goto-line ccglap-line)))))))

(defun css-check-apply-line-at-p ()
  "Apply csstidy CSS tidying at point.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*', `*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (interactive)
  (let ((ccalap-buffer (current-buffer)))
    (if (text-property-any (line-beginning-position) (1+ (line-beginning-position))
                           'face 'css-check-applied-face)
        (message (concat ":FUNCTION `css-check-apply-line-at-p' "
                         "-- line has already been applied"))
      ;; :WAS (let ((ccalap-buffer (current-buffer)))  ;; :MOVED-UP
      (multiple-value-bind (oldtext newtext)
          (save-excursion
            (end-of-line)
            (search-backward-regexp
             "Changed \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$" (line-beginning-position) t)
            `(,(match-string 1) ,(match-string 2)))
        (when (and oldtext newtext)
          (css-check-goto-line-at-p)
          ;; :NOTE what to do about `replace-string'? Safe to change inside m-v-b? -- MON
          ;; (save-excursion
          ;;   (beginning-of-line)
          ;;   (while (search-forward-regexp oldtext nil (line-end-position))
          ;;     (replace-match newtext)))
          ;; OR, Maybe this: 
          ;; (save-excursion
          ;; (apply 'replace-string oldtext `(,newtext nil ,(line-beginning-position) ,(line-end-position))))
          ;;
          ;; :WAS (save-excursion          
          ;; (replace-string oldtext newtext nil (line-beginning-position) (line-end-position)))
          (save-excursion
            (while (search-forward oldtext (point-at-eol) t)
              (replace-match newtext t t)))
          (switch-to-buffer-other-window ccalap-buffer)
          (save-excursion
            (let (buffer-read-only)
              (put-text-property (line-beginning-position)
                                 (progn
                                   (beginning-of-line)
                                   (search-forward-regexp "^[[:digit:]]+" (line-end-position) t)
                                   (point))
                                 'face 'css-check-applied-face))))))))
              

(defun css-check-undo-application-at-p ()
  "Undo `css-check's  pplication of csstidy changes at point.\n
:SEE-ALSO `mon-help-css-check', `css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (interactive)
  (if (text-property-any (line-beginning-position) (1+ (line-beginning-position))
                         'face 'css-check-unapplied-face)
      (message (concat ":FUNCTION `css-check-undo-application-at-p' "
                       "-- line has not been applied"))
    (let ((ccuap-buffer (current-buffer)))
      (multiple-value-bind (newtext oldtext)
          (save-excursion
            (end-of-line)
            (search-backward-regexp
             "Changed \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"  (line-beginning-position) t)
            `(,(match-string 1) ,(match-string 2)))
        (when (and oldtext newtext)
          (css-check-goto-line-at-p)
                      ;; :NOTE what to do about `replace-string'? Safe to change inside m-v-b? -- MON
            ;; (save-excursion
            ;;   (beginning-of-line)
            ;;   (while (search-forward-regexp oldtext nil (line-end-position))
            ;;     (replace-match newtext)))
            ;; OR, Maybe this: 
            ;; (save-excursion
            ;; (apply 'replace-string oldtext `(,newtext nil ,(line-beginning-position) ,(line-end-position))))
            ;; (save-excursion
            ;;   (while (search-forward oldtext (point-at-eol) t)
            ;;     (replace-match newtext t t)))
            ;;
            ;; :WAS (replace-string oldtext newtext nil (line-beginning-position) (line-end-position)))            
            (save-excursion
              (while (search-forward oldtext (point-at-eol) t)
                (replace-match newtext t t)))
          (switch-to-buffer-other-window ccuap-buffer)
          (save-excursion
            (let (buffer-read-only)
              (put-text-property (line-beginning-position)
                                 (progn
                                   (beginning-of-line)
                                   (search-forward-regexp "^[[:digit:]]+" (line-end-position) t)
                                   (point))
                                 'face 'css-check-unapplied-face))))))))


;;; ==============================
;;; :CHANGESET 2053
;;; :CREATED <Timestamp: #{2010-08-07T19:10:38-04:00Z}#{10316} - by MON KEY>
(defun css-check-buffer-mode-css-p (&optional w-prompt-if-not)
  "Is current buffer a css-mode buffer.\n
When optional arg W-PROMPT-IF-NOT is non-nil prompt to make it so.\n
:CALLED-BY `css-check-buffer-offer-save'\n
:SEE-ALSO `mon-help-css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (with-current-buffer (current-buffer)
    (if (not (eq major-mode 'css-mode))
        (when w-prompt-if-not
          (if (y-or-n-p (concat ":FUNCTION `css-check-buffer-mode-css-p'"
                                "-- current-buffer not in css-mode proceed?: "))
              (progn 
                (when (y-or-n-p (concat ":FUNCTION `css-check-buffer-mode-css-p'"
                                        "-- enable css-mode for current-buffer now?: "))
                (css-mode)) 
                t)
            nil))
      t)))

;;; ==============================
;;; :CHANGESET 2053
;;; :CREATED <Timestamp: #{2010-08-07T18:45:45-04:00Z}#{10316} - by MON KEY>
(defun css-check-buffer-offer-save ()
  "Offer to save current CSS file when `buffer-modified-p'.\n
:CALLED-BY `css-check'\n
:SEE-ALSO `mon-help-css-check', `css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face'.\n►►►"
  (with-current-buffer (current-buffer)
    (and (buffer-modified-p)
         (y-or-n-p (format 
                    (concat 
                     ":FUNCTION `css-check-buffer-offer-save' "
                     "-- buffer: %s has been modified since last save. Save buffer?: ")
                    (current-buffer)))
         (save-buffer)))
  (css-check-buffer-mode-css-p t))

(defun css-check ()
  "User interface function to css-check.el features.\n
Run csstidy with `call-process-shell-command' for `buffer-file-name'.\n
:SEE-ALSO `css-check-buffer-offer-save', 
`css-check-toggle-follow',
`css-check-next-line', `css-check-prev-line', `css-check-goto-line-at-p',
`css-check-apply-line-at-p', `css-check-undo-application-at-p',
`*css-check-csstidy-path*',`*css-check-file*', `*css-check-map*',
`*css-check-follow-mode*', `css-check-applied-face',
`css-check-unapplied-face', `mon-help-css-check', `mon-help-css-properties', `mon-help-css-color',
`mon-help-css-mode', `mon-help-css-complete', `mon-help-tidy'.\n►►►"
  (interactive)
  (css-check-buffer-offer-save)
  ;; :WAS
  ;; (and (buffer-modified-p)
  ;;      (y-or-n-p (format 
  ;;                 (concat 
  ;;                  ":FUNCTION `css-check' "
  ;;                  "-- buffer: %s has been modified since last save. Save buffer?: ")
  ;;                 (current-buffer)))
  ;;      (save-buffer))
  (let* ((cc-cur-file (buffer-file-name (current-buffer)))
         ;; :WAS (cmd (format "csstidy %s /dev/null" file))
         ;;      (res (shell-command-to-string cmd)))
         (cmd (format "%s %s %S" *css-check-csstidy-path* cc-cur-file null-device)))
    ;; :WAS (switch-to-buffer *CSS-CHECK)
    ;; Make sure we kill `*CSS-CHECK*' to reset `*css-check-file*' filename
    (when (get-buffer *CSS-CHECK*)
      (kill-buffer (get-buffer *CSS-CHECK*)))
    (with-current-buffer (get-buffer-create *CSS-CHECK*)
      (let (buffer-read-only)
        ;; :WAS (insert res)
        ;;      (goto-char (point-min))
        (display-buffer (current-buffer) t)
        (save-excursion
          (call-process-shell-command cmd nil t t))
        (while (search-forward-regexp "^[[:digit:]]+" nil t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face 'css-check-unapplied-face)
          (put-text-property (match-beginning 0) (match-end 0)
                             'keymap 'highlight))
        (goto-char (buffer-end 0))
        (princ (concat ";; :FUNCTION `css-check'\n"
                       ";; :EVALUATED " 
                       (format-time-string "<Timestamp: #{%Y-%m-%dT%T%zZ}#{%y%V%u}>\n")                 
                       ";; :CSS-FILE " cc-cur-file "\n"
                       (make-string 35 45))
               (current-buffer))
        ;;(beginning-of-line 5))
        ;; (setq *css-check-file* cc-cur-file)
        (set (make-local-variable '*css-check-file*) cc-cur-file)
        ;;    *css-check-file*)
        (use-local-map *css-check-map*)
        ;; (setq buffer-read-only t)
        (set (make-local-variable 'buffer-read-only) t)
        (add-hook 'kill-buffer-hook 'css-check-kill-buffer-hook nil t)))))

;; (provide 'css-check)
;; css-check.el ends here

;;; ==============================
(provide 'mon-css-check)
;;; ==============================

;;; ==============================
;; mon-css-check.el ends here
;;; EOF
