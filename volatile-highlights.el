;;; volatile-highlights.el --- Minor mode for visual feedback on some operations. -*- lexical-binding: t; -*-

;; Copyright (C) 2001, 2010-2016, 2024 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: 03 October 2001. (as utility functions in my `.emacs' file.)
;;          14 March   2010. (re-written as library `volatile-highlights.el')
;; Keywords: emulations convenience wp
;; Revision: $Id: 6ab48fbe23ca62f88d3769a5afe0cc35b8d687d7 $
;; URL: http://www.emacswiki.org/emacs/download/volatile-highlights.el
;; GitHub: http://github.com/k-talo/volatile-highlights.el
;; Version: 1.15
;; Contributed by: Ryan Thompson and Le Wang.

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;;
;; Overview
;; ========
;; This library provides minor mode `volatile-highlights-mode', which
;; brings visual feedback to some operations by highlighting portions
;; relating to the operations.
;;
;; All of highlights made by this library will be removed
;; when any new operation is executed.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path" within Emacs), then add the following line to your
;; .emacs start up file:
;;
;;    (require 'volatile-highlights)
;;    (volatile-highlights-mode t)
;;
;;
;; USING
;; =====
;; To toggle volatile highlighting, type `M-x volatile-highlights-mode <RET>'.
;;
;; While this minor mode is on, a string `VHl' will be displayed on the modeline.
;;
;; Currently, operations listed below will be highlighted While the minor mode
;; `volatile-highlights-mode' is on:
;;
;;    - `undo':
;;      Volatile highlights will be put on the text inserted by `undo'.
;;
;;    - `yank' and `yank-pop':
;;      Volatile highlights will be put on the text inserted by `yank'
;;      or `yank-pop'.
;;
;;    - `kill-region', `kill-line', any other killing function:
;;      Volatile highlights will be put at the positions where the
;;      killed text used to be.
;;
;;    - `delete-region':
;;      Same as `kill-region', but not as reliable since
;;      `delete-region' is an inline function.
;;
;;    - `find-tag':
;;      Volatile highlights will be put on the tag name which was found
;;      by `find-tag'.
;;
;;    - `occur-mode-goto-occurrence' and `occur-mode-display-occurrence':
;;      Volatile highlights will be put on the occurrence which is selected
;;      by `occur-mode-goto-occurrence' or `occur-mode-display-occurrence'.
;;
;;    - Non incremental search operations:
;;      Volatile highlights will be put on the the text found by
;;      commands listed below:
;;
;;        `nonincremental-search-forward'
;;        `nonincremental-search-backward'
;;        `nonincremental-re-search-forward'
;;        `nonincremental-re-search-backward'
;;        `nonincremental-repeat-search-forward'
;;        `nonincremental-repeat-search-backwar'
;;
;; Highlighting support for each operations can be turned on/off individually
;; via customization. Also check out the customization group
;;
;;   `M-x customize-group RET volatile-highlights RET'
;;
;;
;; EXAMPLE SNIPPETS FOR USING VOLATILE HIGHLIGHTS WITH OTHER PACKAGES
;; ==================================================================
;;
;; - vip-mode
;;
;;   (vhl/define-extension 'vip 'vip-yank)
;;   (vhl/install-extension 'vip)
;;
;; - evil-mode
;;
;;   (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before
;;                         'evil-paste-pop 'evil-move)
;;   (vhl/install-extension 'evil)
;;
;; - undo-tree
;;
;;   (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
;;   (vhl/install-extension 'undo-tree)


;;; Change Log:
;;
;; v1.16 Sat Sep 14 06:57:51 2024 JST
;;   - This release is a maintenance release to support new versions of Emacs.
;;     There are no notable new features, but the following fixes have been made.
;;   - Translate `defadvice' to `advice-add'.
;;   - Fixed a bug, `vhl/ext/occur' does not make highlights properly.
;;   - 'occur' command on Emacs >= 28 has volatile highlights feature, so
;;     'vhl/ext/occur' is not required. (fix #26)
;;   - Adapt to lexical binding.
;;   - Add year 2024 to copyright line.
;;   - Use `cl-lib'. (fix #21 and #23)
;;
;; v1.15 Sun Jun 12 10:40:31 2016 JST
;;   - Update documents, example snippets for other packages,
;;     regarding #14.
;;
;; v1.14 Sun Jun 12 10:12:30 2016 JST
;;   - Update documents, especially supporting `evil-mode',
;;     regarding #13.
;;   - Fixed a bug #14, an extension won't be loaded properly
;;     when it was installed by `vhl/install-extension'.
;;
;; v1.13 Sat May 21 11:02:36 2016 JST
;;   - Fixed a bug that highlighting was not working with nested volatile
;;     highlighting aware operations like `yak-pop'.
;;
;; v1.12  Sun Feb 21 19:09:29 2016 JST
;;   - Added autoload cookie.
;;
;; v1.11  Sun Oct  5 13:05:38 2014 JST
;;   - Fixed an error "Symbol's function definition is void: return",
;;     that occurs when highlight being created with `hideshow' commands.
;;
;; v1.10  Thu Mar 21 22:37:27 2013 JST
;;   - Use inherit in face definition when detected.
;;   - Suppress compiler warnings regarding to emacs/xemacs private
;;     functions by file local variable.
;;
;; v1.9  Tue Mar  5 00:52:35 2013 JST
;;   - Fixed errors in shell caused by dummy functions.
;;
;; v1.8  Wed Feb 15 00:08:14 2012 JST
;;   - Added "Contributed by: " line in header.
;;   - Added extension for hideshow.
;;
;; v1.7  Mon Feb 13 23:31:18 2012 JST
;;   - Fixed a bug required features are not loaded.
;;
;; v1.6  Thu Feb  2 06:59:48 2012 JST
;;   - Removed extensions for non standard features.
;;   - Suppress compiler warning "function `vhl/.make-list-string'
;;     defined multiple times".
;;   - Fixed compiler error "Symbol's function definition is void:
;;     vhl/.make-list-string".
;;
;;  v1.5  Tue Jan 31 22:19:04 2012 JST
;;   - Added extension for highlighting the position where text was
;;     killed from.
;;   - Added extension for highlighting the position where text was
;;     deleted from.
;;   - Provide a macro `vhl/define-extension' for easily defining new
;;     simple extensions with a single line of code. For usage
;;     examples, see the definitions of the undo, yank, kill, and
;;     delete extensions.
;;
;;  v1.4  Sun Jan 15 20:23:58 2012 JST
;;   - Suppress compiler warnings regarding to emacs/xemacs private
;;     functions.
;;   - Fixed bugs which occurs to xemacs.
;;
;;  v1.3, Sat Dec 18 16:44:14 2010 JST
;;   - Added extension for non-incremental search operations.
;;   - Fixed a bug that highlights won't be appear when
;;     occurrences is in folded line.
;;
;;  v1.2, Tue Nov 30 01:07:48 2010 JST
;;   - In `vhl/ext/occur', highlight all occurrences.
;;
;;  v1.1, Tue Nov  9 20:36:09 2010 JST
;;   - Fixed a bug that mode toggling feature was not working.

;;; Code:

(defconst vhl/version "1.16")

(eval-when-compile
  (require 'cl-lib)
  (require 'easy-mmode)
  (require 'advice))

(provide 'volatile-highlights)

 
;;;============================================================================
;;;
;;;  Private Variables.
;;;
;;;============================================================================

(eval-and-compile
  (defconst vhl/.xemacsp (string-match "XEmacs" emacs-version)
    "A flag if the emacs is xemacs or not."))

(defvar vhl/.hl-lst nil
  "List of volatile highlights.")

 
;;;============================================================================
;;;
;;;  Faces.
;;;
;;;============================================================================

(defgroup volatile-highlights nil
  "Visual feedback on operations."
  :group 'editing)


;; Borrowed from `slime.el'.
(defun vhl/.face-inheritance-possible-p ()
  "Return true if the :inherit face attribute is supported."
  (assq :inherit custom-face-attributes))

(defface vhl/default-face
  (cond
   ((or vhl/.xemacsp
        (not (vhl/.face-inheritance-possible-p)))
    '((((class color) (background light))
       (:background "yellow1"))
      (((class color) (background dark))
       (:background "SkyBlue4"))
      (t :inverse-video t)))
   (t
    '((t
       :inherit secondary-selection
       ))))
    "Face used for volatile highlights."
    :group 'volatile-highlights)

 
;;;============================================================================
;;;
;;;  Minor Mode Definition.
;;;
;;;============================================================================
;;;###autoload
(easy-mmode-define-minor-mode
 volatile-highlights-mode "Minor mode for visual feedback on some operations."
 :global t
 :init-value nil
 :lighter " VHl"
 (if volatile-highlights-mode
     (vhl/load-extensions)
   (vhl/unload-extensions)))


(defcustom Vhl/highlight-zero-width-ranges nil
  "If t, highlight the positions of zero-width ranges.

For example, if a deletion is highlighted, then the position
where the deleted text used to be would be highlighted."
  :type 'boolean
  :group 'volatile-highlights)

 
;;;============================================================================
;;;
;;;  Public Functions/Commands.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; (vhl/add-range BEG END &OPTIONAL BUF FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/add-range (beg end &optional buf face)
  "Add a volatile highlight to the buffer `BUF' at the position
specified by `BEG' and `END' using the face `FACE'.

When the buffer `BUF' is not specified or its value is `nil',
volatile highlight will be added to current buffer.

When the face `FACE' is not specified or its value is `nil',
the default face `vhl/default-face' will
be used as the value."
  (let* ((face (or face 'vhl/default-face))
		 (hl (vhl/.make-hl beg end buf face)))
	(setq vhl/.hl-lst
		  (cons hl vhl/.hl-lst))
	(add-hook 'pre-command-hook 'vhl/clear-all)))
(define-obsolete-function-alias 'vhl/add 'vhl/add-range "1.5")

;;-----------------------------------------------------------------------------
;; (vhl/add-position POS &OPTIONAL BUF FACE) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/add-position (pos &rest other-args)
  "Highlight buffer position POS as a change.

If Vhl/highlight-zero-width-ranges is nil, do nothing.

Optional args are the same as `vhl/add-range'."
  (when (and Vhl/highlight-zero-width-ranges (not (zerop (buffer-size))))
    (when (> pos (buffer-size))
        (setq pos (- pos 1)))
    (apply 'vhl/add-range pos (+ pos 1) other-args)))

;;-----------------------------------------------------------------------------
;; (vhl/clear-all) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/clear-all ()
  "Clear all volatile highlights."
  (interactive)
  (while vhl/.hl-lst
	(vhl/.clear-hl (car vhl/.hl-lst))
	(setq vhl/.hl-lst
		  (cdr vhl/.hl-lst)))
	  (remove-hook 'pre-command-hook 'vhl/clear-all))

;;-----------------------------------------------------------------------------
;; (vhl/force-clear-all) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/force-clear-all ()
  "Force clear all volatile highlights in current buffer."
  (interactive)
  (vhl/.force-clear-all-hl))

 
;;;============================================================================
;;;
;;;  Private Functions.
;;;
;;;============================================================================

;;-----------------------------------------------------------------------------
;; (vhl/.make-hl BEG END BUF FACE) => HIGHLIGHT
;;-----------------------------------------------------------------------------
(defun vhl/.make-hl (beg end buf face)
  "Make a volatile highlight at the position specified by `BEG' and `END'."
  (let (hl)
	(cond
	 (vhl/.xemacsp
	  ;; XEmacs
	  (setq hl (make-extent beg end buf))
	  (set-extent-face hl face)
	  (highlight-extent hl t)
	  (set-extent-property hl 'volatile-highlights t))
	 (t
	  ;; GNU Emacs
	  (setq hl (make-overlay beg end buf))
	  (overlay-put hl 'face face)
	  (overlay-put hl 'priority 1)
	  (overlay-put hl 'volatile-highlights t)))
	 hl))

;;-----------------------------------------------------------------------------
;; (vhl/.clear-hl HIGHLIGHT) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/.clear-hl (hl)
  "Clear one highlight."
  (cond
   ;; XEmacs (not tested!)
   (vhl/.xemacsp
	(and (extentp hl)
		 (delete-extent hl)))
   ;; GNU Emacs
   (t
	(and (overlayp hl)
		 (delete-overlay hl)))))

;;-----------------------------------------------------------------------------
;; (vhl/.force-clear-all-hl) => VOID
;;-----------------------------------------------------------------------------
(defun vhl/.force-clear-all-hl ()
  "Force clear all volatile highlights in current buffer."
  (cond
   ;; XEmacs (not tested!)
   (vhl/.xemacsp
      (map-extents (lambda (hl _maparg)
                     (and (extent-property hl 'volatile-highlights)
						  (vhl/.clear-hl hl)))))
   ;; GNU Emacs
   (t
	(save-restriction
	  (widen)
	  (mapcar (lambda (hl)
				(and (overlay-get hl 'volatile-highlights)
					 (vhl/.clear-hl hl)))
			  (overlays-in (point-min) (point-max)))))))

 
;;;============================================================================
;;;
;;;  Functions to manage extensions.
;;;
;;;============================================================================
(defvar vhl/.installed-extensions nil)

(defun vhl/install-extension (sym)
  (let ((_fn-on  (intern (format "vhl/ext/%s/on" sym)))
        (_fn-off (intern (format "vhl/ext/%s/off" sym)))
        (cust-name (intern (format "vhl/use-%s-extension-p" sym))))
    (cl-pushnew sym vhl/.installed-extensions)
    (eval `(defcustom ,cust-name t
             ,(format "A flag if highlighting support for `%s' is on or not." sym)
             :type 'boolean
             :group 'volatile-highlights
             :set (lambda (sym-to-set val)
                    (set-default sym-to-set val)
                    (if val
                        (when volatile-highlights-mode
                          (vhl/load-extension (quote ,sym)))
                      (vhl/unload-extension (quote ,sym))))))))

(defun vhl/load-extension (sym)
  (let ((fn-on  (intern (format "vhl/ext/%s/on" sym)))
        (cust-name (intern (format "vhl/use-%s-extension-p" sym))))
    (if (functionp fn-on)
        (when (and (boundp cust-name)
                   (eval cust-name))
          (apply fn-on nil))
      (message "[vhl] No load function for extension  `%s'" sym))))

(defun vhl/unload-extension (sym)
  (let ((fn-off (intern (format "vhl/ext/%s/off" sym))))
    (if (functionp fn-off)
        (apply fn-off nil)
      (message "[vhl] No unload function for extension  `%s'" sym))))

(defun vhl/load-extensions ()
  (dolist (sym vhl/.installed-extensions)
    (vhl/load-extension sym)))

(defun vhl/unload-extensions ()
  (dolist (sym vhl/.installed-extensions)
    (vhl/unload-extension sym)))

 
;;;============================================================================
;;;
;;;  Utility functions/macros for extensions.
;;;
;;;============================================================================
(defvar vhl/.after-change-hook-depth 0)

(defun vhl/.push-to-after-change-hook (_fn-name)
  ;; Debug
  ;; (if (zerop vhl/.after-change-hook-depth)
  ;;     (message "vlh: push: %s" _fn-name)
  ;;   (message "vlh: skip push: %s" _fn-name))
  (when (zerop vhl/.after-change-hook-depth)
    (add-hook 'after-change-functions
              'vhl/.make-vhl-on-change))
  (setq vhl/.after-change-hook-depth
        (1+ vhl/.after-change-hook-depth)))

(defun vhl/.pop-from-after-change-hook (_fn-name)
  (setq vhl/.after-change-hook-depth
        (1- vhl/.after-change-hook-depth))
  ;; Debug
  ;; (if (zerop vhl/.after-change-hook-depth)
  ;;     (message "vlh: pop: %s" _fn-name)
  ;;   (message "vlh: skip pop: %s" _fn-name))
  (when (zerop vhl/.after-change-hook-depth)
    (remove-hook 'after-change-functions
                 'vhl/.make-vhl-on-change)))

(defun vhl/.make-vhl-on-change (beg end len-removed)
  (let ((insert-p (zerop len-removed)))
    (if insert-p
        ;; Highlight the insertion
        (vhl/add-range beg end)
      ;; Highlight the position of the deletion
      (vhl/add-position beg))))

(defmacro vhl/give-advice-to-make-vhl-on-changes (fn-name)
  (let* ((ad-name (intern (concat "vhl/.advice-callback-fn/.make-vhl-on-"
                                  (format "%s" fn-name))))
         (g-orig-fn  (gensym))
         (g-orig-ret (gensym))
         (g-args     (gensym)))
    (or (symbolp fn-name)
        (error "vhl/give-advice-to-make-vhl-on-changes: `%s' is not type of symbol." fn-name))
    `(progn
       (defun ,ad-name (,g-orig-fn &rest ,g-args)
         (let (,g-orig-ret)
           (vhl/\.push-to-after-change-hook (quote ,fn-name))
           (unwind-protect (setq ,g-orig-ret (apply ,g-orig-fn ,g-args))
             (vhl/\.pop-from-after-change-hook (quote ,fn-name)))
           ,g-orig-ret))
       (advice-add  (quote ,fn-name) :around (function ,ad-name)))))

(defmacro vhl/cancel-advice-to-make-vhl-on-changes (fn-name)
  (let ((ad-name (intern (concat "vhl/.advice-callback-fn/.make-vhl-on-"
                                 (format "%s" fn-name)))))
    `(advice-remove (quote ,fn-name) (quote ,ad-name))))

(defun vhl/require-noerror (feature &optional _filename)
  (condition-case _c
      (require feature)
    (file-error nil)))

(eval-and-compile
;; Utility function by Ryan Thompson.
(defun vhl/.make-list-string (items)
  "Makes an English-style list from a list of strings.

Converts a list of strings into a string that lists the items
separated by commas, as well as the word `and' before the last
item. In other words, returns a string of the way those items
would be listed in english.

This is included as a private support function for generating
lists of symbols to be included docstrings of auto-generated
extensions."
  (cl-assert (listp items))
  (cond ((null items)
         ;; Zero items
         "")
        ((null (cdr items))
         ;; One item
         (cl-assert (stringp (cl-first items)))
         (format "%s" (cl-first items)))
        ((null (cddr items))
         ;; Two items
         (cl-assert (stringp (cl-first items)))
         (cl-assert (stringp (cl-second items)))
         (apply 'format "%s and %s" items))
        ((null (cdddr items))
         ;; Three items
         (cl-assert (stringp (cl-first items)))
         (cl-assert (stringp (cl-second items)))
         (cl-assert (stringp (cl-third items)))
         (apply 'format "%s, %s, and %s" items))
        (t
         ;; 4 or more items
         (format "%s, %s" (cl-first items) (vhl/.make-list-string (cl-rest items)))))))

;; The following makes it trivial to define simple vhl extensions
(defmacro vhl/define-extension (name &rest functions)
  "Define a VHL extension called NAME that applies standard VHL
  advice to each of FUNCTIONS."
  (cl-assert (cl-first functions))
  (let* ((name-string (symbol-name (eval name)))
         (function-list-string (vhl/.make-list-string
                                (mapcar (lambda (f) (format "`%s'" (symbol-name (eval f))))
                                        functions)))
         (on-function-name (intern (format "vhl/ext/%s/on" name-string)))
         (on-body-form (cons
                        'progn
                        (mapcar (lambda (f)
                                  `(vhl/give-advice-to-make-vhl-on-changes ,(eval f)))
                                functions)))
         (on-doc-string (format "Turn on volatile highlighting for %s." function-list-string))

         (off-function-name (intern (format "vhl/ext/%s/off" name-string)))
         (off-body-form (cons
                         'progn
                         (mapcar (lambda (f)
                                   `(vhl/cancel-advice-to-make-vhl-on-changes ,(eval f)))
                                 functions)))
         (off-doc-string (format "Turn off volatile highlighting for %s." function-list-string)))
    `(progn
       (defun ,on-function-name ()
         ,on-doc-string
         (interactive)
         ,on-body-form)
       (defun ,off-function-name ()
         ,off-doc-string
         (interactive)
         ,off-body-form)
       nil)))

 
;;;============================================================================
;;;
;;;  Extensions.
;;;
;;;============================================================================
 
;;-----------------------------------------------------------------------------
;; Extension for supporting undo.
;;   -- Put volatile highlights on the text inserted by `undo'.
;;      (and may be `redo'...)
;;-----------------------------------------------------------------------------

(vhl/define-extension 'undo 'primitive-undo)
(vhl/install-extension 'undo)

 
;;-----------------------------------------------------------------------------
;; Extension for supporting yank/yank-pop.
;;   -- Put volatile highlights on the text inserted by `yank' or `yank-pop'.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'yank 'yank 'yank-pop)
(vhl/install-extension 'yank)

;;-----------------------------------------------------------------------------
;; Extension for supporting kill.
;;   -- Put volatile highlights on the positions where killed text
;;      used to be.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'kill 'kill-region)
(vhl/install-extension 'kill)

;;-----------------------------------------------------------------------------
;; Extension for supporting `delete-region'.
;;   -- Put volatile highlights on the positions where deleted text
;;      used to be. This is not so reliable since `delete-region' is
;;      an inline function and is pre-compiled sans advice into many
;;      other deletion functions.
;;-----------------------------------------------------------------------------

(vhl/define-extension 'delete 'delete-region)
(vhl/install-extension 'delete)

 
;;-----------------------------------------------------------------------------
;; Extension for supporting etags.
;;   -- Put volatile highlights on the tag name which was found by `find-tag'.
;;-----------------------------------------------------------------------------
(defun vhl/ext/etags/.after-find-tag (tagname &optional _next-p _regexp-p)
    (let ((len (length tagname)))
      (save-excursion
        (search-forward tagname)
        (vhl/add-range (- (point) len) (point)))))

  (defun vhl/ext/etags/on ()
  "Turn on volatile highlighting for `etags'."
  (interactive)
  (require 'etags)
  (advice-add 'find-tag :after #'vhl/ext/etags/.after-find-tag))

(defun vhl/ext/etags/off ()
  "Turn off volatile highlighting for `etags'."
  (interactive)
  (advice-remove 'find-tag #'vhl/ext/etags/.after-find-tag))

(vhl/install-extension 'etags)

 
;;-----------------------------------------------------------------------------
;; Extension for supporting occur.
;;   -- Put volatile highlights on occurrence which is selected by
;;      `occur-mode-goto-occurrence' or `occur-mode-display-occurrence'.
;;-----------------------------------------------------------------------------
(defvar vhl/ext/occur/*occur-str* "") ;; Text in current line.

(defun vhl/ext/occur/.before-hook-fn (&rest _args)
  (save-excursion
    (let* ((bol (progn (beginning-of-line) (point)))
           (eol (progn (end-of-line) (point))))
      (setq vhl/ext/occur/*occur-str* (and bol eol
                                           ;; Skip line number.
                                           (replace-regexp-in-string
                                            "^[ \t]*[0-9]+:" ""
                                            (buffer-substring bol eol)))))))

(defun vhl/ext/occur/.find-face-ranges-in-str (str face)
  "Find ranges where the specified FACE is applied in STR.
Returns a list of (beg . end), or nil if not found."
  (let ((ptr 0)
        (len (length str))
        be-lst be)
    (while (/= ptr len)
      (setq be (vhl/ext/occur/.find-face-ranges-in-str-aux str face ptr))
      (if (= (car be) len)
          (setq ptr len)
        (setq be-lst (cons be be-lst))
        (setq ptr (cdr be))))
    (reverse be-lst)))

(defun vhl/ext/occur/.str-has-face-at-pos-p (str face pos)
  (let ((found-face (get-text-property pos 'face str)))
    (cond
     ((listp found-face)
      (member face found-face))
     ((atom found-face)
      (eq face found-face))
     (t nil))))

(defun vhl/ext/occur/.find-face-ranges-in-str-aux (str face pos)
  (let ((ptr pos)
        (len (length str))
        beg end)
    
    ;; Find beggining of face range
    (when (vhl/ext/occur/.str-has-face-at-pos-p str face ptr)
      (setq beg ptr))
    
    (while (not beg)
      (setq ptr (next-single-property-change ptr 'face str len))
      (if (vhl/ext/occur/.str-has-face-at-pos-p str face ptr)
          (setq beg ptr)
        (when (= ptr len) (setq beg ptr))))
    
    ;; Find end of face range
    (while (not end)
      (setq ptr (next-single-property-change ptr 'face str len))
      (if (not (vhl/ext/occur/.str-has-face-at-pos-p str face ptr))
          (setq end ptr)
        (when (= ptr len) (setq end ptr))))
    (cons beg end)))

(defun vhl/ext/occur/.after-hook-fn (&rest _args)
  (let ((marker (and vhl/ext/occur/*occur-str*
                     (get-text-property 0 'occur-target vhl/ext/occur/*occur-str*)))
        (be-lst nil))
    (when marker
      ;; Detect position of each occurrence by scanning face
      ;; `list-matching-lines-face' put on them.
      (setq be-lst
            (vhl/ext/occur/.find-face-ranges-in-str vhl/ext/occur/*occur-str*
                                                    list-matching-lines-face))
      ;; Put volatile highlights on occurrences.
      (with-current-buffer (marker-buffer marker)
        (let* ((bol (save-excursion
                      (goto-char (marker-position marker))
                      (beginning-of-line)
                      (point))))
          (dolist (be be-lst)
            (let ((pt-beg (+ bol (car be)))
                  (pt-end (+ bol (cdr be))))
              ;; When the occurrence is in folded line,
              ;; put highlight over whole line which
              ;; contains folded part.
              (dolist (ov (overlays-at pt-beg))
                (when (overlay-get ov 'invisible)
                  ;;(message "INVISIBLE: %s" ov)
                  (save-excursion
                    (goto-char (overlay-start ov))
                    (beginning-of-line)
                    (setq pt-beg (min pt-beg (point)))
                    (goto-char (overlay-end ov))
                    (end-of-line)
                    (setq pt-end (max pt-end (point))))))

              (vhl/add-range pt-beg
                             pt-end
                             nil
                             list-matching-lines-face))))))))

(defun vhl/ext/occur/on ()
  "Turn on volatile highlighting for `occur'."
  (interactive)

  (if (< emacs-major-version 28)
      (progn
        (advice-add 'occur-mode-goto-occurrence :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-goto-occurrence :after #'vhl/ext/occur/.after-hook-fn)
        
        (advice-add 'occur-mode-display-occurrence :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-display-occurrence :after #'vhl/ext/occur/.after-hook-fn)
        
        (advice-add 'occur-mode-goto-occurrence-other-window :before #'vhl/ext/occur/.before-hook-fn)
        (advice-add 'occur-mode-goto-occurrence-other-window :after #'vhl/ext/occur/.after-hook-fn))
    (message "`occur' command on Emacs >= 28 has volatile highlight feature, so `vhl/ext/occur' is not required.")))

(defun vhl/ext/occur/off ()
  "Turn off volatile highlighting for `occur'."
  (interactive)

  (when (< emacs-major-version 28)
    (advice-remove 'occur-mode-goto-occurrence #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-goto-occurrence #'vhl/ext/occur/.after-hook-fn)
    
    (advice-remove 'occur-mode-display-occurrence #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-display-occurrence #'vhl/ext/occur/.after-hook-fn)
    
    (advice-remove 'occur-mode-goto-occurrence-other-window #'vhl/ext/occur/.before-hook-fn)
    (advice-remove 'occur-mode-goto-occurrence-other-window #'vhl/ext/occur/.after-hook-fn)))

;; `occur' command on Emacs >= 28 has volatile highlight feature,
;; so `vhl/ext/occur' is not required.
(when (< emacs-major-version 28)
  (vhl/install-extension 'occur))

 
;;-----------------------------------------------------------------------------
;; Extension for non-incremental search operations.
;;   -- Put volatile highlights on the text found by non-incremental search
;;      operations.
;;-----------------------------------------------------------------------------

(defun vhl/ext/nonincremental-search/.filter-return-fn (retval)
  (when retval
    (vhl/add-range (match-beginning 0) (match-end 0) nil 'match)))

(defun vhl/ext/nonincremental-search/on ()
  "Turn on volatile highlighting for non-incremental search operations."
  (interactive)
  (when (vhl/require-noerror 'menu-bar nil)
    (advice-add 'nonincremental-search-forward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-search-backward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-re-search-forward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-re-search-backward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-repeat-search-forward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-add 'nonincremental-repeat-search-backward :filter-return #'vhl/ext/nonincremental-search/.filter-return-fn)))

(defun vhl/ext/nonincremental-search/off ()
  "Turn off volatile highlighting for  non-incremental search operations."
  (interactive)
  (when (vhl/require-noerror 'menu-bar nil)
    (advice-remove 'nonincremental-search-forward #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-search-backward #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-re-search-forward #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-re-search-backward #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-repeat-search-forward #'vhl/ext/nonincremental-search/.filter-return-fn)
    (advice-remove 'nonincremental-repeat-search-backward #'vhl/ext/nonincremental-search/.filter-return-fn)))

(vhl/install-extension 'nonincremental-search)

 
;;-----------------------------------------------------------------------------


;; Extension for hideshow.
;;   -- Put volatile highlights on the text blocks which are shown/hidden
;;      by hideshow.
;;-----------------------------------------------------------------------------

(defun vhl/ext/hideshow/vhl/around-hook-fn (orig-fn &rest args)
  (let* ((bol (save-excursion (progn (beginning-of-line) (point))))
         (eol (save-excursion (progn (end-of-line) (point))))
         (ov-folded (car (delq nil
                               (mapcar #'(lambda (ov)
                                           (and (overlay-get ov 'hs)
                                                ov))
                                       (overlays-in bol (1+ eol))))))
         (boov (and ov-folded (overlay-start ov-folded)))
         (eoov (and ov-folded (overlay-end ov-folded)))
         retval)

    (setq retval (apply orig-fn args))

    (when (and boov eoov)
      (vhl/add-range boov eoov))
    retval))

(defun vhl/ext/hideshow/.activate ()
  (advice-add 'hs-show-block :around #'vhl/ext/hideshow/vhl/around-hook-fn))

(defun vhl/ext/hideshow/on ()
  "Turn on volatile highlighting for `hideshow'."
  (interactive)

  (cond
   ((featurep 'hideshow)
    (vhl/ext/hideshow/.activate))
   (t
    (eval-after-load "hideshow" '(vhl/ext/hideshow/.activate)))))

(defun vhl/ext/hideshow/off ()
  (advice-remove 'hs-show-block #'vhl/ext/hideshow/vhl/around-hook-fn))

(vhl/install-extension 'hideshow)

 
;;;============================================================================
;;;
;;;  Suppress compiler warnings regarding to emacs/xemacs private functions.
;;;
;;;============================================================================

;; Local variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; volatile-highlights.el ends here
