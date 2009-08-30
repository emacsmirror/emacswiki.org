;;; org-pua.el --- Org pop-up annotation
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Sat Apr 11 14:54:53 2009 (+0100)
;; Version: 0.1
;; Last-Updated: Sat Apr 11 14:54:53 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 2
;; URL: http://www.emacswiki.org/emacs/org-pua.el
;; Keywords: org-mode, files, convenience
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;; This file is NOT part of Emacs.
;;
;;---------------------------------------------------------------------------- ;
;;; Commentary:
;;
;; This is a file annotation system using an org-mode file to store the
;; annotations separate to the original file.  The annotations can be displayed
;; either by jumping to the annotations file or by showing the annotated
;; locations in the file as button-links to the annotations which can either be
;; displayed in a tooltip or in the echo area.
;;
;; This package borrows ideas and code from:
;;
;;     `org-annotate-file.el' package by Philip Jackson (now supplied with
;;     `org-mode' in the `contrib' directory) from which the idea of using
;;     `org-mode' to store the annotations originates,
;;
;;     http://www.emacswiki.org/emacs/ipa.el by Tamas Patrovics from which the
;;     idea of displaying the annotations "in-place" originates,
;;
;;     http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg01726/rw-acronyms.el
;;     by Ralf Wachinger from which the idea of using `widget' buttons to enable
;;     the annotation to be displayed "in-place" on request
;;
;;
;;     http://www.emacswiki.org/emacs/tooltip-help.el by Tamas Patrovics which
;;     provides the mechanism to display the annotation tooltip at the `point'
;;     without the need to resort to mouse wrestling.
;;
;; To use put the following in your .emacs:
;;     (require 'org-pua)
;;
;; And for convenience bind the important functions to key of your choice, e.g.
;;     (global-set-key [(control c) f1] 'org-pua-annotate)
;;     (global-set-key [(control c) (control f1)] 'org-pua-toggle-buttons)
;;
;; To change the location of the annotation file:
;;     (setq org-pua-annotations-file "~/annotated.org")
;;
;; To view an existing annotation or add a new one corresponding to the current
;; file and location hit C-c C-l which will read the corresponding annotations
;; org file, automatically adding a link-headline for the annotation or if one
;; already exist positioning the `point' on the link-headline and narrowing the
;; buffer to show only that entry.
;;
;; To automatically insert a subheading corresponding to the current line set
;; `org-pua-add-line-entry' to `t'.
;;
;; ----------------------------------------------------------------------------
;;
;;; Change log:
;;
;; Version 0.1
;; * Initial release
;;
;; ----------------------------------------------------------------------------
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ----------------------------------------------------------------------------
;;; Code:

(provide 'org-pua)

(require 'org)
(require 'wid-edit)
(require 'tooltip-help)

;; ----------------------------------------------------------------------------
;;;  Customization variables

(defgroup org-pua nil
  "org-pua customization options."
  :tag "Org-pua"
  :group 'org
  :group 'files
  :group 'convenience)

(defcustom org-pua-annotations-file "~/.org-pua.org"
  "File in which to keep annotations.
Note that if `org-pua-annotations-dir' is set that directory rather than
this file will be used."
  :type 'string
  :group 'org-pua)

(defcustom org-pua-annotations-dir nil
  "Directory in which to keep annotations.
Note that if `org-pua-annotations-dir' is nil `org-pua-annotations-file'
will be used instead."
  :type 'string
  :group 'org-pua)

(defcustom org-pua-show-all t
  "If true always expand the full tree when you visit the annotations file."
  :type 'boolean
  :group 'org-pua)

(defcustom org-pua-add-line-entry t
  "If true add an entry in the annotations file corresponding to this line."
  :type 'boolean
  :group 'org-pua)

(defface org-pua-button-face
  '((t :background "palegoldenrod"))
  "Face for the annotations buttons that mark the text."
  :group 'org-pua)

;; ----------------------------------------------------------------------------
;;;  Button keymap

(defvar org-pua-button-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map [f1] 'org-pua-tooltip-show-annotation)
    (define-key map [S-f1] 'org-pua-show-annotation)
    map)
  "Keymap used by `org-pua-buttons'.")

;; ----------------------------------------------------------------------------
;;;  Support functions

(defun org-pua-elipsify-desc (string &optional after)
  "Strip starting and ending whitespace and replace any chars
that appear after the value in `after' with '...'"
  (let* ((after (number-to-string (or after 30)))
         (replace-map (list (cons "^[ \t]*" "")
                            (cons "[ \t]*$" "")
                            (cons (concat "^\\(.\\{" after
                                          "\\}\\).*") "\\1..."))))
    (mapc (lambda (x)
            (when (string-match (car x) string)
              (setq string (replace-match (cdr x) nil nil string))))
          replace-map)
    string))

(defun org-pua-button-get-annotation ()
  "Return annotation for org-pua button at the current position."
  (let* ((widget (widget-at (point)))
         (annotation (and widget (widget-get widget :annotation))))
    annotation))

(defun org-pua-name (filename)
  "Return the full path of the annotation org file corresponding to FILENAME."
  (if org-pua-annotations-dir
      (concat org-pua-annotations-dir filename ".org")
    org-pua-annotations-file))

(defun org-pua-add-upper-level (link)
  (goto-char (point-min))
  (call-interactively 'org-insert-heading)
  (insert link))

(defun org-pua-add-second-level (link)
  (goto-char (point-at-eol))
  (call-interactively 'org-insert-subheading)
  (insert link))

(defvar org-pua-buttons-visible nil
  "Variable to keep track of the visibility of the buttons.")

;; ----------------------------------------------------------------------------
;;;  User functions

(defun org-pua-annotate ()
  "Create/goto the annotation entry corresponding to the current file/line
in the corresponding annotations file."
  (interactive)
  (unless (buffer-file-name)
    (error "This buffer does not have an associated file."))
  (let* ((filename (abbreviate-file-name (buffer-file-name)))
         (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
         (link (org-make-link-string (concat "file:" filename) filename))
         (search-link (org-make-link-string
                       (concat "file:" filename "::" line)
                       (org-pua-elipsify-desc line)))
         (org-pua-name (org-pua-name buffer-file-name)))
    (when org-pua-annotations-dir
      (make-directory (file-name-directory org-pua-name) t))
    (with-current-buffer (find-file org-pua-name)
      (unless (org-mode-p)
        (org-mode))
      (widen)
      (goto-char (point-min))
      (when org-pua-show-all
        (show-all))
      (unless (search-forward-regexp
               (concat "^* " (regexp-quote link)) nil t)
        (org-pua-add-upper-level link))
      (beginning-of-line)
      (org-narrow-to-subtree)
      (when org-pua-add-line-entry
        (unless (search-forward-regexp
                 (concat "^** " (regexp-quote search-link)) nil t)
          (org-pua-add-second-level search-link))))))

(defun org-pua-buttons ()
  "Create buttons in this file from the annotations in corresponding
annotations file."
  (interactive)
  (unless (buffer-file-name)
    (error "This buffer does not have an associated file."))
  (org-pua-remove-buttons)
  (let* ((filename (abbreviate-file-name (buffer-file-name)))
         (file-link (org-make-link-string (concat "file:" filename) filename))
         (annot-file-name (org-pua-name buffer-file-name))
         end-of-file-annotations)
    (save-excursion
      (set-buffer (find-file-noselect annot-file-name))
      (unless (org-mode-p)
        (org-mode))
      (goto-char (point-min))
      (widen)
      ;; Search for the section corresponding to this file
      (re-search-forward (concat "^* " (regexp-quote file-link)) nil t)
      ;; Find the end of the section corresponding to this file
      (save-excursion
        (re-search-forward "^* " nil 1)
        (setq end-of-file-annotations (point)))
      ;; Loop over all the links for this file
      (while (re-search-forward org-any-link-re end-of-file-annotations t)
        (goto-char (match-beginning 0))
        (if (org-invisible-p)
            (org-show-context))

        (let (type link path line text annotation)
          (save-excursion
            (skip-chars-forward "^]\n\r")
            (when (org-in-regexp org-bracket-link-regexp)
              (setq link
                    (org-extract-attributes
                     (org-link-unescape (org-match-string-no-properties 1))))
              (while (string-match " *\n *" link)
                (setq link (replace-match " " t t link)))
              (setq link (org-link-expand-abbrev link))
              (when (string-match org-link-re-with-space3 link)
                (setq type (match-string 1 link)
                      path (match-string 2 link)))))

          (save-excursion
            (when (string= type "file")
              (if (string-match "::\\([0-9]+\\)\\'" path)
                  (setq line (string-to-number (match-string 1 path))
                     path (substring path 0 (match-beginning 0)))
                (if (string-match "::\\(.+\\)\\'" path)
                 (setq text (match-string 1 path)
                       path (substring path 0 (match-beginning 0)))))

              (forward-line 1)
              (let ((pos (point)))
                (re-search-forward "^[^ ]" nil 1)
                (setq annotation (buffer-substring pos (match-beginning 0))))

              (save-window-excursion
                (when (condition-case nil
                          (prog1 t
                            (org-open-file path t nil text))
                        (error nil))
                  (beginning-of-line)
                  (search-forward text nil t)
                  (widget-convert-button
                   'link
                   (match-beginning 0)
                   (match-end 0)
                   :path annot-file-name
                   :text text
                   :annotation annotation
                   :button-face 'org-pua-button-face
                   :action 'org-pua-goto-annotation
                   :help-echo '(lambda (widget) (widget-get widget :annotation))
                   :keymap org-pua-button-map
                   )
                  (setq org-pua-buttons-visible t)))
              ))
          )
        (end-of-line)))
    ))

(defun org-pua-remove-buttons ()
  "Remove all the annotation buttons in the current buffer."
  (interactive)
  (save-excursion
    (save-window-excursion
      (goto-char (point-min))
      (while (condition-case nil
                 (prog1 t
                   (widget-forward 1))
               (error nil))
        (let ((widget (widget-at nil)))
          (when (widget-get widget :annotation)
            (widget-delete widget))))))
  (setq org-pua-buttons-visible nil))

(defun org-pua-toggle-buttons ()
  "Toggle annotation buttons."
  (interactive)
  (if org-pua-buttons-visible
      (org-pua-remove-buttons)
    (org-pua-buttons)))

(defun org-pua-goto-annotation (widget &optional event)
  "Find the annotations file provided by the WIDGET for the current file and
search for the text stored in the WIDGET to which the annotation corresponds."
  (find-file-other-window (widget-get widget :path))
  (widen)
  (goto-char (point-min))
  (when org-pua-show-all
    (show-all))
  ;; Search for the section corresponding to this file
  (re-search-forward
   (concat "^* "
           (regexp-quote
            (org-make-link-string
             (concat "file:" buffer-file-name) buffer-file-name)))
   nil t)
  (search-forward (org-link-escape (widget-get widget :text)) nil t)
  (org-narrow-to-subtree))

(defun org-pua-show-annotation ()
  "Show the annotation for the current button in the echo area."
  (interactive)
  (let ((annotation (org-pua-button-get-annotation)))
    (if annotation
        (message "%s" annotation)
      (message "%s" "Cannot get annotation for button."))))

(defun org-pua-tooltip-show-annotation ()
  "Show the annotation for the current button in a tooltip."
  (interactive)
  (let ((annotation (org-pua-button-get-annotation)))
    (if annotation
        (th-show-tooltip-for-point annotation)
      (message "%s" "Cannot get annotation for button."))))

;;(org-pua-buttons)
;;(org-pua-remove-buttons)

;; ----------------------------------------------------------------------------
;;; org-pua.el ends here
