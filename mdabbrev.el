;;; mdabbrev.el -- Dynamic abbreviation expansion in the middle of a word

;; Copyright (C) 2008  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 03 Dec 2008
;; Version: 1.0
;; Keywords: abbrev expand completion convenience

;; This file is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Dynamic abbreviation expansion in the middle of a word.  For example, if
;; the cursor is on the letter 'b' in "foo_bar" and the word "foo_quux_bar" is
;; in the current or other buffers, "quux_" will be inserted.  You can cycle
;; through all expansions just like dabbrev.  This also works at the beginning
;; and end of words, although at the end it's not as smart as regular dabbrev;
;; e.g. you can't hit space and have it continue to add more text.

;; 03 Dec 2008 -- v1.0
;;                Initial release

;;; Code:

(require 'dabbrev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar mdabbrev-orig-pos nil
  "Original position, i.e. where the expansion will happen.")

(defvar mdabbrev-start-pos nil
  "Starting point of the word being expanded.")

(defvar mdabbrev-end-marker nil
  "Marker at end of word being expanded.")

(defvar mdabbrev-last-search-marker nil
  "Marker of last place searched.")

(defvar mdabbrev-regexp nil
  "Search regexp.")

(defvar mdabbrev-prev-sub-str-list nil
  "Previous substitutions that were tried.")

(defvar mdabbrev-last-sub-str ""
  "Last substitution string tried.")

(defvar mdabbrev-direction nil
  "Direction being searched.")

(defvar mdabbrev-searching-other-buffers nil
  "Buffers other than the original are being searched.")

(defvar mdabbrev-buffer-search-list nil
  "List of buffers being searched.")

(defvar mdabbrev-start-over nil
  "No substitution was acceptable, start over.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

;;;###autoload
(defun mdabbrev-expand ()
  "dabbrev-like completion, but in the middle (or beginning/end) of a word."
  (interactive)
  (when (or mdabbrev-start-over (not (equal this-command last-command)))
    (mdabbrev-setup))
  (let ((sub-str (mdabbrev-find-next)))
    (if sub-str
        (progn
          (dabbrev--substitute-expansion mdabbrev-last-sub-str "" sub-str t)
          (setq mdabbrev-last-sub-str sub-str))
      (delete-region mdabbrev-orig-pos (point))
      (setq mdabbrev-start-over t)
      (error "No further middle dynamic expansions for `%s' found"
             (buffer-substring-no-properties mdabbrev-start-pos
                                             (marker-position mdabbrev-end-marker))))))

(defun mdabbrev-setup ()
  "Setup mdabbrev."
  (setq mdabbrev-orig-pos (point))
  (setq mdabbrev-prev-sub-str-list nil)
  (setq mdabbrev-last-sub-str "")
  (setq mdabbrev-direction 'up)
  (setq mdabbrev-searching-other-buffers nil)
  (setq mdabbrev-buffer-search-list nil)
  (setq mdabbrev-start-over nil)
  (save-excursion
    (skip-syntax-backward "w_")
    (setq mdabbrev-start-pos (point))
    (setq mdabbrev-last-search-marker (point-marker))
    (setq mdabbrev-regexp (concat "\\_<"
                                  (buffer-substring-no-properties (point) mdabbrev-orig-pos)
                                  "\\(\\(\\sw\\|\\s_\\)+?\\)"))
    (skip-syntax-forward "w_")
    (setq mdabbrev-end-marker (point-marker))
    (setq mdabbrev-regexp (concat mdabbrev-regexp
                                  (buffer-substring-no-properties mdabbrev-orig-pos (point))
                                  "\\_>"))))

(defun mdabbrev-find-next ()
  "Find next expansion"
  (let (sub-str)
    (save-excursion
      (set-buffer (marker-buffer mdabbrev-last-search-marker))
      (goto-char (marker-position mdabbrev-last-search-marker))
      (setq sub-str (mdabbrev-search))
      (when (and (not sub-str) (equal mdabbrev-direction 'up))
        (setq mdabbrev-direction 'down)
        (goto-char (marker-position mdabbrev-end-marker))
        (setq sub-str (mdabbrev-search)))
      (unless sub-str
        (unless mdabbrev-searching-other-buffers
          (setq mdabbrev-buffer-search-list (dabbrev--make-friend-buffer-list))
          (setq mdabbrev-searching-other-buffers t))
        (when mdabbrev-buffer-search-list
          (move-marker mdabbrev-last-search-marker (point-min) (car mdabbrev-buffer-search-list))
          (setq mdabbrev-buffer-search-list (cdr mdabbrev-buffer-search-list))
          (setq sub-str (mdabbrev-find-next))))
      (move-marker mdabbrev-last-search-marker (point)))
    sub-str))

(defun mdabbrev-search ()
  "Search for an expansion, return it if one is found"
  (catch 'done
    (let ((case-fold-search (if (equal dabbrev-case-fold-search 'case-fold-search)
                                case-fold-search
                              dabbrev-case-fold-search)))
      (while (if (equal mdabbrev-direction 'up)
                 (re-search-backward mdabbrev-regexp nil t)
               (re-search-forward mdabbrev-regexp nil t))
        (let ((sub-str (match-string-no-properties 1)))
          (unless (mdabbrev-already-found sub-str)
            (setq mdabbrev-prev-sub-str-list (cons sub-str mdabbrev-prev-sub-str-list))
            (throw 'done sub-str)))))))

(defun mdabbrev-already-found (sub-str)
  "Check if a sub-string has already been found."
  (let (found)
    (mapc (lambda (x)
            (when (string= sub-str x)
              (setq found t)))
          mdabbrev-prev-sub-str-list)
    found))

(provide 'mdabbrev)
;;; mdabbrev.el ends here
