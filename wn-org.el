;;; wn-org.el --- an org-mode interface for Word Net
;;
;; Author: Henry G. Weller <hweller0@gmail.com>
;; Maintainer: Henry G. Weller
;;
;; Created: Thu Jul 17 09:49:48 2008 (+0100)
;; Version: 0.1
;; Last-updated: Sat Apr 11 14:51:51 2009 (+0100)
;;           By: Henry Weller
;;     Update #: 2
;; URL: http://www.emacswiki.org/emacs/wn-org.el
;; Keywords: convenience
;; Compatibility: GNU Emacs 23.x (may work with earlier versions)
;;
;; This file is NOT part of Emacs.
;;
;; ----------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; An org-mode based interface for the Word Net dictionary.
;; By default all sections (synonyms, antonyms etc.) are generated in an
;; org-mode buffer to enable easy and convenient viewing.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wn-org)
;;
;; Suggested key binding:
;;   (global-set-key (kbd "M-s") 'wn-org-search)
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
;;; History
;;
;; wn-org.el is based on http://williamxu.net9.org/ref/wordnet.el by William Xu
;; but unlike wordnet.el presents the results of the search in org-mode which
;; is a good way to look through such a potentially large amount of related
;; information.
;
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
;;
;;; Code:

(provide 'wn-org)

(defgroup wn-org nil
  "Wordnet org-mode interface."
  :group 'wn-org)

(defcustom wn-org-command "wn"
  "Shell command for wordnet."
  :type 'string
  :group 'wn-org)

(defcustom wn-org-mode-hook nil
  "Normal hook run after entering wn-org mode."
  :type 'hook
  :group 'wn-org)

(defcustom wn-org-options
  " -over\
    -antsn -antsv -antsa -antsr\
    -hypen -hypev\
    -hypon -hypov\
    -entav\
    -synsn -synsv -synsa -synsr\
    -smemn\
    -ssubn\
    -sprtn\
    -membn\
    -subsn\
    -partn\
    -meron\
    -holon\
    -causv\
    -perta -pertr\
    -attrn -attra\
    -derin -deriv\
    -domnn -domnv -domna -domnr\
    -domtn -domtv -domta -domtr\
    -famln -famlv -famla -famlr\
    -framv\
    -coorn -coorv\
    -simsv\
    -hmern\
    -hholn\
    -grepn -grepv -grepa -grepr"
"Command line options for wn"
  :type 'string
  :group 'wn-org)

(defcustom wn-org-section-headings
  '("Antonyms" "Synonyms" "Hyponyms" "Troponyms"
    "Meronyms" "Holonyms" "Pertainyms"
    "Member" "Substance" "Part"
    "Attributes" "Derived" "Domain" "Familiarity"
    "Coordinate" "Grep" "Similarity"
    "Entailment" "'Cause To'" "Sample" "Overview")
  "Section headings
to which the `*' org-mode section label is added."
  :type 'list
  :group 'wn-org)


(define-derived-mode wn-org-mode
  org-mode "WordNet"
  "Major mode for WordNet dictionary search.
\\{wn-org-mode-map}"
  (run-hooks 'wn-org-mode-hook))

(define-key wn-org-mode-map (kbd "q") 'wn-org-quit)

(defun wn-org-quit ()
  "Kill Word Net windows and buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (delete-windows-on buffer)
    (kill-buffer buffer)))

(defun wn-org-replace-regexp (regexp-string repl-string)
  (goto-char (point-min))
  (while (re-search-forward regexp-string nil t)
    (replace-match repl-string t nil)))

(defun wn-org-search (word)
  "Search WordNet for WORD if provided otherwise prompt for it.
The word at the point is suggested which can be replaced."
  (interactive (list (read-string "Wordnet: " (current-word))))
  (unless word
    (setq word (read-string "Word: ")))
  (let ((buf (get-buffer-create "*WordNet*")))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (shell-command-to-string
          (format "%s %s %s" wn-org-command word wn-org-options)))
        (delete-matching-lines "^ *$" (point-min) (point-max))
        (wn-org-replace-regexp
         (concat "^" (regexp-opt wn-org-section-headings t))
         "* \\1")

        ;; Remove empty entries, those followed by a '*' on the next line
        (goto-char (point-min))
        (while (re-search-forward "^\\*.*\n\\*" nil t)
          (replace-match "*" t t)
          (backward-char) ;; Back over the '*' to remove following empty entries
          )

        (wn-org-replace-regexp "^Sense \\([1-9]*\\)" "  \\1. ")
        (wn-org-replace-regexp "       [=*]>" "    +")
        (unless (eq major-mode 'wn-org-mode)
          (wn-org-mode))
        (indent-region (point-min) (point-max))
        (fill-region (point-min) (point-max))
        (goto-char (point-min))
        ))
    ;; switch to *WordNet* buffer
    (unless (eq (current-buffer) buf)
      (unless (cdr (window-list))
        (split-window-vertically))
      (other-window 1)
      (switch-to-buffer buf))))

(defalias 'wn-org 'wn-org-search)

;; ----------------------------------------------------------------------------
;;; wn-org.el ends here
