;;; ireplace.el --- replace functions invoked from isearch-mode

;; Copyright (C) 2003 Michael Schierl

;; Author: Michael Schierl <schierlm-public@gmx.de>
;; Created: 23 November 2003
;; Keywords: replace search matching
;; Version: 0.1a

(defconst ireplace-version "0.1a"
  "Version of ireplace.")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Sometimes it happens that you are in isearch mode - having entered
;; a larger regular expression - and notice that there are much more
;; hits than you expected.  In that case, you usually want to replace
;; them automatically.  One option to do that is setting
;; `query-replace-interactive', but this will be set for all
;; searches.  Another problem in these cases is that the regexp has
;; grown so fast that it is hard to see if the replacement does the
;; right things.  So in these cases, use ireplace.

;; When using regexp or normal isearch, hit C-RET or M-% to switch to
;; replace mode. This will ask you for a replacement-string and call
;; `query-replace' or `query-replace-regexp' afterwards. A prefix arg
;; to these keystrokes is passed to the `query-replace(-regexp)'
;; function.

;; When using regexp isearch, hit C-RET or M-% twice to switch to
;; ireplace mode. This will ask for a replacement as well, but the
;; first 10 matches and their replacements are shown in a separate
;; buffer while you type. Use a prefix arg to the second C-RET or M-%
;; to specify the number of results, or a simple C-u arg after you
;; typed a text to see the results but not update them.

;; In both cases, you can hit `M-e' to re-edit the search string.

;; When using regexp isearch, hit M-RET or C-M-% to switch to
;; `individual replace' mode. This will ask you for a replacement
;; string for each distinct match of the given regexp and then call
;; `query-replace-regexp-eval' to perform the replace.

;;; Bugs:
;; Minibuffer prompt does not change when you edit the search string.

;;; Code:

(require 'assoc)

(defvar ireplace-match-count 10)
(defvar ireplace-regexp nil)
(defvar ireplace-from nil)
(defvar ireplace-buffer nil)
(defvar ireplace-minibuffer nil)
(defvar ireplace-delim nil)

(defvar ireplace-read-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<C-return>") 'ireplace-show-changes)
    (define-key m (kbd "M-%") 'ireplace-show-changes)
    (define-key m (kbd "M-e") 'ireplace-edit-search)
    (set-keymap-parent m minibuffer-local-map)
    m)
  "Minibuffer keymap used for reading ireplace strings.")

;;;###autoload
(defun ireplace (arg)
  "Read a replace string for replacing an incremental search.
Hand prefix ARG to `query-replace' or `query-replace-regexp',
depending whether the isearch was in regexp mode or not.  You can edit
the search string with `M-e' and check what the replace would do (if
it is a regex replace) by hitting `M-%' or `M-RET'."
  (interactive "P")
  (let (prompt to)
    (setq ireplace-from isearch-string)
    (setq ireplace-regexp isearch-regexp)
    (isearch-done)
    (isearch-clean-overlays)
    (setq ireplace-delim arg)
    (setq ireplace-buffer (current-buffer))
    (setq prompt (concat (if ireplace-regexp
                             "Query replace regexp "
                           "Query replace ")
                         ireplace-from
                         " with: "))
    (save-excursion
      (setq to (read-from-minibuffer prompt nil ireplace-read-map nil
                                     query-replace-to-history-variable
                                     ireplace-from t)))
    (kill-buffer (get-buffer-create "*Ireplace*"))
    (if ireplace-regexp
        (query-replace-regexp ireplace-from to arg (point-min) (point-max))
      (query-replace ireplace-from to ireplace-delim (point-min)
                     (point-max)))))


(defun ireplace-after-change-function (&rest ignore)
  "After change function for ireplace.
Refresh the results list when the minibuffer is edited."
  (if (eq (current-buffer) ireplace-minibuffer)
      (ireplace-show-changes-0)))

(defun ireplace-cleanup (&rest ignore)
  "Remove minibuffer hooks when the minibuffer is exited.  IGNORE args."
  (remove-hook 'after-change-functions 'ireplace-after-change-function)
  (remove-hook 'minibuffer-exit-hook 'ireplace-cleanup))

(defun ireplace-show-changes (arg)
  "Show the results of the replace (a few examples).
With prefix ARG (non-numeric), do not update it automatically.  With a
numeric arg, show that many different replace strings."
  (interactive "P")
  (let ((add t))
    (cond
     ((not ireplace-regexp)
      (error "This does not make sense in non-regexp mode"))
     ((not arg)
      (setq add t))
     ((listp arg)
      (setq add nil))
     ((numberp arg)
      (setq add (not (minusp arg)))
      (setq ireplace-match-count (abs arg))))
    (add-hook 'minibuffer-exit-hook 'ireplace-cleanup)
    (setq ireplace-minibuffer (current-buffer))
    (if add
        (add-hook 'after-change-functions 'ireplace-after-change-function)
      (remove-hook 'after-change-functions 'ireplace-after-change-function))
    (ireplace-show-changes-0)
    ))

(defun ireplace-show-changes-0 ()
  "Internal function to show results of a replace."
  (let ((matches nil) (mbuf (current-buffer)) nm)
    (save-excursion
      (set-buffer ireplace-buffer)
      (goto-char (point-min))
      (while (and (< (length matches) ireplace-match-count)
                  (setq nm (ireplace-next-match)))
        (if (member nm matches)
            nil
          (setq matches (append matches (list nm))))))
    (while (< (length matches) ireplace-match-count)
      (setq matches (append matches '(""))))
    (with-current-buffer (get-buffer-create "*Ireplace*")
      (delete-region (point-min) (point-max))
      (mapc (lambda (x)
              (insert (concat x "\n")))
            matches))
    (pop-to-buffer (get-buffer-create "*Ireplace*"))
    (shrink-window-if-larger-than-buffer (get-buffer-window
                                          (get-buffer-create "*Ireplace*")))
    (pop-to-buffer mbuf)))

(defun ireplace-next-match ()
  "Compute the next match of the current regexp and its replacement."
  (let (ireplace-to)
    (save-excursion
      (set-buffer ireplace-minibuffer)
      (beginning-of-line nil)
      (setq ireplace-to (buffer-substring-no-properties (point) (point-max))))
    (if (re-search-forward ireplace-from ireplace-delim t)
        (let ((oldstring (match-string-no-properties 0))
              newstring)
          (condition-case nil
              (setq newstring (concat "\""
                                      (replace-regexp-in-string ireplace-from
                                                                ireplace-to
                                                                oldstring nil
                                                                ireplace-delim
                                                                0)
                                      "\""))
            (error (setq newstring "[incomplete input]")))
          (format "\"%s\" => %s" oldstring newstring))
      nil)))

(defun ireplace-edit-search ()
  "Edit the current search string."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (setq ireplace-from
          (read-from-minibuffer (concat "Edit search "
                                        (if ireplace-regexp
                                            "regexp: "
                                          "string: " ))
                                ireplace-from nil nil
                                query-replace-to-history-variable
                                ireplace-from t))))

;;;###autoload
(defun ireplace-individually (arg)
  "Replace all different matches of the search regexp individually.
This work (and the prefix ARG) is delegated to
`query-replace-individually'."
  (interactive "P")
  (unless isearch-regexp
    (error "This only works for regexp searches"))
  (setq ireplace-from isearch-string)
  (isearch-done)
  (isearch-clean-overlays)
  (query-replace-individually arg ireplace-from))

;;;###autoload
(defun query-replace-individually (arg from)
  "Replace all matches of FROM individually.
Ask for a replacement string for each (different) match string.
Hand ARG over to `query-replace-regexp-eval'.
When called interactively, ARG is the prefix arg and from is read from
the minibuffer."
  (interactive "P\nMQuery replace regexp individually: ")
  (let (matches match match-alist cnt)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward from nil t)
        (setq match (match-string-no-properties 0))
        (unless (member match matches)
          (setq matches (append matches (list match))))))
    (setq cnt (length matches))
    (mapc
     (lambda (m)
       (let (prompt newval)
         (setq cnt (1- cnt))
         (setq prompt (format "Query replace %s with (%d remaining): " m cnt))
         (setq newval (read-from-minibuffer prompt nil nil nil
                                            query-replace-to-history-variable
                                            m t))
         (if (string= newval "")
             (setq newval m))
         (aput 'match-alist m newval)))
     matches)
    (query-replace-regexp-eval from
                               '(let ((mm (match-string-no-properties 0)))
                                  (or (aget match-alist mm) mm))
                               arg (point-min) (point-max))))

;;;###autoload
(progn
  (define-key isearch-mode-map (kbd "<C-return>") 'ireplace)
  (define-key isearch-mode-map (kbd "M-%") 'ireplace)
  (define-key isearch-mode-map (kbd "C-M-%") 'ireplace-individually)
  (define-key isearch-mode-map (kbd "<M-return>") 'ireplace-individually))

(provide 'ireplace)

;;; ireplace.el ends here
