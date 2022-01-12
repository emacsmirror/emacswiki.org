;;; sortie.el --- Interactive sorting of completion candidates.   -*- lexical-binding:t -*-
;;
;; Filename: sortie.el
;; Description: Sorting of completion candidates.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2020-2022, Drew Adams, all rights reserved.
;; Created: Wed Jun  3 13:54:04 2020 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Wed Jan 12 14:43:51 2022 (-0800)
;;           By: dradams
;;     Update #: 217
;; URL: https://www.emacswiki.org/emacs/download/sortie.el
;; Doc URL: https://www.emacswiki.org/emacs/Sortie
;; Keywords: completion sorting cycling
;; Compatibility: GNU Emacs 24.x, 25.x, 26.x, 27.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Interactive sorting of completion candidates.
;;
;;  During completion you can use the key that is the value of option
;;  `sorti-cycle-key' (`C-,' by default) to cycle among a list of sort
;;  orders.
;;
;;  If you use a prefix arg with `sorti-cycle-key' then, instead of
;;  cycling to the next sort order, the current sort order is
;;  reversed.
;;
;;  To enable cycling of sort orders for your completion code, put
;;  function `sorti-bind-cycle-key-and-complete' or
;;  `sorti-bind-cycle-key' on `minibuffer-setup-hook', to make
;;  `sorti-cycle-key' available.  The former shows completion
;;  candidates initially.  The latter shows them only when you hit
;;  `TAB'.
;;
;;   (minibuffer-with-setup-hook #'sorti-bind-cycle-key-and-complete
;;     ...<COMPLETION CODE>...
;;     )
;;
;;  Your completion function can use the function that is the value of
;;  `sorti-sort-function-chooser' to dynamically set the display and
;;  cycling sort functions, as completion metadata entries
;;  `display-sort-function' and `cycle-sort-function'.
;;
;;  Here's an example.  It defines a command that prompts you to
;;  choose a completion candidate, showing them all initially, and
;;  then it echoes your choice.
;;
;;  The two sort orders used here are called `order1' and `order2'.
;;  They're associated with the string descriptions "alphabetical" and
;;  "by length", respectively, and they're inserted in a ring, for
;;  cycling.
;;
;;  Function `my-collection-fn' dynamically computes the completion
;;  candidates according to the current sort function, which is picked
;;  by `my-sort-fn-chooser' based on the current sort order,
;;  `sorti-current-order'.  Each sort function uses `sort', passing it
;;  a binary string-comparison predicate.
;;
;; (defun my-completion-with-sort-cycling ()
;;   "Read and echo choice using completion with sort-order cycling."
;;   (interactive)
;;   (minibuffer-with-setup-hook #'sorti-bind-cycle-key
;;     (let ((sorti-current-order          'order1)
;;           (sorti-sort-function-chooser  'my-sort-fn-chooser)
;;           (sorti-sort-orders-alist      '((order1 . "alphabetical")
;;                                           (order2 . "by length")))
;;           (sorti-sort-orders-ring       (let ((rng  (make-ring 4)))
;;                                           (ring-insert rng 'order1)
;;                                           (ring-insert rng 'order2)
;;                                           rng)))
;;       (message "RESULT: %S"
;;                (completing-read "fff: "
;;                                 (my-collection-fn
;;                                  '("aa" "bb" "ab" "aaff" "c")))))))
;;
;; (defun my-collection-fn (candidates)
;;   "Collection function that provides metadata for sorting.
;; Sorting is per the current value of `my-sort-fn-chooser'."
;;   (lambda (string pred action)
;;     (if (eq action 'metadata)
;;         (let ((order  (my-sort-fn-chooser)))
;;           `(metadata ,@(and order
;;                             `((display-sort-function . ,order)
;;                               (cycle-sort-function   . ,order)))))
;;       (complete-with-action action candidates string pred))))
;;
;; (defun my-sort-fn-chooser ()
;;   "Return sort function for current value of `sorti-current-order'."
;;   (if (eq 'order2 sorti-current-order)
;;       'my-sort-by-length
;;     'my-sort-alphabetically))
;;
;; (defun my-sort-alphabetically (candidates)
;;   "Sort CANDIDATES alphabetically"
;;   (let ((cands  (copy-sequence candidates)))
;;     (sort cands #'string<)))
;;
;; (defun my-sort-by-length (candidates)
;;   "Sort CANDIDATES by their length"
;;   (let ((cands  (copy-sequence candidates)))
;;     (sort cands (lambda (s1 s2) (< (length s1) (length s2))))))
;;
;;  For a more interesting example, see library `keysee.el'.  It
;;  provides key completion, and you can cycle among three sort
;;  orders: prefix keys first, local keys first, and alphabetically by
;;  command name.
;;
;;
;;  Command defined here:
;;
;;    `sorti-cycle-sort-order'.
;;
;;  Face defined here:
;;
;;    `sorti-msg-emphasis'.
;;
;;  User option defined here:
;;
;;    `sorti-cycle-key'.
;;
;;  Non-interactive functions defined here:
;;
;;    `sorti-bind-cycle-key', `sorti-bind-cycle-key-and-complete',
;;    `sorti-cycle-sort-order', `sorti-reverse-order'.
;;
;;  Internal variables defined here:
;;
;;    `sorti-current-order', `sorti-sort-function-chooser',
;;    `sorti-sort-orders-alist', `sorti-sort-orders-ring'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2022/01/12 dadams
;;     sorti-bind-cycle-key: Added optional arg MAP.
;;     sorti-bind-cycle-key-and-complete: Pass minibuffer-local-completion-map to sorti-bind-cycle-key.
;;     sorti-cycle-sort-order: Handle nil sorti-sort-function-chooser.
;; 2021/03/23 dadams
;;     Added autoload cookie.
;; 2020/06/04 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup sortie nil
  "Preferences for sorting of completion candidates."
  :prefix "sorti-" :group 'completion
  :link `(url-link :tag "Send Bug Report"
                   ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=sortie.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and Keysee library versions."))
  :link '(url-link :tag "Other Libraries by Drew" "https://www.emacswiki.org/emacs/DrewsElispLibraries")
  :link '(url-link :tag "Download" "https://www.emacswiki.org/emacs/download/sortie.el")
  :link '(url-link :tag "Description" "https://www.emacswiki.org/emacs/Sortie"))

(defface sorti-msg-emphasis
  '((((background dark)) (:foreground "Yellow"))
    (t (:foreground "Blue")))
  "Face used to emphasize (part of) a message."
  :group 'sortie :group 'faces)

(defcustom sorti-cycle-key (kbd "C-,")
  "Key to use for cycling sort orders."
  :group 'sortie :type 'key-sequence)

(defvar sorti-current-order nil
  "Current completion-candidates sort order.
The value is a key in `sorti-sort-orders-alist'.")

(defvar sorti-sort-function-chooser nil
  "Function that returns the current function for sorting candidates.
Called with no args.  It can dispatch on `sorti-current-order' to
choose the sort function, for example.

A sort function is used as completion metadata for both
`display-sort-function' and `cycle-sort-function'.  It receives one
arg, a list of candidates (strings), and it returns a sorted list of
candidates.")

(defvar sorti-sort-orders-alist ()
  "Alist of sort-orders.
Each element is (NAME . DESCRIPTION), where NAME (a symbol) and
DESCRIPTION (a string) name and describe a sort order, respectively.
NAME is a possible value for `sorti-current-order'.
DESCRIPTION is echoed when you switch to sort-order NAME.")

(defvar sorti-sort-orders-ring nil
  "Ring of sort orders for cycling with `sorti-cycle-key'.
Use functions such as `ring-insert' to define the ring.")

(defun sorti-bind-cycle-key (&optional map)
  "Bind `sorti-cycle-key' to command `sorti-cycle-sort-order' in KEYMAP.
MAP defaults to `minibuffer-local-map'."
  (define-key (or map  minibuffer-local-map) sorti-cycle-key #'sorti-cycle-sort-order))

(defun sorti-bind-cycle-key-and-complete ()
  "Bind `sorti-cycle-key', then complete the minibuffer contents.
The key is bound in `minibuffer-local-completion-map'."
  (sorti-bind-cycle-key minibuffer-local-completion-map)
  (minibuffer-complete))

;;;###autoload
(defun sorti-cycle-sort-order (&optional reversep msgp)
  "Cycle to the next completion-candidates sort order.
With a prefix arg, just reverse the current sort order (don't cycle).
\(A prefix key has no effect if sorting is currently turned off.)"
  (interactive "P\np")
  (let ((sort-fn  (if sorti-sort-function-chooser
                      (funcall sorti-sort-function-chooser)
                    sorti-current-order)))
    (cond ((and reversep  sort-fn)
           (if (advice-member-p 'sorti-reverse-order sort-fn)
               (advice-remove sort-fn 'sorti-reverse-order)
             (advice-add sort-fn :around 'sorti-reverse-order)))
          (t
           (setq sorti-current-order  (ring-next sorti-sort-orders-ring sorti-current-order)))))
  (setq last-command  'ignore)
  (minibuffer-complete)
  (when msgp
    (message "Sorting is now %s%s"
             (if sorti-current-order
                 (format "%s" (propertize (cdr (assq sorti-current-order sorti-sort-orders-alist))
                                          'face 'sorti-msg-emphasis))
               (format "turned %s" (propertize "OFF" 'face 'sorti-msg-emphasis)))
             (if (advice-member-p 'sorti-reverse-order (funcall sorti-sort-function-chooser))
                 " REVERSED"
               ""))))

(defun sorti-reverse-order (old-fn candidates)
  "Reverse the result of calling OLD-FN with single argument CANDIDATES."
  (setq candidates  (nreverse (funcall old-fn candidates))))


;;;;;;;;;;;;;;;;;;;;;;

(provide 'sortie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sortie.el ends here
