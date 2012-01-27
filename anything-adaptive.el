;;; anything-adaptive.el --- Sort candidates by usage frequency

;; Copyright (C) 2007

;; Author:  Tamas Patrovics
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; User config

(defvar anything-adaptive-history-file "~/.anything_adaptive_history"
  "Path of file where history information is stored.")

(defvar anything-adaptive-history-length 50
  "Max number of candidates stored for a source.")


;;;----------------------------------------------------------------------


(defvar anything-adaptive-done nil
  "Nil if history information is not yet stored for the current
  selection.")

(defvar anything-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defadvice anything-initialize (before anything-adaptive-initialize activate)
  "Advise `anything-initialize' to reset `anything-adaptive-done'
when anything is started."
  (setq anything-adaptive-done nil))

(defadvice anything-exit-minibuffer (before anything-adaptive-exit-minibuffer activate)
  "Advise `anything-exit-minibuffer' to store history information
when a candidate is selected with ENTER."
  (anything-adaptive-store-selection))

(defadvice anything-select-action (before anything-adaptive-select-action activate)
  "Advise `anything-select-action' to store history information
when the user goes to the action list with TAB."
  (anything-adaptive-store-selection))

(defun anything-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless anything-adaptive-done
    (setq anything-adaptive-done t)
    (let* ((source (anything-get-current-source))
           (source-name (or (assoc-default 'type source)
                            (assoc-default 'name source)))
           (source-info (or (assoc source-name anything-adaptive-history)
                            (progn
                              (push (list source-name) anything-adaptive-history)
                              (car anything-adaptive-history))))
           (selection (anything-get-selection))
           (selection-info (progn
                             (setcdr source-info
                                     (cons
                                      (let ((found (assoc selection (cdr source-info))))
                                        (if (not found)
                                            ;; new entry
                                            (list selection)

                                          ;; move entry to the
                                          ;; beginning of the list, so
                                          ;; that it doesn't get
                                          ;; trimmed when the history
                                          ;; is truncated
                                          (setcdr source-info
                                                  (delete found (cdr source-info)))
                                          found))
                                      (cdr source-info)))
                             (cadr source-info)))
           (pattern-info (progn
                           (setcdr selection-info
                                   (cons
                                    (let ((found (assoc anything-pattern (cdr selection-info))))
                                      (if (not found)
                                          ;; new entry
                                          (cons anything-pattern 0)

                                        ;; move entry to the beginning
                                        ;; of the list, so if two
                                        ;; patterns used the same
                                        ;; number of times then the one
                                        ;; used last appears first in
                                        ;; the list
                                        (setcdr selection-info
                                                (delete found (cdr selection-info)))
                                        found))
                                    (cdr selection-info)))
                           (cadr selection-info))))

      ;; increase usage count
      (setcdr pattern-info (1+ (cdr pattern-info)))

      ;; truncate hsitory if needed
      (if (> (length (cdr selection-info)) anything-adaptive-history-length)
          (setcdr selection-info
                  (subseq (cdr selection-info) 0 anything-adaptive-history-length))))))


(if (file-readable-p anything-adaptive-history-file)
    (load-file anything-adaptive-history-file))
(add-hook 'kill-emacs-hook 'anything-adaptive-save-history)


(defun anything-adaptive-save-history ()
  "Save history information to file pointed by
`anything-adaptive-save-history'."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for anything adaptive display.\n")
    (prin1 `(setq anything-adaptive-history ',anything-adaptive-history)
           (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) anything-adaptive-history-file nil
                  (unless (interactive-p) 'quiet))))


(defun anything-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name anything-adaptive-history)))
    (if (not source-info)
        ;; if there is no information is stored for this source then
        ;; do nothing
        candidates

      (let ((usage
             ;; assemble a list containing the (CANIDATE . USAGE-COUNT) pairs
             (mapcar (lambda (candidate-info)
                       (let ((count 0))
                         (dolist (pattern-info (cdr candidate-info))
                           (if (not (equal (car pattern-info)
                                           anything-pattern))
                               (incf count (cdr pattern-info))

                             ;; if current pattern is equal to the
                             ;; previously used one then this
                             ;; candidadte has priority (that's why
                             ;; its count is boosted by 10000) and it
                             ;; only has to compete with other
                             ;; candidates which were also selected
                             ;; with the same pattern
                             (setq count (+ 10000 (cdr pattern-info)))
                             (return)))
                         (cons (car candidate-info) count)))
                     (cdr source-info)))
            sorted)

        ;; sort the list in descending order, so candidates with
        ;; highest priorty come first
        (setq usage (sort usage (lambda (first second)
                                  (> (cdr first) (cdr second)))))

        ;; put those candidates first which have the highest usage
        ;; count
        (dolist (info usage)
          (when (member* (car info) candidates
                         :test 'anything-adaptive-compare)
            (push (car info) sorted)
            (setq candidates (remove* (car info) candidates
                                      :test 'anything-adaptive-compare))))

        ;; and append the rest
        (append (reverse sorted) candidates nil)))))


(defun anything-adaptive-compare (x y)
  ;; Compare candidates X and Y taking into account that the candidate
  ;; can be in (DISPLAY . REAL) format.
  (equal (if (listp x)
             (cdr x)
           x)
         (if (listp y)
             (cdr y)
           y)))


;;; Code:



(provide 'anything-adaptive)
;;; anything-adaptive.el ends here
