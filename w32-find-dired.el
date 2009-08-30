;;; w32-find-dired.el --- light w32 replacement for `find-dired'

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Mathias Dahl (brakjoller@gmail.com)
;; Version: w32-find-dired 0.1.1

;; This file is not part of GNU Emacs (yet)

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is a light variant of `find-dired' for those who run Emacs on
;; w32.

;; Instead of using find, cmd.exe's `dir' command is used. The output
;; is then transformed to something dired can use, using
;; `insert-directory' from ls-lisp.el.

;; It does not support all things that `find-dired' can do, mostly
;; because the dir-command command used is not as powerful. It is also
;; "hard coded" into doing recursive directory listing.

;; Most of the code is ripped from find-dired.el

;;; History

;; * Version 0.1.1, 2006-01-12, Mathias Dahl

;;  - Changed the way `dir' is called.

;;; Code:

(require 'dired)

(defvar w32-find-dired-pattern nil
  "Last arguments given to `for' by \\[w32-find-dired].")

;; History of w32-find-dired-pattern values entered in the minibuffer.
(defvar w32-find-dired-pattern-history nil)

(defun w32-find-dired (dir pattern)
  "Use cmd.exe's `dir' command to find files recursively, and go
into Dired mode on a buffer of the output. The command run (after
changing into DIR) is

    dir /s /b DIR\\PATTERN
"

  (interactive (list (read-file-name "Run find in directory: " nil "" t)
                     (read-string "Search for: " w32-find-dired-pattern
                                  '(w32-find-dired-pattern-history . 1))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory)
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
        (error "w32-find-dired needs a directory: %s" dir))

    (switch-to-buffer (get-buffer-create "*w32-find-dired*"))

    ;; See if there's still a process running, and offer to kill it
    ;; first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
        (if (or (not (eq (process-status find) 'run))
                (yes-or-no-p "A `for' process is running; kill it? "))
            (condition-case nil
                (progn
                  (interrupt-process find)
                  (sit-for 1)
                  (delete-process find))
              (error nil))
          (error "Cannot have two processes in `%s' at once" (buffer-name)))))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
          w32-find-dired-pattern pattern) ; save for next interactive
                                        ; call
    ;; The next statement will bomb in classic dired (no optional arg
    ;; allowed)
    (dired-mode dir (cdr find-ls-option))
    ;; This really should rerun the find command, but I don't
    ;; have time for that.
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v
        ;; 1.15 and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``dir'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (insert "  " pattern "\n")
    ;; Start the dir process.
    (let ((proc (start-process
                 "dir"
                 (current-buffer)
                 "cmd"
                 "/c" "dir" "/b" "/s"
                 (concat (substitute ?\\ ?/ dir) pattern))))
      (set-process-filter proc (function w32-find-dired-filter))
      (set-process-sentinel proc (function w32-find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))


(defun w32-find-dired-filter (proc string)
  ;; Filter for \\[w32-find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)               ; not killed?
        (save-excursion
          (set-buffer buf)
          (save-restriction
            (widen)
            (save-excursion
              (let ((buffer-read-only nil)
                    (end (point-max)))
                (goto-char end)
                ;; Just insert the data, the sentinel will take care
                ;; of the formatting.
                (insert string)
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char end)
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))
                ))))
      ;; The buffer has been killed.
      (delete-process proc))))


(defun w32-find-dired-sentinel (proc state)
  ;; Sentinel for \\[w32-find-dired] processes.
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
        (save-excursion
          (set-buffer buf)
          (let ((buffer-read-only nil)
                (saved-pos nil))
            (save-excursion
              (goto-char (point-max))
              (insert "\n  w32-find-dired " state)
              (forward-char -1)    ;Back up before \n at end of STATE.
              (insert " at " (substring (current-time-string) 0 19))
              (forward-char 1)
              (setq mode-line-process
                    (concat ":"
                            (symbol-name (process-status proc))))
              ;; Since the buffer and mode line will show that the
              ;; process is dead, we can delete it now.  Otherwise it
              ;; will stay around until M-x list-processes.
              (delete-process proc)

              ;; Transform the output from dir into dired format

              ;; Skip header
              (goto-char (point-min))
              (forward-line 2)
              (beginning-of-line)

              ;; Transform each line
              (while (looking-at "^.")
                (setq col0 (point))
                (end-of-line)
                (setq row (buffer-substring col0 (point)))
                (setq row (substitute ?/ ?\\ row))
                (beginning-of-line)
                (kill-line)
                (insert "  ")
                (insert-directory row "")
                ;; insert-directory adds a newline that we want to
                ;; remove
                (delete-char -1)
                (forward-line 1))

              (force-mode-line-update)))
          (message "w32-find-dired %s finished." (current-buffer))))))

(provide 'w32-find-dired)

;;; w32-find-dired.el ends here
