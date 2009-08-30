;;; find-dired++.el -- Redefinition of find-dired
;; 
;; Author:   Cedric Lallain <kandjar76@hotmail.com>
;; Version:  1.0
;; Keywords: find-dired
;; Description: Modified version of find-dired.
;; Tested with: GNU Emacs 21.x and GNU Emacs 22.x
;;
;; This file is *NOT* part of GNU Emacs.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program; if not, write to the Free Software
;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modified version of find-dired due to an issue. 
;; The current find-dired surrounds the filenames with quotes (") and replace 
;; the home path with "~"; this creates an issue while running shell commands
;; which doesn't interpret the symbol "~".
;;
;; This version leaves the quotes, but call the shell command with the full 
;; path instead.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'find-dired)


(defun find-dired-full-path (dir args)
  "{Modified version of 'find-dired'}
Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is

    find . \\( ARGS \\) -ls

except that the variable `find-ls-option' specifies what to use
as the final argument."
  (interactive (list (read-file-name "Run find in directory: " nil "" t)
             (read-string "Run find (with args): " find-args
                  '(find-args-history . 1))))
  (let ((dired-buffers dired-buffers)
    (full-path (file-name-as-directory (expand-file-name dir))))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (abbreviate-file-name full-path))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
    (error "find-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*Find*"))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
    (if (or (not (eq (process-status find) 'run))
        (yes-or-no-p "A `find' process is running; kill it? "))
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
    (setq default-directory full-path
      find-args args        ; save for next interactive call
      args (concat find-dired-find-program " . "
               (if (string= args "")
               ""
             (concat "\\( " args " \\) "))
               (car find-ls-option)))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode full-path (cdr find-ls-option))
    ;; This really should rerun the find command, but I don't
    ;; have time for that.
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
    ;; will work even with nested dired format (dired-nstd.el,v 1.15
    ;; and later)
    (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm) 
      (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " full-path ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line. 
    (insert "  " args "\n")
    ;; Start the find process.
    (let ((proc (start-process-shell-command find-dired-find-program (current-buffer) args)))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defadvice find-dired (around anytime(dir args))
  (find-dired-full-path dir args))
(ad-activate 'find-dired)

(provide 'find-dired++)
