;;; ifind-mode.el -- A minor mode based on isearch, for interactively finding
;;; files in the workspace.

;; (c) 2010 Christian Rovner

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Installation:
;; 
;; 1. Copy this file into your .emacs.d/ directory
;; 2. Add these lines to your .emacs file:
;;      (defvar workspace-dir "~/YOUR/WORKSPACE/DIR")
;;      (load "~/.emacs.d/ifind-mode.el")
;;
;; Replace YOUR/WORKSPACE/DIR above by your actual workspace directory

;; Usage:
;;
;; Just type M-x ifind-mode RET
;; Then type part of the filename you're searching for.  A list of matching
;; filenames in your workspace will appear, and it will get smaller as you keep
;; adding characters. Use the up/down arrows to navigate and press RET to visit
;; the file under the cursor.  Any other key will abort the search.

(defvar ifind-dir workspace-dir
  "Directory where to search files on.")

(defvar ifind-command "find %s \\( %s \\) -prune -o -type f -iname \"*%s*\" -print"
  "Shell command that performs the file search.
It's a string with three %s that get replaced by:
 1. The root directory where to search files on
 2. The directories to exclude; built using vc-directory-exclusion-list
 3. The string to search for in the filenames")

(defvar ifind-min-length 2
  "Minimum length of the search string to trigger the shell command.")

(defvar ifind-string ""
  "The current search string.")

(defvar ifind-mode nil
  "Name of the minor mode, if non-nil.")

(defvar ifind-mode-map
  (let ((i ?\s)
        (map (make-keymap)))
    ;; Printing characters, search
    (while (< i 256)
      (define-key map (vector i) 'ifind-printing-char)
      (setq i (1+ i)))
    (define-key map [up] 'previous-line)
    (define-key map [down] 'next-line)
    (define-key map [return] 'ifind-visit-file)
    (define-key map [backspace] 'ifind-del-char)
    ;; All other keys will abort ifind
    (define-key map [t] 'ifind-abort)
    map)
  "Keymap for `ifind-mode'.")

;; Add ifind-mode to minor mode list
(or (assq 'ifind-mode minor-mode-alist)
    (nconc minor-mode-alist
           (list '(ifind-mode ifind-mode))))

(defvar ifind-excluded-dirs
  (concat "-path "
          (mapconcat #'(lambda (dir)
                         (shell-quote-argument (concat "*/" dir)))
                     vc-directory-exclusion-list
                     " -o -path "))
  "Substring within the find command, specifies which paths to ignore.")

(defun ifind-mode ()
  "Start Ifind minor mode."
  (interactive)
  (setq ifind-string ""
        ifind-mode " Ifind"
        overriding-terminal-local-map ifind-mode-map)
  (looking-at "")
  (force-mode-line-update)
  (ifind-update))

(defun ifind-printing-char ()
  "Add this ordinary printing character to the search string and search."
  (interactive)
  (let ((char last-command-event))
    (setq ifind-string (concat ifind-string (char-to-string char)))
    (ifind-update)))

(defun ifind-abort ()
  "Abort Ifind."
  (interactive)
  (ifind-exit)
  (message "Ifind aborted."))

(defun ifind-visit-file ()
  "Open the file under the cursor in the *ifind* buffer."
  (interactive)
  (set-buffer "*ifind*")
  (let ((filename (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
    (if (> (length filename) 0)
        (find-file filename)))
  (ifind-exit))

(defun ifind-del-char ()
  "Delete character from end of search string and search again."
  (interactive)
  (when (> (length ifind-string) 0)
    (setq ifind-string (substring ifind-string 0 -1))
    (ifind-update)))

(defun ifind-exit ()
  "Exit Ifind minor mode."
  (setq ifind-mode nil
        overriding-terminal-local-map nil)
  (force-mode-line-update)
  (kill-buffer "*ifind*"))

(defun ifind-update ()
  "Display the current search string and search for files."
  (switch-to-buffer "*ifind*")
  (let ((message-log-max nil))
    (if (>= (length ifind-string) ifind-min-length)
        (shell-command
         (format ifind-command ifind-dir ifind-excluded-dirs ifind-string)
         "*ifind*"))
    (message "Find files matching: %s" ifind-string)))
