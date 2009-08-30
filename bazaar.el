;;; sent to janmar@iprimus.com.au but it bounced:
;;; on "GNU Emacs 21.2.1 (i686-pc-cygwin, X toolkit) of 2004-03-23 on cm-test"
;;; following at least gives a *Bazaar* buffer that showed modified files rather
;;; than just ? for everything
;;; 
;;; bzr-propertize() doesn't work for me though
;;; 
;;; --- bazaar.el.orig      2007-05-18 06:57:57.328125000 +0000
;;; +++ bazaar.el   2007-05-30 03:40:29.781250000 +0000
;;; @@ -172,7 +172,7 @@
;;;  (defun bzr-parse-status (buffer)
;;;    (with-current-buffer buffer
;;;      (goto-char (point-min))
;;; -    (loop while (re-search-forward "^\\(.\\) +\\(.+\\)$" nil t 1)
;;; +    (loop while (re-search-forward "^ *\\(.\\) +\\(.+\\)$" nil t 1)
;;;            collect (cons (match-string 1) (match-string 2)))))
;;; 
;;;  (defun bzr-buffer-in-current-branch-p (&optional buffer)
;;; @@ -291,7 +291,7 @@
;;;  (defun bazaar-refresh-status ()
;;;    "Refresh status."
;;;    (interactive)
;;; -  (bzr-run-command "status")
;;; +  (bzr-run-command "status" "--short")
;;;    (setq bzr-current-status (bzr-parse-status (bzr-get-buffer 'process)))
;;;    (bzr-render))


;;; bazaar.el --- A simple user interface for Bazaar-NG.

;; Copyright (C) 2005 jan <janmar@iprimus.com.au>

;; Version: 0.2
;; Keywords: tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; 
;; This file contains an interface for the Bazaar-NG version control
;; system. It provides easy access to the most frequently used Bazaar
;; commands.
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'bazaar)
;;    (add-hook 'find-file-hooks 'bzr-maybe-activate)
;;    
;; To start: `M-x bazaar'
;; 

;;; Code:

(eval-when-compile (require 'cl))


;;;; Utilities
;;;; ------------------------------------------------------------


;;; xemacs compatibility

(defun bzr-cancel-timer (timer)
  (if (featurep 'xemacs)
      (delete-itimer timer)
    (cancel-timer timer)))

(defun bzr-propertize (string &rest properties)
  (cond ((featurep 'xemacs)
         (set-text-properties 0 (length string) properties string)
         string)
        (t
         (apply #'propertize string properties))))

;;; misc

(defmacro bzr-when (test-form &rest body)
  `(let ((it ,test-form))
     (when it ,@body)))

(put 'bzr-when 'lisp-indent-function 1)

(defun bzr-clamp (min value max)
  (max (min value max) min))

(defun bzr-normalize-branch-name (name)
  ;; make sure branch name has a trailing slash
  (unless (file-directory-p name)
    (error "%s is not a directory name." name))
  (expand-file-name (file-name-as-directory name)))

(defun bzr-current-line ()
  (+ (count-lines (point-min) (point)) (if (bolp) 1 0)))

(defun bzr-set-current-line (new-line)
  (goto-char (point-min))
  (forward-line new-line))

(defun bzr-first-line-as-string (buffer)
  (with-current-buffer buffer
    (save-excursion
      (buffer-substring (progn (goto-char (point-min)) (point))
                        (progn (end-of-line) (point))))))

;;; buffer management

(defconst bzr-buffers '((main . "*Bazaar*")
                        (process . "*Bazaar Process*")
                        (commit . "*Bazaar Commit*")))

(defun bzr-buffer-name (symbol-name)
  (or (cdr (assoc symbol-name bzr-buffers))
      (error "%s does not name a buffer." (symbol-name symbol-name))))

(defun bzr-get-buffer (symbol-name)
  (get-buffer-create (bzr-buffer-name symbol-name)))

(defun bzr-kill-buffers ()
  (loop for (symbol . name) in bzr-buffers
        do (bzr-when (get-buffer name)
             (kill-buffer it))))

(defun bzr-clear-buffer (buffer)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    buffer))


;;;; Major Mode
;;;; ------------------------------------------------------------


;;; mode formalities

(defvar bazaar-mode-hook nil
  "Run after `bazaar-mode' is setup.")

(defvar bazaar-mode-map nil
  "Keymap for bazaar major mode.")

(unless bazaar-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'bazaar-quit)
    (define-key map "g" 'bazaar-refresh-status)
    (define-key map "\C-m" 'bazaar-view-file)
    (define-key map "n" 'bazaar-next-file)
    (define-key map "p" 'bazaar-previous-file)
    (define-key map "d" 'bazaar-diff-file)
    (define-key map "a" 'bazaar-add-file)
    (define-key map "c" 'bazaar-commit-changes)
    (setq bazaar-mode-map map)))

;; bazaar mode should only run in the *bazaar* buffer
(put 'bazaar-mode 'mode-class 'special)

(defun bazaar-mode ()
  "Major mode for interacting with Bazaar.
Commands:
\\{bazaar-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "Bazaar"
        major-mode 'bazaar-mode
        buffer-read-only t)
  (use-local-map bazaar-mode-map)
  (run-hooks 'bazaar-mode-hook))

;;; state

(defvar bzr-current-status nil)
(defvar bzr-current-file "")
(defvar bzr-current-branch "")

;;; utils

(defun bzr-run-command (&rest args)
  (let ((default-directory bzr-current-branch)
        (process-buffer (bzr-clear-buffer (bzr-get-buffer 'process))))
    (message "Running bzr %s..." (first args))
    (apply #'call-process "bzr" nil process-buffer nil args)
    (message "Running bzr %s...done" (first args))
    process-buffer))

(defun bzr-parse-status (buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (loop while (re-search-forward "^\\(.\\) +\\(.+\\)$" nil t 1)
          collect (cons (match-string 1) (match-string 2)))))

(defun bzr-buffer-in-current-branch-p (&optional buffer)
  (condition-case nil
      (with-current-buffer (or buffer (current-buffer))
        (string= bzr-local-branch bzr-current-branch))
    (error nil)))

(defun bzr-save-some-buffers ()
  (save-some-buffers nil #'bzr-buffer-in-current-branch-p))

(defun bzr-current-file-index ()
  (bzr-clamp 0 (- (bzr-current-line) 3) (- (length bzr-current-status) 1)))

(defun bzr-set-current-file-index (new-index)
  (let ((index (bzr-clamp 0 new-index (- (length bzr-current-status) 1))))
    (bzr-set-current-line (+ index 2))
    (setq bzr-current-file (cdr (nth index bzr-current-status)))))

(defun bzr-file-at-point ()
  (and (eql major-mode 'bazaar-mode)
       bzr-current-status
       (concat bzr-current-branch 
               (setq bzr-current-file
                     (cdr (nth (bzr-current-file-index) bzr-current-status))))))

(defun bzr-move-point-to-current-file ()
  (bzr-set-current-file-index
   (loop for (code . file) in bzr-current-status
         for i upfrom 0
         when (string= file bzr-current-file) return i
         finally return 0)))

(defun bzr-current-revision ()
  (string-to-number (bzr-first-line-as-string (bzr-run-command "revno"))))

(defun bzr-revision-as-number (revision &optional current-revision)
  (cond ((eql revision 'latest-revision) (or current-revision
                                             (bzr-current-revision)))
        ((integerp revision) revision)
        ((eql revision 'working-copy)
         (error "Working copy does not have a revision number."))        
        (t (error "%s is not a valid revision." revision))))

(defun bzr-revision-name (filename revision)
  (concat (file-name-nondirectory filename)
          ".~"
          (if (symbolp revision)
              (symbol-name revision)
            (number-to-string revision))
          "~"))

(defun bzr-process-to-revision-buffer (buffer name revision)
  (with-current-buffer buffer
    (rename-buffer (bzr-revision-name name revision) t)
    (setq buffer-read-only t)
    buffer))

(defun bzr-get-file-buffer (filename)
  (save-window-excursion
    (find-file filename)
    (current-buffer)))

(defun bzr-diff-maybe-kill-buffer (buffer)
  ;; kill buffers representing vc revisions
  (unless (or (not (buffer-live-p buffer))
              (buffer-file-name buffer))
    (kill-buffer buffer)))

(defun bzr-get-diff-buffer (filename revision &optional current-revision)
  (if (eql revision 'working-copy)
      (bzr-get-file-buffer filename)
    (let* ((revno (number-to-string (bzr-revision-as-number revision)))
           (buffer (bzr-run-command "cat" "-r" revno filename)))
      (bzr-process-to-revision-buffer buffer filename revision))))

(defun bzr-diff-file (filename revision1 revision2)
  ;; todo: push-mark on working copy
  ;; todo: if asking for working-copy and file is modified then prompt to save
  (bzr-save-some-buffers)               ; for now, see line above
  (let* ((window-config (current-window-configuration))
         (buffer1 (bzr-get-diff-buffer filename revision1))
         (buffer2 (bzr-get-diff-buffer filename revision2))
         (cleanup `(lambda ()
                     (set-window-configuration ,window-config)
                     (bzr-diff-maybe-kill-buffer ,buffer1)
                     (bzr-diff-maybe-kill-buffer ,buffer2))))
    (ediff-buffers buffer1
                   buffer2
                   (list (lambda ()
                           ;; add the quit hook here so it's buffer
                           ;; local to the ediff control panel
                           (add-hook 'ediff-after-quit-hook-internal
                                     cleanup
                                     nil
                                     'local))))))

;;; commands

(defun bazaar (branch)
  "Entry point into bazaar mode."
  ;; todo: verify branch is valid
  (interactive "DSelect Branch: ")
  (switch-to-buffer (bzr-get-buffer 'main))
  (bazaar-mode)
  (setq bzr-current-branch (bzr-normalize-branch-name branch)
        default-directory bzr-current-branch)
  (bzr-save-some-buffers)
  (bazaar-refresh-status))

(defun bazaar-quit ()
  "Quit Bazaar Mode and kill all its buffers."
  (interactive)
  (bzr-kill-buffers))

(defun bazaar-refresh-status ()
  "Refresh status."
  (interactive)
  (bzr-run-command "status")
  (setq bzr-current-status (bzr-parse-status (bzr-get-buffer 'process)))
  (bzr-render))

(defun bazaar-next-file (&optional n)
  "Move the selection down N files."
  (interactive "p")
  (bzr-set-current-file-index (+ (bzr-current-file-index) n)))

(defun bazaar-previous-file (&optional n)
  "Move the selection up N files."
  (interactive "p")
  (bazaar-next-file (- n)))

(defun bazaar-diff-file ()
  "Ediff selected file against the latest revision.

With a prefix argument do an arbitrary diff. (not implemented yet)"
  (interactive)
  ;; todo: promting for arbitrary diffs
  (bzr-when (bzr-file-at-point)
    (bzr-diff-file it 'working-copy 'latest-revision)))

(defun bazaar-view-file ()
  "Visit selected file in its own buffer."
  (interactive)
  (bzr-when (bzr-file-at-point)
    (find-file it)))

(defun bazaar-add-file ()
  "Add selected file to revision control."
  (interactive)
  (bzr-when (bzr-file-at-point)
    (bzr-run-command "add" it)
    (bazaar-refresh-status)))

(defun bazaar-commit-changes ()
  "Commit changes to revision control."
  (interactive)
  (bzr-save-some-buffers)
  (bzr-commit-changes))

(defun bazaar-reset ()
  "Reset state, only used to recover from bugs."
  (interactive)
  (bzr-reset-processes)
  (setq bzr-diff-lock nil))

;;; display

;; todo: add colors for other configurations
(defface bzr-field-name-face
  '((((class color) (background light)) (:foreground "dark grey" :bold t)))
  "Bazaar mode face used to highlight field names.")

(defface bzr-modified-face
  '((((class color) (background light)) (:foreground "blue" :bold t)))
  "Bazaar mode face used to highlight modified files.")

(defface bzr-unknown-face
  '((((class color) (background light)) (:foreground "goldenrod" :bold t)))
  "Bazaar mode face used to highlight unknown files.")

(defface bzr-added-face
  '((((class color) (background light)) (:foreground "lime green" :bold t)))
  "Bazaar mode face used to highlight added files.")

(defface bzr-deleted-face
  '((((class color) (background light)) (:foreground "dark orange" :bold t)))
  "Bazaar mode face used to highlight deleted files.")

(defface bzr-renamed-face
  '((((class color) (background light)) (:foreground "purple" :bold t)))
  "Bazaar mode face used to highlight renamed files.")

(defface bzr-conflict-face
  '((((class color) (background light)) (:foreground "red" :bold t)))
  "Bazaar mode face used to highlight files with conflicts.")

(defun bzr-status-code-as-string (code)
  (case (string-to-char code)
    (?M (bzr-propertize "Modified" 'face 'bzr-modified-face))
    (?? (bzr-propertize "Unknown " 'face 'bzr-unknown-face))
    (?A (bzr-propertize "Added   " 'face 'bzr-added-face))
    (?D (bzr-propertize "Deleted " 'face 'bzr-deleted-face))
    (?R (bzr-propertize "Renamed " 'face 'bzr-renamed-face))
    (?C (bzr-propertize "Conflict" 'face 'bzr-conflict-face))
    (t "?       ")))

(defun bzr-render ()
  (with-current-buffer (bzr-get-buffer 'main)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (insert (bzr-propertize "Branch: " 'face 'bzr-field-name-face))
      (insert bzr-current-branch "\n\n")
      (if bzr-current-status
          (loop for (code . file) in bzr-current-status
                do (insert "  " (bzr-status-code-as-string code) "  " file "\n"))
        (insert "  No changes."))
      (bzr-move-point-to-current-file))))


;;;; Minor Mode
;;;; ------------------------------------------------------------


(define-minor-mode bazaar-minor-mode
  "Minor mode for interacting with Bazaar.
Commands:
\\{bazaar-minor-mode-map}"
  nil
  " bzr"
  '(("\C-c,g" . bazaar-minor-mode-goto-branch)
    ("\C-c,d" . bazaar-minor-mode-diff-file)))

;;; commands

(defun bazaar-minor-mode-goto-branch ()
  "Enter *Bazaar* buffer for this file.

Select this file if it is visible. (not implemented)"
  (interactive)
  (setq bzr-current-file
        (substring (buffer-file-name) (length bzr-local-branch)))
  (bazaar bzr-local-branch))

(defun bazaar-minor-mode-diff-file ()
  "Ediff file against the latest revision.

Starts at the diff closest to point. (not implemented)"
  (interactive)
  ;; todo: prompting for arbitrary diffs
  (setq bzr-current-branch bzr-local-branch)
  (bzr-diff-file (buffer-file-name) 'working-copy 'latest-revision))

;;; activation hook

;; the following 2 functions where taken from Luke Gorrie's bzr-mode.el
(defun bzr-toplevel ()
  "Return the top-level directory of the repository."
  (let ((dir (bzr-find-repository)))
    (if dir
        (file-name-directory dir)
      (error "Can't find bzr repository top-level."))))

(defun bzr-find-repository (&optional start-directory)
  "Return the enclosing \".bzr\" directory, or nil if there isn't one."
  (when (and (buffer-file-name)
             (file-directory-p (file-name-directory (buffer-file-name))))
    (let ((dir (or start-directory
                   default-directory
                   (error "No start directory given."))))
      (or (car (directory-files dir t "^\\.bzr$"))
          (let ((next-dir (file-name-directory (directory-file-name dir))))
            (unless (equal dir next-dir)
              (bzr-find-repository next-dir)))))))

(defun bzr-root-directory ()
  (condition-case nil
      (bzr-toplevel)
    (error "")))

;; don't use 'bzr root' command at the moment, it leaves log files everywhere
;; (defun bzr-root-directory ()
;;   (let* ((process-buffer (bzr-clear-buffer (bzr-get-buffer 'process)))
;;          (exit-code (call-process "bzr" nil process-buffer nil "root")))
;;     (or (and (eql exit-code 0)
;;              (bzr-first-line-as-string process-buffer))
;;         "")))

(defun bzr-maybe-activate ()
  (let ((maybe-dir (bzr-root-directory)))
    (when (file-directory-p maybe-dir)
      (set (make-local-variable 'bzr-local-branch)
           (bzr-normalize-branch-name maybe-dir))
      (bazaar-minor-mode 1))))


;;;; Commit
;;;; ------------------------------------------------------------


(define-minor-mode bazaar-commit-mode
  "Minor mode that adds commit behaviour to the *Bazaar Commit* buffer.
Commands:
\\{bazaar-commit-mode-map}"
  nil
  " bzr-commit"
  '(("\C-c,a" . bazaar-commit-message-accept)))

;;; utils

(defvar bzr-commit-message
  "Insert commit message then type `C-c , a' (bazaar-commit-message-accept) to continue:\n")

(defun bzr-extract-commit-message ()
  (with-current-buffer (bzr-get-buffer 'commit)
    (replace-regexp-in-string bzr-commit-message
                              ""
                              (buffer-substring (point-min) (point-max)))))

(defvar bzr-pre-commit-window-config nil)

;;; commands

(defun bazaar-commit-message-accept ()
  "Complete the bazaar commit process."
  (interactive)
  (set-window-configuration bzr-pre-commit-window-config)
  (bzr-run-command "commit" "-m" (bzr-extract-commit-message))
  (bazaar-refresh-status))

;;; support for major mode

(defun bzr-commit-changes ()
  (setq bzr-pre-commit-window-config (current-window-configuration))
  (switch-to-buffer (bzr-clear-buffer (bzr-get-buffer 'commit)))
  (insert bzr-commit-message)
  (condition-case nil
      (rst-mode)                        ; reStructuredText mode
    (error (text-mode)))
  (bazaar-commit-mode t))


;;;; Completion
;;;; ------------------------------------------------------------


;; todo: we should generate completions by parsing the help command
(defun pcomplete/bzr ()
  "Completion rules for Bazaar-NG."
  (pcomplete-here (list "add" "commit" "diff" "export" "help" "info" "init"
                        "log" "remove" "status" "version"))
  (while (pcomplete-here (pcomplete-entries))))

(provide 'bazaar)
;;; bazaar.el ends here
