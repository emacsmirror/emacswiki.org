;;; top-mode.el --- run "top" from emacs

;; Author: Benjamin Rutt
;; Created: Jul 18, 2004
;; Keywords: extensions, processes

;; top-mode.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; top-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code runs top from within emacs (using top's batch mode), and
;; provides functionality to operate on processes.

;; In order to run it, just execute M-x top.  Unlike real top, the
;; resulting buffer doesn't refresh automatically (yet, it's a feature
;; I'd like to add someday).  You can refresh the buffer manually by
;; pressing 'g'.  If you'd like to mark processes for later actions,
;; use 'm' to mark or 'u' to unmark.  If no processes are marked, the
;; default action will apply to the process at the current line.  At
;; the time of this writing, the valid actions are:
;;
;;     -strace a process
;;     -kill processes
;;     -renice processes

;; You can also toggle showing processes of a specific user by
;; pressing 'U'.
;;
;; NOTE:  tested only on Debian GNU/Linux unstable and solaris 8.
;;
;; Should work out of the box for Debian's version of 'top', however
;; for solaris 8's version of top, I found the following settings to
;; be useful:
;;
;;     (defun top-mode-solaris-generate-top-command (user)
;;       (if (not user)
;;           "top -b"
;;         (format "top -b -U%s" user)))
;;     (setq top-mode-generate-top-command-function
;;           'top-mode-solaris-generate-top-command)
;;     (setq top-mode-strace-command "truss")

;;; Code:

(defgroup top-mode nil
  "Emacs frontend to the top command, which monitors system processes."
  :group 'processes)

(defcustom top-mode-generate-top-command-function
  'top-mode-generate-top-command-default
  "*Which function to be called to produce the command line for
running top on your machine.

The function will be called with one argument, USER, which will
either be a string specifying that the processes owned by USER
should be shown, or nil, meaning that all processes should be
shown."
  :type 'function
  :group 'top-mode)

(defcustom top-mode-column-header-regexp "^\\s-+PID\\s-+USER.*COMMAND\\s-*$"
  "*Regexp to match the column header line, which helps this
package to identify where the list of processes begins."
  :type 'regexp
  :group 'top-mode)

(defcustom top-mode-mark-face 'highlight
  "*Face with which to mark lines."
  :type 'face
  :group 'top-mode)

(defcustom top-mode-strace-command "strace"
  "*System call tracer (probably set this to \"truss\" on Solaris, etc)."
  :type 'string
  :group 'top-mode)

;; internals
(defvar top-mode-specific-user nil)
(defvar top-mode-overlay-list nil)
(defvar top-mode-generate-top-command-default-user-arg 'unknown)

(defun top-mode-generate-top-command-default (user)
  (if (not user)
      "top -b -n 1"
    ;; try "-u" argument, and set cache variable based on result
    (if (eq top-mode-generate-top-command-default-user-arg
            'unknown)
        (let ((result (shell-command
                       (format "top -b -n 1 -u %s >/dev/null"
                               user-login-name))))
          (if (= result 0)
              (setq top-mode-generate-top-command-default-user-arg 'yes)
            (setq top-mode-generate-top-command-default-user-arg 'no))))
    (cond
     ((eq top-mode-generate-top-command-default-user-arg 'yes)
      (format "top -b -n 1 -u %s" user))
     ;; fall back on "awk"ward manual removal of commands not owned by
     ;; the user
     ((eq top-mode-generate-top-command-default-user-arg 'no)
      (format "top -b -n 1 | awk 'BEGIN { seenColumnLine=0; } { if (seenColumnLine==0) { print } else if ($2 == \"%s\") { print }; if ($0 ~ /PID.*USER.*COMMAND/) { seenColumnLine=1; } }'" user)))))

(defvar top-mode-map nil    ; Create a mode-specific keymap.
  "Keymap for Top mode.")

(if top-mode-map
    ()             ; Do not change the keymap if it is already set up.
  (setq top-mode-map (make-sparse-keymap))
  (define-key top-mode-map "n" 'top-mode-next-line)
  (define-key top-mode-map "p" 'top-mode-previous-line)
  (define-key top-mode-map "g" 'top)
  (define-key top-mode-map "q" 'quit-window)
  (define-key top-mode-map "k" 'top-mode-kill)
  (define-key top-mode-map "K" 'top-mode-kill-noconfirm)
  (define-key top-mode-map "s" 'top-mode-strace)
  (define-key top-mode-map "S" 'top-mode-strace-noconfirm)
  (define-key top-mode-map "r" 'top-mode-renice)
  (define-key top-mode-map "R" 'top-mode-renice-noconfirm)
  (define-key top-mode-map "m" 'top-mode-mark)
  (define-key top-mode-map "u" 'top-mode-unmark)
  (define-key top-mode-map "U" 'top-mode-show-specific-user))

(defun top-mode ()
  "Major mode for running top and interacting with processes."
  (interactive)
  (kill-all-local-variables)
  (use-local-map top-mode-map)
  (setq mode-name "Top")
  (setq major-mode 'top-mode)
  ;; the following two expressions don't work in concert with
  ;; global-auto-revert-mode, maybe someone can help?
;;   (set (make-local-variable 'revert-buffer-function)
;;        'top-mode-revert-buffer-function)
;;   (set (make-local-variable 'buffer-stale-function)
;;        'top-mode-buffer-stale-function)
  ;; fix for too long lines
  (when (not window-system)
    (setq truncate-lines t)))

(defun top-mode-revert-buffer-function (&optional ignore-auto noconfirm)
  (when (or noconfirm
            (y-or-n-p "Revert *top* buffer? "))
    (top)))
(defun top-mode-buffer-stale-function (&optional noconfirm) t)

;; line-at-pos is in emacs CVS as of 21.3.50 but define it for older
;; emacsen
(when (not (fboundp 'line-at-pos))
  (defun line-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location."
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

(defun top-mode-next-line ()
  (interactive)
  (next-line 1))

(defun top-mode-previous-line ()
  (interactive)
  (previous-line 1))

(defun top-mode-fill-buffer (goto-first-process)
  (switch-to-buffer (get-buffer-create "*top*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((old-term-env (getenv "TERM"))
        (output nil))
    (unwind-protect
        (progn
          (setenv "TERM" "dumb")
          (setq output
                (shell-command-to-string
                 (funcall top-mode-generate-top-command-function
                          top-mode-specific-user))))
      (setenv "TERM" old-term-env))
    (if (not output)
        (kill-buffer (get-buffer-create "*top*"))
      (insert output)
      (when goto-first-process
        (goto-char (point-min))
        (re-search-forward top-mode-column-header-regexp nil t)
        (next-line 1)
        (top-mode-goto-pid))
      (setq buffer-read-only t)
      (top-mode))))

(defun top ()
  "Runs 'top' in an emacs buffer."
  (interactive)
  (let* ((already-in-top (equal major-mode 'top-mode))
         (preserved-line (if already-in-top (line-at-pos) nil))
         (preserved-col (if already-in-top (current-column) nil))
         (preserved-window-start (window-start)))
    (if already-in-top
        (progn
          (top-mode-fill-buffer nil)
          (set-window-start (selected-window) preserved-window-start)
          (goto-line preserved-line)
          (move-to-column preserved-col))
      (top-mode-fill-buffer t))))

(defsubst top-mode-string-trim (string)
  "Lose leading and trailing whitespace.  Also remove all properties
from string."
  (if (string-match "\\`[ \t\n]+" string)
      (setq string (substring string (match-end 0))))
  (if (string-match "[ \t\n]+\\'" string)
      (setq string (substring string 0 (match-beginning 0))))
  (set-text-properties 0 (length string) nil string)
  string)

(defun top-mode-on-pid-line ()
  (when (save-excursion
          (beginning-of-line)
          (let ((orig-line (line-at-pos)))
            (while (and (equal (line-at-pos) orig-line)
                        (looking-at "\\s-"))
              (forward-char 1))
            (and (equal (line-at-pos) orig-line)
                 (looking-at "[0-9]+\\s-"))))
    (let ((after-pid-line-column-header t)
          (done nil))
      (save-excursion
        (while (not done)
          (forward-line 1)
          (if (= (point) (point-max))
              (setq done t)
            (if (looking-at top-mode-column-header-regexp)
                (progn
                  (setq done t)
                  (setq after-pid-line-column-header nil)))))
        after-pid-line-column-header))))

(defun top-mode-goto-pid ()
  (interactive)
  (when (top-mode-on-pid-line)
    (beginning-of-line)
    (while (looking-at "\\s-")
      (forward-char 1))
    (while (looking-at "[0-9]")
       (forward-char 1))
    (forward-char -1)))

(defun top-mode-get-pid-from-line ()
  (save-excursion
    (beginning-of-line)
    (re-search-forward "\\s-*\\([0-9]+\\)\\s-+" (line-end-position))
    (string-to-int (match-string 1))))

(defun top-mode-show-specific-user ()
  (interactive)
  (let ((response (read-from-minibuffer "Which user (blank for all): ") ))
    (if (string= response "")
        (setq top-mode-specific-user nil)
      (setq top-mode-specific-user response))
    (top)))

(defun top-mode-get-target-pids ()
  (or
   (sort
    (delq
     nil
     (mapcar
      (lambda (ov)
        (let ((os (overlay-start ov))
              (oe (overlay-end ov))
              (str nil))
          (when (and os oe)
            (setq str (buffer-substring (overlay-start ov) (overlay-end ov)))
            (string-match "^\\s-*\\([0-9]+\\)" str)
            (string-to-int (top-mode-string-trim
                            (substring str 0 (match-end 0)))))))
      top-mode-overlay-list))
    '<)
   (list (top-mode-get-pid-from-line))))

(defun top-mode-member-at-least-one (ls1 ls2)
  (if (or (null ls1) (null ls2))
      nil
    (or (member (car ls1) ls2)
        (top-mode-member-at-least-one (cdr ls1) ls2))))

(defun top-mode-unmark ()
  (interactive)
  (if (not (top-mode-on-pid-line))
      (message "Not on a process line")
    (let (existing-overlay)
      (mapc
       (lambda (ov)
         (if (member ov top-mode-overlay-list)
             (setq existing-overlay ov)))
       (overlays-at (point)))
      (when existing-overlay
        (setq top-mode-overlay-list
              (delq existing-overlay top-mode-overlay-list))
        (delete-overlay existing-overlay))
      (next-line 1))))

(defun top-mode-mark ()
  (interactive)
  (if (not (top-mode-on-pid-line))
      (message "Not on a process line")
    (when (not (top-mode-member-at-least-one
                (overlays-at (point))
                top-mode-overlay-list))
      (let (o)
        (setq o (make-overlay (line-beginning-position)
                              (line-end-position) nil nil t))
        (overlay-put o (quote face) top-mode-mark-face)
        (overlay-put o (quote evaporate) t)
        (setq top-mode-overlay-list (cons o top-mode-overlay-list))))
    (next-line 1)))

(defun top-mode-confirm-action (action-name pids)
  (y-or-n-p
   (format "Really %s pids %s? " action-name
           (mapconcat (lambda (num) (format "%d" num)) pids " "))))

(defun top-mode-renice (&optional noconfirm)
  (interactive)
  (if (not (top-mode-on-pid-line))
      (message "Not on a process line")
    (let ((pids (top-mode-get-target-pids)))
      (when (or noconfirm
                (top-mode-confirm-action "renice" pids))
        (shell-command
         (format "renice +10 %s"
                 (mapconcat (lambda (num) (format "%d" num)) pids " ")))
        (top)))))

(defun top-mode-renice-noconfirm ()
  (interactive)
  (top-mode-renice t))

(defun top-mode-strace (&optional noconfirm)
  (interactive)
  (if (not (top-mode-on-pid-line))
      (message "Not on a process line")
    (let ((pids (top-mode-get-target-pids)))
      (if (> (length pids) 1)
          (message "Cannot strace more than 1 process")
        (when (or noconfirm
                  (top-mode-confirm-action top-mode-strace-command pids))
          (shell-command
           (format "%s -p %d &" top-mode-strace-command (car pids))))))))

(defun top-mode-strace-noconfirm ()
  (interactive)
  (top-mode-strace t))

(defun top-mode-kill (&optional noconfirm)
  (interactive)
  (if (not (top-mode-on-pid-line))
      (message "Not on a process line")
    (let ((pids (top-mode-get-target-pids)))
      (when (or noconfirm
                (top-mode-confirm-action "kill" pids))
        (shell-command
         (format "kill -9 %s"
                 (mapconcat (lambda (num) (format "%d" num)) pids " ")))
        (top)))))

(defun top-mode-kill-noconfirm ()
  (interactive)
  (top-mode-kill t))

(provide 'top-mode)
