;;; bg-rlogin-setup.el --- Rlogin setup
;;; Commentary: See RloginMode wiki page for details.
;;; Maintainer: Brent Goodrick
;;; URL: http://www.emacswiki.org/cgi-bin/emacs/bg-rlogin-setup.el

;;; Code:

(defun bg-nfs-path (file-path &optional remote-system keep-local)
  "Returns a path useable over NFS given FILE-PATH. If REMOTE-SYSTEM
is specified, then local paths are converted to NFS paths using
REMOTE-SYSTEM. When KEEP-LOCAL is non-nil, do not attempt to NFS-ify
the path for paths that seem to be local filesystem paths."
  (interactive)
  (let ((final-system-name (or remote-system (system-name))))
    (cond
     ((string-match "^/net/" file-path) file-path)
     ((string-match "^/user/" file-path) file-path)
     ((string-match "^/home/" file-path) file-path)
     ((string-match "^/\.automount/\\([^/]+\\)/root/\\(.*\\)$" file-path)
      (concat "/net/" (match-string 1 file-path) "/" (match-string 2 file-path)))
     ((string-match "^/tmp_mnt/\\(.*\\)$" file-path)
      (concat (match-string 0 file-path)))
     ((string-match "^/" file-path)
      (if keep-local
          file-path
        (concat "/net/" final-system-name file-path)))
     (t file-path))))

(defun bg-wait-for-proc-string (proc proc-string)
  "Waits for the process given by PROC to show the PROC-STRING shows up and then returns."
  (while (not (re-search-backward proc-string nil t nil))
    (goto-char (process-mark proc))
    ;; don't block forever, give up after 500 milliseconds
    (accept-process-output proc 0 500))
  ;; leave the point at the end of the buffer:
  (goto-char (process-mark proc)))

(defun bg-post-comint-command (command)
  "Posts COMMAND into the current buffer. The current buffer must be a comint-controlled buffer."
  (let* ((proc (get-buffer-process (current-buffer))))
    (unless proc
      (error "Buffer %S is not a comint-controlled buffer" (buffer-name)))
    (when proc
      (goto-char (point-max))
      (comint-kill-input)
      (insert command)
      (comint-send-input))))

(defun bg-set-display (&optional just-insert)
  "Defines a smarter alias for setting DISPLAY"
  (interactive (list current-prefix-arg))
  (when (eq 'x window-system)
    (let* ((proc (get-buffer-process (current-buffer)))
           (display (getenv "DISPLAY"))
           (user (getenv "USER"))
           (full-display (if (string-match "^:" display) (concat (system-name) display) display))
           (display-command (concat "DISPLAY=\"" full-display "\"; export DISPLAY; echo DISPLAY $DISPLAY"))) ;
      (when proc
        (bg-wait-for-proc-string proc user) ; wait for the username to show up which hopefully is in the shell prompt string:
        (if just-insert
            (progn
              (goto-char (point-max))
              (insert display-command)
              (beginning-of-line)
              (search-forward "DISPLAY=\"" nil t nil))
          (bg-post-comint-command display-command))))))

(defun bg-disable-column-editing ()
  "My default shell is ksh, and has to be in order to use aliases across all systems.
The problem is that, on Linux, ksh is pdksh which likes to do
editing operations using the COLUMNS environment variable. From
the Linux pdksh man page we have:

  In these editing modes, if a line is longer that the screen width  (see
  COLUMNS parameter), a >, + or < character is displayed in the last col-
  umn indicating that there are more characters after, before and  after,
  or  before  the  current  position, respectively.  The line is scrolled
  horizontally as necessary.

In Bash, I have to spawn a subshell with EMACS=t before
invoking. I tried all sorts of other things and this was the only
thing that worked."
  (interactive (list current-prefix-arg))
  (let* ((proc (get-buffer-process (current-buffer))))
    (when proc
      (bg-wait-for-proc-string proc user)
      ;; Find out if the remote system is a Linux system, and if it
      ;; is, then spawn a bash subshell with EMACS env var set to "t":
      (bg-post-comint-command "uname -s")
      (bg-wait-for-proc-string proc user)
      (when (save-excursion
              ;; back up a line
              (goto-char (line-beginning-position 0))
              (looking-at "Linux"))
        (bg-post-comint-command "EMACS=t bash -i")))))

(defun bg-rl (input-args &optional buffer)
  "Wrapper around `rlogin' but sets DISPLAY and current working directory properly.
You must make sure your PS1 environment variable contains the value of USER environment variable on the remote machine,
otherwise the shell prompt cannot be detected."
  (interactive (list
                (read-from-minibuffer "rlogin arguments (hostname first): "
                                      nil nil nil 'rlogin-history)
                current-prefix-arg))
  (let ((original-directory (bg-nfs-path default-directory)))
    (rlogin input-args buffer)
    (let* ((proc (get-buffer-process (current-buffer)))
           (user (or user-login-name (getenv "USER"))))
      (when proc
        (bg-disable-column-editing)
        (bg-set-display)
        (bg-post-comint-command (concat "cd " original-directory))))))

(provide 'bg-rlogin-setup)
;;; bg-rlogin-setup.el ends here
