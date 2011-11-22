;;; rgrep-append.el
;;; ---
;;; another version of rgrep appending its result to previous search
;;; result in the same buffer

;; Copyright (C) 2011, Justin Jiang

;; Author: Justin Jiang <jiangjun.jking AT gmail DOT com>
;; Version: 0.1
;; Date: Nov 22, 2011

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

;;; Acknowlege

;;; Commentary:

;; Put this file in a folder where Emacs can find it.
;;
;; Add following lines to your .emacs initialization file:
;;
;;     (require 'rgrep-append)
;;


(require 'grep)
(require 'compile)

(defvar grep-append-default-option nil
  "Default options for grep append options:
t for append, nil for truncate."
)

(defun compilation-start-result-append (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))

  ;; set the variable `name-of-mode', `this-dir', `outwin', `outbuf'
  (let* ((name-of-mode
          (if (eq mode t)
              "compilation"
            (replace-regexp-in-string "-mode$" "" (symbol-name mode))))
         (thisdir default-directory)
         outwin outbuf saved-pos)
    (with-current-buffer
        (setq outbuf
              (get-buffer-create
               (compilation-buffer-name name-of-mode mode name-function))) ;get the *grep* buffer
      (let ((comp-proc (get-buffer-process (current-buffer))))
        (if comp-proc
            (if (or (not (eq (process-status comp-proc) 'run))
                    (yes-or-no-p
                     (format "A %s process is running; kill it? "
                             name-of-mode)))
                (condition-case ()
                    (progn
                      (interrupt-process comp-proc)
                      (sit-for 1)
                      (delete-process comp-proc))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))
      ;; first transfer directory from where M-x compile was called
      ;; -- (setq default-directory thisdir) --
      ;; -- don't set the default difrectory if append, we use absolute path in the result --

      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
            (default-directory thisdir))
        ;; Then evaluate a cd command if any, but don't perform it yet, else
        ;; start-command would do it again through the shell: (cd "..") AND
        ;; sh -c "cd ..; make"
        (cd (if (string-match "^\\s *cd\\(?:\\s +\\(\\S +?\\)\\)?\\s *[;&\n]" command)
                (if (match-end 1)
                    (substitute-env-vars (match-string 1 command))
                  "~")
              default-directory))

        ;; erase buffer
        ;; -- (erase-buffer)
        ;; Select the desired mode.
        (if (not (eq mode t))
            (progn
              (buffer-disable-undo)
              (funcall mode))
          (setq buffer-read-only nil)
          (with-no-warnings (comint-mode))
          (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be fone after (funcall mode) as that resets local variables
        (set (make-local-variable 'compilation-directory) thisdir)
        (if highlight-regexp
            (set (make-local-variable 'compilation-highlight-regexp)
                 highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
                (eq compilation-scroll-output 'first-error))
            (set (make-local-variable 'compilation-auto-jump-to-next) t))
        ;; Output a mode setter, for saving and later reloading this buffer.

        ;; insert at the end of the current buffer
        (goto-char (point-max))
        (setq saved-pos (point-max))
        ;;(message "test - current position is %d" (point))
        (insert (if (= (point) 1)
                    "-*- mode: "
                  "\n-*- mode: ")
                name-of-mode
                "; default-directory: " (prin1-to-string default-directory)
                " -*-\n"
                (format "%s started at %s\n\n"
                        mode-name
                        (substring (current-time-string) 0 19))
                command "\n")
        (setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf))
    (with-current-buffer outbuf
      (let ((process-environment
             (append
              compilation-environment
              (if (if (boundp 'system-uses-terminfo) ; `if' for compiler warning
                      system-uses-terminfo)
                  (list "TERM=dumb" "TERMCAP="
                        (format "COLUMNS=%d" (window-width)))
                (list "TERM=emacs"
                      (format "TERMCAP=emacs:co#%d:tc=unknown:"
                              (window-width))))
              ;; Set the EMACS variable, but
              ;; don't override users' setting of $EMACS.
              (unless (getenv "EMACS")
                (list "EMACS=t"))
              (list "INSIDE_EMACS=t")
              (copy-sequence process-environment))))
        (set (make-local-variable 'compilation-arguments)
             (list command mode name-function highlight-regexp))
        (set (make-local-variable 'revert-buffer-function)
             'compilation-revert-buffer)
        ;;(set-window-start outwin (point-min))
        (set-window-start outwin saved-pos)

        ;; Position point as the user will see it.
        (let ((desired-visible-point
               ;; Put it at the end if `compilation-scroll-output' is set.
               (if compilation-scroll-output
                   (point-max)
                 ;; Normally put it at the top.
                 ;; (point-min))))
                 saved-pos)))
          (if (eq outwin (selected-window))
              (goto-char desired-visible-point)
            (set-window-point outwin desired-visible-point)))

        ;; The setup function is called before compilation-set-window-height
        ;; so it can set the compilation-window-height buffer locally.
        (if compilation-process-setup-function
            (funcall compilation-process-setup-function))
        (compilation-set-window-height outwin)
        ;; Start the compilation.
        (if (fboundp 'start-process)
            (let ((proc
                   (if (eq mode t)
                       ;; comint uses `start-file-process'.
                       (get-buffer-process
                        (with-no-warnings
                          (comint-exec
                           outbuf (downcase mode-name)
                           (if (file-remote-p default-directory)
                               "/bin/sh"
                             shell-file-name)
                           nil `("-c" ,command))))
                     (start-file-process-shell-command (downcase mode-name)
                                                       outbuf command))))
              ;; Make the buffer's mode line show process state.
              (setq mode-line-process
                    (list (propertize ":%s" 'face 'compilation-warning)))
              (set-process-sentinel proc 'compilation-sentinel)
              (unless (eq mode t)
                ;; Keep the comint filter, since it's needed for proper handling
                ;; of the prompts.
                (set-process-filter proc 'compilation-filter))
              ;; Use (point-max) here so that output comes in
              ;; after the initial text,
              ;; regardless of where the user sees point.
              (set-marker (process-mark proc) (point-max) outbuf)
              (when compilation-disable-input
                (condition-case nil
                    (process-send-eof proc)
                  ;; The process may have exited already.
                  (error nil)))
              (run-hook-with-args 'compilation-start-hook proc)
              (setq compilation-in-progress
                    (cons proc compilation-in-progress)))
          ;; No asynchronous processes available.
          (message "Executing `%s'..." command)
          ;; Fake modeline display as if `start-process' were run.
          (setq mode-line-process
                (list (propertize ":run" 'face 'compilation-warning)))
          (force-mode-line-update)
          (sit-for 0)                   ; Force redisplay
          (save-excursion
            ;; Insert the output at the end, after the initial text,
            ;; regardless of where the user sees point.
            (goto-char (point-max))
            (let* ((buffer-read-only nil) ; call-process needs to modify outbuf
                   (status (call-process shell-file-name nil outbuf nil "-c"
                                         command)))
              (cond ((numberp status)
                     (compilation-handle-exit
                      'exit status
                      (if (zerop status)
                          "finished\n"
                        (format "exited abnormally with code %d\n" status))))
                    ((stringp status)
                     (compilation-handle-exit 'signal status
                                              (concat status "\n")))
                    (t
                     (compilation-handle-exit 'bizarre status status)))))
          ;; Without async subprocesses, the buffer is not yet
          ;; fontified, so fontify it now.
          (let ((font-lock-verbose nil)) ; shut up font-lock messages
            (font-lock-fontify-buffer))
          (set-buffer-modified-p nil)
          (message "Executing `%s'...done" command)
          ))
      ;; Now finally cd to where the shell started make/grep/...
      ;; -- (setq default-directory thisdir) --
      ;; -- don't set the default director --

      ;; The following form selected outwin ever since revision 1.183,
      ;; so possibly messing up point in some other window (bug#1073).
      ;; Moved into the scope of with-current-buffer, though still with
      ;; complete disregard for the case when compilation-scroll-output
      ;; equals 'first-error (martin 2008-10-04).
      (when compilation-scroll-output
        (goto-char (point-max))))

    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)
    ))


(defun grep-read-append-option ()
  "read append options, return t if append is select from minibuffer"
  (interactive)
  (let* ((prompt
          (format "Append to previous results? (y, n, or RET for default action - %s) "
                  (if grep-append-default-option "Append" "Truncate")))

         (choices '(?y ?n ?\n))
         (done nil)
         (cursor-in-echo-area t)
         char
         )
    (while (not done)
      (message
       (apply 'propertize "%s"
              minibuffer-prompt-properties)
       prompt)
      (setq char (read-event))
      (if (eq char 'return)
          (setq char ?\n))
      (if (numberp char)
          (setq done (memq (downcase char) choices))))
    (setq char (downcase char))
    (cond ((= char ?y) t)
          ((= char ?n) nil)
          ((= char ?\n) grep-append-default-option))))

;;(grep-read-append-option)

;;;###autoload
(defun rgrep-append (regexp &optional files dir confirm ifappend)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR.
The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.
With two \\[universal-argument] prefixes, directly edit and run `grep-find-command'.

Collect output in a buffer.  While find runs asynchronously, you
can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] \
in the grep output buffer,
to go to the lines where grep found matches.

This command shares argument histories with \\[lgrep] and \\[grep-find]."
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "Base directory: "
                                          nil default-directory t))
                (confirm (equal current-prefix-arg '(4)))
                ;;(ifappend (y-or-n-p "Result Append?"))
                (ifappend (grep-read-append-option))
                )
           (list regexp files dir confirm ifappend))))))


  (when (and (stringp regexp) (> (length regexp) 0))
    (unless (and dir (file-directory-p dir) (file-readable-p dir))
      (setq dir default-directory))
    (if (null files)
        (if (not (string= regexp grep-find-command))
            (compilation-start regexp 'grep-mode))

      (setq dir (file-name-as-directory (expand-file-name dir)))

      (require 'find-dired)             ; for `find-name-arg'
      (setq templ (if ifappend
                      "find <D> <X> -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>"
                    grep-find-template))
      (let ((command (grep-expand-template
                      ;; grep-find-template
                      templ
                      regexp
                      (concat (shell-quote-argument "(")
                              " " find-name-arg " "
                              (mapconcat #'shell-quote-argument
                                         (split-string files)
                                         (concat " -o " find-name-arg " "))
                              " "
                              (shell-quote-argument ")"))
                      dir
                      (concat
                       (and grep-find-ignored-directories
                            (concat (shell-quote-argument "(")
                                    ;; we should use shell-quote-argument here
                                    " -path "
                                    (mapconcat
                                     #'(lambda (ignore)
                                         (cond ((stringp ignore)
                                                (shell-quote-argument
                                                 (concat "*/" ignore)))
                                               ((consp ignore)
                                                (and (funcall (car ignore) dir)
                                                     (shell-quote-argument
                                                      (concat "*/"
                                                              (cdr ignore)))))))
                                     grep-find-ignored-directories
                                     " -o -path ")
                                    " "
                                    (shell-quote-argument ")")
                                    " -prune -o "))
                       (and grep-find-ignored-files
                            (concat (shell-quote-argument "(")
                                    ;; we should use shell-quote-argument here
                                    " -name "
                                    (mapconcat
                                     #'(lambda (ignore)
                                         (cond ((stringp ignore)
                                                (shell-quote-argument ignore))
                                               ((consp ignore)
                                                (and (funcall (car ignore) dir)
                                                     (shell-quote-argument
                                                      (cdr ignore))))))
                                     grep-find-ignored-files
                                     " -o -name ")
                                    " "
                                    (shell-quote-argument ")")
                                    " -prune -o "))))))
        (when command
          (if confirm
              (setq command
                    (read-from-minibuffer "Confirm: "
                                          command nil nil 'grep-find-history))
            (add-to-history 'grep-find-history command))
          (let ((default-directory dir))
            (if ifappend
                (compilation-start-result-append command 'grep-mode)
              (compilation-start command 'grep-mode)))
          ;; Set default-directory if we started rgrep in the *grep* buffer.
          (if (eq next-error-last-buffer (current-buffer))
              (setq default-directory dir)))))))

(defalias 'rgrep 'rgrep-append)

(provide 'rgrep-append)
