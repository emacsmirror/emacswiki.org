;;; perldb-ui.el --- User Interface for perl debugger

;; Copyright (C) 2007 Free Software Foundation, Inc.
;;
;; Author: Ye Wenbin <wenbinye@gmail.com>
;; Maintainer: Ye Wenbin <wenbinye@gmail.com>
;; Created: 13 Dec 2007
;; Version: 0.01
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; Warning: Not finish yet! But you may have a try and give me some
;; advices.
;; 
;;; Features:
;; 1. Both keybord and mouse support
;; 2. Inpect watch variables, functions, stacks, breakpoints,
;;    global or lexcial variables(maybe not possible).
;; 3. Navigating code source
;; 4. Interactive shell with completion and eval code in editing buffer
;; 5. Debug the evaled code.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'perldb)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'windata)
(require 'gud)
;; maybe not necessary
(require 'gdb-ui)

(defvar perldb-version "0.01"
  "")
(defvar perldb-use-separate-io-buffer nil
  "")
(defvar perldb-many-windows nil
  "")
(defvar perldb-continuation nil
  "")
(defvar perldb-output-sink 'user
  "")
(defvar perldb-command-handler nil
  "")
(defvar perldb-output-handler nil
  "")
(defvar perldb-input-queue nil
  "")
(defvar perldb-temp-file nil
  "")
(defvar perldb-marker-regexp "\032\032\\(.*\\)\n"
  "")
(defvar perldb-prompt-regexp "^  DB<+[0-9]+>+ "
  "")
(defvar perldb-last-status nil
  "")
(defvar perldb-current-item nil
  "")
(defvar perldb-breakpoints nil
  "")
(defvar perldb-current-user-command nil
  "")
(defvar perldb-watchpoints nil
  "")

(defvar perldb-window-configuration
  '((without-io
     (vertical 0.25
               (horizontal 0.50 gub-comint-buffer perldb-locals-buffer)
               (vertical 0.50 source-buffer
                         (horizontal 0.50 perldb-stack-buffer perldb-breakpoints-buffer)))
     0 0)
    (with-io
     (vertical 0.25
               (horizontal 0.50 gub-comint-buffer perldb-locals-buffer)
               (vertical 0.50
                         (horizontal 0.50 source-buffer perldb-inferior-io)
                         (horizontal 0.50 perldb-stack-buffer perldb-breakpoints-buffer)))
     0 0))
  "")

(defvar perldb-source-buffer nil
  "")

;;;###autoload 
(defun perldb-ui (command-line)
  "Run perldb on program FILE in buffer *gud-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'perldb
                            (concat (or (buffer-file-name) "-e 0") " "))))
  (perldb-install-methods)
  (gud-common-init command-line 'gud-perldb-massage-args
                   'perldb-marker-filter)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'perldb-sentinel)
  (set (make-local-variable 'gud-minor-mode) 'perldb)
  (gud-def gud-break  "b %l"         "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove "B %l"         "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step   "s"            "\C-s" "Step one source line with display.")
  (gud-def gud-next   "n"            "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont   "c"            "\C-r" "Continue with display.")
  (gud-def gud-return "r"            "\C-q" "Return from current subroutine.")
  (gud-def gud-print  "p %e"         "\C-p" "Evaluate perl expression at point.")
  (gud-def gud-until  "c %l"         "\C-u" "Continue to current line.")
  (gud-def gud-dump   "x %e"         "\C-x" "Dumper data")
  (setq gud-find-file 'perldb-find-file)
  (setq comint-input-sender 'perldb-send)
  (if perldb-use-separate-io-buffer (perldb-clear-inferior-io))
  (if perldb-many-windows (perldb-setup-windows))
  (setq comint-prompt-regexp perldb-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  ;; init variable
  (setq gdb-buffer-fringe-width (car (window-fringes)))
  (run-hooks 'perldb-mode-hook))

(defun perldb-sentinel (proc msg)
  (dolist (bp perldb-breakpoints)
    (apply 'perldb-remove-breakpoint (overlay-get bp 'break-position)))
  (gud-sentinel proc msg))

(defun perldb-install-methods ()
  (with-temp-buffer
    (let ((conf "~/.perldb")
          found)
      (if (file-exists-p conf)
          (progn
            (insert-file-contents conf)
            (goto-char (point-min))
            (when (re-search-forward "^#### perldb.el version \\([0-9]+\\.[0-9]+\\)" nil t)
              (if (string= (match-string 1) perldb-version)
                  (setq found t)
                (let ((beg (line-beginning-position)) end)
                  (if (re-search-forward "^#### end perldb.el" nil t)
                      (setq end (line-end-position))
                    (setq end (point-max)))
                  (delete-region begin end)))))
        (insert "# -*- perl -*-\n"))
      (unless found
        (message "Install methods...")
        (goto-char (point-max))
        (insert "#### perldb.el version " perldb-version
                "
{
    package DB::emacs;
    use constant PRE => \"\\032\\032pre-prompt\\n\";
    use constant POST => \"\\032\\032post-prompt\";
    use subs qw(output);
    our $o;
    sub output {
        my $val = shift;
        $val = $o unless defined $val;
        $val = '<undef>' unless defined $val;
        print PRE;
        print $val;
        print POST;
    }
    sub status {
        output (
            sprintf \"((current-sub . \\\"%s\\\")\\n (functions . %d)\\n (includes . %d))\",
            $DB::emacs::sub, scalar( keys %DB::sub ), scalar( keys %INC ));
    }
    # FIXME: How to inhibit this error 
    sub trace {
        print PRE;
        DB::print_trace($DB::OUT, 1);
        print POST;
    }
    sub breakpoints {
        print PRE;
        DB->cmd_L('b');
        print POST;
    }
    sub functions {
        output \"(\" . join(\"\\n\", map { qq(\"$_\") } sort keys %DB::sub) . \")\";
    }
    sub includes {
        output \"(\" . join(\"\\n\", map { s/\\.pm$//; s/\\//::/g; qq(\"$_\") } sort keys %INC) . \")\";
    }
}
#### end perldb.el
")
        (write-region (point-min) (point-max) conf)))))

;; FIXME: I need to redefine this function
(defun gud-basic-call (command)
  "Invoke the debugger COMMAND displaying source in other window."
  (interactive)
  (gud-set-buffer)
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (or proc (error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer gud-comint-buffer)
      (save-restriction
        (widen)
        (if (marker-position gud-delete-prompt-marker)
            ;; We get here when printing an expression.
            (goto-char gud-delete-prompt-marker)
          (goto-char (process-mark proc))
          (forward-line 0))
        (if (looking-at comint-prompt-regexp)
            (set-marker gud-delete-prompt-marker (point)))
        (if (memq gud-minor-mode '(gdbmi gdba perldb))
            (apply comint-input-sender (list proc command))
          (process-send-string proc (concat command "\n")))))))

 
;; output filter
(defun perldb-marker-filter (string)
  (when gdb-enable-debug
    (push (cons 'recv string) gdb-debug-log)
    (if (and gdb-debug-log-max
             (> (length gdb-debug-log) gdb-debug-log-max))
        (setcdr (nthcdr (1- gdb-debug-log-max) gdb-debug-log) nil)))
  ;; Recall the left over gud-marker-acc from last time.
  (setq gud-marker-acc (concat gud-marker-acc string))
  ;; Start accumulating output for the GUD buffer.
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match (concat "\\(" perldb-marker-regexp "\\)\\|\\(" perldb-prompt-regexp "\\)")
                         gud-marker-acc)
      (if (match-string 1 gud-marker-acc)
          (let ((annotation (match-string 2 gud-marker-acc)))
            (setq output (perldb-concat-output
                          output (substring gud-marker-acc 0 (match-beginning 0)))
                  gud-marker-acc (substring gud-marker-acc (match-end 0)))
            (cond ((string= annotation "pre-prompt")
                   ;; maybe just setting perldb-output-sink to 'emacs is enough
                   (perldb-pre-prompt))
                  ((string= annotation "post-prompt")
                   ;; maybe just setting perldb-output-sink to 'user is enough
                   (perldb-post-prompt))
                  ((string-match "^\\(\\([a-zA-Z]:\\)?[^:\n]*\\):\\([0-9]*\\):" annotation)
                   ;; Extract the frame position from the marker.
                   (setq gud-last-frame
                         (cons (match-string 1 annotation)
                               (string-to-number (match-string 3 annotation)))))))
        (setq output
              (perldb-concat-output output (substring gud-marker-acc 0 (match-beginning 0))))
        (if (consp perldb-current-item)
            (setq output "")
          (setq
           ;; add prompt to output
           output (concat output (match-string 3 gud-marker-acc))))
        (setq gud-marker-acc (substring gud-marker-acc (match-end 0)))
        (perldb-prompt)))
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; gud-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" gud-marker-acc)
        (progn
          ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring gud-marker-acc
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq gud-marker-acc
                (substring gud-marker-acc (match-beginning 0))))
      (setq output (perldb-concat-output output gud-marker-acc)
            gud-marker-acc ""))
    ;;(message "filter '%S' '%S'" string output)
    output))

(defun perldb-concat-output (so-far new)
  (let ((sink perldb-output-sink))
    (cond
     ((or (eq sink 'user) (eq sink 'inferior))
      ;; handler output, use for watchpoint
      (save-match-data
        (let ((handlers perldb-output-handler)
              result)
          (while handlers
            (funcall (car handlers) new)
            (setq handlers (cdr handlers)))))
      (if (eq sink 'user)
          (concat so-far new)
        (perldb-append-to-inferior-io new)
        so-far))
     ((or (eq sink 'pre-emacs) (eq sink 'post-emacs)) so-far)
     ((eq sink 'emacs)
      (perldb-append-to-partial-output new)
      so-far)
     (t
      (perldb-resync)
      (error "Bogon output sink %S" sink)))))

(defun perldb-resync()
  (setq gud-running nil
        gud-marker-acc "")
  (setq perldb-output-sink (if perldb-use-separate-io-buffer 'inferior 'user)
        perldb-input-queue nil
        perldb-current-item nil
        perldb-last-status nil
        ;; gdb-debug-log nil
        ))

(defalias 'perldb-append-to-partial-output 'gdb-append-to-partial-output)
(defalias 'perldb-clear-partial-output 'gdb-clear-partial-output)

(defun perldb-append-to-inferior-io (string)
  (with-current-buffer (gdb-get-buffer-create 'perldb-inferior-io)
    (goto-char (point-max))
    (insert-before-markers string))
  (if (not (string-equal string ""))
      (gdb-display-buffer (perldb-get-buffer 'perldb-inferior-io) t)))

(defun perldb-clear-inferior-io ()
  (with-current-buffer (gdb-get-buffer-create 'perldb-inferior-io)
    (erase-buffer)))

(defun perldb-locals-buffer-name ()
  "*perldb watch buffer*")
(defun perldb-stack-buffer-name ()
  "*perldb stack*")
(defun perldb-breakpoints-buffer-name ()
  "*perldb breakpoints*")
(defun perldb-inferior-io-name ()
  "*perldb io*")

(defun perldb-pre-prompt ()
  (let ((sink perldb-output-sink))
    (cond ((or (eq sink 'user) (eq sink 'inferior))
           (setq perldb-output-sink 'emacs))
          (t
           (perldb-resync)
           (error "Phase error in perldb-pre-prompt (got %s)" sink)))))

(defun perldb-post-prompt ()
  (let ((sink perldb-output-sink))
    (cond ((eq sink 'emacs)
           (setq perldb-output-sink (if perldb-use-separate-io-buffer 'inferior 'user)))
          (t
           (gdb-resync)
           (error "Phase error in perldb-post-prompt (got %s)" sink)))))

(defun perldb-prompt ()
  (if (or (null perldb-current-item) (stringp perldb-current-item))
      (let ((handlers perldb-command-handler)
            (item perldb-current-item)
            case-fold-search
            handler)
        (when item
          (while handlers
            (setq handler (car handlers))
            (if (string-match (car handler) item)
                (progn
                  (funcall (cdr handler) item)
                  (setq handlers nil))
              (setq handlers (cdr handlers)))))
        (push 
         (list (perldb-make-command "$DB::emacs::sub=$DB::sub;DB::emacs::status()") 'perldb-check-status)
         perldb-input-queue))
    (funcall (cadr perldb-current-item)))
  (let ((input (perldb-dequeue-input)))
    (if input
        (perldb-send-item input))))

 
;; Handler
(defmacro perldb-define-trigger (trigger buf-key command handler
                                         &rest body)
  (declare (indent 1))
  `(progn
     (defun ,trigger (&rest args)
       (if (perldb-get-buffer ',buf-key)
           (push (list (perldb-make-command ,command) ',handler) perldb-input-queue)))
     (defun ,handler (&rest args)
       (with-current-buffer (perldb-get-buffer ',buf-key)
         (erase-buffer)
         (insert-buffer-substring (perldb-get-buffer 'gdb-partial-output-buffer))
         (progn ,@body)))))

(perldb-define-trigger perldb-invalidate-stack
  perldb-stack-buffer "DB::emacs::trace()" perldb-info-stack
  (goto-char (point-min))
  ;; set FIXME in .perldb
  (if (re-search-forward "^@ = DB::DB called from file" nil t)
      (delete-region (point-min) (progn (forward-line 1) (point)))))

(perldb-define-trigger perldb-invalidate-functions
  perldb-functions-buffer
  "DB::emacs::functions()"
  perldb-info-functions)

(perldb-define-trigger perldb-invalidate-includes
  perldb-includes-buffer
  "DB::emacs::includes()"
  perldb-info-includes)

(perldb-define-trigger perldb-invalidate-breakpoints
  perldb-breakpoints-buffer
  "DB::emacs::breakpoints()"
  perldb-info-breakpoints
  (let ((breakpoints perldb-breakpoints)
        bp new-breakpoints file line)
    (goto-char (point-min))
    ;; add new breakpoints
    (while (not (eobp))
      (setq file (buffer-substring (point) (1- (line-end-position))))
      (forward-line 1)
      (while (looking-at "^\\s-+\\([0-9]+\\):")
        (setq line (string-to-number (match-string 1)))
        ;; (message "file: %s line: %d" file line)
        (unless (setq bp (perldb-find-breakpoints file line))
          (setq bp (perldb-put-breakpoint file line)))
        (push bp new-breakpoints)
        (forward-line 2)))
    ;; remove not exists breakpoints
    (dolist (bp breakpoints)
      (unless (memq bp new-breakpoints)
        (apply 'perldb-remove-breakpoint (overlay-get bp 'break-position))))
    ;; install to perldb-breakpoints
    (setq perldb-breakpoints new-breakpoints)))

(defun perldb-check-status ()
  (ignore-errors
    (with-current-buffer (perldb-get-buffer 'gdb-partial-output-buffer)
      (goto-char (point-min))
      (let ((status (read (current-buffer)))
            (last perldb-last-status)
            (handlers '((current-sub . perldb-invalidate-stack)
                        (functions . perldb-invalidate-functions)
                        (includes . perldb-invalidate-includes))))
        (dolist (st status)
          (unless (equal st (car last))
            (funcall (assoc-default (car st) handlers)))
          (setq last (cdr last)))
        (setq perldb-last-status status)))))

(defun perldb-clear-buffer (buf-key)
  (let ((buf (perldb-get-buffer buf-key)))
    (if buf (with-current-buffer buf (erase-buffer)))))

(defun perldb-reset (&rest args)
  (perldb-clear-buffer 'perldb-locals-buffer)
  (perldb-clear-buffer 'perldb-stack-buffer)
  (perldb-clear-buffer 'perldb-inferior-io)
  (setq perldb-current-item nil
        perldb-current-user-command nil
        perldb-watchpoints nil))

 
;; Command sender
(defun perldb-enqueue-input (item)
  (if gud-running
      (push item perldb-input-queue)
    (perldb-send-item item)))

(defun perldb-dequeue-input ()
  (let ((queue perldb-input-queue))
    (and queue
         (let ((last (car (last queue))))
           (unless (nbutlast queue) (setq perldb-input-queue '()))
           last))))

(defun perldb-send-item (item)
  (if gdb-enable-debug (push (cons 'send item) gdb-debug-log))
  (setq perldb-current-item item)
  (setq perldb-output-sink (if perldb-use-separate-io-buffer 'inferior 'user))
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (if (stringp item)                  ; it is user command
        (process-send-string proc item)
      ;; it is editor command
      (perldb-clear-partial-output)
      (process-send-string proc (car item)))))

(defun perldb-send (proc string)
  (with-current-buffer gud-comint-buffer
    (if (and (string-match "\\\\$" string)
             (not comint-input-sender-no-newline)) ;;Try to catch C-d.
        (setq perldb-continuation (concat perldb-continuation string "\n"))
      (let ((item (concat perldb-continuation string "\n")))
        (setq perldb-current-user-command item)
        (perldb-enqueue-input item)
        (setq perldb-continuation nil)))))

(defun perldb-make-command (cmd)
  (concat "@DB::emacs::to_watch=@DB::to_watch;@DB::to_watch=();"
          cmd
          ";@DB::to_watch=@DB::emacs::to_watch;$#DB::hist--\n"))

 
;; find file function
(defun perldb-find-file (file)
  (let ((default-directory (buffer-local-value 'default-directory gud-comint-buffer))
        old-win buf)
    (if perldb-source-buffer
        (setq old-win (get-buffer-window perldb-source-buffer)))
    (setq buf (find-file-noselect file))
    (setq perldb-source-buffer buf)
    ;; reuse the source window
    (if old-win
        (set-window-buffer old-win buf))
    (unless buf
      (setq buf (gdb-get-buffer-create 'perldb-temp-buffer))
      (perldb-invalidate-temp-buffer))
    buf))

 
;; Window setup
(defun perldb-many-windows (arg)
  "Toggle the number of windows in the basic arrangement.
With arg, display additional buffers iff arg is positive."
  (interactive "P")
  (setq perldb-many-windows
        (if (null arg)
            (not perldb-many-windows)
          (> (prefix-numeric-value arg) 0)))
  (message (format "Display of other windows %sabled"
                   (if perldb-many-windows "en" "dis")))
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer))
      (condition-case nil
          (perldb-restore-windows)
        (error nil))))

(defun perldb-restore-windows ()
  (interactive)
  (if perldb-many-windows
      (perldb-setup-windows)
    (pop-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (split-window)
    (perldb-set-window-buffer (next-window) 'source-buffer)))

(defun perldb-use-separate-io-buffer (arg)
  "Toggle separate IO for debugged program.
With arg, use separate IO iff arg is positive."
  (interactive "P")
  (setq perldb-use-separate-io-buffer
        (if (null arg)
            (not perldb-use-separate-io-buffer)
          (> (prefix-numeric-value arg) 0)))
  (message (format "Separate IO %sabled"
                   (if perldb-use-separate-io-buffer "en" "dis")))
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer))
      (condition-case nil
          (if perldb-use-separate-io-buffer
              (if perldb-many-windows (perldb-restore-windows))
            (kill-buffer (perldb-inferior-io-name)))
        (error nil))))

(defun perldb-setup-windows ()
  (let ((windata-data-restore-function 'perldb-set-window-buffer))
    (windata-restore-winconf (cdr (assoc
                                   (if perldb-use-separate-io-buffer
                                       'with-io
                                     'without-io)
                                   perldb-window-configuration)))))

(defun perldb-get-buffer (id)
  (cond ((assoc id gdb-buffer-rules-assoc)
         (gdb-get-buffer-create id))
        ((eq id 'gub-comint-buffer) gud-comint-buffer)
        ((eq id 'source-buffer)
         (if gud-last-last-frame
             (perldb-find-file (car gud-last-last-frame))
           (gdb-get-buffer 'perldb-temp-buffer)))))

(defun perldb-set-window-buffer (win id)
  (let ((buf (perldb-get-buffer id)))
    (if buf
        (set-window-buffer win buf))))

 
;; set or clear breakpoint
(defalias 'perldb-put-string 'gdb-put-string)

(defun perldb-find-breakpoints (file line)
  (let ((breakpoints perldb-breakpoints))
    (when breakpoints
      (let ((pos (list file line))
            found)
        (while (and (not found) breakpoints)
          (if (equal (overlay-get (car breakpoints) 'break-position) pos)
              (setq found (car breakpoints)))
          (setq breakpoints (cdr breakpoints)))
        found))))

(defun perldb-put-breakpoint (file line)
  (unless (perldb-find-breakpoints file line)
    (let ((breakpoints perldb-breakpoints))
      (with-current-buffer (perldb-find-file file)
        (goto-char (point-min))
        (forward-line (1- line))
        (let ((start (line-beginning-position))
              (putstring (propertize "B" 'help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt"))
              (source-window (get-buffer-window (current-buffer) 0))
              ov)
          (if (display-images-p)
              (if (>= (or left-fringe-width
                          (if source-window (car (window-fringes source-window)))
                          gdb-buffer-fringe-width) 8)
                  (perldb-put-string
                   nil start
                   '(left-fringe breakpoint breakpoint-enabled))
                (when (< left-margin-width 2)
                  (save-current-buffer
                    (setq left-margin-width 2)
                    (if source-window
                        (set-window-margins
                         source-window
                         left-margin-width right-margin-width))))
                (put-image
                 (or breakpoint-enabled-icon
                     (setq breakpoint-enabled-icon
                           (find-image `((:type xpm :data
                                                ,breakpoint-xpm-data
                                                :ascent 100 :pointer hand)
                                         (:type pbm :data
                                                ,breakpoint-enabled-pbm-data
                                                :ascent 100 :pointer hand)))))
                 start putstring 'left-margin))
            (when (< left-margin-width 2)
              (save-current-buffer
                (setq left-margin-width 2)
                (let ((window (get-buffer-window (current-buffer) 0)))
                  (if window
                      (set-window-margins
                       window left-margin-width right-margin-width)))))
            (perldb-put-string
             (propertize putstring 'face 'breakpoint-enabled)
             start))
          (setq ov (overlays-in start start))
          (unless (= (length ov) 1)
            (while (and ov
                        (not (or (overlay-get (car ov) 'put-break)
                                 (overlay-get (car ov) 'put-image))))
              (setq ov (cdr ov))))
          (setq ov (car ov))
          (overlay-put ov 'break-position (list file line))
          (add-to-list 'perldb-breakpoints ov)
          ov)))))

(defun perldb-remove-breakpoint (file line)
  (let ((bp (perldb-find-breakpoints file line)))
    (when bp
      (setq perldb-breakpoints (delq bp perldb-breakpoints))
      (delete-overlay bp))))

 
(defun perldb-watchpoint-handler (output)
  (let (start)
    (while (string-match "^Watchpoint [0-9]+:\\s-+\\(.*\\) changed" output start)
      (perldb-invalidate-watchpoints (list (match-string 1 output)))
      (setq start (match-end 0)))
    output))

(defun perldb-invalidate-watchpoints (&rest args)
  (setq args (car args))
  (when (perldb-get-buffer 'perldb-locals-buffer)
    (let ((expr "1")                    ; dummy expr
          case-fold-search)
      (if (listp args)                ; update some watchpoint
          (setq expr (car args))
        (if (string-match "^w " perldb-current-user-command)
            (setq expr (substring perldb-current-user-command 2 -1))))
      (push
       (list
        (perldb-make-command (format "$DB::emacs::o=%s; DB::emacs::output()" expr))
        'perldb-info-watchpoints)
       perldb-input-queue))))

(defun perldb-find-watchpoint (expr)
  (let ((rest (member expr perldb-watchpoints)))
    (when rest
      (- (length perldb-watchpoints)
         (length rest)))))

(defun perldb-trim-whitespace (str)
  (setq str (replace-regexp-in-string "\\`\\s-*" "" str))
  (replace-regexp-in-string "\\s-*\\'" "" str))

(defun perldb-info-watchpoints ()
  (let (case-fold-search expr line)
    (with-current-buffer (perldb-get-buffer 'perldb-locals-buffer)
      (goto-char (point-min))
      (if (string-match "^W " perldb-current-user-command)
          (if (string-match "^W\\s-+\\*" perldb-current-user-command)
              (progn
                (erase-buffer)
                (setq perldb-watchpoints nil))
            (setq expr (perldb-trim-whitespace (substring perldb-current-user-command 2 -1)))
            (forward-line (perldb-find-watchpoint expr))
            (delete-region (point) (progn (forward-line 1) (point)))
            (setq perldb-watchpoints (delete expr perldb-watchpoints)))
        (when (string-match "DB::emacs::o=\\(.*\\); DB::emacs::output()"
                            (car perldb-current-item))
          (setq expr (perldb-trim-whitespace (match-string 1 (car perldb-current-item))))
          (if (string-match "^w " perldb-current-user-command)
              (progn
                (setq perldb-watchpoints (nconc perldb-watchpoints (list expr)))
                (goto-char (point-max))
                (setq line t))
            (setq line (perldb-find-watchpoint expr))
            (if (null line)
                (message "Can't found expr '%s' in watchpoints" expr)
              (forward-line line)
              (delete-region (point) (progn (forward-line 1) (point)))))
          (when line
            (insert (format "%-15s '%s'\n" expr
                            (with-current-buffer (perldb-get-buffer 'gdb-partial-output-buffer)
                              (let ((str (buffer-string)))
                                (setq str (replace-regexp-in-string "\n" "\\n" str nil t))
                                (if (< (length str) 30)
                                    str
                                  (propertize (concat (substring str 0 27) (propertize "..." 'face 'underline))
                                              'help-echo (buffer-string)))))))))))))

 
;; Init
(unless (assoc 'perldb-locals-buffer gdb-buffer-rules-assoc)
  (setq gdb-buffer-rules-assoc
        (nconc gdb-buffer-rules-assoc
               '((perldb-locals-buffer perldb-locals-buffer-name)
                 (perldb-stack-buffer perldb-stack-buffer-name)
                 (perldb-breakpoints-buffer perldb-breakpoints-buffer-name)
                 (perldb-inferior-io perldb-inferior-io-name))))
  (add-to-list 'perldb-command-handler
               '("^[bB] " . perldb-invalidate-breakpoints))
  (add-to-list 'perldb-command-handler
               '("^[wW] " . perldb-invalidate-watchpoints))
  (add-to-list 'perldb-command-handler
               '("^R\\s-*$" . perldb-reset))
  (add-to-list 'perldb-output-handler 'perldb-watchpoint-handler))

(provide 'perldb-ui)
;;; perldb.el ends here
