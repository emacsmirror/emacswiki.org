;;; per-win.el --- a minor mode for persisent window-relative values of point

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.52
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Emacs conveniently allows one to work on different parts of the same
;; buffer at the same time, but the rules governing buffer display are, for
;; some people's editing habits, less than ideal.  Suppose for example that
;; one is editing two parts of buffer <buf> in windows <win1> and <win2>,
;; switches briefly to another buffer in <win2>, then returns to editing
;; <buf> in <win2>.  This latter window will now display the same part of
;; <buf> as <win1>, rather than the portion that one was just recently
;; editing in it.  The per-win package creates persistent values of point and
;; window-start, so that in cases like that just described <win2> will return
;; to its previous position in <buf>.

;; In some cases, as when another Lisp program wants to move point in a
;; buffer and then display that buffer in a window, it makes sense for per-win
;; not to position point in that window.  The package is reasonably
;; intelligent in identifying when that is so.  Further control is given by
;; the variables `per-win-reposition-temp-buffers' and
;; `per-win-reposition-tests'.

;; To install the package, put this file somewhere in your load path and put:
;;
;;  (require 'per-win)
;;
;; in your .emacs.  To toggle per-win on and off, use the command
;; `per-win-mode'.

;; This package has not been tested on versions earlier than v22.3.1.  

;;; Code: 

;;; Declarations

(defgroup per-win nil
  "Enable each window to remember its value of point in each buffer."
  :group 'convenience)

(defcustom per-win-reposition-temp-buffers nil
  "If nil, per-win does not position temporary buffers. 
Temp buffers are understood to be those whose names match the
regexp \"^\\*.+\\*$\"."
  :type 'boolean
  :group 'per-win)

(defcustom per-win-reposition-tests nil
  "List of functions that control whether per-win positions point in a buffer.  
When the buffer displayed in a window changes, per-win calls each
function in this list with two arguments, the window in question
and the buffer about to be displayed.  If any function returns
nil, per-win does not position the buffer."
  :type 'hook
  :group 'per-win)

(defvar per-win-mode nil 
  "Non-nil if per-win-mode is enabled.  

Do not set this variable directly.  Use the command
`per-win-mode' instead.  See the documentation of that command
for a description of the mode.")

(defvar per-win-mode-hook nil 
  "Hook run when enabling and disabling per-win-mode.")

(defvar per-win-mode-on-hook nil 
  "Hook run when enabling per-win-mode.")

(defvar per-win-mode-off-hook nil 
  "Hook run when disabling per-win-mode.")

;; Alist that records all per-win's data. Elements are of the form (WIN
;; BUF-DATA BUF-DATA ...). Each BUF-DATA is of the form (BUFFER MARKER
;; MARKER), where the markers record, respectively, the last values for
;; window-point and window-start for BUFFER in WIN.  (We can't store the
;; various BUF-DATA for each WIN in a window-parameter because (a)
;; window-parameters aren't preserved by save-window-excursion, and (b)
;; they're not available on v22.)
(defvar per-win-alist nil)

;;; Mode Definition

(defun per-win-mode (&optional arg) 
  "Toggle per-win-mode on and off.

With argument ARG, turn per-win-mode on if and only if ARG is t or positive.

Per-win mode changes how Emacs selects point when displaying a
buffer in a window.  While per-win-mode is enabled, each window
keeps a record of the last value of point in every buffer that
has been displayed in that window.  When switched back to one of
those buffers, the window will display that portion of the buffer
that was last displayed in that window.  

The mode is intelligent in inferring when it should defer to
other programs in determining window-point.  Further control over
when repositioning should happen is provided by the variables
`per-win-reposition-temp-buffers' and `per-win-reposition-tests'."

  (interactive "P")
  (setq per-win-mode (if (not arg) (not per-win-mode)
                         (> (prefix-numeric-value arg) 0)))
  (if per-win-mode
      (progn 
        (setq per-win-alist nil)      
        (ad-enable-regexp "per-win")
        (ad-activate-regexp "per-win")
        (add-hook 'temp-buffer-setup-hook 'per-win-before-temp-buffer)
        (add-hook 'window-configuration-change-hook 'per-win-remove-dead-windows)
        (add-hook 'kill-buffer-hook 'per-win-remove-killed-buffer)
        (message "Per-window point values are on")
        (run-hooks 'per-win-mode-on-hook))
    (ad-disable-regexp "per-win")
    (ad-activate-regexp "per-win")     
    (remove-hook 'temp-buffer-setup-hook 'per-win-before-temp-buffer)
    (remove-hook 'window-configuration-change-hook 'per-win-remove-dead-windows)
    (remove-hook 'kill-buffer-hook 'per-win-remove-killed-buffer)
    (message "Per-window point values are off")
    (run-hooks 'per-win-mode-off-hook))
  (run-hooks 'per-win-mode-hook))

;;; Advising Primitives

;; The following four functions are the main primitives that change which
;; buffers are displayed in windows.  We advise them so that they record
;; window-point and window-start for the relevant window(s) before a change in
;; display, then call the repositioning function after the change in display.
(defadvice switch-to-buffer (around per-win)
  (per-win-register-win (selected-window))
  ad-do-it
  (per-win-reposition (selected-window) ad-return-value))

(defadvice set-window-buffer (around per-win)
  (per-win-register-win (ad-get-arg 0))
  ad-do-it
  (per-win-reposition window (get-buffer buffer-or-name)))

(defadvice display-buffer (around per-win)
  (mapc 'per-win-register-win (window-list))
  ad-do-it
  (per-win-reposition ad-return-value (get-buffer (ad-get-arg 0))))

(defadvice replace-buffer-in-windows (around per-win)
  (let* ((buf (or (ad-get-arg 0) (current-buffer)))
         (winlist (get-buffer-window-list buf 'no-minibuf)))
    (mapc 'per-win-register-win winlist)
    ad-do-it
    (dolist (win winlist)
      (per-win-reposition win (window-buffer win)))))

;; The following two primitives call buffer display functions from C code,
;; bypassing our advice.  So we also advise them to record window data and
;; call for repositioning afterwards.
(defadvice kill-buffer (around per-win)
  (let* ((buf (or (ad-get-arg 0) (current-buffer)))
         (winlist (get-buffer-window-list buf 'no-minbuf t)))
    (mapc 'per-win-register-win winlist)
    ad-do-it
    ;; kill-buffer returns nil if the buffer was not killed, in which case we
    ;; don't need to reposition.
    (when ad-return-value 
      (dolist (win winlist)
        (per-win-reposition win (window-buffer win))))))

(defadvice bury-buffer (around per-win)
  ;; Bury buffer only changes which buffer is displayed if called with nil
  ;; argument and if the current buffer is displayed in the selected
  ;; window.  That's the case we care about.
    (if (and (not (ad-get-arg 0))
             (eq (current-buffer) (window-buffer (selected-window))))
        (progn
          (per-win-register-win (selected-window))
          ad-do-it
          (per-win-reposition (selected-window)
                              (window-buffer (selected-window))))
      ad-do-it))

;; The special form with-output-to-temp-buffer also calls one of our advised
;; primitives from within C code.  But here we have a hook we can use, so we
;; don't need advice.  (We don't call for repositioning after display, on the
;; assumption that this form is only used when generating new content for the
;; temp buffer, in which case repositioning would be pointless.)
(defun per-win-before-temp-buffer () 
  (mapc 'per-win-register-win  (window-list nil 'no-minibuf)))

;;; Recording Data

;; Called with argument WIN, records window-point and window-start for the
;; buffer currently displayed in WIN.
(defun per-win-register-win (win)
  ;; Never bother with minibuffers.
  (unless (window-minibuffer-p win)
    ;; Get the window data from the main alist, and the buffer data from
    ;; that window's own alist.  They might be nil, but that doesn't change
    ;; the routine.
    (let* ((win-data (assq win per-win-alist))
           (buf (window-buffer win))
           (buf-data (assq buf win-data)))
      (setq per-win-alist (delq win-data per-win-alist)
            win-data (delq buf-data win-data))
      ;; If we had data on buf for this window, trash the pointers.
      (when buf-data
        (set-marker (nth 1 buf-data) nil)
        (set-marker (nth 2 buf-data) nil))
      ;; Add the new data for window-point and window-start to the window's
      ;; info for that buffer, then add the window's data to the main alist.
      (setq buf-data (list buf
                           (set-marker (make-marker) (window-point win) buf)
                           (set-marker (make-marker) (window-start win) buf))
            win-data (cons win (cons buf-data (cdr win-data)))
            per-win-alist (cons win-data per-win-alist)))))

;;; Repositioning Re-displayed Buffers

(defun per-win-reposition (win buf)
  ;; First, check to see whether BUF is one we should ignore.
  (unless (per-win-exception-p win buf)
    ;; If not, check to see if there's point and window-start info for
    ;; displaying BUF in WIN. Reposition if so.
    (let* ((win-data (assq win per-win-alist))
           (buf-data (assq buf win-data)))
      (when buf-data
            (set-window-point win (nth 1 buf-data))
            (set-window-start win (nth 2 buf-data))))))

;; Function called to determine whether per-win should reposition a
;; buffer.  If it returns t, per-win does not reposition.
(defun per-win-exception-p (win buf)
  (or (per-win-defer-to-program-p win buf)
      (per-win-temp-buffer-exception-p buf)
      (per-win-run-reposition-tests win buf)))

;; Check to see whether a lisp program has repositioned point in BUF, in
;; which case return t.  (For example, when looking up a function definition
;; via `describe-function', point is moved to the function definition before
;; the library that defines the function is displayed; we then don't want to
;; move point away from the definition after display.)  This will fail in one
;; edge case that I doubt ever arises in the wild.
(defun per-win-defer-to-program-p (win buf)
  (let ((point (save-current-buffer
                 (set-buffer buf)
                 (point)))
        (l1 (mapcar 
             (lambda (x) (window-point x))
             (delq win (get-buffer-window-list buf))))
        (l2 (mapcar 
             (lambda (x) 
               (if (not (eq buf (window-buffer (car x))))
                   (per-win-safe-marker-pos (cadr (assq buf x)))))
             per-win-alist)))
    (not (or (memq point l1)
             (memq point l2)))))

(defun per-win-safe-marker-pos (m)
  (if (markerp m) 
      (marker-position m)))

;; Check to see if temp buffers are disallowed for repositioning and, if so,
;; whether BUF is a temp buffer.
(defun per-win-temp-buffer-exception-p (buf)
  (and (not per-win-reposition-temp-buffers)
       (string-match "^\\*.+\\*$" (buffer-name buf))))

;; Check WIN and BUF against test functions the user has supplied.  
(defun per-win-run-reposition-tests (win buf)
 (memq nil (mapcar (lambda (x) (funcall x buf win))
                   per-win-reposition-tests)))

;;; List Maintenance

;; Called when a buffer is killed; removes any info about that buffer from
;; per-win-alist.
(defun per-win-remove-dead-windows ()
  (setq per-win-alist (delq nil 
                            (mapcar (lambda (x) (and (window-live-p (car x)) x))
                                    per-win-alist))))

;; Called when the window configuration changes; removes info about any
;; newly deleted windows from per-win-alist.
(defun per-win-remove-killed-buffer ()
  (setq per-win-alist (mapcar (lambda (x) (delq (assq (current-buffer) x) x))
                              per-win-alist)))

(provide 'per-win)

;;; per-win.el ends here
