;;; rebound.el --- a minor mode for persisent window-relative values of point

;; Copyright (c) 2011 Alp Aker

;; Author: Alp Aker <aker@pitt.edu>
;; Version: 0.58
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
;; editing in it.  The Rebound package creates persistent values of point and
;; window-start, so that in cases like that just described <win2> will return
;; to its previous position in <buf>.

;; In some cases, as when another Lisp program first moves point in a buffer
;; and then displays that buffer in a window, it makes sense for Rebound not
;; to position point in that window.  The package is reasonably intelligent
;; in identifying when that is so.  Further control is given by the variables
;; `rebound-no-reposition-names', `rebound-no-reposition-regexps', and
;; `rebound-reposition-tests'.  Note also that rebound always ignores
;; minibuffer windows.

;; To install the package, put this file in your load path and put:
;;
;;  (require 'rebound)
;;
;; in your .emacs.  To toggle rebound on and off, use the command
;; `rebound-mode'.

;;; Code: 

(unless (<= 22 emacs-major-version)
  (error "Rebound requires at least version 22"))

;;; Declarations: User Level

(defgroup rebound nil
  "Enable each window to remember its value of point in each buffer."
  :group 'convenience)

(defcustom rebound-no-reposition-regexps '("^\\*.+\\*$")
  "Rebound does not reposition a buffer whose name matches a regexp in this list."
  :group 'rebound
  :type '(repeat regexp))

(defcustom rebound-no-reposition-names nil
  "Rebound does not reposition a buffer whose name is an elements of this list."
  :group 'rebound
  :type '(repeat string))

(defcustom rebound-reposition-tests nil
  "List of functions that control whether rebound positions point in a buffer.
When the buffer displayed in a window changes, rebound calls each
function in this list with two arguments, the window in question
and the buffer about to be displayed.  If any function returns
nil, rebound does not position the buffer."
  :group 'rebound
  :type 'hook)

(defcustom rebound-mode-hook nil 
  "Hook run when enabling and disabling rebound mode."
  :group 'rebound
  :type 'hook)

(defcustom rebound-mode-on-hook nil 
  "Hook run when enabling rebound mode."
  :group 'rebound
  :type 'hook)

(defcustom rebound-mode-off-hook nil 
  "Hook run when disabling rebound mode."
  :group 'rebound
  :type 'hook)

(defvar rebound-mode nil 
  "Non-nil if rebound mode is enabled.  

Do not set this variable directly.  Use the command
`rebound-mode' instead.  See the documentation of that command
for a description of the mode.")

;;; Declarations: Internal 

;; Alist that records all rebound's data. Elements are of the form (WIN
;; BUF-DATA BUF-DATA ...). Each BUF-DATA is of the form (BUFFER MARKER
;; MARKER), where the markers record, respectively, the last values for
;; window-point and window-start for BUFFER in WIN.  (We can't store the
;; various BUF-DATA for each WIN in a window-parameter because (a)
;; window-parameters aren't preserved by save-window-excursion, and (b)
;; they're not available on v22.)
(defvar rebound-alist nil)

;;; Mode Definition

(defun rebound-mode (&optional arg) 
  "Toggle rebound mode on and off.

With argument ARG, turn rebound mode on if and only if ARG is t or positive.

Rebound mode changes how Emacs selects point when displaying a
buffer in a window.  While rebound mode is enabled, each window
keeps a record of the last value of point in every buffer that
has been displayed in that window.  When switched back to one of
those buffers, the window will display that portion of the buffer
that was last displayed in that window.  

The mode is intelligent in inferring when it should defer to
other programs in determining window-point.  Further control over
when repositioning should happen is provided by the variables
`rebound-reposition-temp-buffers' and `rebound-reposition-tests'."

  (interactive "P")
  (setq rebound-mode (if (not arg)
                         (not rebound-mode)
                       (> (prefix-numeric-value arg) 0)))
  (if rebound-mode
      (progn 
        (setq rebound-alist nil)      
        (ad-enable-regexp "rebound")
        (ad-activate-regexp "rebound")
        (add-hook 'temp-buffer-setup-hook 'rebound-before-temp-buffer)
        (add-hook 'window-configuration-change-hook 'rebound-forget-dead-windows)
        (add-hook 'delete-frame-functions 'rebound-forget-frame-windows)
        (add-hook 'kill-buffer-hook 'rebound-forget-dead-buffer)
        (message "Per-window point values are on")
        (run-hooks 'rebound-mode-on-hook))
    (ad-disable-regexp "rebound")
    (ad-activate 'switch-to-buffer)
    (ad-activate 'set-window-buffer)
    (ad-activate 'display-buffer)
    (ad-activate 'replace-buffer-in-windows)
    (ad-activate 'kill-buffer)
    (ad-activate 'bury-buffer)
    (remove-hook 'temp-buffer-setup-hook 'rebound-before-temp-buffer)
    (remove-hook 'window-configuration-change-hook 'rebound-forget-dead-windows)
    (remove-hook 'delete-frame-functions 'rebound-forget-frame-windows)
    (remove-hook 'kill-buffer-hook 'rebound-forget-dead-buffer)
    (message "Per-window point values are off")
    (run-hooks 'rebound-mode-off-hook))
  (run-hooks 'rebound-mode-hook))

;;; Advising Primitives

;; The following four functions are the main primitives that change which
;; buffers are displayed in windows.  We advise them so that they record
;; window-point and window-start for the relevant window(s) before a change
;; in display, then call the repositioning function after the change in
;; display.  (Actually, display-buffer is no longer a primitive as of v23; we
;; advise it for v22.)
(defadvice switch-to-buffer (around rebound)
  (rebound-register-win (selected-window))
  ad-do-it
  (rebound-reposition (selected-window)))

(defadvice set-window-buffer (around rebound)
  (rebound-register-win (ad-get-arg 0))
  ad-do-it
  (rebound-reposition (ad-get-arg 0)))

(defadvice display-buffer (around rebound)
  (mapc #'rebound-register-win (window-list))
  ad-do-it
  (rebound-reposition ad-return-value))

(defadvice replace-buffer-in-windows (around rebound)
  (let* ((buf (or (ad-get-arg 0) (current-buffer)))
         (winlist (get-buffer-window-list buf 'no-minibuf)))
    (mapc #'rebound-register-win winlist)
    ad-do-it
    (mapc #'rebound-reposition winlist)))

;; The following two primitives call buffer display functions from C code,
;; bypassing our advice.  So we also advise them.

(defadvice bury-buffer (around rebound)
  ;; Bury buffer only changes which buffer is displayed if called with nil
  ;; argument and if the current buffer is displayed in the selected
  ;; window.  That's the case we care about.
  (if (or (ad-get-arg 0)
          (not (eq (current-buffer) (window-buffer (selected-window)))))
      ad-do-it
    (rebound-register-win (selected-window))
    ad-do-it
    (rebound-reposition (selected-window))))

(defadvice kill-buffer (around rebound)
  (let* ((buf (or (ad-get-arg 0) (current-buffer)))
         ;; record windows we'll need to reposition.  
         (winlist (get-buffer-window-list buf 'no-minbuf t)))
    ad-do-it
    ;; kill-buffer returns non-nil only when the buffer was successfully
    ;; killed, which is the only case in which we need to act.
    (when ad-return-value 
      (mapc #'rebound-reposition winlist))))

;; The special form with-output-to-temp-buffer also calls one of our advised
;; primitives from within C code.  But here we have a hook we can use, so we
;; don't need advice.  (We don't call for repositioning after display, on the
;; assumption that this form is only used when generating new content for the
;; temp buffer, in which case repositioning would be pointless.)
(defun rebound-before-temp-buffer () 
  (mapc #'rebound-register-win (window-list nil 'no-minibuf)))

;;; List Maintenance

;; Called when a buffer is about to be killed; removes info about that buffer
;; from rebound-alist.
(defun rebound-forget-dead-buffer ()
  (setq rebound-alist (mapcar #'(lambda (x) (delq (assq (current-buffer) x) x))
                              rebound-alist)))

;; Called when the window configuration changes; removes info about any newly
;; deleted windows from rebound-alist.
(defun rebound-forget-dead-windows ()
  (setq rebound-alist (delq nil 
                            (mapcar #'(lambda (x) (and (window-live-p (car x)) x))
                                    rebound-alist))))

;; Called when a frame is deleted (that event doesn't run the window
;; configuration change hook); removes any windows that become dead as a
;; result of frame deletion.
(defun rebound-forget-frame-windows (frame)
  (dolist (win (window-list frame 'no-minibuf))
    (setq rebound-alist (delq (assq win rebound-alist) rebound-alist))))

;;; Recording Data

;; Called with argument WIN, records window-point and window-start for the
;; buffer currently displayed in WIN.
(defun rebound-register-win (win)
  ;; Never bother with minibuffer windows.
  (when (and (not (window-minibuffer-p win))
             (window-live-p win))
    ;; Get window data from the main alist, then buffer data from WIN's own
    ;; alist.  They might be nil, but that doesn't change the routine.
    (let* ((win-data (assq win rebound-alist))
           (buf (window-buffer win))
           (buf-data (assq buf win-data)))
      (setq rebound-alist (delq win-data rebound-alist)
            win-data (delq buf-data win-data))
      ;; If we had data on BUF for WIN, trash the markers.
      (when buf-data
        (set-marker (nth 1 buf-data) nil)
        (set-marker (nth 2 buf-data) nil))
      ;; Add the new data for window-point and window-start to WIN's info for
      ;; BUF, then add WIN's data to the main alist.
      (setq buf-data (list buf
                           (set-marker (make-marker) (window-point win) buf)
                           (set-marker (make-marker) (window-start win) buf))
            win-data (cons win (cons buf-data (cdr win-data)))
            rebound-alist (cons win-data rebound-alist)))))

;;; Repositioning Re-displayed Buffers

(defun rebound-reposition (win)
  (when (window-live-p win)
    (let ((buf (window-buffer win)))
      ;; First, check to see whether BUF is one we should ignore.
      (unless (rebound-exception-p win buf)
        ;; If not, check to see if there's point and window-start info for
        ;; displaying BUF in WIN. Reposition if so.
        (let ((data (assq buf (assq win rebound-alist))))
          (when data
            (set-window-point win (nth 1 data))
            (set-window-start win (nth 2 data))))))))

;; Function called to determine whether rebound should reposition a
;; buffer.  If it returns t, rebound does not reposition.
(defun rebound-exception-p (win buf)
  (or (rebound-defer-to-program-p win buf)
      (rebound-buffer-name-exception-p buf)
      (rebound-run-reposition-tests win buf)))

;; Check to see whether a lisp program has repositioned point in BUF, in
;; which case return t.  (For example, when looking up a function definition
;; via `describe-function', point is moved to the function definition before
;; the library that defines the function is displayed; we then don't want to
;; move point away from the definition after the library display is
;; displayed.)  This test used here will return the wrong answer in two
;; corner cases that I doubt ever arise in the wild.
(defun rebound-defer-to-program-p (win buf)
  (let ((point (save-current-buffer
                 (set-buffer buf)
                 (point)))
        (l1 (mapcar 
             #'(lambda (x) (window-point x))
             (delq win (get-buffer-window-list buf))))
        (l2 (mapcar 
             #'(lambda (x) 
                 (if (not (eq buf (window-buffer (car x))))
                     (rebound-safe-marker-pos (cadr (assq buf x)))))
             rebound-alist)))
    (not (or (memq point l1)
             (memq point l2)))))

(defun rebound-safe-marker-pos (m)
  (if (markerp m) 
      (marker-position m)))

;; Check to see if BUF's name matches against disallowed names or disallowed
;; regexps.
(defun rebound-buffer-name-exception-p (buf)
  (let ((name (buffer-name buf)))
    (or (memq t (mapcar #'(lambda (x) (string= x name)) 
                        rebound-no-reposition-names))
        (memq t (mapcar #'(lambda (x) (string-match x name)) 
                        rebound-no-reposition-regexps)))))

;; Check WIN and BUF against test functions the user has supplied.  
(defun rebound-run-reposition-tests (win buf)
 (memq nil (mapcar #'(lambda (x) (funcall x buf win))
                   rebound-reposition-tests)))

(provide 'rebound)

;;; rebound.el ends here
