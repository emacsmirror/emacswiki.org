;;; undo-browse.el --- Powerful Undo system. Browser/movie/redo/hilit
;; Time-stamp: <2005-04-06 08:23:11 deego>
;; Copyright (C) 2004 D. Goel
;; Copyright (C) 2004 FSF (*)
;; Emacs Lisp Archive entry
;; Filename: undo-browse.el
;; Package: undo-browse
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version: 0.1dev
;; URL: http://gnufans.net/~deego
;; * -- this file contains code from highlight-chg.el which is
;;      Copyright FSF.



;; For latest version:
(defconst ub-home-page
  "http://gnufans.net/~deego/emacspub/lisp-mine/undo-browse")
(defconst undo-browse-home-page ub-home-page)

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This library is inspired from an idea by Shae Matijs Erisson
;; <shae at ScannedInAvian.com> on #emacs, about an undo-browse.

;; Shae still has a java-script/html based movie for viewing changes
;; on his TODO :)

;; Other acknowledgements:
;; highlighting code stolen from hilt-chg.el
;; With Thanks to: 
;; Matthieu Moy


;; See also:
;; redo.el, hilit-chg.el (part of GNU Emacs)

;; Tested only with GNU Emacs 21.2 and later.

;; Quick start:
(defconst ub-quick-start
  "Drop this library in your load-path.

Now, make tons of changes to any document, don't forget to kill some
lines for fun, back it up, then type M-x ub-mode on Emacs 21.2 or
later.  Also type M-x ub-hilit-on to enjoy color-coding too. In this
mode, type H for history, or just type h for help, read help and
enjoy.

See also M-x ub-introduction and M-x ub-commentary.

If you want to install this mode as an alternative/supplement to the
default undo system, add something like (require 'undo-browse)
\(require 'cl) (ub-install-example) to ~/.emacs.  The author uses
(ub-install-deego).

If you end up using this instead of the undo system, you might want to
frob the ub-install-example to use the C-_ key.  If you use C-c C-SPC
too often, you might want to set ub-interactivity to 0 or such once
you are experienced.

"
)

(defun ub-quick-start ()
  "Provides electric help from variable `ub-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ub-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst undo-browse-introduction
  "In ub terminology, the undo-history is seen as a movie, each step
being a frame of the movie. You can play (back/forth) the color-coded
movie-history of your document, or manually go back and forth, and
revert your document to the frame you like.  To start, type M-x
ub-quick-start.

Still in pre-alpha, use at own risk :) Comments/patches/developers
welcome.

A TODO: save the document's undo-history alongside the document. This
endeavor started from shapr's musing of an undo-movie on #emacs.

Tested only with GNU Emacs 21.2 and later, but plan to port it to XEmacs.")

;; Formal description for savannah:
;; undo-browse.el is a powerful undo-history browser for
;; \(X)Emacs. Consider a movie of your document starting from the oldest
;; change to the current version. With undo-browse, you can ask it to go
;; back or forth framewise in this movie. Or you can simply let the movie
;; run either direction.  When you like a particular frame and want to
;; retain it, you can choose to retain it in one of several ways, emacs
;; style, redo style, and other styles.

;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl/UndoBrowse

;; Source code URL: http://gnufans.net/~deego/emacspub/lisp-mine/undo-browse/

;; The idea for undo-browse.el came from discussions on the irc
;; channel, #emacs.  There are a host of other ideas we want to
;; implement collaboratively, including some bugfixing, ensuring
;; Xemacs compatibility, color-coding, etc., so we would appreciate a
;; CVS account.

;;;###autoload
(defun ub-introduction ()
  "Provides electric help from variable `undo-browse-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert undo-browse-introduction) nil) "*doc*"))

;;; Commentary:
(defconst ub-commentary
  "MECHANISM:

The current state of the buffer is the END frame in the
entire movie.

The concept employed is that whichever frame of the movie we are, we
maintain 2 histories:

* The usual buffer-undo-history, which contains all information needed
  to get us to the START frame -- well not exactly. It is not usual in
  the sense that emacs' buffer-undo-history first gets you to future,
  and then to the past.  Our buffer-undo-history contains only the
  past information.

* A ub-buffer-future, which is really a history that contains all
  information needed to get us to the END frame.

With these two, it becomes very easy to go a frame backward or forward
at any given time.

We have grand plans for future.  See introduction for now.

 It works here without problems, and I have done extensive testing.  I
am not however, not sure that it leaves everything the internals
buffer-undo-history for the current buffer in its original state, even
though it may leave it in an equivalent state.  So, use at your own
risk and back up your file before trying this file on that work.  If
you use this author's mkback.el, you can plug it into this, See
(ub-install-example)."
)

(defun ub-commentary ()
  "Provides electric help from variable `ub-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ub-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defconst undo-browse-new-features
  "New since version 0.0:
Keybindings changed.
Movie functions terminology changed.

"

)

(defun ub-new-features ()
  "Provides electric help from variable `undo-browse-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ub-new-features) nil) "*doc*"))

;;; TO DO:

(defconst ub-todo
  "Plans:

* Allow saving undo-history alongside files -- session.el and
  desktop.el don't do that, right?
* browse-undo-list should probably also allow the user to trim the
  buffer-undo-list by clicking on a relevant point.
* Color-coding.

BUGS:

* It loses the buffer-time-stamp status and marks it modified..

* It works more-or-less perfectly now re: not mauing the
  buffer-undo-list, but remains a hack.

* If the user was using hilit-chg.el already, we need to treat the user
 properly after exiting undo-browse

* When the user toggles the arrow of time, we end up generating false
  data-loss warnings.

"

)

(defun ub-todo ()
  "Provides electric help from variable `ub-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert ub-todo) nil) "*doc*"))

(defconst undo-browse-version "0.1dev")
(defconst ub-version undo-browse-version)
(defun ub-version (&optional arg)
   "Display ub's version string.
With prefix ARG, insert version string into current buffer at point."
  (interactive "P")
  (if arg
      (insert (message "ub version %s" ub-version))
    (message "ub version %s" ub-version)))

;;; Requires:
(eval-when-compile (require 'cl))
(require 'hilit-chg)

;;; Macros:
(defvar ub-with-buffer-list-var nil)
(defmacro ub-with-buffer-undo-list (&rest code)
  `(let ((ub-with-buffer-list-var (copy-tree buffer-undo-list)))
     (progn ,@code)
     (setq buffer-undo-list ub-with-buffer-list-var)))

(defmacro ub-with-buffer-enabled (&rest code)
  `(progn
     (when (and ub-mode-buffer-read-only)
       (error "This buffer is expected to be read-only"))
     (let ((buffer-read-only ub-mode-buffer-read-only))
       (progn ,@code))))

(defmacro ub-ignore-errors-numeralize (&rest body)
  ""
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(ding t)
	(ding t)
	(ding t)
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	(setq ub-internal-fe-error-p t)
	0))))

(defmacro ub-ignore-errors (&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(ding t)
	(ding t)
	(ding t)
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))

;;; Code:

(defgroup undo-browse nil
  "The group ub."
  :group 'applications)
(defcustom ub-before-load-hook nil
  "Hook to run before loading ub."
  :group 'undo-browse)
(defcustom ub-after-load-hook nil
  "Hook to run after loading ub."
  :group 'undo-browse)

(run-hooks 'ub-before-load-hook)

(defcustom ub-verbosity 50
  "How verbose to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 to +90 are \"sane\".  The
rest are for debugging."
  :type 'integer
  :group 'undo-browse)
(defcustom ub-interactivity 30
  "How interactive to be.
Once you are experienced with this lib, 0 is the recommended
value.  Values between -90 and +90 are \"sane\".  The rest are for
debugging."
  :type 'integer
  :group 'undo-browse)
(defcustom ub-y-or-n-p-function 'ub-y-or-n-p
  "Function to use for interactivity-dependent  `y-or-n-p'.
Format same as that of `ub-y-or-n-p'."
  :type 'function
  :group 'undo-browse)
(defcustom ub-n-or-y-p-function 'ub-y-or-n-p
  "Function to use for interactivity-dependent `n-or-y-p'.
Format same as that of `ub-n-or-y-p'."
  :type 'function
  :group 'undo-browse)
(defun ub-message (points &rest args)
  "Signal message, depending on POINTS andub-verbosity.
ARGS are passed to `message'."
  (unless (minusp (+ points ub-verbosity))
    (apply #'message args)))
(defun ub-y-or-n-p (add prompt)
  "Query or assube t, based on `ub-interactivity'.
ADD is added to `ub-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add ub-interactivity))
        t
      (funcall 'y-or-n-p prompt)))
(defun ub-n-or-y-p (add prompt)
  "Query or assube t, based on `ub-interactivity'.
ADD is added to `ub-interactivity' to decide whether
to query using PROMPT, or just return t."
  (if (minusp (+ add ub-interactivity))
        nil
      (funcall 'y-or-n-p prompt)))

;;; Real Code:

(defvar ub-browse-buffer nil "internal")
(defvar ub-browse-backwards-p t)

(defvar ub-log-level 0 "")

(defvar ub-buffer-future (list nil)
  "Internal. ")
(make-variable-buffer-local 'ub-buffer-future)

(make-variable-buffer-local 'ub-buffer-undo-list)

(defun ub-frame-toggle-timeline ()
  (interactive)
  (when (ub-y-or-n-p 25 "Really Toggle the ARROW of time!?")
    (let ((ub-foo (copy-tree ub-buffer-future)))
      (setq ub-buffer-future (copy-tree buffer-undo-list))
      (setq buffer-undo-list ub-foo))))

(defvar ub-frame-num 0
  "Internal to ub.

This variable stores the current frame number.  Ideally we would like
things to turn out to be consistent such that the final state of the
document always ends up being = frame 0, and all others have negative
numbers.  However, we don't rely on that as a fact, nor do we know for
sure that that will always happen.")

(make-variable-buffer-local 'ub-frame-num)

(defvar ub-log-buffer "*ub-log*")

(defun ub-frame-backward (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (ub-movie-stop)
  (when ub-mode
    (ub-message -10 "Going backward %S frame(s). " arg)
    (ub-frame-backward-noninteractive arg)))

(defun ub-frame-forward (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (ub-movie-stop)
  (when ub-mode
    (ub-message -10 "Going forward %S frame(s). " arg)
    (ub-frame-forward-noninteractive 1)))

(defun ub-frame-backward-noninteractive (arg)
  (ub-frame-forward-noninteractive (- 0 arg)))

(defun ub-frame-forward-noninteractive (arg)
  (ub-log-necc "Before operation")
  (unless ub-mode (error "Please do a M-x ub-mode-on first. "))
  (let (ans)
    (cond
     ((> arg 0)
      (dotimes (f arg)
	(incf ub-frame-num
	      (setq ans (ub-ignore-errors-numeralize
			 (ub--frame-forward-once 1))))))
     ((< arg 0)
      (dotimes (f (- 0 arg))
	;; yes, incf, not decf
	(incf ub-frame-num
	      (setq ans (ub-ignore-errors-numeralize
			 (ub--frame-backward-once 1)))))))
    (ub-log-necc "After operation")
    ans
    ))

(defun ub-data-check ()
  "Internal, should be called from final frame as a check. "
  (unless (equal ub-frame-num 0)
    (ub-warning
"FINAL FRAME NOT 0! POSSIBLE DATA LOSS!! pls report bug"
		)))

(defvar ub-internal-fe-error-p t)

(defun ub-frame-end ()
  "

Here, we force more than we force at ub-frame-beginning..  we ply
on even if there are errors... it is important to get to the
frame-end..  At the very end, we even do a ub-data-check.

However, plying on can lead to infinite loops in case of errors, so in
such a case, we need to do something about it.  Hence the error-p check..
"
  (interactive)
  (ub-message 0 "Going to the last frame.")
  (let ((ctr 0) (ans -1)
	(ub-internal-fe-error-p nil))
    (while
	(or (not (ub-frame-end-p))
	    (and (not ub-internal-fe-error-p) (not (zerop ans))))
      (setq ans (ub-frame-forward-noninteractive 1))
      (incf ctr ans))
    (ub-data-check)
    (setq ub-internal-fe-error-p nil)
    ctr))

(defun ub-warning (msg)
  (ding t)
  (ding t)
  (ding t)
  (message msg)
  (sit-for 0.1)
  (ding t)
  (sit-for 0.1)
  (ding t)
  (sit-for 0.1)
  (ding t)
  (sleep-for 1)
  (sit-for 1))

(defun ub-frame-goto (n)
  (interactive "nFrame to go to: ")
  (let* ((n (round n))
	 (curr ub-frame-num)
	 (forwardp nil)
	 ans
	 (ctr 0)
	 (fn
	  (cond
	   ((> n curr) (setq forwardp t) 'ub-frame-forward-noninteractive)
	   ((< n curr) 'ub-frame-backward-noninteractive)
	   (t nil))))
    (while
	(and fn
	     (if forwardp
		 (< ub-frame-num n)
	       (> ub-frame-num n)))
      (setq ans (funcall fn 1))
      (incf ctr ans)
      (when (zerop ans) (setq fn nil)))
    ctr))

(defun ub-frame-beginning ()
  (interactive)
  (ub-message 0 "Going to the first frame.")
  ;;(while
  ;;(not (ub-frame-beginning-p))
  ;;(ub-frame-backward-noninteractive 1)))
  (let (;;(ub-internal-fb-error nil)
	(ctr 0)
	(ans -1))
    (while (not (zerop ans))
      (setq ans (ub-frame-backward-noninteractive 1)))))

(defun ub-frame-end-p ()
  (not
   (and ub-buffer-future
	(or
	 (> (length ub-buffer-future) 1)
	 (not (equal (car ub-buffer-future) nil))))))

(defun ub-frame-beginning-p ()
  (not
   (and buffer-undo-list
	(or
	 (> (length buffer-undo-list) 1)
	 (not (equal (car buffer-undo-list) nil))))))

(defun ub--frame-forward-once (arg)
  "Arg should always be 0, else catching errors will lead to data
loss. "
  ;;(interactive)
  (let ((ans 0))
    (when ub-mode-buffer-read-only
      (error "This buffer is read-only"))
    (unwind-protect nil

      (let (tmpf (inhibit-quit t))
      (setq tmpf ub-buffer-future)
      (setq ub-buffer-future buffer-undo-list)
      (setq buffer-undo-list tmpf)
      (setq ans (- 0
		   (ub-ignore-errors-numeralize
		    (ub--frame-backward-once arg))))

      (setq tmpf ub-buffer-future)
      (setq ub-buffer-future buffer-undo-list)
      (setq buffer-undo-list tmpf)
      ans))
    ans))

(defun ub-frame-retain-emacs ()
  (interactive)
  (let ((ub-mode--retain-type 'retain-emacs))
    (ub-mode-quit)))

(defun ub-frame-retain-redo ()
  (interactive)
  (when
      (ub-y-or-n-p -20 "Really forget all future?")
    (let ((ub-mode--retain-type 'retain-redo))
      (ub-mode-quit))))

(defun ub-frame-retain-future ()
  (interactive)
  (ub-frame-toggle-timeline)
  (ub-frame-retain-emacs))

(defun ub--changes-remaining-p ()
  "Internal hack.  To test if there are changes remaining.
So that, If there are, we shall rotate faces before performing the
changes.."
  (let ((ptr buffer-undo-list)
	(ans nil)
	)
    (while (and (not ans) ptr)
      (setq ptr (cdr ptr)
	    ans (first ptr)))
    (if ans t nil)))

(defun ub--frame-backward-once (arg)
  "This is really the main logic function. Returns the answer ANS, which is an
estimate of the number of frames we actually went backwards, which
should just be 0 or 1."

  (let ((ans 0))

    (ub-with-buffer-enabled
     (unwind-protect
	 nil
       (let*
	   (
	    ;;(buffer-read-only ub-mode-buffer-read-only)
	    (inhibit-quit t)
	    ;; original buffer undo list
	    ;;(bul-orig (copy-tree buffer-undo-list))
	    bul-orig
	    bul-intermediate

	    ;; pending undo list
	    pul
	    bul-final
	    ;;(pul (primitive-undo arg buffer-undo-list))
	    ;; now that we undid, save the new state
	    ;;       (bul-final buffer-undo-list)
	    thisfuture
	    ;;(ans 0)
	    )

	 (when ub-internal-highlight-modee
	   (when (ub--changes-remaining-p)
	     (ub-highlight-changes-rotate-faces)))

	 (when (> ub-log-level 50)
	   (unless (equal bul-orig buffer-undo-list)
	     (error "buffer undo list has changed!")))

	 ;; take care of any inconsistencies.. permit no nils at the beginning.
	 (progn
	   (while (and buffer-undo-list (equal (car buffer-undo-list)
					       nil))
	     (pop buffer-undo-list))
	   (setq bul-orig (copy-tree buffer-undo-list)))

	 ;; back to normal.
	 (setq pul (let ((undo-in-progress t))
		     (while
			 (and buffer-undo-list (equal (car buffer-undo-list) nil))
		       (pop buffer-undo-list))
		     (setq bul-intermediate buffer-undo-list)
		     (primitive-undo arg buffer-undo-list)))
	 (when (> (length bul-intermediate) (length pul))
	   (setq ans -1))
	 (setq bul-final buffer-undo-list)
	 (setq ub-debug-bul-final bul-final)
	 ;;(ub-log "After one prim. undo.")

	 ;; now get the changes so made, and add them to the future histoty.
	 ;;(ub-log "Before anything")

	 (setq thisfuture (ub-list-difference bul-final bul-orig))
	 ;; shorten the buffer-undo-list
	 (setq buffer-undo-list pul)
	 ;; extend the future list

	 ;; delete any current boundaries, to eliminate duplicates:
	 (while (and ub-buffer-future
		     (equal (car ub-buffer-future) nil))
	   (pop ub-buffer-future))

	 ;; first a boundary
	 (when (> (length ub-buffer-future) 0)
	   (unless
	       (or
		(equal (car ub-buffer-future) nil)
		(equal (first (last thisfuture)) nil))
	     (push nil ub-buffer-future)))
	 (setq ub-buffer-future (append
				 thisfuture
				 ub-buffer-future))
	 ;; now another boundary
	 ;; this should really do nothing.
	 (unless
	     (equal (car ub-buffer-future) nil)
	   (push nil ub-buffer-future))
	 ;;(ub-log "After one -backward.")
	 ans
	 )
       ;;(set-buffer-modified-p t)
       )
     ans)))

(defun ub-list-difference (from to)
  "assumes that FROM ends in TO"
  (copy-tree (subseq from 0 (- (length from) (length to)))))

(defvar ub-debug-bul-final nil)
(defun ub-log-necc (msgstr)
  "This is NECESSARY LOGGING.
We encounter spurious bugs if we turn this off. Apparently, the act of
switching buffers ensures a proper undo-boundary.
So, we currently do it irrespective of ub-log-level, we just hide it
from the user.

"
  (let ((buf (current-buffer))
	(bul buffer-undo-list)
	(pul pending-undo-list)
	(ubf ub-buffer-future)
	;;prvwin
	(winp (get-buffer-window ub-log-buffer))
	)
    ;;(setq prvwin (selected-window))
    (set-buffer (get-buffer-create ub-log-buffer))
    (goto-char (point-max))
    (insert "\n")
    (insert msgstr)
    (insert "\n")
    (insert (format "Buffer-undo-list: %S \n"
		    bul))

    (insert (format "Temp Buffer-undo-list after primitive undo: %S \n"
		    ub-debug-bul-final))

					;(insert (format "Pending -undo-list: %S \n"
					;pul))
    (insert (format "ub-buffer-future: %S \n"
		    ubf))

    (goto-char (point-max))
    ;; scroll when winp
    (when winp
      (switch-to-buffer-other-window ub-log-buffer)
      (goto-char (point-max))
      (when winp (switch-to-buffer-other-window buf)))
    (set-buffer buf)))

(defun ub-debug-log-clear ()
  (interactive)
  (set-buffer ub-log-buffer)
  (delete-region (point-min) (point-max)))

(defun ub-debug-reset ()
  (interactive)
  (switch-to-buffer "foo")
  (setq ub-buffer-future (list nil))
  (ignore-errors (kill-buffer "foo"))
  (let ((buf (current-buffer)))
    (switch-to-buffer (get-buffer-create "*ub-debug*"))
    (when (set-buffer "*ub-debug*")
      (delete-region (point-min) (point-max)))
    (delete-other-windows)
    (split-window-vertically)
    (find-file "~/tmp/foo")))

(defun ub-debug-undo ()
  (interactive)
  (error "to be completed")
  (setq ub-buffer-future (list nil))
  (ignore-errors (kill-buffer "foo"))
  (let ((buf (current-buffer)))
    (switch-to-buffer (get-buffer-create "*ub-debug*"))
    (when (set-buffer "*ub-debug*")
      (delete-region (point-min) (point-max)))
    (delete-other-windows)
    (split-window-vertically)
    (find-file "~/tmp/foo")))

;;; Mode

(defvar ub-mode-buffer-read-only nil
  "internal, stores the original state of the buffer. ")

(make-variable-buffer-local 'ub-mode-buffer-read-only)

(defcustom ub-mode-string
  '(:eval (format
	   (if ub-movie-active-p
	       " MOVIE:%S"
	       " FRAME:%S" )
	   ub-frame-num))
  "This is really any general lighter spec.
Most simply, a string. "
  :group 'vel)

(defvar ub-mode-map-default
  '(keymap))

(defcustom ub-mode-map ub-mode-map-default
  "Change this to what you like in your .emacs"
  :group 'undo-browse)

(defcustom ub-mode-lower-keys-p nil
  "Set to nil if you would rather have the lower keys beep at you, to
remind you that you are now in ub-mode. ")

(define-key ub-mode-map-default (kbd "C-c C-b") 'ub-frame-backward)

(define-key ub-mode-map-default (kbd "C-c C-b") 'ub-frame-forward)
(define-key ub-mode-map-default (kbd "C-c C-e") 'ub-frame-end)
(define-key ub-mode-map-default (kbd "C-c C-g") 'ub-frame-goto)
(define-key ub-mode-map-default (kbd "C-c C-f") 'ub-frame-forward)
(define-key ub-mode-map-default (kbd "C-c C-a") 'ub-frame-beginning)
(define-key ub-mode-map-default (kbd "C-c C-e") 'ub-frame-end)

(define-key ub-mode-map-default (kbd "C-c C-n") 'ub-movie-forward)
(define-key ub-mode-map-default (kbd "C-c C-m") 'ub-movie)
(define-key ub-mode-map-default (kbd "C-c C-p") 'ub-movie-backward)
(define-key ub-mode-map-default (kbd "C-c C-n") 'ub-movie-forward)

(define-key ub-mode-map-default (kbd "C-c C-c") 'ub-frame-retain-emacs)
(define-key ub-mode-map-default (kbd "C-c C-r") 'ub-frame-retain-redo)
(define-key ub-mode-map-default (kbd "C-c C-SPC") 'ub-frame-retain-redo)
;;(define-key ub-mode-map-default (kbd "C-c C-T") 'ub-frame-retain-future)
(define-key ub-mode-map-default (kbd "\C-c\C-t")
  'ub-frame-toggle-timeline)

(define-key ub-mode-map-default (kbd "C-c C-h") 'ub-help)
(define-key ub-mode-map-default (kbd "C-c C-?") 'ub-help)

(define-key ub-mode-map-default (kbd "C-c C-q") 'ub-mode-quit)
(define-key ub-mode-map-default (kbd "C-x u") 'ub-mode-backward)

;; Alternatives, esp. Numpad alternatives to some keys:

;; Easily accessible alternatives should not move frames, they MAY
;; stop frames.  The reason is that we would like the user to not
;; accidentally move frames after she has pressed ONE C-_ to invoke
;; us..  Any keys she types now should remind her that she's in
;; ub-mode, with minimal damage.

(define-key ub-mode-map-default (kbd "q") 'ub-mode-quit)

;;
(define-key ub-mode-map-default (kbd "?") 'ub-help)
(define-key ub-mode-map-default (kbd "SPC") 'ub-movie-stop)

(define-key ub-mode-map-default (kbd "<left>") 'ub-frame-backward)
(define-key ub-mode-map-default (kbd "4") 'ub-frame-backward)

(define-key ub-mode-map-default (kbd "6") 'ub-frame-forward)
(define-key ub-mode-map-default (kbd "<right>") 'ub-frame-forward)

;; <end> and pgdn
(define-key ub-mode-map-default (kbd "1") 'ub-frame-end)
(define-key ub-mode-map-default (kbd "<end>") 'ub-frame-end)
(define-key ub-mode-map-default (kbd "<next>") 'ub-frame-end)

;; <home> and prior
(define-key ub-mode-map-default (kbd "7") 'ub-frame-beginning)
(define-key ub-mode-map-default (kbd "<prior>") 'ub-frame-beginning)

(define-key ub-mode-map-default "\C-_" 'ub-frame-backward)
(define-key ub-mode-map-default "\M-_" 'ub-frame-backward)

(define-key ub-mode-map-default (kbd "<up>") 'ub-movie-backward)
(define-key ub-mode-map-default (kbd "8") 'ub-movie-backward)

(define-key ub-mode-map-default (kbd "2") 'ub-movie-forward)
(define-key ub-mode-map-default (kbd "<down>") 'ub-movie-forward)

(define-key ub-mode-map-default (kbd "C-x C-c") 'ub-mode-sorry)
(define-key ub-mode-map-default (kbd "C-x C-s") 'ub-mode-sorry)

;; Disable new marks for now, the user can override it should they
;; wish so, but it can lead to bugs.

(define-key ub-mode-map-default (kbd "C-x C-x") 'ub-mode-sorry-bugs)
(define-key ub-mode-map-default (kbd "C-SPC") 'ub-mode-sorry-bugs)
(define-key ub-mode-map-default (kbd "M-<") 'ub-mode-sorry-bugs)
(define-key ub-mode-map-default (kbd "M->") 'ub-mode-sorry-bugs)

(when ub-mode-lower-keys-p
  ;; the author doesn't like using these keys any more.
  (define-key ub-mode-map-default (kbd "f") 'ub-frame-forward)
  (define-key ub-mode-map-default (kbd "n") 'ub-movie-forward)

  (define-key ub-mode-map-default (kbd "p") 'ub-movie-backward)
  (define-key ub-mode-map-default (kbd "m") 'ub-movie)

  (define-key ub-mode-map-default (kbd "g") 'ub-frame-goto)
  (define-key ub-mode-map-default (kbd "e") 'ub-frame-end)
  (define-key ub-mode-map-default (kbd "r") 'ub-frame-retain-redo)
)

(defun ub-mode-sorry (&optional msg)
  (interactive)
  (unless (stringp msg) (setq msg "Please q first to exit ub-mode."))
  (error msg))

(defun ub-mode-sorry-bugs ()
  (interactive)
  (ub-mode-sorry
   "This command discouraged in ub-mode for sanity, else possible bugs"))

(eval
 `(easy-mmode-define-minor-mode
   ub-mode
   "The undo movie mode"
   nil
   ,ub-mode-string
   ub-mode-map))

;;;###autoload
(defun ub-mode-on ()
  (interactive)
  (cond
   ((and ub-mode-prevent-p (funcall ub-mode-prevent-p)) (funcall ub-mode-undo-alternative))
   (t (ub-mode 1)))
   ;;(ub-mode-initialize)
   ;;(run-hooks 'ub-mode-on-hook)
  )

(defcustom ub-mode-prevent-p 'ub-mode-prevent-default "")
(defcustom ub-mode-undo-alternative 'ub-mode-undo-alternative-default "")

(defun ub-mode-prevent-default ()
  (member major-mode '(erc-mode)))

(defun ub-mode-undo-alternative-default ()
  (undo 1))

;; easy-mmode forgets to defcustom the on and off hooks:

(defvar ub-mode-on-hook  nil
  "Internal to ub-mode, Use ub-mode-on-user-hook instead.
DON'T ADD YOUR OWN HOOKS HERE. ")

(add-hook 'ub-mode-on-hook 'ub-mode-initialize)
(add-hook 'ub-mode-off-hook 'ub-mode-deinitialize)
(defcustom ub-mode-on-user-hook nil ""
  :group 'undo-browse
  )
(defcustom ub-mode-off-user-hook nil ""
  :group 'undo-browse

  )

(defvar ub-mode-off-hook 'ub-mode-deinitialize
  "Internal to ub-mode. Use ub-mode-off-user-hook instead.
DON'T ADD YOUR OWN HOOKS HERE. ")

(defun ub-mode-initialize ()
  (progn
    (setq ub-mode-buffer-read-only buffer-read-only)
    (setq buffer-read-only t)
    ;;(setq ub-buffer-undo-list buffer-undo-list)
    (run-hooks 'ub-mode-on-user-hook)))

(defvar ub-mode--retain-type 'end
  "Internal.")

(defvar ub-mode-off-internal-hook nil)

(defcustom ub-mode-off-user-final-hook nil
  "Just use ub-mode-off-user-hook, unless you need to do some fancy
tweaking. ")

(defvar ub-mode-deinitialized-p nil)

(defun ub-mode-deinitialize ()
  (unless ub-mode-deinitialized-p
    (let (ub-mode-off-internal-hook ans (ub-mode-deinitialized-p t))
      (ub-movie-stop)
      (ub-hilit-off)
      (setq
       ans
       (case ub-mode--retain-type
	 ('end
	  (ub-frame-end))
	 ('retain-emacs
	  (progn
	    (let* ((ub-num-to-come-back (ub-frame-end)))
	      (setq ub-mode-off-internal-hook
		    (list `(lambda ()
			     (progn (undo ,ub-num-to-come-back)
				    (setq this-command 'ub-mode-on))))))

	    0))
	 ('retain-redo 0)
	 ;;('retain-future (error "To be implemented. Try C-c C-SPC for now. "))
	 (t (ub-frame-end))))
      (when ub-mode (ub-mode -1))
      (setq buffer-read-only ub-mode-buffer-read-only)

      ;; shouldn't this already be 0 now?
      ;; hm, not so in either of 2 cases:
      ;; [1] one does a retain
      ;; [2] a bug.
      ;; but in either case, we want to reset it to 0.
      (setq ub-frame-num 0)

      (run-hooks 'ub-mode-off-user-hook)
      (run-hooks 'ub-mode-off-internal-hook)
      (run-hooks 'ub-mode-off-user-final-hook)
      ans)
    ;; just to make sure..
    (setq ub-mode-off-internal-hook nil)

    ))

(defun ub-mode-quit ()
  "Use this to quit ub-mode.  DO NOT USE M-X ub-mode-off"
  (interactive)
  (ub-mode-deinitialize))

;;;====================================================

(defcustom ub-movie-interval-initial 0.1 "" :group 'undo-browse)
(defcustom ub-movie-interval 0.4 "Number of seconds per frame"
  :group 'undo-browse
  )

(defvar ub-movie-timer-spec nil "List of bufname and timer")

(defun ub-movie-stop ()
  (interactive)
  (when (timerp (second ub-movie-timer-spec))
    (cancel-timer (second ub-movie-timer-spec))
    (ub-message 0 "Undo-movie stopped. "))
  (setq ub-movie-active-p nil))

(defvar ub-movie-last nil "internal")

(defvar ub-movie-active-p nil "")

;;;###autoload
(defun ub-movie-backward (&optional direction)
  "Run a movie.  By default, backwards, with argument, forward. "
  (interactive "P")
  (unless ub-mode
    (ub-mode 1))
  ;; stop any previous movies.
  (let ((ub-verbosity -200)) (ub-movie-stop))
  (setq ub-movie-active-p t)
  (setq ub-movie-last ub-frame-num)
  (setq ub-movie-timer-spec
	(list (buffer-name)
	      (run-with-timer
	       ub-movie-interval-initial
	       ub-movie-interval
	       'ub-movie-once direction))))

;;;###autoload
(defun ub-movie-history ()
  (interactive)
  (unless ub-mode (ub-mode 1))
  (ub-frame-beginning)
  (ub-movie-forward))

(defalias 'ub-movie 'ub-movie-history)

(defun ub-movie-forward ()
  (interactive)
  (ub-movie-backward t))

(defun ub-movie-once (dir)
  (let
      (
       ;;(lst ub-movie-last)
       nxt passp ans)
    (setq passp (equal (buffer-name) (first ub-movie-timer-spec)))
    (when (and passp ub-mode)
      (setq ans
	    (if dir
		(ub-frame-forward-noninteractive 1)
	      (ub-frame-backward-noninteractive 1)))
      (setq ub-movie-last ub-frame-num)
      (unless (and (numberp ans) (not (= ans 0)))
	(setq passp nil)))
    (unless passp
      (ub-movie-stop))))

(defvar ub-frame-help-doc
  "Undo-browser help: Please C-h k, this may be out of date
+-------------------------------------+--------------------------------+
|Movie history                        |                                |
+-------------------------------------+--------------------------------+
|Movie backward || forward || stop    |                                |
+-------------------------------------+--------------------------------+
|Go back one frame                    |                                |
+-------------------------------------+--------------------------------+
|Quit, restoring where we started     |                                |
+-------------------------------------+--------------------------------+
|Quit at current position             |                                |
+-------------------------------------+--------------------------------+
|Go forward one frame                 |                                |
+-------------------------------------+--------------------------------+
|Go forward N frames                  |                                |
+-------------------------------------+--------------------------------+
|Go to frame N                        |                                |
+-------------------------------------+--------------------------------+
|Go to the last frame                 |                                |
+-------------------------------------+--------------------------------+
|Go to the first frame                |                                |
+-------------------------------------+--------------------------------+
|retain current state, forget future  |                                |
|(like redo mode's undo)              |                                |
+-------------------------------------+--------------------------------+
|Retain current state, but preserve   |                                |
|future as well as past, as if bunches|                                |
|of GNU EMacs' C-_                    |                                |
+-------------------------------------+--------------------------------+
|reTain current state, keep future as |                                |
|if it was past, but forget the past  |                                |
+-------------------------------------+--------------------------------+
|This Help                            |                                |
+-------------------------------------+--------------------------------+
|Stop movie                           |                                |
+-------------------------------------+--------------------------------+

Now press q to exit this help.
")

(defun ub-frame-help ()
  (interactive)
  (with-electric-help
   '(lambda ()
      (insert ub-frame-help-doc) nil) "*ub-frame-help*"))

(defalias 'ub-help 'ub-frame-help)

;;;###autoload
(defun undo-browse ()
  (ub-mode 1))

;;;###autoload
(defalias 'undo-movie 'ub-movie-history)

(defun ub-mkback-buffer ()
  (interactive)
  (when
      (ignore-errors (require 'mkback))
    (ub-ignore-errors (mkback-buffer))))

(defun ub-debug-install-nocolor ()
  (interactive)
  (ub-install-example)
  (setq ub-mode-on-user-hook
	(list 'ub-mkback-buffer
	      ;; turn off for now, for debugging.
	      ;;'ub-hilit-on
	      'ub-frame-backward)))

;;;###autoload
(defun ub-install-example ()
  (interactive)

  ;; should we cause any changes to be lost, make a backup first if
  ;; the user uses mkback.

  ;; turn on syntax highlighting

  ;; Mimic C-_'s behavior.. so go back one frame upon invocation.

  (setq ub-mode-on-user-hook
	(list 'ub-mkback-buffer
	      ;; turn off for now, for debugging.
	      'ub-hilit-on
	      ;; ding to remind you that you are in ub-mode.
	      'ding
	      'ub-frame-backward))

  ;; Set a key similar to C-_.  If too comfortable with this, you may
  ;; even prefer C-_ instead of M-_.

  (global-set-key "\M-_" 'ub-mode-on)
  (global-set-key (kbd "C-x C-/") 'ub-mode-on)

  )

;;;====================================================
;; Still under development.
(defcustom ub-internal-highlight-modee-map '(keymap)
  "Change this to what yoyu like in your .emacs"
  :group 'undo-browse)

(easy-mmode-define-minor-mode
 ub-internal-highlight-modee
 "The undo highlight changes mode"
 nil
 ub-internal-highlight-modee-string
 'ub-internal-highlight-modee-map)

(defun ub-hilit-on ()
  (interactive)
  (unless ub-mode
    (error "This mode functions only when ub-mode is active."))
  (ub-internal-highlight-modee 1)
  (ub-hilit-chg-set 'active))

(defalias 'ub-hilit 'ub-hilit-on)
(defalias 'ub-highlight-on 'ub-hilit-on)

(defun ub-hilit-chg-set (value)
  "Turn on Highlight Changes mode for this buffer."
  (ub-with-buffer-undo-list
   (ub-with-buffer-enabled
    (setq highlight-changes-mode value)
    (remove-hook 'after-change-functions 'ub-hilit-chg-set-face-on-change t)
    (hilit-chg-make-list)
    (if (eq highlight-changes-mode 'active)
	(progn
	  (setq hilit-chg-string highlight-changes-active-string)
	  (or buffer-read-only
	      (hilit-chg-display-changes)))
      ;; mode is passive .. NEVER FOR US.
      (setq hilit-chg-string highlight-changes-passive-string)
      (or buffer-read-only
	  (hilit-chg-hide-changes)))
    (force-mode-line-update)
    ;;(make-local-hook 'after-change-functions)
    (add-hook 'after-change-functions 'ub-hilit-chg-set-face-on-change
	      nil t))))

(defun ub-hilit-chg-set-face-on-change (&rest args)
  (ub-with-buffer-undo-list
   (ub-with-buffer-enabled
    (apply 'ub--hilit-chg-set-face-on-change args))))

(defun ub--hilit-chg-set-face-on-change (beg end leng-before
					 &optional no-property-change)
  "Similar to hilit-chg, but remove the undo-test.

Record changes and optionally display them in a distinctive face.
`hilit-chg-set' adds this function to the `after-change-functions' hook."
  ;;
  ;; This function is called by the `after-change-functions' hook, which
  ;; is how we are notified when text is changed.
  ;; It is also called from `highlight-compare-with-file'.
  ;;
  ;; We do NOT want to simply do this if this is an undo command, because
  ;; otherwise an undone change shows up as changed.  While the properties
  ;; are automatically restored by undo, we must fix up the overlay.
  (save-match-data
    (let ((beg-decr 1) (end-incr 1)
	  (type 'hilit-chg)
	  old)
      (if (and (= beg end) (> leng-before 0))
	  ;; deletion
	  (progn
	    ;; The eolp and bolp tests are a kludge!  But they prevent
	    ;; rather nasty looking displays when deleting text at the end
	    ;; of line, such as normal corrections as one is typing and
	    ;; immediately makes a correction, and when deleting first
	    ;; character of a line.
;;;	      (if (= leng-before 1)
;;;		  (if (eolp)
;;;		      (setq beg-decr 0 end-incr 0)
;;;		    (if (bolp)
;;;			(setq beg-decr 0))))
;;;	      (setq beg (max (- beg beg-decr) (point-min)))
	    (setq end (min (+ end end-incr) (point-max)))
	    (setq type 'hilit-chg-delete))
	;; Not a deletion.
	;; Most of the time the following is not necessary, but
	;; if the current text was marked as a deletion then
	;; the old overlay is still in effect, so if we add some
	;; text then remove the deletion marking, but set it to
	;; changed otherwise its highlighting disappears.
	(if (eq (get-text-property end 'hilit-chg) 'hilit-chg-delete)
	    (progn
	      (remove-text-properties end (+ end 1) '(hilit-chg nil))
	      (put-text-property end (+ end 1) 'hilit-chg 'hilit-chg)
	      (if (eq highlight-changes-mode 'active)
		  (hilit-chg-fixup beg (+ end 1))))))
      (unless no-property-change
	(put-text-property beg end 'hilit-chg type))
      (if (or (eq highlight-changes-mode 'active) no-property-change)
	  (hilit-chg-make-ov type beg end)))))

(defun ub-hilit-off ()
  (interactive)
  (ub-hilit-chg-clear)
  (when ub-internal-highlight-modee
    (ub-internal-highlight-modee -1)))

(defun ub-hilit-chg-clear ()
  "Remove Highlight Changes mode for this buffer.
This removes all saved change information."
  (ub-with-buffer-undo-list
   (ub-with-buffer-enabled
    (if buffer-read-only
	;; We print the buffer name because this function could be called
	;; on many buffers from `global-highlight-changes'.
	(message "Cannot remove highlighting from read-only mode buffer %s"
		 (buffer-name))
      (remove-hook 'after-change-functions 'hilit-chg-set-face-on-change t)
      (let ((after-change-functions nil))
	(hilit-chg-hide-changes)
	(hilit-chg-map-changes
	 '(lambda (prop start stop)
	    (remove-text-properties start stop '(hilit-chg nil))))
	)
      (setq highlight-changes-mode nil)
      (force-mode-line-update)
      ;; If we type:  C-u -1 M-x highlight-changes-mode
      ;; we want to turn it off, but hilit-chg-post-command-hook
      ;; runs and that turns it back on!
      (remove-hook 'post-command-hook
		   'hilit-chg-post-command-hook)))))

(defun ub-highlight-changes-rotate-faces ()
  (interactive)
  (ub-with-buffer-undo-list
   (ub-with-buffer-enabled
    (highlight-changes-rotate-faces))))

(defun ub-install-deego ()
  (interactive)
  ;; I also have this in my .emacs:
  ;;(global-set-key "\C-_" 'ub-mode-on)

  (ub-install-example)
  (global-set-key "\C-_" 'ub-mode-on)
  (global-set-key "\M-_" 'undo)
  ;;(global-set-key (kbd "C-x C-/") 'ub-mode-on)
  )

(defun ub-uninstall-deego ()
  (interactive)
  (global-set-key "\C-_" 'undo))

(provide 'ub)
(run-hooks 'ub-after-load-hook)

;;; ub.el ends here
