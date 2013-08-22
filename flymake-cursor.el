;;; flymake-cursor.el --- displays flymake error msg in minibuffer after delay
;;
;; Author     : ??
;; origin     : http://paste.lisp.org/display/60617,1/raw
;; Maintainer : Dino Chiesa <dpchiesa@hotmail.com>
;;            : Donald Curtis <dcurtis@milkbox.net>
;; Created    : May 2011
;; Modified   : December 2012
;; Version    : 0.1.5
;; Keywords   : languages mode flymake
;; X-URL      : http://www.emacswiki.org/emacs/flymake-cursor.el
;; Last-saved : <2012-Dec-20 09:49:28>
;;
;; -------------------------------------------------------
;;
;; License: None.  This code is in the Public Domain.
;;
;;
;; Additional functionality that makes flymake error messages appear
;; in the minibuffer when point is on a line containing a flymake
;; error. This saves having to mouse over the error, which is a
;; keyboard user's annoyance.
;; -------------------------------------------------------
;;
;; This flymake-cursor module displays the flymake error in the
;; minibuffer, after a short delay.  It is based on code I found roaming
;; around on the net, unsigned and unattributed. I suppose it's public
;; domain, because, while there is a "License" listed in it, there
;; is no license holder, no one to own the license.
;;
;; This version is modified slightly from that code. The post-command fn
;; defined in this code does not display the message directly. Instead
;; it sets a timer, and when the timer fires, the timer event function
;; displays the message.
;;
;; The reason to do this: the error message is displayed only if the
;; user doesn't do anything, for about one second. This way, if the user
;; scrolls through a buffer and there are myriad errors, the minibuffer
;; is not constantly being updated.
;;
;; If the user moves away from the line with the flymake error message
;; before the timer expires, then no error is displayed in the minibuffer.
;;
;; I've also updated the names of the defuns. They all start with flyc now.
;;
;; To use this, include this line in your .emacs:
;;
;;    ;; enhancements for displaying flymake errors
;;    (require 'flymake-cursor)
;;
;; You can, of course, put that in an eval-after-load clause.
;;
;; -------------------------------------------------------
;;
;; Update 2012-03-06 by Donald Curtis
;; --
;; Added some autoload statements and the closing comment to make
;; compatible with package.el parser.
;;
;; Update 2012-12-20 by Jeremy Moore
;; --
;; Alter post-command-hook's local value via add-hook so that it plays
;; nicely with other packages.
;;


(require 'cl)

(defvar flyc--e-at-point nil
  "Error at point, after last command")

(defvar flyc--e-display-timer nil
  "A timer; when it fires, it displays the stored error message.")

(defun flyc/maybe-fixup-message (errore)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
        ((null (flymake-ler-file errore))
         ;; normal message do your thing
         (flymake-ler-text errore))
        (t ;; could not compile error
         (format "compile error, problem on line %s" (flymake-ler-line errore)))))

(defun flyc/show-stored-error-now ()
  "Displays the stored error in the minibuffer."
  (interactive)
  (let ((editing-p (= (minibuffer-depth) 0)))
   (if (and flyc--e-at-point editing-p)
       (progn
         (message "%s" (flyc/maybe-fixup-message flyc--e-at-point))
         (setq flyc--e-display-timer nil)))))


(defun flyc/-get-error-at-point ()
  "Gets the first flymake error on the line at point."
  (let ((line-no (line-number-at-pos))
        flyc-e)
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
          (setq flyc-e (car (second elem)))))
    flyc-e))


;;;###autoload
(defun flyc/show-fly-error-at-point-now ()
  "If the cursor is sitting on a flymake error, display
the error message in the  minibuffer."
  (interactive)
  (if flyc--e-display-timer
      (progn
        (cancel-timer flyc--e-display-timer)
        (setq flyc--e-display-timer nil)))
  (let ((error-at-point (flyc/-get-error-at-point)))
    (if error-at-point
        (progn
          (setq flyc--e-at-point error-at-point)
          (flyc/show-stored-error-now)))))


;;;###autoload
(defun flyc/show-fly-error-at-point-pretty-soon ()
  "If the cursor is sitting on a flymake error, grab the error,
and set a timer for \"pretty soon\". When the timer fires, the error
message will be displayed in the minibuffer.

This allows a post-command-hook to NOT cause the minibuffer to be
updated 10,000 times as a user scrolls through a buffer
quickly. Only when the user pauses on a line for more than a
second, does the flymake error message (if any) get displayed.

"
  (if flyc--e-display-timer
      (cancel-timer flyc--e-display-timer))

  (let ((error-at-point (flyc/-get-error-at-point)))
    (if error-at-point
        (setq flyc--e-at-point error-at-point
              flyc--e-display-timer
              (run-at-time "0.9 sec" nil 'flyc/show-stored-error-now))
      (setq flyc--e-at-point nil
            flyc--e-display-timer nil))))


;;;###autoload
(eval-after-load "flymake"
  '(progn

     (defadvice flymake-goto-next-error (after flyc/display-message-1 activate compile)
       "Display the error in the mini-buffer rather than having to mouse over it"
       (flyc/show-fly-error-at-point-now))

     (defadvice flymake-goto-prev-error (after flyc/display-message-2 activate compile)
       "Display the error in the mini-buffer rather than having to mouse over it"
       (flyc/show-fly-error-at-point-now))

     (defadvice flymake-mode (before flyc/post-command-fn activate compile)
       "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer (rather than having to mouse over
it)"
       (add-hook 'post-command-hook 'flyc/show-fly-error-at-point-pretty-soon t t))))


(provide 'flymake-cursor)

;;; flymake-cursor.el ends here
