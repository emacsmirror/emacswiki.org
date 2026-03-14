;;; thinks.el --- Insert text in a think bubble.
;; Copyright 2000,2001,2002 by Dave Pearson <davep@davep.org>
;; $Revision: 1.8 $

;; thinks is free software distributed under the terms of the GNU General
;; Public Licence, version 2. For details see the file COPYING.

;;; Commentary:

;; thinks.el is a little bit of silliness inspired by the think bubbles you
;; see in cartoons. It allows you to
;;
;; . o O ( insert text that looks like this )
;;
;; into a buffer. This could possibly be handy for use in email and usenet
;; postings.
;;
;; Note that the code can handle multiple lines
;;
;; . o O ( like this. That is, a body of text where the number of characters )
;;       ( exceeds the bounds of what you might consider to be a acceptable  )
;;       ( line length (he says, waffling on to fill a couple of lines).     )
;;
;; You can also control how the bubble looks with `thinks-from'. The above
;; had it set to `top'. You can have `middle':
;;
;;       ( like this. That is, a body of text where the number of characters )
;; . o O ( exceeds the bounds of what you might consider to be a acceptable  )
;;       ( line length (he says, waffling on to fill a couple of lines).     )
;;
;; `bottom':
;;
;;       ( like this. That is, a body of text where the number of characters )
;;       ( exceeds the bounds of what you might consider to be a acceptable  )
;; . o O ( line length (he says, waffling on to fill a couple of lines).     )
;;
;; and `bottom-diagonal':
;;
;;       ( like this. That is, a body of text where the number of characters )
;;       ( exceeds the bounds of what you might consider to be a acceptable  )
;;       ( line length (he says, waffling on to fill a couple of lines).     )
;;     O
;;   o
;; .
;;
;; By default all of the thinking functions will fill (word wrap) the text
;; taking into account the value of `fill-column' minus the space required
;; for the bubble. Prefix a call to any of the functions with C-u to turn
;; off this behaviour.
;;
;; The latest thinks.el is always available from:
;;
;;   <URL:http://www.davep.org/emacs/#thinks.el>

;;; Thanks:

;; Special thanks go to Gareth Owen for being barmy enough to be (as far as
;; we know) the first person to post to Usenet using thinks.el. If not the
;; first posting to Usenet at least the first posting to gnu.emacs.soruces.
;;
;; Thanks also go to Gareth for inspiring the extra-silliness option.
;;
;; Thanks to Jason Rumney for suggesting the diagonal option.
;;
;; Thanks to Martin Blais for `thinks-maybe-region'.

;; src: https://web.archive.org/web/20060514072952/http://www.davep.org/emacs/thinks.el [2026-03-14]
;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl))

;; Attempt to handle older/other emacs.

(eval-and-compile
  
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring)))
  
  ;; If `line-beginning-position' isn't available provide one.
  (unless (fboundp 'line-beginning-position)
    (defun line-beginning-position (&optional n)
      "Return the `point' of the beginning of the current line."
      (save-excursion
        (beginning-of-line n)
        (point))))

  ;; If `line-end-position' isn't available provide one.
  (unless (fboundp 'line-end-position)
    (defun line-end-position (&optional n)
      "Return the `point' of the end of the current line."
      (save-excursion
        (end-of-line n)
        (point)))))

;; Customize options.

(defgroup thinks nil
  "Insert text in a think bubble."
  :group  'editing
  :prefix "thinks-")

(defcustom thinks-bubbles ". o O "
  "*The lead-in think bubbles.

Note that parts of the code assume that the string with always have even
length and that every second character is a space. If you want to modify
this string it is best that you stick to this format."
  :type  'string
  :group 'thinks)

(defcustom thinks-main-bubble-left "( "
  "*The characters to use for the left hand side of the main bubble."
  :type  'string
  :group 'thinks)

(defcustom thinks-main-bubble-right " )"
  "*The characters to use for the right hand side of the main bubble."
  :type  'string
  :group 'thinks)

(defcustom thinks-from 'top
  "*Do we think from the TOP or the BOTTOM?"
  :type  '(choice
           (const :tag "Think from the top of the bubble"               top)
           (const :tag "Think from the middle of the bubble"            middle)
           (const :tag "Think from the bottom of the bubble"            bottom)
           (const :tag "Think diagonally from the bottom of the bubble" bottom-diagonal))
  :group 'thinks)

(defcustom thinks-extra-silliness nil
  "*Do we want some extra silliness?

Note that the extra silliness only kicks in when `thinks-from' is set to
`bottom' or `bottom-diagonal'."
  :type  '(choice
           (const :tag "Yes, let's get really silly" t)
           (const :tag "No, I actually have a serious use for this" nil))
  :group 'thinks)

;; Support code for working in different flavours of emacs.

(defun thinks-xemacs-p ()
  "Are we running in XEmacs?"
  (and (boundp 'running-xemacs) (symbol-value 'running-xemacs)))

(defun thinks-mark-active-p ()
  "Is there a mark active?"
  (if (thinks-xemacs-p)
      (funcall (symbol-function 'region-active-p))
    (symbol-value 'mark-active)))

;; Main code:

(defun thinks-bubble-wrap (text &optional no-filling)
  "Bubble wrap TEXT, don't fill if NO-FILLING is non-nil."
  (with-temp-buffer
    (let* ((extra-silly (and thinks-extra-silliness (or (eq thinks-from 'bottom)
                                                        (eq thinks-from 'bottom-diagonal))))
           (thinks-bubbles (concat (if extra-silly "  " "") thinks-bubbles)))
      (insert text)
      (unless no-filling
        (let ((fill-column (- fill-column (+ (length thinks-bubbles)
                                             (length thinks-main-bubble-left)
                                             (length thinks-main-bubble-right)))))
          (fill-region (point-min) (point-max))))
      (setf (point) (point-min))
      (let ((max-line-width 0))
        (save-excursion
          (while (not (eobp))
            (setq max-line-width (max max-line-width (- (line-end-position) (point))))
            (forward-line)))
        (let ((current-line 1)
              (total-lines  (count-lines (point-min) (point-max)))
              (filler       (make-string (length thinks-bubbles) 32)))
          (while (not (eobp))
            (insert (cond ((and (eq thinks-from 'top)
                                (= current-line 1))
                           thinks-bubbles)
                          ((and (eq thinks-from 'bottom)
                                (= current-line total-lines))
                           thinks-bubbles)
                          ((and (eq thinks-from 'middle)
                                (= current-line (truncate (/ (1+ total-lines) 2))))
                           thinks-bubbles)
                          (t
                           filler))
                            thinks-main-bubble-left)
            (save-excursion
              (end-of-line)
              (insert (make-string (- max-line-width
                                      (- (- (point) (line-beginning-position))
                                         (+ (length thinks-bubbles)
                                            (length thinks-main-bubble-left))))
                                   32))
              (insert thinks-main-bubble-right))
            (incf current-line)
            (forward-line))))
      (when (eq thinks-from 'bottom-diagonal)
        (unless (bolp)
          (insert "\n"))
        (loop for n downfrom (- (length thinks-bubbles) 2) to (if extra-silly 2 0) by 2
              do (insert (make-string n 32)
                         (substring thinks-bubbles n (1+ n))
                         "\n")))
      (when extra-silly
        (setf (point) (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert " o\n/|\\\n |\n/ \\\n"))
      (buffer-string))))

;;;###autoload
(defun thinks (text)
  "Insert TEXT wrapped in a think bubble.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you."
  (interactive "sThinks: ")
  (unless (bolp)
    (insert "\n"))
  (insert (thinks-bubble-wrap text current-prefix-arg))
  (insert "\n"))

;;;###autoload
(defun thinks-region (start end)
  "Bubble wrap region bounding START and END.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you."
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (save-excursion
      (delete-region start end)
      (setf (point) start)
      (insert (flet ((bolp-string (n)
                       (save-excursion
                         (setf (point) n)
                         (if (bolp) "" "\n"))))
                (concat (bolp-string start)
                        (thinks-bubble-wrap text current-prefix-arg)
                        (bolp-string end)))))))

;;;###autoload
(defun thinks-yank ()
  "Do a `yank' and bubble wrap the yanked text.

Prefix a call to this function with \\[universal-argument] if you don't want
the text to be filled for you."
  (interactive)
  (thinks (with-temp-buffer
            (yank)
            (buffer-string))))

;;;###autoload
(defun thinks-maybe-region ()
  "If region is active, bubble wrap region bounding START and END.
If not, query for text to insert in bubble."
  (interactive)
  (if (thinks-mark-active-p)
      (call-interactively #'thinks-region)
    (call-interactively #'thinks)))

(provide 'thinks)

;;; thinks.el ends here.
