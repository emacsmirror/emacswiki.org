;;; vm-thread.el --- Functions for collapsing threads in a VM Summary buffer

;; Eric Schulte 2/20/2008

;; some code taken from u-vm-color.el by Ulf Jasper

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; to use include the following in your init file

;; (load "~/path/to/vm-thread.el")
;; (add-hook 'vm-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "<tab>") 'vm-thread-toggle-thread-on-current-line)
;;             (local-set-key (kbd "C") 'vm-thread-collapse-all-threads)))

;;; Code:

(defcustom vm-thread-count-display-string "(%d) "
  "When threads are folded the indentation is replaced with a
counter of the number of threads hidden.  This string defines how
this number is displayed using the `format' command."
  :group 'vm-thread
  :type 'string)

(defcustom vm-thread-minimum-message-num-to-collapse 1
  "The minimum number of messages in a thread required for the
thread to be folded."
  :group 'vm-thread
  :type 'integer)

;; navigation
(defun vm-thread-open-the-door ()
  (if (vm-thread-current-line-collapsed-p)
      (vm-thread-show-thread-on-current-line)))
  
(defun vm-thread-shut-the-door (direction)
  "returns true if moving one line in DIRECTION results in
leaving the current thread.  Expands the current thread if it is
collapsed, and collapses if it will be left."
  ;; expand thread if collapsed
  (if (vm-thread-current-line-collapsed-p)
      (vm-thread-show-thread-on-current-line))
  ;; if leaving collapse
  (let* ((bounds (vm-thread-current-thread-bounds))
	 (destination (save-excursion
			(forward-line direction)
			(point))))
    (unless (and (<= (car bounds) destination)
		 (>= (cdr bounds) destination))
      (vm-thread-collapse-thread-on-current-line))))

(defadvice vm-next-message (around step-in-or-out-down)
  "Allow vm navigation commands to step into and out of threads,
so to expand threads upon entry, and collapse when leaving."
  (if (string-match "Summary" (buffer-name (current-buffer)))
      (progn
	(vm-thread-shut-the-door 1)
	ad-do-it
	(vm-thread-open-the-door))
    ad-do-it))

(defadvice vm-previous-message (around step-in-or-out-up)
  "Allow vm navigation commands to step into and out of threads,
so to expand threads upon entry, and collapse when leaving."
  (if (string-match "Summary" (buffer-name (current-buffer)))
      (progn
	(vm-thread-shut-the-door -1)
	ad-do-it
	(vm-thread-open-the-door))
    ad-do-it))

;; functions
(defun vm-thread-collapse-all-threads ()
  "Collapse every thread in the current line."
  (interactive)
  (goto-char (point-min))
  (if (not (vm-thread-current-line-collapsed-p))
      (vm-thread-collapse-thread-on-current-line))
  (while (vm-thread-forward-thread)
    (if (not (vm-thread-current-line-collapsed-p))
	(vm-thread-collapse-thread-on-current-line))))

(defun vm-thread-toggle-thread-on-current-line ()
  "Toggle the visibility of the thread on the current line."
  (interactive)
  (save-excursion
    (if (vm-thread-current-line-collapsed-p)
	(vm-thread-show-thread-on-current-line)
      (vm-thread-collapse-thread-on-current-line))))

;; TODO (can this handle hidden threads)
(defun vm-thread-forward-thread ()
  "move to the beginning of the next thread"
  (interactive)
  (let ((dest (+ 1 (cdr (vm-thread-current-thread-bounds)))))
    (if (< dest (point-max))
	(goto-char dest))))

;; TODO (can this handle hidden threads)
(defun vm-thread-backward-thread ()
  "move to the beginning of the previous thread"
  (interactive)
  (let ((dest (- (car (vm-thread-current-thread-bounds)) 1)))
    (if (> dest (point-min))
	(progn
	  (goto-char dest)
	  (goto-char (car (vm-thread-current-thread-bounds)))))))

(defun vm-thread-show-thread-on-current-line ()
  "Show the thread on the current line, return nil if the current
thread is already shown."
  (interactive)
  (save-excursion
    (mapcar
     (lambda (ol)
       (if (vm-thread-clean-overlay ol)
	   (progn
	     (goto-char (cdr (vm-thread-current-thread-bounds)))
	     (move-beginning-of-line 1)
	     (vm-thread-restore-current-line-part 'indent))))
     (vm-thread-get-overlays (point)))))

(defun vm-thread-collapse-thread-on-current-line ()
  "Collapse the thread of the message on the current line."
  (interactive)
  (let ((bounds (vm-thread-current-thread-collapsing-bounds)))
    (if bounds
        (let* ((start (car bounds))
              (end (cdr bounds))
              (lines (count-lines start end)))
          (vm-thread-collapse start end)
          (vm-thread-replace-current-line-part
	   'indent
	   (format vm-thread-count-display-string (+ 1 lines)))))))

(defun vm-thread-replace-current-line-part (vm-summary-part my-string)
  "Replace the part of the current message line specified by
vm-summary-part with the given string.  This uses
`vm-summary-format' and `vm-thread-make-summary-keywords'
which is ripped off from `vm-color'."
  (interactive)
  (save-excursion
    (let* ((keywords (vm-thread-make-summary-keywords))
           (regex (car keywords))
           (keywords (cdr keywords))
           (k-num))
      (mapcar (lambda (le)
                (if (equal (cadr (cadr le)) vm-summary-part)
                    (setq k-num (car le))))
              keywords)
      ;; now lets apply to the current message (as display property)
      (if (and k-num (re-search-forward regex nil t))
            (overlay-put
	     (make-overlay (match-beginning k-num) (match-end k-num))
	     'display my-string)))))

(defun vm-thread-restore-current-line-part (vm-summary-part)
  "Undoes the effects of `vm-thread-replace-current-line-part'."
  (interactive)
  (save-excursion
    (let* ((keywords (vm-thread-make-summary-keywords))
           (regex (car keywords))
           (keywords (cdr keywords))
           (k-num))
      (mapcar (lambda (le)
                (if (equal (cadr (cadr le)) vm-summary-part)
                    (setq k-num (car le))))
              keywords)
      ;; now lets apply to the current message (as display property)
      (if (and k-num (re-search-forward regex nil t))
          (let ((overs (vm-thread-get-overlays (match-beginning k-num))))
            (mapcar 'vm-thread-clean-overlay overs)))))
  ;; we return nil for some `and' gymastics later on
  nil)

(defun vm-thread-current-line-collapsed-p ()
  "return t if the thread on the current line is hidden"
  (interactive)
  (save-excursion
  (let ((all (vm-thread-get-overlays (point)))
        shown)
    (mapcar
     (lambda (ol)
       (if (vm-thread-check-overlay ol)
           (setq shown t)))
     all)
    shown)))

(defun vm-thread-collapse (start end)
  "Collapse the part of the buffer between `start' and `end'"
  (interactive)
  (let ((range (make-overlay start end)))
    (overlay-put range 'invisible t)
    (overlay-put range 'isearch-open-invisible 'vm-thread-reveal-overlay)))

(defun vm-thread-current-thread-collapsing-bounds ()
  "Return the bounds of the parts of the current thread which
should be hidden."
  (let* ((bounds (vm-thread-current-thread-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (if (<= vm-thread-minimum-message-num-to-collapse (count-lines start end))
        (progn
          (goto-char start)
          (move-beginning-of-line 1)
          (setq start (point))
          (goto-char end)
          (move-beginning-of-line 1)
          (setq end (point))
          (cons start end)))))

(defun vm-thread-current-thread-bounds ()
  "Return a cons cell of the start and end points of the current
thread in the summary buffer."
  (interactive)
  (let ((place (point))
        (start)
        (end))
    (while (and (not (vm-thread-current-line-collapsed-p))
		(< 0 (vm-thread-current-message-thread-depth))
		(= 0 (forward-line -1))))
    (move-beginning-of-line 1)
    (setq start (point))
    (goto-char place)
    (while (and (= 0 (forward-line 1))
		(not (vm-thread-current-line-collapsed-p))
                (< 0 (vm-thread-current-message-thread-depth)))
      (move-end-of-line 1)
      (setq end (point)))
    (unless end
      (progn
        (forward-line -1)
        (move-end-of-line 1)
        (setq end (point))))
    (goto-char place)
    (cons start end)))

(defun vm-thread-current-message-thread-depth ()
  "Return the current message's thread depth or return '-1' if
there are no threads left in the summary."
  (interactive)
  (let ((place (point))
        (keywords (vm-thread-make-summary-keywords))
        (num nil))
    (move-beginning-of-line 1)
    (if (re-search-forward
         (car keywords) nil t)
        ;; which part of the regex matches the indentation
        (mapcar (lambda (le)
                  (if (and (listp le) (equal (cadr (cadr le)) 'indent))
                      (setq num (/ (length (match-string (car le))) 2))))
                keywords)
      (setq num -1))
    (goto-char place)
    num))

;; needed to show text during isearch
;; (info "(elisp)isearch-open-invisible")
(defun vm-thread-reveal-overlay (overlay)
  (overlay-put overlay 'invisible nil))

(defun vm-thread-get-overlays (begin)
  "Get all the overlays for the thread at the current line."
  (save-excursion
    (let (start
	  end)
      (goto-char begin)
      (move-beginning-of-line 1)
      (if (> (point) (point-min))
	  (setq start (- (point) 1))
	(setq start (point-min)))
      (goto-char begin)
      (move-end-of-line 1)
      (setq end (point))
      (goto-char begin)
      (overlays-in start end))))

(defun vm-thread-clean-overlay (ol)
  "If the overlay OL has it's `display' or `invisible' properties
set the properties are reset to nil.  Return true if any overlays
are changed."
  (let (shown)
    (if (overlay-get ol 'display)
        (progn
          (overlay-put ol 'display nil)
          (setq shown t)))
    (if (overlay-get ol 'invisible)
        (progn
          (overlay-put ol 'invisible nil)
          (setq shown t)))
    shown))

(defun vm-thread-check-overlay (ol)
  "Returns true if the overlay is hidden or it's display is
obscured."
  (if (or
       (overlay-get ol 'display)
       (overlay-get ol 'invisible))
      t
    nil))

;; ripped from u-vm-color
;; (require 'u-vm-color)
(defun vm-thread-make-summary-keywords ()
  "Parse `vm-summary-format' and return a font-lock keyword list.
List consists of one big regexp and lots of face instructions for
subexpressions."
  (let ((search-start 0)
        (length 0)                      ; (maximum) length
        (m-length 0)                    ; minimum length
        (rest "")
        (f-element "")
        (m-element "")
        (value "")
        (u-format "^..")
        (u-match nil)
        (count 1)
        (t-vm-summary-format vm-summary-format)
        (u-vm-color-xemacs-workaround
         (string-match "XEmacs\\|Lucid" emacs-version)))
    ;; pick up all elements in the vm-summary-format
    (while (string-match
            (concat "%-?\\([0-9]+\\.\\)?\\([0-9]+\\)?"
                    "\\([aAcdfFhHiIlLmMnstTwyz*]\\|U.\\)\\([^%\n]*\\)")
            t-vm-summary-format search-start)
      (setq search-start (match-end 0))
      (if (match-beginning 1)
          (setq m-length (string-to-number
                          (substring t-vm-summary-format (match-beginning 1)
                                     (1- (match-end 1)))))
        (setq m-length 0))
      (if (match-beginning 2)
          (setq length (string-to-number
                        (substring t-vm-summary-format (match-beginning 2)
                                   (match-end 2))))
        (setq length 0))
      (if (match-beginning 3)
          (setq value (substring t-vm-summary-format (match-beginning 3)
                                 (match-end 3)))
        (setq value ""))
      (if (match-beginning 4)
          (setq rest (substring t-vm-summary-format (match-beginning 4)
                                (match-end 4)))
        (setq rest ""))
      (setq rest (regexp-quote rest))
      ;;(message "--> %s, %s, %s" length m-length value)
      ;; Should use the length and m-length values for things like %5d
      ;; instead of doing [0-9 ]+ for numerics...
      ;; No!
      (cond ((string-equal value "a") ;; attributes -- make sure that all
             ;; possible letters are given!
             (setq f-element "\\([DNU ][FW ][RZB ][E ]\\)")
             (setq m-element (list count (quote 'u-vm-color-attribute-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "A") ;; attributes -- long
             (setq f-element "\\([DNU ][r ][z ][b ][f ][w ][e ]\\)")
             (setq m-element (list count (quote 'u-vm-color-attribute-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "c") ;; number of characters
             (setq f-element "\\( *?[0-9]+ *?\\)")
             (setq m-element (list count (quote 'u-vm-color-length-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "d") ;; day -- numeric
             (setq f-element "\\( *?[0-9]+ *?\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "f") ;; authors / recipients address
             ;;(setq f-element "\\(To: [^ \n]+\\)?\\([^ \n]+\\)?")
             (setq f-element (concat
                              "\\("
                              (u-vm-color-make-specific-length-regexp
                               ;;"[ [:graph:]]"
                               "." (- m-length 4) (- length 4) "To: ")
                              "\\|"
                              (u-vm-color-make-specific-length-regexp
                               ;;"[ [:graph:]]"
                               "." m-length length)
                              "\\)"))
             (setq count (+ 1 count))
             (setq m-element (list count
                                   (quote 'u-vm-color-recipient-face) t t))
             (setq count (+ 1 count))
             (setq u-match (append u-match (list m-element)))
             (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
            ((or (string-equal value "F")
                 (string-equal value "UA") ;; IS THIS CORRECT!????????
                 (string-equal value "UB")) ;; authors / recipients full names
             ;;(setq f-element "\\(To:.+\\)?\\([^:\n]+\\)?")
             (setq f-element (concat
                              "\\("
                              (u-vm-color-make-specific-length-regexp
                               ;;"[ [:graph:]]"
                               "." (- m-length 4) (- length 4) "To: ")
                              "\\|"
                              (u-vm-color-make-specific-length-regexp
                               ;;"[ [:graph:]]"
                               "." m-length length)
                              "\\)"))
             (setq count (+ 1 count))
             (setq m-element (list count
                                   (quote 'u-vm-color-recipient-face) t t))
             (setq count (+ 1 count))
             (setq u-match (append u-match (list m-element)))
             (setq m-element (list count (quote 'u-vm-color-author-face) t t)))
            ((string-equal value "h") ;; time
             (setq f-element "\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)")
             (setq m-element (list count (quote 'u-vm-color-time-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "H") ;; time -- short
             (setq f-element "\\([0-9][0-9]:[0-9][0-9]\\)")
             (setq m-element (list count (quote 'u-vm-color-time-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "i") ;; id
             (setq f-element "\\(<[^ \n]+>\\)")
             (setq m-element (list count (quote 'u-vm-color-id-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "I") ;; indentation
             (setq f-element "\\( *\\)")
             (setq m-element (list count (quote 'indent) nil nil)))
            ((string-equal value "l") ;; number of lines
             (setq f-element "\\( *?[0-9]+ *?\\)")
             (setq m-element (list count (quote 'u-vm-color-length-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "L") ;; label
             (setq f-element (u-vm-color-make-specific-length-regexp
                              ;;"[ [:graph:]]"
                              "." m-length length))
             (setq m-element (list count (quote 'u-vm-color-label-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "m") ;; month
             (setq f-element "\\([A-Za-z]+\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "M") ;; month -- numeric
             (setq f-element "\\( *?[0-9]+ *?\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "n") ;; message number
             (setq f-element "\\( *?[0-9]+ *?\\)")
             (setq m-element  (list count (quote 'u-vm-color-number-face))))
            ((string-equal value "s") ;; subject
             (setq f-element (u-vm-color-make-specific-length-regexp
                              ;;"[ [:graph:]]"
                              "." m-length length))
             (setq m-element (list count (quote 'u-vm-color-subject-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "t") ;; recipient addresses
             (setq f-element "\\([^ \n]+\\)")
             (setq m-element (list count (quote 'u-vm-color-recipient-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "T") ;; recipient full names
             (setq f-element "\\(.+\\)")
             (setq m-element (list count (quote 'u-vm-color-recipient-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "w") ;; week day (is missing in some mails!)
             (setq f-element "\\([A-Za-z ]+\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "y") ;; year
             (setq f-element "\\([0-9][0-9][0-9][0-9]\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "z") ;; timezone
             (setq f-element "\\(.+\\)")
             (setq m-element (list count (quote 'u-vm-color-date-face)
                                   nil u-vm-color-xemacs-workaround)))
            ((string-equal value "*") ;; mark-marker
             (setq f-element "\\(\\*\\| \\)")
             (setq m-element (list count (quote 'u-vm-color-attribute-face)
                                   nil u-vm-color-xemacs-workaround)))
            (t ;; user defined and everything else
             (setq f-element ".*?")
             (setq m-element nil)))
      (setq u-format (concat u-format f-element rest))
      (if m-element
          (progn
            (setq count (+ 1 count))
            (setq u-match (append u-match (list m-element))))))
    (setq u-format (concat u-format "$"))
    (append (list u-format) u-match)))

(provide 'vm-thread)
;;; vm-thread.el ends here
