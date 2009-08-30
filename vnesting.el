;;; vnesting.el --- Show form nesting with vertical bars
;;
;;;; License
;;     Copyright (C) 2008 Edward Marco Baringer
;;
;;     This code is not (yet) part of SLIME.
;;
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;;     Author: Marco Baringer <mb@bese.it>

;; * TODO
;; ** Deal with tabs
;; ** make it faster
;; ** background color?
;; ** more docs?
;; ** highlight next toplevel? currenty we do the previous toplevel

(eval-when-compile (require 'cl))

(define-minor-mode vnest-mode
  "Display vertical line mode."
  :global nil
  :lighter " VN"
  :group 'vnest
  (make-local-variable 'vnest-overlays)
  (make-local-variable 'vnest-timer)
  (if vnest-mode
    (progn
      (setq vnest-timer
            (run-with-idle-timer vnest-update-delay t 'vnest-this-definition)))
    (cancel-timer vnest-timer)
    (vnest-clear-overlays)))

(defcustom vnest-interesting-symbols
    (regexp-opt-group (list "cond"
                            "do"
                            "flet"
                            "if"
                            "let"
                            "labels"
                            "loop"
                            "macrolet"
                            "multiple-value"
                            "unless"
                            "unwind-protect"
                            "when"
                            "with-"))
  "Regexp matching the symbols we want to show indentation for.

If you simply want all forms to have vertical bars set this to
\"\"."
  :type 'regexp
  :group 'vnest)

(setf vnest-interesting-symbols
    (regexp-opt-group (list "cond"
                            "do"
                            "flet"
                            "if"
                            "let"
                            "labels"
                            "loop"
                            "macrolet"
                            "multiple-value"
                            "unless"
                            "unwind-protect"
                            "when"
                            "with-")))

(defcustom vnest-visible-forms-only-p t
  "When non-NIL we only draw the indentation bars whose opening
parent are visible in the current window."
  :type 'boolean
  :group 'vnest)

(defcustom vnest-interesting-form-minimum-height 2
  "How many lines a form has to span in order for us to draw it a nesting bar."
  :type 'integer
  :group 'vnest)

(defcustom vnest-update-delay 1
  "Number of seconds between vnesting updates.")

(defun vnest-this-definition ()
  (save-excursion
    (beginning-of-defun)
    (vnest-clear-overlays)
    ;; is this a valid form?
    (when (save-excursion
            (ignore-errors
              (forward-sexp)
              ;; 'tis
              t))
      (vnest-this-tree))))

(defun* vnest-this-tree (&optional (depth 0))
  (save-excursion
    (if (save-match-data (looking-at (concat "(\\(" vnest-interesting-symbols "\\)")))
      (if vnest-visible-forms-only-p
        (when (< (window-start) (point))
          (vnest-this-form depth))
        (vnest-this-form depth)))
    (if (ignore-errors (down-list) t)
      (while (ignore-errors (forward-sexp) t)
        (backward-sexp)
        (if (save-match-data (looking-at "("))
          (vnest-this-tree (1+ depth)))
        (forward-sexp)))))

(defun* vnest-this-form (&optional (depth 0))
  (save-excursion
    (let ((stop-point (save-excursion (forward-sexp) (point)))
          (my-column (current-column)))
      (when (< vnest-interesting-form-minimum-height
               (- (save-excursion
                    (forward-sexp)
                    (line-number-at-pos))
                  (line-number-at-pos)))
        (vnest-do-available-lines
         (lambda ()
           (vnest-add-overlay (list 'face (list :stipple 
                                                (list (frame-char-width)
                                                      1
                                                      (string #b1100000 #b0))))))
         stop-point)))))

(defun vnest-goto-column-maybe (column)
  (forward-line 0)
  (loop
    while (and (/= (current-column) column)
               (not (eolp)))
      do (forward-char 1))
  (if (= (current-column) column)
    column
    nil))

(defun* vnest-do-available-lines (thunk &optional (stop (point-max)) (column (current-column)))
  (loop
    while (< (point) stop)
      do (forward-line 1)
    when (and (vnest-goto-column-maybe column)
              (< (point) stop))
      do (funcall thunk)))

(defvar vnest-overlays '())

(defun* vnest-add-overlay (props &optional (begin (point)) (end (1+ (point))))
  (let ((overlay (make-overlay begin end)))
    (push overlay vnest-overlays)
    (while props
      (let ((name (pop props))
            (value (pop props)))
        (overlay-put overlay name value)))
    overlay))

(defun vnest-clear-overlays ()
  (dolist (overlay vnest-overlays)
    (delete-overlay overlay))
  (setq vnest-overlays '()))

(provide 'vnesting)
