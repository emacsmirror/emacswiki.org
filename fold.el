;;; fold.el --- Folding

;;;$Header: /cvs/mtb/elisp/fold.el,v 1.2 2000/02/08 10:03:49 mb Exp $
;;{{{}}}

;; This file is intended to be used with GNU Emacs, Version 20.3 or later

;;{{{  Copyright and License 

;; Designed and programmed 1999 by Marcus Breiing. 
;;  http://uuid.com/~marcus/computing/fold-mode.html

;; Remotely based on Origami-compatible folding.el
;;  (C)Copyright Marcus Breiing 1993
;; which in turn was based on folding.el, version 1.6.2
;;  Copyright (C) 1992, 1993, Jamie Lokier.

;; Minor mode framework from outline.el, part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;{{{  TODO
;; Make overlays window-specific. This will allow differently folded
;; views of a file.

;; fold-hide on an already closed nested fold should close the
;; surrounding fold (like folding.el does)

;; Fix all bugs:)
;;}}}
;;{{{  BUGS

;; It is way too easy to get text and overlays out of sync, because
;; many operations, especially undo, don't operate sensibly on
;; overlays. Maybe Fold mode could use text attributes, but I don't
;; want to go there right now.

;; fold-whole-buffer re-scans fold marks, which is slow. We have so
;; many ways for getting things out of whack that it seems necessary
;; to have a re-scan user command, but it should be a separate
;; command, and it should try to preserve invisibilty state.

;; kill-and-yank of folds loses fold overlays

;; kill-and-undo of folds loses fold overlays

;; There's a bug in undo that can shift overlay boundaries

;; Undo of fold-region leaves overlays in the buffer.  Many kinds of
;; kill or edit can leave us with orpaned or unusable overlays.

;; kill-line (C-k) in the title of a hidden fold will kill the hidden
;; part. This is not really a bug, but a result of the way Emacs
;; treats the hidden text in kill-line, which btw is inconsistent with
;; the behavior of end-of-line.

;;{{{ ---FIXED--- Some recently fixed bugs

;; close-fold doesn't work on last line of fold (usually the
;; terminating mark.) The reason is the move to end-of-line in
;; close-fold, which is there to move point inside folds that start
;; late on the line. 

;;}}}

;;}}}
;;{{{  fold-mode and friends

(defvar fold-mode-active nil)
(make-variable-buffer-local 'fold-mode-active)
(or (assq 'fold-mode-active minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   (list '(fold-mode-active " Fold")))))

(defun fold-mode (&optional arg) ;;{{{  
  "Toggle Fold minor mode.
With arg, turn Fold minor mode on if arg is positive, off otherwise."
  (interactive "P")
  (setq fold-mode-active
	(if (null arg) (not fold-mode-active)
            (> (prefix-numeric-value arg) 0)))
  (if fold-mode-active
      (progn
	(make-local-variable 'line-move-ignore-invisible)
	(setq line-move-ignore-invisible t)
	(add-to-invisibility-spec '(fold . t))
        (fold-use-normal-marks)
        (add-hook 'post-command-hook 
                  (function fold-post-command-handler) nil t)
	(run-hooks 'fold-mode-hook)
        (if fold-close-on-startup
            (fold-whole-buffer)
            (save-excursion
              (goto-char (point-min))
              (fold-scan-forward (function fold-create-fold-overlays) nil))))
      (setq line-move-ignore-invisible nil)
      (remove-from-invisibility-spec '(outline . t))
      (remove-hook 'post-command-hook (function fold-post-command-handler) t)
      (fold-delete-all-overlays))
  (force-mode-line-update))
;;}}}

;;}}}
;;{{{  Keymap
;;{{{    fold-mode-prefix-map
(defvar fold-mode-prefix-map nil)
(if fold-mode-prefix-map
    nil
  (setq fold-mode-prefix-map (make-sparse-keymap))
  (define-key fold-mode-prefix-map "\C-w" 'fold-whole-buffer)
  ;; I like use C-o instead of C-a, and bind C-a to hide-other
  ;; (define-key fold-mode-prefix-map "\C-a" 'fold-show-all)
  (define-key fold-mode-prefix-map "\C-s" 'fold-show)
  ;; use C-h to see fold-mode-map
  ;; (define-key fold-mode-prefix-map "\C-h" 'fold-hide)
  (define-key fold-mode-prefix-map "\C-f" 'fold-region)
  (define-key fold-mode-prefix-map "\C-d" 'fold-hide)
  (define-key fold-mode-prefix-map "\C-q" 'fold-toggle)
  (define-key fold-mode-prefix-map "\C-o" 'fold-show-all)
  (define-key fold-mode-prefix-map "\C-a" 'fold-hide-other)
  (define-key fold-mode-prefix-map "\C-n" 'fold-next)
  (define-key fold-mode-prefix-map "\C-p" 'fold-previous))
;;}}}
;;{{{    fold-mode-menu-bar-map
(defvar fold-mode-menu-bar-map nil)
(if fold-mode-menu-bar-map
    nil
    (setq fold-mode-menu-bar-map (make-sparse-keymap))
    (define-key fold-mode-menu-bar-map [fold]
      (cons "Fold" (make-sparse-keymap "Fold")))
    (define-key fold-mode-menu-bar-map [fold fold-region]
      '("Create Fold" . fold-region))
    (define-key fold-mode-menu-bar-map [fold fold-whole-buffer]
      '("Fold Whole Buffer" . fold-whole-buffer))
    (define-key fold-mode-menu-bar-map [fold fold-show-all]
      '("Show All" . fold-show-all))
    (define-key fold-mode-menu-bar-map [fold fold-show]
      '("Show Fold" . fold-show))
    (define-key fold-mode-menu-bar-map [fold fold-hide]
      '("Hide Fold" . fold-hide)))
;;}}}

(defvar fold-mode-prefix-key "\C-c@"  ;;{{{
  "Set this *before* loading fold mode to assign fold mode keymap to a
different prefix. The default \\C-c@ is also used by outline minor
mode, which is not normally a conflict.

Author's note: Emacs has too many key bindings already, so we stick
ours in a somewhat obscure place. I don't actually use these bindings,
but define my own in my .emacs file, and I encourage you to do
likewise. Pushing three keys with different sets of bucky bits to
open/close a fold is just wrong. Let's not talk about getting an `@'
on language specific keyboards...")
;;}}}
;;{{{    fold-mode-map
(defvar fold-mode-map nil)
(if fold-mode-map
    nil
    (setq fold-mode-map (make-sparse-keymap))
    (define-key fold-mode-map [menu-bar] fold-mode-menu-bar-map)
    (define-key fold-mode-map fold-mode-prefix-key fold-mode-prefix-map))

(or (assq 'fold-mode-active minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'fold-mode-active fold-mode-map)
		minor-mode-map-alist)))
;;}}}
;;}}}
;;{{{  Hook
(defvar fold-mode-hook nil
  "Hook called when Fold mode is entered.")
;;}}}
;;{{{  Regular expressions
(defvar fold-mode-marks-alist
  ;;Setting from comment-start and comment-end will usually work fine,
  ;;I think.  The various lisp modes are a notable exception.
  '((emacs-lisp-mode ";;" "")
    (lisp-interaction-mode ";;" "")
    (lisp-mode ";;" "")
    (scheme-mode ";;" "")))

(defvar fold-prefix nil)
(defvar fold-suffix nil)
(defvar fold-top-mark nil)
(defvar fold-bottom-mark nil)
(defvar fold-top-regexp nil)
(defvar fold-bottom-regexp nil)
(defvar fold-regexp nil)
(defvar fold-magic-regexp nil)

(defun fold-set-marks (&optional prefix suffix top bottom special)
  ;;{{{  documentation
  "Set the fold marks for the current buffer.

Set the fold mark PREFIX, SUFFIX, and optionally the TOP, BOTTOM, and
SPECIAL mark.  Let '.' mean concatenation, then your folds will look
like this:

PREFIX.TOP.title.SUFFIX
  fold-contents
PREFIX.BOTTOM.SUFFIX

Magic markers look like PREFIX.TOP.nonwhite.BOTTOM.SUFFIX, where
nonwhite matches the [^ \t\n]* regexp. Something that looks like a
magic marker can never be a fold mark.

SPECIAL is not used anywhere.

Default values for, PREFIX, SUFFIX, TOP, BOTTOM, and SPECIAL are \"\",
\"\", \"{{{\", \"}}}\" and \":::\", respectively.

Using other than the default values for TOP and BOTTOM is not
recommended unless *really* necessary. For certain choices of TOP and
BOTTOM, fold-mode will not work correctly. Don't do it. Just say no."
  ;;}}}
  ;;{{{  code
  (make-local-variable 'fold-prefix)
  (set (make-local-variable 'fold-prefix)
       (or prefix ""))
  (make-local-variable 'fold-suffix)
  (set (make-local-variable 'fold-suffix)
       (or suffix ""))
  (make-local-variable 'fold-top-mark)
  (set (make-local-variable 'fold-top-mark)
       (or top "{{{"))
  (make-local-variable 'fold-bottom-mark)
  (set (make-local-variable 'fold-bottom-mark)
       (or bottom "}}}"))
  (make-local-variable 'fold-top-regexp)
  (set (make-local-variable 'fold-top-regexp)
       (concat "\\("
               (regexp-quote fold-prefix)
               "[ \t]*"
               (regexp-quote fold-top-mark)
               "\\)\\([^}\n]*\\)"
               (regexp-quote fold-suffix)
               "[ \t]*\n"))
  (make-local-variable 'fold-bottom-regexp)
  (set (make-local-variable 'fold-bottom-regexp)
       (concat (regexp-quote fold-prefix)
               "[ \t]*"
               (regexp-quote fold-bottom-mark)
               "[ \t]*" 
               (regexp-quote fold-suffix)
               "[ \t]*$"))
  (make-local-variable 'fold-magic-regexp)
  (set (make-local-variable 'fold-magic-regexp)
       (concat (regexp-quote fold-prefix)
               (regexp-quote fold-top-mark)
               "[^ \t\n]*"
               (regexp-quote fold-bottom-mark)
               (regexp-quote fold-suffix)))
  (make-local-variable 'fold-regexp)
  (set (make-local-variable 'fold-regexp)
       (concat fold-top-regexp
               "\\|"
               fold-bottom-regexp)))
  ;;}}}

(defun fold-determine-fold-marks ()
  "Determine fold marks for current buffer. Return a list that can be
used in (apply fold-set-marks returned-list)"
  (or (cdr (assq major-mode fold-mode-marks-alist))
      (list (fold-trim-string (or comment-start ""))
            (fold-trim-string (or comment-end "")))))

(defun fold-use-normal-marks ()
  "Set fold marks for current buffer as determined by
fold-determine-fold-marks."
  (interactive)
  (apply 'fold-set-marks (fold-determine-fold-marks)))
;;}}}
;;{{{  User options
(defvar fold-close-on-startup t
  "*If non-nil, folds are closed when starting Fold mode, as if
\\[fold-whole-buffer] was used. If nil, folds remain open.")

(defvar fold-whole-buffer-moves-point nil
  "*If non-nil, \\[fold-whole-buffer] will move point if
it would end up inside a fold, so it can close all folds. If nil,
folds are left open in order to keep point visible.")

(defvar fold-autoclose-other-folds t
  "*If non-nil \\[fold-show] will close all other folds")

(defvar fold-automatic-show t
  "*If non-nil, moving into invisible folds automatically invokes
\\[fold-show]")

;;}}}
;;{{{  User commands
(defun fold-whole-buffer (&optional arg) ;;{{{  
  "Re-parse the buffer for fold marks and set folds to the closed state

Point is never left inside a closed fold. Depending on the setting of
fold-whole-buffer-moves-point, point is moved to the beginning of
the outermost enclosing fold, or folds are opened until point is
visible.

With prefix arg, always close all folds regardless of
fold-whole-buffer-moves-point setting."
  (interactive "P")
  (fold-delete-all-overlays)
  (save-excursion
    (goto-char (point-min))
    (fold-scan-forward (function fold-create-fold-overlays) t))
  (if (or arg fold-whole-buffer-moves-point)
      (fold-move-point)
      (fold-foreach (function fold-make-overlay-visible)
                    (fold-overlays-at (point) 'fold-overlay-class-hidden))))
;;}}}

(defun fold-show-all ()
  (interactive)
  (fold-foreach (function fold-make-overlay-visible)
                (fold-overlays-in (point-min) (point-max)
                                  'fold-overlay-class-hidden)))
(defun fold-hide ()
  (interactive)
  (fold-hide-fold-at (fold-last-char-on-line))
  (fold-move-point)
  (beginning-of-line))

(defun fold-show ()
  (interactive)
  (fold-show-fold-at (fold-last-char-on-line)))

(defun fold-region (begin end)
  (interactive "r")
  (if (not (eq (fold-innermost-fold-at begin)
               (fold-innermost-fold-at end)))
      (error "New fold cannot span existing folds")
      (fold-fold-region begin end)))
;;}}}
;;{{{  Helper functions

(defun fold-show-fold-at (pos);;{{{  
  (let ((fold (car (sort (fold-overlays-at pos 'fold-overlay-class-span)
                         (lambda (ovl1 ovl2)
                           (> (overlay-start ovl1) (overlay-start ovl2)))))))
    (if fold
        (progn
          (and fold-autoclose-other-folds
               (fold-foreach (function fold-make-overlay-invisible)
                             (fold-overlays-in (point-min) (point-max)
                                               'fold-overlay-class-inner)))
          (fold-foreach (function fold-make-overlay-visible)
                        (fold-overlays-at (- (overlay-end fold) 1)
                                          'fold-overlay-class-hidden))))))
;;}}}
(defun fold-hide-fold-at (pos) ;;{{{  
  (let ((fold (car (sort (fold-overlays-at pos 'fold-overlay-class-span)
                         (lambda (ovl1 ovl2)
                           (> (overlay-start ovl1) (overlay-start ovl2)))))))
    (if fold
        (fold-foreach (function fold-make-overlay-invisible)
                      (fold-filter (lambda (ovl)
                                     (and
                                      (< (overlay-start fold) 
                                         (overlay-start ovl))
                                      (<= (overlay-end ovl)
                                          (overlay-end fold))))
                                   (fold-overlays-in (overlay-start fold)
                                                     (overlay-end fold)
                                        'fold-overlay-class-inner))))))
;;}}}
(defun fold-move-point () ;;{{{
  (let ((outer-hidden
         (car (sort (fold-overlays-at (point) 'fold-overlay-class-hidden)
                    (lambda (ovl1 ovl2)
                      (< (overlay-start ovl1) (overlay-start ovl2)))))))
    (if outer-hidden
        (let ((fold
               (car (sort (fold-overlays-at
                           (overlay-start outer-hidden)
                           'fold-overlay-class-span)
                          (lambda (ovl1 ovl2)
                            (> (overlay-start ovl1) (overlay-start ovl2)))))))
          (if fold
	      ;; fixes the overlay jump direction (dmr@c0nc3pt.com)
	      (if (equal this-command 'forward-char)
		  (goto-char (overlay-end fold))
		(goto-char (overlay-start fold)))
	    (error "Hidden overlay outside of fold?"))))))
;;}}}
(defun fold-innermost-fold-at (pos)  ;;{{{  
  (car (sort (fold-overlays-at pos 'fold-overlay-class-span)
             (lambda (ovl1 ovl2)
               (> (overlay-start ovl1) (overlay-start ovl2))))))
;;}}}
(defun fold-post-command-handler ()  ;;{{{  
  (and fold-mode-active
       (get-char-property (point) 'invisible)
       (if (not fold-automatic-show)
           (fold-move-point)
           (fold-show-fold-at (point)))))
;;}}}
(defun fold-last-char-on-line ()  ;;{{{  
  (let ((bol (save-excursion
               (beginning-of-line)
               (point)))
        (eol (save-excursion
               (end-of-line)
               (point))))
    (if (= bol eol)
        eol
        (- eol 1))))
;;}}}

;;}}}
;;{{{  Overlays
(defun fold-overlay-readonly (overlay when &rest r)
  nil);;FIXME
;;{{{    Overlay classes (categories)
(defvar fold-overlay-classes 
  '(fold-overlay-class-span
    fold-overlay-class-mark
    fold-overlay-class-inner
    fold-overlay-class-hidden)
  "Overlay classes used by Fold mode")

;;These overlays span the whole fold, including marks. 
(setplist 'fold-overlay-class-span
          '(priority 90
            evaporate t))

;;These overlays are used to paint fold marks
(setplist 'fold-overlay-class-mark
          '(priority 100
            evaporate t
            modification-hooks (fold-overlay-readonly)
            face bold))

;;These are used for hiding
(setplist 'fold-overlay-class-inner
          '(priority 100
            evaporate t))

(setplist 'fold-overlay-class-hidden
          '(priority 100
            evaporate t
            invisible fold
            isearch-open-invisible fold-make-overlay-visible))
;;}}}
(defun fold-create-fold-overlays (start end hidden);;{{{  
  "Create all overlays for a fold starting at START and ending at END
with initial state HIDDEN. Correct fold markers must be present."
  (save-excursion
    (goto-char start)
    (and (looking-at fold-top-regexp)
         (let ((span (make-overlay start end nil t nil))
               (mark (make-overlay (match-beginning 1) (match-end 1)
                                   nil t nil))
               (inner (make-overlay (match-end 2) end nil t nil)))
           (overlay-put span 'category 'fold-overlay-class-span)
           (overlay-put mark 'category 'fold-overlay-class-mark)
           (overlay-put inner 'category
                        (if hidden 'fold-overlay-class-hidden
                            'fold-overlay-class-inner))))))
;;}}}
(defun fold-overlays-in (from to &optional class) ;;{{{  
  (fold-filter (if class
                   (lambda (ovl)
                     (eq (overlay-get ovl 'category) class))
                   (lambda (ovl)
                     (memq (overlay-get ovl 'category) fold-overlay-classes)))
               (overlays-in from to)))
;;}}}
(defun fold-overlays-at (pos &optional class) ;;{{{  
  (fold-filter (if class
                   (lambda (ovl)
                     (eq (overlay-get ovl 'category) class))
                   (lambda (ovl)
                     (memq (overlay-get ovl 'category) fold-overlay-classes)))
               (overlays-at pos)))
;;}}}
(defun fold-delete-all-overlays ()  ;;{{{  
  "Delete all Fold mode specific overlays"
  (fold-foreach (function delete-overlay) 
                (fold-overlays-in (point-min) (point-max))))
;;}}}
(defun fold-make-overlay-visible (ovl)  ;;{{{  
  (overlay-put ovl 'invisible nil)     
  (if (eq (overlay-get ovl 'category) 'fold-overlay-class-hidden)
      (overlay-put ovl 'category 'fold-overlay-class-inner)))
;;}}}
(defun fold-make-overlay-invisible (ovl)  ;;{{{  
  (overlay-put ovl 'invisible 'fold)     
  (if (eq (overlay-get ovl 'category) 'fold-overlay-class-inner)
      (overlay-put ovl 'category 'fold-overlay-class-hidden)))
;;}}}

;;}}}
;;{{{  Create overlays according to fold marks

(defun fold-scan-forward (make-fold close &optional bound)
  "Scan forward through the file, starting at point, creating fold
overlays (using function MAKE-FOLD) as we go. New folds are closed iff
CLOSE is non-nil. Returns position after first unmatched
bottom-of-fold mark, or nil if only balanced folds were found. Signals
an error if there were unmatched top-of-fold marks. Doesn't preserve
point. Pays no attention to existing fold overlays."
  (let (retval)
    (while 
      (let ((pos (re-search-forward fold-regexp bound t)))
        (and pos
             (let ((begin (match-beginning 0)))
               (cond 
                 ((save-excursion
                    (goto-char begin)
                    (looking-at fold-magic-regexp)))
                 ((save-excursion 
                    (goto-char begin)
                    (looking-at fold-bottom-regexp))
                  (not (setq retval pos)))
                 ((let ((end (fold-scan-forward make-fold close bound)))
                    (if (not end)
                        (error "Unmatched top-of-fold mark at %s"
                               (goto-char pos))
                        (funcall make-fold begin end close)
                        (goto-char end)))))))))
    retval))

;;}}}
;;{{{  Create new folds

(defvar fold-indentation-of-new-marks nil)

(defun fold-fold-region (start end) ;;{{{  
  "Places fold marks at the beginning and end of a specified region.
The region is specified by two arguments START and END.  The point is
left at a suitable place ready to insert the title of the fold."
  ;; FIXME creation of fold when point==mark, placed in middle of a line
  (and (< end start)
       (setq start (prog1 end
                     (setq end start))))
  (let ((indentation (or fold-indentation-of-new-marks
                         (fold-region-min-indentation start end))))
    (setq end (set-marker (make-marker) end))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (insert-before-markers fold-prefix fold-top-mark "  " )
    (save-excursion
      (beginning-of-line)
      (indent-to indentation))
    (let ((saved-point (point)))
      (and fold-suffix
           (insert-before-markers fold-suffix))
      (insert-before-markers ?\n)
      (goto-char (marker-position end))
      (and (not (bolp))
           (eq 0 (forward-line))
           (eobp)
           (insert ?\n))
      (indent-to indentation)
      (set-marker end nil)
      (insert fold-prefix fold-bottom-mark (or fold-suffix ""))
      (insert ?\n)
      (let ((bound (point)))
        (goto-char start)
        (fold-scan-forward (function fold-create-fold-overlays) nil bound)
        (fold-show-fold-at (point)))
      (goto-char saved-point))))
;;}}}

(defun fold-region-min-indentation (start end) ;;{{{
  "Get minimum indentation of any nonblank line in region.
For null region, or region of only blank lines, return current column."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (set-mark end)
    (if ( > (point) (mark))
        (exchange-point-and-mark))
    (let ((minindent (point-max)))
      (while (< (point) (mark))
        (beginning-of-line)
        (if (not (looking-at "[ \t]*$"))
            (setq minindent (min minindent (current-indentation))))
        (beginning-of-line 2))
      (if (equal (point-max) minindent) 
          (current-column)
        minindent))))
;;}}}
;;}}}
;;{{{  Auxiliary list and string functions

(defun fold-filter (f list)
  (let (result)
    (while list
      (if (funcall f (car list))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (nreverse result)))

(defun fold-foreach (f list)
  (mapcar f list)
  list)

(defun fold-trim-string (s)
  (string-match "^[ \t]*" s)
  (let ((s1 (replace-match "" nil t s)))
    (string-match "[ \t]*$" s1)
    (replace-match "" nil t s1)))
;;}}}

;;{{{ commands add by Ye Wenbin
(defun fold-toggle ()
  (interactive)
  (let ((fold (car (sort (fold-overlays-at (fold-last-char-on-line)
                                           'fold-overlay-class-span)
                         (lambda (ovl1 ovl2)
                           (> (overlay-start ovl1) (overlay-start ovl2)))))))
    (when fold
      (if (fold-overlays-at (- (overlay-end fold) 1)
                            'fold-overlay-class-hidden)
          (fold-show)
        (fold-hide)))))

(defun fold-next ()
  (interactive)
  (let ((pos (point))
        fold-automatic-show)
    (and (re-search-forward (concat (regexp-quote fold-prefix)
                                    "[ \t]*"
                                    (regexp-quote fold-top-mark))  nil t)
         (or (save-excursion (re-search-backward "\n" pos t))
             (fold-next)))))

(defun fold-previous ()
  (interactive)
  (let (fold-automatic-show
        (pos (point)))
    (and (re-search-backward (concat (regexp-quote fold-prefix)
                                     "[ \t]*"
                                     (regexp-quote fold-top-mark)) nil t)
         (or (save-excursion (re-search-forward "\n" pos t))
             (fold-previous)))))

(defun fold-hide-other ()
  (interactive)
  (let ((fold-autoclose-other-folds t))
    (fold-show-fold-at (fold-last-char-on-line))))

(defvar fold-check-size nil
  "If nil, search whole buffer for fold marks, if value is an
interger, check for in that range. If value is a function, call it to
set a region to search")
(defun fold-find-file-hook ()
  (let* ((marks (fold-determine-fold-marks))
         (prefix (or (car marks) ""))
         (suffix (or (cadr marks) ""))
         (top-mark (or (nth 2 marks) "{{{"))
         (bottom-mark (or (nth 3 marks) "}}}"))
         (top-regexp (concat (regexp-quote prefix)
                             "[ \t]*" (regexp-quote top-mark)
                             "[^}\n]*" (regexp-quote suffix)
                             "[ \t]*\n"))
         (bottom-regexp (concat (regexp-quote prefix)
                                "[ \t]*" (regexp-quote bottom-mark)
                                "[ \t]*" (regexp-quote suffix)
                                "[ \t]*$"))
         (size (if (or (null fold-check-size)
                       (numberp fold-check-size))
                   fold-check-size (funcall fold-check-size))))
    (if (save-excursion
          (goto-char (point-min))
          (and (re-search-forward top-regexp size t)
               (re-search-forward bottom-regexp size t)))
        (fold-mode 1)
      nil)))
;;}}}
(provide 'fold)
;;; fold.el ends here
