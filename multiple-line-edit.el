;;; multiple-line-edit.el --- Edit multiple line at a time.

;; Copyright (C) 2010 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: 10 Oct 2010 PM 02:44 JST
;; Keywords: abbrev convenience emulations wp
;; Revision: $Id: 16d1fe108a0150265b15e04b909d3922673b1afd $
;; URL: http://www.emacswiki.org/emacs/download/multiple-line-edit.el
;; GitHub: http://github.com/k-talo/multiple-line-edit.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; NOTE
;;
;; This library is just tested on Emacs 23.2.1 on Ubuntu 10.04
;; and Mac OS X 10.6.3, and won't be run with any version of XEmacs.

;;; Commentary:
;;
;; Overview
;; ========
;; This library provides `multiple line edit' feature.
;;
;; You can edit each side, leading and trailing edge, of
;; multiple line with this library.
;;
;;
;; INSTALLING
;; ==========
;; To install this library, save this file to a directory in your
;; `load-path' (you can view the current `load-path' using "C-h v
;; load-path RET" within Emacs), then add the following line to your
;; .emacs startup file:
;;
;;    (require 'multiple-line-edit)
;;
;;
;; USING
;; =====
;; To start multiple line edit, follow the instructions described
;; below.
;;
;;   1. Select multiple line that you want to edit.
;;
;;   2. If you want to edit the leading edges of each line,
;;      type `M-x mulled/edit-leading-edges RET'.
;;
;;      Or if you want to edit the trailing edges of each line,
;;      type `M-x mulled/edit-trailing-edges RET'.
;;
;;      NOTE: You can run these commands by selecting menu items
;;            placed in `"Edit" > "Multiple Line Edit"'.
;;
;;   3. When multiple line edit is activated, the cursor will be
;;      appeared on each selected lines.
;;
;;      And then, your modification will be applied to each lines
;;      at a time.
;;
;;      NOTE: While multiple line edit is active, indicator icons
;;            will be appear on the fringes if you are running Emacs
;;            on graphical environment like X Window System.
;;
;;           (The icon `>>' will be used for leading edges edit, and
;;            the icon `<<' will be used for trailing edges edit)
;;
;;      While you are in multiple line editing, you can toggle leading
;;      edge edit and trailing edge edit by commands `mulled/edit-leading-edges'
;;      and `mulled/edit-trailing-edges'.
;;
;;      Note that multiple edit will be exit when the color of cursor
;;      on each line is red, it means cursor position is out of the
;;      range on some lines.
;;
;;   4. To finish multiple line edit, type `C-g', or move cursor
;;      to outside of the each line.
;;
;;   5. If you finished multiple line edit unexpectedly, you can
;;      restore multiple line edit session by command `undo'.
;;
;;      This behavior is customized via user option
;;      `mulled/reactivate-by-undo'.
;;
;; Also check out the customization group
;;
;;   `M-x customize-group RET multiple-line-edit RET'
;;
;;
;; COMMANDS
;; ========
;;  `M-x mulled/edit-trailing-edges RET'
;;
;;      Start editing on trailing edge of multiple line.
;;      
;;      You have to select lines, that you want to edit at a time,
;;      before you run this command.
;;      
;;      When called with prefix argument, cursor remains at current
;;      position.
;;      
;;      Otherwise, cursor will be moved to beginning of the lines.
;;      
;;      You can abort multiple lines edit by typing "C-g"
;;      or move cursor to outside of each line.
;;
;;  `M-x mulled/edit-leading-edges RET'
;;
;;      Start editing on leading edge of multiple line.
;;      
;;      You have to select lines, that you want to edit at a time,
;;      before you run this command.
;;      
;;      When called with prefix argument, cursor remains at current
;;      position.
;;      
;;      Otherwise, cursor will be moved to end of the lines.
;;      
;;      You can abort multiple lines edit by typing "C-g"
;;      or move cursor to outside of each line.
;;
;;  `M-x mulled/switch-direction RET'
;;
;;      Switch Leading Edges Edit and Trailing Edges Edit.
;;            
;;      When called with prefix argument, cursor won't be moved to
;;      each edges.
;;            
;;      Otherwise, cursor will be moved to either edge.
;;      
;;      You can also switch editing direction by commands
;;      `mulled/edit-leading-edges' and `mulled/edit-trailing-edges'.
;;
;;  `C-g'
;;  `M-x mulled/abort RET'
;;
;;      Exit from multiple line edit.
;;      You can restore multiple line edit session, which aborted
;;      by this command, with the command `undo'.
;;
;;  `M-x mulled/force-abort RET'
;;      Force abort multiple line edit.
;;      Try this command when multiple line edit is broken
;;      by errors.
;;
;;
;; Key map Examples
;; ================
;; This library does not assign keys by default.
;; Put lines below to your .emacs start up file then
;; customize them as you like.
;;
;;   (global-set-key "\C-c<" 'mulled/edit-trailing-edges)
;;   (global-set-key "\C-c>" 'mulled/edit-leading-edges)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; - Codes aside, this document should be rewritten.
;;   My English sucks :-(
;;
;; - Multiple line edit won't work with commands that sets
;;   non-nil value to the variable `inhibit-modification-hooks'
;;   like `YASnippet'.
;;  
;;   We wrote experimental patches to run `YASnippet' with
;;   `multiple-line-edit', put a line below in your .emacs startup
;;   file if you are interested in.
;;  
;;      (mulled/experimental/install-yas-support)
;;


;;; Change Log:

;;  v2.0.0 Mon Dec 27 03:06:00 2010 JST
;;   - Changed version numbering rule from x.y to x.y.z.
;;   - Display pseudo cursors on each multiple edit lines.
;;
;;  v1.11 Mon Dec 27 01:48:21 2010
;;   - Fixed a bug that the feature `reactivation by undo' does not
;;     work if the deactivation was not triggered by keyboard quit.
;;
;;   - Fixed error in `mulled/pt-offset-by-col-num' which occurred
;;     when the COL-NUM is negative number.
;;
;;  v1.10 Sat Dec  4 16:15:07 2010 JST
;;   - Fixed a bug that cursor position won't be set properly
;;     by `mulled/edit-leading-edges' and `mulled/edit-trailing-edges'
;;     with `keep-offset' option when wide-width character such as
;;     TAB and Kanji characters are in lines.
;;
;;  v1.9, Fri Dec  3 17:55:52 2010 JST
;;   - Restore position of cursor when multiple line edit is
;;     re-activated by `undo'.
;;
;;   - New option `keep-offset' to multiple lines edit commands.
;;
;;  v1.8, Sun Nov 14 22:05:08 2010 JST
;;   - Fixed a bug that the indicator on left fringe was not displayed
;      when the code is byte compiled.
;;
;;  v1.7, Tue Nov  9 16:48:21 2010 JST
;;   - Fixed a bug that column number are not calculated properly
;;     on lines which contains wide width characters.
;;
;;  v1.6, Wed Oct 27 02:58:04 2010 JST
;;   - Experimental user option `mulled/apply-special-fn-to-each-line-p'.
;;     When this value is non-nil, special function defined in
;;     `mulled/special-fn-alist' will be applied to each line
;;     instead of mirroring the result of a function applied at 1st line.
;;
;;  v1.5, Mon Oct 25 11:45:06 2010 JST
;;   - Fixed bugs regarding to reactivation by undo command.
;;     This bug was caused when line breaks ware inserted in
;;     multiple line edit session.
;;
;;  v1.4, Sun Oct 24 22:44:13 2010 JST
;;   - Fixed a fatal error:
;;     "Symbol's value as variable is void: remove-text-properties-p"
;;     This bug was brought by failure of merge operation.
;;
;;   - Made `mulled/experimental/install-yas-support' not to run
;;     automatically.
;;
;;  v1.3, Sun Oct 24 00:07:02 2010 JST
;;   - Fixed bugs regarding to reactivation by undo command.
;;
;;  v1.2
;;   - Added support for replacement operations like `upcase-region'.
;;
;;  v1.1
;;   - Fixed bug that state of multiple line edit was destroyed
;;     by a undo command in some cases.

;;; Code:

(provide 'multiple-line-edit)

(defconst multiple-line-edit/version "1.6")

(eval-when-compile
  (require 'cl)
  (require 'easymenu))

;; Private Variables
;;
(defvar mulled/.running-primitive-undo-p nil)
(defvar mulled/.undo-at nil)
(defvar mulled/.last-pt nil)
(make-variable-buffer-local 'mulled/.last-pt)

 
;;; ===========================================================================
;;;
;;;  User customizable things.
;;;
;;; ===========================================================================

(defgroup multiple-line-edit nil
  "Multiple line edit"
  :group 'editing)

(defcustom mulled/reactivate-by-undo t
  "Non-nil means reactivate multiple line edit by undo/redo."
  :type 'boolean
  :group 'multiple-line-edit)

(defface mulled/cursor-face
  '((((class color) (background light))
     :background "grey")
    (((class color) (background dark))
     :background "white")
     (t :inverse-video t))
  "Face used for cursor of multiple line edit."
  :group 'multiple-line-edit)

(defface mulled/out-of-range-cursor-face
  '((((class color)) :foreground "white" :background "Red1")
    (t :inverse-video t :weight bold))
  "Face used for cursor of multiple line edit when
when either cursor is outside of each line."
  :group 'multiple-line-edit)

(defface mulled/fringe-face
  '((((class color) (background light)) nil)
    (t nil))
  "The face used to display icons on the fringes."
  :group 'multiple-line-edit)

(defcustom mulled/apply-special-fn-to-each-line-p nil
  "Non-nil means special functions defined in `mulled/special-fn-alist'
will be applied to each lines instead of mirroring the result of
a function applied at 1st line.

Default value is `t'.

*THIS FEATURE IS EXPERIMENTAL AND MAY BE REMOVED IN THE FUTURE*"
  :type 'boolean
  :group 'multiple-line-edit)

;; XXX: Need `mulled/special-cmd-alist'?
(defcustom mulled/special-fn-alist
  '((capitalize-region . (lambda (beg end &rest orig-args)
                           (capitalize-region beg end)))
    (downcase-region . (lambda (beg end &rest orig-args)
                           (downcase-region beg end)))
    (upcase-region . (lambda (beg end &rest orig-args)
                           (upcase-region beg end))))
  "Alist name of special functions and callback function which have to
be applied to each line directory instead of mirroring the result of
a function applied at 1st line.

*THIS FEATURE IS EXPERIMENTAL AND MAY BE REMOVED IN THE FUTURE*"
  :group 'multiple-line-edit)

 
;;; ===========================================================================
;;;
;;;  Commands and Menus.
;;;
;;; ===========================================================================

;; Commands
;;
(defun mulled/edit-trailing-edges (&optional keep-offset)
  "Start editing on trailing edge of multiple line.

You have to select lines, that you want to edit at a time,
before you run this command.

When called with prefix argument, cursor remains at current
position.

Otherwise, cursor will be moved to beginning of the lines.

You can abort multiple lines edit by typing \"C-g\"
or move cursor to outside of each line."
  (interactive "P")
  (cond
   ((mulled/is-leading-edges-edit-in-progress )
    (mulled/switch-direction keep-offset))
   ((mulled/is-trailing-edges-edit-in-progress)
    (message "[mulled] Multiple line edit on trailing edge is in progress."))
   ((mulled/lines-selected-p)
    (mulled/ov-1st-line/activate (region-beginning) (region-end) t keep-offset))
   (t
    (message "[mulled] Select multiple line first."))))

(defun mulled/edit-leading-edges (&optional keep-offset)
  "Start editing on leading edge of multiple line.

You have to select lines, that you want to edit at a time,
before you run this command.

When called with prefix argument, cursor remains at current
position.

Otherwise, cursor will be moved to end of the lines.

You can abort multiple lines edit by typing \"C-g\"
or move cursor to outside of each line."
  (interactive "P")
  (cond
   ((mulled/is-trailing-edges-edit-in-progress)
    (mulled/switch-direction keep-offset))
   ((mulled/is-leading-edges-edit-in-progress)
    (message "[mulled] Multiple line edit on leading edge is in progress."))
   ((mulled/lines-selected-p)
    (mulled/ov-1st-line/activate (region-beginning) (region-end) nil keep-offset))
   (t
    (message "[mulled] Select multiple line first."))))

(defun mulled/switch-direction (&optional keep-offset)
  "Switch Leading Edges Edit and Trailing Edges Edit.
      
When called with prefix argument, cursor won't be moved to
each edges.
      
Otherwise, cursor will be moved to either edge.

You can also switch editing direction by commands
`mulled/edit-leading-edges' and `mulled/edit-trailing-edges'."
  (interactive "P")
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if ov
        (mulled/ov-1st-line/switch-direction ov keep-offset)
      (message "[mulled] Multiple line edit is not in progress."))))

(defun mulled/abort ()
  "Exit from multiple line edit.
You can restore multiple line edit session, which aborted
by this command, with the command `undo'."
  (interactive)
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if ov
        (progn (mulled/ov-1st-line/dispose ov)
               (message "[mulled] Multiple line edit exited."))
      (mulled/force-abort))))

(defun mulled/force-abort ()
  "Force abort multiple line edit.
Try this command when multiple line edit is broken
by errors."
  (interactive)
  (save-restriction
    (widen)
    (let ((ov-lst (overlays-in (point-min)
                               (point-max)))
          disposed-p)
      (dolist (ov ov-lst)
        (when (mulled/ov-1st-line/ov-1st-line-p ov)
          (mulled/ov-1st-line/dispose ov)
          (setq disposed-p t)))
      (when disposed-p
        (message "[mulled] Multiple line edit force exited.")))))


;; Menus
;;
(defun mulled/install-menu ()
  (when (easy-menu-item-present-p nil '("Edit") "Multiple Line Edit")
    (easy-menu-remove-item nil '("Edit") "Multiple Line Edit"))
  (let ((map (make-sparse-keymap)))
    (easy-menu-define mulled/menu
      map
      "Menu items for multiple line edit."
      '("Multiple Line Edit"
        ["Edit Leading Edges" mulled/edit-leading-edges
         :active (or (mulled/is-trailing-edges-edit-in-progress)
                     (mulled/lines-selected-p))
         :help "Edit Leading Edge of Multiple Line."]

        ["Edit Trailing Edges" mulled/edit-trailing-edges
         :active (or (mulled/is-leading-edges-edit-in-progress)
                     (mulled/lines-selected-p))
         :help "Edit Trailing Edge of Multiple Line."]

        ["Toggle Leading/Trailing Edges Edit" mulled/switch-direction
         :active (mulled/ov-1st-line/find-at (point))
         :help "Switch Leading Edges Edit and Trailing Edges Edit."]

        ["Exit Edit" mulled/abort
         :active (mulled/ov-1st-line/find-at (point))
         :help "Finish Current Multiple Line Edit."]

        "----"

        ("Misc"
         ["Reactivate multiple line Edit on Undo" 
          (setq mulled/reactivate-by-undo
                (not mulled/reactivate-by-undo))
          :help "Non-nil means reactivate multiple line edit after undo/redo."
          :style toggle :selected mulled/reactivate-by-undo]
         )))
    (add-submenu '("Edit") mulled/menu)))

(defun mulled/uninstall-menu ()
  (when (easy-menu-item-present-p nil '("Edit") "Multiple Line Edit")
    (easy-menu-remove-item nil '("Edit") "Multiple Line Edit")))

(mulled/uninstall-menu)
(mulled/install-menu)


 
;;; ===========================================================================
;;;
;;;  Utility Functions.
;;;
;;; ===========================================================================

(defun mulled/lines-selected-p ()
  (and (if transient-mark-mode mark-active t) ;;(region-active-p)
       (< 1 (count-lines (region-beginning) (region-end)))))

(defun mulled/is-trailing-edges-edit-in-progress ()
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (and ov
         (mulled/ov-1st-line/edit-trailing-edges-p ov))))

(defun mulled/is-leading-edges-edit-in-progress ()
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (and ov
         (not (mulled/ov-1st-line/edit-trailing-edges-p ov)))))

;; Point and Column translations.
;; NOTE: Point counts number of characters.
;;       Column counts visual width of characters.

(defun mulled/pt-offset-by-col-num (pt-beg col-num)
  "Calculate point offset from PT-BEG by COL-NUM.

Line break character will be counted as one column.

When COL-NUM is negative number, returns PT-BEG.
When the result exceeds `point-max' of current buffer,
returns `point-max'."
  (let ((done nil))
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region pt-beg (point-max))
        (goto-char (point-min))
        
        (while (and (not (eobp))
                    (not done))
          (end-of-line)
          (let ((cur-col (current-column)))
            (if (<= col-num cur-col)
                (progn
                  (move-to-column (max 0 col-num))
                  (setq done t))
              ;; Count line break as 1 column.
              (progn
                (goto-char (1+ (point)))
                (setq col-num (- col-num cur-col 1))))))
        (widen)
        (point)))))

(defun mulled/col-num-in-region (pt-beg pt-end)
  "Calculate number of columns, based on visible char width,
in the region specified by PT-BEG and PT-END.

Line break character will be counted as one column."
  (let ((col-num 0))
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region pt-beg pt-end)
        (goto-char (point-min))

        (while (not (= (point) (point-max)))
          (setq col-num (+ col-num
                           (progn (end-of-line)
                                  (current-column))))
          ;; Count line break as 1 column.
          (when (not (= (point) (point-max)))
            (goto-char (1+ (point)))
            (setq col-num (1+ col-num))))))
    col-num))

 
;;; ===========================================================================
;;;
;;;  ov-1st-line: Overlay placed on 1st line, to observe user input.
;;;
;;; ===========================================================================

;; Class Variables
;;
(defvar mulled/ov-1st-line/keymap (make-sparse-keymap)
  "The keymap active while a multiple line edit is in progress.")
(define-key mulled/ov-1st-line/keymap [?\C-g] 'mulled/abort)


;; Some class methods for convenience.
;;
(defun mulled/ov-1st-line/ov-1st-line-p (unknown-obj)
  (and (overlayp unknown-obj)
       (overlay-get unknown-obj 'mulled/ov-1st-line-p)))

(defun mulled/ov-1st-line/find-at (point)
  (dolist (ov (overlays-at point))
    (when (overlay-get ov 'mulled/lines)
      (return ov))))

(defun mulled/ov-1st-line/find-and-dispose-at (point)
  (let ((ov (mulled/ov-1st-line/find-at point)))
    (when ov
      (mulled/ov-1st-line/dispose ov))))


;; Constructor.
;;
(defun mulled/ov-1st-line/activate (r-pt-beg r-pt-end edit-trailing-edges-p &optional keep-offset)
  (mulled/ov-1st-line/activate-aux (mulled/lines/new r-pt-beg r-pt-end edit-trailing-edges-p)
                                   edit-trailing-edges-p
                                   keep-offset))
  
(defun mulled/ov-1st-line/reactivate (be-pair-lst edit-trailing-edges-p &optional keep-offset)
  (mulled/ov-1st-line/activate-aux (mulled/lines/new-by-be-pair-lst be-pair-lst
                                                                    edit-trailing-edges-p)
                                   edit-trailing-edges-p
                                   keep-offset))

(defun mulled/ov-1st-line/activate-aux (lines edit-trailing-edges-p &optional keep-offset)
  ;; To detect operations listed below, we put overlay
  ;; with extra padding around the line.
  ;;
  ;;   1. `delete-backward-char' at leading edge of the line.
  ;;   2. `delete-char' at trailing edge of the line.
  ;;
  (let* ((beg (mulled/lines/pt-nth-beg lines 0))
         (end (mulled/lines/pt-nth-end lines 0))
         (beg-from-point-min-p (= beg (point-min)))
         (ov (make-overlay (if beg-from-point-min-p
                               beg
                             (1- beg))
                           (1+ end)
                           nil
                           nil
                           nil))
         (cur-pt (point))) ;; Remember current point for undo.

    (let ((ov (mulled/ov-1st-line/find-at beg)))
      (when ov
        ;; May be rubbish.
        (mulled/ov-1st-line/dispose ov)))

    ;; Set attributes.
    (overlay-put ov 'mulled/ov-1st-line-p t)
    (overlay-put ov 'mulled/lines lines)
    (overlay-put ov 'mulled/edit-trailing-edges-p edit-trailing-edges-p)
    (overlay-put ov 'mulled/beg-from-point-min-p beg-from-point-min-p)

    ;; Set modification hooks.
    (overlay-put ov 'modification-hooks    '(mulled/ov-1st-line/mod-hook-fn))
    (overlay-put ov 'insert-in-front-hooks '(mulled/ov-1st-line/mod-hook-fn))
    (overlay-put ov 'insert-behind-hooks   nil)

    ;; Set keymap.
    (overlay-put ov 'keymap mulled/ov-1st-line/keymap)
    
    ;; For debugging.
    ;; (overlay-put ov 'face 'secondary-selection)


    ;; Set pre/post command hooks.
    (lexical-let ((ov ov)
                  fn)
      (setq fn (lambda ()
                 (condition-case c
                     (when (mulled/ov-1st-line/ov-1st-line-p ov) ;; Overlay is not disposed.
                       (let* ((ov-beg (mulled/ov-1st-line/get-beg-without-padding ov))
                              (ov-end (mulled/ov-1st-line/get-end-without-padding ov))
                              (ov-buf (overlay-buffer ov))
                              (cur-pt (progn
                                        ;; Keep cursor inside of 1st line
                                        ;; moved by undo operation.
                                        (when mulled/.undo-at
                                          (goto-char mulled/.undo-at)
                                          (setq mulled/.undo-at nil))
                                        (point))))
                         (when (eq (current-buffer)
                                   ov-buf)
                           (cond
                            ((not (and (<= ov-beg cur-pt)
                                       (<= cur-pt ov-end)))
                             ;; When the cursor went out of 1st line,
                             ;; stop multiple line edit.
                             (mulled/ov-1st-line/dispose ov)
                             (message "[mulled] Multiple line edit exited."))
                            ((not (equal mulled/.last-pt cur-pt))
                             ;; Cursor is moved.
                             (mulled/ov-1st-line/update-cursor-pos ov)))
                         
                           (setq mulled/.last-pt cur-pt))))
                   ;; To not break pre/post command hooks,
                   ;; we do not rise error in these hooks.
                   (error
                    (message "[mulled] Multiple line edit exited by error:\n%s." c)
                    (mulled/ov-1st-line/dispose ov)))))
      (add-hook 'pre-command-hook fn)
      (add-hook 'post-command-hook fn)

      ;; Remember this hook function.
      (overlay-put ov 'mulled/hook-fn fn))    
    
    (when (and mulled/reactivate-by-undo
               (listp buffer-undo-list))
      (push (mulled/ov-1st-line/make-dispose-form ov) buffer-undo-list)
      (push `(apply goto-char ,cur-pt) buffer-undo-list))
    

    ;; Move cursor to 1st line.
    ;;
    (cond
     ((and keep-offset
           ;; Inside of multiple line edit?
           (mulled/lines/nth-from-pt lines (point)))
      ;; Do not move cursor as possible as we can.
      ;;
      (let* ((nth         (mulled/lines/nth-from-pt lines (point)))
             (col-num     (mulled/lines/col-num-nth lines nth))
             (col-num-1st (mulled/lines/col-num-nth lines 0))
             (new-col-1st (cond
                           (edit-trailing-edges-p
                            (let ((offset (- col-num (current-column))))
                              (max 0 (- col-num-1st offset))))
                           (t
                            (min col-num (current-column))))))
        (goto-char beg) ;; Move to 1st line.
        (beginning-of-line)
        (move-to-column new-col-1st)))
     (t
      ;; Move to edges.
      ;;
      (goto-char beg)
      (cond
       (edit-trailing-edges-p
        (end-of-line))
       (t
        (beginning-of-line)))))

    ;; Deactivate selection.
    (setq mark-active nil)
    
    (mulled/lines/update-cursor-ov-lst lines edit-trailing-edges-p)
    ov))

;; Destructor.
;;
(defun mulled/ov-1st-line/dispose (ov)
  (let* ((lines (overlay-get ov 'mulled/lines))
         (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p))
         (line-num (mulled/lines/count-of lines))
         (fn (overlay-get ov 'mulled/hook-fn))
         (lines (overlay-get ov 'mulled/lines)))
    (when (and mulled/reactivate-by-undo
               (listp buffer-undo-list))
      (when (not (null (car buffer-undo-list)))
        (push nil buffer-undo-list))
      (when mulled/.last-pt
        (push `(apply goto-char ,mulled/.last-pt) buffer-undo-list))
      (push (mulled/ov-1st-line/make-reactivate-form ov) buffer-undo-list))

    (mulled/lines/dispose lines)

    ;; Delete attributes.
    (overlay-put ov 'mulled/ov-1st-line-p nil)
    (overlay-put ov 'mulled/lines nil)
    (overlay-put ov 'mulled/edit-trailing-edges-p nil)
    (overlay-put ov 'mulled/beg-from-point-min-p nil)
    (overlay-put ov 'mulled/hook-fn nil)
    
    ;; Set modification hooks.
    (overlay-put ov 'modification-hooks    nil)
    (overlay-put ov 'insert-in-front-hooks nil)
    (overlay-put ov 'insert-behind-hooks   nil)

    ;; Set keymap.
    (overlay-put ov 'keymap nil)

    (remove-hook 'pre-command-hook fn)
    (remove-hook 'post-command-hook fn)
    (delete-overlay ov)
    (setq mulled/.undo-at nil)
    (setq mulled/.last-pt nil)))


;; Instance Methods.
;;
(defun mulled/ov-1st-line/make-reactivate-form (ov)
  `(apply mulled/ov-1st-line/reactivate
          ;; Do not save marker to buffer-undo-list
          ;; not to make emacs slow.
          ,(mulled/lines/be-pair-lst/dup-without-marker
            (mulled/lines/be-pair-lst-of (overlay-get ov 'mulled/lines)))
          ,(overlay-get ov 'mulled/edit-trailing-edges-p)))

(defun mulled/ov-1st-line/make-dispose-form (ov)
`(apply mulled/ov-1st-line/find-and-dispose-at
        ,(mulled/ov-1st-line/get-beg-without-padding ov)))

(defun mulled/ov-1st-line/update-cursor-pos (ov)
  (let* ((lines                 (overlay-get ov 'mulled/lines))
         (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p)))
    (mulled/lines/update-cursor-ov-lst
     lines
     edit-trailing-edges-p)))

(defun mulled/ov-1st-line/switch-direction (ov &optional keep-offset)
  (let* ((lines (overlay-get ov 'mulled/lines))
         (be-pair-lst-wo-marker (mulled/lines/be-pair-lst/dup-without-marker
                                 (mulled/lines/be-pair-lst-of lines)))
         (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p)))
    (mulled/ov-1st-line/dispose ov)
    (mulled/ov-1st-line/reactivate be-pair-lst-wo-marker
                                   (not edit-trailing-edges-p)
                                   keep-offset)))

(defun mulled/ov-1st-line/get-beg-without-padding (ov)
  (+ (overlay-start ov)
     (if (overlay-get ov 'mulled/beg-from-point-min-p) 0 1)))

(defun mulled/ov-1st-line/get-end-without-padding (ov)
  (1- (overlay-end ov)))

(defun mulled/ov-1st-line/get-beg-with-padding (ov)
  (- (overlay-start ov)
     (if (overlay-get ov 'mulled/beg-from-point-min-p) 1 0)))

(defun mulled/ov-1st-line/get-end-with-padding (ov)
  (overlay-end ov))

(defun mulled/ov-1st-line/edit-trailing-edges-p (ov)
  (overlay-get ov 'mulled/edit-trailing-edges-p))


;;  Modification Hooks for a overlay on the 1st line.
;;

(defvar mulled/ov-1st-line/.str-to-be-modified nil)
(make-variable-buffer-local 'mulled/ov-1st-line/.str-to-be-modified)

;; The hook defined below will be called when modifications is
;; occur on the overlay placed over the 1st line of multiple edit.
(defun mulled/ov-1st-line/mod-hook-fn (&rest args)
  (let* ((parent-frame   (backtrace-frame 3))
         (fn-name        (when (symbolp (nth 1 parent-frame))
                           (let ((name (symbol-name (nth 1 parent-frame))))
                             (when (string-match "^ad-Orig-" name)
                               (setq name (replace-match "" nil nil name)))
                             (intern name))))
         (fn-callback-pair (and fn-name
                                (assoc fn-name mulled/special-fn-alist))) ;; **EXPERIMENTAL**
         (orig-fn-args   (nthcdr 2 parent-frame))

         (ov             (nth 0 args))
         (after-p        (nth 1 args))
         (pt-beg         (nth 2 args))
         (pt-end         (nth 3 args))
         (len-removed    (nth 4 args))) ;; Not set in before modification.
    (when (mulled/ov-1st-line/ov-1st-line-p ov) ;; Overlay is not disposed.
      (let* ((lines   (overlay-get ov 'mulled/lines ))
             (col-beg (mulled/lines/col-from-pt-in-nth lines 0 pt-beg))
             (col-end (mulled/lines/col-from-pt-in-nth lines 0 pt-end))
             (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p)))

        (if (not after-p)
            ;; Before modification.
            ;;
            (progn
              ;; Remember the string to be modified
              ;; current operation.
              (setq mulled/ov-1st-line/.str-to-be-modified
                    (buffer-substring pt-beg pt-end))

              (when (not mulled/.running-primitive-undo-p)
                (when (or (mulled/lines/out-of-1st-line-p lines pt-beg)
                          (mulled/lines/out-of-1st-line-p lines pt-end))
                  (mulled/ov-1st-line/dispose ov)
                  (message "[mulled] Multiple line edit exited. (out of range)"))))

          ;; After modification.
          ;;
          (progn
            (if mulled/.running-primitive-undo-p
                (setq mulled/.undo-at (point))
              (let* ((str-maybe-removed mulled/ov-1st-line/.str-to-be-modified)
                     (col-num-removed (with-temp-buffer
                                        (insert str-maybe-removed)
                                        (mulled/col-num-in-region (point-min)
                                                                  (+ (point-min)
                                                                     len-removed))))
                     (out-of-range-p (mulled/lines/out-of-range-op-p lines
                                                                     edit-trailing-edges-p
                                                                     col-beg
                                                                     col-end
                                                                     col-num-removed)))
                (cond
                 ;; Special function
                 ((and mulled/apply-special-fn-to-each-line-p
                       fn-callback-pair)
                  (mulled/lines/mirror-special-fn lines
                                                  edit-trailing-edges-p
                                                  col-beg
                                                  col-end
                                                  col-num-removed
                                                  fn-name
                                                  (cdr fn-callback-pair)
                                                  orig-fn-args))
                 ;; Insertion
                 ((zerop len-removed)
                  (if out-of-range-p
                      (progn
                        (mulled/ov-1st-line/dispose ov)
                        (message "[mulled] Multiple line edit exited. (out of range)"))
                    (mulled/lines/mirror-insert-op lines
                                                   edit-trailing-edges-p
                                                   col-beg
                                                   col-end)))
                 ;; Replacement
                 ((not (= pt-beg pt-end))
                  (mulled/lines/mirror-replace-op lines
                                                  edit-trailing-edges-p
                                                  col-beg
                                                  col-end
                                                  col-num-removed))
                 ;; Deletion
                 (t
                  (let ( ;; May be "C-d" at end of line.
                        (remove-newline-at-eol-p
                         (= pt-end (mulled/ov-1st-line/get-end-with-padding ov)))
                    
                        ;; May be backspace at beginning of line.
                        (backspace-at-bol-p
                         (= pt-end (mulled/ov-1st-line/get-beg-with-padding ov))))
                
                    (if (or out-of-range-p
                            remove-newline-at-eol-p
                            (and backspace-at-bol-p
                                 (not (overlay-get ov 'mulled/beg-from-point-min-p)))) ;;XXX Should be removed?
                        (progn
                          (mulled/ov-1st-line/dispose ov)
                          (message "[mulled] Multiple line edit exited. (out of range)"))
                      (mulled/lines/mirror-delete-op lines
                                                     edit-trailing-edges-p
                                                     col-beg
                                                     col-end
                                                     col-num-removed)))))))
            (setq mulled/ov-1st-line/.str-to-be-modified "")
            (mulled/ov-1st-line/update-cursor-pos ov)))))))

;; To prevent duplication of edit, in the lines next to 1st line,
;; which caused by undo/redo operation, we have to aware if the hook
;; function `mulled/ov-1st-line/mod-hook-fn' is driven by undo/redo or not.
(defadvice primitive-undo (around mulled/undo-hook-fn (n list))
  (setq mulled/.running-primitive-undo-p t)
  (unwind-protect
      ad-do-it
    (setq mulled/.running-primitive-undo-p nil)))
(ad-activate 'primitive-undo)


 
;;; ===========================================================================
;;;
;;;  Lines which should be managed for multiple line edit.
;;;
;;; ===========================================================================

;; Class Variables
;;

;; Constructor.
;;
(defun mulled/lines/new (r-pt-beg r-pt-end edit-trailing-edges-p)
  (mulled/lines/new-by-be-pair-lst (mulled/lines/be-pair-lst/new r-pt-beg r-pt-end)
                                   edit-trailing-edges-p))

(defun mulled/lines/new-by-be-pair-lst (be-pair-lst edit-trailing-edges-p)
  (mulled/lines/be-pair-lst/activate-marker be-pair-lst)
  (let* ((fringe-ov-lst (mulled/lines/init-fringe-ov-lst (list be-pair-lst)
                                                         edit-trailing-edges-p))
         (cursor-ov-lst (mulled/lines/init-cursor-ov-lst (list be-pair-lst)
                                                         edit-trailing-edges-p)))
    (list be-pair-lst fringe-ov-lst cursor-ov-lst)))

;; Initialize indicator icons on fringe of each line.
;;
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'mulled/indicator-left  [#x00 #xcc #x66 #x33 #x33 #x66 #xcc #x00])
  (define-fringe-bitmap 'mulled/indicator-right [#x00 #x33 #x66 #xcc #xcc #x66 #x33 #x00]))

(defun mulled/lines/init-fringe-ov-lst (lines edit-trailing-edges-p)
  (lexical-let (fringe-ov-lst
                (cur-win (selected-window)))
    (when ;; Test if fringe bitmaps is available.
        (and (boundp 'fringe-bitmaps)
             (display-images-p)
             (>= (or left-fringe-width
                     (and cur-win (car (window-fringes cur-win)))
                     0)
                 8)
             (>= (or right-fringe-width
                     (and cur-win (cadr (window-fringes cur-win)))
                     0)
                 8))
      (mulled/lines/map
       lines
       (lambda (nth)
         ;; Copy strings of `indicator-l' and `indicator-r' not to make them
         ;; identical when the code is byte compiled.
         ;; -- This may be a bug of the elisp compiler.
         (let* ((beg (mulled/lines/pt-nth-beg lines nth))
                (end (mulled/lines/pt-nth-end lines nth))
                (indicator-l (copy-sequence (if edit-trailing-edges-p "<" ">")))
                (indicator-r (copy-sequence (if edit-trailing-edges-p "<" ">")))
                (fringe (if edit-trailing-edges-p 'right-fringe 'left-fringe))
                (fringe-bmp (if edit-trailing-edges-p 'mulled/indicator-right
                              'mulled/indicator-left))
                (ov (make-overlay beg
                                  end
                                  nil
                                  nil
                                  t)))
           (when (boundp 'fringe-bitmaps)
             (put-text-property 0 (length indicator-l)
                                'display (list 'left-fringe
                                               fringe-bmp
                                               'mulled/fringe-face)
                                indicator-l)
             (put-text-property 0 (length indicator-r)
                                'display (list 'right-fringe
                                               fringe-bmp
                                               'mulled/fringe-face)
                                indicator-r))
           (overlay-put ov 'before-string indicator-l)
           (overlay-put ov 'after-string indicator-r)
                          
           (overlay-put ov  'priority 100)
                          
           (push ov fringe-ov-lst)))))
    (reverse fringe-ov-lst)))

(defun mulled/lines/init-cursor-ov-lst (lines edit-trailing-edges-p)
  (lexical-let ((cursor-ov-lst nil))
    (mulled/lines/map
     lines
     (lambda (idx)
       (push (let ((ov (make-overlay (point) (point))))
               (overlay-put ov 'priority 1000)
               ov)
             cursor-ov-lst)))
    ;; (mulled/lines/update-cursor-ov-lst-aux lines cursor-ov-lst
    ;;                                        edit-trailing-edges-p)
    cursor-ov-lst))

(defun mulled/lines/dispose (lines)
  ;; Dispose icons on fringe.
  (dolist (ov (mulled/lines/indicator-ov-lst-of lines))
    (delete-overlay ov))
  ;; Dispose cursors on each line.
  (dolist (ov (mulled/lines/cursor-ov-lst-of lines))
    (delete-overlay ov))
  (mulled/lines/be-pair-lst/dispose (mulled/lines/be-pair-lst-of lines)))

(defun mulled/lines/be-pair-lst-of (lines)
  "Fetch list of `beginning-of-line/end-of-line' pairs of each lines."
  (nth 0 lines))

(defun mulled/lines/indicator-ov-lst-of (lines)
  (nth 1 lines))

(defun mulled/lines/cursor-ov-lst-of (lines)
  (nth 2 lines))

(defun mulled/lines/count-of (lines)
  "Total amount of lines targeted for multiple line edit."
  (length (mulled/lines/be-pair-lst-of lines)))

(defun mulled/lines/pt-nth-beg (lines nth)
  "Start point of the `NTH' line."
  (marker-position (car (nth nth (mulled/lines/be-pair-lst-of lines)))))

(defun mulled/lines/pt-nth-end (lines nth)
  "End point of the `NTH' line."
  (marker-position (cdr (nth nth (mulled/lines/be-pair-lst-of lines)))))

(defun mulled/lines/nth-from-pt (lines pt)
  "Returns index of the line in LINES which
contains PT.

Returns nil when PT is outside of LINES."
  (let ((retval nil))
    (mulled/lines/map lines
                      (lambda (nth)
                        (let ((col-num-cur-line (mulled/lines/col-num-nth lines nth)))
                          (when (and (<= (mulled/lines/pt-nth-beg lines nth) pt)
                                     (<= pt (mulled/lines/pt-nth-end lines nth)))
                            (setq retval nth)))))
    retval))

(defun mulled/lines/col-from-pt-in-nth (lines nth pt)
  "Get column at PT in NTH line."
  (let ((pt-beg (mulled/lines/pt-nth-beg lines nth))
        (pt-end (mulled/lines/pt-nth-end lines nth)))
    (when (not (and (<= pt-beg pt)
                    (<= pt pt-end)))
      (error "[mulled/lines/pt-to-col-num-nth] Out of range: nth[%s], pt[%s]."
             nth pt))
    (mulled/col-num-in-region pt-beg pt)))

(defun mulled/lines/pt-from-col-in-nth (lines nth col)
  "Get column at PT in NTH line."
  (mulled/pt-offset-by-col-num (mulled/lines/pt-nth-beg lines nth) col))

(defun mulled/lines/col-num-nth (lines nth)
  "Count number of columns in NTH line."
  (mulled/col-num-in-region (mulled/lines/pt-nth-beg lines nth)
                            (mulled/lines/pt-nth-end lines nth)))

(defun mulled/lines/col-num-max (lines)
  "Count number of columns in the longest line of LINES."
  (let ((retval 0))
  (dotimes (nth (mulled/lines/count-of lines))
    (setq retval (max retval
                      (mulled/lines/col-num-nth lines nth))))
  retval))

(defun mulled/lines/col-num-min (lines)
  "Count number of columns in the shortest line of LINES."
  (let ((retval nil))
  (dotimes (nth (mulled/lines/count-of lines))
    (setq retval (min (or retval
                          (mulled/lines/col-num-nth lines nth))
                      (mulled/lines/col-num-nth lines nth))))
  (or retval 0)))

(defun mulled/lines/map (lines fn)
  "Apply the function `FN' to each line of multiple line edit."
  (loop for i from 0 to (1- (mulled/lines/count-of lines)) do
        (funcall fn i)))

(defun mulled/lines/map-but-1st (lines fn)
  "Apply the function `FN' to each line of multiple line edit, except 1st line."
  (loop for nth from 1 to (1- (mulled/lines/count-of lines)) do ;; skip 1st line.
        (funcall fn nth)))

(defun mulled/lines/out-of-1st-line-p (lines pt)
  "Test if the point is within 1st-line."
  (let ((beg (mulled/lines/pt-nth-beg lines 0))
        (end (mulled/lines/pt-nth-end lines 0)))
    (not (and (<= beg pt)
              (<= pt end)))))

(defun mulled/lines/out-of-range-op-p (lines edit-trailing-edges-p col-beg col-end col-num-removed)
  "Test if the operation, specified by `COL-BEG', `COL-END' and `LEN-REMOVE', is
accepted by each line of multiple line edit."
  (lexical-let* ((retval nil)
                 (col-required
                  ;; Calculate required length of each line.
                  (+ col-num-removed ;; Zero when insertion.
                     (if edit-trailing-edges-p
                         (- (mulled/lines/col-num-nth lines 0) col-end)
                       col-beg))))
    (mulled/lines/map-but-1st lines
                              (lambda (nth)
                                ;; Test if each line has enough length or not.
                                (let ((col-num-cur-line (mulled/lines/col-num-nth lines nth)))
                                  (when (< col-num-cur-line col-required)
                                    (setq retval t)))))
    retval))

(defun mulled/lines/update-cursor-ov-lst (lines edit-trailing-edges-p)
  (mulled/lines/update-cursor-ov-lst-aux
   lines
   (mulled/lines/cursor-ov-lst-of lines)
   edit-trailing-edges-p))

(defun mulled/lines/update-cursor-ov-lst-aux (lines cursor-ov-lst edit-trailing-edges-p)
  (let* ((tail-p           edit-trailing-edges-p)
         (col-1st-line     (mulled/lines/col-from-pt-in-nth
                            lines 0 (point)))
         (col-num-1st-line (mulled/lines/col-num-nth lines 0))
         (offset-1st-line  (cond
                            (tail-p (- col-num-1st-line col-1st-line))
                            (t      col-1st-line)))
         (out-of-range-err (mulled/lines/out-of-range-op-p
                            lines tail-p
                            col-1st-line col-1st-line 0))
         (face             (if out-of-range-err
                               'mulled/out-of-range-cursor-face
                             'mulled/cursor-face))
         (pseudo-cursor    (let ((str (copy-sequence " ")))
                             (put-text-property 0 1 'face face str)
                             str)))
    (mulled/lines/map
     lines
     (lambda (nth)
       (let* ((ov      (nth nth cursor-ov-lst))
              (col-num (mulled/lines/col-num-nth lines nth))
              (col     (cond
                        ;; COL must be in current line.
                        (tail-p (max 0 (- col-num offset-1st-line)))
                        (t      (min offset-1st-line col-num))))
              (pt      (mulled/lines/pt-from-col-in-nth lines nth col)))
         
         ;; Make looking of the pseudo cursor good.
         ;;
         (let ((display       nil)
               (after-str     nil))
           (cond
            ;; Trim "\t" into cursor and spaces.
            ((eq (char-after pt) 09)
             (let ((col-next-char (mulled/lines/col-from-pt-in-nth
                                   lines nth (1+ pt))))
               (setq display   pseudo-cursor)
               (setq after-str (make-string (1- ;; = Exclude cursor width.
                                             (- col-next-char col))
                                            ?\ ))))
            ;; Put cursor over "\n".
            ((eq (char-after pt) 10)
             (setq display   (concat pseudo-cursor))
             (setq after-str "\n"))
            
            ;; Put cursor at EOF.
            ((null (char-after pt))
             (setq after-str (concat pseudo-cursor))))
           
           (overlay-put ov 'display      display)
           (overlay-put ov 'after-string after-str))
         
         (move-overlay ov pt (1+ pt))
         (overlay-put ov 'face face))))
    
    ;; Redisplay pseudo cursors.
    (when (and (not mulled/.running-primitive-undo-p) ;; Inhibit flicker.
               (not executing-kbd-macro))
      (sit-for 0))))

(defun mulled/lines/mirror-insert-op (lines edit-trailing-edges-p col-beg col-end)
  "Reflect input operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((str (buffer-substring (mulled/lines/pt-from-col-in-nth lines 0 col-beg)
                                        (mulled/lines/pt-from-col-in-nth lines 0 col-end)))
                 (col-offset (if edit-trailing-edges-p
                                 ;; Offset from end of line.
                                 (- (mulled/lines/col-num-nth lines 0) col-end)
                               ;; Offset from beginning of line.
                               col-beg)))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st
         lines
         (lambda (nth)
           (goto-char (mulled/lines/pt-from-col-in-nth
                       lines
                       nth
                       (if edit-trailing-edges-p
                           (- (mulled/lines/col-num-nth lines nth)
                              col-offset)
                         col-offset)))
           (insert str)))))))

(defun mulled/lines/mirror-delete-op (lines edit-trailing-edges-p col-beg col-end col-num-removed)
  "Reflect delete operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((col-offset (if edit-trailing-edges-p
                                 ;; Offset from end of line.
                                 (- (mulled/lines/col-num-nth lines 0) col-end)
                               ;; Offset from beginning of line.
                               col-beg)))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st
         lines
         (lambda (nth)
           (if edit-trailing-edges-p
               (let ((pt-beg (mulled/lines/pt-from-col-in-nth
                             lines nth (- (mulled/lines/col-num-nth lines nth)
                                          col-offset
                                          col-num-removed)))
                     (pt-end (mulled/lines/pt-from-col-in-nth
                               lines nth (- (mulled/lines/col-num-nth lines nth)
                                            col-offset))))
                 (delete-region pt-beg pt-end))
             (let ((pt-beg (mulled/lines/pt-from-col-in-nth
                            lines nth col-offset))
                   (pt-end (mulled/lines/pt-from-col-in-nth
                             lines nth (+ col-offset col-num-removed))))
               (delete-region pt-beg pt-end)))))))))

(defun mulled/lines/mirror-replace-op-aux (lines edit-trailing-edges-p col-beg col-end col-num-removed fn)
  (lexical-let* ((edit-trailing-edges-p edit-trailing-edges-p)
                 (col-num-removed col-num-removed)
                 (fn fn)
                 (col-offset (if edit-trailing-edges-p
                                 ;; Offset from end of line.
                                 (- (mulled/lines/col-num-nth lines 0) col-end)
                               ;; Offset from beginning of line.
                               col-beg)))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st
         lines
         (lambda (nth)
           (if edit-trailing-edges-p
               (let ((pt-beg (mulled/lines/pt-from-col-in-nth
                              lines nth (- (mulled/lines/col-num-nth lines nth)
                                           col-offset
                                           col-num-removed)))
                     (pt-end (mulled/lines/pt-from-col-in-nth
                              lines nth (- (mulled/lines/col-num-nth lines nth)
                                           col-offset))))
                 (funcall fn pt-beg pt-end))
             (let ((pt-beg (mulled/lines/pt-from-col-in-nth
                            lines nth col-offset))
                   (pt-end (mulled/lines/pt-from-col-in-nth
                            lines nth (+ col-offset col-num-removed))))
               (funcall fn pt-beg pt-end)))))))))

(defun mulled/lines/mirror-replace-op (lines edit-trailing-edges-p col-beg col-end col-num-removed)
  "Reflect replace operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((str (buffer-substring (mulled/lines/pt-from-col-in-nth lines 0 col-beg)
                                        (mulled/lines/pt-from-col-in-nth lines 0 col-end))))
    (mulled/lines/mirror-replace-op-aux lines edit-trailing-edges-p col-beg col-end col-num-removed
                                        (lambda(pt-beg pt-end)
                                          (delete-region pt-beg pt-end)
                                          (goto-char pt-beg)
                                          (insert str)))))

(defun mulled/lines/mirror-special-fn (lines edit-trailing-edges-p col-beg col-end col-num-removed fn-name callback-fn orig-args)
  "Reflect replace operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((fn-name fn-name)
                 (callback-fn callback-fn)
                 (orig-args orig-args))
    (mulled/lines/mirror-replace-op-aux lines edit-trailing-edges-p col-beg col-end col-num-removed
                                        (lambda (pt-beg pt-end)
                                          (condition-case c
                                              (apply callback-fn pt-beg pt-end orig-args)
                                            (error
                                             (message "[mulled] Error in special function `%s':\n%s"
                                                      fn-name c)))))))

 
;;; ===========================================================================
;;;
;;;  beginning-of-line/end-of-line pairs for each lines.
;;;
;;; ===========================================================================

(defun mulled/lines/be-pair-lst/new (r-pt-beg r-pt-end)
  (let (be-pair-lst)
    (save-excursion
      (goto-char r-pt-beg)
      (dotimes (i (count-lines r-pt-beg r-pt-end))
        (push (cons (progn (beginning-of-line)
                           (point))
                    (progn (end-of-line)
                           (point)))
              be-pair-lst)
        (when (not (= (point-max) (point)))
          (forward-char))))
    (mulled/lines/be-pair-lst/activate-marker (reverse be-pair-lst))))

(defun mulled/lines/be-pair-lst/activate-marker (be-pair-lst)
  (save-excursion
    (dolist (be-pair be-pair-lst)
      (goto-char (car be-pair))
      (setcar be-pair (let ((m (point-marker)))
                        (set-marker-insertion-type m nil)
                        m))
      (goto-char (cdr be-pair))
      (setcdr be-pair (let ((m (point-marker)))
                        (set-marker-insertion-type m t)
                        m))))
  be-pair-lst)

(defun mulled/lines/be-pair-lst/dispose (be-pair-lst)
  ;; Not to make emacs slow, we should discard markers immediately.
  (dolist (be-pair be-pair-lst)
    (and (markerp (car be-pair)) (set-marker (car be-pair) nil))
    (and (markerp (cdr be-pair)) (set-marker (cdr be-pair) nil))))

(defun mulled/lines/be-pair-lst/dup-without-marker (be-pair-lst)
  ;; Copy be-pair-list, with replacing markers with numbers.
  (mapcar (lambda (pair)
            (cons (marker-position (car pair))
                  (marker-position (cdr pair))))
          be-pair-lst))

 
;;; ===========================================================================
;;;
;;; YASnippet support. (* EXPERIMENTAL *)
;;;
;;; ===========================================================================

;;; NOTE
;;
;; 'Cos `YASnippet' prevents `modification-hooks' from running while expanding
;; snippets, multiple line edit won't work with it out of the box.
;;
;; Here we make patches to cope with this restriction.


;; Utility functions.
;;
(defun mulled/mod-hook-fix/run-orig-insert (&rest args)
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if (or (not (mulled/ov-1st-line/ov-1st-line-p ov))
            (and (boundp 'run-orig-insert-fn-p) run-orig-insert-fn-p))
        (apply mulled/mod-hook-fix/orig-insert-fn args)
      (let* ((run-orig-insert-fn-p t) ;; Don't loop eternally.
             (insert-beg (point))
             (retval (progn (mulled/ov-1st-line/mod-hook-fn ov
                                                            nil
                                                            insert-beg
                                                            insert-beg)
                            (apply mulled/mod-hook-fix/orig-insert-fn args)))
             (insert-end (save-restriction (widen) (point))))
        (mulled/ov-1st-line/mod-hook-fn ov
                                        t
                                        insert-beg
                                        insert-end
                                        0)
        retval))))

(defun mulled/mod-hook-fix/run-orig-delete-region (start end)
  (interactive "r")
  (let ((ov (mulled/ov-1st-line/find-at start)))
    (if (or (not (mulled/ov-1st-line/ov-1st-line-p ov))
            (and (boundp 'run-orig-delete-region-fn-p) run-orig-delete-region-fn-p))
        (funcall mulled/mod-hook-fix/orig-delete-region-fn start end)
      (let* ((run-orig-delete-region-fn-p t) ;; Don't loop eternally.
             (remove-beg (save-excursion
                           (goto-char start)
                           (point)))
             (remove-end (save-excursion
                           (goto-char end)
                           (point)))
             (remove-len (- remove-end remove-beg))
             (mulled/ov-1st-line/.str-to-be-modified
              (buffer-substring start end)))
        (prog2
            (mulled/ov-1st-line/mod-hook-fn ov
                                            nil
                                            remove-beg
                                            remove-end)
            (funcall mulled/mod-hook-fix/orig-delete-region-fn start end)
          ;; mirror insertion.
          (mulled/ov-1st-line/mod-hook-fn ov
                                          t
                                          remove-beg
                                          remove-beg
                                          remove-len))))))

(defun mulled/mod-hook-fix/run-orig-replace-match (newtext &optional fixedcase literal string subexp)
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if (or (not (match-string 0))
            (not (mulled/ov-1st-line/ov-1st-line-p ov))
            (and (boundp 'run-orig-delete-region-fn-p) run-orig-delete-region-fn-p))
        (funcall mulled/mod-hook-fix/orig-replace-match-fn newtext fixedcase literal string subexp)
      (let* ((match-beg (save-excursion
                          (goto-char (match-beginning 0))
                          (point-marker)))
             (match-end (save-excursion
                          (goto-char (match-end 0))
                          (point-marker)))
             (len-removed (length (match-string 0))))
        (mulled/ov-1st-line/mod-hook-fn ov
                                        nil
                                        match-beg
                                        match-end)
        (funcall mulled/mod-hook-fix/orig-replace-match-fn newtext fixedcase literal string subexp)
        (mulled/ov-1st-line/mod-hook-fn ov
                                        t
                                        match-beg
                                        match-end
                                        len-removed)))))
  
(defun mulled/mod-hook-fix/run-with-mod-hook-fix (fn)
  ;; NOTE: These three variables are used in dynamic binding context,
  ;;       so just ignore compiler warning `reference to free variable'.
  (let ((mulled/mod-hook-fix/orig-insert-fn        (symbol-function 'insert))
        (mulled/mod-hook-fix/orig-delete-region-fn (symbol-function 'delete-region))
        (mulled/mod-hook-fix/orig-replace-match-fn (symbol-function 'replace-match)))
    (unwind-protect
        (progn
          (setf (symbol-function 'insert)        'mulled/mod-hook-fix/run-orig-insert)
          (setf (symbol-function 'delete-region) 'mulled/mod-hook-fix/run-orig-delete-region)
          (setf (symbol-function 'replace-match) 'mulled/mod-hook-fix/run-orig-replace-match)
          (funcall fn))
      (setf (symbol-function 'insert)        mulled/mod-hook-fix/orig-insert-fn)
      (setf (symbol-function 'delete-region) mulled/mod-hook-fix/orig-delete-region-fn)
      (setf (symbol-function 'replace-match) mulled/mod-hook-fix/orig-replace-match-fn))))


;; YAS support functions.
;;
(defun mulled/experimental/install-yas-support-aux ()
  (remove-hook 'yas/minor-mode-hook 'mulled/experimental/install-yas-support-aux)
  
  ;; When snippets are expanded in 1st line, mirror them to another lines.
  ;;
  (defadvice yas/expand-snippet (after mulled/yas-hook-fn (template &optional start end expand-env))
    (let ((ov (mulled/ov-1st-line/find-at (point))))
      (when (and ov
                 (string= ad-return-value "[yas] snippet expanded."))
        (let* ((remove-at  (cdr (nth 2 buffer-undo-list)))
               (remove-str (car (nth 2 buffer-undo-list)))
               (remove-len (length (car (nth 2 buffer-undo-list))))
               (insert-beg (car (nth 1 buffer-undo-list)))
               (insert-end (cdr (nth 1 buffer-undo-list)))
               (insert-len (- insert-end insert-beg))
               (edit-trailing-edges-p (mulled/ov-1st-line/edit-trailing-edges-p ov)))
          ;; Remove keyword.
          (setq mulled/ov-1st-line/.str-to-be-modified remove-str)
          (mulled/ov-1st-line/mod-hook-fn ov
                                          t
                                          (+ remove-at (if edit-trailing-edges-p insert-len 0))
                                          (+ remove-at (if edit-trailing-edges-p insert-len 0))
                                          remove-len)
          ;; Insert snippet.
          (mulled/ov-1st-line/mod-hook-fn ov
                                          nil
                                          insert-beg
                                          insert-beg)
          (mulled/ov-1st-line/mod-hook-fn ov
                                          t
                                          insert-beg
                                          insert-end
                                          0)))))
  (ad-activate 'yas/expand-snippet)


  ;; When snippets are expanded in 1st line, mirror them to another lines.
  ;;
  (defadvice yas/skip-and-clear (around mulled/yas/skip-and-clear-hook-fn (field))
    (mulled/mod-hook-fix/run-with-mod-hook-fix (lambda () ad-do-it)))
  (ad-activate 'yas/skip-and-clear)


  ;; When modification in a field is mirrored to another fields by YASnippet,
  ;; reflect mirrored modification to another lines.
  (defadvice yas/mirror-update-display (around mulled/yas/mirror-update-display-hook-fn (mirror field))
    (mulled/mod-hook-fix/run-with-mod-hook-fix (lambda () ad-do-it)))


  ;; Give advice to `yas/mirror-update-display' while
  ;; `yas/on-field-overlay-modification' is running.
  (defadvice yas/on-field-overlay-modification (around mulled/turn-on-yas/mirror-update-display  (overlay after? beg end &optional length))
    (ad-enable-advice 'yas/mirror-update-display
                      'around
                      'mulled/yas/mirror-update-display-hook-fn)
    (ad-activate 'yas/mirror-update-display)
    (unwind-protect
        ad-do-it
      (ad-disable-advice 'yas/mirror-update-display
                        'around
                        'mulled/yas/mirror-update-display-hook-fn)
      (ad-activate 'yas/mirror-update-display)))
  (ad-activate 'yas/on-field-overlay-modification))

;; Install YASnippet support.
;;
(defun mulled/experimental/install-yas-support ()
  (if (featurep 'yasnippet)
      (mulled/experimental/install-yas-support-aux)
    (add-hook 'yas/minor-mode-hook
              'mulled/experimental/install-yas-support-aux)))

;;; multiple-line-edit.el ends here
