;;; multiple-line-edit.el --- Edit multiple line at a time.

;; Copyright (C) 2010 K-talo Miyazaki, all rights reserved.

;; Author: K-talo Miyazaki <Keitaro dot Miyazaki at gmail dot com>
;; Created: 10 Oct 2010 PM 02:44 JST
;; Keywords: abbrev convenience emulations wp
;; Revision: $Id: 79304a2f98a444e188742ea3fd279438ef09a85f $
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
;
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
;;      moved to the 1st line of the selected lines.
;;
;;      And then, your modification in the 1st line will be
;;      reflected to another lines automatically.
;;
;;      NOTE: While multiple line edit is active, indicator icons
;;            will be appear on the fringes.
;;
;;           (The icon `>>' will be used for leading edges edit, and
;;            the icon `<<' will be used for trailing edges edit)
;;
;;   4. To finish multiple line edit, type `C-g' or move cursor
;;      to outside of the 1st line.
;;
;; Also check out the customization group
;;
;;   `M-x customize-group RET multiple-line-edit RET'
;;
;;
;; Key map Examples
;; ================
;; (global-set-key [(control c) (<)] 'mulled/edit-trailing-edges)
;; (global-set-key [(control c) (>)] 'mulled/edit-leading-edges)
;;
;;
;; KNOWN PROBLEMS
;; ==============
;; Multiple line edit won't work with commands that sets
;; non-nil value to the variable `inhibit-modification-hooks'
;; like `YASnippet'.
;;
;; We wrote experimental patches to run `YASnippet' with
;; `multiple-line-edit', put a line below in your .emacs startup
;; file if you are interested in.
;;
;;    (mulled/experimental/install-yas-support)
;;


;;; Change Log:

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

(defconst multiple-line-edit/version "1.5")

(eval-when-compile
  (require 'cl)
  (require 'easymenu))

;; Private Variables
;;
(defvar mulled/.running-primitive-undo-p nil)
(defvar mulled/.undo-at nil)

 
;;; ===========================================================================
;;;
;;;  User customizable things.
;;;
;;; ===========================================================================

(defgroup multiple-line-edit nil
  "Multiple line edit"
  :group 'editing)

(defcustom mulled/reactivate-by-undo t
  "Non-nil means reactivate multiple line edit after undo/redo."
  :type 'boolean
  :group 'multiple-line-edit)

(defface mulled/fringe-face
  '((((class color) (background light)) nil)
    (t nil))
  "The face used to display icons on the fringes."
  :group 'multiple-line-edit)

 
;;; ===========================================================================
;;;
;;;  Commands and Menus.
;;;
;;; ===========================================================================

;; Commands
;;
(defun mulled/edit-trailing-edges ()
  "Edit trailing edge of multiple line."
  (interactive)
  (cond
   ((mulled/is-leading-edges-edit-in-progress)
    (mulled/switch-direction))
   ((mulled/is-trailing-edges-edit-in-progress)
    (message "[mulled] Multiple line edit on trailing edge is in progress."))
   ((mulled/lines-selected-p)
    (mulled/ov-1st-line/activate (region-beginning) (region-end) t))
   (t
    (message "[mulled] Select multiple line first."))))

(defun mulled/edit-leading-edges ()
  "Edit leading edge of multiple line."
  (interactive)
  (cond
   ((mulled/is-trailing-edges-edit-in-progress)
    (mulled/switch-direction))
   ((mulled/is-leading-edges-edit-in-progress)
    (message "[mulled] Multiple line edit on leading edge is in progress."))
   ((mulled/lines-selected-p)
    (mulled/ov-1st-line/activate (region-beginning) (region-end) nil))
   (t
    (message "[mulled] Select multiple line first."))))

(defun mulled/switch-direction ()
  "Switch Leading Edges Edit and Trailing Edges Edit."
  (interactive)
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if ov
        (mulled/ov-1st-line/switch-direction ov)
      (message "[mulled] Multiple line edit is not in progress."))))

(defun mulled/abort ()
  "Exit from multiple line edit."
  (interactive)
  (let ((ov (mulled/ov-1st-line/find-at (point))))
    (if ov
        (progn (mulled/ov-1st-line/dispose ov)
               (message "[mulled] Multiple line edit exited."))
      (mulled/force-abort))))

(defun mulled/force-abort ()
  "Force exit from multiple line edit."
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
(defun mulled/ov-1st-line/activate (r-beg r-end edit-trailing-edges-p)
  (mulled/ov-1st-line/activate-aux (mulled/lines/new r-beg r-end edit-trailing-edges-p)
                                   edit-trailing-edges-p))
  
(defun mulled/ov-1st-line/reactivate (be-pair-lst edit-trailing-edges-p)
  (mulled/ov-1st-line/activate-aux (mulled/lines/new-by-be-pair-lst be-pair-lst
                                                                    edit-trailing-edges-p)
                                   edit-trailing-edges-p))

(defun mulled/ov-1st-line/activate-aux (lines edit-trailing-edges-p)
  ;; To detect operations listed below, we put overlay
  ;; with extra padding around the line.
  ;;
  ;;   1. `delete-backward-char' at leading edge of the line.
  ;;   2. `delete-char' at trailing edge of the line.
  ;;
  (let* ((beg (mulled/lines/nth-beg lines 0))
         (end (mulled/lines/nth-end lines 0))
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
                         ;; When the cursor went out of 1st line,
                         ;; stop multiple line edit.
                         (when (and (eq (current-buffer)
                                        ov-buf)
                                    (not (and (<= ov-beg cur-pt)
                                              (<= cur-pt ov-end))))
                           (mulled/ov-1st-line/dispose ov)
                           (message "[mulled] Multiple line edit exited."))))
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
    (goto-char beg)
    (if edit-trailing-edges-p
        (end-of-line)
      (beginning-of-line))

    ;; Deactivate selection.
    (setq mark-active nil)
    
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
    (setq mulled/.undo-at nil)))


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

(defun mulled/ov-1st-line/switch-direction (ov)
  (let* ((lines (overlay-get ov 'mulled/lines))
         (beg (mulled/lines/nth-beg lines 0))
         (end (mulled/lines/nth-end lines (1- (mulled/lines/count-of lines))))
         (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p)))
    (mulled/ov-1st-line/dispose ov)
    (mulled/ov-1st-line/activate beg end (not edit-trailing-edges-p))))

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

;; The hook defined below will be called when modifications is
;; occur on the overlay placed over the 1st line of multiple edit.
(defun mulled/ov-1st-line/mod-hook-fn (&rest args)
  (let* ((ov             (nth 0 args))
         (after-p        (nth 1 args))
         (beg            (nth 2 args))
         (end            (nth 3 args))
         (len-removed    (nth 4 args)) ;; Not set in before modification.
         (lines       (overlay-get ov 'mulled/lines ))
         (edit-trailing-edges-p (overlay-get ov 'mulled/edit-trailing-edges-p)))
    (when (mulled/ov-1st-line/ov-1st-line-p ov) ;; Overlay is not disposed.
      (if (not after-p)
          ;; Before modification.
          (when (not mulled/.running-primitive-undo-p)
            (when (or (mulled/lines/out-of-1st-line-p lines beg)
                      (mulled/lines/out-of-1st-line-p lines end))
              (mulled/ov-1st-line/dispose ov)
              (message "[mulled] Multiple line edit exited. (out of range)")))
        ;; After modification.
        (if mulled/.running-primitive-undo-p
            (setq mulled/.undo-at (point))
          (let* ((out-of-range-p (mulled/lines/out-of-range-op-p lines
                                                                 edit-trailing-edges-p
                                                                 beg
                                                                 end
                                                                 len-removed)))
            (cond
             ;; Insertion
             ((zerop len-removed)
              (if out-of-range-p
                  (progn
                    (mulled/ov-1st-line/dispose ov)
                    (message "[mulled] Multiple line edit exited. (out of range)"))
                (mulled/lines/mirror-insert-op lines edit-trailing-edges-p beg end)))
             ;; Replacement
             ((not (= beg end))
              (mulled/lines/mirror-replace-op lines edit-trailing-edges-p beg end len-removed))
             ;; Deletion
             (t
              (let (;; May be "C-d" at end of line.
                    (remove-newline-at-eol-p
                     (= end (mulled/ov-1st-line/get-end-with-padding ov)))
                    
                    ;; May be backspace at beginning of line.
                    (backspace-at-bol-p
                     (= end (mulled/ov-1st-line/get-beg-with-padding ov))))
                
                (if (or out-of-range-p
                        remove-newline-at-eol-p
                        (and backspace-at-bol-p
                             (not (overlay-get ov 'mulled/beg-from-point-min-p)))) ;;XXX Should be removed?
                    (progn
                      (mulled/ov-1st-line/dispose ov)
                      (message "[mulled] Multiple line edit exited. (out of range)"))
                  (mulled/lines/mirror-delete-op lines edit-trailing-edges-p beg end len-removed)))))))))))

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
(defun mulled/lines/new (r-beg r-end edit-trailing-edges-p)
  (mulled/lines/new-by-be-pair-lst (mulled/lines/be-pair-lst/new r-beg r-end)
                                   edit-trailing-edges-p))

(defun mulled/lines/new-by-be-pair-lst (be-pair-lst edit-trailing-edges-p)
  (mulled/lines/be-pair-lst/activate-marker be-pair-lst)
  (let* ((fringe-ov-lst (mulled/lines/init-fringe-ov-lst (list be-pair-lst)
                                                         edit-trailing-edges-p)))
    (list be-pair-lst fringe-ov-lst)))

;; Initialize indicator icons on fringe of each line.
;;
(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'mulled/indicator-left  [#x00 #xcc #x66 #x33 #x33 #x66 #xcc #x00])
  (define-fringe-bitmap 'mulled/indicator-right [#x00 #x33 #x66 #xcc #xcc #x66 #x33 #x00]))

(defun mulled/lines/init-fringe-ov-lst (lines edit-trailing-edges-p)
  (lexical-let (fringe-ov-lst)
    (mulled/lines/map lines
                      (lambda (beg end)
                        (let* ((indicator-l (if edit-trailing-edges-p "<" ">"))
                               (indicator-r (if edit-trailing-edges-p "<" ">"))
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
                          
                          (push ov fringe-ov-lst))))
    (reverse fringe-ov-lst)))

(defun mulled/lines/dispose (lines)
  ;; Dispose icons on fringe.
  (dolist (ov (mulled/lines/indicator-ov-lst-of lines))
    (delete-overlay ov))
  (mulled/lines/be-pair-lst/dispose (mulled/lines/be-pair-lst-of lines)))

(defun mulled/lines/be-pair-lst-of (lines)
  "Fetch list of `beginning-of-line/end-of-line' pairs of each lines."
  (nth 0 lines))

(defun mulled/lines/indicator-ov-lst-of (lines)
  (nth 1 lines))

(defun mulled/lines/count-of (lines)
  "Total amount of lines targeted for multiple line edit."
  (length (mulled/lines/be-pair-lst-of lines)))

(defun mulled/lines/nth-beg (lines nth)
  "Start point of the `NTH' line."
  (marker-position (car (nth nth (mulled/lines/be-pair-lst-of lines)))))

(defun mulled/lines/nth-end (lines nth)
  "End point of the `NTH' line."
  (marker-position (cdr (nth nth (mulled/lines/be-pair-lst-of lines)))))

(defun mulled/lines/map (lines fn)
  "Apply the function `FN' to each line of multiple line edit."
  (loop for i from 0 to (1- (mulled/lines/count-of lines)) do
        (funcall fn (mulled/lines/nth-beg lines i) (mulled/lines/nth-end lines i))))

(defun mulled/lines/map-but-1st (lines fn)
  "Apply the function `FN' to each line of multiple line edit, except 1st line."
  (loop for i from 1 to (1- (mulled/lines/count-of lines)) do ;; skip 1st line.
        (funcall fn (mulled/lines/nth-beg lines i) (mulled/lines/nth-end lines i))))

(defun mulled/lines/out-of-1st-line-p (lines pt)
  "Test if the point is within 1st-line."
  (let ((beg (mulled/lines/nth-beg lines 0))
        (end (mulled/lines/nth-end lines 0)))
    (not (and (<= beg pt)
              (<= pt end)))))

(defun mulled/lines/out-of-range-op-p (lines edit-trailing-edges-p op-beg op-end len-removed)
  "Test if the operation, specified by `OP-BEG', `OP-END' and `LEN-REMOVE', is
accepted by each line of multiple line edit."
  (lexical-let* ((retval nil)
                 (len-required
                  ;; Calculate required length of each line.
                  (+ len-removed ;; Zero when insertion.
                     (if edit-trailing-edges-p
                         (- (mulled/lines/nth-end lines 0) op-end)
                       (- op-beg (mulled/lines/nth-beg lines 0))))))
    (mulled/lines/map-but-1st lines
                              (lambda (beg end)
                                ;; Test if each line has enough length or not.
                                (let ((len-cur-line (- end beg)))
                                  (when (< len-cur-line len-required)
                                    (setq retval t)))))
    retval))

(defun mulled/lines/mirror-insert-op (lines edit-trailing-edges-p op-beg op-end)
  "Reflect input operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((str (buffer-substring op-beg op-end))
                 (offset (if edit-trailing-edges-p
                             ;; Offset from end of line.
                             (- (mulled/lines/nth-end lines 0) op-end)
                           ;; Offset from beginning of line.
                           (- op-beg (mulled/lines/nth-beg lines 0)))))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st lines
                                  (lambda (beg end)
                                    (goto-char (if edit-trailing-edges-p
                                                   (- end offset)
                                                 (+ beg offset)))
                                    (insert str)))))))

(defun mulled/lines/mirror-delete-op (lines edit-trailing-edges-p op-beg op-end len-removed)
  "Reflect delete operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((1st-pair (car lines))
                 (offset (if edit-trailing-edges-p
                             ;; Offset from end of line.
                             (- (mulled/lines/nth-end lines 0) op-end)
                           ;; Offset from beginning of line.
                           (- op-beg (mulled/lines/nth-beg lines 0)))))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st lines
                                  (lambda (beg end)
                                    (if edit-trailing-edges-p
                                        (delete-region (- end offset len-removed)
                                                       (- end offset))
                                      (delete-region (+ beg offset)
                                                     (+ beg offset len-removed)))))))))

(defun mulled/lines/mirror-replace-op (lines edit-trailing-edges-p op-beg op-end len-removed)
  "Reflect replace operation, which is occurred in 1st line, to another lines."
  (lexical-let* ((str (buffer-substring op-beg op-end))
                 (1st-pair (car lines))
                 (offset (if edit-trailing-edges-p
                             ;; Offset from end of line.
                             (- (mulled/lines/nth-end lines 0) op-end)
                           ;; Offset from beginning of line.
                           (- op-beg (mulled/lines/nth-beg lines 0)))))
    (save-excursion
      (save-restriction
        (widen)
        (mulled/lines/map-but-1st lines
                                  (lambda (beg end)
                                    (if edit-trailing-edges-p
                                        (let ((beg (- end offset len-removed))
                                              (end (- end offset)))
                                          (delete-region beg end)
                                          (goto-char beg)
                                          (insert str))
                                      (let ((beg (+ beg offset))
                                            (end (+ beg offset len-removed)))
                                        (delete-region beg end)
                                        (goto-char beg)
                                        (insert str)))))))))

 
;;; ===========================================================================
;;;
;;;  beginning-of-line/end-of-line pairs for each lines.
;;;
;;; ===========================================================================

(defun mulled/lines/be-pair-lst/new (r-beg r-end)
  (let (be-pair-lst)
    (save-excursion
      (goto-char r-beg)
      (dotimes (i (count-lines r-beg r-end))
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

(defun mulled/experimental/install-yas-support-aux ()
  (remove-hook 'yas/minor-mode-hook 'mulled/experimental/install-yas-support-aux)
  
  ;; When snippets are expanded in 1st line, mirror them to another lines.
  ;;
  (defadvice yas/expand-snippet (after mulled/yas-hook-fn (template &optional start end expand-env))
    (let ((ov (mulled/ov-1st-line/find-at (point))))
      (when (and ov
                 (string= ad-return-value "[yas] snippet expanded."))
        (let* ((remove-at (cdr (nth 2 buffer-undo-list)))
               (remove-len (length (car (nth 2 buffer-undo-list))))
               (insert-beg (car (nth 1 buffer-undo-list)))
               (insert-end (cdr (nth 1 buffer-undo-list)))
               (insert-len (- insert-end insert-beg))
               (edit-trailing-edges-p (mulled/ov-1st-line/edit-trailing-edges-p ov)))
          ;; Remove keyword.
          (mulled/ov-1st-line/mod-hook-fn ov
                                          t
                                          (+ remove-at (if edit-trailing-edges-p insert-len 0))
                                          (+ remove-at (if edit-trailing-edges-p insert-len 0))
                                          remove-len)
          ;; Insert snippet.
          (mulled/ov-1st-line/mod-hook-fn ov
                                          t
                                          insert-beg
                                          insert-end
                                          0)))))
  (ad-activate 'yas/expand-snippet)
  

  ;; When snippets are expanded in 1st line, mirror them to another lines.
  ;;
  (defadvice yas/skip-and-clear (around mulled/yas/skip-and-clear-hook-fn (field))
    (lexical-let ((ov (mulled/ov-1st-line/find-at (point)))
                  (orig-delete-region-fn (symbol-function 'delete-region)))
      (if ov
          (unwind-protect
              (progn
                (setf (symbol-function 'delete-region)
                      (lambda (beg end)
                        (setf (symbol-function 'delete-region) orig-delete-region-fn)

                        (let ((beg (+ 0 beg))
                              (end (+ 0 end)))
                          (funcall orig-delete-region-fn beg end)
                          (mulled/ov-1st-line/mod-hook-fn ov
                                                          t
                                                          beg
                                                          beg
                                                          (- end beg)))))
                ad-do-it)
            (setf (symbol-function 'delete-region) orig-delete-region-fn))
        ad-do-it)))
  (ad-activate 'yas/skip-and-clear)



  ;; When modification in a field is mirrored to another fields by YASnippet,
  ;; reflect mirrored modification to another lines.
  (defadvice yas/mirror-update-display (around mulled/yas/mirror-update-display-hook-fn (mirror field))
    (lexical-let ((ov (mulled/ov-1st-line/find-at (point)))
                  (orig-insert-fn (symbol-function 'insert))
                  (orig-delete-region-fn (symbol-function 'delete-region)))
      (if ov
          (unwind-protect
              (progn
                (setf (symbol-function 'insert)
                      (lambda (&rest args)
                        (setf (symbol-function 'insert) orig-insert-fn)
                        (let ((beg (point))
                              (end (progn (apply orig-insert-fn args) (point))))
                          (mulled/ov-1st-line/mod-hook-fn ov
                                                          t
                                                          beg
                                                          end
                                                          0))))
                (setf (symbol-function 'delete-region)
                      (lambda (beg end)
                        (setf (symbol-function 'delete-region) orig-delete-region-fn)
                        (let ((beg (+ 0 beg))
                              (end (+ 0 end)))
                          (funcall orig-delete-region-fn beg end)
                          (mulled/ov-1st-line/mod-hook-fn ov
                                                          t
                                                          beg
                                                          beg
                                                          (- end beg)))))
                ad-do-it)
            (setf (symbol-function 'insert) orig-insert-fn)
            (setf (symbol-function 'delete-region) orig-delete-region-fn))
        ad-do-it)))

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
