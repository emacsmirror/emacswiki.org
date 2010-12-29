;;; mouse3.el --- Variable behavior for `mouse-3' second click at same spot.
;; 
;; Filename: mouse3.el
;; Description: Variable behavior for `mouse-3' second click at same spot.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2010, Drew Adams, all rights reserved.
;; Created: Tue Nov 30 15:22:56 2010 (-0800)
;; Version: 
;; Last-Updated: Tue Dec 28 12:56:53 2010 (-0800)
;;           By: dradams
;;     Update #: 263
;; URL: http://www.emacswiki.org/cgi-bin/wiki/mouse3.el
;; Keywords: mouse menu kill rectangle region
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; This library redefines standard command `mouse-save-then-kill'.
;; The only change affects a second `mouse-3' click at the same spot.
;; Instead of this behavior being hard-wired to kill or delete the
;; region, it is variable.
;;
;; You can use option `mouse3-second-click-default-command' to
;; customize this behavior to either pop up a menu or invoke a command
;; you choose.  To obtain the standard behavior, just customize the
;; value to command `mouse3-kill/delete-region'.
;;
;; If you choose to pop up a menu, then you can use option
;; `mouse3-region-popup-submenus' to customize that menu.
;;
;; If you choose to pop up a menu, you can double-click `mouse-3' to
;; kill or delete the selected text.  In other words, in this case you
;; have two possibilities:
;;
;;  single-click: Pop up a menu and choose how to act on the seletion.
;;  double-click: Kill or delete the selection, according to option
;;                `mouse-drag-copy-region'.
;;
;;
;; User options defined here:
;;
;;   `mouse3-region-popup-submenus',
;;   `mouse3-second-click-default-command'.
;;
;; Commands defined here:
;;
;;   `mouse3-dired-flag-region-files-for-deletion',
;;   `mouse3-dired-mark-region-files', `mouse3-dired-region-menu',
;;   `mouse3-dired-toggle-marks-in-region',
;;   `mouse3-dired-unmark-region-files', `mouse3-dired-use-menu',
;;   `mouse3-dired-use-toggle-marks', `mouse3-kill/delete-region',
;;   `mouse3-region-popup-menu'.
;;   
;; Non-interactive functions defined here:
;;
;;   `mouse3-second-click-command'.
;;
;; Internal variables defined here:
;;
;;   `mouse3-save-then-kill-command'.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-save-then-kill' - Uses `mouse3-second-click-command' to
;;                           define second `mouse-3' click behavior.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;;
;; 2010/12/28 dadams
;;     mouse3-region-popup-menu: Name the menu Selection, not Region.
;;     mouse3-dired-region-menu: Name the menu Selected Files, not Files in Region.
;;     mouse3-dired-use-*: Added doc string.
;; 2010/12/02 dadams
;;     Removed: mouse3-org-region-menu, mouse3-picture-rectangle-menu.
;;     Mode add-hook's: Add mode-specific submenu to mouse3-region-popup-submenus, instead of
;;       setting a local value for mouse3-save-then-kill-command.
;;     mouse-save-then-kill: Emacs 20/21 fix: Always copy as kill (no mouse-drag-copy-region).
;; 2010/11/30 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Quiet the byte-compiler.
(defvar picture-killed-rectangle)
(defvar mouse-drag-copy-region)         ; For Emacs < 22.

(defvar mouse3-save-then-kill-command nil
  "Command used for a 2nd `mouse-3' click at the same location, or nil.
The command must accept 2 args: mouse click event and prefix arg.
If non-nil, the command is used in priority over the value of user
option `mouse3-second-click-default-command'.  If nil, this var has no
effect on `mouse-3' behavior.")

(defcustom mouse3-second-click-default-command 'mouse3-region-popup-menu
  "Command used for a second `mouse-3' click at the same location.
The command must accept 2 args: mouse click event and prefix arg.
This is a default value, which can be programmatically overridden in
various contexts.  This option is used only if variable
`mouse3-save-then-kill-command' is nil.

Two particular values:
 `mouse3-region-popup-menu':  Pop up a menu of actions on the region.
 `mouse3-kill/delete-region': Kill or delete the region, according to
                              `mouse-drag-copy-region'."
  :type '(choice
          (const :tag "Menu"
           :value mouse3-region-popup-menu)
          (const :tag "Kill/delete, per `mouse-drag-copy-region'"
           :value mouse3-kill/delete-region)
          (restricted-sexp :tag "Other Command (two args)"
           :match-alternatives (commandp)))
  :set (lambda (symbol value)
         (set symbol value)
         (if (eq value 'mouse3-region-popup-menu)
             (global-set-key [double-mouse-3] 'mouse3-kill/delete-region)
           (global-set-key [double-mouse-3] nil)))
  :initialize 'custom-initialize-set
  :group 'mouse)

(defcustom mouse3-region-popup-submenus
  `(("Remove/Replace"
     ,@`(("Kill"                                . kill-region)
         ("Delete"                              . delete-region)
         ("Yank (Replace)"                      . (lambda (start end)
                                                    "Replace selected text by last text killed."
                                                    (interactive "r")
                                                    (when (string=
                                                           (buffer-substring-no-properties
                                                            (point) (mark))
                                                           (car kill-ring))
                                                      (current-kill 1))
                                                    (delete-region start end)
                                                    (yank)))
         ("--")
         ("Kill Rectangle"                      . kill-rectangle)
         ("Delete Rectangle"                    . delete-rectangle)
         ,@`,(and (boundp 'killed-rectangle) killed-rectangle
                  '(("Yank Rectangle (Replace)"
                     . (lambda (start end)
                         "Replace the selected rectangle by the last rectangle killed."
                         (interactive "r")
                         (delete-rectangle start end)
                         (exchange-point-and-mark)
                         (yank-rectangle)))))
         ("Clear Rectangle (Replace)"           . clear-rectangle)
         ("String Rectangle (Replace)"          . string-rectangle)
         ("Replace Rectangle from Register"
          . (lambda (start end)
              "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
              (interactive "r")
              (kill-rectangle start end)
              (exchange-point-and-mark)
              (condition-case nil
                  (call-interactively #'insert-register)
                (error (exchange-point-and-mark)
                       (yank-rectangle)))))))
    ("Copy"
     ("Copy as Kill"                            . kill-ring-save)
     ("Copy to Register"                        . copy-to-register)
     ("--")
     ("Copy Rectangle to Register"              . copy-rectangle-to-register))
    ("To Register"
     ("Copy"                                    . copy-to-register)
     ("Delete"
      . (lambda (register start end)
          "Delete the selected text, and copy it to a register you name."
          (interactive "cDelete region to register: \nr")
          (copy-to-register register start end t)))
     ("Append"                                  . append-to-register)
     ("Prepend"                                 . prepend-to-register)
     ("--")
     ("Copy Rectangle"                          . copy-rectangle-to-register)
     ("Delete Rectangle"
      . (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t))))
    ("Rectangle"
     ("Kill"                                    . kill-rectangle)
     ("Delete"                                  . delete-rectangle)
     ("Open"                                    . open-rectangle)
     ,@`,(and (boundp 'killed-rectangle) killed-rectangle
              '(("Yank (Replace)"
                 . (lambda (start end)
                     "Replace the selected rectangle by the last rectangle killed."
                     (interactive "r")
                     (delete-rectangle start end)
                     (exchange-point-and-mark)
                     (yank-rectangle)))))
     ("Clear (Replace)"                         . clear-rectangle)
     ("String (Replace)"                        . string-rectangle)
     ,@`,(and (fboundp 'delimit-columns-rectangle) ; Emacs 21+.
              '(("Delimit Columns"              . delimit-columns-rectangle)))
     ("--")
     ("Delete to Register"
      . (lambda (register start end)
          "Delete the selected rectangle, and copy it to a register you name."
          (interactive "cDelete rectangle to register: \nr")
          (copy-rectangle-to-register register start end t)))
     ("Replace from Register"
      . (lambda (start end)
          "Replace the selected rectangle by the contents of a register you name.
Note that the rectangle currently selected is first killed.  You can
restore it by yanking."
          (interactive "r")
          (kill-rectangle start end)
          (exchange-point-and-mark)
          (condition-case nil
              (call-interactively #'insert-register)
            (error (exchange-point-and-mark)
                   (yank-rectangle)))))
     ("Copy to Register"                        . copy-rectangle-to-register))
    ("Change Text"
     ,@`,(and (fboundp 'boxquote-region) ; Defined in `boxquote.el'.
              '(("Boxquote"                     . boxquote-region)))
     ,@`,(and (fboundp 'boxquote-unbox-region) ; Defined in `boxquote.el'.
              '(("Unboxquote"                   . boxquote-unbox-region)))
     ,@`,(and (fboundp 'delimit-columns-rectangle) ; Emacs 21+.
              '(("Delimit Columns"              . delimit-columns-region)))
     ,@`,(if (fboundp 'comment-or-uncomment-region)
             '(("Comment/Uncomment"             . comment-or-uncomment-region))
             '(("Comment"                       . comment-region)
               ("Uncomment"                     . uncomment-region)))
     ("--")
     ("Fill"                                    . fill-region)
     ("Fill as Paragraph"                       . fill-region-as-paragraph)
     ("Canonically Space"                       . canonically-space-region)
     ("Indent"                                  . indent-region)
     ("--")
     ("Capitalize"                              . capitalize-region)
     ("Upcase"                                  . upcase-region)
     ("Downcase"                                . downcase-region)
     ,@`,(and (fboundp 'unaccent-region)
              '(("Remove Accents"               . unaccent-region)))
     ("--")
     ("Center"                                  . center-region)
     ("Reverse Line Order"                      . reverse-region))
    ("Check, Correct, Convert"
     ("Ispell"                                  . ispell-region)
     ("Flyspell"                                . flyspell-region)
     ,@`,(and (fboundp 'whitespace-cleanup-region)
              '(("Check Whitespace"             . whitespace-report-region)
                ("Clean Up Whitespace"          . whitespace-cleanup-region)))
     ("Printify"                                . printify-region)
     ("PR Printify"                             . pr-printify-region)
     ("Compose Characters"                      . compose-region)
     ("Decompose Characters"                    . decompose-region)
     ("--")
     ("Encode using Coding System"              . encode-coding-region)
     ("Decode using Coding System"              . decode-coding-region)
     ("Encode using Format"                     . format-encode-region)
     ("Decode using Format"                     . format-decode-region)
     ,@`,(and (fboundp 'yenc-decode-region)
              '(("Decode Yenc"                  . yenc-decode-region)))
     ("--")
     ("EPA Encrypt"                             . epa-encrypt-region)
     ("EPA Decrypt"                             . epa-decrypt-region)
     ("PGG Encrypt"                             . pgg-encrypt-region)
     ("PGG Decrypt"                             . pgg-decrypt-region))
    ,@(and (fboundp 'hlt-highlight-region) ; Defined in `highlight.el'.
           '(("Highlight"
              ("Highlight"                      . hlt-highlight-region)
              ("Highlight Regexp"               . hlt-highlight-regexp-region)
              ("Unhighlight"                    . hlt-unhighlight-region)
              ("Unhighlight for Face"           . hlt-unhighlight-region-for-face))))
    ("Print"
     ("PostScript Print"                        . ps-print-region)
     ("PostScript Print with Faces"             . ps-print-region-with-faces)
     ("PostScript Preview"                      . pr-ps-region-preview)
     ("PostScript Number of Pages"              . ps-nb-pages-region)
     ("--")
     ("Print to Text Printer"                   . pr-txt-region)
     ("Print to Text Printer (`lpr')"           . lpr-region)
     ("Print with Paging (`pr')"                . print-region)
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("--")
                ("BNF PostScript Analyze"       . ebnf-syntax-region)))
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("BNF PostScript Print "        . ebnf-print-region)))
     ,@`,(and (fboundp 'ebnf-print-region) ; Defined in `ebnf2ps.el'.
              '(("BNF PostScript Save"          . ebnf-eps-region))))
    ("Count"
     ,@`,(and (fboundp 'region-length)
              '(("Characters"                   . region-length)))
     ("Words"                                   . count-words-region)
     ("Lines"                                   . count-lines-region))
    ("Misc"
     ("Narrow"                                  . narrow-to-region)
     ("Eval"                                    . eval-region)
     ,@`,(and last-kbd-macro
              '(("Key-Macro on Region Lines"    . apply-macro-to-region-lines)))
     ("Shell Command"                           . shell-command-on-region)
     ("Write to File"                           . write-region)
     ,@`,(and (fboundp 'bmkp-set-autonamed-regexp-region) ; Defined in `bookmark+-1.el'.
              '(("Create Bookmarks Matching"    . bmkp-set-autonamed-regexp-region)))
     ,@`,(and (fboundp 'bmkp-light-bookmarks-in-region) ; Defined in `bookmark+-lit.el'.
              '(("Highlight Bookmarks"          . bmkp-light-bookmarks-in-region)))
     ,@`,(and (fboundp 'browse-url-of-region) ; Defined in `browse-url.el'.
              '(("Open in Browser"              . browse-url-of-region)))))
  "Submenus of popup menu for `mouse-3' to act on the selected text."
  :type '(alist
          :key-type   (string   :tag "Submenu Name")
          :value-type (repeat
                       (choice
                        (cons :tag "Item"
                         (string :tag "Name")
                         ;; This is more correct but gives `mismatch' in Emacs < version 24:
                         ;; (restricted-sexp :tag "Command" :match-alternatives (commandp))
                         (sexp :tag "Command"))
                        (list :tag "Separator" (const "--")))))
  :group 'mouse)

(defun mouse3-second-click-command ()
  "Command used for a second `mouse-3' click at the same location.
The command must accept 2 args: mouse click event and prefix arg.
Return the value of `mouse3-save-then-kill-command' if non-nil, else
return the value of `mouse3-second-click-default-command'."
  (or mouse3-save-then-kill-command
      mouse3-second-click-default-command
      'mouse3-kill/delete-region))

(defun mouse3-kill/delete-region (event killp)
  "Delete the active region.  Kill it if KILLP is non-nil.
Kill it anyway if `mouse-drag-copy-region' is non-nil.
For Emacs prior to Emacs 22, always kill region."
  (interactive "e\nP")
  (let* ((posn         (event-start event))
         (window       (posn-window posn))
         (buf          (window-buffer window))
         (mark-active  t))              ; Just to be sure.
    (with-current-buffer buf
      (if (or killp (and (boundp 'mouse-drag-copy-region) mouse-drag-copy-region))
          (kill-region (region-beginning) (region-end))
        (delete-region (region-beginning) (region-end))))))

(defun mouse3-region-popup-menu (event prefix)
  "Pop up a menu of actions for the selected text."
  (interactive "e\nP")
  (sit-for 0)
  (let ((selection  (x-popup-menu event `("Selection" ,@mouse3-region-popup-submenus))))
    (and selection (call-interactively selection))))


;; REPLACE ORIGINAL in `mouse.el'.
;;
;; Use `mouse3-second-click-command' to determine the action for a second `mouse-3' click.
;;
(defun mouse-save-then-kill (click &optional prefix)
  "Like vanilla `mouse-save-then-kill', but uses `mouse3-second-click-command'."
  (interactive "e\nP")
  (mouse-minibuffer-check click)
  (let* ((posn          (event-start click))
         (click-pt      (posn-point posn))
         (window        (posn-window posn))
         (buf           (window-buffer window))
         (this-command  this-command)   ; Don't let subsequent kill command append to this one.
         ;; Check whether the user has multi-clicked to select words/lines.
         (click-count   (if (and (eq mouse-selection-click-count-buffer buf)
                                 (with-current-buffer buf (mark t)))
                            mouse-selection-click-count
                          0)))
    (cond ((not (numberp click-pt)) nil)
          ((and (eq last-command 'mouse-save-then-kill) ; User clicked without moving point.
                (eq click-pt mouse-save-then-kill-posn)
                (eq window (selected-window)))
           (funcall (mouse3-second-click-command) click prefix)
           (setq mouse-selection-click-count  0
                 mouse-save-then-kill-posn    nil))
          ;; If there is a suitable region, adjust it by moving the closest end to CLICK-PT.
          ((or (with-current-buffer buf (and transient-mark-mode mark-active))
               (and (eq window (selected-window))
                    (mark t)
                    (or (and (eq last-command 'mouse-save-then-kill)
                             mouse-save-then-kill-posn)
                        (and (memq last-command '(mouse-drag-region mouse-set-region))
                             (or mark-even-if-inactive (not transient-mark-mode))))))
           (select-window window)
           (let* ((range  (mouse-start-end click-pt click-pt click-count)))
             (if (< (abs (- click-pt (mark t))) (abs (- click-pt (point))))
                 (set-mark (car range))
               (goto-char (nth 1 range)))
             (setq deactivate-mark  nil)
             (mouse-set-region-1)
             ;; Previous region was copied to kill-ring, so replace with adjusted region.
             (if (boundp 'mouse-drag-copy-region)
                 (when mouse-drag-copy-region ; Emacs 22+.
                   (kill-new (filter-buffer-substring (mark t) (point)) t))
               (kill-new (buffer-substring (point) (mark t)) t)) ; Emacs 20 & 21.
             (setq mouse-save-then-kill-posn  click-pt))) ; Repeated `mouse-3' kills the region.
          (t                            ; Set the mark where point is and move to CLICK-PT.
           (select-window window)
           (mouse-set-mark-fast click)
           (let ((before-scroll (with-current-buffer buf point-before-scroll)))
             (when before-scroll (goto-char before-scroll)))
           (exchange-point-and-mark)
           (mouse-set-region-1)
           ;; Previous region was copied to kill-ring, so replace with adjusted region.
           (if (boundp 'mouse-drag-copy-region)
               (when mouse-drag-copy-region ; Emacs 22+.
                 (kill-new (filter-buffer-substring (mark t) (point))))
             (kill-new (buffer-substring (point) (mark t)))) ; Emacs 20 & 21.
           (setq mouse-save-then-kill-posn  click-pt)))))
 
;;; Behavior for particular modes.

;;; Dired mode.
(defun mouse3-dired-region-menu (event prefix)
  "Popup menu for Dired mode, to act on the selected files."
  (interactive "e\nP")
  (sit-for 0)
  (let ((selection
         (x-popup-menu
          event
          (list
           "Selected Files"
           '(""
             ("Mark" . mouse3-dired-mark-region-files)
             ("Unmark" . mouse3-dired-unmark-region-files)
             ("Toggle Marked/Unmarked" . mouse3-dired-toggle-marks-in-region)
             ("Flag for Deletion" . mouse3-dired-flag-region-files-for-deletion))))))
    (and selection (call-interactively selection))))

(defun mouse3-dired-use-menu ()
  "Make a second `mouse-3' click at the same place pop up a menu in Dired."
  (interactive)
  ;; The `define-key's are not needed unless you use `dired+.el'.
  ;; In that case, this overrides the Dired+ behavior, which is a more complex menu.
  (when (featurep 'dired+)
    (define-key dired-mode-map [down-mouse-3] nil)
    (define-key dired-mode-map [mouse-3] nil))
  (remove-hook 'dired-mode-hook
               (lambda ()
                 (set (make-local-variable 'mouse3-save-then-kill-command)
                      'mouse3-dired-toggle-marks-in-region)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (set (make-local-variable 'mouse3-save-then-kill-command)
                   'mouse3-dired-region-menu))))

(defun mouse3-dired-use-toggle-marks ()
  "Make a second `mouse-3' click at the same place toggle marks in Dired."
  (interactive)
  ;; The `define-key's are not needed unless you use `dired+.el'.
  ;; In that case, this overrides the Dired+ behavior, which is a complex menu.
  (when (featurep 'dired+)
    (define-key dired-mode-map [down-mouse-3] nil)
    (define-key dired-mode-map [mouse-3] nil))
  (remove-hook 'dired-mode-hook
               (lambda ()
                 (set (make-local-variable 'mouse3-save-then-kill-command)
                      'mouse3-dired-region-menu)))
  (add-hook 'dired-mode-hook
            (lambda ()
              (set (make-local-variable 'mouse3-save-then-kill-command)
                   'mouse3-dired-toggle-marks-in-region))))

(defun mouse3-dired-toggle-marks-in-region (ignore1 ignore2)
  "Toggle marked and unmarked files and directories in region."
  (interactive "e\nP")
  (save-restriction (narrow-to-region (region-beginning) (region-end))
                    (if (< emacs-major-version 21)
                        (dired-do-toggle)
                      (dired-toggle-marks))))

;; The next 3 commands are borrowed from `dired+.el'.
(defun mouse3-dired-mark-region-files (&optional unmark-p)
  "Mark all of the files in the current region (if it is active).
With non-nil prefix arg UNMARK-P, unmark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point))
          end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  (if unmark-p ?\040 dired-marker-char)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

(defun mouse3-dired-unmark-region-files (&optional mark-p)
  "Unmark all of the files in the current region (if it is active).
With non-nil prefix arg UNMARK-P, mark them instead."
  (interactive "P")
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point))
          end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  (if mark-p dired-marker-char ?\040)))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

(defun mouse3-dired-flag-region-files-for-deletion ()
  "Flag all of the files in the current region (if it is active) for deletion."
  (interactive)
  (let ((beg                        (min (point) (mark)))
        (end                        (max (point) (mark)))
        (inhibit-field-text-motion  t)) ; Just in case.
    (setq beg (save-excursion (goto-char beg) (beginning-of-line) (point))
          end (save-excursion (goto-char end) (end-of-line) (point)))
    (let ((dired-marker-char  dired-del-marker))
      (dired-mark-if (and (<= (point) end) (>= (point) beg)) "in region"))))

;;; Picture mode.
;;;; (defcustom mouse3-picture-mode-submenu
;;;;   `,@`(("Clear Rectangle"             . picture-clear-rectangle)
;;;;        ("Kill Rectangle"
;;;;         . (lambda (start end)
;;;;             "Kill the selected rectangle.
;;;; You can yank it using \\<picture-mode-map>`\\[picture-yank-rectangle]'."
;;;;             (interactive "r")
;;;;             (picture-clear-rectangle start end 'KILLP)))
;;;;        ("Clear Rectangle to Register" . picture-clear-rectangle-to-register)
;;;;        ("Draw Rectangle"              . picture-draw-rectangle)
;;;;        ,@(and (consp picture-killed-rectangle) 
;;;;               '(("Yank Picture Rectangle (Replace)"
;;;;                  . picture-yank-rectangle)))
;;;;        ("Yank Rectangle from Register (Replace)"
;;;;         . (lambda ()
;;;;             "Replace the selected rectangle by the contents of a register you name."
;;;;             (interactive)
;;;;             (exchange-point-and-mark)
;;;;             (call-interactively #'picture-yank-rectangle-from-register))))
;;;;   "Picture mode submenu of popup menu for `mouse-3'."
;;;;   :type '(repeat
;;;;           (choice
;;;;            (cons :tag "Item"
;;;;             (string :tag "Name")
;;;;             ;; This is more correct but gives `mismatch' in Emacs < version 24:
;;;;             ;; (restricted-sexp :tag "Command" :match-alternatives (commandp))
;;;;             (sexp :tag "Command"))
;;;;            (list :tag "Separator" (const "--"))))
;;;;   :group 'mouse)

;;;; (add-hook
;;;;  'picture-mode-hook
;;;;  (lambda ()
;;;;    (set (make-local-variable 'mouse3-region-popup-submenus)
;;;;         (cons
;;;;          `("Picture Mode" mouse3-picture-mode-submenu)
;;;;          (copy-tree mouse3-region-popup-submenus)))))

(add-hook
 'picture-mode-hook
 (lambda ()
   (set (make-local-variable 'mouse3-region-popup-submenus)
        (cons
         `("Picture Mode"
           ,@`(("Clear Rectangle"             . picture-clear-rectangle)
               ("Kill Rectangle"
                . (lambda (start end)
                    "Kill the selected rectangle.
You can yank it using \\<picture-mode-map>`\\[picture-yank-rectangle]'."
                    (interactive "r")
                    (picture-clear-rectangle start end 'KILLP)))
               ("Clear Rectangle to Register" . picture-clear-rectangle-to-register)
               ("Draw Rectangle"              . picture-draw-rectangle)
               ,@(and (consp picture-killed-rectangle) 
                      '(("Yank Picture Rectangle (Replace)"
                         . picture-yank-rectangle)))
               ("Yank Rectangle from Register (Replace)"
                . (lambda ()
                    "Replace the selected rectangle by the contents of a register you name."
                    (interactive)
                    (exchange-point-and-mark)
                    (call-interactively #'picture-yank-rectangle-from-register)))))
         (copy-tree mouse3-region-popup-submenus)))))

;;; Org mode.
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'mouse3-region-popup-submenus)
                 (cons '("Org Mode"
                         ("Shift Time"          . org-timer-change-times-in-region)
                         ("Convert to ASCII"    . org-replace-region-by-ascii)
                         ("Convert to HTML"     . org-replace-region-by-html)
                         ("Convert to DocBook"  . org-replace-region-by-docbook)
                         ("Convert to LaTeX"    . org-replace-region-by-latex))
                       (copy-tree mouse3-region-popup-submenus)))))



;;; An example of binding variable `mouse3-save-then-kill-command', taken from Icicles.
;;; Without the variable this definition would need to duplicate all of the
;;; `mouse-save-then-kill' code, changing just one line of it.
;;;
;;; (defun icicle-mouse-save-then-kill (click &optional arg) ; `mouse-3' in *Completions*.
;;;   "`mouse-save-then-kill', but click same place saves selected candidates."
;;;   (interactive "e\nP")
;;;   (let ((mouse3-save-then-kill-command
;;;          `(lambda (event prefix-arg)
;;;             (icicle-mouse-candidate-set-save-more nil ,arg))))
;;;     (mouse-save-then-kill click))
;;;   (setq this-command  'mouse-save-then-kill))

;;;;;;;;;;;;;;;;;;;;;;

(provide 'mouse3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse3.el ends here
