;;; sunrise-x-tabs.el --- Tabs for the Sunrise Commander File Manager.

;; Copyright (C) 2009-2010 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 24 Oct 2009
;; Version: 1
;; RCS Version: $Rev: 309 $
;; Keywords: Sunrise Commander Emacs File Manager Tabs Minor Mode
;; URL: http://www.emacswiki.org/emacs/sunrise-x-tabs.el
;; Compatibility: GNU Emacs 22+

;; This file is *NOT* part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation,  either  version  3 of the License, or (at your option) any later
;; version.
;;
;; This  program  is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR  A  PARTICULAR  PURPOSE.  See the GNU General Public License for more de-
;; tails.

;; You  should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This  extension brings tab‐based navigation to the Sunrise Commander. It adds
;; to the list of optional mechanisms already available in  Sunrise  for  moving
;; around  the  file system (like regular bookmarks, checkpoints, history rings,
;; materialized virtual buffers, navigable paths and file‐following) another way
;; to  maintain a list of selected locations one wants to return later on, or to
;; compose "breadcrumb trails" for complex repetitive operations.

;; The  main  difference between tabs and other mechanisms is that once a buffer
;; has been assigned to a tab, it will not be killed automatically  by  Sunrise,
;; so  it’s  possible  to keep it around as long as necessary with all its marks
;; and state untouched.  Tabs can be persisted across sessions using the DeskTop
;; feature.

;; Creating, using and destroying tabs are fast and easy operations, either with
;; mouse or keyboard:

;; * Press C-j (or select Sunrise > Tabs > Add Tab in the menu) to create a new
;; tab or to rename an already existing tab.

;; * Press C-k (or right-click the tab) to kill an existing tab.

;; *  Press  C‐n and C‐p to move from tab to tab ("Next", "Previous"), or simply
;; left‐click on the tab to focus its assigned buffer. These two keybindings can
;; be prefixed with an integer to move faster.

;; *  The last four bindings can be combined with Meta (i.e. C‐M‐j, C‐M‐k, C‐M‐n
;; and C‐M‐p) to perform the same operation on the  passive  pane  or  (when  in
;; synchronized navigation mode) on both panes simultaneously.

;; *  Killing  the  current  buffer with C‐x k automatically switches to the one
;; assigned to the first available tab (if any).

;; The  extension  is  provided  as a minor mode, so you can enable / disable it
;; totally by issuing the command (M-x) sr-tabs-mode.

;; It does *not* pretend to be a generic solution for tabs in emacs. If you need
;; one, have a look at TabBar  mode  (http://www.emacswiki.org/emacs/TabBarMode)
;; by David Ponce. I wrote this just because it turned out to be easier to write
;; this than to customize tabbar to behave exactly  like  I  wanted  inside  the
;; Sunrise  panes.  It’s meant to be simple and to work nicely with Sunrise with
;; just a few tabs (up to 10‐15 per pane, maybe).

;; This is version 1 $Rev: 309 $ of the Sunrise Commander Tabs Extension.

;; It  was  written  on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 23) for  Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs load-path.

;; 2)  Add  a (require ’sunrise‐x‐tabs) expression to your .emacs file somewhere
;; after the (require ’sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart emacs.

;; 4) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)
(eval-when-compile (require 'desktop))

(defcustom sr-tabs-follow-panes t
  "Whether tabs should be swapped too when transposing the Sunrise panes."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-tabs-max-tabsize 10
  "Maximum length of a tab in the Sunrise Commander FM."
  :group 'sunrise
  :type 'integer)

(defface sr-tabs-active-face
  '((((type tty) (class color) (min-colors 88))
     :background "white")
    (((type tty) (class color) (min-colors 8))
     :background "green" :foreground "yellow" :bold t)
    (((type tty) (class mono)) :inverse-video t)
    (t
     :inherit variable-pitch :bold t :background "white" :height 0.9))
  "Face of the currently selected tab in any of the Sunrise panes."
  :group 'sunrise)

(defface sr-tabs-inactive-face
  '((((type tty) (class color) (min-colors 88))
     :background "color-84" :foreground "white")
    (((type tty) (class color) (min-colors 8))
     :background "white" :foreground "cyan")
    (t
     :inherit variable-pitch :background "gray95" :height 0.9))
  "Face of all non-selected tabs in both Sunrise panes."
  :group 'sunrise)

(defface sr-tabs-separator-face
  '((t (:height 0.3)))
  "Face of the string used to separate the Sunrise tabs from one another."
  :group 'sunrise)

(defconst sr-tabs-sep #(" " 0 1 (face sr-tabs-separator-face))
  "Sunrise Tabs separator character.")

(defconst sr-tabs-ligature #(" ║" 0 1 (face sr-tabs-separator-face))
  "Sunrise Tabs line separator string.")

(defconst sr-tabs-max-cache-length 30
  "Max number of tab labels cached for reuse.")

(defvar sr-tabs '((left)(right)))
(defvar sr-tabs-labels-cache '((left) (right))) 
(defvar sr-tabs-line-cache '((left)(right)))
(defvar sr-tabs-mode nil)
(defvar sr-tabs-on nil)

;;; ============================================================================
;;; Core functions:

(defun sr-tabs-add ()
  "Assigns  the  current  buffer to exactly one tab in the active pane, or calls
  interactively sr-tabs-rename if a tab already exists for the current  buffer."
  (interactive)
  (let ((tab-name (buffer-name))
        (tab-set (assq sr-selected-window sr-tabs)))
      (if (member tab-name (cdr tab-set))
          (call-interactively 'sr-tabs-rename)
        (setcdr tab-set (cons tab-name (cdr tab-set)))))
  (sr-tabs-refresh))

(defun sr-tabs-remove (&optional tab-buffer side)
  "Removes  the tab to which the given buffer is assigned in the active pane. If
  the optional argument is nil, removes the tab to which the current  buffer  is
  assigned, if any."
  (interactive)
  (let* ((tab-name (buffer-name tab-buffer))
         (side (or side sr-selected-window))
         (tab-set (assq side sr-tabs)))
    (setcdr tab-set (delete tab-name (cdr tab-set))))
  (sr-tabs-refresh))

(defun sr-tabs-kill (&optional name side)
  "Removes  the tab  with the  given name  from the  active pane  and  kills its
  assigned buffer, unless it's currently visible or it's assigned to other tab."
  (interactive)
  (let ((to-kill (or (and name (get-buffer name)) (current-buffer))) (stack)
        (side (or side sr-selected-window)))
    (sr-tabs-remove to-kill side)
    (if (and (not (memq to-kill (list sr-left-buffer sr-right-buffer)))
             (not (member to-kill (apply 'append (mapcar 'cdr sr-tabs)))))
        (kill-buffer to-kill))
    (sr-tabs-refresh)))

(defun sr-tabs-next (&optional n)
  "Moves focus to the next tab (left to right) in the active pane. When prefixed
  with an integer moves focus to the tab n places ahead, or to the last  one  if
  there are fewer tabs than requested."
  (interactive "p")
  (sr-tabs-step n))

(defun sr-tabs-prev (&optional n)
  "Moves  focus  to  the  previous  tab (right to left) in the active pane. When
  prefixed with an integer moves focus to the tab n places  behind,  or  to  the
  first one if there are fewer tabs than requested."
  (interactive "p")
  (sr-tabs-step n t))

(defun sr-tabs-step (count &optional back)
  "Moves  focus from the current tab to the one ``count'' places ahead or behind
  (depending on the value of ``back'')."
  (let* ((stack (cdr (assq sr-selected-window sr-tabs)))
         (stack (if back (reverse stack) stack))
         (target (member (buffer-name) stack)))
    (unless (null stack)
      (if (or (null count) (zerop count))
          (setq count 1))
      (if (< 1 (length target))
          (sr-tabs-switch-to-buffer (or (nth count target) (car (last target))))
        (sr-tabs-switch-to-buffer (car stack))))))

(defun sr-tabs-switch-to-buffer (to-buffer)
  "Takes  care of correctly changing the context of the active Sunrise pane when
  switching buffers."
  (let ((from-buffer (current-buffer)))
    (unless (eq from-buffer to-buffer)
      (switch-to-buffer to-buffer)
      (setq sr-this-directory default-directory)
      (set (sr-symbol sr-selected-window 'buffer) (current-buffer))
      (set (sr-symbol sr-selected-window 'directory) default-directory)
      (unless (eq from-buffer (sr-other 'buffer))
        (kill-buffer from-buffer))
      (revert-buffer)
      (sr-history-push default-directory)))
  (sr-tabs-refresh))

(defun sr-tabs-focus (name side)
  "Gives focus to the tab with the given name in the given pane."
  (unless (eq side sr-selected-window)
    (sr-change-window))
  (sr-tabs-switch-to-buffer name))

(defun sr-tabs-kill-and-go ()
  "Kills the current Sunrise buffer, removes its assigned tab (if any) and moves
  to the next buffer tabbed in the active pane, unless there are no more  tabbed
  buffers to fall back, in which case just removes the tab."
  (interactive)
  (let ((to-kill (current-buffer)) (stack))
    (sr-tabs-kill)
    (setq stack (cdr (assq sr-selected-window sr-tabs)))
    (sr-tabs-next)
    (unless (or (null stack) (eq to-kill (current-buffer)))
      (kill-buffer to-kill))))

(defun sr-tabs-rename (&optional new-name)
  (interactive "sRename current tab to: ")
  (let* ((key (buffer-name))
         (cache (assq sr-selected-window sr-tabs-labels-cache))
         (label (cadr cache)))
    (if label
        (sr-tabs-redefine-label key new-name))))

(defun sr-tabs-transpose ()
  "Swaps the sets of tabs from one pane to the other."
  (interactive)
  (setq sr-tabs (mapc (lambda (x)
                        (if (eq 'left (car x))
                            (setcar x 'right)
                          (setcar x 'left))) sr-tabs))
  (sr-in-other (sr-tabs-refresh))
  (sr-tabs-refresh))

;; This synchronizes the tabs with the panes if so required (see variable
;; sr-tabs-follow-panes). Activated in method sr-tabs-engage.
(defadvice sr-transpose-panes
  (after sr-tabs-advice-sr-transpose-panes ())
  (if sr-tabs-follow-panes (sr-tabs-transpose)))

;;; ============================================================================
;;; Graphical interface:

(defun sr-tabs-focus-cmd (name side)
  "Returns  an anonymous function that can be used to give focus to the tab with
  the given name in the given pane."
  (let ((selector (if (eq side (caar sr-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (sr-tabs-focus ,name (funcall ',selector sr-tabs)))))

(defun sr-tabs-rename-cmd (name)
  "Returns  an  anonymous  function  that can be used to rename the tab with the
  given name in both panes."
  `(lambda (&optional new-name)
     (interactive "sRename tab to: ")
     (sr-tabs-redefine-label ,name new-name)))

(defun sr-tabs-kill-cmd (name side)
  "Returns  an  anonymous  function  that can be used to delete the tab with the
  given name in the given pane."
  (let ((selector (if (eq side (caar sr-tabs)) #'caar #'caadr)))
    `(lambda ()
       (interactive)
       (if (eq sr-selected-window (funcall ',selector sr-tabs))
           (sr-tabs-kill ,name)
         (sr-in-other
          (sr-tabs-kill ,name))))))

(defsubst sr-tabs-propertize-tag (string face keymap)
  "Propertizes the given string with the given face and keymap so it can be used
  as a tab tag."
  (propertize string
              'face face
              'help-echo "mouse-1: select tab\n\mouse-2: rename tab\n\mouse-3: kill tab"
              'local-map keymap))                

(defun sr-tabs-make-tag (name as-active &optional tag)
  "Prepares and returns a propertized string for decorating a tab with the given
  name in the given state (nil = passive, t =  active).  The  optional  argument
  allows to provide a pretty name to label the tag."
  (let ((tag (or tag name))
        (side sr-selected-window)
        (keymap (make-sparse-keymap)))
    (if (< sr-tabs-max-tabsize (length tag))
        (setq tag (concat (substring tag 0 sr-tabs-max-tabsize) "…")))
    (setq tag (concat sr-tabs-sep tag sr-tabs-sep))
    (define-key keymap [header-line mouse-1] (sr-tabs-focus-cmd name side))
    (define-key keymap [header-line mouse-2] (sr-tabs-rename-cmd name))
    (define-key keymap [header-line mouse-3] (sr-tabs-kill-cmd name side))
    (if as-active
        (sr-tabs-propertize-tag tag 'sr-tabs-active-face keymap)
      (sr-tabs-propertize-tag tag 'sr-tabs-inactive-face keymap))))

(defun sr-tabs-make-label (name &optional alias)
  "Prepares  and returns a new label for decorating a tab with the given name. A
  label is a dotted pair of tags, for active and passive state. The new label is
  put in cache for later reuse. The optional argument allows to provide a pretty
  name to label the tab."
  (let* ((alias (or alias name))
         (label (cons (sr-tabs-make-tag name t alias)
                      (sr-tabs-make-tag name nil alias)))
         (entry (list (cons name label)))
         (cache (assq sr-selected-window sr-tabs-labels-cache)))
    (setcdr cache (append (cdr cache) entry))
    label))

(defun sr-tabs-trim-label (label)
  "Remove all properties and trailing whitespace from the given string."
  (replace-regexp-in-string "^\\s-+\\|\\s-+$"
                            ""
                            (substring-no-properties label)))

(defun sr-tabs-redefine-label (name alias)
  "Allows to modify the pretty name (alias) of the label with the given name."
  (let* ((alias (sr-tabs-trim-label (or alias ""))) (cache))
    (if (string= "" alias)
        (error "Cancelled: invalid tab name")
      (progn
        (setq cache (assq sr-selected-window sr-tabs-labels-cache))
        (setcdr cache (delq nil
                       (mapcar (lambda(x)
                                 (and (not (equal (car x) name)) x))
                               (cdr cache))))
        (sr-tabs-make-label name alias)
        (sr-tabs-refresh)))))

(defun sr-tabs-get-tag (name is-active)
  "Retrieves  the  cached tag for the tab with the given name in the given state
  (nil = inactive, t = active), creating new labels when needed."
  (let* ((cache (assq sr-selected-window sr-tabs-labels-cache))
         (label (cdr (assoc name (cdr cache)))))
    (if (null label)
        (setq label (sr-tabs-make-label name)))
    (if (< sr-tabs-max-cache-length (length (cdr cache)))
        (setcdr cache (cddr cache)))
    (if is-active (car label) (cdr label))))

(defun sr-tabs-make-line ()
  "Assembles a new tab line from cached tags and puts it in the line cache."
  (if (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
      (let ((tab-set (cdr (assq sr-selected-window sr-tabs)))
            (tab-line (if (or (cdr (first sr-tabs))
                              (cdr (second sr-tabs))) "" nil))
            (current-name (buffer-name)))
        (mapc (lambda (x)
                (let ((is-current (equal current-name x)))
                  (setq tab-line (concat tab-line sr-tabs-sep
                                         (sr-tabs-get-tag x is-current)))))
              tab-set)
        (setcdr (assq sr-selected-window sr-tabs-line-cache) tab-line)
        tab-line)
    nil))

(defsubst sr-tabs-empty-p (line)
  (or (null line) (string= "" line)))

(defsubst sr-tabs-empty-mask (line)
  (or (and (null line) "") line))

(defsubst sr-tabs-empty-null (line)
  (if (sr-tabs-empty-p line) nil line))

(defun has-nonempty-p (line-list)
  "Tells whether the given list contains at least one non-nil element."
  (or (not (sr-tabs-empty-p (car line-list)))
      (and (cdr line-list) (has-nonempty-p (cdr line-list)))))

(defun sr-tabs-xor (list1 list2)
  "Replacement for function set-exclusive-or, written exclusively to eliminate
  a soft dependency on cl-seq.el (isn't this getting a bit ridiculous?)"
  (cond ((null list1) list2)
        ((null list2) list1)
        ((equal list1 list2) nil)
        (t
         (let (result)
           (mapc (lambda (element)
                   (if (member element result)
                       (setq result (delete element result))
                     (setq result (cons element result))))
                 (append list1 list2))
           result))))

(defun sr-tabs-refresh ()
  "Updates  the  header-line-format variable in the buffers on both panes, using
  the line cache for the passive one, and assembling a  new  tab  line  for  the
  active  one.  In  the  (corner)  case when both panes contain the same buffer,
  glues together the tab lines with a ``double bar'' separator."
  (setq sr-tabs-mode sr-tabs-on)
  (sr-tabs-make-line)
  (let ((line-list (mapcar 'cdr sr-tabs-line-cache))
        (same-buffer (eq sr-left-buffer sr-right-buffer)))
    (if same-buffer
        (setq header-line-format
              (and (has-nonempty-p line-list)
                   (mapconcat 'concat line-list sr-tabs-ligature)))
      (let ((other-buffer (sr-other 'buffer)))
        (if (eq 'right sr-selected-window)
            (setq line-list (nreverse line-list)))
        (if (apply 'sr-tabs-xor (mapcar 'sr-tabs-empty-p line-list))
            (setq line-list (mapcar 'sr-tabs-empty-mask line-list))
          (setq line-list (mapcar 'sr-tabs-empty-null line-list)))

        (setq header-line-format (first line-list))

        (when (buffer-live-p other-buffer)
          (with-current-buffer other-buffer
            (setq header-line-format (second line-list)))))))
  (force-window-update))

;;; ============================================================================
;;; Private interface:

(defun sr-tabs-bury-all ()
  "Buries all currently tabbed buffers."
  (let ((all-buffers (apply 'append (mapcar 'cdr sr-tabs))))
    (if all-buffers
        (mapc 'bury-buffer all-buffers))))

(defun sr-tabs-protect-buffer ()
  "Protects the current buffer from being automatically disposed by Sunrise when
  moving to another directory (called from kill-buffer-query-functions hook.)"
  (let ((tab-name (buffer-name)))
    (not (or (member tab-name (first sr-tabs))
             (member tab-name (second sr-tabs))))))

(defun sr-tabs-engage ()
  "Enables the Sunrise Tabs extension."
  (setq sr-tabs-on t)
  (add-hook 'sr-refresh-hook 'sr-tabs-refresh)
  (add-hook 'sr-quit-hook 'sr-tabs-bury-all)
  (add-hook 'kill-buffer-query-functions 'sr-tabs-protect-buffer)
  (ad-activate 'sr-transpose-panes)
  (sr-tabs-refresh))

(defun sr-tabs-disengage ()
  "Disables the Sunrise Tabs extension."
  (setq sr-tabs-on nil)
  (remove-hook 'sr-refresh-hook 'sr-tabs-refresh)
  (remove-hook 'sr-quit-hook 'sr-tabs-bury-all)
  (remove-hook 'kill-buffer-query-functions 'sr-tabs-protect-buffer)
  (ad-deactivate 'sr-transpose-panes)
  (setq header-line-format (default-value 'header-line-format))
  (sr-in-other (setq header-line-format (default-value 'header-line-format))))

;;; ============================================================================
;;; User interface:

(defvar sr-tabs-mode-map (make-sparse-keymap))
(define-key sr-tabs-mode-map [(control ?j)] 'sr-tabs-add)
(define-key sr-tabs-mode-map [(control ?k)] 'sr-tabs-remove)
(define-key sr-tabs-mode-map [(control ?p)] 'sr-tabs-prev)
(define-key sr-tabs-mode-map [(control ?n)] 'sr-tabs-next)

(define-key sr-tabs-mode-map [(control meta ?j)]
  (lambda () (interactive) (sr-in-other (sr-tabs-add))))
(define-key sr-tabs-mode-map [(control meta ?k)]
  (lambda () (interactive) (sr-in-other (sr-tabs-remove))))
(define-key sr-tabs-mode-map [(control meta ?p)]
  (lambda () (interactive) (sr-in-other (sr-tabs-prev))))
(define-key sr-tabs-mode-map [(control meta ?n)]
  (lambda () (interactive) (sr-in-other (sr-tabs-next))))

(define-key sr-tabs-mode-map "\C-xk" 'sr-tabs-kill-and-go)
(define-key sr-tabs-mode-map "\M-T"  'sr-tabs-transpose)

(define-minor-mode sr-tabs-mode
  "Tabs support for the Sunrise Commander file manager. This minor mode provides
  the following keybindings:

        C-j ........... Create new tab (or rename existing tab) in active pane.
        C-k ........... Kill the tab of the current buffer in the active pane.
        C-n ........... Move to the next tab in the active pane.
        C-p ........... Move to the previous tab in the active pane.

        C-M-j ......... Assign the current buffer to a tab in the passive pane.
        C-M-k ......... Kill the tab of the current buffer in the passive pane.
        C-M-n ......... Move to the next tab in the passive pane.
        C-M-p ......... Move to the previous tab in the passive pane.

        C-x k ......... Kill buffer and move to the next tabbed one (if any).
"
  nil nil sr-tabs-mode-map
  (unless (memq major-mode '(sr-mode sr-virtual-mode sr-tree-mode))
    (setq sr-tabs-mode nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander."))
  (if sr-tabs-mode
      (sr-tabs-engage)
    (sr-tabs-disengage)))



;;; ============================================================================
;;; Bootstrap:

(defun sr-tabs-menu-init ()
  "Initializes the Sunrise Tabs extension menu."
  (unless (lookup-key sr-mode-map [menu-bar Sunrise])
    (define-key sr-mode-map [menu-bar Sunrise]
      (cons "Sunrise" (make-sparse-keymap))))
  (let ((menu-map (make-sparse-keymap "Tabs")))
    (define-key sr-mode-map [menu-bar Sunrise tabs] (cons "Tabs" menu-map))
    (define-key menu-map [help] '("Help" . (lambda ()
                                             (interactive)
                                             (describe-function 'sr-tabs-mode))))
    (define-key menu-map [transpose] '("Transpose" . sr-tabs-transpose))
    (define-key menu-map [kill]      '("Kill and go to next" . sr-tabs-kill-and-go))
    (define-key menu-map [next]      '("Next"         . sr-tabs-next))
    (define-key menu-map [prev]      '("Previous"     . sr-tabs-prev))
    (define-key menu-map [remove]    '("Remove"       . sr-tabs-remove))
    (define-key menu-map [add]       '("Add/Rename"   . sr-tabs-add))))

(defun sr-tabs-start-once ()
  "Bootstraps  the  tabs  mode  on the first execution of the Sunrise Commander,
  after module installation."
  (sr-tabs-mode t)
  (sr-tabs-menu-init)
  (remove-hook 'sr-start-hook 'sr-tabs-start-once)
  (unintern 'sr-tabs-menu-init)
  (unintern 'sr-tabs-start-once))
(add-hook 'sr-start-hook 'sr-tabs-start-once)

;;; ============================================================================
;;; Desktop support:

(defun sr-tabs-desktop-save-buffer (desktop-dirname)
  "Returns the additional data for saving the tabs of the current sunrise buffer
  into a desktop file."
  (let* ((left-tab (car (member (buffer-name) (assoc 'left sr-tabs))))
         (left-cache (cdr (assq 'left sr-tabs-labels-cache)))
         (left-label (cadr (assoc left-tab left-cache)))
         (right-tab (car (member (buffer-name) (assoc 'right sr-tabs))))
         (right-cache (cdr (assq 'right sr-tabs-labels-cache)))
         (right-label (cadr (assoc right-tab right-cache))))
    (delq
     nil
     (list
      (if left-label (cons 'left-tab (sr-tabs-trim-label left-label)))
      (if right-label (cons 'right-tab (sr-tabs-trim-label right-label)))))))

(defun sr-tabs-desktop-restore-buffer (desktop-buffer-file-name
                                       desktop-buffer-name
                                       desktop-buffer-misc)
  "Restores  all  the  tabs  in  a  Sunrise  (normal  or  VIRTUAL) buffer from a
  description in a desktop file."
  (mapc (lambda (side)
          (let* ((sr-selected-window side)
                 (tab-symbol (intern (concat (symbol-name side) "-tab")))
                 (name (buffer-name))
                 (label (cdr (assq tab-symbol desktop-buffer-misc)))
                 (tab-set (assq side sr-tabs)))
            (when label
              (setcdr tab-set (cons name (cdr tab-set)))
              (sr-tabs-make-label name label))))
        '(left right))
  (unless sr-tabs-on
    (sr-tabs-engage)))

(defun sr-tabs-reset-state ()
  "Resets  some  environment  variables that control the behavior of tabs in the
  Sunrise Commander (used for desktop support.)"
  (mapc (lambda (x) (setcdr x nil)) sr-tabs-labels-cache)
  (mapc (lambda (x) (setcdr x nil)) sr-tabs)
  nil)

;; These append the previous functions to the generic desktop support in Sunrise:
(add-to-list 'sr-desktop-save-handlers 'sr-tabs-desktop-save-buffer)
(add-to-list 'sr-desktop-restore-handlers 'sr-tabs-desktop-restore-buffer)

;; This activates the tabs support after desktop restoration:
(add-hook
 'desktop-after-read-hook
 (lambda ()
   (unless (assq 'sr-tabs-on desktop-globals-to-clear)
     (add-to-list 'desktop-globals-to-clear
                  '(sr-tabs-on . (sr-tabs-reset-state))))))

(provide 'sunrise-x-tabs)

;;; sunrise-x-tabs.el ends here.
