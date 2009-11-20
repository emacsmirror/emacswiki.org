;;; sunrise-x-tabs.el --- Tabs for the Sunrise Commander File Manager.

;; Copyright (C) 2009 José Alfredo Romero Latouche (j0s3l0)

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;; Keywords: Sunrise Commander Emacs File Manager Tabs Minor Mode

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
;; and state untouched. On the  other  hand,  tabs  are  not  persistent  across
;; sessions; if you need a persistent mechanism consider bookmarks, checkpoints,
;; materialized virtual buffers or history listings.

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

;; This is version 1 $Rev: 226 $ of the Sunrise Commander Tabs Extension.

;; It  was  written  on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for  Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs load-path.

;; 2)  Add  a (require ’sunrise‐x‐tabs) expression to your .emacs file somewhere
;; after the (require ’sunrise‐commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart emacs.

;; 4) Enjoy ;-)

;;; Code:

(require 'sunrise-commander)
(require 'easymenu)

(defcustom sr-tabs-max-tabsize 10
  "Maximum length of tabs in the Sunrise Commander FM."
  :group 'sunrise
  :type 'integer)

(defface sr-tabs-active-face
  '((t (:inherit variable-pitch :bold t :background "white" :height 0.9)))
  "Face of the currently selected tab in any of the Sunrise panes."
  :group 'sunrise)

(defface sr-tabs-inactive-face
  '((t (:inherit variable-pitch :background "gray95" :height 0.9)))
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
(defvar sr-tabs-labels-cache nil) 
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
        (tab-set (assoc sr-selected-window sr-tabs)))
      (if (member tab-name (cdr tab-set))
          (call-interactively 'sr-tabs-rename)
        (setcdr tab-set (cons tab-name (cdr tab-set)))))
  (sr-tabs-refresh))

(defun sr-tabs-remove (&optional tab-buffer)
  "Removes  the tab to which the given buffer is assigned in the active pane. If
  the optional argument is nil, removes the tab to which the current  buffer  is
  assigned, if any."
  (interactive)
  (let ((tab-name (buffer-name tab-buffer)))
    (setq sr-tabs (mapcar (lambda (x) (delete tab-name x)) sr-tabs)))
  (sr-tabs-refresh))

(defun sr-tabs-kill (&optional name)
  "Removes  the  tab  with  the  given  name  from the active pane and kills its
  assigned buffer, unless it's currently visible."
  (interactive)
  (let ((to-kill (or (and name (get-buffer name)) (current-buffer))) (stack))
    (sr-tabs-remove to-kill)
    (setq stack (cdr (assoc sr-selected-window sr-tabs)))
    (if (and stack (not (memq to-kill (list sr-left-buffer sr-right-buffer))))
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
  (let* ((stack (cdr (assoc sr-selected-window sr-tabs)))
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
      (sr-revert-buffer)
      (sr-history-push default-directory))))

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
    (setq stack (cdr (assoc sr-selected-window sr-tabs)))
    (sr-tabs-next)
    (unless (or (null stack) (eq to-kill (current-buffer)))
      (kill-buffer to-kill))))

(defun sr-tabs-rename (&optional new-name)
  (interactive "sRename current tab to: ")
  (let* ((key (buffer-name))
         (label (cdr (assoc key sr-tabs-labels-cache))))
    (if label
        (sr-tabs-redefine-label key new-name))))

;;; ============================================================================
;;; Graphical interface:

(defun sr-tabs-focus-cmd (name side)
  "Returns  an anonymous function that can be used to give focus to the tab with
  the given name in the given pane."
  `(lambda ()
     (interactive)
     (sr-tabs-focus ,name ',side)))

(defun sr-tabs-rename-cmd (name)
  "Returns  an  anonymous  function  that can be used to rename the tab with the
  given name in both panes."
  `(lambda (&optional new-name)
     (interactive "sRename tab to: ")
     (sr-tabs-redefine-label ,name new-name)))

(defun sr-tabs-kill-cmd (name side)
  "Returns  an  anonymous  function  that can be used to delete the tab with the
  given name in the given pane."
  `(lambda ()
     (interactive)
     (if (eq sr-selected-window ',side)
         (sr-tabs-kill ,name)
       (sr-in-other
        (sr-tabs-kill ,name)))))

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
         (entry (list (cons name label))))
    (setq sr-tabs-labels-cache (append sr-tabs-labels-cache entry))
    label))

(defun sr-tabs-redefine-label (name alias)
  "Allows to modify the pretty name (alias) of the label with the given name."
  (let* ((alias (or alias ""))
         (alias (replace-regexp-in-string "^\\s-+\\|\\s-+$" "" alias)))
    (if (string= "" alias)
        (error "Cancelled: invalid tab name")
      (progn
        (setq sr-tabs-labels-cache
              (delq nil (mapcar (lambda(x) (and (not (equal (car x) name)) x))
                                sr-tabs-labels-cache)))
        (sr-tabs-make-label name alias)
        (sr-tabs-refresh)))))

(defun sr-tabs-get-tag (name is-active)
  "Retrieves  the  cached tag for the tab with the given name in the given state
  (nil = inactive, t = active), creating new labels when needed."
  (let ((label (cdr (assoc name sr-tabs-labels-cache))))
    (if (null label)
        (setq label (sr-tabs-make-label name)))
    (if (< sr-tabs-max-cache-length (length sr-tabs-labels-cache))
        (setq sr-tabs-labels-cache (cdr sr-tabs-labels-cache)))
    (if is-active (car label) (cdr label))))

(defun sr-tabs-make-line ()
  "Assembles a new tab line from cached tags and puts it in the line cache."
  (if (memq major-mode '(sr-mode sr-virtual-mode))
      (let ((tab-set (cdr (assoc sr-selected-window sr-tabs)))
            (tab-line (if (or (cdr (first sr-tabs))
                              (cdr (second sr-tabs))) "" nil))
            (current-name (buffer-name)))
        (mapc (lambda (x)
                (let ((is-current (equal current-name x)))
                  (setq tab-line (concat tab-line sr-tabs-sep
                                         (sr-tabs-get-tag x is-current)))))
              tab-set)
        (setcdr (assoc sr-selected-window sr-tabs-line-cache) tab-line)
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
        (if (apply 'set-exclusive-or (mapcar 'sr-tabs-empty-p line-list))
            (setq line-list (mapcar 'sr-tabs-empty-mask line-list))
          (setq line-list (mapcar 'sr-tabs-empty-null line-list)))

        (setq header-line-format (first line-list))

        (when (buffer-live-p other-buffer)
          (set-buffer other-buffer)
          (setq header-line-format (second line-list))))))
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
  (sr-tabs-refresh))

(defun sr-tabs-disengage ()
  "Disables the Sunrise Tabs extension."
  (setq sr-tabs-on nil)
  (remove-hook 'sr-refresh-hook 'sr-tabs-refresh)
  (remove-hook 'sr-quit-hook 'sr-tabs-bury-all)
  (remove-hook 'kill-buffer-query-functions 'sr-tabs-protect-buffer)
  (setq header-line-format (default-value 'header-line-format))
  (sr-in-other (setq header-line-format (default-value 'header-line-format))))

;;; ============================================================================
;;; User interface:

(defvar sr-tabs-menu
  (easy-menu-create-menu
   "Tabs"
   '(["Add/Rename tab" sr-tabs-add]
     ["Remove tab" sr-tabs-remove]
     ["Go to next tab" sr-tabs-next]
     ["Go to previous tab" sr-tabs-prev]
     ["Kill buffer and go to next tab" sr-tabs-kill-and-go]
     ["Tabs mode help" (lambda ()
                         (interactive)
                         (describe-function 'sr-tabs-mode))])))

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
  (unless (memq major-mode '(sr-mode sr-virtual-mode))
    (setq sr-tabs-mode nil)
    (error "Sorry, this mode can be used only within the Sunrise Commander."))
  (if sr-tabs-mode
      (sr-tabs-engage)
    (sr-tabs-disengage)))

;;; ============================================================================
;;; Bootstrap:

(defun sr-tabs-menu-init ()
  "Initializes the Sunrise Tabs extension menu."
  (unless (fboundp 'easy-menu-binding) ;;<-- not available in emacs 22
    (defsubst easy-menu-binding (menu &optional item-name) (ignore)))
  (define-key sr-tabs-mode-map
    (vector 'menu-bar (easy-menu-intern "Sunrise"))
    (easy-menu-binding sr-tabs-menu "Sunrise")))

(defun sr-tabs-start-once ()
  "Bootstraps  the  tabs  mode  on the first execution of the Sunrise Commander,
  after module installation."
  (sr-tabs-mode t)
  (sr-tabs-menu-init)
  (remove-hook 'sr-start-hook 'sr-tabs-start-once)
  (unintern 'sr-tabs-start-once))
(add-hook 'sr-start-hook 'sr-tabs-start-once)

(provide 'sunrise-x-tabs)

;;; sunrise-x-tabs.el ends here
