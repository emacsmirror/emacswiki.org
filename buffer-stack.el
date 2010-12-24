;;; buffer-stack.el --- Enhanced intelligent switch-to-other-buffer replacement.

;; Copyright (C) 2002 Adrian Kubala

;; Author: Adrian Kubala <adrian@sixfingeredman.net>
;; URL: <http://www.sixfingeredman.net/proj/xemacs>
;; Created: Thu 13 Jun 02
;; Version: 1.5, Sun 30 Jun 02
;; Keywords: buffer, buffers, switching buffers

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary
;;
;; This is tested with XEmacs 21.4 and GNU Emacs 21.
;;
;; If you have used "Alt-Tab" in Windows, you know the basic principle
;; behind this buffer model. Your buffers are in a stack, with the
;; most recent on top. When you want a new buffer, you scan through
;; them until you find the one you want, and then it goes on top of
;; the stack. Unlike cycle-buffer, the list is not rearranged until
;; you find the buffer you want, so you can easily jump back to where
;; you started.
;;
;; This has some further enhancements. You can filter the stack in
;; different ways; see `buffer-stack-filter'. You can use the
;; frame-local buffer ordering; see `buffer-stack-frame-local'. You
;; can monitor your position in different ways; see
;; `buffer-stack-show-position'. And you can bury/kill buffers while
;; switching through the stack; see `buffer-stack-bury'.
;;
;; To use, put this file in your library path. In your .emacs or
;; init.el, put:
;;
;; (require 'buffer-stack)
;;
;; I use these key-bindings:
;;
;; (global-set-key [(f9)] 'buffer-stack-bury)
;; (global-set-key [(control f9)] 'buffer-stack-bury-and-kill)
;; (global-set-key [(f10)] 'buffer-stack-up)
;; (global-set-key [(f11)] 'buffer-stack-down)
;; (global-set-key [(f12)] 'buffer-stack-track)
;; (global-set-key [(control f12)] 'buffer-stack-untrack)
;;
;; See the customization group `buffer-stack' for settings. If you
;; want to preserve settings between sessions (i.e. tracked/untracked
;; buffers), just save them in custom. If you don't like custom,
;; setting variables manually after load is also respected.
;;
;; This is similar to some other libraries which I didn't know about
;; when I first wrote it, though in my opinion (of course) it's better.
;; See also http://www.emacswiki.org/cgi-bin/wiki.pl?SwitchingBuffers
;;      ibs.el -- http://www.geekware.de/software/emacs/#ibs
;;      bubble-buffer.el -- http://www.glue.umd.edu/~deego/emacspub/lisp-mine/bubble/
;;	pc-bufsw -- http://asfys3.fi.uib.no/~boukanov/emacs/index.html
;;
;; Thanks to gerds@paracelsus.fdm.uni-freiburg.de for feedback and
;; requesting the inclusive stack, deego@glue.umd.edu for feedback,
;; LathI for requesting frame-local stacks, and someone on EmacsWiki
;; for suggesting indirectly `buffer-stack-show-position-buffers'.

;;; Bugs
;;
;; The switch from a frame-local to a global stack in GNU Emacs can
;; start you with a strange ordering.
;;
;; Untracked buffers may get rearranged relative to tracked ones.
;;
;; You need to use the buffer-stack-* commands directly with
;; keybindings; `M-x buffer-stack-*' will not work, because this
;; changes `last-command'.


;;; ChangeLog

;; 1.5 2002-06-30
;;      Filter and show position can now be user functions. Fixed a
;;      conceivable (but incredibly rare) bug with the buffer list
;;      rearranging itself while switching.

;; 1.4 2002-06-28
;;      Polishing to a fine sheen.

;; 1.3 2002-06-27
;;      Can show previous and next buffers in stack.

;; 1.2 2002-06-27
;;      Workaround for broken GNU Emacs bury-buffer.

;; 1.1 2002-06-23
;;      New implementation which uses buffer-list directly. Dropping
;;      frame-local-tracking, mountain stack. Works with GNU Emacs!

;; 1.0 2002-06-23
;;      Sealing corner cases and fixing bugs in frame-local usage.

;; 0.9 2002-06-18
;;      Public release. Includes customization variables.

;; 0.3--0.4b 2002-06-16
;;      First attempts at frame-local and inclusive-mode features.

;; 0.1--0.2 2002-06-13
;;      Initial release and minor correction. Basic mountain stack.

;;; Code:

(provide 'buffer-stack)

;;; public variables / configuration

(defgroup buffer-stack nil
  "Smart movement through the buffer list."
  :group 'editing
  :prefix "buffer-stack-"
  :link '(emacs-commentary-link :tag "Commentary" "buffer-stack.el")
  :link '(emacs-library-link :tag "Lisp File" "buffer-stack.el"))

(defcustom buffer-stack-frame-local t
  "Does each frame maintain a seperate buffer stack?
If you switch this off during a GNU Emacs session, the initial buffer
ordering might be strange."
  :type 'boolean
  :group 'buffer-stack)

(defcustom buffer-stack-quiet t
  "No beeping."
  :type 'boolean
  :group 'buffer-stack)

(defcustom buffer-stack-show-position 'buffer-stack-show-position-number
  "How do we display our position in the stack while switching?
\"as number\" prints something like \"BUFFER: 2/4\". \"as surrounding
buffers\" prints something like \"DOWN: *Next Buffer* ---- UP: *Last
Buffer*\". If you supply your own function, it should take two
arguments: the current index in the stack and the stack itself."
  :type '(choice (const :tag "as number" buffer-stack-show-position-number)
                 (const :tag "as surrounding buffers" buffer-stack-show-position-buffers)
                 (function :tag "using my function")
                 (const :tag "don't show position" nil))
  :group 'buffer-stack)

(defcustom buffer-stack-filter 'buffer-stack-filter-exclusive
  "How do we filter the stack?
Exclusive means explicitly untracked buffers are hidden while all
others are shown. Inclusive means explicitly tracked buffers are shown
while all others are hidden. If you supply your own function, it
should take a buffer and return non-nil if that buffer is to be
included in the stack."
  :type '(choice (const :tag "exclusive" buffer-stack-filter-exclusive)
                 (const :tag "inclusive" buffer-stack-filter-inclusive)
                 (function :tag "using my function"))
  :group 'buffer-stack)

(defcustom buffer-stack-untracked
  '("KILL" "*Compile-Log*" "*Compile-Log-Show*"
    "*Group*" "*Completions*" "*Messages*")
  "The list of buffer names to hide when switching through the stack.
  This is used only with `buffer-stack-filter-exclusive', and is in
addition to permanently-hidden buffers which start with a space."
  :type '(repeat string)
  :group 'buffer-stack)

(defcustom buffer-stack-tracked nil
  "The list of buffer names to show when switching through the stack.
This is used only with `buffer-stack-filter-inclusive'."
  :type '(repeat string)
  :group 'buffer-stack)

;;; private variables

;; these all only have meaning during switching
(defvar buffer-stack-last-frame nil
  "The frame we're moving in.")
(defvar buffer-stack-last-buffer nil
  "The buffer we last put on top.")
(defvar buffer-stack-last-filter nil
  "Last filter used for switching.")
(defvar buffer-stack-index nil
  "Our position in the stack.")
(defvar buffer-stack nil
  "Stack of buffers in order, from most recent to least.")

;;; interactive functions

;;;###autoload
(defun buffer-stack-track ()
  "Track the current buffer.
Remove it from the untracked list, and add it to the tracked list."
  (interactive)
  (setq buffer-stack-untracked (delete (buffer-name (current-buffer)) buffer-stack-untracked))
  (add-to-list 'buffer-stack-tracked (buffer-name (current-buffer))))

;;;###autoload
(defun buffer-stack-untrack ()
  "Untrack the current buffer.
Remove it from the tracked list, and add it to the untracked list."
  (interactive)
  (add-to-list 'buffer-stack-untracked (buffer-name (current-buffer)))
  (setq buffer-stack-tracked (delete (buffer-name (current-buffer)) buffer-stack-tracked)))

;;;###autoload
(defun buffer-stack-down ()
  "Move down in the buffer stack.
Down is the direction of less-recent buffers."
  (interactive)
  (buffer-stack-move 1)
  (buffer-stack-show-position))

;;;###autoload
(defun buffer-stack-up ()
  "Move up in the buffer stack.
If you were switching, up is where you came from."
  (interactive)
  (buffer-stack-move -1)
  (buffer-stack-show-position))

;;;###autoload
(defun buffer-stack-bury-and-kill ()
  "Bury the current buffer, then kill it.
Civilized people kill BEFORE burying, but who's civilized here? This
command counts as switching."
  (interactive)
  (let ((buffer (current-buffer)))
    (buffer-stack-bury)
    (kill-buffer buffer)
    (setq buffer-stack (buffer-stack-clean buffer-stack))
    (when (null buffer-stack)
      ;; we killed the last tracked buffer -- do a "mini-rebuild"
      (setq buffer-stack (list (current-buffer)))
      (setq buffer-stack-last-buffer (current-buffer)))
    (buffer-stack-show-position)))

;;;###autoload
(defun buffer-stack-bury ()
  "Bury the current buffer and move to the next in the stack.
This command counts as switching, meaning you can do it while
switching buffers and then continue switching buffers."
  (interactive)
  (if (and (buffer-stack-switching-p) (>= buffer-stack-index (- (length buffer-stack) 1)))
      (progn
        (or buffer-stack-quiet
            (beep))
        (message "Tried to bury bottom-most buffer!")
        (unless (= (length buffer-stack) 1)
          (buffer-stack-move -1)))
    (let ((buffer (current-buffer)))
      (when (buffer-stack-switching-p)
        (buffer-stack-assert-not-empty)
        ;; send to bottom of stack
        (setq buffer-stack (delq buffer buffer-stack))
        (when (buffer-stack-tracked-p buffer)
          (setq buffer-stack (nconc buffer-stack (list buffer)))))
      ;; bury in the real list
      (buffer-stack-bury-buffer buffer)
      ;; pull the "next" buffer to the top
      (buffer-stack-move 0)
      (if (= (length buffer-stack) 1)
          ;; buffer-stack-move already beeped
          (message "Tried to bury bottom-most buffer!")
        (buffer-stack-show-position)))))

;;; private functions

(defun buffer-stack-move (direction)
  "Move through the stack by one buffer.
This is THE switching command; all other motions are based on this."
  (setq this-command 'buffer-stack-move)
  ;; The principle is, if we're starting a switch, first build a list
  ;; of buffers to switch between. If we're doing a switch, use this
  ;; list to find the buffer we're supposed to jump to, and bubble it
  ;; with the current buffer.
  (unless (buffer-stack-switching-p)
    ;; prepare a stack
    (setq buffer-stack-index 0)
    (buffer-stack-rebuild)
    (setq buffer-stack-last-frame (selected-frame))
    (setq buffer-stack-last-filter buffer-stack-filter)
    (unless (buffer-stack-tracked-p (current-buffer))
      (setq buffer-stack (cons (current-buffer) buffer-stack))))
  (buffer-stack-assert-not-empty)
  ;; This relies on the fact that the buffer list doesn't change
  ;; during switching.
  (let ((max-index (- (length buffer-stack) 1))
        (buffer (current-buffer)))
    ;; the following sets `buffer' to the target buffer
    (if (= max-index 0)
        ;; only one buffer, so we don't have to move
        (or buffer-stack-quiet
            (beep))
      ;; find the new index
      (if (> direction 0)
          (incf buffer-stack-index)
        (if (< direction 0)
            (decf buffer-stack-index)))
      (if (< buffer-stack-index 0)
          ;; go backwards to the last buffer
          (progn (setq buffer-stack-index max-index)
                 ;; this works correctly in GNU Emacs
                 (setq buffer (nth buffer-stack-index buffer-stack))
                 (or buffer-stack-quiet
                     (beep)))
        (if (> buffer-stack-index max-index)
            ;; wrap to the first buffer
            (progn (setq buffer-stack-index 0)
                   (buffer-stack-bury-buffer (current-buffer))
                   (setq buffer (first buffer-stack))
                   (or buffer-stack-quiet
                       (beep)))
          ;; the usual case, we put the top buffer before the indexed
          ;; buffer and the indexed buffer on top
          (setq buffer (nth buffer-stack-index buffer-stack))
          (unless (eq (current-buffer) buffer)
            ;; If we were already at the top of the stack and moved 0,
            ;; this would try and bury the buffer before itself -- so
            ;; don't do it in that case.
            (buffer-stack-bury-buffer (current-buffer) buffer)))))
    ;; now we move
    (switch-to-buffer buffer)
    (setq buffer-stack-last-buffer buffer)))

(defun buffer-stack-bury-buffer (buffer &optional before)
  "Emulate xemacs's bury-buffer for GNU Emacs."
  (if (featurep 'xemacs) (bury-buffer buffer before)
    ;; GNU Emacs 21.2.1 has a bug where it buries the buffer in the
    ;; wrong frame, so here we reimplement burying a buffer.
    ;; Unfortunately we do not bury in the global list like we should,
    ;; so if you switch frame-local off the global list will
    ;; probably be wrong. Why are you switching frame-local off?
    (if buffer-stack-frame-local
        (let* ((frame (selected-frame))
               (new-list (buffer-list frame))
               (rest new-list))
          (setq new-list (delq buffer new-list))
          (if (null new-list)
              (setq new-list (list buffer))
            (if (null before)
                (setq new-list (nconc new-list (list buffer)))
              (if (eq before (car new-list))
                  (setq new-list (cons buffer new-list))
                (while (not (or (null (cdr rest)) (eq (cadr rest) before)))
                  (setq rest (cdr rest)))
                (setcdr rest (cons buffer (cdr rest)))
                )))
          (modify-frame-parameters frame (list (cons 'buffer-list new-list))))
      ;; This works on the global list and will work for the frame
      ;; list too once that bug is fixed.
      (dolist (b (buffer-list nil))
        (when (eq b before)
          (bury-buffer buffer))
        (unless (eq b buffer)
          (bury-buffer b))))))

(defun buffer-stack-rebuild ()
  "Create `buffer-stack' from the buffer list."
  (setq buffer-stack (buffer-stack-clean (buffer-list (buffer-stack-frame)))))

(defun buffer-stack-clean (buffer-list)
  "Remove untracked buffers from a list by side effect."
  (let ((rest buffer-list)
	buffer
        last)
    (while (not (null rest))
      (setq buffer (car rest))
      (if (buffer-stack-tracked-p buffer)
          (setq last rest)
        (if last
            (setcdr last (cdr rest))
          (setq buffer-list (cdr rest))))
      (setq rest (cdr rest)))
    buffer-list))

(defun buffer-stack-switching-p ()
  "Are we switching buffers?"
  (and (eq last-command 'buffer-stack-move)
       ;; this is not perfect, but should catch 99% of the cases where
       ;; things change behind our backs
       (eq buffer-stack-last-frame (selected-frame))
       (eq buffer-stack-last-buffer (current-buffer))
       (eq buffer-stack-last-filter buffer-stack-filter)))

(defun buffer-stack-frame ()
  (if buffer-stack-frame-local
      (selected-frame)
    (if (featurep 'xemacs)
        t
      nil)))

(defun buffer-stack-assert-not-empty ()
  (if (null buffer-stack)
      (error "The buffer stack is empty! Please report this as a bug.")))

;;; show position

(defun buffer-stack-show-position ()
  "Print the current position."
  (buffer-stack-assert-not-empty)
  (unless (null buffer-stack-show-position)
    (funcall buffer-stack-show-position buffer-stack-index buffer-stack)))

(defun buffer-stack-show-position-number (buffer-stack-index buffer-stack)
  "Show position like this: BUFFER 1/3
That's number/total."
  (message (concat "BUFFER: "
                   (prin1-to-string (+ buffer-stack-index 1))
                   "/"
                   (prin1-to-string (length buffer-stack)))))

(defun buffer-stack-show-position-buffers (buffer-stack-index buffer-stack)
  "Show position like this: DOWN: *Next Buffer* ---- UP: *Previous Buffer*"
  (let (up-buffer-index
        down-buffer-index
        (max-index (- (length buffer-stack) 1)))
    (if (eq buffer-stack-index 0)
        (setq up-buffer-index max-index)
      (setq up-buffer-index (- buffer-stack-index 1)))
    (if (eq buffer-stack-index max-index)
        (setq down-buffer-index 0)
      (setq down-buffer-index (+ buffer-stack-index 1)))
    (message (concat "DOWN: "
                     (buffer-name (nth down-buffer-index buffer-stack))
                     " ---- " "UP: "
                     (buffer-name (nth up-buffer-index buffer-stack))))
    ))

;;; filter stack

(defun buffer-stack-tracked-p (buffer)
  "Is this buffer tracked?"
  (funcall buffer-stack-filter buffer))

(defun buffer-stack-filter-exclusive (buffer)
  "Non-nil if buffer is not in buffer-stack-untracked or a 'hidden' buffer."
  (let ((name (buffer-name buffer)))
    (not (or (null name)
             (char-equal ?  (string-to-char name))
             (member name buffer-stack-untracked)))))

(defun buffer-stack-filter-inclusive (buffer)
  "Non-nil if buffer is in buffer-stack-tracked."
  (member (buffer-name buffer) buffer-stack-tracked))

;;; buffer-stack.el ends here
