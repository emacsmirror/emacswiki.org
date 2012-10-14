;;; minimal-session-saver.el --- Very lean session saver
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/minimal-session-saver
;; URL: http://raw.github.com/rolandwalker/minimal-session-saver/master/minimal-session-saver.el
;; Version: 0.5.2
;; Last-Updated: 14 Sep 2012
;; EmacsWiki: MinimalSessionSaver
;; Keywords: frames, tools, session, project
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'minimal-session-saver)
;;
;;     (minimal-session-saver-install-aliases)
;;
;;     M-x mss-store RET
;;
;;     ;; quit and restart Emacs
;;
;;     M-x mss-load RET
;;
;; Explanation
;;
;; The only information stored by this library is a list of visited
;; files.  Not window configuration, nor point position.
;;
;; Giving a universal prefix argument to any of the interactive
;; session-management commands prompts for the session-state file
;; location, allowing minimal-session-saver to be used as a (very)
;; minimal project manager.
;;
;; To use minimal-session-saver, place the minimal-session-saver.el
;; library somewhere Emacs can find it, and add the following to your
;; ~/.emacs file:
;;
;;     (require 'minimal-session-saver)
;;
;; Five interactive commands are provided to manage sessions:
;;
;;     minimal-session-saver-store
;;     minimal-session-saver-store-frame
;;     minimal-session-saver-load
;;     minimal-session-saver-add-buffer
;;     minimal-session-saver-remove-buffer
;;     minimal-session-saver-mark-stored-buffers
;;
;; without keybindings.
;;
;; An additional command
;;
;;     minimal-session-saver-install-aliases
;;
;; installs shorter command aliases for the above.
;;
;; See Also
;;
;;     M-x customize-group RET minimal-session-saver RET
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     No external dependencies
;;
;; Bugs
;;
;; TODO
;;
;;     Load to a frame
;;
;;     Optional save or prompt on kill-emacs hook
;;
;;     Prompt to save all files before running -store
;;
;;     timestamp in data file header
;;
;;     don't count already marked buffers in buff-menu function
;;
;;     separate history variable?
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for callf, assert, incf, remove-if, remove-if-not
(require 'cl)

;;; customizable variables

;;;###autoload
(defgroup minimal-session-saver nil
  "Very lean session saver."
  :version "0.5.2"
  :link '(emacs-commentary-link "minimal-session-saver")
  :prefix "minimal-session-saver-"
  :group 'tools)

(defcustom minimal-session-saver-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'minimal-session-saver)

(defcustom minimal-session-saver-data-file (expand-file-name "minimal-session-saver-data.el"
                                                             (locate-user-emacs-file "data/minimal-session-saver"))
  "Path to store/retrieve the set of visited files."
  :type 'string
  :group 'minimal-session-saver)

;;; macros

(defmacro minimal-session-saver-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (if (eq 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
      '(called-interactively-p)
    `(called-interactively-p ,kind)))

;;; utility functions

(defun minimal-session-saver-read (path)
  "Read and return the file list from PATH."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun minimal-session-saver-write (path file-list)
  "Write FILE-LIST to PATH."
  (let ((print-level nil)
        (print-length nil))
    (condition-case nil
        (progn
          (assert file-list)
          (with-temp-file path
            (set-buffer-file-coding-system 'utf-8)
            (insert ";; minimal-session-saver data file. -*- coding: utf-8 -*-\n")
            (insert ";; Do not edit this file.\n")
            (prin1 file-list (current-buffer))
            (insert "\n")))
      (error "Cannot save file listing to %s" path))))

(defun minimal-session-saver-mkdir-for-file (path)
  "Create the directory containing PATH, a file."
  (make-directory (file-name-directory path) 'parents))

;;; interactive commands

;;;###autoload
(defun minimal-session-saver-store (&optional path file-list)
  "Save the list of currently visited files to PATH.

Optional FILE-LIST overrides the list of currently visited
files.

With universal prefix argument, enter PATH interactively."
  (interactive)
  (callf or path minimal-session-saver-data-file)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Store visited files to: " path)))
  (minimal-session-saver-mkdir-for-file path)
  (callf or file-list (delq nil (mapcar 'buffer-file-name (buffer-list))))
  (when (or file-list (prog1 (y-or-n-p (propertize "Really store an empty list?" 'face 'highlight)) (message "")))
    (minimal-session-saver-write path file-list)
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (message "Stored %s filenames" (length file-list)))))

;;;###autoload
(defun minimal-session-saver-store-frame (&optional path)
  "Save currently visited files associated with the current frame to PATH.

Requires frame-bufs.el.

With universal prefix argument, enter PATH interactively."
  (interactive)
  (assert (fboundp 'frame-bufs-associated-p) nil "Frame-bufs library not loaded")
  (callf or path minimal-session-saver-data-file)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Store visited files on frame to: " path)))
  (let ((file-list (delq nil (mapcar 'buffer-file-name
                                     (remove-if-not 'frame-bufs-associated-p
                                                    (buffer-list))))))
    (minimal-session-saver-store path file-list)
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (message "Stored %s filenames" (length file-list)))))

;;;###autoload
(defun minimal-session-saver-load (&optional path)
  "Load the saved set of visited files from PATH.

With universal prefix argument, enter PATH interactively."
  (interactive)
  (callf or path minimal-session-saver-data-file)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Load visited files from: " path)))
  (let* ((file-list (minimal-session-saver-read path))
         (nonexistent-list (remove-if 'file-exists-p
                                      (remove-if 'file-remote-p file-list)))
         (visiting-list (remove-if-not 'find-buffer-visiting file-list))
         (reporter (make-progress-reporter "Visiting: " 0 (length file-list)))
         (counter 0)
         (warning ""))
    (unless file-list
      (error "Cannot read visited files at %s" path))
    (dolist (f file-list)
      (progress-reporter-update reporter (incf counter))
      (find-file f))
    (progress-reporter-done reporter)
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (when visiting-list
        (callf concat warning (format ", %s already open" (length visiting-list))))
      (when nonexistent-list
        (callf concat warning (format ", creating %s" (length nonexistent-list))))
      (message "Visited %s files%s" (length file-list) warning))))

;;;###autoload
(defun minimal-session-saver-add-buffer (&optional path buffer)
  "Add the current buffer to the saved set of visited files in PATH.

With universal prefix argument, enter PATH interactively.

BUFFER is optional, and defaults to the currently visited buffer.
When BUFFER is not visiting a file, there is no effect."
  (interactive)
  (callf or path minimal-session-saver-data-file)
  (callf or buffer (current-buffer))
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Add to session listing at: " path)))
  (let* ((file-list (minimal-session-saver-read path))
         (orig-count (length file-list)))
    (add-to-list 'file-list (buffer-file-name buffer))
    (minimal-session-saver-write path file-list)
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (message "Added %s file/s to session listing" (- (length file-list) orig-count)))))

;;;###autoload
(defun minimal-session-saver-remove-buffer (&optional path buffer)
  "Remove the current buffer from the saved set of visited files in PATH.

With universal prefix argument, enter PATH interactively.

BUFFER is optional, and defaults to the currently visited buffer.
When the BUFFER is not visiting a file, or is visiting a file
which was not in the list, there is no effect."
  (interactive)
  (callf or path minimal-session-saver-data-file)
  (callf or buffer (current-buffer))
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Remove from session listing at: " path)))
  (let* ((file-list (minimal-session-saver-read path))
         (orig-count (length file-list)))
    (setq file-list (remove (buffer-file-name buffer) file-list))
    (minimal-session-saver-write path file-list)
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (message "Removed %s file/s from session listing" (- orig-count (length file-list))))))

;;;###autoload
(defun minimal-session-saver-mark-stored-buffers (&optional path char col)
  "Mark buff-menu entries matching the saved set of visited files in PATH.

With universal prefix argument, enter PATH interactively.

Optional CHAR defaults to `buff-menu-marker-char' or ?> if that
variable is not defined.

Optional COL is a 0-indexed position in the line at which to draw
the requested marker character.  Default is 0.

This command can only be called from within a `buff-menu' buffer."
  (interactive)
  (assert (eq major-mode 'Buffer-menu-mode) nil "Not in a buffer-menu buffer")
  (callf or path minimal-session-saver-data-file)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (read-file-name "Read session listing from: " path)))
  (callf or char (if (boundp 'buff-menu-marker-char) buff-menu-marker-char ?>))
  (callf or col 0)
  (let ((inhibit-read-only t)
        (file-list (minimal-session-saver-read path))
        (counter 0))
    (save-excursion
      (Buffer-menu-beginning)
      (while (not (eobp))
        (when (and (Buffer-menu-buffer nil)
                   (member (buffer-file-name (Buffer-menu-buffer nil)) file-list))
          (incf counter)
          (if (fboundp 'ucs-utils-subst-char-in-region)
              (ucs-utils-subst-char-in-region (+ col (point)) (+ 1 col (point)) (char-after (+ col (point))) char)
            (subst-char-in-region (+ col (point)) (+ 1 col (point)) (char-after (+ col (point))) char)))
        (forward-line 1)))
    (when (and (minimal-session-saver-called-interactively-p 'any)
               (not minimal-session-saver-less-feedback))
      (message "Marked %s buffer/s" counter))))

;;;###autoload
(defun minimal-session-saver-install-aliases ()
  "Install aliases outside the \"minimal-session-saver-\" namespace.

The following aliases will be installed

   mss-store                for   minimal-session-saver-store
   mss-store-frame          for   minimal-session-saver-store-frame
   mss-load                 for   minimal-session-saver-load
   mss-add-buffer           for   minimal-session-saver-add-buffer
   mss-remove-buffer        for   minimal-session-saver-remove-buffer
   mss-mark-stored-buffers  for   minimal-session-saver-mark-stored-buffers"
  (interactive)
  (defalias 'mss-store                 'minimal-session-saver-store)
  (defalias 'mss-store-frame           'minimal-session-saver-store-frame)
  (defalias 'mss-load                  'minimal-session-saver-load)
  (defalias 'mss-add-buffer            'minimal-session-saver-add-buffer)
  (defalias 'mss-remove-buffer         'minimal-session-saver-remove-buffer)
  (defalias 'mss-mark-stored-buffers   'minimal-session-saver-mark-stored-buffers))

(provide 'minimal-session-saver)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: MinimalSessionSaver incf callf bufs
;;

;;; minimal-session-saver.el ends here
