;;; minimal-session-saver.el --- Very lean session saver
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/minimal-session-saver
;; URL: http://raw.github.com/rolandwalker/minimal-session-saver/master/minimal-session-saver.el
;; Version: 0.6.2
;; Last-Updated: 29 Oct 2013
;; EmacsWiki: MinimalSessionSaver
;; Keywords: tools, frames, project
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
;; session-management commands causes a prompt for the session-state
;; file location, allowing minimal-session-saver to be used as a
;; (very) minimal project manager.
;;
;; When frame-bufs.el is present, the session associated with a
;; particular frame can be stored and recovered.
;;
;; To use minimal-session-saver, place the minimal-session-saver.el
;; library somewhere Emacs can find it, and add the following to your
;; ~/.emacs file:
;;
;;     (require 'minimal-session-saver)
;;
;; Several interactive commands are provided to manage sessions:
;;
;;     minimal-session-saver-store
;;     minimal-session-saver-load
;;     minimal-session-saver-store-frame         ; requires frame-bufs.el
;;     minimal-session-saver-load-frame          ; requires frame-bufs.el
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
;; installs shorter command aliases for the above, and can
;; be run at autoload-time through a setting in customize.
;;
;; See Also
;;
;;     M-x customize-group RET minimal-session-saver RET
;;
;; Notes
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes, with some limitations
;;     GNU Emacs version 21.x and lower : unknown
;;
;;     Uses if present: frame-bufs.el
;;
;; Bugs
;;
;; TODO
;;
;;     Store multiple sets/frames in same data file and
;;     prompt for choice on load?
;;
;;     Prompt to save all files before running -store
;;
;;     don't count already marked buffers in buff-menu function
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

;;; requirements

;; for callf, assert, incf, remove-if, remove-if-not
(require 'cl)

;;; declarations

(eval-when-compile
  (defvar minimal-session-saver-store-on-exit))

(declare-function frame-bufs-add-buffer "frame-bufs.el")

;;; compatibility functions

(unless (fboundp 'string-match-p)
  ;; added in 23.x
  (defun string-match-p (regexp string &optional start)
    "Same as `string-match' except this function does not change the match data."
    (let ((inhibit-changing-match-data t))
      (string-match regexp string start))))

(unless (fboundp 'locate-user-emacs-file)
  (unless (boundp 'user-emacs-directory)
    (defvar user-emacs-directory "~/.emacs.d/"
      "Directory beneath which additional per-user Emacs-specific files are placed."))
  (defun locate-user-emacs-file (new-name &optional old-name)
    "Return an absolute per-user Emacs-specific file name.
If OLD-NAME is non-nil and ~/OLD-NAME exists, return ~/OLD-NAME.
Else return NEW-NAME in `user-emacs-directory', creating the
directory if it does not exist."
    (convert-standard-filename
     (let* ((home (concat "~" (or init-file-user "")))
            (at-home (and old-name (expand-file-name old-name home))))
       (if (and at-home (file-readable-p at-home))
           at-home
         ;; Make sure `user-emacs-directory' exists,
         ;; unless we're in batch mode or dumping Emacs
         (or noninteractive
             purify-flag
             (file-accessible-directory-p
              (directory-file-name user-emacs-directory))
             (let ((umask (default-file-modes)))
               (unwind-protect
                   (progn
                     (set-default-file-modes ?\700)
                     (make-directory user-emacs-directory))
                 (set-default-file-modes umask))))
         (abbreviate-file-name
          (expand-file-name new-name user-emacs-directory)))))))

;;; customizable variables

;;;###autoload
(defun minimal-session-saver-customize-set-hooks (symbol value)
  "Set function which adds or removes hooks.

SYMBOL and VALUE are passed to `custom-set-default'."
  (custom-set-default symbol value)
  (if minimal-session-saver-store-on-exit
      (add-hook 'kill-emacs-hook 'minimal-session-saver-kill-emacs-hook)
    (remove-hook 'kill-emacs-hook 'minimal-session-saver-kill-emacs-hook)))

;;;###autoload
(defgroup minimal-session-saver nil
  "Very lean session saver."
  :version "0.6.2"
  :link '(emacs-commentary-link :tag "Commentary" "minimal-session-saver")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/minimal-session-saver")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/MinimalSessionSaver")
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

;;;###autoload
(defcustom minimal-session-saver-store-on-exit nil
  "Automatically store the session data every time you quit Emacs.

This value may also be a string representing a separate data file
to be used for store-on-exit session data."
  :set 'minimal-session-saver-customize-set-hooks
  :type '(choice
          (const  :tag "No"   nil)
          (const  :tag "Yes"  t)
          (string :tag "Custom Location"))
  :group 'minimal-session-saver)

;;;###autoload
(defcustom minimal-session-saver-install-short-aliases nil
  "Install short aliases such as `mss-load' for `minimal-session-saver-load'."
  :type 'boolean
  :group 'minimal-session-saver)

;;; variables

(defvar minimal-session-saver-file-name-history nil
  "History of data file names entered in minimal-session-saver.")

;;; aliases

;;;###autoload
(defun minimal-session-saver-install-aliases (&optional arg)
  "Install aliases outside the \"minimal-session-saver-\" namespace.

With optional negative ARG, uninstall aliases.

The following aliases will be installed

   mss-store                for   minimal-session-saver-store
   mss-load                 for   minimal-session-saver-load
   mss-store-frame          for   minimal-session-saver-store-frame
   mss-load-frame           for   minimal-session-saver-load-frame
   mss-add-buffer           for   minimal-session-saver-add-buffer
   mss-remove-buffer        for   minimal-session-saver-remove-buffer
   mss-mark-stored-buffers  for   minimal-session-saver-mark-stored-buffers"
  (let ((syms '(
                store
                load
                store-frame
                load-frame
                add-buffer
                remove-buffer
                mark-stored-buffers
                )))
    (cond
      ((and (numberp arg)
            (< arg 0))
       (dolist (sym syms)
         (fmakunbound (intern (format "mss-%s" sym)))))
      (t
       (dolist (sym syms)
         (defalias (intern (format "mss-%s" sym)) (intern (format "minimal-session-saver-%s" sym))))))))

;;;###autoload
(when minimal-session-saver-install-short-aliases
  (minimal-session-saver-install-aliases))

;;; macros

(defmacro minimal-session-saver-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (cond
    ((not (fboundp 'called-interactively-p))
     '(interactive-p))
    ((condition-case nil
         (progn (called-interactively-p 'any) t)
       (error nil))
     `(called-interactively-p ,kind))
    (t
     '(called-interactively-p))))

;;; utility functions

(defun minimal-session-saver-read-file-name (prompt &optional dir default-filename mustmatch initial predicate)
  "Prompt for a data file for use by minimal-session-saver.

PROMPT, DIR, DEFAULT-FILENAME, MUSTMATCH, INITIAL, and PREDICATE
are as documented for `read-file-name'.

History of input is kept in `minimal-session-saver-file-name-history'."
  (let ((file-name-history minimal-session-saver-file-name-history))
    (prog1
        (read-file-name prompt dir default-filename mustmatch initial predicate)
      (setq minimal-session-saver-file-name-history file-name-history))))

(defun minimal-session-saver-read (path)
  "Read and return the file list from PATH."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun minimal-session-saver-write (path file-list)
  "Write to fully-qualified filename PATH, the contents of FILE-LIST."
  (let ((print-level nil)
        (print-length nil)
        (time (current-time)))
    (when (file-directory-p path)
      (error "PATH is an existing directory, not a file"))
    (when (file-exists-p path)
      (unless
          (string-match-p
           "\\`;+ *minimal-session-saver data file"
           (with-temp-buffer
             (insert-file-contents path)
             (goto-char (point-min))
             (buffer-substring (point-min) (line-end-position))))
        (error "PATH exists and is not a minimal-session-saver data file"))
      (copy-file path (concat path "~") t))
    (condition-case nil
        (progn
          (assert file-list)
          (with-temp-file path
            (set-buffer-file-coding-system 'utf-8)
            (insert ";; minimal-session-saver data file. -*- coding: utf-8 -*-\n")
            (insert ";; Do not edit this file.\n")
            (insert (format ";; Timestamp: %s <%s>\n"
                            (float-time time)
                            (format-time-string "%e %b %Y %r" time)))
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
    (setq path (minimal-session-saver-read-file-name "Store visited files to: " nil path)))
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

When PATH is not supplied, prompts to enter value interactively."
  (interactive)
  (assert (fboundp 'frame-bufs-associated-p) nil "Frame-bufs library not loaded")
  (callf or path 'prompt)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (minimal-session-saver-read-file-name "Store visited files on frame to: " default-directory "")))
  (let ((file-list (delq nil (mapcar 'buffer-file-name
                                     (remove-if-not 'frame-bufs-associated-p
                                                    (buffer-list))))))
    (let ((current-prefix-arg nil))
      (minimal-session-saver-store path file-list))
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
    (setq path (minimal-session-saver-read-file-name "Load visited files from: " nil path)))
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
      (when (> (- (length file-list) (length visiting-list)) 25)
        (callf concat warning " -- it may take a moment for hooks to run"))
      (message "Visited %s files%s" (length file-list) warning)
      (sit-for 1))))

;;;###autoload
(defun minimal-session-saver-load-frame (&optional path)
  "Load the saved set of visited files from PATH into a new frame.

Requires frame-bufs.el.

When PATH is not supplied, prompts to enter value interactively."
  (interactive)
  (assert (fboundp 'frame-bufs-associated-p) nil "Frame-bufs library not loaded")
  (callf or path 'prompt)
  (when (or (consp current-prefix-arg)
            (eq path 'prompt))
    (setq path (minimal-session-saver-read-file-name "Load visited files from: " default-directory "")))
  (let ((file-list (minimal-session-saver-read path))
        (frame nil))
    (with-current-buffer "*scratch*"
      (setq frame (make-frame))
      (let ((current-prefix-arg nil))
        (minimal-session-saver-load path))
      (dolist (f file-list)
        (let ((buf (get-file-buffer f)))
          (when buf
            (frame-bufs-add-buffer buf frame)))))))

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
    (setq path (minimal-session-saver-read-file-name "Add to session listing at: " nil path)))
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
    (setq path (minimal-session-saver-read-file-name "Remove from session listing at: " nil path)))
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
    (setq path (minimal-session-saver-read-file-name "Read session listing from: " nil path)))
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

;;; hooks

;;;###autoload
(defun minimal-session-saver-kill-emacs-hook ()
  "Optionally save session data at shutdown time.

This function has not effect unless the variable
`minimal-session-saver-store-on-exit' is non-nil."
  (when minimal-session-saver-store-on-exit
    (let ((minimal-session-saver-data-file (if (stringp minimal-session-saver-store-on-exit)
                                               minimal-session-saver-store-on-exit
                                             minimal-session-saver-data-file)))
      (minimal-session-saver-store))))

;;;###autoload
(when minimal-session-saver-store-on-exit
  (add-hook 'kill-emacs-hook 'minimal-session-saver-kill-emacs-hook))

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
;; LocalWords: MinimalSessionSaver incf callf bufs MUSTMATCH devel
;;

;;; minimal-session-saver.el ends here
