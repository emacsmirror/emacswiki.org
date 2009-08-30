;;; lusty-explorer.el --- Dynamic filesystem explorer and buffer switcher
;;
;; Copyright (C) 2008 Stephen Bach <this-file@sjbach.com>
;;
;; Version: 1.0.2
;; Created: January 26, 2009
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22 and 23
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from the
;; use of this software.

;;; Commentary:
;;
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'lusty-explorer)
;;
;; To launch the explorer, run or bind the following commands:
;;
;;    M-x lusty-file-explorer
;;    M-x lusty-buffer-explorer
;;
;;  And then use as you would `find-file' or `switch-to-buffer'.  (There are
;;  minor differences in entry selection, e.g. tab-completing when there is a
;;  only a single completion will select that completion.)  A split window
;;  shows the *Lusty-Completions* buffer, which updates dynamically as you
;;  type.
;;
;;  Respects these variables:
;;    completion-ignored-extensions
;;
;;  Latest release: <http://www.emacswiki.org/cgi-bin/wiki/LustyExplorer>
;;  Development:    <http://github.com/sjbach/lusty/tree/master>
;;

;;; Contributors
;; Jan Rehders
;; Hugo Schmitt
;;

;;; Code:

(require 'advice)
(require 'cl)

;; Used only for the completion algorithms in these functions:
;; - iswitchb-get-matched-buffers
;; - iswitchb-set-common-completion
(require 'iswitchb)

;; Used only for its faces (for color-theme).
(require 'font-lock)

(defvar lusty-match-face font-lock-function-name-face)
(defvar lusty-directory-face font-lock-type-face)
(defvar lusty-slash-face font-lock-keyword-face)
(defvar lusty-file-face font-lock-string-face)

(defvar lusty-buffer-name " *Lusty-Completions*")
(defvar lusty-column-separator "    ")
(defvar lusty-no-entries-string
  (propertize "-- NO ENTRIES --" 'face 'font-lock-warning-face))
(defvar lusty-truncated-string
  (propertize "-- TRUNCATED --" 'face 'font-lock-comment-face))

(defvar lusty--active-mode nil)
(defvar lusty--previous-contents nil)
(defvar lusty--initial-window-config nil)
(defvar lusty--completion-ignored-extensions nil)

;;;###autoload
(defun lusty-file-explorer ()
  "Launch the file/directory mode of LustyExplorer"
  (interactive)
  (let* ((lusty--active-mode :file-explorer)
         (lusty--completion-ignored-extensions
          (mapcar (lambda (ext) (concat (regexp-quote ext) "$"))
                  completion-ignored-extensions))
         (file (lusty--run 'read-file-name)))
    (when file
      (switch-to-buffer
       (find-file-noselect
        (expand-file-name file))))))

;;;###autoload
(defun lusty-buffer-explorer ()
  "Launch the buffer mode of LustyExplorer"
  (interactive)
  (let* ((lusty--active-mode :buffer-explorer)
         (buffer (lusty--run 'read-buffer)))
    (when buffer
      (switch-to-buffer buffer))))

;; TODO: completion-ignore-case
;;       (let ((completion-ignore-case (blah))) ...
;;       read-file-name-completion-ignore-case
;; TODO: highlight opened buffers in filesystem explorer

(defun lusty-normalize-dir (dir)
  "Clean up the given directory path."
  (if (and dir (plusp (length dir)))
      (setq dir (abbreviate-file-name
                 (expand-file-name
                  (substitute-in-file-name dir))))
    (setq dir "."))
  (and (file-directory-p dir)
       dir))

(defun lusty-complete-env-variable (path)
  "Look for an environment variable in PATH and try to complete it as
much as possible."
  (when (string-match "\$\\([[:word:]_]+\\)" path)
    (let* ((partial-var (match-string 1 path))
           (vars (mapcar (lambda (x)
                           (string-match "^[^=]+" x)
                           (match-string 0 x))
                         (remove-if-not
                          (lambda (x)
                            (string-match (concat "^" partial-var) x))
                          process-environment)))
           (longest-completion (try-completion partial-var vars)))
      (cond ((eq t longest-completion) nil)
            ((null longest-completion) nil)
            ((> (length longest-completion) (length partial-var))
             (replace-regexp-in-string (concat "\$" partial-var)
                                       (concat "\$" longest-completion)
                                       path t t))))))

(defun lusty-filter-buffers (buffers)
  "Return BUFFERS converted to strings with hidden buffers removed."
  (flet ((ephemeral-p (name)
           (string= (substring name 0 1) " ")))
    (remove-if 'ephemeral-p (mapcar 'buffer-name buffers))))

(defun lusty-filter-files (file-portion files)
  "Return FILES with './' removed and hidden files if FILE-PORTION
does not begin with '.'."
  (flet ((hidden-p (str)
           (char-equal (string-to-char str) ?.))
         (pwd-p (str)
           (string= (directory-file-name str) "."))
         (ignored-p (name)
           (some (lambda (ext) (string-match ext name))
                 lusty--completion-ignored-extensions )))
    (remove-if 'ignored-p
               (if (hidden-p file-portion)
                   (remove-if 'pwd-p files)
                 (remove-if 'hidden-p files)))))

(defun lusty-set-minibuffer-text (&rest args)
  "Sets ARGS into the minibuffer after the prompt."
  (assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point-max))
  (apply 'insert args))

(defadvice minibuffer-complete (around lusty-completion-wrapper activate)
  "Circumvent default completion in *Completions* window."
  (ecase lusty--active-mode
    (:file-explorer (lusty-file-explorer-minibuffer-tab-complete))
    (:buffer-explorer (lusty-buffer-explorer-minibuffer-tab-complete))
    ((nil) ad-do-it)))

(defun lusty-file-explorer-minibuffer-tab-complete ()
  (let* ((path (minibuffer-contents-no-properties))
         (var-completed-path (lusty-complete-env-variable path)))
    (if var-completed-path
        ;; We've completed a variable name (at least partially) -- set it and
        ;; leave, since it's confusing to do two kinds of completion at once.
        (lusty-set-minibuffer-text var-completed-path)
      (let* ((dir (file-name-directory path))
             (file-portion (file-name-nondirectory path))
             (normalized-dir (lusty-normalize-dir dir))
             (completion (and normalized-dir
                              ;; TODO: let bind
                              ;; read-file-name-completion-ignore-case based on
                              ;; whether file-portion includes uppercase
                              (file-name-completion file-portion
                                                    normalized-dir))))
        (unless (string= dir normalized-dir)
          ;; Clean up the path in the minibuffer.
          (lusty-set-minibuffer-text normalized-dir file-portion))
        (cond ((null completion))
              ((eq t completion)
               (minibuffer-complete-and-exit))
              ((> (length completion) (length file-portion))
               (insert (substring completion (length file-portion))))))
      (lusty-update-completion-buffer t))))

(defun lusty-buffer-explorer-minibuffer-tab-complete ()
  (let* ((contents (minibuffer-contents-no-properties))
         (buffers (lusty-filter-buffers (buffer-list)))
         (completion (let ((iswitchb-text contents)
                           (iswitchb-matches buffers))
                       (iswitchb-set-common-completion))))
    (cond ((null completion))
          ((eq t completion)
           (minibuffer-complete-and-exit))
          ((> (length completion) (length contents))
           (delete-region (minibuffer-prompt-end) (point-max))
           (insert completion))))
  (lusty-update-completion-buffer t))

;; This may seem overkill, but it's the only way I've found to update the
;; completion list for every edit to the minibuffer.  Wrapping the keymap can't
;; account for user bindings or commands and would also fail for viper.
(defun lusty--post-command-function ()
  (assert lusty--active-mode)
  (when (and (minibufferp)
             (or (null lusty--previous-contents)
                 (not (string= lusty--previous-contents
                               (minibuffer-contents-no-properties)))))
    (unless lusty--initial-window-config
      ;; (Only run when the explorer function is initially executed.)
      (lusty-setup-completion-window)
      ;;
      ;; Window configuration may be restored intermittently.
      (setq lusty--initial-window-config (current-window-configuration)))

    ;; TODO: check that last 'element' in minibuffer string is valid
    ;; if last char is a '/'
    ;;  if last directory is all lower case
    ;;   if an all lower case directory of that name exists
    ;;     do nothing
    ;;   else if a mixed case directory of that name exists
    ;;     convert directory name to mixed case
    ;;     remember to add back the '/'
    ;;     update the minibuffer contents
    ;;   
    (setq lusty--previous-contents (minibuffer-contents-no-properties))
    (lusty-update-completion-buffer)))

;; Cribbed with modification from tail-select-lowest-window.
(defun lusty-lowest-window ()
  "Return the lowest window on the frame."
  (let* ((current-window (if (minibufferp)
                             (next-window (selected-window) :skip-mini)
                           (selected-window)))
         (lowest-window current-window)
         (bottom-edge (car (cdr (cdr (cdr (window-edges current-window))))))
         (last-window (previous-window current-window :skip-mini))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window current-window :skip-mini))
             (next-bottom-edge (cadr (cddr (window-edges this-window)))))
        (when (< bottom-edge next-bottom-edge)
          (setq bottom-edge next-bottom-edge)
          (setq lowest-window this-window))
        (setq current-window this-window)
        (when (eq last-window this-window)
          (setq window-search nil))))
    lowest-window))

(defun lusty-setup-completion-window ()
  (let ((lowest-window (lusty-lowest-window))
        (lusty-buffer (get-buffer-create lusty-buffer-name)))
    (save-selected-window
      (select-window lowest-window)
      (let ((new-lowest
             ;; Create the window for lusty-buffer
             (split-window-vertically)))
        (select-window new-lowest)
        ;; Try to get a window covering the full frame.  Sometimes
        ;; this takes more than one try, but we don't want to do it
        ;; infinitely in case of weird setups.
        (loop repeat 3
              while (< (window-width) (frame-width))
              do (enlarge-window-horizontally (- (frame-width)
                                                 (window-width))))
        (set-window-buffer new-lowest lusty-buffer)))))

(defun lusty-update-completion-buffer (&optional tab-pressed-p)
  (assert (minibufferp))
  (multiple-value-bind (completions match single-selectable-completion)
      (ecase lusty--active-mode
        (:file-explorer (lusty-file-explorer-completions))
        (:buffer-explorer (lusty-buffer-explorer-completions)))

    (if (and tab-pressed-p
             single-selectable-completion)
        ;;
        ;; The user pressed TAB (or some other completion key) and we're
        ;; fully completable -- go ahead and choose it.
        (progn
          (lusty-set-minibuffer-text single-selectable-completion)
          (minibuffer-complete-and-exit))
      ;;
      ;; Update the completion window.
      (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
        (with-current-buffer lusty-buffer
          (setq buffer-read-only t)
          (let ((buffer-read-only nil))
            (erase-buffer)
            (lusty-display-completion-list completions match)
            (goto-char (point-min))

            ;; If only our completions window is open,
            (when (one-window-p t)
              ;; Restore original window configuration before fitting the
              ;; window so the minibuffer won't grow and look silly.
              (set-window-configuration lusty--initial-window-config))
            (fit-window-to-buffer (display-buffer lusty-buffer)
                                  ; TODO vvv do smarter
                                  (- (frame-height) 3)))
          (set-buffer-modified-p nil))))))

(defun lusty-buffer-explorer-completions ()
  (let* ((contents (minibuffer-contents-no-properties))
         (buffers (lusty-filter-buffers (buffer-list)))
         (completions (let ((iswitchb-text contents))
                        (iswitchb-get-matched-buffers contents nil buffers)))
         (match (find contents completions :test 'string=))
         (unique-completion
          ;; Only one entry.
          (let ((first-entry (car completions)))
            (and (stringp first-entry)
                 (endp (cdr completions))
                 first-entry))))
    (values completions
            match
            unique-completion)))

(defun lusty-file-explorer-completions ()
  (let* ((path (minibuffer-contents-no-properties))
         (dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         (completions
          (sort (and dir
                     (lusty-filter-files
                      file-portion
                      ;; TODO: let bind read-file-name-completion-ignore-case 
                      ;; based on whether file-portion includes uppercase
                      (file-name-all-completions file-portion dir)))
                'string<))
         (match (find file-portion completions :test 'string=))
         (unique-completion
          ;; Only one entry and not a directory.
          (let ((first-entry (car completions)))
            (and (stringp first-entry)
                 (endp (cdr completions))
                 (let ((new-path (concat dir first-entry)))
                   (and (not (file-directory-p new-path))
                        new-path))))))
    (values completions
            match
            unique-completion)))

(defun lusty-propertize-path (path)
  "Propertize the given PATH like so: <dir></><dir></><file>
Uses `lusty-directory-face', `lusty-slash-face', `lusty-file-face'"
  (loop with start = 0
        for i from 0
        for c across path
        when (char-equal c ?/) ; <-- FIXME nonportable
        do (put-text-property start i 'face lusty-directory-face path)
           (put-text-property i (1+ i) 'face lusty-slash-face path)
           (setq start (1+ i))
        finally
           (when (not (= start i))
             (put-text-property start i 'face lusty-file-face path))
           (return path)))

(defun lusty-longest-length (lst)
  (loop for item in lst
        maximizing (length item)))

(defun* lusty-display-completion-list (entries match)
  (when (endp entries)
    (lusty-print-no-entries)
    (return-from lusty-display-completion-list))

  (let* ((max-possibly-displayable-entries
          (* (- (frame-height) 3)
             (/ (window-width)
                (1+ (length lusty-column-separator)))))
         (cut-off (nthcdr max-possibly-displayable-entries entries))
         (truncate-p (consp cut-off)))

    (when truncate-p
      (setf (cdr cut-off) nil))

    (setq entries
          (mapcar
           (cond ((endp (cdr entries))
                  (lambda (e) (propertize e 'face lusty-match-face)))
                 (match
                  (lambda (e)
                    (if (eq match e)
                        (propertize e 'face lusty-match-face)
                      (lusty-propertize-path e))))
                 (t
                  'lusty-propertize-path))
           entries))

    (loop for column-count downfrom (lusty-column-count-upperbound entries)
          ;; FIXME this is calculated one too many times in the degenerate
          ;; case.
          for columns = (lusty-columnize entries column-count)
          for widths = (mapcar 'lusty-longest-length columns)
          for full-width = (+ (reduce '+ widths)
                              (* (length lusty-column-separator)
                                 (1- column-count)))
          until (or (<= column-count 1)
                    (< full-width (window-width)))
          finally
          (when (<= column-count 1)
            (setq columns (list entries)
                  widths (list 0)))
          (lusty-print-columns columns widths))

    (when truncate-p
      (lusty-print-truncated))))
  
(defun lusty-print-no-entries ()
  (insert lusty-no-entries-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty-print-truncated ()
  (insert lusty-truncated-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty-print-columns (columns widths)
  (dotimes (i (lusty-longest-length columns))
    (unless (> (line-number-at-pos)
               (- (frame-height) 3)) ;; TODO: determine dynamically
      (loop with row = ""
            for j to (1- (length columns))
            for entry = (nth i (nth j columns))
            for spacer = (make-string (max 0 (- (nth j widths)
                                                (length entry)))
                                      ?\ )
            until (null entry)
            do (setq row (concat row entry spacer lusty-column-separator))
            finally (insert
                     (substring row 0
                                (- (length row)
                                   (length lusty-column-separator)))
                            "\n")))))

;; Get a starting upperbound on the number of columns.
(defun lusty-column-count-upperbound (strings)
  (let ((sorted (sort* (copy-list strings) '< :key 'length))
        (max-width (window-width))
        (sep-len (length lusty-column-separator)))
    (loop for column-count from 0
          for str in sorted
          summing (length str) into length-so-far
          while (< length-so-far max-width)
          do (incf length-so-far sep-len)
          finally (return column-count))))

;; Break entries into sublists representing columns.
(defun lusty-columnize (entries column-count)
  (let ((nrows (ceiling (/ (length entries)
                           (float column-count)))))
    (nreverse
     (mapcar 'nreverse
             (reduce (lambda (lst e)
                       (if (< (length (car lst))
                              nrows)
                           (push e (car lst))
                         (push (list e) lst))
                       lst)
                     entries
                     :initial-value (list (list)))))))

(defun lusty--run (read-fn)
  (add-hook 'post-command-hook 'lusty--post-command-function t)
  (unwind-protect 
      (save-window-excursion
        (funcall read-fn ">> "))
    (remove-hook 'post-command-hook 'lusty--post-command-function)
    (setq lusty--previous-contents nil
          lusty--initial-window-config nil)))

(provide 'lusty-explorer)

;;; lusty-explorer.el ends here.
