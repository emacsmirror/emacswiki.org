;;; lusty-explorer.el --- Dynamic filesystem explorer and buffer switcher
;;
;; Copyright (C) 2008 - 2010 Stephen Bach <this-file@sjbach.com>
;;
;; Version: 2.0
;; Created: January 26, 2009
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22 and 23
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
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
;; And then use as you would `find-file' or `switch-to-buffer'. A split window
;; shows the *Lusty-Matches* buffer, which updates dynamically as you type
;; using a fuzzy matching algorithm.  One entry is highlighted; you can move
;; the highlight using C-n / C-p.  Pressing TAB or RET will select the
;; highlighted entry.
;;
;;; Customization:
;;  --------------
;;
;; To modify the keybindings, use something like:
;;
;;   (add-hook 'lusty-setup-hook 'my-lusty-hook)
;;   (defun my-lusty-hook ()
;;     (define-key lusty-mode-map "\C-j" 'lusty-highlight-next))
;;
;; Respects these variables:
;;   completion-ignored-extensions
;;
;; Latest release: <http://www.emacswiki.org/cgi-bin/wiki/LustyExplorer>
;; Development:    <http://github.com/sjbach/lusty/tree/master>
;;

;;; Contributors:
;;
;; Jan Rehders
;; Hugo Schmitt
;; Volkan Yazici
;;

;;; Code:

;; Used for many functions and macros.
(require 'cl)

;; Used only for its faces (for color-theme).
(require 'font-lock)

(defgroup lusty-explorer nil
  "Quickly open new files or switch among open buffers."
  :group 'extensions
  :group 'convenience
  :version "23")

(defcustom lusty-setup-hook nil
  "Hook run after the lusty keymap has been setup.
Additional keys can be defined in `lusty-mode-map'."
  :type 'hook
  :group 'lusty-explorer)

(defvar lusty-match-face font-lock-function-name-face)
(defvar lusty-directory-face font-lock-type-face)
(defvar lusty-slash-face font-lock-keyword-face)
(defvar lusty-file-face font-lock-string-face)

(defvar lusty-buffer-name " *Lusty-Matches*")
(defvar lusty-column-separator "    ")
(defvar lusty-no-entries-string
  (propertize "-- NO ENTRIES --" 'face 'font-lock-warning-face))
(defvar lusty-truncated-string
  (propertize "-- TRUNCATED --" 'face 'font-lock-comment-face))

(defvar lusty-mode-map nil
  "Minibuffer keymap for `lusty-file-explorer' and `lusty-buffer-explorer'.")

;;;###autoload
(defun lusty-file-explorer ()
  "Launch the file/directory mode of LustyExplorer."
  (interactive)
  (lusty--define-mode-map)
  (let* ((lusty--active-mode :file-explorer)
         (lusty--ignored-extensions
          (mapcar (lambda (ext) (concat (regexp-quote ext) "$"))
                  completion-ignored-extensions))
         (minibuffer-local-filename-completion-map lusty-mode-map)
         (file (lusty--run 'read-file-name)))
    (when file
      (switch-to-buffer
       (find-file-noselect
        (expand-file-name file))))))

;;;###autoload
(defun lusty-buffer-explorer ()
  "Launch the buffer mode of LustyExplorer."
  (interactive)
  (lusty--define-mode-map)
  (let* ((lusty--active-mode :buffer-explorer)
         (minibuffer-local-completion-map lusty-mode-map)
         (buffer (lusty--run 'read-buffer)))
    (when buffer
      (switch-to-buffer buffer))))

;;;###autoload
(defun lusty-highlight-next ()
  "Highlight the next entry in *Lusty-Matches*."
  (interactive)
  (incf lusty--highlighted-index)
  (lusty-refresh-matches-buffer))

;;;###autoload
(defun lusty-highlight-previous ()
  "Highlight the previous entry in *Lusty-Matches*."
  (interactive)
  (decf lusty--highlighted-index)
  (when (minusp lusty--highlighted-index)
    (setq lusty--highlighted-index 0))
  (lusty-refresh-matches-buffer))

;;;###autoload
(defun lusty-select-entry ()
  "Select the highlighted entry in *Lusty-Matches*."
  (interactive)
  (when lusty--previous-printed-matches
    (let ((selected-entry (nth lusty--highlighted-index
                               lusty--previous-printed-matches)))
      (ecase lusty--active-mode
        (:file-explorer (lusty--file-explorer-select selected-entry))
        (:buffer-explorer (lusty--buffer-explorer-select selected-entry))))))

;; TODO:
;; - highlight opened buffers in filesystem explorer
;; - FIX: deal with permission-denied


(defvar lusty--active-mode nil)
(defvar lusty--wrapping-ido-p nil)
(defvar lusty--initial-window-config nil)
(defvar lusty--previous-minibuffer-contents nil)
(defvar lusty--ignored-extensions nil)
(defvar lusty--highlighted-index 0)
(defvar lusty--previous-printed-matches '())

(defun lusty-sort-by-fuzzy-score (strings abbrev)
  ;; TODO: case-sensitive when abbrev contains capital letter
  (if (or (string= abbrev "")
          (string= abbrev "."))
      (sort strings 'string<)
    (let* ((all-entries (mapcar (lambda (s) (cons s (LM-score s abbrev)))
                                strings))
           (filtered (remove-if (lambda (c) (zerop (cdr c)))
                                all-entries))
           (sorted (sort filtered
                         (lambda (a b) (< (cdr b) (cdr a))))))
      (mapcar 'car sorted))))

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
                 lusty--ignored-extensions)))
    (remove-if 'ignored-p
               (if (hidden-p file-portion)
                   (remove-if 'pwd-p files)
                 (remove-if 'hidden-p files)))))

(defun lusty-set-minibuffer-text (&rest args)
  "Sets ARGS into the minibuffer after the prompt."
  (assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point-max))
  (apply 'insert args))

(defun lusty--file-explorer-select (entry)
  (let* ((path (minibuffer-contents-no-properties))
         (var-completed-path (lusty-complete-env-variable path)))
    (if var-completed-path
        ;; We've completed a variable name (at least partially) -- set it and
        ;; leave, since it's confusing to do two kinds of completion at once.
        (lusty-set-minibuffer-text var-completed-path)
      (let* ((dir (file-name-directory path))
             (file-portion (file-name-nondirectory path))
             (normalized-dir (lusty-normalize-dir dir)))
        ;; Clean up the path when selecting in case we recurse
        (lusty-set-minibuffer-text normalized-dir entry)
        (if (file-directory-p (concat normalized-dir entry))
            (lusty-refresh-matches-buffer)
          (minibuffer-complete-and-exit))))))

(defun lusty--buffer-explorer-select (entry)
  (lusty-set-minibuffer-text entry)
  (minibuffer-complete-and-exit))

;; This may seem overkill, but it's the only way I've found to update the
;; matches list for every edit to the minibuffer.  Wrapping the keymap can't
;; account for user bindings or commands and would also fail for viper.
(defun lusty--post-command-function ()
  (assert lusty--active-mode)
  (when (and (minibufferp)
             (or (null lusty--previous-minibuffer-contents)
                 (not (string= lusty--previous-minibuffer-contents
                               (minibuffer-contents-no-properties)))))

    (when (null lusty--initial-window-config)
      ;; (Only run when the explorer function is initially executed.)
      (lusty--setup-matches-window))

    (setq lusty--previous-minibuffer-contents (minibuffer-contents-no-properties)
          lusty--highlighted-index 0)
    (lusty-refresh-matches-buffer)))

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

(defun lusty--setup-matches-window ()
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
        (loop repeat 5
              while (< (window-width) (frame-width))
              do
              (condition-case nil
                  (enlarge-window-horizontally (- (frame-width)
                                                  (window-width)))
                (error
                 (return))))
        (set-window-buffer new-lowest lusty-buffer))))
  ;;
  ;; Window configuration may be restored intermittently.
  (setq lusty--initial-window-config (current-window-configuration)))

(defun lusty-refresh-matches-buffer ()
  "Refresh *Lusty-Matches*."
  (assert (minibufferp))
  (let* ((minibuffer-text (if lusty--wrapping-ido-p
                              ido-text
                            (minibuffer-contents-no-properties)))
         (matches
          (ecase lusty--active-mode
            (:file-explorer (lusty-file-explorer-matches minibuffer-text))
            (:buffer-explorer (lusty-buffer-explorer-matches minibuffer-text)))))

    (multiple-value-bind (truncated-matches truncated-p)
        (lusty-truncate-entries matches)
      (setq lusty--previous-printed-matches truncated-matches)

      ;; Update the matches window.
      (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
        (with-current-buffer lusty-buffer
          (setq buffer-read-only t)
          (let ((buffer-read-only nil))
            (erase-buffer)
            (lusty--display-entries truncated-matches truncated-p)
            (goto-char (point-min))))

        ;; If only our matches window is open,
        (when (one-window-p t)
          ;; Restore original window configuration before fitting the
          ;; window so the minibuffer won't grow and look silly.
          (set-window-configuration lusty--initial-window-config))
        (fit-window-to-buffer (display-buffer lusty-buffer)
                              ; TODO vvv do smarter
                              (- (frame-height) 3))
        (set-buffer-modified-p nil)))))

(defun lusty-buffer-explorer-matches (text)
  (let* ((buffers (lusty-filter-buffers (buffer-list))))
    (lusty-sort-by-fuzzy-score 
     buffers
     text)))

(defun lusty-file-explorer-matches (path)
  (let* ((dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         (files
          (and dir
               ; NOTE: directory-files is quicker but
               ;       doesn't append slash for directories.
               ;(directory-files dir nil nil t)
               (file-name-all-completions "" dir))))
    (lusty-sort-by-fuzzy-score
     (lusty-filter-files file-portion files)
     file-portion)))

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

(defun lusty-truncate-entries (entries)
  (let* ((lusty-buffer (get-buffer-create lusty-buffer-name))
         (max-possibly-displayable-entries
          (with-current-buffer lusty-buffer
            (* (- (frame-height) 3)
               (/ (window-width)
                  (1+ (length lusty-column-separator))))))
         (cut-off-point (nthcdr max-possibly-displayable-entries entries))
         (truncate-p (consp cut-off-point)))

    (when truncate-p
      ;; Trim down the entries
      (setf (cdr cut-off-point) nil))

    (values entries truncate-p)))

(defun* lusty--display-entries (entries truncated-p)

  (when (endp entries)
    (lusty--print-no-entries)
    (return-from lusty--display-entries))

  (let ((propertized
         ; Add font faces to the entries
         (loop with len = (length entries)
               with highlighted-index = (mod lusty--highlighted-index len)
               for e in entries
               for i from 0
               when (= i highlighted-index)
               collect (propertize e 'face lusty-match-face)
               else
               collect (lusty-propertize-path e))))

    (loop for column-count downfrom (lusty-column-count-upperbound propertized)
          ;; FIXME this is calculated one too many times in the degenerate
          ;; case.
          for columns = (lusty-columnize propertized column-count)
          for widths = (mapcar 'lusty-longest-length columns)
          for full-width = (+ (reduce '+ widths)
                              (* (length lusty-column-separator)
                                 (1- column-count)))
          until (or (<= column-count 1)
                    (< full-width (window-width)))
          finally
          (when (<= column-count 1)
            (setq columns (list propertized)
                  widths (list 0)))
          (lusty--print-columns columns widths)))

  (when truncated-p
    (lusty--print-truncated)))
  
(defun lusty--print-no-entries ()
  (insert lusty-no-entries-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty--print-truncated ()
  (insert lusty-truncated-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty--print-columns (columns widths)
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

(defun lusty-columnize (entries n-columns)
  "Split ENTRIES into N-COLUMNS sublists."
  (let ((n-rows (ceiling (length entries) n-columns))
        (sublists))
    (while entries
      (push (subseq entries 0 (min n-rows (length entries)))
            sublists)
      (setq entries (nthcdr n-rows entries)))
    (nreverse sublists)))

(defun lusty--define-mode-map ()
  ;; Re-generated every run so that it can inherit new functions.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; TODO: perhaps RET should be:
    ;; - if buffer explorer, same as \t
    ;; - if file explorer, opens current name (or recurses if existing dir)
    (define-key map (kbd "RET") 'lusty-select-entry)
    (define-key map "\t" 'lusty-select-entry)
    (define-key map "\C-n" 'lusty-highlight-next)
    (define-key map "\C-p" 'lusty-highlight-previous)
    (setq lusty-mode-map map))
  (run-hooks 'lusty-setup-hook))

(defun lusty--run (read-fn)
  (let ((lusty--highlighted-index 0)
        (lusty--previous-printed-matches '()))
    (add-hook 'post-command-hook 'lusty--post-command-function t)
    (unwind-protect 
        (save-window-excursion
          (funcall read-fn ">> "))
      (remove-hook 'post-command-hook 'lusty--post-command-function)
      (setq lusty--previous-minibuffer-contents nil
            lusty--initial-window-config nil))))


;;
;; Start LiquidMetal
;;
;; Port of Ryan McGeary's LiquidMetal fuzzy matching algorithm found at:
;;   http://github.com/rmm5t/liquidmetal/tree/master.
;;

(defconst LM--score-no-match 0.0)
(defconst LM--score-match 1.0)
(defconst LM--score-trailing 0.8)
(defconst LM--score-trailing-but-started 0.90)
(defconst LM--score-buffer 0.85)

(defun LM-score (str abbrev)
  (cond ((string= abbrev "")
         LM--score-trailing)
        ((> (length abbrev) (length str))
         LM--score-no-match)
        (t

         (let* ((scores (LM--build-score-array str abbrev))
                (sum (reduce '+ scores)))
           (/ sum (length scores))))))

(defun* LM--build-score-array (str abbrev)
  (let ((scores (make-vector (length str) LM--score-no-match))
        (lower (downcase str))
        (last-index -1)
        (started-p nil))
    (loop for c across (downcase abbrev)
          for i = (position c lower :start (1+ last-index))
          do
        (when (null i)
          (fillarray scores LM--score-no-match)
          (return-from LM--build-score-array scores))
        (when (= i 0)
          (setq started-p t))
        (cond ((and (> i 0)
                    (let ((C (aref str (1- i))))
                      (or (char-equal C ?\ )
                          (char-equal C ?\t)
                          (char-equal C ?/)
                          (char-equal C ?.)
                          (char-equal C ?_)
                          (char-equal C ?-))))
               (aset scores (1- i) LM--score-match)
               (fill scores LM--score-buffer :start (1+ last-index) :end (1- i)))
              ((and (>= (aref str i) ?A)
                    (<= (aref str i) ?Z)
               (fill scores LM--score-buffer :start (1+ last-index) :end i)))
              (t
               (fill scores LM--score-no-match :start (1+ last-index) :end i)))
        (aset scores i LM--score-match)
        (setq last-index i))

    (let ((trailing-score
           (if started-p
               LM--score-trailing-but-started
             LM--score-trailing)))
      (fill scores trailing-score :start (1+ last-index))
      scores)))

;;
;; End LiquidMetal
;;


(provide 'lusty-explorer)

;;; lusty-explorer.el ends here.
