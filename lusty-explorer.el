;;; lusty-explorer.el --- Dynamic filesystem explorer and buffer switcher
;;
;; Copyright (C) 2008-2010 Stephen Bach <this-file@sjbach.com>
;;
;; Version: 2.1
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
;; Note: lusty-explorer.el benefits greatly from byte-compilation.  To byte-
;; compile this library, M-x byte-compile-file and choose lusty-explorer.el.
;; Then, restart Emacs or M-x load-library and choose the newly generated
;; lusty-explorer.elc file.
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

(declaim (optimize (speed 3) (safety 0)))

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
(defvar lusty-prompt ">> ")
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
         (lusty--ignored-extensions-regex
          (concat (regexp-opt completion-ignored-extensions) "$"))
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
  (when lusty--active-mode
    (incf lusty--highlighted-index)
    (lusty-refresh-matches-buffer :use-previous-matches)))

;;;###autoload
(defun lusty-highlight-previous ()
  "Highlight the previous entry in *Lusty-Matches*."
  (interactive)
  (when lusty--active-mode
    (decf lusty--highlighted-index)
    (when (minusp lusty--highlighted-index)
      (setq lusty--highlighted-index 0))
    (lusty-refresh-matches-buffer :use-previous-matches)))

;;;###autoload
(defun lusty-select-entry ()
  "Select the highlighted entry in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             lusty--previous-printed-matches)
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
(defvar lusty--ignored-extensions-regex nil)
(defvar lusty--highlighted-index 0)
(defvar lusty--previous-printed-matches '())
(defconst lusty--greatest-factors
  (let ((vec (make-vector 1000 nil)))
    (dotimes (n 1000)
      (let ((factor
             (loop for i from 2 upto (ash n -1)
                   when (zerop (mod n i))
                   return (/ n i))))
      (aset vec n factor)))
    vec))

(when lusty--wrapping-ido-p
  (require 'ido))

(defun lusty-sort-by-fuzzy-score (strings abbrev)
  ;; TODO: case-sensitive when abbrev contains capital letter
  (let* ((strings+scores
          (loop for str in strings
                for score = (LM-score str abbrev)
                unless (zerop score)
                collect (cons str score)))
         (sorted
          (sort* strings+scores '> :key 'cdr)))
    (mapcar 'car sorted)))

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
  (macrolet ((ephemeral-p (name)
               `(eq (string-to-char ,name) ?\ )))
    (loop for buffer in buffers
          for name = (buffer-name buffer)
          unless (ephemeral-p name)
          collect name)))

;; Written kind-of silly for performance.
(defun lusty-filter-files (file-portion files)
  "Return FILES with './' removed and hidden files if FILE-PORTION
does not begin with '.'."
  (macrolet ((leading-dot-p (str)
               `(eq (string-to-char ,str) ?.))
             (pwd-p (str)
               `(string= ,str "./"))
             (ignored-p (name)
               `(string-match lusty--ignored-extensions-regex ,name)))
    (let ((filtered-files '()))
      (if (leading-dot-p file-portion)
          (dolist (file files)
            (unless (or (pwd-p file)
                        (ignored-p file))
              (push file filtered-files)))
        (dolist (file files)
          (unless (or (leading-dot-p file)
                      (ignored-p file))
            (push file filtered-files))))
      (nreverse filtered-files))))

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
         (bottom-edge (fourth (window-pixel-edges current-window)))
         (last-window (previous-window current-window :skip-mini))
         (window-search-p t))
    (while window-search-p
      (let* ((this-window (next-window current-window :skip-mini))
             (next-bottom-edge (fourth (window-pixel-edges this-window))))
        (when (< bottom-edge next-bottom-edge)
          (setq bottom-edge next-bottom-edge)
          (setq lowest-window this-window))
        (setq current-window this-window)
        (when (eq last-window this-window)
          (setq window-search-p nil))))
    lowest-window))

(defun lusty-max-window-height ()
  "Return the expected maximum allowable height of a window on this frame"
  ;; FIXME: are there cases where this is incorrect?
  (let* ((lusty-window
          (get-buffer-window
           (get-buffer-create lusty-buffer-name)))
         (other-window
          ;; In case the *LustyMatches* window was closed
          (or lusty-window
              (if (minibufferp)
                  (next-window (selected-window) :skip-mini)
                (selected-window))))
         (test-window
          (or lusty-window other-window)))
    (assert test-window)
    (- (frame-height)
       ;; Account for modeline and/or header...
       (- (window-height test-window)
          (window-body-height test-window))
       ;; And minibuffer height.
       (window-height (minibuffer-window)))))

(defun lusty-max-window-width ()
  (frame-width))

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

(defun lusty-refresh-matches-buffer (&optional use-previous-matches-p)
  "Refresh *Lusty-Matches*."
  (assert (minibufferp))
  (let* ((minibuffer-text (if lusty--wrapping-ido-p
                              ido-text
                            (minibuffer-contents-no-properties)))
         (matches
          (if use-previous-matches-p
              lusty--previous-printed-matches
            (ecase lusty--active-mode
              (:file-explorer
               (lusty-file-explorer-matches minibuffer-text))
              (:buffer-explorer
               (lusty-buffer-explorer-matches minibuffer-text))))))

    ;; Update the matches window.
    (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
      (with-current-buffer lusty-buffer
        (setq buffer-read-only t)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (lusty--display-entries matches)
          (setq lusty--previous-printed-matches matches)
          (goto-char (point-min))))

      ;; If only our matches window is open,
      (when (one-window-p t)
        ;; Restore original window configuration before fitting the
        ;; window so the minibuffer won't grow and look silly.
        (set-window-configuration lusty--initial-window-config))
      (fit-window-to-buffer (display-buffer lusty-buffer))
      (set-buffer-modified-p nil))))

(defun lusty-buffer-explorer-matches (text)
  (let* ((buffers (lusty-filter-buffers (buffer-list))))
    (if (string= text "")
        buffers
      (lusty-sort-by-fuzzy-score
       buffers
       text))))

;; FIXME: return an array instead of a list?
(defun lusty-file-explorer-matches (path)
  (let* ((dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         (files
          (and dir
               ; NOTE: directory-files is quicker but
               ;       doesn't append slash for directories.
               ;(directory-files dir nil nil t)
               (file-name-all-completions "" dir)))
         (filtered (lusty-filter-files file-portion files)))
    (if (or (string= file-portion "")
            (string= file-portion "."))
        (sort filtered 'string<)
      (lusty-sort-by-fuzzy-score filtered file-portion))))

(defsubst lusty-propertize-path (path)
  "Propertize the given PATH like so: <dir></> or <file>.
Uses `lusty-directory-face', `lusty-slash-face', `lusty-file-face'"
  (let ((last (1- (length path))))
    ;; Note: shouldn't get an empty path, so for performance
    ;; I'm not going to check for that case.
    (if (eq (aref path last) ?/) ; <-- FIXME nonportable?
        (progn
          ;; Directory
          (put-text-property 0 last 'face lusty-directory-face path)
          (put-text-property last (1+ last) 'face lusty-slash-face path))
      (put-text-property 0 (1+ last) 'face lusty-file-face path)))
  path)

(defun lusty--compute-optimal-layout (items)
  (let* ((max-visible-rows (1- (lusty-max-window-height)))
         (max-width (lusty-max-window-width))
         (upper-bound most-positive-fixnum)
         (n-items (length items))
         (lengths-v (make-vector n-items 0))
         (separator-length (length lusty-column-separator)))

    (let ((length-of-longest-name 0)) ; used to determine upper-bound

      ;; Initialize lengths-v
      (loop for i from 0
            for item in items
            for len = (length item)
            do
            (aset lengths-v i len)
            (setq length-of-longest-name
                  (max length-of-longest-name len)))

      ;; Calculate upper-bound
      (let ((width (+ length-of-longest-name
                      separator-length))
            (columns 1)
            (sorted-shortest (sort (append lengths-v nil) '<)))
        (dolist (item-len sorted-shortest)
          (incf width item-len)
          (when (> width max-width)
            (return))
          (incf columns)
          (incf width separator-length))
        (setq upper-bound (* columns max-visible-rows))))

    ;; Determine optimal row count.
    (multiple-value-bind (optimal-n-rows truncated-p)
        (cond ((< upper-bound n-items)
               (values max-visible-rows t))
              ((<= (reduce (lambda (a b) (+ a separator-length b))
                           lengths-v)
                   max-width)
               ;; All fits in a single row.
               (values 1 nil))
              (t
               (lusty--compute-optimal-row-count lengths-v
                                                 separator-length)))
      (let ((n-columns 0)
            (column-widths '()))

        ;; Calculate n-columns and column-widths
        (loop with total-width = 0
              for start = 0 then end
              for end = optimal-n-rows then
                        (min (length lengths-v)
                             (+ end optimal-n-rows))
              while (< start end)
              for col-width = (reduce 'max lengths-v
                                      :start start
                                      :end end)
              do
              (incf total-width col-width)
              (when (> total-width max-width)
                (return))
              (incf n-columns)
              (push col-width column-widths)
              (incf total-width separator-length))

        (values optimal-n-rows n-columns (nreverse column-widths)
                lengths-v truncated-p)))))

;; Returns number of rows and whether this truncates the entries.
(defun* lusty--compute-optimal-row-count (lengths-v separator-length)
  (let* ((n-items (length lengths-v))
         (max-visible-rows (1- (lusty-max-window-height)))
         (available-width (lusty-max-window-width))
         (lengths-h (make-hash-table :test 'equal
                                     ; not scientific
                                     :size n-items)))

    ;; FIXME: do binary search instead of linear
    (do ((n-rows 2 (1+ n-rows)))
        ((>= n-rows max-visible-rows)
         (values max-visible-rows t))
      (let ((col-start-index 0)
            (col-end-index (1- n-rows))
            (total-width 0)
            (split-factor (aref lusty--greatest-factors n-rows)))

        ;; Calculate required total-width for this number of rows.
        (while (< col-end-index n-items)
          (let ((column-width
                 (lusty--compute-column-width
                  col-start-index col-end-index split-factor
                  lengths-v lengths-h)))

            (incf total-width column-width)
            (incf total-width separator-length))

          (incf col-start-index n-rows) ; setq col-end-index
          (incf col-end-index n-rows)

          (when (and (>= col-end-index n-items)
                     (< col-start-index n-items))
            ;; Remainder; last iteration will not be a full column.
            (setq col-end-index (1- n-items)
                  split-factor nil)))

        ;; The final column doesn't need a separator.
        (decf total-width separator-length)

        (when (<= total-width available-width)
          (return-from lusty--compute-optimal-row-count
            (values n-rows nil)))))))

(defsubst lusty--compute-column-width (start-index end-index split-factor
                                       lengths-v lengths-h)
  (let ((width 0)
        (iter start-index))
    (cond ((= start-index end-index)
           ;; Single-element remainder
           (setq width (aref lengths-v iter)))
          ((null split-factor)
           ;; Prime number, or a remainder
           (while (<= iter end-index)
             (setq width (max width (aref lengths-v iter)))
             (incf iter)))
          (t
           (while (<= iter end-index)
             (setq width
                   (max width
                        (gethash (cons iter (+ iter (1- split-factor))) lengths-h)))
             (incf iter split-factor))))
    (puthash (cons start-index end-index) width lengths-h)
    width))

(defun* lusty--display-entries (entries)

  (when (endp entries)
    (lusty--print-no-entries)
    (return-from lusty--display-entries))

  (multiple-value-bind (n-rows n-columns column-widths lengths-v truncated-p)
      (lusty--compute-optimal-layout entries)

    (let* ((n-entries (min (* n-rows n-columns)
                           (length lengths-v)))
           (propertized
            ;; Add font faces to the entries
            (loop with highlighted-index = (mod lusty--highlighted-index
                                                n-entries)
                  for e in entries
                  for i from 0
                  when (= i highlighted-index)
                  collect (propertize e 'face lusty-match-face)
                  else
                  collect (lusty-propertize-path e))))

      ;; Compile and print rows.
      (let ((rows (make-vector n-rows nil)))
        (loop with col = 0
              with column-width = (car column-widths)
              for count from 0 upto (1- n-entries)
              for row = (mod count n-rows)
              for len = (aref lengths-v count)
              for entry in propertized
              for spacer = (make-string (- column-width len) ?\ )
              do
              (push entry (aref rows row))
              (when (< col (1- n-columns))
                (push spacer (aref rows row))
                (push lusty-column-separator (aref rows row)))
              (when (> (/ (1+ count) n-rows) col)
                (incf col)
                (setq column-width (nth col column-widths))))

        (loop for row across rows
              do
          (apply 'insert (nreverse row))
          (insert "\n"))))

    (when truncated-p
      (lusty--print-truncated))))

(defun lusty--print-no-entries ()
  (insert lusty-no-entries-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty--print-truncated ()
  (insert lusty-truncated-string)
  (let ((fill-column (window-width)))
    (center-line)))


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
          (funcall read-fn lusty-prompt))
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

(defsubst* LM-score (str abbrev)
  (let ((str-len (length str))
        (abbrev-len (length abbrev)))
    (cond ;((string= abbrev "")  ; Disabled; can't happen in practice
          ; LM--score-trailing)
          ((> abbrev-len str-len)
           LM--score-no-match)
          (t
           ;; Content of LM--build-score-array...
           ;; Inline for performance.
           (let* ((scores (make-vector str-len LM--score-no-match))
                  (str-lower (downcase str))
                  (abbrev-lower (downcase abbrev))
                  (last-index -1)
                  (started-p nil))
             (dotimes (i abbrev-len)
               (let ((pos (position (aref abbrev-lower i) str-lower
                                    :start (1+ last-index)
                                    :end str-len)))
                 (when (null pos)
                   (return-from LM-score LM--score-no-match))
                 (when (zerop pos)
                   (setq started-p t))
                 (cond ((and (plusp pos)
                             (let ((C (aref str (1- pos))))
                               (or (eq C ?\ )
                                   (eq C ?.)
                                   (eq C ?_)
                                   (eq C ?-))))
                        ;; New word.
                        (aset scores (1- pos) LM--score-match)
                        (fill scores LM--score-buffer
                              :start (1+ last-index)
                              :end (1- pos)))
                       ((and (>= (aref str pos) ?A)
                             (<= (aref str pos) ?Z))
                        ;; Upper case.
                        (fill scores LM--score-buffer
                              :start (1+ last-index)
                              :end pos))
                       (t
                        (fill scores LM--score-no-match
                              :start (1+ last-index)
                              :end pos)))
                 (aset scores pos LM--score-match)
                 (setq last-index pos)))

             (let ((trailing-score
                    (if started-p
                        LM--score-trailing-but-started
                      LM--score-trailing)))
               (fill scores trailing-score :start (1+ last-index))

               (/ (reduce '+ scores)
                  str-len )))))))

;;
;; End LiquidMetal
;;


;;
;; XEmacs compatibility functions
;;

(unless (fboundp 'minibufferp)
  (defun minibufferp ()
    (eq (window-buffer (minibuffer-window))
        (current-buffer))))

(unless (fboundp 'minibuffer-contents-no-properties)
  (defun minibuffer-contents-no-properties ()
    (with-current-buffer (window-buffer (minibuffer-window))
      (let ((start (1+ (length lusty-prompt)))
            (end (point-max)))
        (if (>= end start)
            (buffer-substring-no-properties start end)
          "")))))

(unless (fboundp 'minibuffer-prompt-end)
  (defun minibuffer-prompt-end ()
    (1+ (length lusty-prompt))))

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    (line-number pos)))

;; Cribbed from cal-fit-window-to-buffer
(unless (fboundp 'fit-window-to-buffer)
  (defun fit-window-to-buffer (owin max-height)
    (interactive)
    (if owin
	(delete-other-windows))
    (when (> (length (window-list nil 'nomini)) 1)
      (let* ((window (selected-window))
	     (buf (window-buffer window))
	     (height (window-displayed-height (selected-window)))
	     (new-height
              (min (with-current-buffer buf
                     (count-lines (point-min) (point-max)))
                   max-height))
	     (diff (- new-height height)))
	(unless (zerop diff)
	  (enlarge-window diff))
	(let ((end (with-current-buffer buf (point-max))))
	  (while (and (> (length (window-list nil 'nomini)) 1)
		      (not (pos-visible-in-window-p end)))
	    (enlarge-window 1)))))))


(provide 'lusty-explorer)

;;; lusty-explorer.el ends here.
