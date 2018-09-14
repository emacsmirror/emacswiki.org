;;; color-rg.el --- Search and refacotry code with rg

;; Filename: color-rg.el
;; Description: Search and refacotry code with rg
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-08-26 14:22:12
;; Version: 1.6
;; Last-Updated: 2018-09-11 11:50:35
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/color-rg.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `cl' `subr-x'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; color-rg is search and refactoring tool based on ripgrep.
;;
;; I'm a big fan of color-moccur.el, this extension's name is used for tribute color-moccur.el!
;;

;;; Installation:
;;
;; Put color-rg.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'color-rg)
;;
;; If you use Mac, you also need install `exec-path-from-shell'
;;

;;; Customize:
;;
;; `color-rg'
;;
;; All of the above can customize by:
;;      M-x customize-group RET color-rg RET
;;

;;; Change log:
;;
;; 2018/09/11
;;      * Switch to literal search automaticity when parsing keyword regexp failed.
;;      * Adjust regex to match "Error parsing regex near".
;;
;; 2018/09/04
;;      * Use `color-rg-process-setup' monitor process finished, then output search hit in minibuffer.
;;      * Avoid function `move-to-column' change search file content.
;;      * Add `color-rg-filter-match-files' and `color-rg-filter-mismatch-files'
;;      * Flash match line after open search file.
;;
;; 2018/08/31
;;      * Fix `color-rg-window-configuration-before-search' override if user multiple search.
;;
;; 2018/08/30
;;      * Add color-rg-recover-buffer
;;      * Enhance rerun function
;;      * Add new functions: color-rg-filter-match-results, color-rg-filter-mismatch-results, color-rg-remove-line-from-results
;;      * Add function color-rg-filter-results
;;      * Add smart-case to color-rg-rerun-literal and color-rg-rerun-no-ignore
;;      * Use color-rg-get-row-column-position remove duplicate code.
;;      * Add customize option color-rg-default-argument
;;      * Add some research functions
;;      * Add color-rg-replace-all-matches
;;      * Add new functions: color-rg-change-search-keyword and color-rg-change-search-directory
;;
;; 2018/08/26
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'cl)
(require 'subr-x)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup color-rg nil
  "Search and refacotry code base on ripgrep."
  :group 'color-rg)

(defcustom color-rg-buffer "*color-rg*"
  "The buffer name of search result."
  :type 'string
  :group 'color-rg)

(defcustom color-rg-temp-buffer " *color-rg temp* "
  "The buffer name of clone temp buffer"
  :type 'string
  :group 'color-rg)

(defcustom color-rg-default-argument "--column --color=always --smart-case --regexp"
  "The default search argument to ripgrep."
  :type 'string
  :group 'color-rg)

(defcustom color-rg-mode-hook '()
  "color-rg mode hook."
  :type 'hook
  :group 'color-rg-mode)

(defface color-rg-font-lock-header-line-text
  '((t (:foreground "Green3" :bold t)))
  "Face for header line text."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-keyword
  '((t (:foreground "Gold" :bold t)))
  "Face for header line keyword."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-directory
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for header line directory."
  :group 'color-rg)

(defface color-rg-font-lock-header-line-edit-mode
  '((t (:foreground "Gold" :bold t)))
  "Face for header line edit mode."
  :group 'color-rg)

(defface color-rg-font-lock-command
  '((t (:foreground "Gray30" :bold t)))
  "Face for filepath."
  :group 'color-rg)

(defface color-rg-font-lock-file
  '((t (:foreground "DodgerBlue" :bold t)))
  "Face for filepath."
  :group 'color-rg)

(defface color-rg-font-lock-line-number
  '((t (:foreground "gray35")))
  "Face for line number."
  :group 'color-rg)

(defface color-rg-font-lock-column-number
  '((t (:foreground "gray35")))
  "Face for column number."
  :group 'color-rg)

(defface color-rg-font-lock-position-splitter
  '((t (:foreground "gray25")))
  "Face for position splitter."
  :group 'color-rg)

(defface color-rg-font-lock-match
  '((t (:foreground "Gold3" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defface color-rg-font-lock-mark-changed
  '((t (:foreground "White" :background "SystemBlueColor" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defface color-rg-font-lock-mark-deleted
  '((t (:foreground "SystemRedColor" :bold t)))
  "Face for keyword match."
  :group 'color-rg)

(defcustom color-rg-flash-line-delay .3
  "How many seconds to flash `color-rg-font-lock-flash' after navigation.

Setting this to nil or 0 will turn off the indicator."
  :type 'number
  :group 'color-rg)

(defface color-rg-font-lock-flash
  '((t (:inherit highlight)))
  "Face to flash the current line."
  :group 'color-rg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-temp-visit-buffers nil
  "The temp visit buffers use to kill temp buffer after quit color-rg.")

(defvar color-rg-window-configuration-before-search nil
  "Save window configuration before search,
used to restore window configuration after finish search.")

(defvar color-rg-search-counter 0
  "Just save window configuration when this counter is 0.
Avoid multiple search overwrite window configuration.")

(defvar color-rg-window-configuration-before-apply nil
  "Save window configuration before apply changed,
used to restore window configuration after apply changed.")

(defvar color-rg-hit-count 0
  "Search keyword hit counter.")

(defvar color-rg-regexp-file "^[/\\~].*"
  "Regexp to match filename.")

(defvar color-rg-regexp-split-line "\n\n"
  "Regexp to match empty line between two files.")

(defvar color-rg-regexp-position "^\\([1-9][0-9]*\\):\\([1-9][0-9]*\\):"
  "Regexp to match line/column string.")

(defvar color-rg-changed-lines nil
  "The list that record the changed lines.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; color-rg mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar color-rg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'color-rg-beginning-of-line)

    (define-key map (kbd "j") 'color-rg-jump-next-keyword)
    (define-key map (kbd "k") 'color-rg-jump-prev-keyword)
    (define-key map (kbd "h") 'color-rg-jump-next-file)
    (define-key map (kbd "l") 'color-rg-jump-prev-file)
    (define-key map (kbd "RET") 'color-rg-open-file)
    (define-key map (kbd "C-m") 'color-rg-open-file)

    (define-key map (kbd "m") 'color-rg-change-search-customized)
    (define-key map (kbd "r") 'color-rg-replace-all-matches)
    (define-key map (kbd "f") 'color-rg-filter-match-results)
    (define-key map (kbd "F") 'color-rg-filter-mismatch-results)
    (define-key map (kbd "x") 'color-rg-filter-match-files)
    (define-key map (kbd "X") 'color-rg-filter-mismatch-files)
    (define-key map (kbd "D") 'color-rg-remove-line-from-results)
    (define-key map (kbd "i") 'color-rg-rerun-no-ignore)
    (define-key map (kbd "t") 'color-rg-rerun-literal)
    (define-key map (kbd "c") 'color-rg-rerun-case-senstive)
    (define-key map (kbd "s") 'color-rg-change-search-keyword)
    (define-key map (kbd "d") 'color-rg-change-search-directory)
    (define-key map (kbd "e") 'color-rg-switch-to-edit-mode)
    (define-key map (kbd "q") 'color-rg-quit)
    map)
  "Keymap used by `color-rg-mode'.")

(defvar color-rg-mode-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-a") 'color-rg-beginning-of-line)

    (define-key map (kbd "C-c C-j") 'color-rg-jump-next-keyword)
    (define-key map (kbd "C-c C-k") 'color-rg-jump-prev-keyword)
    (define-key map (kbd "C-c C-h") 'color-rg-jump-next-file)
    (define-key map (kbd "C-c C-l") 'color-rg-jump-prev-file)
    (define-key map (kbd "C-c <C-return>") 'color-rg-open-file)

    (define-key map (kbd "C-c C-d") 'color-rg-delete-line)
    (define-key map (kbd "C-c C-r") 'color-rg-recover-line)
    (define-key map (kbd "C-c C-R") 'color-rg-recover-buffer)
    (define-key map (kbd "C-c C-q") 'color-rg-quit)
    (define-key map (kbd "C-c C-c") 'color-rg-apply-changed)
    map)
  "Edit keymap used by `color-rg-mode'.")

(define-derived-mode color-rg-mode text-mode "color-rg"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'color-rg-mode)
  (setq mode-name "color-rg")
  (read-only-mode 1)
  (color-rg-highlight-keywords)
  (use-local-map color-rg-mode-map)
  (add-hook 'compilation-filter-hook 'color-rg-filter nil t)
  (set (make-local-variable 'compilation-process-setup-function) 'color-rg-process-setup)
  (run-hooks 'color-rg-mode-hook)
  )

(defun color-rg-highlight-keywords ()
  "Highlight keywords."
  ;; Add keywords for highlight.
  (font-lock-add-keywords
   nil
   '(
     ("^rg\\s-.*" . 'color-rg-font-lock-command)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 1 'color-rg-font-lock-line-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 2 'color-rg-font-lock-position-splitter)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 3 'color-rg-font-lock-column-number)
     ("^\\([1-9][0-9]*\\)\\(:\\)\\([1-9][0-9]*\\)\\(:\\)" 4 'color-rg-font-lock-position-splitter)
     ("^[/\\~].*" . 'color-rg-font-lock-file)
     ))
  ;; NOTE:
  ;; Because search line maybe just contains *half* of string/comment that make rest content of buffer mark as string.
  ;; So we need turn off comment/string font-lock through set `font-lock-keywords-only'.
  (set (make-local-variable 'font-lock-keywords-only) t)
  ;; Enable font lock.
  (font-lock-mode 1))

(defun color-rg-filter ()
  "Handle match highlighting escape sequences inserted by the rg process.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;; Highlight filename.
        (while (re-search-forward "^\033\\[[0]*m\033\\[35m\\(.*?\\)\033\\[[0]*m$" end 1)
          (replace-match (concat (propertize (match-string 1)
                                             'face nil 'font-lock-face 'color-rg-font-lock-file))
                         t t))

        ;; Highlight rg matches and delete marking sequences.
        (goto-char beg)
        (while (re-search-forward "\033\\[[0]*m\033\\[[3]*1m\033\\[[3]*1m\\(.*?\\)\033\\[[0]*m" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face 'color-rg-font-lock-match)
                         t t)
          (setq color-rg-hit-count (+ color-rg-hit-count 1)))

        ;; Delete all remaining escape sequences
        (goto-char beg)
        (while (re-search-forward "\033\\[[0-9;]*[0mK]" end 1)
          (replace-match "" t t))))
    ))

(defun color-rg-process-setup ()
  "Setup compilation variables and buffer for `color-rg'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             ;; This relies on the fact that `compilation-start'
             ;; sets buffer-modified to nil before running the command,
             ;; so the buffer is still unmodified if there is no output.
             (cond ((and (zerop code) (buffer-modified-p))
                    `(,(format "finished (%d matches found)\n" color-rg-hit-count) . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (if (string-prefix-p "exited abnormally with code" msg)
                        ;; Switch to literal search automaticity when parsing keyword regexp failed.
                        (with-current-buffer color-rg-buffer
                          (when (search-forward-regexp "^Error parsing regex near" nil t)
                            (run-at-time "2sec" nil
                                         (lambda ()
                                           (message "COLOR-RG: parsing keyword regexp failed, switch to literal search automaticity.")))
                            (color-rg-rerun-literal t)
                            ))
                      (cons msg code))
                    ))
           (cons msg code)))))

(defun color-rg-update-header-line ()
  (setq header-line-format (format "%s%s%s%s%s%s"
                                   (propertize "[COLOR-RG] Search '" 'font-lock-face 'color-rg-font-lock-header-line-text)
                                   (propertize search-keyword 'font-lock-face 'color-rg-font-lock-header-line-keyword)
                                   (propertize "' in directory: " 'font-lock-face 'color-rg-font-lock-header-line-text)
                                   (propertize search-directory 'font-lock-face 'color-rg-font-lock-header-line-directory)
                                   (propertize " Mode: " 'font-lock-face 'color-rg-font-lock-header-line-text)
                                   (propertize edit-mode 'font-lock-face 'color-rg-font-lock-header-line-edit-mode)
                                   ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-rg-search (keyword directory &optional argument)
  (let* ((rg-argument (or argument
                          color-rg-default-argument
                          ))
         (search-command (format "rg %s \"%s\" %s" rg-argument keyword directory)))
    ;; Erase or create search result.
    (if (get-buffer color-rg-buffer)
        (with-current-buffer color-rg-buffer
          (let ((inhibit-read-only t))
            ;; Switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
            (color-rg-mode)
            ;; Erase buffer content.
            (read-only-mode -1)
            (erase-buffer)))
      (generate-new-buffer color-rg-buffer))
    (setq color-rg-changed-lines nil)
    ;; Run search command.
    (with-current-buffer color-rg-buffer
      ;; Start command.
      (compilation-start search-command 'color-rg-mode)
      ;; Set header line.
      (set (make-local-variable 'search-argument) rg-argument)
      (set (make-local-variable 'search-keyword) keyword)
      (set (make-local-variable 'search-directory) directory)
      (set (make-local-variable 'default-directory) directory)
      (set (make-local-variable 'edit-mode) "View")
      (color-rg-update-header-line)
      )
    ;; Pop search buffer.
    (pop-to-buffer color-rg-buffer)
    (goto-char (point-min))
    ))

(defun color-rg-read-input ()
  (let* ((current-symbol (color-rg-pointer-string))
         (input-string (string-trim (read-string (format "COLOR-RG Search (%s): " current-symbol)))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    input-string))

(defun color-rg-pointer-string ()
  (if (use-region-p)
      ;; Get region string if mark is set.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get current symbol but remove prefix char before return.
    (let ((current-symbol (thing-at-point 'symbol)))
      (cond ((string-prefix-p "." current-symbol)
             (string-remove-prefix "." current-symbol))
            ((string-prefix-p "#" current-symbol)
             (string-remove-prefix "#" current-symbol))
            (t current-symbol)))
    ))

(defun color-rg-find-next-position (regexp)
  (save-excursion
    (end-of-line)
    (search-forward-regexp regexp nil t)))

(defun color-rg-get-match-file ()
  (save-excursion
    (search-backward-regexp color-rg-regexp-file nil t)
    (string-remove-suffix "\n" (buffer-substring-no-properties (beginning-of-thing 'line) (end-of-thing 'line)))))

(defun color-rg-get-match-line ()
  (beginning-of-line)
  (string-to-number (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol))))

(defun color-rg-get-match-column ()
  (search-forward ":")
  (string-to-number (buffer-substring-no-properties (beginning-of-thing 'symbol) (end-of-thing 'symbol))))

(defun color-rg-get-match-buffer (filepath)
  (catch 'find-match
    (dolist (buffer (buffer-list))
      (when (string-equal (buffer-file-name buffer) filepath)
        (throw 'find-match buffer)))
    nil))

(defun color-rg-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun color-rg-get-line-content (buffer line)
  (with-current-buffer buffer
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (search-forward-regexp color-rg-regexp-position nil t)
      (setq start (point))
      (end-of-line)
      (setq end (point))
      (buffer-substring start end))
    ))

(defun color-rg-get-row-column-position ()
  (let* ((search-bound
          (save-excursion
            (end-of-line)
            (point)))
         (row-column-position
          (save-excursion
            (beginning-of-line)
            (search-forward-regexp color-rg-regexp-position search-bound t))))
    row-column-position))

(defun color-rg-after-change-function (beg end leng-before)
  ;; NOTE:
  ;; We should use `save-match-data' wrap function that hook in `after-change-functions'.
  ;; Otherwise will got error: "replace-match-maybe-edit: Match data clobbered by buffer modification hooks"
  (save-match-data
    (let* ((change-line (save-excursion
                          (goto-char beg)
                          (line-number-at-pos)))
           change-line-content
           original-line-content)
      (setq changed-line-content (color-rg-get-line-content color-rg-buffer change-line))
      (setq original-line-content (color-rg-get-line-content color-rg-temp-buffer change-line))
      (if (string-equal changed-line-content original-line-content)
          (progn
            (setq color-rg-changed-lines (remove change-line color-rg-changed-lines))
            (color-rg-mark-position-clear change-line))
        (add-to-list 'color-rg-changed-lines change-line)
        (if (string-equal changed-line-content "")
            (color-rg-mark-position-deleted change-line)
          (color-rg-mark-position-changed change-line)))
      )))

(defun color-rg-mark-position-clear (line)
  (save-excursion
    (goto-line line)
    (beginning-of-line)
    (forward-char)
    (dolist (overlay (overlays-at (point)))
      (when (or (string-equal (overlay-get overlay 'overlay-type) "changed")
                (string-equal (overlay-get overlay 'overlay-type) "deleted"))
        (delete-overlay overlay)
        ))))

(defun color-rg-mark-position (line type face)
  (save-excursion
    (color-rg-mark-position-clear line)
    ;; Create mark changed overlay if not exists.
    (let (start end)
      (save-excursion
        (beginning-of-line)
        (setq start (point))
        (end-of-line)
        (setq end (point))
        (setq changed-overlay (make-overlay start end))
        (overlay-put changed-overlay 'overlay-type type)
        (overlay-put changed-overlay 'face face)
        ))))

(defun color-rg-mark-position-changed (line)
  (color-rg-mark-position line "changed" 'color-rg-font-lock-mark-changed))

(defun color-rg-mark-position-deleted (line)
  (color-rg-mark-position line "deleted" 'color-rg-font-lock-mark-deleted))

(defun color-rg-kill-temp-buffer ()
  (when (get-buffer color-rg-temp-buffer)
    (kill-buffer color-rg-temp-buffer)
    (setq color-rg-changed-lines nil)))

(defun color-rg-clone-to-temp-buffer ()
  (color-rg-kill-temp-buffer)
  (with-current-buffer color-rg-buffer
    (add-hook 'kill-buffer-hook 'color-rg-kill-temp-buffer nil t)
    (generate-new-buffer color-rg-temp-buffer)
    (append-to-buffer color-rg-temp-buffer (point-min) (point-max))
    ))

(defun color-rg-switch-to-view-mode ()
  (with-current-buffer color-rg-buffer
    ;; Do clean work.
    (dolist (line color-rg-changed-lines)
      (color-rg-mark-position-clear line))
    (setq color-rg-changed-lines nil)
    (color-rg-kill-temp-buffer)
    (remove-hook 'after-change-functions 'color-rg-after-change-function t)
    ;; Switch to view mode.
    (read-only-mode 1)
    (use-local-map color-rg-mode-map)
    (kill-local-variable 'query-replace-skip-read-only)
    (set (make-local-variable 'edit-mode) "View")
    (color-rg-update-header-line)
    ))

(defun color-rg-filter-results (match-regexp)
  (let ((filter-regexp (read-string
                        (format (if match-regexp
                                    "Filter result match regexp: "
                                  "Filter result not match regexp: ")))))
    (save-excursion
      (with-current-buffer color-rg-buffer
        (setq remove-counter 0)
        (goto-char (point-min))
        (while (setq start (search-forward-regexp color-rg-regexp-position nil t))
          (setq line-content (color-rg-get-line-content color-rg-buffer (line-number-at-pos)))
          (if match-regexp
              (unless (string-match filter-regexp line-content)
                (color-rg-remove-line-from-results)
                (setq remove-counter (+ 1 remove-counter))
                )
            (when (string-match filter-regexp line-content)
              (color-rg-remove-line-from-results)
              (setq remove-counter (+ 1 remove-counter))
              )))
        (if match-regexp
            (message (format "Remove %s lines not match regexp '%s'." remove-counter filter-regexp))
          (message (format "Remove %s lines match regexp '%s'." remove-counter filter-regexp)))
        ))))

(defun color-rg-filter-files (match-files)
  (let (file-extensions start end)
    (save-excursion
      (goto-char (point-min))
      (while (setq end (search-forward-regexp color-rg-regexp-file nil t))
        (beginning-of-line)
        (setq start (point))
        (setq filename (buffer-substring-no-properties start end))
        (end-of-line)
        (add-to-list 'file-extensions (file-name-extension filename))))
    (if (< (length file-extensions) 2)
        (message (format "Has one type files now."))
      (setq filter-extension (completing-read (if match-files
                                                  "Only display file suffix with: "
                                                "Remove file suffix with: ")
                                              file-extensions))
      (save-excursion
        (with-current-buffer color-rg-buffer
          (setq remove-counter 0)
          (goto-char (point-min))
          (while (setq end (search-forward-regexp color-rg-regexp-file nil t))
            (beginning-of-line)
            (setq start (point))
            (setq file-extension (file-name-extension (buffer-substring-no-properties start end)))
            (if match-files
                (if (string-equal file-extension filter-extension)
                    (end-of-line)
                  (color-rg-remove-lines-under-file))
              (if (string-equal file-extension filter-extension)
                  (color-rg-remove-lines-under-file)
                (end-of-line))))
          )))))

(defun color-rg-remove-lines-under-file ()
  (let (start end)
    (save-excursion
      (with-current-buffer color-rg-buffer
        (read-only-mode -1)
        (beginning-of-line)
        (setq start (point))
        (when (search-forward-regexp color-rg-regexp-split-line nil t)
          (setq end (point))
          (kill-region start end))
        (read-only-mode 1)))))

(defun color-rg-flash-line (&optional pos end-pos face delay)
  "Flash a temporary highlight to help the user find something.

POS is optional, and defaults to the current point.

If optional END-POS is set, flash the characters between the two
points, otherwise flash the entire line in which POS is found.

The flash is normally not inclusive of END-POS.  However, when
POS is equal to END-POS, the single character at POS will flash.

Optional FACE defaults to `color-rg-font-lock-flash'.  Optional DELAY
defaults to `color-rg-flash-line-delay' seconds.  Setting DELAY to 0 makes
this function a no-op."
  (callf or pos (point))
  (unless end-pos
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (goto-char pos)
        (beginning-of-visual-line)
        (setq pos (point))
        (end-of-visual-line)
        (setq end-pos (1+ (point))))))
  (when (eq pos end-pos)
    (incf end-pos))
  (callf or delay color-rg-flash-line-delay)
  (callf or face 'color-rg-font-lock-flash)
  (when (and (numberp delay)
             (> delay 0))
    (when (timerp next-error-highlight-timer)
      (cancel-timer next-error-highlight-timer))
    (setq compilation-highlight-overlay (or compilation-highlight-overlay
                                            (make-overlay (point-min) (point-min))))
    (overlay-put compilation-highlight-overlay 'face face)
    (overlay-put compilation-highlight-overlay 'priority 10000)
    (move-overlay compilation-highlight-overlay pos end-pos)
    (add-hook 'pre-command-hook 'compilation-goto-locus-delete-o)
    (setq next-error-highlight-timer
          (run-at-time delay nil 'compilation-goto-locus-delete-o))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun color-rg-search-input (&optional keyword directory argument)
  (interactive)
  ;; Save window configuration before do search.
  (when (equal color-rg-search-counter 0)
    (setq color-rg-window-configuration-before-search (current-window-configuration)))
  (setq color-rg-search-counter (+ 1 color-rg-search-counter))
  ;; Set `enable-local-variables' to :safe, avoid emacs ask annoyingly question when open file by color-rg.
  (setq enable-local-variables :safe)
  ;; Reset hit count.
  (setq color-rg-hit-count 0)
  ;; Search.
  (let* ((search-keyboard
          (or keyword
              (color-rg-read-input)))
         (search-directory
          (or directory
              default-directory))
         (search-argument
          (or argument
              color-rg-default-argument)))
    (color-rg-search search-keyboard search-directory search-argument)))

(defun color-rg-search-symbol ()
  (interactive)
  (color-rg-search-input (color-rg-pointer-string) default-directory))

(defun color-rg-search-project ()
  (interactive)
  (require 'projectile)
  (color-rg-search-input (color-rg-read-input) (projectile-project-root))
  )

(defun color-rg-search-project-rails ()
  (interactive)
  (require 'projectile)
  (color-rg-search-input (color-rg-read-input) (concat (projectile-project-root) "app")))

(defun color-rg-replace-all-matches ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (let* ((replace-text (read-string (format "Replace '%s' all matches with: " search-keyword) search-keyword)))
        (color-rg-switch-to-edit-mode)
        (query-replace search-keyword replace-text nil (point-min) (point-max))
        (color-rg-apply-changed)
        (color-rg-switch-to-view-mode)
        ;; Set search keyword with new replace text.
        (set (make-local-variable 'search-keyword) replace-text)
        ))))

(defun color-rg-filter-match-results ()
  (interactive)
  (color-rg-filter-results t))

(defun color-rg-filter-mismatch-results ()
  (interactive)
  (color-rg-filter-results nil))

(defun color-rg-filter-match-files ()
  (interactive)
  (color-rg-filter-files t))

(defun color-rg-filter-mismatch-files ()
  (interactive)
  (color-rg-filter-files nil))

(defun color-rg-remove-line-from-results ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (when (color-rg-get-row-column-position)
        (read-only-mode -1)
        (beginning-of-line)
        (kill-line)
        (kill-line)
        (read-only-mode 1)
        ))))

(defun color-rg-change-search-keyword ()
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* ((new-keyword (read-string (format "Re-search with new keyword: ") search-keyword)))
      (color-rg-switch-to-view-mode)
      (color-rg-search-input new-keyword search-directory)
      (set (make-local-variable 'search-keyword) new-keyword)
      (set (make-local-variable 'search-argument) color-rg-default-argument)
      )))

(defun color-rg-literal-search-helper (search-argument)
  (if (cl-search (regexp-quote "--fixed-strings") search-argument)
      t
    nil))

(defun color-rg-literal-string-escape (string)
  "escape double quote and backslash."
  (replace-regexp-in-string (regexp-quote "\"") "\\\\\""
                            (replace-regexp-in-string (regexp-quote "\\") "\\\\\\\\"
                                                      string)
                            ))

(defun color-rg-change-search-directory ()
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* ((new-directory (read-file-name (format "Re-search with new directory: ") search-directory))
           (unused (if (not (file-exists-p new-directory))
                       (error "directory not exist")))
           (original-keyword search-keyword)
           (literal-search (color-rg-literal-search-helper search-argument))
           (new-keyword (if literal-search
                            (color-rg-literal-string-escape search-keyword)
                          original-keyword)))
      (color-rg-switch-to-view-mode)
      (set (make-local-variable 'default-directory) new-directory)
      (color-rg-search-input new-keyword new-directory search-argument)
      (set (make-local-variable 'search-directory) new-directory)
      (set (make-local-variable 'search-keyword) original-keyword)
      )))

(defun color-rg-change-search-customized ()
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* ((new-argument (read-string (format "Re-search with new argument: ") search-argument)))
      (color-rg-switch-to-view-mode)
      (color-rg-search-input search-keyword search-directory new-argument)
      (set (make-local-variable 'search-argument) new-argument)
      )))


(defun color-rg-rerun-literal (&optional nointeractive)
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* (
           (new-argument (cond ((cl-search (regexp-quote "--regexp") search-argument)
                                (replace-regexp-in-string (regexp-quote "--regexp") "--fixed-strings" search-argument))
                               ((cl-search (regexp-quote "--fixed-strings") search-argument)
                                search-argument)
                               (t
                                (replace-regexp-in-string (regexp-quote "--regexp") "--fixed-strings" color-rg-default-argument))
                               ))
           (input-keyword (if nointeractive
                              search-keyword
                            (read-string (format "Re-search with literal: ") search-keyword)))
           (literal-keyword
            (color-rg-literal-string-escape input-keyword)))

      (color-rg-switch-to-view-mode)
      (color-rg-search-input literal-keyword search-directory new-argument)
      (set (make-local-variable 'search-argument) new-argument)
      (set (make-local-variable 'search-keyword) input-keyword)
      )))

(defun color-rg-rerun-no-ignore ()
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* ((new-argument (if (cl-search (regexp-quote "--no-ignore") search-argument)
                             (replace-regexp-in-string (regexp-quote "--no-ignore ") "" search-argument)
                           (concat "--no-ignore " search-argument)))
           (original-keyword search-keyword)
           (literal-search (color-rg-literal-search-helper new-argument))
           (new-keyword (if literal-search
                            (color-rg-literal-string-escape search-keyword)
                          original-keyword)))
      (color-rg-switch-to-view-mode)
      (color-rg-search-input new-keyword search-directory new-argument)
      (set (make-local-variable 'search-argument) new-argument)
      (set (make-local-variable 'search-keyword) original-keyword)
      )))

(defun color-rg-rerun-case-senstive ()
  (interactive)
  (with-current-buffer color-rg-buffer
    (let* ((new-argument (cond ((cl-search (regexp-quote "--smart-case") search-argument)
                                (replace-regexp-in-string (regexp-quote "--smart-case") "--case-sensitive" search-argument))
                               ((cl-search (regexp-quote "--case-sensitive") search-argument)
                                (replace-regexp-in-string (regexp-quote "--case-sensitive") "--smart-case" search-argument))
                               (t color-rg-default-argument)))
           (original-keyword search-keyword)
           (literal-search (color-rg-literal-search-helper new-argument))
           (new-keyword (if literal-search
                            (color-rg-literal-string-escape search-keyword)
                          original-keyword))
           )
      (color-rg-switch-to-view-mode)
      (color-rg-search-input new-keyword search-directory new-argument)
      (set (make-local-variable 'search-argument) new-argument)
      (set (make-local-variable 'search-keyword) original-keyword)
      )))

(defun isearch-toggle-color-rg ()
  "toggle `color-rg' in isearch-mode."
  (interactive)
  (color-rg-search-input isearch-string)
  (isearch-exit)
  )

(defun color-rg-jump-next-keyword ()
  (interactive)
  (let* ((next-position (color-rg-find-next-position color-rg-regexp-position)))
    (if next-position
        (progn
          (goto-char next-position)
          (color-rg-open-file))
      (message "Reach to last line."))))

(defun color-rg-jump-prev-keyword ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp color-rg-regexp-position nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-position nil t)
                       (line-number-at-pos))))
               (if (equal first-search-line (line-number-at-pos))
                   ;; Search previous again if first search is same line of point.
                   (save-excursion
                     (beginning-of-line)
                     (search-backward-regexp color-rg-regexp-position nil t))
                 (save-excursion (search-backward-regexp color-rg-regexp-position nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (color-rg-open-file))
      (message "Reach to first line."))))

(defun color-rg-jump-next-file ()
  (interactive)
  (let*  ((next-position (color-rg-find-next-position color-rg-regexp-file)))
    (if next-position
        (progn
          (goto-char next-position)
          (forward-line)
          (color-rg-open-file))
      (message "Reach to last file."))))

(defun color-rg-jump-prev-file ()
  (interactive)
  (let ((prev-match-pos
         (if (save-excursion (search-backward-regexp color-rg-regexp-file nil t))
             (let* ((first-search-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-file nil t)
                       (line-number-at-pos)))
                    (prev-empty-line
                     (save-excursion
                       (search-backward-regexp color-rg-regexp-split-line)
                       (line-number-at-pos))))
               (if (and (> first-search-line prev-empty-line)
                        (not (color-rg-current-line-empty-p)))
                   ;; Search filename previous again if first search is current file result area.
                   (save-excursion
                     (search-backward-regexp color-rg-regexp-split-line)
                     (search-backward-regexp color-rg-regexp-file nil t))
                 (save-excursion (search-backward-regexp color-rg-regexp-file nil t)))
               )
           nil)))
    (if prev-match-pos
        (progn
          (goto-char prev-match-pos)
          (forward-line)
          (color-rg-open-file))
      (message "Reach to first file."))))

(defun color-rg-open-file ()
  (interactive)
  (let* ((match-file (color-rg-get-match-file))
         (match-line (color-rg-get-match-line))
         (match-column (color-rg-get-match-column))
         (match-buffer (color-rg-get-match-buffer match-file)))
    (save-excursion
      ;; Open file in other window.
      (find-file-other-window match-file)
      ;; Add to temp list if file's buffer is not exist.
      (unless match-buffer
        (add-to-list 'color-rg-temp-visit-buffers (current-buffer)))
      ;; Jump to match position.
      (goto-line match-line)
      ;; NOTE:
      ;; Don't turn on FORCE option of `move-to-column', it will modification search file when force move to target column.
      (move-to-column (- match-column 1))
      ;; Flash match line.
      (color-rg-flash-line)
      )
    ;; Keep cursor in search buffer's window.
    (select-window (get-buffer-window color-rg-buffer))
    ;; Ajust column position.
    (beginning-of-line)
    (search-forward-regexp color-rg-regexp-position)
    ;; Forward to column if current line is not empty line (delete by `color-rg-delete-line').
    (unless (looking-at "[[:space:]]*$")
      (forward-char (- match-column 1)))
    ))

(defun color-rg-switch-to-edit-mode ()
  (interactive)
  ;; Clone content to temp buffer.
  (color-rg-clone-to-temp-buffer)
  ;; Update header-line.
  (set (make-local-variable 'edit-mode) "Edit")
  ;; Set `query-replace-skip-read-only' to avoid read-only error when do `query-replace'.
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (color-rg-update-header-line)
  ;; Turn off readonly mode.
  (read-only-mode -1)
  ;; Clean keymap.
  ;; Load edit keymap.
  (use-local-map color-rg-mode-edit-map)
  ;; Set edit area.
  (let (start end)
    ;; Make all buffer with readonly text property.
    (let ((inhibit-read-only t))
      (save-excursion
        (put-text-property 1 2 'front-sticky '(read-only))
        (put-text-property (point-min) (point-max) 'read-only t)
        ))
    ;; Make all code with edit property.
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (setq start (search-forward-regexp color-rg-regexp-position nil t))
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (put-text-property (1- start) end 'read-only nil)))
      ))
  ;; Add change monitor.
  (add-hook 'after-change-functions 'color-rg-after-change-function nil t)
  ;; Message to user.
  (message "Switch to edit mode")
  )

(defun color-rg-quit ()
  (interactive)
  ;; Kill temp buffer open by color-rg.
  (dolist (temp-buffer color-rg-temp-visit-buffers)
    (kill-buffer temp-buffer))
  (setq color-rg-temp-visit-buffers nil)
  ;; Kill search buffer.
  (kill-buffer color-rg-buffer)
  ;; Restore window configuration before search.
  (when color-rg-window-configuration-before-search
    (set-window-configuration color-rg-window-configuration-before-search)
    (setq color-rg-window-configuration-before-search nil))
  (setq color-rg-search-counter 0))

(defun color-rg-beginning-of-line ()
  (interactive)
  (let* ((row-column-position (color-rg-get-row-column-position)))
    (if row-column-position
        (goto-char row-column-position)
      (move-beginning-of-line 1))))

(defun color-rg-delete-line ()
  (interactive)
  (let* ((row-column-position (color-rg-get-row-column-position)))
    (when row-column-position
      (setq start row-column-position)
      (end-of-line)
      (setq end (point))
      (kill-region start end)
      )))

(defun color-rg-recover-line ()
  (interactive)
  (color-rg-delete-line)
  (insert (color-rg-get-line-content color-rg-temp-buffer (line-number-at-pos))))

(defun color-rg-recover-buffer ()
  (interactive)
  (save-excursion
    (with-current-buffer color-rg-buffer
      (let ((inhibit-read-only t))
        ;; Save local variables.
        (setq current-edit-mode edit-mode)
        (setq current-search-argument search-argument)
        (setq current-search-keyword search-keyword)
        (setq current-search-directory search-directory)
        ;; Recover buffer content from temp buffer.
        (color-rg-mode) ; switch to `color-rg-mode' first, otherwise `erase-buffer' will cause "save-excursion: end of buffer" error.
        (read-only-mode -1)
        (erase-buffer)
        (insert (with-current-buffer color-rg-temp-buffer
                  (buffer-substring (point-min) (point-max))))
        ;; Restore local variables.
        (set (make-local-variable 'search-argument) current-search-argument)
        (set (make-local-variable 'search-keyword) current-search-keyword)
        (set (make-local-variable 'search-directory) current-search-directory)
        (set (make-local-variable 'edit-mode) current-edit-mode)
        ;; Switch to edit mode.
        (color-rg-switch-to-edit-mode)
        ))))

(defun color-rg-apply-changed ()
  (interactive)
  (if (equal (length color-rg-changed-lines) 0)
      (message "Nothing need change.")
    ;; Save window configuration before do apply.
    (setq color-rg-window-configuration-before-apply (current-window-configuration))
    ;; Apply changed.
    (save-excursion
      (dolist (line color-rg-changed-lines)
        (let (match-file match-line changed-line-content)
          (setq changed-line-content (color-rg-get-line-content color-rg-buffer line))
          (with-current-buffer color-rg-buffer
            ;; Get match file and line.
            (goto-line line)
            (setq match-file (color-rg-get-match-file))
            (setq match-line (color-rg-get-match-line)))
          ;; Open file in other window.
          (find-file match-file)
          ;; Remove from temp list if file's buffer is exist.
          (setq color-rg-temp-visit-buffers (remove (current-buffer) color-rg-temp-visit-buffers))
          ;; Kill target line.
          (goto-line match-line)
          (kill-line)
          ;; Insert change line.
          (if (string-equal changed-line-content "")
              ;; Kill empty line if line mark as deleted.
              (kill-line)
            ;; Otherwise insert new line into file.
            (insert changed-line-content))
          )))
    ;; Restore window configuration before apply changed.
    (when color-rg-window-configuration-before-apply
      (set-window-configuration color-rg-window-configuration-before-apply)
      (setq color-rg-window-configuration-before-apply nil))
    ;; Message to user.
    (message (format "Apply %s lines" (length color-rg-changed-lines))))
  (color-rg-switch-to-view-mode))

(provide 'color-rg)

;;; color-rg.el ends here
