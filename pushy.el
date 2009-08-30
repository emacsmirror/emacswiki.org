;;; pushy.el --- pushy completion

;; Copyright (C) 2009  Tamas Patrovics

;; Author: Tamas Patrovics
;; Maintainer: Tamas Patrovics
;; Keywords: completion
;; X-URL: http://www.emacswiki.org/cgi-bin/emacs/pushy.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Pushy completion is a variation of timid completion. It's timid
;; completion without the timidity.
;;
;; When using a command with minibuffer history, completions from the
;; minibuffer are offered automatically if the user hesitates a bit
;; (configurable).
;;
;; If there are possible completions then pushy completion displays
;; them and the user can select a match with the C-UP/DOWN keys and
;; C-ENTER. If more characters are typed the list of matches is
;; updated. Pushy completion list can be dismissed with ESC.
;;
;; Pushy completion can be enabled with
;;
;; 	(pushy-mode t)
;;
;; This enables pushy completion mode, but you also have to specify
;; which commands you want to have affected by pushy.
;;
;; Pushy completion can be turned on for all commands by customizing
;; variable `pushy-enable-globally'. Alternatively, it can be enabled
;; for individual commands by putting property `pushy-completion' with
;; value `enabled' on the command. Also, it can be enabled globally
;; and be disabled for individual commands by setting value of
;; property `pushy-completion' to `disabled' for the command. See
;; below for a complete list of possible command properties.
;;
;; Pushy completion can also offer items which are outside of the
;; current command's scope. For example, iswitchb can offer
;; completions from the file history. This way iswitchb can be used
;; for switching buffers, but also for quickly revisiting a previously
;; opened file, so there is no need to use a separate command for
;; it. According to my experience it is a very convenient and
;; efficient setup.
;;
;; Iswitchb requires special treatment, so use this command to enable
;; pushy for it:
;;
;; 	(pushy-iswitchb-setup)
;;
;;
;; In order to take full advantage of pushy history completion use of
;; package savehist is recommended with a sufficently high
;; history-length value (I use 1000).
;;
;;
;; Complete list of command properties for pushy completion:
;;
;;   Property: pushy-completion
;;
;;   Values:
;;
;;     enabled - Pushy is enabled for commands having this
;;               property. This is not necessary if you enabled pushy
;;               globally via variable `pushy-enable-globally'.
;;
;;     disabled - Pushy is disabled for this command. This is useful
;;                if you have enabled pushy globally via variable
;;                `pushy-enable-globally', but want to inhibit it for
;;                certain commands.
;;
;;
;;   Property: pushy-search-delay-function
;;
;;   Value: Function to determine how much should timit wait before
;;          the completion list is updated. If not set the value of
;;          `pushy-search-delay' is used. The function is called with
;;          no argument.
;;
;;
;;   Property: pushy-pattern-function
;;
;;   Value: Function to get the current search pattern. If not set the
;;          pattern is retrieved from the minibuffer. The function is
;;          called with no argument.
;;
;;
;;   Property: pushy-candidates-variable
;;
;;   Value: The variable containing the list of completion candidates. If not
;;          set then `minibuffer-history-variable' is used.
;;
;;
;;   Property: pushy-visit-file-function
;;
;;   Value: Function to visit the selected file. It is called with one
;;          argument, the path of the selected file. If not set then
;;          the file path is inserted into the minibuffer and the
;;          function `exit-minibuffer' is called.
;;
;;
;; pushy.el is tested on Gnu Emacs 22.
;;

;;; Code:

;;; User options

(defgroup pushy nil
  "Pushy Completion Group."
  :tag "Pushy Completion"
  :link '(emacs-library-link :tag "Source Lisp File" "pushy.el")
  :prefix "pushy-"
  :version "22"
  :group 'emacs)

(defcustom pushy-enable-globally nil
  "*Enable pushy for all commands which use the minibuffer. 

Note that this is not yet tested with every emacs command. YMMV."
  :type 'boolean
  :version "22"
  :group 'pushy)

(defcustom pushy-search-delay 0.5
  "*Idle time after last input event before searching for matches in the history."
  :type 'number
  :version "22"
  :group 'pushy)

(defcustom pushy-list-basename-matches-first t
  "*In case of file completion put those files first in the
completion list where the basename matches the pattern."
  :type 'boolean
  :version "22"
  :group 'pushy)

(defcustom pushy-filter-regexps nil
 "*List of regular expressions to filter out unwanted files from the
output."
 :type '(repeat regexp)
 :group 'pushy)

(defcustom pushy-transform-regexps nil
 "*List of (REGEXP . REPLACEMENT) pairs to transform matching file
path names. It's useful when the matching path names are very long and
they have a component which can safely be replaced with a shorter
indicator string.

For example this rule:

    (push '(\"^/very/long/path/to/projectx/\" . \"<projx>/\")
      pushy-transform-regexps)

will display file names under \"projectx\" like this:

    <projx>/sources/main.c
    <projx>/sources/test.c

"
 :type '(repeat (cons regexp regexp))
 :version "22"
 :group 'pushy)

(defcustom pushy-use-locate nil
  "*Non-nil means use locate.
It uses locate to find further completions not present in the history in case
of file completion."
  :type 'boolean
  :version "22"
  :group 'pushy)

(defcustom pushy-locate-databases nil
  "*Database files string or nil.
If it is string, it has a list of database files separated with colon to be
used by the locate command.

If it is nil, the system default database is used."
  :type '(choice (const  :tag "Default Database" nil)
		 (string :tag "Database(s)"))
  :version "22"
  :group 'pushy)

(defcustom pushy-locate-minimum-pattern-length 3
  "*Minimum number of characters needed to start file searching with locate."
  :type 'integer
  :version "22"
  :group 'pushy)

(defcustom pushy-locate-matching-filename-limit 500
  "*If there are more matching file names than the given limit the
search is terminated automatically. This is useful if a too broad
search input is given and there are hundreds or thousands of matches.

The limit is checked only if `pushy-use-locate' is enabled, because
matches from the internal history lists can be processed quickly.

If you don't want to limit the number of matches then set it to nil
instead of a very high number."
  :type '(choice integer (const nil))
  :group 'pushy)

(defcustom pushy-keys
  (list (cons (kbd "C-<return>") 'pushy-select-item)
        (cons (kbd "M-<return>") 'pushy-insert-item)
	(cons (kbd "<ESC>") 'pushy-cleanup)
	(cons (kbd "C-<up>") 'pushy-previous-line)
	(cons (kbd "C-<down>") 'pushy-next-line)
	(cons (kbd "C-<prior>") 'pushy-previous-page)
	(cons (kbd "C-<next>") 'pushy-next-page))
  "*Alist of keybindings for pushy completion."
  :type '(alist :key-type   (sexp     :tag "Key")
		:value-type (function :tag "Function"))
  :version "22"
  :group 'pushy)

;;;----------------------------------------------------------------------

;; these commands don't work well with pushy completion
(dolist (cmd '(globalff))
  (put cmd 'pushy-completion 'disabled))


(defvar pushy-buffer "*pushy*"
  "Name of buffer showing completions.")

(defvar pushy-saved-commands nil
  "List of commands saved when pushy overrides keys in the local map.")

(defvar pushy-window-config nil
  "Window configuration before pushy completions were shown.")

(defvar pushy-pattern-start-position nil
  "Position of beginning of pattern in the minibuffer.")

(defvar pushy-overlay nil
  "Overlay used to highlight the currently selected file.")

(defvar pushy-previous-pattern ""
  "The last pattern used to update the completion buffer.")

(defvar pushy-trigger-command nil
  "The current command which triggered pushy completion.")

(defvar pushy-history-matches nil
  "The list of matches for the current input pattern from the history.")

(defvar pushy-locate-process nil
  "The current locate process.")

(defvar pushy-locate-basename-search nil
  "Indicates whether this is the basename search pass of locate.

When searching with locate those files are listed first where the
basename matches the search pattern and then those where the match is
anywhere in the path.")

(defvar pushy-locate-pattern nil
  "The current search pattern used by locate.")


(defvar pushy-mode nil
  "*Non-nil means Pushy Completion mode is enabled.")


(defun pushy-mode (&optional arg)
    "Toggle Pushy Completion mode.

Non-nil prefix ARG turns mode on if ARG is positive, else turns it off."
    (interactive "P")
    (setq pushy-mode (if arg
                         (> (prefix-numeric-value arg) 0)
                       (not pushy-mode)))
    (if pushy-mode
        (progn (add-hook 'minibuffer-setup-hook  'pushy-minibuffer-setup)
               (add-hook 'minibuffer-exit-hook  'pushy-minibuffer-exit)
               (message "Pushy completion mode is enabled."))

      (remove-hook 'minibuffer-setup-hook  'pushy-minibuffer-setup)
      (remove-hook 'minibuffer-exit-hook  'pushy-minibuffer-exit)
      (message "Pushy completion mode is disabled.")))


(defun pushy-minibuffer-setup ()
  "Prepare pushy completion if it is enabled for the current command."
  (when (and (symbolp this-command)
             (not (eq (get this-command 'pushy-completion) 'disabled))
             (or pushy-enable-globally
                 (eq (get this-command 'pushy-completion) 'enabled)))
    (setq pushy-pattern-start-position (point))
    (setq pushy-previous-pattern "")
    (setq pushy-trigger-command this-command)
    (add-hook 'post-command-hook 'pushy-post-command)))


(defun pushy-minibuffer-exit ()
  "Cleanup after pushy before exiting the minibuffer."
  (pushy-cleanup))


(defun pushy-post-command ()
  "If a pattern editing command is used during pushy completion and
there is no new input event within `pushy-search-delay' seconds then
update the list of completions."
  (when (sit-for (let ((func (get pushy-trigger-command
                                  'pushy-search-delay-function)))
                   (if func
                       (funcall func)
                     pushy-search-delay)))
    (let ((pattern
           (or (let ((func (get pushy-trigger-command
                                'pushy-pattern-function)))
                 (if func
                     (funcall func)))

               (if (>= (line-end-position) pushy-pattern-start-position)
                   (buffer-substring pushy-pattern-start-position
                                     (line-end-position))
                 "")))

          (history-variable (or (get pushy-trigger-command
                                     'pushy-candidates-variable)
                                minibuffer-history-variable)))

      (unless (equal pattern pushy-previous-pattern)
        (setq pushy-previous-pattern pattern)

        (with-current-buffer (get-buffer-create pushy-buffer)
          (erase-buffer)
          (if pushy-overlay
              ;; make sure the overlay belongs to the pushy buffer if
              ;; it's newly created
              (move-overlay pushy-overlay (point-min) (point-min)
                            (get-buffer pushy-buffer))

            (setq pushy-overlay (make-overlay (point-min) (point-min)
                                              (get-buffer pushy-buffer)))
            (overlay-put pushy-overlay 'face 'highlight))
          (pushy-set-state nil)

          (if (not (equal pattern ""))
              (let ((items (symbol-value history-variable))
                    matches)
                (unless (stringp (car items))
                  (setq items (mapcar (lambda (x) (prin1-to-string x))
                                      items)))

                ;; the list is reversed here, so that when it is
                ;; filtered below and there are duplicates element
                ;; in it then from the multiple duplicates the
                ;; most recent element is put in front of the list
                (setq items (reverse items))     ; reserve is used
                                        ; instead of nreserve
                                        ; to avoid clobbering
                                        ; the original
                                        ; history list

                ;; if we're completing file names and last buffer
                ;; has an associated file then show all matching
                ;; files not already present in the history from
                ;; the directory of the associated file
                (if (eq history-variable 'file-name-history)
                    (let ((file (buffer-file-name (cadr (buffer-list)))))
                      (if file
                          (setq items (append 
                                       (directory-files 
                                        (file-name-directory file)
                                        t)
                                       items)))))

                (let ((transformers 
                       (if (and pushy-list-basename-matches-first
                                (eq history-variable 'file-name-history))
                           '(file-name-directory file-name-nondirectory)
                         '(identity))))

                  (dolist (transformer transformers)
                    (dolist (item items)
                      (let ((transformed-item (funcall transformer item)))
                        (when (and transformed-item ; file-name-directory
                                        ; returns nil for ~
                                   (string-match pattern transformed-item))
                          (setq matches (delete item matches))
                          (push item matches))))))

                (setq pushy-history-matches matches)

                (dolist (match matches)
                  (insert match "\n"))

                (when (eq history-variable 'file-name-history)
                  (goto-char (point-min))
                  (pushy-transform-file-names)))))

        (if (or (= (with-current-buffer (get-buffer pushy-buffer)
                     (buffer-size))
                   0)
                ;; don't show the completion window if there is
                ;; only one match and it is identical to the
                ;; current pattern
                (with-current-buffer (get-buffer pushy-buffer)
                  (goto-char (point-min))
                  (and (equal (buffer-substring (line-beginning-position)
                                                (line-end-position))
                              pattern)
                       (forward-line 1)
                       (eobp))))
            (pushy-hide-completion-window)

          (pushy-show-completion-window)
          (save-selected-window
            (select-window (get-buffer-window pushy-buffer))
            (goto-char (point-min))
            (pushy-mark-current-line)))

        (when (and pushy-use-locate
                   (eq history-variable 'file-name-history)
                   (>= (length pattern)
                       pushy-locate-minimum-pattern-length))
          (setq pushy-locate-basename-search pushy-list-basename-matches-first)
          (setq pushy-locate-pattern pattern)
          (pushy-locate pattern))))))


(defun pushy-mark-current-line ()
  "Mark current line as selected."
  (move-overlay pushy-overlay
                (line-beginning-position)
                (1+ (line-end-position))))


(defun pushy-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (pushy-move-selection 'next-line -1))


(defun pushy-next-line ()
  "Move selection to the next line."
  (interactive)
  (pushy-move-selection 'next-line 1))


(defun pushy-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (pushy-move-selection 'scroll-down nil))


(defun pushy-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (pushy-move-selection 'scroll-up nil))


(defun pushy-move-selection (movefunc movearg)
  "Move the selection marker to a new position determined by
MOVEFUNC and MOVEARG."
  (save-selected-window
    (select-window (get-buffer-window pushy-buffer))

    (condition-case nil
        (funcall movefunc movearg)
      (beginning-of-buffer (goto-char (point-min)))
      (end-of-buffer (goto-char (point-max))))

    (if (eobp)
        (next-line -1))

    (pushy-mark-current-line)))


(defun pushy-select-item (&optional insert-only)
  "Selected the current pushy item.

if INSERT-ONLY is t then the item is inserted into the minibuffer, but it is not selected.
"
  (interactive)
  (let ((selected (with-current-buffer pushy-buffer
                    (or (get-text-property (overlay-start pushy-overlay)
                                           'pushy-orig-filename)
                        (buffer-substring-no-properties
                         (overlay-start pushy-overlay)
                         (line-end-position)))))
        (func (get pushy-trigger-command 'pushy-visit-file-function)))
    (if func
        (funcall func selected)

      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position))
      (insert selected)
      (unless insert-only
        (exit-minibuffer)))))


(defun pushy-insert-item ()
  "Insert the current pushy item into the minibuffer."
  (interactive)
  (pushy-select-item t))


(defun pushy-cleanup ()
  "Hide the pushy completion window if necessary and remove the
installed hooks."
  (interactive)
  (pushy-hide-completion-window)
  (remove-hook 'post-command-hook 'pushy-post-command))


(defun pushy-show-completion-window ()
  "Show the completion window and rebind keys for file selection if
necessary."
  (unless pushy-window-config
    (setq pushy-window-config (current-window-configuration))
    (save-selected-window
      (pop-to-buffer pushy-buffer)
      (setq mode-name "Pushy")
      (setq cursor-type nil))

    (dolist (key pushy-keys)
      (pushy-redefine-key (car key) (cdr key)))))


(defun pushy-hide-completion-window ()
  "Hide the completion window and restore bindings for the keys used
for file selection if necessary. Also kill the locate process if it is
running."
  (when pushy-window-config
    (set-window-configuration pushy-window-config)
    (setq pushy-window-config nil)
    (with-current-buffer pushy-buffer
      (setq cursor-type t))

    (dolist (def pushy-saved-commands)
      (define-key (current-local-map) (car def) (cdr def)))
    (setq pushy-saved-commands nil)

    (pushy-kill-locate-process)))


(defun pushy-redefine-key (key command)
  "Redefine KEY to COMMAND and save the previous binding to
`pushy-saved-commands'."
  (push (cons key (lookup-key (current-local-map) key))
        pushy-saved-commands)
  (define-key (current-local-map) key command))


(defun pushy-set-state (state)
  "Set STATE in mode line."
  (with-current-buffer pushy-buffer
    (setq mode-line-process (if state (concat ":" state)))
    (force-mode-line-update)))


(defun pushy-transform-file-names ()
  "Transform file names in the pushy buffer from the current line to
the end of *pushy* buffer according to `pushy-transform-regexps'."
  (let ((begin (line-beginning-position)))
    (dolist (rule pushy-transform-regexps)
      (goto-char begin)
      (while (re-search-forward (car rule) nil t)
        ;; original path is saved in a text property
        (let ((orig-path                         
               (or (get-text-property (line-beginning-position)
                                      'pushy-orig-filename)
                   (buffer-substring-no-properties 
                    (line-beginning-position) (line-end-position)))))

          (replace-match (cdr rule))

          (put-text-property (line-beginning-position) (line-end-position) 
                             'pushy-orig-filename
                             orig-path))))))


(defun pushy-locate (pattern)
  "Start a locate process for query PATTERN."
  (pushy-kill-locate-process)
  (setq pushy-locate-process (apply 'start-process "pushy-process" nil
                                    "locate"
                                    (append
                                     (if pushy-locate-databases
                                         (list (concat
                                                "--database="
                                                pushy-locate-databases)))
                                     (if pushy-locate-basename-search
                                         (list "-b"))
                                     (list "-i"
                                           "-r"
                                           pattern))))

  (set-process-filter pushy-locate-process 'pushy-locate-output-filter)
  (set-process-sentinel pushy-locate-process 'pushy-locate-process-sentinel)
  (pushy-set-state "locating"))


(defun pushy-locate-output-filter (process string)
  "Process output from locate."
  (with-current-buffer (get-buffer-create pushy-buffer)
    (save-excursion
      (goto-char (point-max))
      (let (line)
        (save-excursion
          (insert string)
          ;; the last inserted line from the output can be incomplete,
          ;; so store and remove it before filtering
          (setq line (buffer-substring (line-beginning-position)
                                       (line-end-position)))
          (delete-region (line-beginning-position) (line-end-position)))

        ;; filter out unwanted files from locate output
        (dolist (regexp pushy-filter-regexps)
          (flush-lines regexp))

        ;; filter out files already shown from the history
        (save-excursion
          (beginning-of-line)
          (let (start file)
            (while (not (eobp))
              (setq file (buffer-substring (line-beginning-position) 
                                           (line-end-position)))
              (if (member file pushy-history-matches)
                  (setq start (line-beginning-position))

                (push file pushy-history-matches)
                (setq start nil))

              (forward-line)
              (if start
                  (delete-region start (point))))))

        (pushy-transform-file-names)

        ;; reinsert deleted line
        (goto-char (point-max))
        (insert line))))

  (when (= (overlay-start pushy-overlay) ; no selection yet
           (overlay-end pushy-overlay))
     (pushy-show-completion-window)
     (save-selected-window
       (select-window (get-buffer-window pushy-buffer))
       (pushy-mark-current-line)))

  (when (and pushy-locate-matching-filename-limit
             (>= (length pushy-history-matches) 
                 pushy-locate-matching-filename-limit))
    (pushy-kill-locate-process)
    (pushy-set-state "limit reached")))


(defun pushy-locate-process-sentinel (process event)
  "Set status to finished if the locate process exits."
  (unless (eq 'run (process-status process))
    (if pushy-locate-basename-search
        (progn (setq pushy-locate-basename-search nil)
               (pushy-locate pushy-locate-pattern))
      (pushy-set-state "finished"))))


(defun pushy-kill-locate-process ()
  "Kill locate process."
  (when pushy-locate-process
    ;; detach associated functions
    (set-process-filter pushy-locate-process nil)
    (set-process-sentinel pushy-locate-process nil)
    (delete-process pushy-locate-process)
    (setq pushy-locate-process nil)))


;;----------------------------------------------------------------------
;; Iswitchb support
;;----------------------------------------------------------------------

;; Iswitchb is disabled by default, because it requires special
;; treatment, so it shouldn't be allowed automatically if
;; pushy-enable-globally is set
(put 'iswitchb-buffer 'pushy-completion 'disabled)


(defvar pushy-iswitchb-selected-file nil
  "The file which was selected using pushy from iswitchb.")


(defun pushy-iswitchb-setup ()
  "Setup pushy to work with iswitchb."
  (interactive)
  (require 'iswitchb)
  (put 'iswitchb-buffer 'pushy-pattern-function 'pushy-iswitchb-get-pattern)
  (put 'iswitchb-buffer 'pushy-candidates-variable 'file-name-history)
  (put 'iswitchb-buffer 'pushy-visit-file-function 'pushy-iswitchb-visit-file)
  (put 'iswitchb-buffer 'pushy-search-delay-function 'pushy-iswitchb-search-delay)
  (put 'iswitchb-buffer 'pushy-completion 'enabled))


(defun pushy-iswitchb-get-pattern ()
  "Return the current iswitch pattern."
  iswitchb-text)


(defun pushy-iswitchb-search-delay ()
  "If there are matching buffers use a longer completion delay to
interfere less with normal buffer switching. Otherwise, use the
standard delay."
  (if iswitchb-matches
      (* 2 pushy-search-delay)
    pushy-search-delay))


(defun pushy-iswitchb-visit-file (file)
  "Visit file from iswitchb."
  (setq pushy-iswitchb-selected-file file)
  ;; is there a nicer way to do this?
  (add-hook 'minibuffer-setup-hook 'pushy-iswitchb-visit-file-hook)
  (iswitchb-find-file))


(defun pushy-iswitchb-visit-file-hook ()
  "Visit file selected from iswitchb."
  (remove-hook 'minibuffer-setup-hook 'pushy-iswitchb-visit-file-hook)
  (delete-region (line-beginning-position) (line-end-position))
  (insert pushy-iswitchb-selected-file)
  ;; is there a better way to do this?
  (setq unread-command-events (cons ?\n unread-command-events)))


(provide 'pushy)

