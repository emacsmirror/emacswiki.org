;;; timid.el --- timid completion

;; Copyright (C) 2007  Tamas Patrovics

;; Author: Tamas Patrovics
;; Maintainer: Tamas Patrovics
;; Keywords: completion
;; X-URL: http://www.emacswiki.org/cgi-bin/emacs/timid.el

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

;; I work with files. Lots of files. So I want to open files as
;; effortlessly as possible. This package is a part of the quest for the
;; effortless file opening.
;;
;; I noticed lately that I use the file history more and more for opening
;; files. I use savehist with a history length of 1000, duplicates
;; filtered out, so there is a good chance a file I want to open is in
;; the history.
;;
;; I use Icicles, so I can use it to match files from the history:
;;
;;   find-file <delete input> <type pattern> M-h
;;
;; Pretty easy. But not effortless enough for a frequent operation. In my
;; opinion at least.
;;
;; Why do I have to tell emacs I want to complete a file from the
;; history, not from the file system? It should simply tell me if there
;; is a match from the history without me doing anything explicitly.
;;
;; And that's what timid completion does.
;;
;; When find-file is started there is nothing unusual. If the user
;; presses TAB, or looks for a file with UP/DOWN in the history, or
;; anything then everything works as expected.
;;
;; If the user simply types a few characters and hesitates a bit
;; (configurable) then timid completion looks for matches in the history
;; and displays them if there is any and the user can select a match with
;; the UP/DOWN keys and ENTER (so these keys are redefined if there are
;; matches to select from). If more characters are typed the list of
;; matches is updated. If any other command (e.g. TAB) is used then timid
;; completion disables itself.
;;
;; So timid completion shows matches from the history automatically while
;; being as transparent as possible. It appears only if it can offer
;; something to choose from and disappears instantly if the user chooses
;; to ignore it. It's timid, you know.
;;
;; Timid completion can be enabled with
;;
;; 	(timid-mode t)
;;
;; This enables timid completion mode, but you also have to specify
;; which commands you want to have affected by timid.
;;
;; Timid completion can be turned on for all commands by customizing
;; variable `timid-enable-globally'. Alternatively, it can be enabled
;; for individual commands by putting property `timid-completion' with
;; value `enabled' on the command. Also, it can be enabled globally
;; and be disabled for individual commands by setting value of
;; property `timid-completion' to `disabled' for the command. See
;; below for a complete list of possible command properties.
;;
;; Iswitchb requires special treatment, so use this command to enable
;; timid for it:
;;
;; 	(timid-iswitchb-setup)
;;
;;
;; In order to take full advantage of timid completion use of package
;; savehist is recommended with a sufficently high history-length
;; value (I use 1000).
;;
;; Also it is recommended to filter duplicates from the file history
;; and push every file opened or written in Emacs on it, so they can
;; be reopened easily if they are needed again:
;;
;;      (defun my-add-file-hook ()
;;       "Add the name of the file just opened or written to
;;      `file-name-history'"
;;       (and buffer-file-name
;;            (progn (setq file-name-history
;;                         (delete buffer-file-name file-name-history))
;;                   (push buffer-file-name file-name-history)))
;;       nil)
;;
;;      (add-hook 'find-file-hooks  'my-add-file-hook)
;;      (add-hook 'write-file-hooks 'my-add-file-hook)
;;
;; I use Emacs 21, there may be a builtin method for the code above
;; in Emacs 22.
;;
;;
;; Complete list of command properties for timid completion:
;;
;;   Property: timid-completion
;;
;;   Values:
;;
;;     enabled - Timid is enabled for commands having this
;;               property. This is not necessary if you enabled timid
;;               globally via variable `timid-enable-globally'.
;;
;;     disabled - Timid is disabled for this command. This is useful
;;                if you have enabled timid globally via variable
;;                `timid-enable-globally', but want to inhibit it for
;;                certain commands.
;;
;;     allowed - The command is allowed to use during timid completion
;;               without terminating it.
;;
;;     edit - The command is used to modify the search pattern during
;;            timid completion.
;;
;;
;;   Property: timid-search-delay-function
;;
;;   Value: Function to determine how much should timit wait before
;;          the completion list is updated. If not set the value of
;;          `timid-search-delay' is used. The function is called with
;;          no argument.
;;
;;
;;   Property: timid-pattern-function
;;
;;   Value: Function to get the current search pattern. If not set the
;;          pattern is retrieved from the minibuffer. The function is
;;          called with no argument.
;;
;;
;;   Property: timid-candidates-variable
;;
;;   Value: The variable containing the list of completion candidates. If not
;;          set then `minibuffer-history-variable' is used.
;;
;;
;;   Property: timid-visit-file-function
;;
;;   Value: Function to visit the selected file. It is called with one
;;          argument, the path of the selected file. If not set then
;;          the file path is inserted into the minibuffer and the
;;          function `exit-minibuffer' is called.
;;
;;
;; timid.el is tested on Gnu Emacs 21. It is known to work with Gnu
;; Emacs 22. Compatibility with other Emacs versions is purely
;; coincidental. :)
;;
;; Thanks to Drew Adams for his valuable suggestions on improving this
;; package.
;;
;; Thanks to Vinicius José Latorre for customize support.
;;

;;; Code:

;;; User options

(defgroup timid nil
  "Timid Completion Group."
  :tag "Timid Completion"
  :link '(emacs-library-link :tag "Source Lisp File" "timid.el")
  :prefix "timid-"
  :version "22"
  :group 'emacs)

(defcustom timid-enable-globally nil
  "*Enable timid for all commands which use the minibuffer. 

Note that this is not yet tested with every emacs command. YMMV."
  :type 'boolean
  :version "22"
  :group 'timid)

(defcustom timid-search-delay 0.5
  "*Idle time after last input event before searching for matches in the history."
  :type 'number
  :version "22"
  :group 'timid)

(defcustom timid-list-basename-matches-first t
  "*In case of file completion put those files first in the
completion list where the basename matches the pattern."
  :type 'boolean
  :version "22"
  :group 'timid)

(defcustom timid-filter-regexps nil
 "*List of regular expressions to filter out unwanted files from the
output."
 :type '(repeat regexp)
 :group 'timid)

(defcustom timid-transform-regexps nil
 "*List of (REGEXP . REPLACEMENT) pairs to transform matching file
path names. It's useful when the matching path names are very long and
they have a component which can safely be replaced with a shorter
indicator string.

For example this rule:

    (push '(\"^/very/long/path/to/projectx/\" . \"<projx>/\")
      timid-transform-regexps)

will display file names under \"projectx\" like this:

    <projx>/sources/main.c
    <projx>/sources/test.c

"
 :type '(repeat (cons regexp regexp))
 :version "22"
 :group 'timid)

(defcustom timid-use-locate nil
  "*Non-nil means use locate.
It uses locate to find further completions not present in the history in case
of file completion."
  :type 'boolean
  :version "22"
  :group 'timid)

(defcustom timid-locate-databases nil
  "*Database files string or nil.
If it is string, it has a list of database files separated with colon to be
used by the locate command.

If it is nil, the system default database is used."
  :type '(choice (const  :tag "Default Database" nil)
		 (string :tag "Database(s)"))
  :version "22"
  :group 'timid)

(defcustom timid-locate-minimum-pattern-length 3
  "*Minimum number of characters needed to start file searching with locate."
  :type 'integer
  :version "22"
  :group 'timid)

(defcustom timid-locate-matching-filename-limit 500
  "*If there are more matching file names than the given limit the
search is terminated automatically. This is useful if a too broad
search input is given and there are hundreds or thousands of matches.

The limit is checked only if `timid-use-locate' is enabled, because
matches from the internal history lists can be processed quickly.

If you don't want to limit the number of matches then set it to nil
instead of a very high number."
  :type '(choice integer (const nil))
  :group 'timid)

(defcustom timid-keys
  (list (cons (kbd "<RET>") 'timid-select-file)
	(cons (kbd "<ESC>") 'timid-cleanup)
	(cons (kbd "<up>") 'timid-previous-line)
	(cons (kbd "<down>") 'timid-next-line)
	(cons (kbd "<prior>") 'timid-previous-page)
	(cons (kbd "<next>") 'timid-next-page))
  "*Alist of keybindings for timid completion."
  :type '(alist :key-type   (sexp     :tag "Key")
		:value-type (function :tag "Function"))
  :version "22"
  :group 'timid)

;;;----------------------------------------------------------------------

;; these commands are allowed during timid completion
(dolist (cmd '(forward-char
               forward-char-nomark
               backward-char
               backward-char-nomark

               ;; timid commands
               timid-select-file
               timid-previous-line
               timid-next-line
               timid-previous-page
               timid-next-page))
  (put cmd 'timid-completion 'allowed))

;; these commands are allowed too, but they can also modify the pattern
(dolist (cmd '(self-insert-command
               delete-backward-char
               icicle-self-insert
               icicle-delete-backward-char))
  (put cmd 'timid-completion 'edit))

;; these commands don't work well with timid completion
(dolist (cmd '(globalff))
  (put cmd 'timid-completion 'disabled))


(defvar timid-buffer "*timid*"
  "Name of buffer showing completions.")

(defvar timid-saved-commands nil
  "List of commands saved when timid overrides keys in the local map.")

(defvar timid-window-config nil
  "Window configuration before timid completions were shown.")

(defvar timid-pattern-start-position nil
  "Position of beginning of pattern in the minibuffer.")

(defvar timid-overlay nil
  "Overlay used to highlight the currently selected file.")

(defvar timid-previous-pattern ""
  "The last pattern used to update the completion buffer.")

(defvar timid-trigger-command nil
  "The current command which triggered timid completion.")

(defvar timid-history-matches nil
  "The list of matches for the current input pattern from the history.")

(defvar timid-locate-process nil
  "The current locate process.")

(defvar timid-locate-basename-search nil
  "Indicates whether this is the basename search pass of locate.

When searching with locate those files are listed first where the
basename matches the search pattern and then those where the match is
anywhere in the path.")

(defvar timid-locate-pattern nil
  "The current search pattern used by locate.")


(defvar timid-mode nil
  "*Non-nil means Timid Completion mode is enabled.")


(defun timid-mode (&optional arg)
    "Toggle Timid Completion mode.

Non-nil prefix ARG turns mode on if ARG is positive, else turns it off."
    (interactive "P")
    (setq timid-mode (if arg
                         (> (prefix-numeric-value arg) 0)
                       (not timid-mode)))
    (if timid-mode
        (progn (add-hook 'minibuffer-setup-hook  'timid-minibuffer-setup)
               (add-hook 'minibuffer-exit-hook  'timid-minibuffer-exit)
               (message "Timid completion mode is enabled."))

      (remove-hook 'minibuffer-setup-hook  'timid-minibuffer-setup)
      (remove-hook 'minibuffer-exit-hook  'timid-minibuffer-exit)
      (message "Timid completion mode is disabled.")))


(defun timid-minibuffer-setup ()
  "Prepare timid completion if it is enabled for the current command."
  (when (and (not (eq (get this-command 'timid-completion) 'disabled))
             (or timid-enable-globally
                 (eq (get this-command 'timid-completion) 'enabled)))
    (setq timid-pattern-start-position (point))
    (setq timid-previous-pattern "")
    (setq timid-trigger-command this-command)
    (add-hook 'pre-command-hook 'timid-pre-command)
    (add-hook 'post-command-hook 'timid-post-command)))


(defun timid-minibuffer-exit ()
  "Cleanup after timid before exiting the minibuffer."
  (timid-cleanup))


(defun timid-pre-command ()
  "Terminate timid if a command is used which is not allowed during
timid completion."
  (unless (or (eq (get this-command 'timid-completion) 'allowed)
              (eq (get this-command 'timid-completion) 'edit))
    (timid-cleanup)))


(defun timid-post-command ()
  "If a pattern editing command is used during timid completion and
there is no new input event within `timid-search-delay' seconds then
update the list of completions."
  (if (eq (get this-command 'timid-completion) 'edit)
      (when (sit-for (let ((func (get timid-trigger-command
                                      'timid-search-delay-function)))
                       (if func
                           (funcall func)
                         timid-search-delay)))
        (let ((pattern
               (or (let ((func (get timid-trigger-command
                                    'timid-pattern-function)))
                     (if func
                         (funcall func)))

                   (if (>= (line-end-position) timid-pattern-start-position)
                       (buffer-substring timid-pattern-start-position
                                         (line-end-position))
                     "")))

              (history-variable (or (get timid-trigger-command
                                            'timid-candidates-variable)
                                       minibuffer-history-variable)))

          (unless (equal pattern timid-previous-pattern)
            (setq timid-previous-pattern pattern)

            (with-current-buffer (get-buffer-create timid-buffer)
              (erase-buffer)
              (if timid-overlay
                  ;; make sure the overlay belongs to the timid buffer if
                  ;; it's newly created
                  (move-overlay timid-overlay (point-min) (point-min)
                                (get-buffer timid-buffer))

                (setq timid-overlay (make-overlay (point-min) (point-min)
                                                  (get-buffer timid-buffer)))
                (overlay-put timid-overlay 'face 'highlight))
              (timid-set-state nil)

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
                    (setq items (reverse items)) ; reserve is used
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
                           (if (and timid-list-basename-matches-first
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

                    (setq timid-history-matches matches)

                    (dolist (match matches)
                      (insert match "\n"))

                    (when (eq history-variable 'file-name-history)
                      (goto-char (point-min))
                      (timid-transform-file-names)))))

            (if (or (= (with-current-buffer (get-buffer timid-buffer)
                         (buffer-size))
                       0)
                    ;; don't show the completion window if there is
                    ;; only one match and it is identical to the
                    ;; current pattern
                    (with-current-buffer (get-buffer timid-buffer)
                      (goto-char (point-min))
                      (and (equal (buffer-substring (line-beginning-position)
                                                    (line-end-position))
                                  pattern)
                           (forward-line 1)
                           (eobp))))
                (timid-hide-completion-window)

              (timid-show-completion-window)
              (save-selected-window
                (select-window (get-buffer-window timid-buffer))
                (goto-char (point-min))
                (timid-mark-current-line)))

            (when (and timid-use-locate
                       (eq history-variable 'file-name-history)
                       (>= (length pattern)
                           timid-locate-minimum-pattern-length))
              (setq timid-locate-basename-search timid-list-basename-matches-first)
              (setq timid-locate-pattern pattern)
              (timid-locate pattern)))))))


(defun timid-mark-current-line ()
  "Mark current line as selected."
  (move-overlay timid-overlay
                (line-beginning-position)
                (1+ (line-end-position))))


(defun timid-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (timid-move-selection 'next-line -1))


(defun timid-next-line ()
  "Move selection to the next line."
  (interactive)
  (timid-move-selection 'next-line 1))


(defun timid-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (timid-move-selection 'scroll-down nil))


(defun timid-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (timid-move-selection 'scroll-up nil))


(defun timid-move-selection (movefunc movearg)
  "Move the selection marker to a new position determined by
MOVEFUNC and MOVEARG."
  (save-selected-window
    (select-window (get-buffer-window timid-buffer))

    (condition-case nil
        (funcall movefunc movearg)
      (beginning-of-buffer (goto-char (point-min)))
      (end-of-buffer (goto-char (point-max))))

    (if (eobp)
        (next-line -1))

    (timid-mark-current-line)))


(defun timid-select-file ()
  "Open the selected file."
  (interactive)
  (let ((selected (with-current-buffer timid-buffer
                    (or (get-text-property (overlay-start timid-overlay)
                                           'timid-orig-filename)
                        (buffer-substring-no-properties
                         (overlay-start timid-overlay)
                         (line-end-position)))))
        (func (get timid-trigger-command 'timid-visit-file-function)))
    (if func
        (funcall func selected)

      (beginning-of-line)
      (kill-line)
      (insert selected)
      (exit-minibuffer))))


(defun timid-cleanup ()
  "Hide the timid completion window if necessary and remove the
installed hooks."
  (interactive)
  (timid-hide-completion-window)
  (remove-hook 'pre-command-hook 'timid-pre-command)
  (remove-hook 'post-command-hook 'timid-post-command))


(defun timid-show-completion-window ()
  "Show the completion window and rebind keys for file selection if
necessary."
  (unless timid-window-config
    (setq timid-window-config (current-window-configuration))
    (save-selected-window
      (pop-to-buffer timid-buffer)
      (setq mode-name "Timid")
      (setq cursor-type nil))

    (dolist (key timid-keys)
      (timid-redefine-key (car key) (cdr key)))))


(defun timid-hide-completion-window ()
  "Hide the completion window and restore bindings for the keys used
for file selection if necessary. Also kill the locate process if it is
running."
  (when timid-window-config
    (set-window-configuration timid-window-config)
    (setq timid-window-config nil)
    (with-current-buffer timid-buffer
      (setq cursor-type t))

    (dolist (def timid-saved-commands)
      (define-key (current-local-map) (car def) (cdr def)))
    (setq timid-saved-commands nil)

    (timid-kill-locate-process)))


(defun timid-redefine-key (key command)
  "Redefine KEY to COMMAND and save the previous binding to
`timid-saved-commands'."
  (push (cons key (lookup-key (current-local-map) key))
        timid-saved-commands)
  (define-key (current-local-map) key command))


(defun timid-set-state (state)
  "Set STATE in mode line."
  (with-current-buffer timid-buffer
    (setq mode-line-process (if state (concat ":" state)))
    (force-mode-line-update)))


(defun timid-transform-file-names ()
  "Transform file names in the timid buffer from the current line to
the end of *timid* buffer according to `timid-transform-regexps'."
  (let ((begin (line-beginning-position)))
    (dolist (rule timid-transform-regexps)
      (goto-char begin)
      (while (re-search-forward (car rule) nil t)
        ;; original path is saved in a text property
        (let ((orig-path                         
               (or (get-text-property (line-beginning-position)
                                      'timid-orig-filename)
                   (buffer-substring-no-properties 
                    (line-beginning-position) (line-end-position)))))

          (replace-match (cdr rule))

          (put-text-property (line-beginning-position) (line-end-position) 
                             'timid-orig-filename
                             orig-path))))))


(defun timid-locate (pattern)
  "Start a locate process for query PATTERN."
  (timid-kill-locate-process)
  (setq timid-locate-process (apply 'start-process "timid-process" nil
                                    "locate"
                                    (append
                                     (if timid-locate-databases
                                         (list (concat
                                                "--database="
                                                timid-locate-databases)))
                                     (if timid-locate-basename-search
                                         (list "-b"))
                                     (list "-i"
                                           "-r"
                                           pattern))))

  (set-process-filter timid-locate-process 'timid-locate-output-filter)
  (set-process-sentinel timid-locate-process 'timid-locate-process-sentinel)
  (timid-set-state "locating"))


(defun timid-locate-output-filter (process string)
  "Process output from locate."
  (with-current-buffer (get-buffer-create timid-buffer)
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
        (dolist (regexp timid-filter-regexps)
          (flush-lines regexp))

        ;; filter out files already shown from the history
        (save-excursion
          (beginning-of-line)
          (let (start file)
            (while (not (eobp))
              (setq file (buffer-substring (line-beginning-position) 
                                           (line-end-position)))
              (if (member file timid-history-matches)
                  (setq start (line-beginning-position))

                (push file timid-history-matches)
                (setq start nil))

              (forward-line)
              (if start
                  (delete-region start (point))))))

        (timid-transform-file-names)

        ;; reinsert deleted line
        (goto-char (point-max))
        (insert line))))

  (when (= (overlay-start timid-overlay) ; no selection yet
           (overlay-end timid-overlay))
     (timid-show-completion-window)
     (save-selected-window
       (select-window (get-buffer-window timid-buffer))
       (timid-mark-current-line)))

  (when (and timid-locate-matching-filename-limit
             (>= (length timid-history-matches) 
                 timid-locate-matching-filename-limit))
    (timid-kill-locate-process)
    (timid-set-state "limit reached")))


(defun timid-locate-process-sentinel (process event)
  "Set status to finished if the locate process exits."
  (unless (eq 'run (process-status process))
    (if timid-locate-basename-search
        (progn (setq timid-locate-basename-search nil)
               (timid-locate timid-locate-pattern))
      (timid-set-state "finished"))))


(defun timid-kill-locate-process ()
  "Kill locate process."
  (when timid-locate-process
    ;; detach associated functions
    (set-process-filter timid-locate-process nil)
    (set-process-sentinel timid-locate-process nil)
    (delete-process timid-locate-process)
    (setq timid-locate-process nil)))


;;----------------------------------------------------------------------
;; Iswitchb support
;;----------------------------------------------------------------------

;; Iswitchb is disabled by default, because it requires special
;; treatment, so it shouldn't be allowed automatically if
;; timid-enable-globally is set
(put 'iswitchb-buffer 'timid-completion 'disabled)


(defvar timid-iswitchb-selected-file nil
  "The file which was selected using timid from iswitchb.")


(defun timid-iswitchb-setup ()
  "Setup timid to work with iswitchb."
  (interactive)
  (require 'iswitchb)
  (put 'iswitchb-buffer 'timid-pattern-function 'timid-iswitchb-get-pattern)
  (put 'iswitchb-buffer 'timid-candidates-variable 'file-name-history)
  (put 'iswitchb-buffer 'timid-visit-file-function 'timid-iswitchb-visit-file)
  (put 'iswitchb-buffer 'timid-search-delay-function 'timid-iswitchb-search-delay)
  (put 'iswitchb-buffer 'timid-completion 'enabled))


(defun timid-iswitchb-get-pattern ()
  "Return the current iswitch pattern."
  iswitchb-text)


(defun timid-iswitchb-search-delay ()
  "If there are matching buffers use a longer completion delay to
interfere less with normal buffer switching. Otherwise, use the
standard delay."
  (if iswitchb-matches
      (* 2 timid-search-delay)
    timid-search-delay))


(defun timid-iswitchb-visit-file (file)
  "Visit file from iswitchb."
  (setq timid-iswitchb-selected-file file)
  ;; is there a nicer way to do this?
  (add-hook 'minibuffer-setup-hook 'timid-iswitchb-visit-file-hook)
  (iswitchb-find-file))


(defun timid-iswitchb-visit-file-hook ()
  "Visit file selected from iswitchb."
  (remove-hook 'minibuffer-setup-hook 'timid-iswitchb-visit-file-hook)
  (delete-region (line-beginning-position) (line-end-position))
  (insert timid-iswitchb-selected-file)
  ;; is there a better way to do this?
  (setq unread-command-events (cons ?\n unread-command-events)))


(provide 'timid)

