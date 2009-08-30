;;; analog.el --- monitor lists of files or command output

;;; Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005 Matthew P. Hodges

;; Author: Matthew P. Hodges <MPHodges@member.fsf.org>
;; Version: $Id: analog.el,v 1.112 2005/01/17 09:38:34 mphodges-guest Exp $

;; analog.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; analog.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; Package to keep track of a list of specified files or the output of
;; specified commands. The principal variable to modify is
;; analog-entry-list, which should be set to a list of entries. This
;; can be modified with:
;;
;;     M-x customize-variable RET analog-entry-list RET
;;
;; Each element of analog-entry-list is a list where the car is a file
;; (or command) and the cdr is an association list of properties.
;;
;; By default, entries are files, but commands can also be specified.
;; Each entry can have a list of attributes describing whether the
;; head or the tail of the output is wanted, how many lines should be
;; kept, a list of regexps to keep or flush etc. Entries can be
;; collected into named groups.

;;; Code:

(defconst analog-version "1.9.99"
  "Version number of this package.")

(require 'dired)
(require 'pp)

;; Customizable variables

(defgroup analog nil
  "Monitor lists of files or command output."
  :group 'tools
  :link '(url-link "http://www.tc.bham.ac.uk/~matt/published/Public/AnalogEl.html"))

(defcustom analog-default-no-lines 4
  "*The default number of lines to display."
  :group 'analog
  :type 'integer)

(defcustom analog-default-position 'tail
  "*The default position of the file to display.
This can be 'head or 'tail."
  :group 'analog
  :type '(choice (const tail)
                 (const head)))

(defcustom analog-default-type 'file
  "*The default type of entry.
Unless otherwise specified, an entry is taken to be this type."
  :group 'analog
  :type '(choice (const file)
                 (const directory)
                 (const shell-command)
                 (const lisp-function)))

(defun analog-eval-string (arg)
  "If arg is a string return it. Otherwise eval expression."
  (cond
   ((stringp arg)
    arg)
   ((functionp arg)
    (funcall arg))
   (t
    nil)))

(defun analog-eval-stringp (value)
  "Return t if VALUE is a string of a lambda expression resulting in a string.
Otherwise, return nil."
  (stringp (analog-eval-string value)))

(define-widget 'analog-actions-widget 'lazy
  "Widget for `analog' actions.  See `analog-entry-list'."
  :tag "Actions"
  :type '(repeat
          (choice
           (cons :tag "Flush lines matching regexp"
                 (function-item flush-lines)
                 (repeat
                  (choice
                   (regexp)
                   (restricted-sexp :tag "Lambda expression evaluating to string"
                                    :match-alternatives (analog-eval-stringp)))))
           (cons :tag "Keep lines matching regexp"
                 (function-item keep-lines)
                 (repeat
                  (choice
                   (regexp)
                   (restricted-sexp :tag "Lambda expression evaluating to string"
                                    :match-alternatives (analog-eval-stringp)))))
           (cons :tag "Replace regexp (choose regexp and to-string)"
                 (const replace-regexp)
                 (repeat (cons :tag "regexp/to-string"
                               (choice
                                (regexp)
                                (restricted-sexp :tag "Lambda expression evaluating to string"
                                                 :match-alternatives (analog-eval-stringp)))
                               (choice
                                (regexp)
                                (restricted-sexp :tag "Lambda expression evaluating to string"
                                                 :match-alternatives (analog-eval-stringp))))))
           (cons :tag "Apply shell command to region"
                 (const shell-command-on-region)
                 (repeat string))
           (cons :tag "Apply lisp function [no args]"
                 (const lisp-function)
                 (repeat function))
           (cons :tag "Apply lisp function to region [(point-min) (point-max) as args]"
                 (const lisp-region-function)
                 (repeat function))
           (cons :tag "Apply lisp function to each line"
                 (const lisp-function-map-lines)
                 (repeat function)))))

(define-widget 'analog-faces-widget 'lazy
  "Widget for `analog' faces.  See `analog-entry-list'."
  :tag "Faces"
  :type '(repeat
          (cons
           (choice
            (const :tag "Add face to lines matching regexp" highlight-lines)
            (const :tag "Add face to matching regexp" highlight-regexp))
           (cons
            (choice
             (const :tag "Analog highlight face" analog-highlight-face)
             (const :tag "Analog warning face" analog-warning-face)
             face)
            (repeat
             (choice 
              (regexp)
              (restricted-sexp :tag "Lambda expression evaluating to string"
                               :match-alternatives (analog-eval-stringp))))))))

(define-widget 'analog-alerts-widget 'lazy
  "Widget for `analog' alerts.  See `analog-entry-list'."
  :tag "Alerts"
  :type '(repeat
          (choice
           (cons :tag "Display warning"
                 (const display-warning)
                 (repeat
                  (choice 
                   (regexp)
                   (restricted-sexp :tag "Lambda expression evaluating to string"
                                    :match-alternatives (analog-eval-stringp))))))))

(defcustom analog-entry-list nil
  "*This is a list of analog entries and their associated properties.
If properties are undeclared, defaults will be used."
  :type `(repeat
          (list
           :tag "Analog group" string
           (repeat
            (list
             :tag "Analog entry"
             (string :tag "Entry (file, directory, shell command or lisp function)")
             (cons :tag "Label (optional)" (const label) string)
             (cons :tag "Type of entry" (const type)
                   (choice (const :tag ,(format "default (%s)" analog-default-type) ,analog-default-type)
                           (const :tag "File" file)
                           (const :tag "Directory" directory)
                           (const :tag "Shell command" shell-command)
                           (const :tag "Lisp function name (or lambda expression)" lisp-function)))
             (cons :tag "Maximum number of lines" (const lines)
                   (choice (const :tag ,(format "default (%s)" analog-default-no-lines) default)
                           (integer :value ,analog-default-no-lines)
                           (const all)))
             (cons :tag "Part of entry to show" (const position)
                   (choice (const :tag ,(format "default (%s)" analog-default-position) ,analog-default-position)
                           (const tail)
                           (const head)))
             (cons (const actions) analog-actions-widget)
             (cons (const faces) analog-faces-widget)
             (cons (const alerts) analog-alerts-widget)))))
  :set (lambda (sym val)
         (set sym val)
         (when (and (fboundp 'analog-refresh)
                    analog-entry-list
                    (buffer-live-p (get-buffer "*analog*")))
           (save-excursion (analog-refresh))))
  :group 'analog)

(defcustom analog-group-actions nil
  "*List of actions to apply to specified analog groups.
If group matches the symbol all, the actions are applied to all
groups. See `analog-insert-entry' and `analog-apply-actions'."
  :type `(repeat
          (list
           ,(append (list 'choice :tag "Analog group")
                    (mapcar (lambda (g)
                              (list 'const g))
                            (mapcar #'car analog-entry-list))
                    (list (list 'const :tag "Apply to all groups" :value 'all)))
           analog-actions-widget))
  :set (lambda (sym val)
         (set sym val)
         (when (and (fboundp 'analog-refresh)
                    analog-entry-list
                    (buffer-live-p (get-buffer "*analog*")))
           (save-excursion (analog-refresh))))
  :group 'analog)

(defcustom analog-group-faces nil
  "*List of faces to apply to specified analog groups.
If group matches the symbol all, the faces are applied to all
groups. See `analog-insert-entry' and `analog-apply-faces'."
  :type `(repeat
          (list
           ,(append (list 'choice :tag "Analog group")
                    (mapcar (lambda (g)
                              (list 'const g))
                            (mapcar #'car analog-entry-list))
                    (list (list 'const :tag "Apply to all groups" :value 'all)))
           analog-faces-widget))
  :set (lambda (sym val)
         (set sym val)
         (when (and (fboundp 'analog-refresh)
                    analog-entry-list
                    (buffer-live-p (get-buffer "*analog*")))
           (save-excursion (analog-refresh))))
  :group 'analog)

(defcustom analog-group-alerts nil
  "*List of alerts to apply to specified analog groups.
If group matches the symbol all, the alerts are applied to all
groups. See `analog-insert-entry' and `analog-apply-alerts'."
  :type `(repeat
          (list
           ,(append (list 'choice :tag "Analog group")
                    (mapcar (lambda (g)
                              (list 'const g))
                            (mapcar #'car analog-entry-list))
                    (list (list 'const :tag "Apply to all groups" :value 'all)))
           analog-alerts-widget))
  :set (lambda (sym val)
         (set sym val)
         (when (and (fboundp 'analog-refresh)
                    analog-entry-list
                    (buffer-live-p (get-buffer "*analog*")))
           (save-excursion (analog-refresh))))
  :group 'analog)

(defcustom analog-use-timer nil
  "*If t, the *analog* buffer will periodically be updated.
The frequency of updates is controlled by `analog-timer-period'."
  :group 'analog
  :type 'boolean)

(defcustom analog-timer-period 60
  "*The number of seconds between updates of the *analog* buffer.
Only relevant if timers are being used; see `analog-timer'."
  :group 'analog
  :type 'integer)

(defcustom analog-verbose nil
  "*If t, analog will print a message to the echo area after each update."
  :group 'analog
  :type 'boolean)

(defcustom analog-entry-string "#   Entry: "
  "String indicating entry name in *analog* buffer."
  :group 'analog
  :type 'string)

(defcustom analog-group-string "# Group: "
  "String indicating group name in *analog* buffer."
  :group 'analog
  :type 'string)

(defcustom analog-max-insert-bytes (* 1024 1024)
  "Maximum number of bytes to insert for any file."
  :group 'analog
  :type 'integer)

(defcustom analog-kill-buffer-confirmation-function 'yes-or-no-p
  "Function called before killing any buffers.
The function is called with one argument, which is a prompt.
Suitable non-nil values include `yes-or-no-p', `y-or-n-p' and
`ignore'."
  :group 'analog
  :type '(choice (const :tag "Kill buffers only after yes or no query" yes-or-no-p)
                 (const :tag "Kill buffers only after y or n query" y-or-n-p)
                 (const :tag "Never kill buffers" ignore)
                 (const :tag "Kill buffers without confirmation" nil)))

;; Faces

(defface analog-group-header-face
  '((((class color) (background light))(:foreground "green4"))
    (((class color) (background dark)) (:foreground "green")))
  "Face used for group headings."
  :group 'analog)

(defface analog-entry-header-face
  '((((class color) (background light)) (:foreground "blue" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t)))
  "Face used for entry headings."
  :group 'analog)

(defface analog-entry-face
  '((((class color) (background light)) (:foreground "black"))
    (((class color) (background dark)) (:foreground "white")))
  "Face used for entries."
  :group 'analog)

(defface analog-error-face
  '((((class color) (background light)) (:foreground "red"))
    (((class color) (background dark)) (:foreground "red")))
  "Face used for error messages."
  :group 'analog)

(defface analog-highlight-face
  '((((class color) (background light)) (:background "yellow" :foreground "blue"))
    (((class color) (background dark)) (:background "yellow" :foreground "blue")))
  "Face used for highlighted lines."
  :group 'analog)

(defface analog-warning-face
  '((((class color) (background light)) (:background "yellow" :foreground "red"))
    (((class color) (background dark)) (:background "yellow" :foreground "red")))
  "Face used for warning lines."
  :group 'analog)

;; Internal variables

(defvar analog-timer nil
  "Timer object controlling updates of the *analog* buffer.
Updates occur if `analog-use-timer' is t. The frequency of updates is
controlled by `analog-timer-period'.")

(defvar analog-group-list nil
  "List of `analog' groups; inferred from `analog-entry-list'.")

(defvar analog-current-group nil
  "The group of entries currently active.")

(defvar analog-entries-in-current-group nil
  "A list of entries associated with the current group.")

(defvar analog-alerts-warnings nil
  "Warnings displayed using `display-warning' for alerts.")

;; XEmacs support

(defconst analog-xemacs-p
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "True if we are using analog under XEmacs.")

;; Other version-dependent configuration

(defalias 'analog-line-beginning-position
  (cond
   ((fboundp 'line-beginning-position) 'line-beginning-position)
   ((fboundp 'point-at-bol) 'point-at-bol)))

(defalias 'analog-line-end-position
  (cond
   ((fboundp 'line-end-position) 'line-end-position)
   ((fboundp 'point-at-eol) 'point-at-eol)))

(defconst analog-face-property
  (if (with-temp-buffer
        ;; We have to rename to something without a leading space,
        ;; otherwise font-lock-mode won't get activated.
        (rename-buffer "*test-font-lock*")
        (font-lock-mode 1)
        (and (boundp 'char-property-alias-alist)
             (member 'font-lock-face
                     (assoc 'face char-property-alias-alist))))
      'font-lock-face
    'face)
  "Use font-lock-face if `add-text-properties' supports it.
Otherwise, just use face.")

(cond
 ;; Emacs 21
 ((fboundp 'replace-regexp-in-string)
  (defalias 'analog-replace-regexp-in-string 'replace-regexp-in-string))
 ;; Emacs 20
 ((and (require 'dired)
       (fboundp 'dired-replace-in-string))
  (defalias 'analog-replace-regexp-in-string 'dired-replace-in-string))
 ;; XEmacs
 ((fboundp 'replace-in-string)
  (defun analog-replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))
 ;; Bail out
 (t
  (error "No replace in string function found")))

;; Entry point

;;;###autoload
(defun analog ()
  "Start analog mode.
Also update and select the *analog* buffer."
  (interactive)
  (cond
   ((buffer-live-p (get-buffer "*analog*"))
    (set-buffer "*analog*"))
   (t
    (set-buffer (get-buffer-create "*analog*"))
    (analog-mode)))
  (analog-init)
  (analog-revert-buffer)
  (set-window-buffer (selected-window) "*analog*"))

;; Internal functions

(defun analog-get-entry-property (entry property)
  "Return for entry ENTRY the associated property PROPERTY."
  (let ((result (cdr (assoc property entry))))
    (cond
     ((eq result 'default)
      nil)
     (t
      result))))

(defun analog-get-entry-name (entry)
  "Get the name for ENTRY."
  (car entry))

(defun analog-get-entry-lines (entry)
  "Get the number of lines associated with ENTRY."
  (or (analog-get-entry-property entry 'lines)
      analog-default-no-lines))

(defun analog-get-entry-type (entry)
  "Get the type of ENTRY."
  (or (analog-get-entry-property entry 'type)
      analog-default-type))

(defun analog-get-entry-position (entry)
  "Get the position for ENTRY.
This can be head/tail for the beginning/end of the file."
  (or (analog-get-entry-property entry 'position)
      analog-default-position))

(defun analog-get-entry-actions (entry)
  "Get the list of actions for ENTRY."
  (analog-get-entry-property entry 'actions))

(defun analog-get-entry-faces (entry)
  "Get the list of faces for ENTRY."
  (analog-get-entry-property entry 'faces))

(defun analog-get-entry-alerts (entry)
  "Get the list of alerts for ENTRY."
  (analog-get-entry-property entry 'alerts))

(defun analog-init ()
  "Function used to set internal variables."
  (when (null analog-entry-list)
    (error "No Analog entries defined"))
  (let ((saved-group analog-current-group))
    ;; build the list of groups
    (analog-update-group-list)
    ;; set the current group
    (if (member saved-group analog-group-list)
        (setq analog-current-group saved-group)
      (setq analog-current-group (car analog-group-list)))
    ;; get the files in the current group
    (analog-update-entries-in-current-group)
    ;; kill existing timer; the timer frequency may have changed or
    ;; the timer may have switched on by hand
    (if analog-timer (progn (cancel-timer analog-timer)
                            (setq analog-timer nil)))
    ;; start new timer according to the default behaviour
    (if analog-use-timer
        (analog-toggle-timer))))

(defun analog-group-list ()
  "Retuns list of `analog' groups."
  (mapcar #'car analog-entry-list))

(defun analog-update-group-list ()
  "Update the list of groups in `analog-group-list'."
  (setq analog-group-list (analog-group-list)))

(defun analog-update-entries-in-current-group ()
  "Update the entries in the current group.
The current group is stored in `analog-current-group'."
  (setq analog-entries-in-current-group
        (cadr (assoc analog-current-group analog-entry-list))))

;; Commands associated with keybindings (and related functions)

(defun analog-refresh ()
  "Rebuild the internal variables/refresh the display."
  (interactive)
  (analog-init)
  (with-current-buffer (get-buffer-create "*analog*")
    (analog-revert-buffer)))

(defun analog-revert-buffer ()
  "Refresh the displayed information."
  (interactive)
  (unless (eq major-mode 'analog-mode)
    (error "Not in Analog buffer"))
  ;; Update the displayed information
  (let ((inhibit-read-only t)
        (standard-output (current-buffer))
        file-name-start file-name-end header saved-entry)
    ;; If we're on an entry line, preserve it
    (setq saved-entry (or (analog-entry-on-line t)
                          (progn (analog-move-to-previous-entry 1)
                                 (analog-entry-on-line t))))
    (erase-buffer)
    ;; Enter group header
    (setq header (format "%s%s (updated %s)" analog-group-string
                         (or analog-current-group "none")
                         (current-time-string)))
    (cond
     ((boundp 'header-line-format)
      (setq header-line-format header)
      (put-text-property 0 (length header-line-format)
                         analog-face-property 'analog-group-header-face
                         header-line-format))
     (t
      (insert header)
      (put-text-property (analog-line-beginning-position) (analog-line-end-position)
                         analog-face-property 'analog-group-header-face)
      (terpri)))
    ;; Go through list of entries
    (let (analog-alerts-warnings)
      (mapcar (lambda (entry)
                (setq type (analog-get-entry-type entry))
                ;; Insert entry header
                (insert analog-entry-string)
                (setq file-name-start (point))
                (insert (format "%s" (analog-format-entry-name entry)))
                (setq file-name-end (point))
                ;; Add text properties
                (put-text-property file-name-start (1+ file-name-start)
                                   'analog-entry-start
                                   entry)
                (put-text-property (analog-line-beginning-position)
                                   (analog-line-end-position)
                                   analog-face-property 'analog-entry-header-face)
                ;; Make clickable the associated file/command
                (put-text-property file-name-start file-name-end
                                   `mouse-face 'highlight)
                (terpri)
                ;; Insert the file or output of command
                (analog-insert-entry entry))
              (cadr (assoc analog-current-group analog-entry-list)))
      ;; Move to the saved entry, if there is one
    (if (member saved-entry analog-entries-in-current-group)
        (analog-find-entry saved-entry)
      (analog-find-entry (car analog-entries-in-current-group)))
    (set-buffer-modified-p nil)
    (when analog-alerts-warnings
      (let* ((warning-string (format "Analog Alert (%s)" (current-time-string)))
             (dashes (make-string (length warning-string) ?-))
             (warning-levels
              `((:warning ,(concat dashes "\n" warning-string "\n" dashes "\n")))))
        (display-warning nil (concat "\n" analog-alerts-warnings)
                         nil
                         "*analog alerts*")))))
  (if (and analog-verbose
           (not (eq (window-buffer) (get-buffer "*analog*"))))
      (message (format "*analog* last updated at %s"
                       (format-time-string "%H:%M:%S")))))

(defun analog-format-entry-name (entry)
  "Format the name for ENTRY.
This returns a combination of the entry name and its label
property, which can be used to disambiguate entries which have
the same file, directory or command."
  (let ((name (car entry))
        (label (cdr (assoc 'label (cdr entry)))))
    (if (> (length label) 0)
        (format "%s [%s]" name label)
      name)))

(defun analog-insert-entry (entry)
  "Insert the output from ENTRY into the current buffer."
  (let ((entry-name (analog-get-entry-name entry))
        (type (analog-get-entry-type entry))
        (entry-string)
        bytes problem)
    ;; We can run into problems if default-directory doesn't exist
    (cd (expand-file-name "~/"))
    (with-temp-buffer
      ;; Deal with entry depending on type, inserting into the
      ;; temporary buffer
      (cond
       ((eq type 'file)
        (if (file-readable-p entry-name)
            (progn
              (setq bytes (nth 7 (file-attributes entry-name)))
              (insert-file-contents entry-name nil
                                    (max 0 (- bytes analog-max-insert-bytes))
                                    bytes))
          (setq problem t
                entry-string "File does not exist")
          (put-text-property 0 (length entry-string)
                             analog-face-property 'analog-error-face entry-string)))
       ((eq type 'directory)
        (if (file-readable-p entry-name)
            (insert-directory entry-name dired-listing-switches)
          (setq problem t
                entry-string "Directory does not exist")
          (put-text-property 0 (length entry-string)
                             analog-face-property 'analog-error-face entry-string)))
       ((eq type 'shell-command)
        (let (command args)
          (setq command (car (split-string entry-name)))
          (setq args (cdr (split-string entry-name)))
          (apply 'call-process command nil t nil args)))
       ((eq type 'lisp-function)
        (condition-case err
            (funcall (read entry-name))
          (error
           (when err (setq problem t
                           entry-string "Error occurred evaluating function")
                 (put-text-property 0 (length entry-string)
                                    analog-face-property 'analog-error-face entry-string)))))
       (t
        (error "Unknown entry type: %s" (symbol-name type))))
      (unless problem
        ;; Apply entry actions
        (analog-apply-actions (analog-get-entry-actions entry))
        ;; Apply group actions
        (analog-apply-actions (cadr (assoc analog-current-group analog-group-actions)))
        ;; Apply global actions
        (analog-apply-actions (cadr (assoc 'all analog-group-actions)))
        ;; Limit the output according to how many lines are required;
        ;; take into account whether we want the head or tail of the
        ;; file
        (let ((line-prop (analog-get-entry-lines entry))
              (position (analog-get-entry-position entry))
              extreme)
          ;; Move to beginning or end of input
          (cond
           ((equal position 'head)
            (goto-char (point-min)))
           ((equal position 'tail)
            (goto-char (point-max)))
           (t
            (error "Unrecognized position property for %s" entry)))
          (setq extreme (point))
          ;; keep the required number of lines
          (cond
           ((integerp line-prop)
            (cond
             ((equal position 'head)
              (forward-line line-prop))
             ((equal position 'tail)
              (forward-line (- line-prop)))))
           ((eq line-prop 'all)
            (cond
             ((equal position 'head)
              (goto-char (point-max)))
             ((equal position 'tail)
              (goto-char (point-min)))))
           (t
            (error "Unknown line type: %s" (symbol-name line-prop))))
          ;; Narrow to region we're interested in
          (narrow-to-region (point) extreme)
          ;; Add default face
          (put-text-property (point-min) (point-max) analog-face-property 'analog-entry-face)
          ;; Apply entry faces
          (analog-apply-faces (analog-get-entry-faces entry))
          ;; Apply group faces
          (analog-apply-faces (cadr (assoc analog-current-group analog-group-faces)))
          ;; Apply global faces
          (analog-apply-faces (cadr (assoc 'all analog-group-faces)))
          ;; 
          ;; Now look for text in the current buffer that will trigger
          ;; alerts.
          ;; 
          ;; Add entry alerts
            (analog-apply-alerts (analog-get-entry-alerts entry))
            ;; Apply group alerts
            (analog-apply-alerts (cadr (assoc analog-current-group analog-group-alerts)))
            ;; Apply global alerts
            (analog-apply-alerts (cadr (assoc 'all analog-group-alerts)))
            (setq entry-string
                  (buffer-substring (point-min) (point-max))))))
    (insert entry-string)
    ;; Insert newline if needed
    (unless (bolp)
      (terpri))))

(defun analog-apply-actions (actions)
  "Apply list of ACTIONS specified for the current entry."
  (let ((message-log-max nil)
        args type
        regexp to-string)
    (mapcar (lambda (action)
              (setq type (car action)
                    args (cdr action))
              (cond
               ((or (eq type 'flush-lines)
                    (eq type 'keep-lines))
                (mapcar (lambda (r)
                          (goto-char (point-min))
                          (funcall type r))
                        args))
               ((eq type 'replace-regexp)
                (mapcar (lambda (regexp-and-string)
                          (goto-char (point-min))
                          (setq regexp (analog-eval-string (car regexp-and-string))
                                to-string (analog-eval-string (cdr regexp-and-string)))
                          (while (re-search-forward regexp nil t)
                            (replace-match to-string nil nil)))
                        args))
               ((eq type 'shell-command-on-region)
                (shell-command-on-region (point-min) (point-max) (car args) t))
               ((eq type 'lisp-function)
                (mapcar (lambda (f)
                          (funcall f))
                        args))
               ((eq type 'lisp-region-function)
                (mapcar (lambda (f)
                          (funcall f (point-min) (point-max)))
                        args))
               ((eq type 'lisp-function-map-lines)
                (mapcar (lambda (f)
                          (goto-char (point-min))
                          (while (not (eobp))
                            (goto-char (analog-line-beginning-position))
                            (save-excursion
                              (funcall f))
                            (forward-line)))
                        args))))
            actions))
  (message nil))

(defun analog-apply-faces (faces)
  "Apply list of FACES specified for the current entry."
  (let (args regexps face type)
    (mapcar (lambda (props)
              (setq type (car props)
                    face (cadr props)
                    args (cddr props))
              (cond
               ((eq type 'highlight-lines)
                (analog-highlight-lines face args))
               ((eq type 'highlight-regexp)
                (analog-highlight-regexps face args))))
            faces)))

(defun analog-apply-alerts (actions)
  "Apply list of ALERTS specified for the current entry."
  (let (regexps type)
    ;; At this point, we are in a temporary buffer with only the entry
    ;; text that will appear in the *analog* buffer.
    (mapcar (lambda (props)
              (setq type (car props)
                    regexps (cdr props))
              (cond
               ((eq type 'display-warning)
                (analog-display-warning-alerts regexps))
               ;; Other types
               ))
            actions)))

(defun analog-highlight-lines (face regexps)
  "Highlight lines matching REGEXPS in current buffer with face FACE."
  (mapcar (lambda (regexp)
            (goto-char (point-min))
            (setq regexp (analog-eval-string regexp))
            (when (stringp regexp)
              (while (re-search-forward regexp
                                        (point-max) t)
                (put-text-property (analog-line-beginning-position)
                                   (analog-line-end-position)
                                   analog-face-property
                                   face))))
          regexps))

(defun analog-highlight-regexps (face regexps)
  "Highlight strings matching REGEXPS in current buffer with face FACE."
  (mapcar (lambda (regexp)
            (goto-char (point-min))
            (setq regexp (analog-eval-string regexp))
            (when (stringp regexp)
              (while (re-search-forward regexp
                                        (point-max) t)
                (put-text-property (match-beginning 0)
                                   (match-end 0)
                                   analog-face-property
                                   face))))
          regexps))

(defun analog-display-warning-alerts (regexps)
  "Alert according to REGEXPS."
  (let (warnings)
    (mapcar (lambda (regexp)
              (goto-char (point-min))
              (while (re-search-forward regexp (point-max) t)
                ;; Cache (match-string 0)
                (setq analog-alerts-warnings
                      (concat analog-alerts-warnings
                              (buffer-substring
                               (analog-line-beginning-position)
                               (analog-line-end-position))
                              "\n"))))
            regexps)))

(defun analog-next-group (arg &optional reverse)
  "Choose the next group of entries.

With prefix ARG, choose the arg'th next group.  With non-nil
REVERSE, reverse the order of the list (see
`analog-previous-group'.  A list of groups is kept in
`analog-group-list'.  The current group is kept in
`analog-current-group'."
  (interactive "p")
  (let ((list (copy-alist analog-group-list)))
    ;; make a circular copy of the list
    (when reverse (setq list (reverse list)))
    (setcdr (last list) list)
    (setq analog-current-group
          (nth arg (member analog-current-group list))))
  (analog-update-entries-in-current-group)
  (analog-revert-buffer))

(defun analog-previous-group (arg)
  "Choose the previous group of entries.
With prefix ARG, choose the arg'th previous group."
  (interactive "p")
  (analog-next-group arg t))

(defun analog-find-entry (entry)
  "Find the entry ENTRY."
  (let ((posn
         (text-property-any (point-min)
                            (point-max)
                            'analog-entry-start
                            entry)))
    (when posn (goto-char posn))))

(defun analog-choose-entry ()
  "Goto entry chosen from a list."
  (interactive)
  (if analog-entries-in-current-group
      (let ((completion-ignore-case t)
            (completions (mapcar (lambda (elt)
                                   (cons (analog-format-entry-name elt)
                                         elt))
                                 analog-entries-in-current-group))
            choice)
        (setq choice (completing-read "Choose entry: "
                                      completions
                                      nil t))
        (when (> (length choice) 0)
          (analog-find-entry (cdr (assoc choice completions)))))
    (message "No entries in group")))

(defun analog-choose-group ()
  "Goto group chosen from a list."
  (interactive)
  (if analog-group-list
      (let ((completion-ignore-case t)
            choice)
        (setq choice (completing-read "Choose group: "
                                      (mapcar (lambda (elt)
                                                (cons elt elt))
                                              analog-group-list)
                                      nil t))
        (when (> (length choice) 0)
          (setq analog-current-group choice)
          (analog-update-entries-in-current-group)
          (analog-revert-buffer)))
    (message "No groups defined")))

(defvar analog-use-property-change
  (fboundp 'next-single-char-property-change)
  "If non-nil, use single character property change functions.")

;; Adapted from widget-move

(defun analog-move-to-next-entry (arg)
  "Move point to the ARG next `analog' entry.
ARG may be negative to move backward."
  (interactive "p")
  (unless (eq major-mode 'analog-mode)
    (error "Not in *analog* buffer"))
  (let ((posn (point)))
    (catch 'none-found
      (let ((wrapped 0)
            (number arg)
            (old (analog-entry-at)))
        ;; Forward.
        (while (> arg 0)
          (cond ((eobp)
                 (goto-char (point-min))
                 (setq wrapped (1+ wrapped)))
                (analog-use-property-change
                 (goto-char (next-single-char-property-change (point) 'analog-entry-start)))
                (t
                 (forward-char 1)))
          (and (= wrapped 2)
               (eq arg number)
               (throw 'none-found (goto-char posn)))
          (let ((new (analog-entry-at)))
            (when new
              (unless (eq new old)
                (setq arg (1- arg))
                (setq old new)))))
        ;; Backward.
        (while (< arg 0)
          (cond ((bobp)
                 (goto-char (point-max))
                 (setq wrapped (1+ wrapped)))
                (analog-use-property-change
                 (goto-char (previous-single-char-property-change (point) 'analog-entry-start)))
                (t
                 (backward-char 1)))
          (and (= wrapped 2)
               (eq arg number)
               (throw 'none-found (goto-char posn)))
          (let ((new (analog-entry-at)))
            (when new
              (unless (eq new old)
                (setq arg (1+ arg))))))
        (let ((new (analog-entry-at)))
          (while (eq (analog-entry-at) new)
            (backward-char)))
        (forward-char)))))

(defun analog-move-to-previous-entry (arg)
  "Move point to the ARG previous `analog' entry.
ARG may be negative to move forward."
  (interactive "p")
  (analog-move-to-next-entry (- arg)))

(defun analog-entry-at (&optional posn)
  "Get atomic number text property at point, or POSN if specified."
  (get-text-property (or posn (point)) 'analog-entry-start))

(defun analog-bury-buffer ()
  "Bury the *analog* buffer."
  (interactive)
  (if (eq (window-buffer) (get-buffer "*analog*"))
      (quit-window)))

(defun analog-quit ()
  "Quit analog.
Kill the *analog* buffer and destroy the timer if present."
  (interactive)
  ;; Cancel the timer
  (if analog-timer
      (analog-toggle-timer))
  (if (buffer-live-p (get-buffer "*analog*"))
      (kill-buffer "*analog*")))

(defun analog-toggle-timer ()
  "Toggle analog timer."
  (interactive)
  (if analog-timer
      (progn (cancel-timer analog-timer)
             (setq analog-timer nil)
             (message "Analog timer cancelled"))
    (setq analog-timer (run-with-timer analog-timer-period
                                       analog-timer-period
                                       'analog-timer-refresh-display-buffer))
    (message "Analog timer started")))

(defun analog-timer-refresh-display-buffer ()
  "Called by the timer to refresh the *analog* buffer.
This function will cancel any existing timer if the *analog* buffer is
dead."
  ;; We must make sure that the selected buffer is restored, otherwise
  ;; it will be left as *analog* with confusing side-effects
  (let ((buffer (current-buffer)))
    (if (buffer-live-p (get-buffer "*analog*"))
        (analog-revert-buffer)
      ;; Timer must be non-nil; cancel it
      (analog-toggle-timer))
    (set-buffer buffer)))

(defun analog-kill-other-window-buffers ()
  "Kill buffers in other windows and the windows themselves.
See `analog-kill-buffer-confirmation-function' for
customisation options."
  (interactive)
  (unless (eq major-mode 'analog-mode)
    (error "Not in Analog buffer"))
  (when (or (null analog-kill-buffer-confirmation-function)
            (apply analog-kill-buffer-confirmation-function
                   (list "Kill buffers in other windows? ")))
    (let ((buffer-list
           (delq (current-buffer)
                 (mapcar #'window-buffer (window-list)))))
      (mapc (lambda (b)
              (when (buffer-live-p b)
                (kill-buffer b)))
            buffer-list))
    (delete-other-windows))
  (message nil))

(defun analog-entry-on-line (&optional ignore-error)
  "Find the entry on the current line.
Ignore errors if IGNORE-ERROR argument given."
  (let (entry result)
    (setq result (next-single-property-change (analog-line-beginning-position)
                                              'analog-entry-start
                                              nil
                                              (analog-line-end-position)))
    (if (and result (not (eq result (analog-line-end-position))))
        (setq entry (get-text-property result 'analog-entry-start))
      (unless ignore-error
        (error "Cannot find analog entry on current line"))
      entry)))

(defun analog-show-entry ()
  "Show entry at point."
  (interactive)
  (let* ((entry (analog-entry-on-line t))
         (entry-name (analog-get-entry-name entry))
         (type (analog-get-entry-property entry 'type))
         (position (analog-get-entry-position entry)))
    (save-selected-window
      (cond
       ((null type)
        (message "Not on analog entry"))
       ((eq type 'file)
        (view-file-other-window entry-name)
        (if (equal position 'tail)
            (goto-char (point-max))))
       ((eq type 'directory)
        (dired-other-window entry-name)
        (if (equal position 'tail)
            (goto-char (point-max))))
       ((eq type 'shell-command)
        (let ((resize-mini-windows nil))
          (shell-command entry-name "*analog shell-command output*")
          (when (equal position 'tail)
            (walk-windows
             (lambda (w)
               (when (equal (buffer-name (window-buffer w))
                            "*analog shell-command output*")
                 (select-window w)
                 (goto-char (point-max))))))))
       ((eq type 'lisp-function)
        (with-output-to-temp-buffer "*analog lisp-function output*"
          (set-buffer "*analog lisp-function output*")
          (condition-case err
              (funcall (read entry-name))
            (error (insert "Error occurred evaluating function")))))))))

(defun analog-mouse-show-entry (event)
  "Find the entry associated with the entry at mouse.
Argument EVENT is a mouse event."
  (interactive "e")
  (let* ((mouse-buffer (buffer-name))
         entry entry-name type position)
    (save-selected-window
      (mouse-set-point event)
      (setq entry (analog-entry-on-line t)
            entry-name (analog-get-entry-name entry)
            type (analog-get-entry-property entry 'type)
            position (analog-get-entry-position entry))
      (cond
       ((null type)
        (message "Not on analog entry"))
       ((eq type 'file)
        (if (equal mouse-buffer "*analog*")
            (view-file-other-window entry-name)
          (view-file entry-name))
        (if (equal position 'tail)
            (goto-char (point-max))))
       ((eq type 'directory)
        (if (equal mouse-buffer "*analog*")
            (dired-other-window entry-name)
          (dired entry-name))
        (if (equal position 'tail)
            (goto-char (point-max))))
       ((eq type 'shell-command)
        (shell-command entry-name "*analog shell-command output*")
        (select-window (display-buffer "*analog shell-command output*"))
        (when (equal position 'tail)
          (goto-char (point-max))))
       ((eq type 'lisp-function)
        (with-output-to-temp-buffer "*analog lisp-function output*"
          (set-buffer "*analog lisp-function output*")
          (condition-case err
              (funcall (read entry-name))
            (error (insert "Error occurred evaluating function"))))
        (select-window (display-buffer "*analog lisp-function output*"))
        (when (equal position 'tail)
          (goto-char (point-max))))))))

(defun analog-insert-entry-details (entry)
  "Insert the details for ENTRY into current buffer."
  (let ((list (append `((entry . ,(car entry))
                        (group . ,analog-current-group))
                      (cdr entry)
                      `((group-actions . ,(cadr (assoc analog-current-group analog-group-actions)))
                        (global-actions . ,(cadr (assoc 'all analog-group-actions)))
                        (group-faces . ,(cadr (assoc analog-current-group analog-group-faces)))
                        (global-faces . ,(cadr (assoc 'all analog-group-faces)))
                        (group-alerts . ,(cadr (assoc analog-current-group analog-group-alerts)))
                        (global-alerts . ,(cadr (assoc 'all analog-group-alerts))))))
        (standard-output (current-buffer))
        format width)
    (setq width (car (sort (mapcar (lambda (elt) (length (symbol-name (car elt)))) list) #'>)))
    (setq format (concat (format "%%-%ds:" width) " %s%s"))
    (mapcar
     (lambda (elt)
       (insert (format format
                       (symbol-name (car elt))
                       (pp-to-string (cdr elt))
                       (if (and (cdr elt)
                                (listp (cdr elt)))
                           "" "\n"))))
     list)
    (goto-char (point-min))
    ;; Deal with leading whitespace
    (while (re-search-forward "^\\(\\s-+[^:]\\)" nil t)
      (replace-match (concat (make-string (+ width 2) ?\ )
                             "\\1")))))

(defun analog-show-entry-details ()
  "Display info for analog entry at point."
  (interactive)
  (let ((entry (analog-entry-on-line t)))
    (cond
     ((null entry)
      (message "Not on analog entry"))
     (t
      (set-buffer (get-buffer-create "*analog entry details*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (analog-insert-entry-details entry))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (display-buffer (get-buffer "*analog entry details*"))))))

;; Define the major mode and keymap

(defvar analog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") 'delete-other-windows)
    (define-key map (kbd "<") 'analog-previous-group)
    (define-key map (kbd ">") 'analog-next-group)
    (define-key map (kbd "?") 'describe-mode)
    (define-key map (kbd "C") 'analog-choose-group)
    (define-key map (kbd "DEL") 'scroll-down)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "b") 'analog-bury-buffer)
    (define-key map (kbd "c") 'analog-choose-entry)
    (define-key map (kbd "g") 'analog-revert-buffer)
    (when (fboundp 'window-list)
      (define-key map (kbd "k") 'analog-kill-other-window-buffers))
    (define-key map (kbd "o") 'other-window)
    (define-key map (kbd "q") 'analog-quit)
    (define-key map (kbd "r") 'analog-refresh)
    (define-key map (kbd "t") 'analog-toggle-timer)
    (define-key map (kbd "RET") 'analog-show-entry)
    (define-key map (kbd "TAB") 'analog-move-to-next-entry)
    (define-key map (kbd "M-TAB") 'analog-move-to-previous-entry)
    (define-key map [(shift tab)] 'analog-move-to-previous-entry)
    (define-key map [(shift iso-lefttab)] 'analog-move-to-previous-entry)
    (define-key map (kbd "M-RET") 'analog-show-entry-details)
    (define-key map
      (if analog-xemacs-p '(button2) (kbd "<mouse-2>"))
      'analog-mouse-show-entry)
    map)
  "Keymap for analog mode.")

;; Menus

(defvar analog-menu nil
  "Menu to use for `analog-mode'.")

(when (fboundp 'easy-menu-define)

  (easy-menu-define analog-menu analog-mode-map "Analog Menu"
    '("Analog"
      ["Next Group"         analog-next-group t]
      ["Previous Group"     analog-previous-group t]
      ["Choose Group"       analog-choose-group t]
      "---"
      ["Next Entry"         analog-move-to-next-entry t]
      ["Previous Entry"     analog-move-to-previous-entry t]
      ["Show Entry"         analog-show-entry t]
      ["Show Entry Details" analog-show-entry-details t]
      ["Choose Entry"       analog-choose-entry t]
      "---"
      ["Toggle Timer"       analog-toggle-timer t]
      ["Revert Buffer"      analog-revert-buffer t]
      "---"
      ["Bury Buffer"        analog-bury-buffer t]
      ["Quit"               analog-quit t])))

(defun analog-mode ()
  "Major mode for controlling the *analog* buffer.

Analog is a mode for listing files (and output from commands) in
a single buffer.

Entries can be gathered into logical groups, which can be cycled
using:

    \\[analog-next-group] move to next group
    \\[analog-previous-group] move to previous group
    \\[analog-choose-group] choose group to move to

To move to entries in the current group, use:

    \\[analog-move-to-next-entry] move to next entry
    \\[analog-move-to-previous-entry] move to previous entry
    \\[analog-choose-entry] choose entry to move to

With the cursor at a given entry:

    \\[analog-show-entry] show the current entry (file or command output)
    \\[analog-show-entry-details] show current entry details

Modify the entry list with:

    \\[customize-variable] RET analog-entry-list RET

To update entries, use:

    \\[analog-toggle-timer] toggle timer (for automatic updates)
    \\[analog-revert-buffer] revert buffer

To quit the *analog* buffer, use:

    \\[analog-bury-buffer] bury buffer
    \\[analog-quit] quit analog (kill buffer; cancel timer, if any)

Key definitions:
\\{analog-mode-map}"
  (kill-all-local-variables)
  (use-local-map analog-mode-map)
  (setq major-mode 'analog-mode)
  (setq mode-name "Analog")
  (setq buffer-read-only t)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  ;; XEmacs
  (when (and (fboundp 'easy-menu-add)
             analog-menu)
    (easy-menu-add analog-menu))
  (run-hooks 'analog-mode-hook))

(provide 'analog)

;;; analog.el ends here
