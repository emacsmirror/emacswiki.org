;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wcheck-mode.el (2009-08-23)
;;
;; Interface for external spell-checkers and text-filtering programs.


;; Copyright (C) 2009 Teemu Likonen <tlikonen@iki.fi>
;;
;; LICENSE
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.


;; INSTALLATION
;;
;; Put this file to some directory in your "load-path" and add the
;; following lines to your Emacs initialization file (~/.emacs):
;;
;;     (autoload 'wcheck-mode "wcheck-mode"
;;       "Toggle Wcheck mode." t)
;;     (autoload 'wcheck-change-language "wcheck-mode"
;;       "Switch Wcheck-mode languages." t)
;;
;; See customize group "wcheck" for information on how to configure
;; Wcheck mode. (M-x customize-group RET wcheck RET)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings


;;;###autoload
(defgroup wcheck nil
  "Interface for external text-filtering programs."
  :group 'applications)


;;;###autoload
(defcustom wcheck-language-data nil
  "Language configuration for `wcheck-mode'.

Elements of this alist are of the form:

  (LANGUAGE (KEY . VALUE) [(KEY . VALUE) ...])

LANGUAGE is a name string for a language and KEY and VALUE pairs
denote settings for the language. Here is a list of possible KEYs
and a description of VALUE types:

program
    VALUE is a string that is the executable program responsible
    for spell-checking LANGUAGE. This setting is mandatory.

args
     Optional command-line argument string for the program.

connection
    The value is used to set variable `process-connection-type'
    when starting the process for LANGUAGE. See the documentation
    of variable `process-connection-type' for more information.
    The default is to use a pipe for communication (nil).

face
    A symbol referring to a face which is used to mark text with
    this LANGUAGE. The default value is `wcheck-default-face'.

syntax
    VALUE is a symbol referring to an Emacs syntax table. This
    will be the effective syntax table is used with regular
    expressions. See the Info node `(elisp)Syntax Tables' for
    more information. The default value is
    `text-mode-syntax-table'.

regexp-start
regexp-body
regexp-end
    Regular expression strings which match the start of a string
    body, characters within the body and the end of the body,
    respectively.

    This is how they are used in practice: Wcheck mode looks for
    text that matches the construct `regexp-start + regexp-body +
    regexp-end'. The text that matches regexp-body is sent to an
    external program to analyze. When strings return from the
    external program they are marked in Emacs buffer using the
    following construction: `regexp-start + (regexp-quote STRING)
    + regexp-end'. The middle part is marked with face.

    Do not use grouping constructs `\\( ... \\)' in the regular
    expressions because the back reference `\\1' is used for
    separating the body string from the start and end match. You
    can use \"shy\" groups `\\(?: ... \\)' which do not record
    the matched substring.

    The default values for the regular expressions are:

        \\=\\<'*         (regexp-start)
        \\w+?         (regexp-body)
        '*\\=\\>         (regexp-end)

    Effectively they match word characters defined in the
    effective syntax table. Single quotes (') at the start and
    end of a word are excluded. This is probably a good thing
    when using Wcheck mode as a spelling checker.

regexp-discard
    The string that matched regexp-body is then matched against
    the value of this option. If this regular expression matches,
    then the word is discarded and won't be sent to the external
    program. You can use this to define exceptions to the
    previous regexp rules. The default value is

        \\`'+\\'

    which discards the body string if it consists only of single
    quotes. This was chosen as the default because the standard
    syntax table `text-mode-syntax-table' defines single quote as
    a word character. It's probably not useful to mark individual
    single quotes in a buffer when Wcheck mode is used as a
    spelling checker. If you don't want to have any discarding
    rules set this to empty string.

case-fold
    This boolean value is used to set value for variable
    `case-fold-search' for LANGUAGE. Similarly to
    `case-fold-search' the nil value means case-sensitive and a
    non-nil means case-insensitive search. The default is
    case-sensitive (nil). Note that this only has effect on
    `wcheck-mode's internal regular expression search.

An example contents of the `wcheck-language-data' variable:

    ((\"suomi\"
      (program . \"/usr/bin/enchant\")
      (args . \"-l -d fi_FI\"))
      (syntax . my-finnish-syntax-table)
     (\"British English\"
      (program . \"/usr/bin/ispell\")
      (args . \"-l -d british\")
     (\"Trailing whitespace\"
      (program . \"/bin/cat\")
      (regexp-start . \"\")
      (regexp-body . \"\\\\s-+\")
      (regexp-end . \"$\")
      (regexp-discard . \"\"))))"

  :group 'wcheck
  :type
  '(alist
    :key-type (string :tag "Language")
    :value-type
    (repeat
     :tag "Settings"
     (choice
      :format "%[Value Menu%] %v"
      (cons :tag "Program" :format "%v"
            (const :tag "Program: " :format "%t" program)
            (file :format "%v"))
      (cons :tag "Arguments" :format "%v"
            (const :tag "Arguments: " :format "%t" args)
            (string :format "%v"))
      (cons :tag "Connection type" :format "%v"
            (const :tag "Connection type: " :format "%t" connection)
            (choice :format "%[Value Menu%] %v" :value nil
                    (const :tag "pipe (nil)" nil)
                    (const :tag "pty" pty)))
      (cons :tag "Face" :format "%v"
            (const :tag "Face: " :format "%t" face)
            (symbol :format "%v" :value wcheck-default-face))
      (cons :tag "Syntax table" :format "%v"
            (const :tag "Syntax table: " :format "%t" syntax)
            (variable :format "%v" :value text-mode-syntax-table))
      (cons :tag "Regexp start" :format "%v"
            (const :tag "Regexp start: " :format "%t" regexp-start)
            (regexp :format "%v" :value "\\<'*"))
      (cons :tag "Regexp body" :format "%v"
            (const :tag "Regexp body: " :format "%t" regexp-body)
            (regexp :format "%v" :value "\\w+?"))
      (cons :tag "Regexp end" :format "%v"
            (const :tag "Regexp end: " :format "%t" regexp-end)
            (regexp :format "%v" :value "'*\\>"))
      (cons :tag "Regexp discard" :format "%v"
            (const :tag "Regexp discard: " :format "%t" regexp-discard)
            (regexp :format "%v" :value "\\`'+\\'"))
      (cons :tag "Regexp case" :format "%v"
            (const :tag "Regexp case: " :format "%t" case-fold)
            (choice :format "%[Value Menu%] %v" :value nil
                    (const :tag "sensitive" nil)
                    (const :tag "insensitive" t)))))))


(defconst wcheck-language-data-defaults
  '((args . "")
    (connection . nil)
    (face . wcheck-default-face)
    (syntax . text-mode-syntax-table)
    (regexp-start . "\\<'*")
    (regexp-body . "\\w+?")
    (regexp-end . "'*\\>")
    (regexp-discard . "\\`'+\\'")
    (case-fold . nil))
  "Default language configuration for `wcheck-mode'.
This constant is for Wcheck mode's internal use only. This
provides useful defaults for `wcheck-language-data'.")


;;;###autoload
(defcustom wcheck-language ""
  "Default language for `wcheck-mode'.
The default language used by new buffers. For buffer-local
languages use the command `\\[wcheck-change-language]'."
  :type '(string :tag "Default language")
  :group 'wcheck)
(make-variable-buffer-local 'wcheck-language)


;;;###autoload
(defface wcheck-default-face
  '((t (:underline "red")))
  "Default face for marking strings in a buffer.
This is used when language does not define a face."
  :group 'wcheck)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables


(defvar wcheck-mode nil)
(defvar wcheck-mode-map (make-sparse-keymap)
  "Keymap for `wcheck-mode'.")

(defvar wcheck-timer nil)
(defconst wcheck-timer-idle .4
  "`wcheck-mode' idle timer delay (in seconds).")
(defvar wcheck-timer-read-requested nil)
(defvar wcheck-timer-paint-requested nil)

(defvar wcheck-change-language-history nil
  "Language history for command `wcheck-change-language'.")

(defvar wcheck-received-words nil)
(make-variable-buffer-local 'wcheck-received-words)

(defvar wcheck-buffer-window-areas nil)
(make-variable-buffer-local 'wcheck-buffer-window-areas)

(defvar wcheck-buffer-data nil)

(defconst wcheck-process-name "wcheck"
  "Process name for `wcheck-mode'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive commands


;;;###autoload
(defun wcheck-change-language (language &optional global)
  "Change language for current buffer (or globally).
Change `wcheck-mode' language to LANGUAGE. The change is
buffer-local but if GLOBAL is non-nil (prefix argument if called
interactively) then change the global default language."
  (interactive
   (let* ((comp (mapcar #'car wcheck-language-data))
          (default (cond ((member wcheck-language comp)
                          wcheck-language)
                         ((car comp))
                         (t ""))))
     (list (completing-read
            (format (if current-prefix-arg
                        "Global default language (%s): "
                      "Language for the current buffer (%s): ")
                    default)
            comp nil t nil 'wcheck-change-language-history default)
           current-prefix-arg)))

  ;; Change the language, locally or globally, and update buffer
  ;; database, if needed.
  (when (stringp language)
    (if global
        (setq-default wcheck-language language)
      (setq wcheck-language language))

    ;; If the mode is currently turned on we check if language's program
    ;; is executable and if all is OK request update for the buffer.
    ;; Otherwise turn off the mode.
    (when wcheck-mode
      (let ((program (wcheck-query-language-data language 'program)))
        (if (wcheck-program-executable-p program)
            ;; It's executable; update the buffer.
            (progn
              (wcheck-update-buffer-data (current-buffer) language)
              (wcheck-timer-add-read-request (current-buffer))
              (wcheck-remove-overlays))

          ;; It's not executable; turn off.
          (wcheck-mode -1)
          (when (interactive-p)
            (wcheck-error-program-not-executable language program)))))

    (wcheck-get-data :buffer (current-buffer) :language)))


;;;###autoload
(define-minor-mode wcheck-mode
  "Interface for external spell-checkers and filtering programs.

With optional (prefix) ARG turn on the mode if ARG is positive,
otherwise turn it off. If ARG is not given toggle the mode.

Wcheck is a minor mode for automatically checking and marking
words or other text elements in Emacs buffer. Wcheck sends (parts
of) buffer's content to an external text-filtering program and,
based on its output, decides if some parts of text should be
marked.

Wcheck can be used with spell-checker programs such as Ispell,
Aspell and Enchant. Then the semantics of operation is that the
words returned from a spelling checker are spelling mistakes and
are marked as such in Emacs buffer.

The mode can also be useful with other kind of external tools.
Any tool that can receive text stream from standard input and
send text to standard output can be used. User is free to
interpret the semantics. In Wcheck configuration different
semantical units are called \"languages\".

See the documentation of variable `wcheck-language-data' for
information on how to configure Wcheck mode. Interactive command
`wcheck-change-language' is used to switch languages."

  :init-value nil
  :lighter " wck"
  :keymap wcheck-mode-map
  (if wcheck-mode
      ;; Turn on Wcheck mode, but first some checks...

      (cond
       ((minibufferp (current-buffer))
        ;; This is a minibuffer; stop here.
        (message "Can't use `wcheck-mode' in a minibuffer")
        (setq wcheck-mode nil))

       ((not (wcheck-language-exists-p wcheck-language))
        ;; Not a valid language.
        (wcheck-mode -1)
        (message "Language %sdoes not exist; check the configuration"
                 (if (and (stringp wcheck-language)
                          (> (length wcheck-language) 0))
                     (format "\"%s\" " wcheck-language)
                   "")))

       ((not (wcheck-program-executable-p
              (wcheck-query-language-data wcheck-language 'program)))
        ;; The program does not exist or is not executable.
        (wcheck-mode -1)
        (wcheck-error-program-not-executable
         wcheck-language
         (wcheck-query-language-data wcheck-language 'program)))

       (t
        ;; We are ready to really turn on the mode.

        ;; Add hooks.
        (wcheck-add-local-hooks (current-buffer))
        (wcheck-add-global-hooks)

        ;; Add this buffer to buffer database.
        (wcheck-update-buffer-data (current-buffer) wcheck-language)

        ;; Start idle timer if it's not already started. The timer runs
        ;; a function which updates buffers which have requested for
        ;; that.
        (unless wcheck-timer
          (setq wcheck-timer
                (run-with-idle-timer wcheck-timer-idle t
                                     #'wcheck-timer-read-event)))

        ;; Request update for this buffer.
        (wcheck-timer-add-read-request (current-buffer))))

    ;; Turn off the mode.

    ;; We clear overlays form the buffer, remove the buffer from buffer
    ;; database and clear the variable holding words received from
    ;; external process.
    (wcheck-remove-overlays)
    (wcheck-update-buffer-data (current-buffer) nil)

    ;; If there are no buffers using wcheck-mode anymore, stop the idle
    ;; timer and remove global hooks.
    (when (null (wcheck-get-all-data :buffer))
      (wcheck-remove-global-hooks)
      (when wcheck-timer
        (cancel-timer wcheck-timer)
        (setq wcheck-timer nil)))

    ;; Remove buffer-local hooks.
    (wcheck-remove-local-hooks (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timers


(defun wcheck-timer-add-read-request (buffer)
  (add-to-list 'wcheck-timer-read-requested buffer))
(defun wcheck-timer-remove-read-request (buffer)
  (setq wcheck-timer-read-requested
        (delq buffer wcheck-timer-read-requested)))

(defun wcheck-timer-add-paint-request (buffer)
  (add-to-list 'wcheck-timer-paint-requested buffer))
(defun wcheck-timer-remove-paint-request (buffer)
  (setq wcheck-timer-paint-requested
        (delq buffer wcheck-timer-paint-requested)))


(defun wcheck-timer-read-event ()
  "Send windows' content to external program.
This function is usually called by the wcheck-mode idle timer.
The function walks through all windows which belong to buffer
that have requested update. It reads windows' content and sends
it to an external program. Finally, this function starts another
idle timer (just once) for marking words or other text elements
in buffers."

  (dolist (buffer wcheck-timer-read-requested)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer

        ;; We are about to fulfill buffer's window-reading request so
        ;; remove this buffer from the request list.
        (wcheck-timer-remove-read-request buffer)

        ;; Reset also the list of received words and visible window
        ;; areas.
        (setq wcheck-received-words nil
              wcheck-buffer-window-areas nil)

        ;; Walk through all windows which belong to this buffer.
        (let (area-alist words)
          (walk-windows #'(lambda (window)
                            (when (eq buffer (window-buffer window))
                              ;; Store the visible buffer area.
                              (push (cons (window-start window)
                                          (window-end window t))
                                    area-alist)))
                        'nomb t)

          ;; Combine overlapping buffer areas and read words from all
          ;; areas.
          (setq wcheck-buffer-window-areas (wcheck-combine-overlapping-areas
                                            area-alist))
          (dolist (area wcheck-buffer-window-areas)
            (setq words (append (wcheck-read-words
                                 buffer (car area) (cdr area))
                                words)))
          ;; Send words to external process.
          (wcheck-send-words buffer words)))))

  ;; Start a timer which will mark text in buffers/windows.
  (run-with-idle-timer (+ wcheck-timer-idle
                          (wcheck-current-idle-time-seconds))
                       nil #'wcheck-timer-paint-event
                       ;; Repeat the timer 3 times after the initial
                       ;; call:
                       3))


(defun wcheck-receive-words (process string)
  "`wcheck-mode' process output handler function."
  (let ((buffer (wcheck-get-data :process process :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer

        ;; If process is running proceed to collect and paint the words.
        (if (eq 'run (process-status process))
            (progn (setq wcheck-received-words
                         (append (split-string string "\n+" t)
                                 wcheck-received-words))
                   (wcheck-timer-add-paint-request buffer))

          ;; It's not running. Turn off the mode.
          (wcheck-mode -1)
          (message "Process is not running for buffer \"%s\""
                   (buffer-name buffer)))))))


(defun wcheck-timer-paint-event (&optional repeat)
  "Mark text in windows.

This is normally called by the `wcheck-mode' idle timer. This
function marks (with overlays) words or other text elements in
buffers that have requested it through the variable
`wcheck-timer-paint-requested'.

If the optional argument REPEAT exists and is integer then also
call the function repeatedly that many times after the first
call. The delay between consecutive calls is defined in variable
`wcheck-timer-idle'."

  (dolist (buffer wcheck-timer-paint-requested)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (wcheck-remove-overlays)

        ;; We are about to mark text in this buffer so remove the buffer
        ;; from the request list.
        (wcheck-timer-remove-paint-request buffer)

        ;; Walk through the visible text areas and mark text based on
        ;; the word list returned by an external process.
        (when wcheck-mode
          (dolist (area wcheck-buffer-window-areas)
            (wcheck-paint-words buffer (car area) (cdr area)
                                wcheck-received-words))))))

  ;; If REPEAT is positive integer call this function again after
  ;; waiting wcheck-timer-idle. Pass REPEAT minus one as the argument.
  (when (and (integerp repeat)
             (> repeat 0))
    (run-with-idle-timer (+ wcheck-timer-idle
                            (wcheck-current-idle-time-seconds))
                         nil #'wcheck-timer-paint-event
                         (1- repeat))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hooks


(defun wcheck-add-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck-hook-kill-buffer)
                    (window-scroll-functions . wcheck-hook-window-scroll)
                    (after-change-functions . wcheck-hook-after-change)
                    (change-major-mode-hook . wcheck-hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck-hook-outline-view-change)))
      (add-hook (car hook) (cdr hook) nil t))))


(defun wcheck-remove-local-hooks (buffer)
  (with-current-buffer buffer
    (dolist (hook '((kill-buffer-hook . wcheck-hook-kill-buffer)
                    (window-scroll-functions . wcheck-hook-window-scroll)
                    (after-change-functions . wcheck-hook-after-change)
                    (change-major-mode-hook . wcheck-hook-change-major-mode)
                    (outline-view-change-hook
                     . wcheck-hook-outline-view-change)))
      (remove-hook (car hook) (cdr hook) t))))


(defun wcheck-add-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck-hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck-hook-window-configuration-change)))
    (add-hook (car hook) (cdr hook))))


(defun wcheck-remove-global-hooks ()
  (dolist (hook '((window-size-change-functions
                   . wcheck-hook-window-size-change)
                  (window-configuration-change-hook
                   . wcheck-hook-window-configuration-change)))
    (remove-hook (car hook) (cdr hook))))


(defun wcheck-hook-window-scroll (window window-start)
  "`wcheck-mode' hook for window scroll.
Request update for the buffer when its window have been
scrolled."
  (with-current-buffer (window-buffer window)
    (when wcheck-mode
      (wcheck-timer-add-read-request (current-buffer)))))


(defun wcheck-hook-window-size-change (frame)
  "`wcheck-mode' hook for window size change.
Request update for the buffer when its window's size has
changed."
  (walk-windows #'(lambda (window)
                    (with-current-buffer (window-buffer window)
                      (when wcheck-mode
                        (wcheck-timer-add-read-request
                         (current-buffer)))))
                'nomb
                frame))


(defun wcheck-hook-window-configuration-change ()
  "`wcheck-mode' hook for window configuration change.
Request update for the buffer when its window's configuration has
changed."
  (walk-windows #'(lambda (window)
                    (with-current-buffer (window-buffer window)
                      (when wcheck-mode
                        (wcheck-timer-add-read-request
                         (current-buffer)))))
                'nomb
                'currentframe))


(defun wcheck-hook-after-change (beg end len)
  "`wcheck-mode' hook for buffer content change.
Request update for the buffer when its content has been edited."
  ;; The buffer that has changed is the current buffer when this hook
  ;; function is called.
  (when wcheck-mode
    (wcheck-timer-add-read-request (current-buffer))))


(defun wcheck-hook-outline-view-change ()
  "`wcheck-mode' hook for outline view change.
Request update for the buffer when its outline view has changed."
  (when wcheck-mode
    (wcheck-timer-add-read-request (current-buffer))))


(defun wcheck-hook-kill-buffer ()
  "`wcheck-mode' hook for kill-buffer operation.
Turn off `wcheck-mode' when buffer is being killed."
  (wcheck-mode -1))


(defun wcheck-hook-change-major-mode ()
  "`wcheck-mode' hook for major mode change.
Turn off `wcheck-mode' before changing major mode."
  (wcheck-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processes


(defun wcheck-start-get-process (buffer)
  "Start or get external process for BUFFER.
Start a new process or get already existing process for BUFFER.
Return the object of that particular process or nil if the
operation was unsuccessful."
  ;; If process for this BUFFER exists return it.
  (or (wcheck-get-data :buffer buffer :process)
      ;; It doesn't exist so start a new one.
      (let* ((language (wcheck-get-data :buffer buffer :language))
             (program (wcheck-query-language-data language 'program))
             (args (split-string-and-unquote
                    (wcheck-query-language-data language 'args t)
                    "[ \t\n]+"))
             (process-connection-type
              (wcheck-query-language-data language 'connection t))
             proc)

        (when (wcheck-program-executable-p program)
          ;; Start the process.
          (setq proc (apply 'start-process wcheck-process-name nil
                            program args))
          ;; Add the process Lisp object to database.
          (wcheck-set-buffer-data buffer :process proc)
          ;; Set the output handler function.
          (set-process-filter proc #'wcheck-receive-words)
          ;; Prevent Emacs from querying user about running processes
          ;; when killing Emacs.
          (set-process-query-on-exit-flag proc nil)
          ;; Return the process object.
          proc))))


(defun wcheck-update-buffer-data (buffer language)
  "Update process and language data for BUFFER.
Calling this function is the primary way to maintain the language
and process data associated to BUFFER. If LANGUAGE is nil remove
BUFFER from the list."
  (when (and (bufferp buffer)
             (or (stringp language)
                 (not language)))

    ;; Construct a list of currently used processes.
    (let ((old-processes (wcheck-get-all-data :process))
          new-processes)

      ;; Remove dead buffers and possible minibuffers from the list.
      (dolist (item (wcheck-get-all-data :buffer))
        (when (or (not (buffer-live-p item))
                  (minibufferp item))
          (wcheck-delete-buffer-data item)))

      (if language
          (progn
            ;; LANGUAGE was given. If data for this buffer does not
            ;; exist create it.
            (unless (wcheck-get-data :buffer buffer)
              (wcheck-create-buffer-data buffer))
            ;; Add this BUFFER's language info and reset the process
            ;; info.
            (wcheck-set-buffer-data buffer :language language)
            (wcheck-set-buffer-data buffer :process nil))

        ;; LANGUAGE was not given so this normally means that
        ;; wcheck-mode is being turned off for this buffer. Remove
        ;; BUFFER from the list of buffers which request for wcheck
        ;; update and remove all buffer data.
        (wcheck-timer-remove-read-request buffer)
        (wcheck-delete-buffer-data buffer)
        (setq wcheck-received-words nil
              wcheck-buffer-window-areas nil))

      ;; Construct a list of processes that are still used.
      (setq new-processes (wcheck-get-all-data :process))
      ;; Stop those processes which are no longer needed.
      (dolist (proc old-processes)
        (unless (memq proc new-processes)
          (delete-process proc)))))

  wcheck-buffer-data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read and paint words


(defun wcheck-read-words (buffer beg end)
  "Return a list of text elements in BUFFER.
Scan BUFFER between positions BEG and END and search for text
elements according to buffer's language settings (see
`wcheck-language-data'). Return a list containing visible text
elements between BEG and END; all hidden parts are omitted."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion

        (let* ((language (wcheck-get-data :buffer buffer :language))
               (regexp (concat
                        (wcheck-query-language-data language 'regexp-start t)
                        "\\("
                        (wcheck-query-language-data language 'regexp-body t)
                        "\\)"
                        (wcheck-query-language-data language 'regexp-end t)))

               (syntax (eval (wcheck-query-language-data language 'syntax t)))
               (discard (wcheck-query-language-data language 'regexp-discard t))
               (case-fold-search
                (wcheck-query-language-data language 'case-fold t))
               (old-point 0)
               words)

          (with-syntax-table syntax
            (goto-char beg)
            (catch 'infinite
              (while (re-search-forward regexp end t)
                (cond ((= (point) old-point)
                       ;; Make sure we don't end up in an infinite loop
                       ;; when the regexp always matches with zero width
                       ;; in the current point position.
                       (throw 'infinite t))

                      ((invisible-p (match-beginning 1))
                       ;; This point is invisible. Let's jump forward to
                       ;; next change of "invisible" property.
                       (goto-char (next-single-char-property-change
                                   (match-beginning 1) 'invisible buffer
                                   end)))

                      ((or (equal discard "")
                           (not (string-match
                                 discard (match-string-no-properties 1))))
                       ;; Add the match to the word list.
                       (add-to-list 'words (match-string-no-properties 1))))
                (setq old-point (point)))))
          words)))))


(defun wcheck-send-words (buffer strings)
  "Send STRINGS for the process that handles BUFFER.
STRINGS is a list of strings to be sent as input for the external
process which handles BUFFER. Each string in STRINGS is sent as
separate line."
  (process-send-string (wcheck-start-get-process buffer)
                       (concat (mapconcat #'identity strings "\n") "\n")))


(defun wcheck-paint-words (buffer beg end wordlist)
  "Mark words of WORDLIST in BUFFER.
Mark all words (or other text elements) of WORDLIST which are
visible in BUFFER within position range from BEG to END."

  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (let* ((language (wcheck-get-data :buffer buffer :language))
               (r-start (wcheck-query-language-data language 'regexp-start t))
               (r-end (wcheck-query-language-data language 'regexp-end t))
               (syntax (eval (wcheck-query-language-data language 'syntax t)))
               (face (wcheck-query-language-data language 'face t))
               (case-fold-search
                (wcheck-query-language-data language 'case-fold t))
               regexp old-point)

          (with-syntax-table syntax
            (dolist (word wordlist)
              (setq regexp (concat r-start "\\("
                                   (regexp-quote word) "\\)"
                                   r-end)
                    old-point 0)
              (goto-char beg)

              (catch 'infinite
                (while (re-search-forward regexp end t)
                  (cond ((= (point) old-point)
                         ;; We didn't move forward so break the loop.
                         ;; Otherwise we would loop endlessly.
                         (throw 'infinite t))
                        ((invisible-p (match-beginning 1))
                         ;; The point is invisible so jump forward to
                         ;; the next change of "invisible" text property.
                         (goto-char (next-single-char-property-change
                                     (match-beginning 1) 'invisible buffer
                                     end)))
                        (t
                         ;; Make an overlay.
                         (wcheck-make-overlay
                          buffer face (match-beginning 1) (match-end 1))))
                  (setq old-point (point)))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous low-level functions


(defun wcheck-query-language-data (language key &optional default)
  "Query `wcheck-mode' language data.
Return LANGUAGE's value for KEY as defined in variable
`wcheck-language-data'. If DEFAULT is non-nil and value for KEY
does not exist return the default value for the KEY as defined in
variable `wcheck-language-data-defaults'. Also, if DEFAULT is
non-nil and value for KEY is invalid return the default value as
defined in `wcheck-language-data-defaults'."
  (let* ((key-value (assq key (cdr (assoc language wcheck-language-data))))
         (value (cdr key-value))
         (default-value
           (and default (cdr (assq key wcheck-language-data-defaults)))))
    (cond ((not key-value)
           default-value)
          ((eq key 'syntax)
           (if (syntax-table-p (and (boundp value)
                                    (eval value)))
               value default-value))
          ((eq key 'face)
           (if (facep value) value default-value))
          ((or (eq key 'program)
               (eq key 'args)
               (eq key 'regexp-start)
               (eq key 'regexp-body)
               (eq key 'regexp-end)
               (eq key 'regexp-discard))
           (if (stringp value) value default-value))
          ((or (eq key 'connection)
               (eq key 'case-fold))
           value))))


(defun wcheck-language-exists-p (language)
  "Return t if LANGUAGE exists in `wcheck-language-data'."
  (and (member language (mapcar #'car wcheck-language-data))
       (stringp language)
       (> (length language) 0)
       t))


(defun wcheck-program-executable-p (program)
  "Return t if PROGRAM is executable regular file."
  (and (stringp program)
       (file-regular-p program)
       (file-executable-p program)
       t))


(defun wcheck-error-program-not-executable (language program)
  (if (and (stringp program)
           (> (length program) 0))
      (message "Language \"%s\": program \"%s\" is not executable"
               language program)
    (message "Language \"%s\": program is not configured" language)))


(defun wcheck-current-idle-time-seconds ()
  "Return current idle time in seconds.
The returned value is a floating point number."
  (let* ((idle (or (current-idle-time)
                   '(0 0 0)))
         (high (nth 0 idle))
         (low (nth 1 idle))
         (micros (nth 2 idle)))
    (+ (* high
          (expt 2 16))
       low
       (/ micros 1000000.0))))


(defun wcheck-combine-overlapping-areas (alist)
  "Combine overlapping items in ALIST.
ALIST is a list of (A . B) items in which A and B are integers.
Each item denote a buffer position range from A to B. This
function returns a new list which has items in increasing order
according to A's and all overlapping A B ranges are combined."
  (let ((alist (sort (copy-tree alist)
                     #'(lambda (a b)
                         (< (car a) (car b)))))
        ready prev)
    (while alist
      (while (not (equal prev alist))
        (setq prev alist
              alist (append (wcheck-combine-two (car prev) (cadr prev))
                            (nthcdr 2 prev))))
      (setq ready (append ready (list (car alist)))
            alist (cdr alist)
            prev nil))
    ready))


(defun wcheck-combine-two (a b)
  (let ((a1 (car a))
        (a2 (cdr a))
        (b1 (car b))
        (b2 (cdr b)))
    (cond ((and a b)
           (if (>= a2 b1)
               (list (cons a1 (if (> b2 a2) b2 a2)))
             (list a b)))
          ((not a) (list b))
          (t (append (list a) b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlays


(defun wcheck-make-overlay (buffer face beg end)
  "Create an overlay to mark text.
Create an overlay in BUFFER from range BEG to END. Put FACE as
the overlay's \"face\" property."
  (let ((overlay (make-overlay beg end buffer)))
    (dolist (prop `((wcheck-mode . t)
                    (face . ,face)
                    (modification-hooks . (wcheck-remove-changed-overlay))
                    (insert-in-front-hooks . (wcheck-remove-changed-overlay))
                    (insert-behind-hooks . (wcheck-remove-changed-overlay))
                    (evaporate . t)))
      (overlay-put overlay (car prop) (cdr prop)))))


(defun wcheck-remove-overlays (&optional beg end)
  "Remove `wcheck-mode' overlays from current buffer.
If optional arguments BEG and END exist remove overlays from
range BEG to END. Otherwise remove all overlays."
  (remove-overlays beg end 'wcheck-mode t))


(defun wcheck-remove-changed-overlay (overlay after beg end &optional len)
  "Hook for removing overlay which is being edited."
  (unless after
    (delete-overlay overlay)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer data access functions


(defun wcheck-create-buffer-data (buffer)
  "Create data instance for BUFFER."
  (unless (wcheck-get-data :buffer buffer)
    (push (list :buffer buffer :process nil :language nil)
          wcheck-buffer-data))
  wcheck-buffer-data)


(defun wcheck-delete-buffer-data (buffer)
  "Delete all data associated to BUFFER."
  (setq wcheck-buffer-data
        (remove nil (mapcar #'(lambda (item)
                                (unless (eq buffer (plist-get item :buffer))
                                  item))
                            wcheck-buffer-data))))


(defun wcheck-get-data (key value &optional target-key)
  "Query the first matching KEY VALUE pair and return TARGET-KEY.
If optional TARGET-KEY is not given return all data associated
with the matching KEY VALUE."
  (catch :answer
    (dolist (item wcheck-buffer-data)
      (when (equal value (plist-get item key))
        (throw :answer (if target-key (plist-get item target-key) item))))))


(defun wcheck-get-all-data (key)
  "Return every buffer's value for KEY."
  (remove nil (mapcar #'(lambda (item)
                          (plist-get item key))
                      wcheck-buffer-data)))


(defun wcheck-set-buffer-data (buffer key value)
  "Set KEY's VALUE for BUFFER."
  (let ((item (wcheck-get-data :buffer buffer)))
    (when item
      (wcheck-delete-buffer-data buffer)
      (setq item (plist-put item key value))
      (push item wcheck-buffer-data))))


(provide 'wcheck-mode)
