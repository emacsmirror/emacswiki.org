;;; ecasound.el --- Interactive and programmatic interface to Ecasound

;; Copyright (C) 2001, 2002  Mario Lang 

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: audio, ecasound, eci, comint, process, pcomplete
;; Version: 0.7.3

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file implements several aspects of ecasound use:
;;
;; * A derived-major-mode, from comint mode for an inferior ecasound
;; process (ecasound-aim-mode).  Complete with context sensitive
;; completion and interactive features to control the current process
;; using ECI.
;;
;; * Ecasound Control Interface (ECI) library for programmatic control
;; of a Ecasound process.  This allows you to write Ecasound batch
;; jobs in Emacs-Lisp with Lisp functions and return values.  Have a
;; look at eci-example and ecasound-normalize.
;;
;; * ecasound-ewf-mode, a mode for editing .ewf files.
;;
;;
;; Usage:
;;
;; You need a very recent version of ecasound for this file to work correclty.
;; Something >=2.1dev12, this currently means you need to compile it
;; from CVS.
;;
;; Put ecasound.el in your load-path and require it in your .emacs.
;; Set `ecasound-program' to the path to your ecasound executable.
;;
;;  (setq load-path (cons "/home/user/elisp")
;;  (require 'ecasound)
;;  (setq ecasound-program "/home/user/bin/ecasound"
;;        eci-program "/home/user/bin/ecasound")
;;
;; Use M-x ecasound RET to invoke an inferior ecasound process.
;;
;; For programmatic use of the ECI API, have a look at `eci-init',
;; `eci-command' and in general the eci-* namespace.
;;
;; Bugs:
;;
;; The function ecasound-copp-increase is used to allow the use of +/-
;; on the copp-select button to change the value of the number.
;; Strangely, if this function is used, the number field changes,
;; it is no longer a field, i.e., it can no longer
;; be edited.   Any ideas?
;;
;; Todo:
;;
;; * Convince Kai to make NetECI look and behave exactly like ecasound -c.
;; This would make it very easy to make neteci connections using the current
;; code.  Just set eci-program to (host . port).
;; * Cache things like c-, cs- and cop-list as well as c-, cs- and
;; engine-status in buffer-local variables like it's already done for
;; cop- and ladspa-register.
;; * Tweak the mode-line to include status informations like engine-status
;; or inputs/outputs/chains or the current selected cs or chains.
;; * Add NetECI support to fetch status info (eci-hide-output t calls)
;; via a separate, not visible buffer.
;; * Write eci-cop-register alike support for preset-register.
;; * Copy documentation for ECI commands into eci-* docstrings and menu
;; :help keywords.
;; * Expand the menu.
;; * Bind most important interactive functions in ecasound-aim-mode-map
;; (which layout to use?)
;; * Collapse all the duplicate code into a macro and an alist which
;; defines the available eci commands.  (This seems rather complicated
;; and is probably not worth the effort.)

;;; History:
;; 
;; Version: 0.7.3
;;
;; * Fixed missing require.
;;
;; Version: 0.7.2
;;
;; * Integrated ladspa-register into ecasound-cop-add
;; Now we've a very huge list to select from using completion.
;; * Some little cleanups.
;; * Fixed ecasound-cop-add to actually add the ':' between name and args.
;; * Removed the slider widget for now from the :format property of
;; ecasound-copp.
;; * Added `ecasound-messages' for a nice customisable interface to
;; loglevels, strangely, cvs version doesnt seem to recognize
;; -d:%d
;;
;; Version: 0.7.1
;;
;; * Created a slider widget.  It's not flawless, but it works!
;;
;; Version: 0.7
;;
;; * Rewrote ecasound-cop-edit completely.
;; Now we are using our own widget types and all that
;; funky stuff.  Added cop-select and copp-select buttons.
;; Added +/- stepping functionality, but not as expected (see Bugs).
;;
;; Version 0.65:
;;
;; * Wrote `eci-process-cop-status' and `eci-cop-status' on top of that.
;; * First Wizard!  `ecasound-cop-edit', uses `eci-cop-status' and the
;; ECI library to offer you a complete cop-editor.  Values are
;; editable-fields, and hitting RET in such a field does cop*-select and
;; copp-set for you, try that!
;;
;; Version 0.62:
;;
;; * Fixed `eci-error-p' to work correctly.
;; * Rewrote `eci-parse' to be less regexp dependent and more
;; spec compliant.
;; * Fixed `eci-command' behaviour in the error case.
;; Now, all results which are a non-error are non-nil, an error
;; produces nil as result.  You can also use `eci-error-p' to check if
;; last command produced an error or `eci-return-value' to get the
;; error. Example:
;;  (unless (eci-command "cs-connect")
;;    (error eci-return-value))
;;
;; Version 0.61:
;;
;; * Added Usage: info.
;;
;; Version 0.6:
;;
;; * Fix the problem when using eci-* commands while typing a command
;; in the buffer (input area).  Now already typed text is preserved.
;;
;; * pcomplete/ecasound-aim-mode/cop-add for completing cop-add in
;; the buffer directly.  Only glitch is that it doesn't add ':' and ','
;; at the right place by default.  Works for -el: completion too now, can
;; complete all ladspa plugin arguments (it gives help for them).
;;
;; * Completely integrated comint and ECI handling.  The main work
;; is now done in the comint-output-filter-function `eci-output-filter'
;; which sets `eci-return-value', `eci-return-type' and `eci-result'
;; if a return value was parsed correctly.  Also calls `eci-message-hook'
;; and `eci-error-hook'.
;; This now allows use to be very flexible, i.e., user types a command
;; manually on the ecasound prompt.  After he sent it you can use
;; `eci-last-command' to test which command it was, and above variables to
;; see what the result was.
;; `eci-output-filter' now also does automatic handling of cop-register
;; and ladspa-register result values (i.e., sets the buffer-local
;; variables).
;;
;; * `eci-last-command' is used to track the last issued command via
;; `comint-input-filter-functions' (`eci-input-filter').
;; This allows us to catch useful results like those of *-registerr
;; command even if the user issued them manually.
;;
;; * `eci-hide-output': If t, `eci-command' will delete the generated
;; output so that it is not visible in the buffer.
;; This is useful for fetching *-register info while completing without
;; producing unwanted output...
;; Only bug there is that everytime we do this, the window redisplays
;; such that the prompt+point is in the first line (strange).
;;
;; Version 0.5:
;;
;; * ecasound.el, ecasound-ewf.el and eci.el were merged into a single
;; file, ecasound.el.  Code was integrated (ecasound-aim-mode uses eci-*
;; functions to offer interactive features).
;; 
;; * To work correctly, an ecasound version implementing the
;; int-output-mode-wellformed command is expected.  Older versions are
;; only supported in a limited way.  So, you probably want to do
;;   (setq ecasound-program "/path/to/cvs/built/version")
;; first.
;;
;; * ecasound-cop-add: An interactive version of eci-cop-add, with a
;; completion based menu and separate prompts for arguments.  This
;; uses eci-cop-register to get the information and is a first attempt
;; to show how much ecasound.el can actually make ecasound more fun!

;;; Code:

(require 'comint)
(require 'easymenu)
(require 'pcomplete)
(require 'widget)
(require 'wid-edit)

(defgroup ecasound nil
  "Ecasound is a multitrack audio recorder.

Variables in this group affect inferior ecasound processes started from
within Emacs using the command `ecasound'.

See the subgroup `eci' for settings which affect the programmatic interface
to ECI."
  :prefix "ecasound-"
  :group 'processes)

(defcustom ecasound-arguments '("-c")
  "*Default command line arguments when starting a ecasound process."
  :group 'ecasound
  :type '(repeat string))

(defcustom ecasound-program "/home/mlang/bin/ecasound"
  "*Ecasound's executable.
This program is executed when the user invokes \\[ecasound]."
  :group 'ecasound
  :type 'file)

(defcustom ecasound-messages '(1 2 256)
  "Defines the type of logmessages ecasound should report.
This variable is used at initialisation time in `ecasound' except
the parameter -d is given in `ecasound-arguments'."
  :group 'ecasound
  :type '(set (const :tag "Errors" 1)
              (const :tag "Info" 2)
              (const :tag "Subsystems" 4)
              (const :tag "Module names" 8)
              (const :tag "User objects" 16)
              (const :tag "System objects" 32)
              (const :tag "Functions" 64)
              (const :tag "Continuous" 128)
              (const :tag "EIAM return values" 256)))

(defcustom ecasound-prompt-regexp "^ecasound[^>]*> "
  "Regexp to use to match the prompt."
  :group 'ecasound
  :type 'regexp)

(defconst ecasound-iam-commands
  '( ;; GENERAL
    "quit" "q"
    "start" "t"
    "stop" "s"
    "run"
    "debug"
    "help" "h"
    ;; GLOBAL
    "status" "st"
    "engine-status"
    ;; CHAINSETUPS
    "cs-add"
    "cs-remove"
    "cs-list"
    "cs-select"
    "cs-selected"
    "cs-index-select" "cs-iselect"
    "cs-load"
    "cs-save"
    "cs-save-as"
    "cs-edit"
    "cs-is-valid"
    "cs-connect"
    "cs-disconnect"
    "cs-connected"
    "cs-rewind" "rewind" "rw"
    "cs-forward" "forward" "fw"
    "cs-set-position" "cs-setpos" "setpos" "set-position"
    "cs-get-position" "cs-getpos" "getpos" "get-position"
    "cs-get-length" "get-length"
    "cs-set-length"
    "cs-toggle-loop"
    "cs-set-param"
    "cs-set-audio-format"
    "cs-status" "cs"
    ;; CHAINS
    "c-add"
    "c-remove"
    "c-list"
    "c-select"
    "c-index-select" "c-iselect"
    "c-select-all"
    "c-select-add"
    "c-deselect"
    "c-selected"
    "c-clear"
    "c-rename"
    "c-muting"
    "c-bypass"
    "c-forward" "c-fw"
    "c-rewind" "c-rw"
    "c-set-position" "c-setpos"
    "c-status"
    ;; AUDIO INPUT/OUTPUT OBJECTS
    "ai-add"
    "ao-add"
    "ai-select"
    "ao-select"
    "ai-index-select" "ai-iselect"
    "ao-index-select" "ao-iselect"
    "ai-selected"
    "ao-selected"
    "ai-attach"
    "ao-attach"
    "ai-remove"
    "ao-remove"
    "ai-forward" "ai-fw"
    "ao-forward" "ao-fw"
    "ai-rewind" "ai-rw"
    "ao-rewind" "ao-rw"
    "ai-set-position" "ai-setpos"
    "ao-set-position" "ao-setpos"
    "ai-get-position" "ai-getpos"
    "ao-get-position" "ao-getpos"
    "ai-get-length"
    "ao-get-length"
    "ai-get-format"
    "ao-get-format"
    "ai-wave-edit"
    "ao-wave-edit"
    "ai-list"
    "ao-list"
    "aio-register"
    "aio-status"
    ;; CHAIN OPERATORS
    "cop-add"
    "cop.list"
    "cop-remove"
    "cop-select"
    "cop-index-select" "cop-iselect"
    "cop-selected"
    "cop-set"
    "cop-status"
    "copp-list"
    "copp-select"
    "copp-index-select" "copp-iselect"
    "copp-selected"
    "copp-set"
    "copp-get"
    "cop-register"
    "preset-register"
    "ladspa-register"
    ;; CONTROLLERS
    "ctrl-add"
    "ctrl-remove"
    "ctrl-list"
    "ctrl-select"
    "ctrl-index-select" "ctrl-iselect"
    "ctrl-selected"
    "ctrl-status"
    "ctrl-register"
    ;; OBJECT MAPS
                                        ; Unimplemented currently...
    )
  "Available Ecasound IAM commands.")
  
(defvar ecasound-iam-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\t" 'pcomplete)
    (define-key map (kbd "M-\"") 'eci-command)
                                        ;    (define-key map "\M-?"
                                        ;      'comint-dynamic-list-filename-completions)
                                        ;    (define-key map [menu-bar completion]
                                        ;      (copy-keymap (lookup-key comint-mode-map [menu-bar completion])))
    map))

(easy-menu-define
  ecasound-iam-cs-menu ecasound-iam-mode-map
  "Chainsetup menu."
  (list "Chainsetup"
        ["Add..." eci-cs-add t]
        ["Load..." eci-cs-load t]
        ["Save" eci-cs-save t]
        ["Save As..." eci-cs-save-as t]
        ["List" eci-cs-list t]
        ["Select" eci-cs-select t]
        ["Select via index" eci-cs-index-select t]
        "-"
        ["Selected" eci-cs-selected t]
        ["Valid?" eci-cs-is-valid t]
        ["Connect" eci-cs-connect t]
        ["Disconnect" eci-cs-disconnect t]
        ["Get position" eci-cs-get-position t]
        ["Get length" eci-cs-get-length t]
        ["Get length in samples" eci-cs-get-length-samples t]))
(easy-menu-add ecasound-iam-cs-menu ecasound-iam-mode-map)
(easy-menu-define
  ecasound-iam-c-menu ecasound-iam-mode-map
  "Chain menu."
  (list "Chain"
        ["Add..." eci-c-add t]
        ["Select..." eci-c-select t]
        ["Select All" eci-c-select-all t]
        ["Deselect..." eci-c-deselect t]
        ["Selected" eci-c-selected t]
        ))
(easy-menu-add ecasound-iam-c-menu ecasound-iam-mode-map)
(easy-menu-define
  ecasound-iam-cop-menu ecasound-iam-mode-map
  "Chain Operator menu."
  (list "ChainOp"
        ["Add..." ecasound-cop-add t]
        ["Select..." eci-cop-select t]
        ["Edit..." ecasound-cop-edit t]
        "-"
        ["Select parameter..." eci-copp-select t]
        ["Get parameter value" eci-copp-get t]
        ["Set parameter value..." eci-copp-set t]
        ))
(easy-menu-add ecasound-iam-c-menu ecasound-iam-mode-map)

(define-derived-mode ecasound-iam-mode comint-mode "Ecasound-IAM"
  "Special mode for ecasound processes in interactive mode."
  (set (make-local-variable 'comint-prompt-regexp)
       (set (make-local-variable 'paragraph-start)
            ecasound-prompt-regexp))
  (add-hook 'comint-output-filter-functions 'eci-output-filter nil t)
  (add-hook 'comint-input-filter-functions 'eci-input-filter nil t)
  (ecasound-iam-setup-pcomplete))

;;;###autoload
(defun ecasound (&optional buffer)
  "Run an inferior ecasound, with I/O through BUFFER.
BUFFER defaults to `*ecasound*'.
Interactively, a prefix arg means to prompt for BUFFER.
If BUFFER exists but ecasound process is not running, make new ecasound
process using `ecasound-arguments'.
If BUFFER exists and ecasound process is running, just switch to BUFFER.
The buffer is put in ecasound mode, giving commands for sending input and
completing IAM commands.  See `ecasound-iam-mode'.

\(Type \\[describe-mode] in the ecasound buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Ecasound buffer: " "*ecasound*"))))
  (when (null buffer)
    (setq buffer "*ecasound*"))
  (if (not (comint-check-proc buffer))
      (pop-to-buffer
       (save-excursion
         (set-buffer
          (apply 'make-comint-in-buffer
                 "ecasound" buffer
                 ecasound-program
                 nil
                 (if (member* "^-d" ecasound-arguments :test #'string-match)
                     ecasound-arguments
                   (cons (format "-d:%d" (apply #'+ ecasound-messages))
                         ecasound-arguments))))
         (ecasound-iam-mode)
         (while (accept-process-output
                 (get-buffer-process (current-buffer))
                 1))
         (if (not (eq (eci-command "int-output-mode-wellformed") t))
             (message "Failed to initialize properly"))
         (current-buffer)))
    (pop-to-buffer buffer)))

(defun ecasound-delete-last-in-and-output ()
  "Delete the region of text generated by the last in and output.
This is usually used to hide ECI requests from the user."
  (delete-region
   (save-excursion (goto-char comint-last-input-end) (forward-line -1)
                   (unless (looking-at ecasound-prompt-regexp)
                     (error "Assumed ecasound-prompt"))
                   (point))
   comint-last-output-start))

(defun eci-input-filter (string)
  "Tracks commands sent to ecasound.
Argument STRING is the input sent."
  (if (string-match "^[[:space:]]*\\([a-zA-Z-]+\\)[\n\t ]+" string)
      (setq eci-last-command (match-string-no-properties 1 string))))

(defun eci-output-filter (string)
  "Parse and use ECI messages.
This function should be used on `comint-output-filter-functions' hook.
Argument STRING is the string originally received and inserted into the buffer."
  (let ((start comint-last-input-end)
        (end (process-mark (get-buffer-process (current-buffer)))))
    (goto-char start)
    (if (re-search-forward ecasound-prompt-regexp end t)
      (let ((result (eci-parse start (progn (forward-char -1)
                                            (beginning-of-line)
                                            (point)))))
        (when result
          (setq eci-result
                (cond
                 ((string= eci-last-command "cop-register")
                  (eci-process-cop-register result))
                 ((string= eci-last-command "ladspa-register")
                  (eci-process-ladspa-register result))
                 ((string= eci-last-command "int-output-mode-wellformed")
                  (if (eq result t)
                      (setq eci-int-output-mode-wellformed-flag t)))
                 (t
                  (if (and (listp result)
                           (eq (nth 0 result) 'error))
                      nil
                    result)))))))))

;;; Ecasound-iam-mode pcomplete functions

(defun ecasound-iam-setup-pcomplete ()
  "Setup buffer-local functions for pcomplete in `ecasound-iam-mode'."
  (set (make-local-variable 'pcomplete-command-completion-function)
       (lambda ()
         (pcomplete-here ecasound-iam-commands)))
  (set (make-local-variable 'pcomplete-command-name-function)
       (lambda ()
         (pcomplete-arg 'first)))
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ecasound-iam-pcomplete-parse-arguments))

(defun ecasound-iam-pcomplete-parse-arguments ()
  "Parse arguments in the current region.
\" :,\" are considered for splitting."
  (let ((begin (save-excursion (comint-bol nil) (point)))
        (end (point))
        begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (skip-chars-forward " \t\n,:")
        (setq begins (cons (point) begins))
        (let ((skip t))
          (while skip
            (skip-chars-forward "^ \t\n,:")
            (if (eq (char-before) ?\\)
                (skip-chars-forward " \t\n,:")
              (setq skip nil))))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) (point))
                         args)))
      (cons (reverse args) (reverse begins)))))

(defun ecasound-input-file-or-device ()
  "Return a list of possible completions for input device name."
  (append (delq
           nil
           (mapcar
            (lambda (elt)
              (when (string-match
                     (concat "^" (regexp-quote pcomplete-stub)) elt)
                elt))
            (list "alsa" "alsahw" "alsalb" "alsaplugin"
                  "arts" "loop" "null" "stdin")))
          (pcomplete-entries)))

;;;; IAM commands

(defun pcomplete/ecasound-iam-mode/cs-add ()
  (message "Adds a new chainsetup with name `name`.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-remove ()
  (message "Removes currently selected chainsetup.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-list ()
  (message "Returns a list of all chainsetups.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-select ()
  (message "Selects chainsetup `name`.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-selected ()
  (message "Returns the name of currently selected chainsetup.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-load ()
  (pcomplete-here (pcomplete-entries)))

(defun pcomplete/ecasound-iam-mode/cs-save-as ()
  (pcomplete-here (pcomplete-entries)))

(defun pcomplete/ecasound-iam-mode/cs-is-valid ()
  (message "Whether currently selected chainsetup is valid (=can be connected)?")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-connect ()
  (message "Connect currently selected chainsetup to engine.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-disconnect ()
  (message "Disconnect currently connected chainsetup.")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-iam-mode/cs-connected ()
  (message "Returns the name of currently connected chainsetup.")
  (throw 'pcompleted t))

(defun eci-register-find-arg (arg register)
  (let (result)
    (while register
      (if (string= (nth 1 (car register)) arg)
          (setq result (nth 2 (car register))
                register nil))
      (setq register (cdr register)))
    result))

(defun pcomplete/ecasound-iam-mode/cs-rewind ()
  (message "Rewinds the current chainsetup position by `time-in-seconds` seconds.")
  (throw 'pcompleted t))
(defalias 'pcomplete/ecasound-iam-mode/rewind 'pcomplete/ecasound-iam-mode/cs-rewind)
(defalias 'pcomplete/ecasound-iam-mode/rw 'pcomplete/ecasound-iam-mode/cs-rewind)

(defun pcomplete/ecasound-iam-mode/ai-add ()
  (pcomplete-here (ecasound-input-file-or-device)))

(defun pcomplete/ecasound-iam-mode/ao-add ()
  (pcomplete-here (ecasound-input-file-or-device)))

(defun pcomplete/ecasound-iam-mode/cop-add ()
  (unless eci-cop-register
    (eci-cop-register))
  (unless eci-ladspa-register
    (eci-ladspa-register))
  (cond
   ((= pcomplete-last 1)
    (pcomplete-here
     (cons
      "-el:"
      (mapcar
       (lambda (elt)
         (concat (nth 1 elt) ":"))
      eci-cop-register))))
   ((> pcomplete-last 1)
    (if (and (= pcomplete-last 2)
             (string= (pcomplete-arg) "-el"))
            (progn (pcomplete-next-arg)
                   (pcomplete-here
                    (sort (mapcar (lambda (elt) (substring (nth 1 elt) 4))
                                  eci-ladspa-register)
                          #'string-lessp)))
      (if (and (string= (pcomplete-arg) "-el")
               (> pcomplete-last 2))
              (let* ((args (eci-register-find-arg (pcomplete-arg -1)
                                                  eci-ladspa-register))
                     (arg (nth (- pcomplete-last 3) args)))
                (if arg
                    (message "%s" arg)
                  (message "No help available")))
        (let* ((args (eci-register-find-arg (pcomplete-arg)
                                            eci-cop-register))
               (arg (nth (- pcomplete-last 2) args)))
          (if arg
              (message "%s" arg)
            (message "No help available")))))))
  (throw 'pcompleted t))

.
;;; ECI --- The Ecasound Control Interface

(defgroup eci nil
  "Ecasound Control Interface."
  :group 'ecasound)

(defcustom eci-program "ecasound"
  "*Program to invoke when doing `eci-init'."
  :group 'eci
  :type '(choice string (cons string string)))

(defcustom eci-arguments '("-c" "-D")
  "*Arguments used by `eci-init'."
  :group 'eci
  :type '(repeat string))

(defcustom eci-buffer-name "*eci-ecasound*"
  "Buffer name to use for ecasound process buffers."
  :group 'eci
  :type 'string)

(defcustom eci-parse-cleanup-buffer t
  "*Indicates if `eci-parse' should cleanup the buffer.
This means the loglevel, msgsize and return type will get removed if
parsed successfully."
  :group 'eci
  :type 'boolean)

(defcustom eci-error-hook nil
  "*Called whenever a ECI error happens."
  :group 'eci
  :type 'hook)

(defcustom eci-message-hook '(eci-print-message)
  "*Hook called whenever a message except loglevel 256 (eci) is received.
Arguments are LOGLEVEL and STRING."
  :group 'eci
  :type 'hook)

(defface eci-error-face '((t (:foreground "White" :background "Red")))
  "Face used to highlight errors."
  :group 'eci)

(defvar eci-hide-output nil
  "If non-nil, `eci-command' will remove the output generated.")

(defvar eci-last-command nil
  "Last command sent to the ecasound process.")
(make-variable-buffer-local 'eci-last-command)

(defvar eci-return-type nil
  "The return type of the last received return value as a string.")
(make-variable-buffer-local 'eci-return-type)

(defvar eci-return-value nil
  "The last received return value as a string.")
(make-variable-buffer-local 'eci-return-value)

(defvar eci-result nil
  "The last received return value as a Lisp Object.")
(make-variable-buffer-local 'eci-result)

(defvar eci-int-output-mode-wellformed-flag nil
  "Indicates if int-output-mode-wellformed was successfully initialized.")
(make-variable-buffer-local 'eci-int-output-mode-wellformed-flag)

(defvar eci-cop-register nil
  "If non-nil, contains the list of registered chainops.
It has the form
 ((NAME PREFIX (ARGNAME ...)) ...)

Use `eci-cop-register' to fill this list with data.")
(make-variable-buffer-local 'eci-cop-register)

(defvar eci-ladspa-register nil
  "If non-nil, contains the list of registered ladspa plugins.
It has the form
 ((FULL-NAME NAME (ARGNAME ...)) ...)

Use `eci-ladspa-register' to fill this list with data.")
(make-variable-buffer-local 'eci-cop-register)

(defun eci-init ()
  "Initialize a programmatic ECI session.
Every call to this function results in a new sub-process being created
according to `eci-program' and `eci-arguments'.  Returns the newly
created buffer.
The caller is responsible for terminating the subprocess at some point."
  (save-excursion
    (set-buffer (generate-new-buffer eci-buffer-name))
    (apply 'make-comint-in-buffer
           "eci" (current-buffer)
           eci-program
           nil
           eci-arguments)
    (ecasound-iam-mode)
    (while (accept-process-output (get-buffer-process (current-buffer)) 1))
    (if (string-match "^256 0 -" (eci-command "int-output-mode-wellformed"))
        (setq eci-int-output-mode-wellformed-flag t))
    (current-buffer)))

(defun eci-interactive-startup ()
  "Used to interactively startup a ECI session using `eci-init'.
This will mostly be used for testing sessions and is equivalent
to `ecasound'."
  (interactive)
  (switch-to-buffer (eci-init)))

(defun eci-command (command &optional buffer-or-process)
  "Send a ECI command to a ECI host process.
COMMAND is the string to be sent, without a newline character.
If BUFFER-OR-PROCESS is nil, first look for a ecasound process in the current
buffer, then for a ecasound buffer with the name *ecasound*,
otherwise use the buffer or process supplied.
Return the string we received in reply to the command except
`eci-int-output-mode-wellformed-flag' is set, which means we can parse the
output via `eci-parse' and return a meaningful value."
  (interactive "sECI Command: ")
  (let ((proc (cond ((processp buffer-or-process) buffer-or-process)
                    ((bufferp buffer-or-process) (get-buffer-process buffer-or-process))
                    ((and (eq major-mode 'ecasound-iam-mode)
                          (comint-check-proc (current-buffer)))
                     (get-buffer-process (current-buffer)))
                    ((comint-check-proc "*ecasound*")
                     (get-buffer-process (get-buffer "*ecasound*")))
                    (t (error
                        "Could not determine suitable ecasound process")))))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (point-max))))
        (goto-char (process-mark proc))
        (insert (format "%s" command))
        (let (comint-eol-on-send)
          (comint-send-input))
        (let ((here (point)) result)
          (when (accept-process-output proc 2)
            (goto-char comint-last-input-end)
            (while (not (re-search-forward ecasound-prompt-regexp nil t))
              (accept-process-output proc 0 50))
            (setq result
                  (if eci-int-output-mode-wellformed-flag
                      eci-result
                    ;; Backward compatibility.  Just return the string
                    (buffer-substring-no-properties here (save-excursion
                                        ; Strange hack to avoid fields
                                                           (forward-char -1)
                                                           (beginning-of-line)
                                                           (if (not (= here (point)))
                                                               (forward-char -1))
                                                           (point)))))
            (if moving (goto-char (point-max)))
            (when (and eci-hide-output result)
              (ecasound-delete-last-in-and-output))
            result))))))

(defun eci-print-message (level msg)
  "Simple interactive function which prints every message regardless
which logleve."
  (message "%d: %s" level msg))

(defun eci-message (loglevel msg)
  (run-hook-with-args 'eci-message-hook loglevel msg))

(defun eci-parse (start end)
  "Parse output of ECI command in int-output-mode-wellformed mode.
START and END is the region of output received excluding the prompt.
Return an appropriate lisp value if possible."
  (save-excursion
    (let (type value (end (copy-marker end)))
      (goto-char start)
      (while (re-search-forward
              "^\\([0-9]\\{1,3\\}\\) \\([0-9]+\\)\\( \\(.*\\)\\)?\n"
              end t)
        (let* ((loglevel (string-to-number (match-string-no-properties 1)))
              (msgsize (string-to-number (match-string-no-properties 2)))
              (return-type (match-string-no-properties 4))
              (msg (buffer-substring-no-properties
                    (point)
                    (progn
                      (forward-char msgsize)
                      (if (not (save-match-data
                                 (looking-at "\n\n")))
                          (error "Malformed ECI message")
                        (point))))))
          (if (and (= loglevel 256)
                   (string= return-type "e"))
              (add-text-properties
               (match-end 0) (point)
               (list 'face 'eci-error-face)))
          (when eci-parse-cleanup-buffer
            (delete-region (match-beginning 0) (if (= msgsize 0)
                                                   (point)
                                                 (match-end 0)))
            (delete-char 1))
          (if (not (= loglevel 256))
              (eci-message loglevel msg)
            (setq value msg
                  type return-type))))
;      (unless (= (point) end)
;       (error "Parser out of sync"))
      (when type
        (setq eci-return-value value eci-return-type type)
        (cond
         ((string= type "e")
          (run-hook-with-args 'eci-error-hook value)
          (list 'error value))
         ((string= type "f")
          (string-to-number value))
         ((or (string= type "i")
              (string= type "li"))
          (string-to-number value))
         ((string= type "s")
          value)
         ((string= type "S")
          (split-string value ","))
         ((string= type "-")
          t)
         (t (error "Unimplemented return type %s" type)))))))

(defsubst eci-error-p ()
  "Predicate which can be used to check if the last command produced an error."
  (string= eci-return-type "e"))

;;; ECI commands implemented as lisp functions

(defun eci-run (&optional buffer-or-process)
  (interactive)
  (eci-command "run"))

(defun eci-start (&optional buffer-or-process)
  (interactive)
  (eci-command "start"))

(defun eci-cs-add (chainsetup &optional buffer-or-process)
  (interactive "sChainsetup to add: ")
  (eci-command (format "cs-add %s" chainsetup) buffer-or-process))

(defun eci-cs-connect (&optional buffer-or-process)
  (interactive)
  (eci-command "cs-connect" buffer-or-process))

(defun eci-cs-disconnect (&optional buffer-or-process)
  (interactive)
  (eci-command "cs-disconnect" buffer-or-process))

(defun eci-cs-get-length (&optional buffer-or-process)
  (eci-command "cs-get-length" buffer-or-process))
(defalias 'eci-get-length 'eci-cs-get-length)

(defun eci-cs-get-length-samples (&optional buffer-or-process)
  (interactive)
  (eci-command "cs-get-length-samples" buffer-or-process))
(defalias 'eci-get-length-samples 'eci-cs-get-length-samples)

(defun eci-cs-get-position (&optional buffer-or-process)
  (interactive)
  (eci-command "cs-get-position" buffer-or-process))
(defalias 'eci-cs-getpos 'eci-cs-get-position)
(defalias 'eci-getpos 'eci-cs-get-position)
(defalias 'eci-get-position 'eci-cs-get-position)

(defun eci-cs-index-select (index &optional buffer-or-process)
  (interactive "nChainsetup index: ")
  (eci-command (format "cs-index-select %d" index) buffer-or-process))
(defalias 'eci-cs-iselect 'eci-cs-index-select)

(defun eci-cs-is-valid (&optional buffer-or-process)
  (interactive)
  (let ((val (eci-command "cs-is-valid" buffer-or-process)))
    (setq val 
          (cond
           ((= val 0)
            nil)
           ((string= val "1")
            t)
           (t (error "Unexpected reply in cs-is-valid"))))
    (if (interactive-p)
        (message (format "Chainsetup is%s valid" (if val "" " not"))))
    val))

(defun eci-cs-list (&optional buffer-or-process)
  (interactive)
  (let ((val (eci-command "cs-list" buffer-or-process)))
    (if (interactive-p)
        (message (concat "Available chainsetups: " 
                         (mapconcat #'identity val ", "))))
    val))

(defun eci-cs-load (filename &optional buffer-or-process)
  (interactive "fChainsetup filename: ")
  (eci-command (format "cs-load %s" filename) buffer-or-process))

(defun eci-cs-save (filename &optional buffer-or-process)
  (interactive)
  (eci-command "cs-save" buffer-or-process))

(defun eci-cs-save-as (filename &optional buffer-or-process)
  (interactive "FChainsetup filename: ")
  (eci-command (format "cs-save-as %s" filename) buffer-or-process))

(defun eci-cs-selected (&optional buffer-or-process)
  (interactive)
  (let ((val (eci-command "cs-selected")))
    (if (interactive-p)
        (message (format "Selected chainsetup: %s" val)))
    val))

(defun eci-cs-status (&optional buffer-or-process)
  "Return ChainSetup status as a Lisp object."
  ;; FIXME, only works with stable
  (let ((chains (split-string (eci-command "cs-status" buffer-or-process)
                              "^Chainsetup " ))
        (chainlist))
    (if (string-match "[\\* Controller/Chainsetup status \\*]" (car chains))
        (progn
          (setq chains (cdr chains))
          (mapc
           (lambda (str)
             (if (string-match "(\\([0-9]+\\)) \"\\([^\"\n]+\\)\".*\n\tFilename:\t\t\\([^\n]*\\)\n\tSetup:\t\t\tinputs \\([0-9]+\\) - outputs \\([0-9]+\\) - chains \\([0-9]+\\)\n\tBuffersize:\t\t\\([0-9]+\\)\n\tInternal sample rate:\t\\([0-9]+\\)\n\tDefault sformat:\t\\(.*\\)\n\tFlags:\t\t\t\\(.*\\)\n\tState:[\t ]+\\(.*\\)" str)
                 (setq
                  chainlist
                  (cons
                   (list
                    (string-to-number (match-string 1 str))
                    (match-string-no-properties 2 str)
                    (list 'filename (match-string-no-properties 3 str))
                    (list 'inputs (string-to-number (match-string 4 str)))
                    (list 'outputs (string-to-number (match-string 5 str)))
                    (list 'chains (string-to-number (match-string 6 str)))
                    (list 'buffersize (string-to-number (match-string 7 str)))
                    (list 'srate (string-to-number (match-string 8 str)))
                    (list 'sformat (match-string-no-properties 9 str))
                    (list 'flags (match-string-no-properties 10 str))
                    (list 'state (match-string-no-properties 11 str))) chainlist))))
           chains))
      (error "Unknown return value"))
    chainlist))

(defun eci-c-add (chains &optional buffer-or-process)
  "Adds a set of chains.  Added chains are automatically selected.
If argument CHAINS is a list, its elements are concatenated with ','."
  (interactive "sChain(s) to add: ")
  (eci-command (format "c-add %s"
                       (if (stringp chains)
                           chains
                         (mapconcat #'identity chains ",")))
               buffer-or-process))

(defun ecasound-read-list (prompt list)
  "Interactively prompt for a number of inputs until empty string.
PROMPT is used as prompt and LIST is a list of choices to choose from."
  (let ((avail list)
        result current)
    (while
        (and avail
             (not
              (string=
               (setq current (completing-read prompt (mapcar #'list avail)))
               "")))
      (setq result (cons current result)
            avail (delete current avail)))
    (nreverse result)))

(defun eci-c-deselect (chains &optional buffer-or-process)
  "Deselects chains."
  (interactive
   (list
    (ecasound-read-list "Chain to deselect: " (eci-c-selected))))
  (eci-command
   (format "c-deselect %s"
           (if (stringp chains)
               chains
             (mapconcat #'identity chains ",")))))

(defun eci-c-list (&optional buffer-or-process)
  (interactive)
  (eci-command "c-list"))

(defun eci-c-select (chains &optional buffer-or-process)
  "Selects chains.  Other chains are automatically deselected."
  (interactive
   (list
    (ecasound-read-list "Chain: " (eci-c-list))))
  (eci-command
   (format "c-select %s"
           (if (stringp chains)
               chains
             (mapconcat #'identity chains ",")))))

(defun eci-c-selected (&optional buffer-or-process)
  (interactive)
  (let ((val (eci-command "c-selected" buffer-or-process)))
    (if (interactive-p)
        (if (null val)
            (message "No selected chains")
          (message (concat "Selected chains: "
                           (mapconcat #'identity val ", ")))))
    val))

(defun eci-c-select-all (&optional buffer-or-process)
  "Selects all chains."
  (interactive)
  (eci-command "c-select-all" buffer-or-process))

(defun eci-cs-select (chainsetup &optional buffer-or-process)
  (interactive (list (completing-read "Chainsetup: " (mapcar
                                                      #'list (eci-cs-list)))))
  (eci-command (format "cs-select %s" chainsetup)))

(defun eci-ai-add (filename)
  (interactive "fInput filename: ")
  (eci-command (format "ai-add %s" filename)))

(defun eci-ao-add (filename)
  (interactive "FOutput filename: ")
  (eci-command (format "ao-add %s" filename)))

(defun eci-engine-status (&optional buffer-or-process)
  (eci-command "engine-status"))

(defun eci-cop-add (string &optional buffer-or-process)
  (interactive "sChainop to add: ")
  (eci-command (format "cop-add %s" string buffer-or-process)))

(defun eci-cop-select (integer &optional buffer-or-process)
  (interactive "nChainop to select: ")
  (eci-command (format "cop-select %d" integer buffer-or-process)))

(defun eci-copp-select (integer &optional buffer-or-process)
  (interactive "nChainop parameter to select: ")
  (eci-command (format "copp-select %d" integer buffer-or-process)))

(defun eci-copp-get (&optional buffer-or-process)
  (eci-command "copp-get" buffer-or-process))

(defun eci-copp-set (value &optional buffer-or-process)
  (interactive "nValue for Chain operator parameter: ")
  (eci-command (format "copp-set %f" value buffer-or-process)))

;;;; ECI Examples

(defun eci-example ()
  "Implements the example given in the ECI documentation."
  (interactive)
  (save-current-buffer
    (set-buffer (eci-init))
    (display-buffer (current-buffer))
    (eci-cs-add "play_chainsetup")
    (eci-c-add "1st_chain")
    (call-interactively #'eci-ai-add)
    (eci-ao-add "/dev/dsp")
    (eci-cop-add "-efl:100")
    (eci-cop-select 1) (eci-copp-select 1)
    (eci-cs-connect)
    (eci-command "start")
    (sit-for 1)
    (while (and (string= (eci-engine-status) "running")
                (< (eci-get-position) 15))
      (eci-copp-set (+ (eci-copp-get) 500))
      (sit-for 1))
    (eci-command "stop")
    (eci-cs-disconnect)
    (message (concat "Chain operator status: "
                      (eci-command "cop-status")))))

(defun eci-make-temp-file-name (suffix)
  (concat (make-temp-name
           (expand-file-name "emacs-eci" temporary-file-directory))
          suffix))

(defun ecasound-normalize (filename)
  "Normalize a audio file using ECI."
  (interactive "fFile to normalize: ")
  (let ((tmpfile (eci-make-temp-file-name ".wav")))
    (unwind-protect
        (with-current-buffer (eci-init)
          (display-buffer (current-buffer)) (sit-for 1)
          (eci-cs-add "analyze") (eci-c-add "1")
          (eci-ai-add filename) (eci-ao-add tmpfile)
          (eci-cop-add "-ev")
          (message "Analyzing sample data...")
          (eci-cs-connect) (eci-run)
          (eci-cop-select 1) (eci-copp-select 2)
          (let ((gainfactor (eci-copp-get)))
            (eci-cs-disconnect)
            (if (<= gainfactor 1)
                (message "File already normalized!")
              (eci-cs-add "apply") (eci-c-add "1")
              (eci-ai-add tmpfile) (eci-ao-add filename)
              (eci-cop-add "-ea:100")
              (eci-cop-select 1)
              (eci-copp-select 1)
              (eci-copp-set (* gainfactor 100))
              (eci-cs-connect) (eci-run) (eci-cs-disconnect)
              (message "Done"))))
      (if (file-exists-p tmpfile)
          (delete-file tmpfile)))))

;;; Utility functions for converting strings to data-structures.

(defun eci-process-cop-register (string)
  (set
   (make-local-variable 'eci-cop-register)
   (mapcar
    (lambda (cop)
      (when (string-match
             "^[0-9]+\\. \\([^,]+\\), \\(-[a-zA-Z]+\\):\\(.*\\)" cop)
        (list (match-string-no-properties 1 cop)
              (match-string-no-properties 2 cop)
              (split-string (match-string-no-properties 3 cop) ","))))
    (split-string string "\n"))))

(defun eci-process-ladspa-register (string)
  (let (result)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "[0-9]+\\. \\(.*\\)\n\t\\(-el:[^,\n]+\\),\\(.*\\)" nil t)
        (let ((full-name (match-string-no-properties 1))
              (name (match-string-no-properties 2))
              (args (split-string (concat "'," (match-string-no-properties 3)
                                          ",'") "','")))
          (setq result (cons (list full-name name args) result)))))
    (set (make-local-variable 'eci-ladspa-register)
         (nreverse result))))

(defun eci-process-cop-status (string)
  (with-temp-buffer
    (insert string) (goto-char (point-min))
    (when (re-search-forward
           "### Chain operator status (chainsetup '\\([^']+\\)') ###\n" nil t)
      (let ((cs (match-string-no-properties 1)) result)
        (while (re-search-forward "Chain \"\\([^\"]+\\)\":\n" nil t)
          (let ((c (match-string-no-properties 1)) chain)
            (while (re-search-forward
                    "\t\\([0-9]+\\)\\. \\([^:]+\\): \\(.*\\)\n" nil t)
              (let ((n (string-to-number (match-string 1)))
                    (name (match-string-no-properties 2))
                    (args
                     (mapcar
                      (lambda (elt)
                        (when (string-match
                               "\\[\\([0-9]+\\)\\] \\(.*\\) \\([0-9.-]+\\)$"
                               elt)
                          (list (match-string-no-properties 2 elt)
                                (string-to-number (match-string 1 elt))
                                (string-to-number (match-string 3 elt)))))
                      (split-string
                       (match-string-no-properties 3) ", "))))
                (setq chain (cons (append (list name n) args) chain))))
            (setq result (cons (reverse (append chain (list c))) result))))
        (reverse result)))))

(defun eci-cop-register ()
  (interactive)
  (let ((eci-hide-output (not (interactive-p))))
    (eci-command "cop-register")))

(defun eci-cop-status (&optional buffer-or-process)
  (interactive)
  (let ((eci-hide-output (not (interactive-p))))
    (eci-process-cop-status (eci-command "cop-status"))))

(defun eci-ladspa-register ()
  (interactive)
  (let ((eci-hide-output (not (interactive-p))))
    (eci-command "ladspa-register")))

(defun ecasound-cop-add ()
  "Interactively prompt for the name and argument of a chain operator to add."
  (interactive)
  (unless eci-cop-register
    (eci-cop-register))
  (unless eci-ladspa-register
    (eci-ladspa-register))
  (let* ((cop (completing-read "Chain operator: "
                               (append eci-cop-register eci-ladspa-register)))
         (args (nth 2 (or (assoc cop eci-cop-register)
                          (assoc cop eci-ladspa-register))))
         (arg (nth 1 (or (assoc cop eci-cop-register)
                         (assoc cop eci-ladspa-register))))
         args2)
    (while args
      (setq args2 (cons (read-from-minibuffer (concat (car args) ": "))
                        args2))
      (setq args (cdr args)))
    (setq args2 (nreverse args2))
    (eci-cop-add (concat arg ":" (mapconcat #'identity args2 ",")))))

;;; ChainOp Editor

(defvar ecasound-cop-edit-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map widget-keymap)
    map))

(define-derived-mode ecasound-cop-edit-mode fundamental-mode "COP-edit"
  "A major mode for editing ecasound chain operators.")

(defun ecasound-cop-edit ()
  "Edit the chain operator settings of the current session interactively.
This is done using the ecasound-cop widget."
  (interactive)
  (let ((cb (current-buffer))
        (chains (eci-cop-status)))
    (switch-to-buffer-other-window (generate-new-buffer "*cop-edit*"))
    (ecasound-cop-edit-mode)
    (mapc
     (lambda (chain)
       (widget-insert (format "Chain %s:\n" (car chain)))
       (mapc
        (lambda (cop)
          (apply 'widget-create 'ecasound-cop :buffer cb cop))
        (cdr chain)))
     chains)
    (widget-setup)
    (goto-char (point-min))))

(define-widget 'ecasound-cop 'default
  "A Chain Operator.
:children is a list of ecasound-copp widgets."
  :convert-widget 'ecasound-cop-convert
  :value-create 'ecasound-cop-value-create
  :format "%i %t\n%v"
  :format-handler 'ecasound-cop-format-handler)

(defun ecasound-cop-convert (widget)
  "Convert arguments passed to `widget-create' for this widget.
The arguments should be of the form NAME NUM (COPP-ARGS) ..."
  (let ((args (widget-get widget :args)))
    (when args
      (widget-put widget :tag (car args))
      (widget-put widget :cop-number (nth 1 args))
      (widget-put widget :args (cddr args))))
  widget)
  
(defun ecasound-cop-value-create (widget)
  "Insert the value of this widget into the buffer.
This function is called if the %v escape appears.
The \"value\" in this case is the list of chain operator parameters."
  (widget-put
   widget :children
   (mapcar
    (lambda (copp-arg)
      (apply 'widget-create-child-and-convert
             widget '(ecasound-copp) copp-arg))
    (widget-get widget :args))))

(defun ecasound-cop-format-handler (widget escape)
  (cond
   ((eq escape ?i)
    (widget-put
     widget :cop-select
     (widget-create-child-value widget
      '(ecasound-cop-select) (widget-get widget :cop-number))))))

(define-widget 'ecasound-cop-select 'link
  "Select this chain operator parameter."
  :help-echo "RET to select."
  :button-prefix ""
  :button-suffix ""
  :format "%[%v.%]"
  :action 'ecasound-cop-select-action)

(defun ecasound-cop-select-action (widget &rest ignore)
  "Selects WIDGET in its associated ecasound buffer."
  (let ((buffer (widget-get (widget-get widget :parent) :buffer)))
    (eci-cop-select (widget-value widget) buffer)))

;;;; A Chain Operator Parameter Widget.

; This is used as a component of the cop widget.

(define-widget 'ecasound-copp 'number
  "A Chain operator parameter."
  :action 'ecasound-copp-action
  :convert-widget 'ecasound-copp-convert
  :format "  %i %v (%t)\n"
  :format-handler 'ecasound-copp-format-handler
  :size 10)

(defun ecasound-copp-convert (widget)
  "Convert args."
  (let ((args (widget-get widget :args)))
    (when args
      (widget-put widget :tag (car args))
      (widget-put widget :copp-number (nth 1 args))
      (widget-put widget :value (nth 2 args))
      (widget-put widget :args nil)))
  widget)

(defun ecasound-copp-format-handler (widget escape)
  (cond
   ((eq escape ?i)
    (widget-put
     widget
     :copp-select
     (widget-create-child-value
      widget
      '(ecasound-copp-select)
      (widget-get widget :copp-number))))
   ((eq escape ?s)
    (widget-put
     widget
     :slider
     (widget-create-child-value
      widget
      '(slider)
      (string-to-number (widget-get widget :value)))))))

(defun ecasound-copp-action (widget &rest ignore)
  "Sets WIDGETs value in its associated ecasound buffer."
  (let ((buffer (widget-get (widget-get widget :parent) :buffer)))
    (if (widget-apply widget :match (widget-value widget))
        (progn
          (widget-apply (widget-get (widget-get widget :parent)
                                    :cop-select) :action)
          (widget-apply (widget-get widget :copp-select) :action)
          (eci-copp-set (widget-value widget) buffer))
      (message "Invalid"))))

(defvar ecasound-copp-select-keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "+" 'ecasound-copp-increase)
    (define-key map "-" 'ecasound-copp-decrease)
    map)
  "Keymap used inside an copp.")

(defun ecasound-copp-increase (pos &optional event)
  (interactive "@d")
  ;; BUG, if we do this, the field is suddently no longer editable, why???
  (let ((widget (widget-get (widget-at pos) :parent)))
    (widget-value-set
     widget
     (+ (widget-value widget) 1))
    (widget-apply widget :action)))

(defun ecasound-copp-decrease (pos &optional event)
  (interactive "@d")
  (let ((widget (widget-get (widget-at pos) :parent)))
    (widget-value-set
     widget
     (- (widget-value widget) 1))
    (widget-apply widget :action)))

(define-widget 'ecasound-copp-select 'link
  "Select this chain operator parameter."
  :help-echo "RET to select, +/- to set in steps."
  :keymap ecasound-copp-select-keymap
  :format "%[%v%]"
  :action 'ecasound-copp-select-action)

(defun ecasound-copp-select-action (widget &rest ignore)
  "Selects WIDGET in its associated ecasound buffer."
  (let ((buffer (widget-get (widget-get (widget-get widget :parent) :parent)
                            :buffer)))
    (eci-copp-select (widget-get widget :value) buffer)))

(define-widget 'slider 'default
  "A slider."
  :action 'widget-slider-action
  :button-prefix ""
  :button-suffix ""
  :format "(%[%v%])"
  :keymap
  (let ((map (copy-keymap widget-keymap)))
    (define-key map "\C-m" 'widget-slider-press)
    (define-key map "+" 'widget-slider-increase)
    (define-key map "-" 'widget-slider-decrease)
    map)
  :value-create 'widget-slider-value-create
  :value-delete 'ignore
  :value-get 'widget-value-value-get
  :size 70
  :value 0)

(defun widget-slider-press (pos &optional event)
  "Invoke slider at POS."
  (interactive "@d")
  (let ((button (get-char-property pos 'button)))
    (if button
        (widget-apply-action
         (widget-value-set
          button
          (- pos (overlay-start (widget-get button :button-overlay))))
         event)
      (let ((command (lookup-key widget-global-map (this-command-keys))))
        (when (commandp command)
          (call-interactively command))))))

(defun widget-slider-increase (pos &optional event)
  "Increase slider at POS."
  (interactive "@d")
  (widget-slider-change pos #'+ 1 event))

(defun widget-slider-decrease (pos &optional event)
  "Decrease slider at POS."
  (interactive "@d")
  (widget-slider-change pos #'- 1 event))

(defun widget-slider-change (pos function value &optional event)
  "Change slider at POS by applying FUNCTION to old-value and VALUE."
  (let ((button (get-char-property pos 'button)))
    (if button
        (widget-apply-action
         (widget-value-set button (apply function (widget-value button) value))
         event)
      (let ((command (lookup-key widget-global-map (this-command-keys))))
        (when (commandp command)
          (call-interactively command))))))

(defun widget-slider-action (widget &rest ignore)
  "Set the current :parent value to :value."
  (widget-value-set (widget-get widget :parent)
                    (widget-value widget)))

(defun widget-slider-value-create (widget)
  "Create a sliders value."
  (let ((size (widget-get widget :size))
        (value (string-to-int (format "%.0f" (widget-get widget :value))))
        (from (point)))
    (insert-char ?\  value)
    (insert-char ?\| 1)
    (insert-char ?\  (- size value 1))))

.
;;; Ecasound .ewf major mode

(defgroup ecasound-ewf nil
  "Ecasound .ewf file mode related variables and faces."
  :prefix "ecasound-ewf-"
  :group 'ecasound)

(defcustom ecasound-ewf-output-device "/dev/dsp"
  "*Default output device used for playing .ewf files."
  :group 'ecasound-ewf
  :type 'string)

(defface ecasound-ewf-keyword-face '((t (:foreground "IndianRed")))
  "The face used for highlighting keywords."
  :group 'ecasound-ewf)

(defface ecasound-ewf-time-face '((t (:foreground "Cyan")))
  "The face used for highlighting time information."
  :group 'ecasound-ewf)

(defface ecasound-ewf-file-face '((t (:foreground "Green")))
  "The face used for highlighting the filname."
  :group 'ecasound-ewf)

(defface ecasound-ewf-boolean-face '((t (:foreground "Orange")))
  "The face used for highlighting boolean values."
  :group 'ecasound-ewf)

(defvar ecasound-ewf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'pcomplete)
    (define-key map "\C-c\C-p" 'ecasound-ewf-play)
    map)
  "Keymap for `ecasound-ewf-mode'.")

(defvar ecasound-ewf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `ecasound-ewf-mode'.")

(defvar ecasound-ewf-font-lock-keywords
  '(("^\\s-*\\(source\\)[^=]+=\\s-*\\(.*\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-file-face))
    ("^\\s-*\\(offset\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(start-position\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(length\\)[^=]+=\\s-*\\([0-9.]+\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-time-face))
    ("^\\s-*\\(looping\\)[^=]+=\\s-*\\(true\\|false\\)$"
     (1 'ecasound-ewf-keyword-face)
     (2 'ecasound-ewf-boolean-face)))
  "Keyword highlighting specification for `ecasound-ewf-mode'.")

;;;###autoload
(define-derived-mode ecasound-ewf-mode fundamental-mode "EWF"
  "A major mode for editing ecasound .ewf files."
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(ecasound-ewf-font-lock-keywords))
  (ecasound-ewf-setup-pcomplete))

;;; .ewf-mode pcomplete support

(defun ecasound-ewf-keyword-completion-function ()
  (pcomplete-here
   (list "source" "offset" "start-position" "length" "looping")))

(defun pcomplete/ecasound-ewf-mode/source ()
  (pcomplete-here (pcomplete-entries)))

(defun pcomplete/ecasound-ewf-mode/offset ()
  (message "insert audio object at offset (seconds) [read,write]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/start-position ()
  (message "start offset inside audio object (seconds) [read]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/length ()
  (message "how much of audio object data is used (seconds) [read]")
  (throw 'pcompleted t))

(defun pcomplete/ecasound-ewf-mode/looping ()
  (pcomplete-here (list "true" "false")))

(defun ecasound-ewf-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  (let ((begin (save-excursion (beginning-of-line) (point)))
        (end (point))
        begins args)
    (save-excursion
      (goto-char begin)
      (while (< (point) end)
        (skip-chars-forward " \t\n=")
        (setq begins (cons (point) begins))
        (let ((skip t))
          (while skip
            (skip-chars-forward "^ \t\n=")
            (if (eq (char-before) ?\\)
                (skip-chars-forward " \t\n=")
              (setq skip nil))))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) (point))
                         args)))
      (cons (reverse args) (reverse begins)))))

(defun ecasound-ewf-setup-pcomplete ()
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'ecasound-ewf-parse-arguments)
  (set (make-local-variable 'pcomplete-command-completion-function)
       'ecasound-ewf-keyword-completion-function)
  (set (make-local-variable 'pcomplete-command-name-function)
       (lambda ()
         (pcomplete-arg 'first)))
  (set (make-local-variable 'pcomplete-arg-quote-list)
       (list ? )))

;;; Interactive commands

;; FIXME: Make it use ECI.
(defun ecasound-ewf-play ()
  (interactive)
  (let ((ecasound-arguments (list "-c"
                                  "-i" buffer-file-name
                                  "-o" ecasound-ewf-output-device)))
    (and (buffer-modified-p)
         (y-or-n-p "Save file before playing? ")
         (save-buffer))
    (ecasound "*Ecasound-ewf Player*")))

(add-to-list 'auto-mode-alist (cons "\\.ewf$" 'ecasound-ewf-mode))

;; Local variables:
;; mode: outline-minor
;; outline-regexp: ";;;;* \\|."
;; End:



(provide 'ecasound)

;;; ecasound.el ends here
