;;; psysh.el --- Comint-based integration for the "PsySH" REPL for PHP.  -*- lexical-binding: t; -*-
;;
;; Author: Phil Sainty
;; Created: April 2018
;; Version: 0.4.10

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; To start the repl:  M-x run-psysh
;;
;; To configure:  M-x customize-group RET psysh
;;
;; "C-d" exits the psysh process, as usual.
;; "g" will restart the process when it is not running.
;;
;; Customize `ansi-color-names-vector' if you find any colours hard
;; to see.  Alternatively, call `ansi-color-for-comint-mode-filter'
;; in `psysh-mode-hook' to filter the colour codes from the output;
;; or you can prevent PsySH from generating colour codes at all by
;; setting 'colorMode' in `psysh-config'.
;;
;; Note that if you are calling "sudo" in your `psysh-command' then
;; you may need to pass "--preserve-env=PSYSH_CONFIG" as an option to
;; the sudo command.

(require 'comint)
(require 'ansi-color)
(require 'subr-x)
(require 'tramp)
(require 'cc-mode) ;; `c-mode-syntax-table' is guaranteed to be available.
(require 'php-mode nil :noerror) ;; Derives from c-mode; mightn't be installed.

(defconst psysh--latest-version "0.4.10")

(defvar psysh-process-name "psysh"
  "Name for the comint process.")

(defvar psysh-buffer-name "*PsySH*"
  "Name for the comint buffer, or derive from `psysh-process-name'.")

(defvar psysh-run-psysh-function 'psysh-run-psysh
  "This affects the `run-psysh' command, including when restarting.

Setting this value buffer-locally is an easy way for wrapper
libraries to ensure that the correct function will be called when
interactively restarting the process.")

(defgroup psysh nil
  "PsySH REPL for PHP"
  :tag "PsySH"
  :prefix "psysh"
  :group 'php)

(defcustom psysh-command "psysh"
  "Command and arguments used by `run-psysh'.

This string is processed by `split-string-and-unquote' to establish
the command and its arguments.  No shell quoting of special characters
is needed, but you may need to double-quote arguments which would
otherwise be split."
  :type 'string)

(defcustom psysh-process-connection-type nil
  "The `process-connection-type' used when running PsySH.

The default nil value means that we use a pipe, not a pty.  If you are
experiencing problems connecting to the PsySH process then try setting
this value to t, to use a pty instead; but be aware of the following:

A pty will be in canonical / line-editing mode by default (use
\"stty -F <device> -icanon\" to disable that).  That provides support
for special line-editing key sequences, and also imposes a maximum
input size for any line (4096 bytes on my system), discarding the
overflow, which which means that PHP won't receive all the input.
Using a pipe instead of a pty avoids this 4K buffer issue, as well as
any other unwanted terminal-oriented weirdness which might occur.

If psysh's \\='useReadline\\=' option is enabled and one of the supported
implementations is available, then this issue shouldn't crop up (as
they will not be using canonical mode); however we need our default
behavior to cope with the fallback readline implementation provided by
psysh (Psy\\Readline\\Transient).

See also `psysh-tramp-process-connection-type'."
  :type '(choice (const :tag "Pipe" nil)
                 (const :tag "PTY" t)))

(defcustom psysh-tramp-process-connection-type t
  "The `tramp-process-connection-type' used when running PsySH.

If you are connecting to PsySH via tramp, then this value will be
used in place of `psysh-process-connection-type'.

The default t value means that we use a pty, not a pipe, which is
the same default as `tramp-process-connection-type' itself.

We default to a pty in this scenario because tramp connections
are liable to fail without a pty, and so this is the more
reliable default.

See `psysh-process-connection-type' for more information."
  :type '(choice (const :tag "Pipe" nil)
                 (const :tag "PTY" t)))

(defcustom psysh-prompt-main ">>> "
  "The REPL's main input prompt.

This must match the \\='prompt\\=' theme setting in `psysh-config'."
  :type 'string)

(defcustom psysh-prompt-continuation "... "
  "The REPL's input continuation prompt.

This must match the \\='bufferPrompt\\=' theme setting in `psysh-config'."
  :type 'string)

(defcustom psysh-prompt-regexp (rx-to-string
                                `(seq bol (or ,psysh-prompt-main
                                              ,psysh-prompt-continuation))
                                :nogroup)
  "Regexp matching any REPL prompt.

Defaults to matching either of `psysh-prompt-main' or
`psysh-prompt-continuation'.

See also the \\='theme\\=' setting in `psysh-config'."
  :type 'string)

(defvar psysh-remap-php-send-region)
(defvar php-mode-map)

(defun psysh-remap-php-send-region-setter (&optional option value)
  "Adds or removes remapping for `php-send-region' in `php-mode-map'."
  (when option
    (set option value))
  (with-eval-after-load "php-mode"
    (define-key php-mode-map [remap php-send-region]
      (if psysh-remap-php-send-region
          'psysh-send-region
        nil))))

(defcustom psysh-remap-php-send-region t
  "Whether to remap bindings for `php-send-region' to `psysh-send-region'."
  :type '(choice (const :tag "Remap to psysh-send-region" t)
                 (const :tag "Do not remap" nil))
  :set #'psysh-remap-php-send-region-setter)

(defcustom psysh-history-file nil
  "If non-nil, name of the file to read/write input history.

`comint-input-ring-file-name' will be set to this value, and the file will
be read from and written to automatically."
  :type `(choice (const :tag "Do not save input history" nil)
                 (file :tag "File"
                       :value ,(locate-user-emacs-file
                                ".psysh-history"))))

(define-widget 'psysh-text-widget 'text
  "Defined due to Emacs bug #31309."
  ;; We could use :type (widget-convert 'text :format "%{%t%}: %v") in
  ;; the `psysh-config' definition without defining a named
  ;; widget -- but that necessitates the loading of wid-edit.el, which
  ;; is not reasonable when it might not be otherwise needed.
  :format "%{%t%}: %v")

(defcustom psysh-config
  ;; `describe-variable' only displays this nicely since Emacs 26.1,
  ;; but `customize-option' will display it nicely in older versions.
  ;; Using a 2-space indent to minimise the chances of `indent-tabs-mode'
  ;; creating inconsistent indentation, as a 4-space `tab-width' is
  ;; quite common.
  ;;
  ;; TODO: There is some kind of bug with the text widget which causes
  ;; the *first* (only?) use of RET to go awry.  That is very annoying.
  ;; Investigate...
  "<?php
return array(
  // Configure the prompts to match `psysh-prompt-main' and
  // `psysh-prompt-continuation' explicitly.  Our defaults
  // are equivalent to specifying 'theme' => 'classic';
  // See \\Psy\\Output\\Theme::CLASSIC_THEME for details.
  'theme' => [
    'compact' => true,
    'prompt' => '>>> ',
    'bufferPrompt' => '... ',
    'replayPrompt' => '--> ',
    'returnValue' => '=>  ',
  ],
  'pager' => 'cat', # Comint provides a dumb terminal.
  'useReadline' => false, # No need for this in Emacs.
  'requireSemicolons' => false, # More convenient, but may cause issues.
  'startupMessage' => '<fg=blue>Emacs:</> M-x customize-group RET psysh RET',
  'errorLoggingLevel' => E_ALL,
  'warnOnMultipleConfigs' => true,
  // We use a pipe rather than a pty to talk to the process, and so psysh
  // does not know that colour support is available; but we can enable it.
  // 'colorMode' => 'disabled',
  'colorMode' => 'forced',
  // Similarly, prevent the pipe from causing psysh to drop into its
  // non-interactive mode.
  'interactiveMode' => 'forced',
);"
  "PHP code for the PsySH config file.

The PSYSH_CONFIG environment variable will point to a temporary
file containing this configuration, when psysh is invoked.

Refer to URL `http://github.com/bobthecow/psysh/wiki/Config-options'
for details of all the available config options.

At minimum the \\='pager\\=' value should be set to \\='cat\\=', as the comint
buffer provides only a dumb terminal.

For PsySH v0.11.9 or later, it's important for the \\='theme\\=' value
to specify prompts which match the values of `psysh-prompt-main'
and `psysh-prompt-continuation'.  If the \\='prompt\\=' value is set,
it similarly must match `psysh-prompt-main'.

If you set `psysh-comint-input-sender' to use the default comint
sender then you may wish to set \\='requireSemicolons\\=' => true, as
otherwise PsySH can send the input to PHP earlier than you had
intended.  For example, the following code would be submitted
immediately following the first method call, meaning that the
attempted second method call is treated as an entirely new PHP
statement, resulting in a parse error.

$object->method1()
       ->method2();

With \\='requireSemicolons\\=' enabled, the behaviour will be correct.
That setting does make it more awkward to use `psysh-send-region'
on arbitrary fragments of code, however.

If `psysh-comint-input-sender' is configured to use the custom
sender, then all input is manipulated into a single line, and
consequently \\='requireSemicolons\\=' is not necessary."
  ;; See also https://github.com/bobthecow/psysh/issues/361
  :type 'psysh-text-widget)

(defcustom psysh-initial-input nil
  ;; `describe-variable' only displays this nicely since Emacs 26.1,
  ;; but `customize-option' will display it nicely in older versions.
  "Initial input to send to PsySH.

For example you could send \"help\" to display the help text automatically
when invoking the REPL.

You may enter multiple lines.  Enter one command per line."
  :type '(choice (const :tag "No initial input" nil)
                 (psysh-text-widget :tag "Commands")))

(defcustom psysh-temp-file-mode nil
  "The file mode to set for temporary files.

This affects files for `psysh-config' and `psysh-initial-input'.

If nil, the default file mode is not modified, which is fine if
the same user is running both Emacs and PsySH; but any custom
commands for running PsySH as a different user may necessitate
that these temporary files be made more visible.

If non-nil, the value should be a string representation of an
octal number, such as \"640\".  Refer to man page `chmod(1)' for
details of the octal bit pattern.  The value may alternatively
be an integer value such as #o640, however Emacs will display
integer values in decimal which is more difficult for users to
recognise."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Octal mode string")
                 (integer :tag "Integer mode")))

(defun psysh-temp-file-mode ()
  "Return user option `psysh-temp-file-mode' as an integer, or nil."
  (cond ((not psysh-temp-file-mode)
         nil)
        ((integerp psysh-temp-file-mode)
         psysh-temp-file-mode)
        ((stringp psysh-temp-file-mode)
         (string-to-number psysh-temp-file-mode 8))))

(defcustom psysh-comint-input-sender t
  "How to send input to PsySH.

nil means use the default comint sender.
A function symbol means use that function as the sender.
Any other non-nil value means use the custom PsySH sender.

The custom PsySH sender manipulates all the input into a single
line of PHP, which circumvents bugs, including one whereby parts
of the PHP input are inadvertently treated as PsySH commands.
Refer to URL `https://github.com/bobthecow/psysh/issues/490'.

Unfortunately the creation of much longer input lines might also
cause problems in older versions of PsySH.  In particular, PHP
input exceeding 1024 characters may produce bugs/errors.  Refer
to URL `https://github.com/bobthecow/psysh/issues/496'

If you use the default comint sender then you may also wish to
set \\='requireSemicolons\\=' => true, in `psysh-config'."
  :type '(choice (const :tag "Standard comint sender" nil)
                 (const :tag "PsySH sender" t)
                 (function :tag "Custom function"
                           :value comint-simple-send)))

(defcustom psysh-doc-refill t
  "Whether to automatically refill psysh \\='doc\\=' output.

This will wrap lines at the current `window-width' if it is
smaller than 100 columns (being the hard-coded wrapping column
used for this documentation).

This occasionally causes unwanted indentations due to (rare)
errors in the text of the \\='doc\\=' database content.  At the time of
writing, \"doc var_export\" provides an example.  In practice, such
wrongly-wrapped text is usually still easy to read, and so these
minor disadvantages are vastly outweighed by the overall benefits.

Refer to URL `https://github.com/bobthecow/psysh/issues/595'."
  :type 'boolean)

(defvar psysh-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'psysh-comint-send-input-maybe)
    (define-key map [remap move-beginning-of-line]
      'psysh-move-beginning-of-line)
    ;; When there is no running process, 'g' starts a new one.
    ;; So to restart psysh, use 'C-d' (to exit), and then 'g'.
    (define-key map "g"
      `(menu-item "" run-psysh
                  :filter ,(lambda (cmd)
                             (unless (get-buffer-process (current-buffer))
                               cmd))))
    map)
  "Keymap for `psysh-mode'.")

(defvar psysh-syntax-table
  ;; Particularly needed to recognise comments in `psysh-comint-input-sender'.
  (let ((syntab (make-syntax-table)))
    (set-char-table-parent syntab c-mode-syntax-table)
    (modify-syntax-entry ?_  "_" syntab)
    (modify-syntax-entry ?`  "\"" syntab)
    (modify-syntax-entry ?\" "\"" syntab)
    (modify-syntax-entry ?#  "< b" syntab)
    (modify-syntax-entry ?\n "> b" syntab)
    (modify-syntax-entry ?$  "'" syntab)
    syntab)
  "Syntax table for PsySH.")

(defvar-local psysh-duplicate-prompt-regexp nil
  "Used by `psysh-comint-output-filter' to detect duplicate prompts.")

(defvar psysh-temp-file-hash-table
  (make-hash-table :test 'equal :size 16) ;; Not expecting many values.
  "Maps `psysh-config' values to config files with that config.

Each alist maps a user/host pair to a temporary file path on that host
which contains the same configuration.")

(defun psysh-temp-file (contents &optional prefix dir)
  "Returns the path of a temporary file containing CONTENTS.

PREFIX is the temp file name prefix.

If directory DIR (or `default-directory', if unspecified) is a
tramp path for a remote host, the temporary file will be created
on that host; otherwise the file will be created locally using
`make-temp-file'.  Any DIR value which is not `tramp-tramp-file-p'
can be used to enforce a local file.

If `psysh-temp-file-mode' is non-nil, `set-file-modes' will be
called after a new temp file is created.  Pre-existing temporary
files will not be affected by changes to `psysh-temp-file-mode'."
  (unless prefix
    (setq prefix "psysh."))
  (unless dir
    (setq dir default-directory))
  (let ((filemap (gethash contents psysh-temp-file-hash-table))
        (isremote (tramp-tramp-file-p dir))
        vec method user domain host port _localname hop)
    ;; Determine the user and host for the given DIR.
    (if isremote
        (setq vec (tramp-dissect-file-name dir)
              method (tramp-file-name-method vec)
              user (tramp-file-name-user vec)
              domain (unless (version< tramp-version "2.3")
                       (tramp-file-name-domain vec))
              host (tramp-file-name-host vec)
              port (unless (version< tramp-version "2.3")
                     (tramp-file-name-port vec))
              hop (tramp-file-name-hop vec))
      (setq user (user-login-name)
            host "localhost"))
    ;; Check for an existing cached config filename.
    (let* ((key (cons user host))
           (temp (cdr (assoc key filemap)))
           (fulltemp (psysh--remote-temp-filename
                      (and temp isremote) method user domain host port temp hop)))
      ;; Check that the cached temp file still exists.
      (unless (and fulltemp (file-exists-p fulltemp))
        (setq temp nil))
      ;; If there is no valid temp file, generate and cache a new one.
      (unless temp
        (setq temp (if isremote
                       (let ((tramp-temp-name-prefix prefix))
                         ;; Is there a way to get the full VEC out of
                         ;; tramp when making a temp file?!
                         (tramp-make-tramp-temp-file vec))
                     (make-temp-file prefix)))
        (push (cons key temp) filemap)
        (puthash contents filemap psysh-temp-file-hash-table)
        ;; Write the PsySH config to the new file.
        (setq fulltemp (psysh--remote-temp-filename
                        isremote method user domain host port temp hop))
        (with-temp-file fulltemp
          (insert (concat contents "\n")))
        ;; Set the file mode.
        (when psysh-temp-file-mode
          (set-file-modes fulltemp (psysh-temp-file-mode))))
      ;; Return the cached filename.
      temp)))

(defun psysh--remote-temp-filename (isremote method user domain host port temp hop)
  "Use `tramp-make-tramp-file-name' if ISREMOTE."
  (if isremote
      (apply #'tramp-make-tramp-file-name
             (if (version< tramp-version "2.3")
                 (list method user host temp hop)
               (list method user domain host port temp hop)))
    temp))

;;;###autoload
(defun run-psysh ()
  "Run an inferior instance of psysh inside Emacs."
  (interactive)
  (funcall psysh-run-psysh-function))

(defun psysh-run-psysh ()
  "Run an inferior instance of psysh inside Emacs."
  ;; Create the comint process if there is no buffer.
  (let ((buffer (get-buffer psysh-buffer-name)))
    (unless (and buffer (comint-check-proc buffer))
      (let* ((command (split-string-and-unquote psysh-command)))
        (let ((inhibit-read-only t)
              (process-connection-type psysh-process-connection-type)
              (tramp-process-connection-type psysh-tramp-process-connection-type)
              ;; nil by default (we use a pipe, not a pty).
              ;;
              ;; A pty will be in canonical / line-editing mode by
              ;; default ("stty -F <device> -icanon" to disable that).
              ;; That provides support for special line-editing key
              ;; sequences, and also imposes a maximum input size for
              ;; any line (4096 bytes in my tests), discarding the
              ;; overflow, which which means that PHP won't receive
              ;; all the input.  Using a pipe instead of a pty avoids
              ;; this 4K buffer issue, as well as any other unwanted
              ;; terminal-oriented weirdness which might occur.
              ;;
              ;; If psysh's 'useReadline' option is enabled and one of
              ;; the supported implementations is available, then this
              ;; issue shouldn't crop up (as they will not be using
              ;; canonical mode); however we need our default behavior
              ;; to cope with the fallback readline implementation
              ;; provided by psysh (Psy\Readline\Transient).
              )
          (setq buffer (apply 'make-comint-in-buffer
                              psysh-process-name psysh-buffer-name
                              "env"
                              (and psysh-initial-input
                                   (psysh-temp-file psysh-initial-input
                                                    "psysh-startfile." t))
                              (format "PSYSH_CONFIG=%s"
                                      (psysh-temp-file
                                       psysh-config "psysh-config."))
                              command)))
        ;; The default `internal-default-process-sentinel' sentinel
        ;; used by comint conflicts with `comint-prompt-read-only',
        ;; generating errors like the following:
        ;; * comint-exec: Text is read-only
        ;; * error in process sentinel: Text is read-only
        ;; In lieu of any custom sentinel requirements, set the
        ;; sentinel to 'ignore in otder to avoid these errors.
        (let ((proc (get-buffer-process buffer)))
          (when (processp proc)
            (set-process-sentinel proc 'psysh-sentinel)))))
    ;; Check that buffer is in `psysh-mode'.
    (with-current-buffer buffer
      (unless (derived-mode-p 'psysh-mode)
        (psysh-mode)))
    ;; Pop to the psysh buffer.
    (pop-to-buffer-same-window buffer)))

(define-derived-mode psysh-mode comint-mode "PsySH"
  "Major mode for `run-psysh'.

\\<psysh-mode-map>"
  nil "Psysh"
  ;; This sets up the prompt so it matches >>> and ...
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-regexp psysh-prompt-regexp)

  ;; `psysh-comint-output-filter' needs a version of the prompt
  ;; which is not anchored to the start of the line, in order to
  ;; remove any additional prompts following the initial one.
  (setq-local psysh-duplicate-prompt-regexp
              (string-remove-prefix "^" psysh-prompt-regexp))

  ;; Make the prompt read only.
  (setq-local comint-prompt-read-only t)

  ;; Assume that the subprocess echoes input appropriately.
  (setq-local comint-process-echoes t)

  ;; Filter process output before insertion into buffer.
  (add-hook 'comint-preoutput-filter-functions
            'psysh-comint-preoutput-filter nil :local)

  ;; Filter process output after insertion into buffer.
  (add-hook 'comint-output-filter-functions
            'psysh-comint-output-filter nil :local)

  ;; Enable ANSI colour.
  (ansi-color-for-comint-mode-on)
  ;; (ansi-color-for-comint-mode-filter)
  ;;
  ;; Note that these are also commands:
  ;; M-x ansi-color-for-comint-mode-on
  ;; M-x ansi-color-for-comint-mode-off
  ;; M-x ansi-color-for-comint-mode-filter

  ;; Re-format documentation AFTER converting the ansi colour escape
  ;; characters -- otherwise the indentations will be all wrong.
  (add-hook 'comint-output-filter-functions
            'psysh-comint-output-filter-docs :append :local)

  ;; Don't highlight whitespace.
  (setq show-trailing-whitespace nil)

  ;; Restore the input history.
  (setq-local comint-input-ring-file-name psysh-history-file)
  (comint-read-input-ring :silent)

  ;; Use custom input sender function (which will hand off to another
  ;; function if necessary, in accordance with the user option).
  (setq-local comint-input-sender 'psysh-comint-input-sender)

  ;; Paragraph delimiters??
  ;; I don't know what these should be for PHP...
  ;; Maybe the main prompt, for skipping back over previous output?
  ;; This makes it so commands like M-{ and M-} work.
  ;; (setq-local paragraph-separate "\\'")
  ;; (setq-local paragraph-start psysh-prompt-regexp)

  ;; Certain `php-mode'-alike settings.
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (set-syntax-table psysh-syntax-table)
  (comment-normalize-vars)

  ;; Use `php-eldoc' if available.
  (when (require 'php-eldoc nil t)
    (setq-local eldoc-documentation-function 'php-eldoc-function)
    (eldoc-mode 1))

  ;; end of `psysh-mode' body
  )

(defvar psysh-process-mark-position nil
  "Remembers the process mark before `comint-send-input' is called.")

(defun psysh-comint-send-input-maybe ()
  "This is a hack, due to https://github.com/bobthecow/psysh/issues/490

We bind RET to this command in our keymap so a newline following a '}'
does not send the input, but instead just inserts a literal newline."
  (interactive)
  ;; Require a double-newline after a "}" to send the input, otherwise
  ;; we can very easily lose the 'else' clause from an if-then-else
  ;; statement.  (We can still lose it this way, but it's much less
  ;; likely to happen.)
  (if (looking-back "}" (line-beginning-position 0))
      ;; Just insert a newline, faking a continuation prompt.
      (insert (propertize "\n" 'display
                          (propertize (concat "\n" psysh-prompt-continuation)
                                      'face 'comint-highlight-prompt)))
    ;; If another newline is entered at the fake continuation prompt,
    ;; remove that display property before sending the input.
    (let* ((prevchar (1- (point)))
           (tidyup (looking-back "\n" prevchar)))
      (when tidyup
        (let ((display (plist-get (text-properties-at prevchar) 'display)))
          (and display
               (equal display (concat "\n" psysh-prompt-continuation))
               (remove-text-properties prevchar (point) '(display nil))))))
    ;; Remember the process mark position for `psysh-comint-input-sender'.
    (setq psysh-process-mark-position (marker-position
                                       (process-mark (get-buffer-process
                                                      (current-buffer)))))
    ;; Send the input.
    (comint-send-input)
    ;; FIXME: A fake continuation on the previous line now gets
    ;; highlighted with the `comint-highlight-input' face. It would be
    ;; nice to detect that and either undo or override it.
    ))

(defun psysh-comint-input-sender (proc string)
  "This is a hack, due to https://github.com/bobthecow/psysh/issues/490

It replaces newlines in the input, so that no PsySH command
can ever be executed unless it appears at the very start of the
input.

Newlines in strings are left alone, as that case has been fixed
upstream.

Comments are deleted entirely, so newlines at the end of line
comments (i.e. //... and #...) will not cause the subsequent
lines to be commented out as well.

Note that certain PsySH commands are intended to be usable in
positions other than the start of the input, and this sender
function inhibits that ability in favour of guaranteeing that
PsySH commands will not be executed inadvertantly.

If you wish to disable this functionality, you can customize the
`psysh-comint-input-sender' user option."
  (cond
   ;; user-defined
   ((functionp psysh-comint-input-sender)
    (funcall psysh-comint-input-sender proc string))
   ;; default comint
   ((or (not psysh-comint-input-sender)
        ;; We also use this at a continuation prompt, because we are
        ;; only seeing a part of the input, and therefore we cannot do
        ;; any syntax parsing.
        (and psysh-process-mark-position
             (save-excursion
               (goto-char psysh-process-mark-position)
               (looking-back (concat "^" (regexp-quote
                                          psysh-prompt-continuation))
                             (line-beginning-position)))))
    (comint-simple-send proc string))
   ;; psysh
   (t
    (comint-simple-send
     proc (with-temp-buffer
            ;; (display-buffer (current-buffer)) ;; debug
            (set-syntax-table (or (bound-and-true-p php-mode-syntax-table)
                                  psysh-syntax-table))
            (setq-local comment-start "//")
            (setq-local comment-end "")
            (comment-normalize-vars)
            (insert string)
            (let ((pos (point-min)))
              (goto-char pos)
              (when (looking-at "<\\?php")
                (replace-match ""))
              (save-match-data
                (while (re-search-forward "\\(;[[:blank:]]*\\)?\\(\n+\\)" nil :noerror)
                  (save-excursion
                    (goto-char (match-beginning 0))
                    (let ((ppss (syntax-ppss)))
                      (cond ;; ;; Newlines in strings.
                            ;; ;; Substitute the escape sequence.
                            ;; ((nth 3 ppss) ;; string
                            ;;  (replace-match
                            ;;   ;; n.b. This isn't actually correct, as
                            ;;   ;; the newline escape sequence is for
                            ;;   ;; double-quoted strings only!
                            ;;   (mapconcat 'identity (make-list
                            ;;                         (length (match-string 2)) "\\n")
                            ;;              "")
                            ;;   t t nil 2))
                            ;;
                            ;; Newlines in comments.
                            ;; We delete the entire comment.
                            ((nth 4 ppss) ;; comment
                             (let* ((start (comment-search-backward pos :noerror))
                                    (end (and start
                                              (progn (goto-char start)
                                                     (while (forward-comment 1))
                                                     (point)))))
                               (when (and start end)
                                 (delete-region start end)
                                 (goto-char start)
                                 (unless (or (bobp) (eobp))
                                   (insert " ")))))
                            ;; Newlines following a semicolon.
                            ;; We retain a newline, but make it 'safe' by ensuring
                            ;; that the following line cannot be interpreted as a
                            ;; PsySH command.
                            ((eq (char-after) ?\;)
                             (replace-match "\n;"))
                            ;; Other newlines are replaced by a space.
                            (t
                             (replace-match " ")))))
                  (setq pos (point)))))
            ;; Delete leading whitespace, which can cause errors.
            (goto-char (point-min))
            (while (forward-comment 1))
            (delete-region (point-min) (point))
            ;; (message "%s" (buffer-string))
            (buffer-string))))))

(defun psysh-comint-preoutput-filter (output)
  "Strip trailing whitespace from comint process output lines.

Called via `comint-preoutput-filter-functions'."
  (replace-regexp-in-string "  +$" "" output t t))

(defun psysh-comint-output-filter (_output)
  "Delete any duplicate prompts.

Called via `comint-output-filter-functions'."
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (when (looking-at comint-prompt-regexp)
      (comint-skip-prompt)
      (let ((pos (point))
            (comint-prompt-regexp psysh-duplicate-prompt-regexp))
        (while (comint-skip-prompt)
          (delete-region pos (point)))))))

(defun psysh-comint-output-filter-docs (_output)
  "Re-fill the output from the psysh `doc' command.

Wrap lines at the current `window-width'.

Called via `comint-output-filter-functions'."
  ;; The logic is essentially:
  ;;
  ;; - Find the 'most indented' column in the current line, where any
  ;;   sequence of 2+ spaces indicates a new level of indentation.
  ;;
  ;; - Remember that position in the text as the start point for
  ;;   reformatting.
  ;;
  ;; - Find all the immediately-following lines (if any) which are
  ;;   indented directly (no intervening text) to that same column,
  ;;   and which do not appear to be a list item (based on * prefix).
  ;;
  ;; - Remember the end of the final line in that group as the end
  ;;   point for reformatting.
  ;;
  ;; - Generate an indentation/padding string of spaces matching the
  ;;   indent level.
  ;;
  ;; - Set the wrapping column based on the window width.
  ;;
  ;; - Pass the text from the start point to the end point to the
  ;;   re-formatter, along with the wrap column and the padding string
  ;;   to prefix to each new line when wrapping.
  ;;
  ;; - Move to the next line, and loop until finished.
  ;;
  ;; The "2+ spaces" heuristic is a fragile one, but it works
  ;; perfectly in most cases (and all the counter-examples I've seen
  ;; appear to be errors in the documentation.  E.g. doc var_export).
  ;; I find that the benefits outweigh these occasional disadvantages.
  (unless (or (not psysh-doc-refill)
              (>= (window-width) 100))
    (save-excursion
      (goto-char comint-last-input-start)
      (when (looking-at " *doc ")
        (save-restriction
          (narrow-to-region comint-last-input-end
                            (marker-position
                             (process-mark (get-buffer-process
                                            (current-buffer)))))
          (while (let* (;;(linestart (point))
                        (lineend (line-end-position))
                        (start (progn
                                 (while (re-search-forward
                                         " \\{2,\\}" lineend t))
                                 (point)))
                        (indent (current-column)))
                   (while (and (forward-line 1)
                               (not (eobp))
                               (not (looking-at " *\\* "))
                               (progn (back-to-indentation)
                                      (eql (current-column) indent))))
                   (unless (eobp)
                     (goto-char (1- (line-beginning-position)))
                     (let ((fill-column (1- (window-width)))
                           (fill-prefix (make-string indent ?\s)))
                       (fill-region start (point))))
                   ;; Outer `while' loop condition:
                   (eql 0 (forward-line 1)))))))))

(defun psysh-sentinel (process _str)
  "Process signals from the psysh process."
  ;; Upon process exit, write `comint-input-ring' history file.
  (when (memq (process-status process) '(exit signal))
    (comint-write-input-ring)))

(defun psysh-move-beginning-of-line ()
  "Move to the beginning of the line, respecting the prompt."
  ;; Derived from `comint-line-beginning-position'.
  (interactive)
  (beginning-of-line)
  (comint-skip-prompt))

(defun psysh-move-beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation, respecting the prompt."
  (interactive)
  (if (not (bolp))
      ;; Find the end of the prompt (if any) on this line.
      (let ((eop (save-excursion
                   (beginning-of-line)
                   (comint-skip-prompt)
                   (point))))
        (if (<= (point) eop) ;; At (or within) the prompt, so go to bol.
            (beginning-of-line)
          (goto-char eop))) ;; Else beyond the prompt, so return to it.
    ;; Otherwise we were initially at bol, and may want to move forwards.
    (comint-skip-prompt) ;; If there is a prompt, move to that.
    (when (bolp) ;; There was no prompt to skip.  Skip indentation instead.
      (back-to-indentation))))

(defun psysh-send-region ()
  "Send the current line or active region to PsySH.

If no PsySH buffer is found, fall back to calling `php-send-region'.

If \\='requireSemicolons\\=' is set to true in `psysh-config', then you
will not be able to directly execute regions which are not
terminated with a semicolon -- you would need to switch to the
REPL and enter the semicolon manually."
  (interactive)
  (let* ((buf (get-buffer psysh-buffer-name))
         (proc (get-buffer-process buf))
         (bounds (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (line-beginning-position)
                         (line-end-position)))))
    (if proc
        (progn (psysh-comint-input-sender
                proc (apply #'buffer-substring-no-properties bounds))
               (display-buffer buf))
      (if (fboundp 'php-send-region)
          (progn (apply #'php-send-region bounds)
                 (display-buffer "*PHP*"))
        (error "No %s buffer or process was found" psysh-buffer-name)))))

(psysh-remap-php-send-region-setter) ;; Arrange the initial remapping.

(defvar psysh-version psysh--latest-version
  "The loaded version of psysh.el.")

;; Version-specific updates.
(when (version< psysh-version psysh--latest-version)
  ;; Perform each update in sequence, as necessary.
  ;; Update to version 1.N from earlier versions:
  ;; (when (version< psysh-version "1.N") ...)
  ;;
  ;; All updates completed.
  (setq psysh-version psysh--latest-version))

(provide 'psysh)
;;; psysh.el ends here
