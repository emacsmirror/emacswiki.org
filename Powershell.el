;;; powershell.el --- run powershell as an inferior shell in emacs
;;
;; Author:     Dino Chiesa <dpchiesa@hotmail.com>
;; Created:    10 Apr 2008
;; Modified:   May 2010
;; Version:    0.2.1
;; Keywords:   powershell shell
;; X-URL:      http://www.emacswiki.org/emacs/PowerShell#toc3
;;

;;; Commentary:
;;
;; Run Windows PowerShell v1.0 ir v2.0 as an inferior shell within
;; emacs. Tested with emacs v22.2.
;;
;; To use it, M-x powershell .
;;
;; ==============
;;
;; TODO:
;;
;; - get TAB to do proper completion for powershell commands, filenames,
;;   etc.
;;
;;

;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2010, Dino Chiesa
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.
;;
;; Neither the name of the author or any contributors, nor the names of
;; any organizations they belong to, may be used to endorse or promote
;; products derived from this software without specific prior written
;; permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY
;; WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;;



(require 'shell)



(defvar powershell-prompt-regex  "PS [^#$%>]+>"
  "Regexp for powershell prompt.

Powershell.el uses this regex to determine when a command has completed.

Therefore, you need to set this appropriately if you explicitly change the prompt function in powershell.

The default value will match the default PowerShell prompt.
")

(defvar powershell-command-reply nil
  "For internal use only. It holds the reply of powershell commands sent for housekeeping purposes.")


(defvar powershell--max-window-width  0
  "The maximum width of a powershell window.  You shouldn't need to ever set this.  It gets set automatically when the powershell starts up. "
  )

(defvar powershell-command-timeout-seconds 12
  "The timeout for a powershell command.")


(defvar powershell-squish-results-of-silent-commands t
"The function `powershell-invoke-command-silently' returns the results of a command in a string.  PowerShell by default, inserts newlines when the output exceeds the configured width of the powershell virtual window. In some cases callers might want to get the results with the newlines removed.   If this is true, then newlines are removed.")


(defvar powershell--need-rawui-resize t
  "No need to fuss with this.  It's intended for internal use only.  It gets set when powershell needs to be informed that emacs has resized its window. ")



(defconst powershell--find-max-window-width-command
  (concat
  "function _Emacs_GetMaxPhsWindowSize \n"
"{\n"
"  $rawui = (Get-Host).UI.RawUI\n"
"  $mpws_exists = ($rawui | Get-Member | ? {$_.Name -eq \"MaxPhysicalWindowSize\"})\n"
"  if ($mpws_exists -eq $null) {\n"
"    \"210\" | Out-Host\n"
"  } else {\n"
"    $rawui.MaxPhysicalWindowSize.Width | Out-Host\n"
"  }\n"
"}\n"
"_Emacs_GetMaxPhsWindowSize\n"
)
  "The powershell logic to determine the max physical window width."
  )



(defconst powershell--set-window-width-fn-name  "_Emacs_SetWindowWidth"
  "The name of the function this mode defines in PowerShell to set the window width. Intended for internal use only. ")


(defconst powershell--text-of-set-window-width-ps-function
  ;; see http://blogs.msdn.com/lior/archive/2009/05/27/ResizePowerShellConsoleWindow.aspx
  ;;
  ;; When making the console window narrower, you mus set the window
  ;; size first. When making the console window wider, you must set the
  ;; buffer size first.

    (concat  "function " powershell--set-window-width-fn-name "([string] $pswidth)\n"
             "{\n"
             ;;"  \"resetting window width to $pswidth\n\" | Out-Host\n"
             "  $rawui = (Get-Host).UI.RawUI\n"
             "  # retrieve the values\n"
             "  $bufsize = $rawui.BufferSize\n"
             "  $winsize = $rawui.WindowSize\n"
             "  $cwidth = $winsize.Width\n"
             "  $winsize.Width = $pswidth \n"
             "  $bufsize.Width = $pswidth\n"
             "  if ($cwidth -lt $pswidth) {\n"
             "    # increase the width\n"
             "    $rawui.BufferSize = $bufsize\n"
             "    $rawui.WindowSize = $winsize\n"
             "  }\n"
             "  elseif ($cwidth -gt $pswidth) {\n"
             "    # decrease the width\n"
             "    $rawui.WindowSize = $winsize\n"
             "    $rawui.BufferSize = $bufsize\n"
             "  }\n"
             "  # destroy variables\n"
             "  Set-Variable -name rawui -value $null\n"
             "  Set-Variable -name winsize -value $null\n"
             "  Set-Variable -name bufsize -value $null\n"
             "  Set-Variable -name cwidth -value $null\n"
             "}\n\n")

    "The text of the powershell function that will define the function _Emacs_SetWindowWidth within powershell.")




;; (defun dino-powershell-complete (arg)
;;   "do powershell completion on the given STRING. Pop up a buffer with the completion list."
;;   (interactive
;;    (list (read-no-blanks-input "\
;; Stub to complete: ")))

;;   (let ((proc
;;          (get-buffer-process (current-buffer))))
;;    (comint-proc-query proc (concat "Get-Command " arg "*\n"))
;;    )
;; )

;; (defun dino-powershell-cmd-complete ()
;;   "try to get powershell completion to work."
;;   (interactive)
;;   (let ((proc
;;          (get-buffer-process (current-buffer))))
;; ;;   (comint-proc-query proc "Get-a\t")
;; ;;   (comint-simple-send proc "Get-a\t")
;;        (comint-send-string proc "Get-a\t\n")
;; ;;   (process-send-eof)
;;    )
;; )



(defun powershell--define-set-window-width-function (proc)
  "Sends a function definition to the PowerShell identified by PROC.  The function set the window width, and later, it will be called when the width of the emacs window changes.
"
    (if proc
        (progn
          ;;process-send-string
          (comint-simple-send
           proc
           powershell--text-of-set-window-width-ps-function))))




(defun powershell--get-max-window-width  (buffer-name)
  "Gets the max width of the virtual window for PowerShell running in the buffer with name BUFFER-NAME.

In PowerShell 1.0, the maximum WindowSize.Width for
PowerShell is 210, hardcoded, I believe. In PowerShell 2.0, the max
windowsize.Width is provided in the RawUI.MaxPhysicalWindowSize
property.

This function does the right thing, and sets the buffer-local
`powershell--max-window-width' variable with the correct value.

"
  (let ((proc (get-buffer-process buffer-name)))

    (if proc
        (save-excursion
          (set-buffer buffer-name) ;; to get buffer-local variables

          (powershell-invoke-command-silently
           proc
           powershell--find-max-window-width-command
           0.90)

          ;; store the retrieved width
          (setq powershell--max-window-width
                (if (and (not (null powershell-command-reply))
                         (string-match
                          "\\([1-9][0-9]*\\)[ \t\f\v\n]+"
                          powershell-command-reply))
                    (string-to-number (match-string 1 powershell-command-reply))
                  200)))))) ;; could go to 210, but let's use 200 to be safe




(defun powershell--set-window-width (proc)
  "Run the PowerShell function that sets the RawUI width
appropriately for a PowerShell shell.

This is necessary to get powershell to do the right thing, as far
as text formatting, when the emacs window gets resized.
"
  (let ((ps-width
         (number-to-string (min powershell--max-window-width (window-width)))))
    (progn
      ;;(process-send-string
      (comint-simple-send
       proc
       (concat powershell--set-window-width-fn-name
               "('" ps-width "')")))))




;;;###autoload
(defun powershell (&optional buffer prompt-string)

  "Run an inferior PowerShell, with I/O through tne named BUFFER (which defaults to `*PowerShell*').

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a new shell.

If BUFFER exists and the shell process is running, just switch to BUFFER.

If PROMPT-STRING is non-nil, sets the prompt to the given value.

See the help for `shell' for more details.  \(Type
\\[describe-mode] in the shell buffer for a list of commands.)

"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*PowerShell*")))))

  ;; get a name for the buffer
  (setq buffer (get-buffer-create (or buffer "*PowerShell*")))

  (let ((tmp-shellfile explicit-shell-file-name))
    ;; set arguments for the powershell exe.
    ;; This needs to be tunable.
    (setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")
    (setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt

    ;; launch the shell
    (shell buffer)

    ;; restore the original shell
    (if tmp-shellfile
        (setq explicit-shell-file-name tmp-shellfile)))


  ;; (powershell--get-max-window-width "*PowerShell*")
  ;; (powershell-invoke-command-silently (get-buffer-process "*csdeshell*") "[Ionic.Csde.Utilities]::Version()" 2.9)

  ;;  (comint-simple-send (get-buffer-process "*csdeshell*") "prompt\n")



  (let ((proc (get-buffer-process buffer)))

    (make-local-variable 'powershell-prompt-regex)
    (make-local-variable 'powershell-command-reply)
    (make-local-variable 'powershell--max-window-width)
    (make-local-variable 'powershell-command-timeout-seconds)
    (make-local-variable 'powershell-squish-results-of-silent-commands)
    (make-local-variable 'powershell--need-rawui-resize)
    (make-local-variable 'comint-prompt-read-only)

    ;; disallow backspace over the prompt:
    (setq comint-prompt-read-only t)

    ;; We need to tell powershell how wide the emacs window is, because
    ;; powershell pads its output to the width it thinks its window is.
    ;;
    ;; The way it's done: every time the width of the emacs window changes, we
    ;; set a flag. Then, before sending a powershell command that is
    ;; typed into the buffer, to the actual powershell process, we check
    ;; that flag.  If it is set, we  resize the powershell window appropriately,
    ;; before sending the command.

    ;; If we didn't do this, powershell output would get wrapped at a
    ;; column width that would be different than the emacs buffer width,
    ;; and everything would look ugly.

    ;; get the maximum width for powershell - can't go beyond this
    (powershell--get-max-window-width buffer)

    ;; define the function for use within powershell to resize the window
    (powershell--define-set-window-width-function proc)

    ;; add the hook that sets the flag
    (add-hook 'window-size-change-functions
              '(lambda (&optional x)
                 (setq powershell--need-rawui-resize t)))

    ;; set the flag so we resize properly the first time.
    (setq powershell--need-rawui-resize t)

    (if prompt-string
        (progn
          ;; This sets up a prompt for the PowerShell.  The prompt is
          ;; important because later, after sending a command to the
          ;; shell, the scanning logic that grabs the output looks for
          ;; the prompt string to determine that the output is complete.
          (comint-simple-send
           proc
           (concat "function prompt { '" prompt-string "' }"))

          (setq powershell-prompt-regex prompt-string)))


    ;; hook the kill-buffer action so we can kill the inferior process?
    (add-hook 'kill-buffer-hook 'powershell-delete-process)

    ;; wrap the comint-input-sender with a PS version
    ;; must do this after launching the shell!
    (make-local-variable 'comint-input-sender)
    (setq comint-input-sender 'powershell-simple-send)

    ;; set a preoutput filter for powershell.  This will trim newlines after the prompt.
    (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)

    ;; send a carriage-return  (get the prompt)
    (comint-send-input)
    (accept-process-output proc)

    )


  ;; The launch hooks for powershell has not (yet?) been implemented
  ;;(run-hooks 'powershell-launch-hook)

  ;; return the buffer created
  buffer)




(defun powershell--silent-cmd-filter (process result)
  "A process filter that captures output from a shell and stores it to `powershell-command-reply', rather than allowing the output to be displayed in the shell buffer.

"
  ;;(csde-log 4 "csde-shell-exec-filter: got output: '%s'" result)

  ;;(message "ps-reply: %s" powershell-command-reply)

  (let ((end-of-result
         (string-match (concat ".*\n\\(" powershell-prompt-regex "\\)[ \n]*\\'")
                       ;;powershell-command-reply
                       result
                       )))
    (if (and end-of-result (numberp end-of-result))

        (progn
          ;; Store everything except the follow-on prompt.
          ;; The result probably includes a final newline!
          (setq result (substring result 0 (match-beginning 1)))

          (if powershell-squish-results-of-silent-commands
              (setq result
                    (replace-regexp-in-string "\n" "" result)))

          (setq powershell-command-reply
                (concat powershell-command-reply result)))


      (progn
        (if powershell-squish-results-of-silent-commands
              (setq result
                    (replace-regexp-in-string "\n" "" result)))

        (setq powershell-command-reply
              (concat powershell-command-reply result))

        ;; recurse.  For very very long output, the recursion can
        ;; cause stack overflow. Careful!
        (accept-process-output process powershell-command-timeout-seconds)))))



(defun powershell-invoke-command-silently (proc command &optional timeout-seconds)
  "Invoke COMMAND in the PowerShell instance PROC, silently, without echoing the results to the associated buffer.  Use TIMEOUT-SECONDS as the timeout, waiting for a response.  The COMMAND should be a string, and need not be terminated with a newline.

This is helpful when, for example, doing setup work.  Or other sneaky stuff.

Returns the result of the command, a string, without the follow-on command prompt.  The result will probably end in a newline. This result is also stored in the buffer-local variable `powershell-command-reply'.

This function should be invoked within a call to `save-excursion' in order to insure that the buffer-local values of `powershell-command-reply', `powershell-prompt-regex', and `powershell-command-timeout-seconds' are used.

Example:

    (save-excursion
      (set-buffer buffer-name)

      (powershell-invoke-command-silently
       proc
       command-string
       0.90)
     )
"

  (let ((old-timeout powershell-command-timeout-seconds)
        (original-filter (process-filter proc)))

    (setq powershell-command-reply nil)

    (if timeout-seconds
        (setq powershell-command-timeout-seconds timeout-seconds))

    (set-process-filter proc 'powershell--silent-cmd-filter)

    ;; Send the command plus the "prompt" command.  The filter
    ;; will know the command is finished when it sees the command
    ;; prompt.
    ;;
    (process-send-string proc (concat command "\nprompt\n"))

    (accept-process-output proc powershell-command-timeout-seconds)

    ;; output of the command is now available in powershell-command-reply


          ;; Trim prompt from the beginning of the output.
          ;; this can happen for the first command through
          ;; the shell.  I think there's a race condition.
          (if (string-match (concat "^" powershell-prompt-regex "\\(.*\\)\\'")
                            powershell-command-reply)
              (setq powershell-command-reply
                    (substring powershell-command-reply
                               (match-beginning 1)
                               (match-end 1))))




    ;; restore the original filter
    (set-process-filter proc original-filter)

    ;; restore the original timeout
    (if timeout-seconds
        (setq powershell-command-timeout-seconds old-timeout))

    ;; the result:
    powershell-command-reply))







(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))



(defun powershell-preoutput-filter-for-prompt (string)
  "Trim the newline from STRING, the prompt that we get back from powershell.  This fn is set into the preoutput filters, so the newline is trimmed before being put into the output buffer.
"
   (if
       (string-match powershell-prompt-regex  string)
       (substring string 0 -1) ;; remove newline
     string))


(defun powershell-simple-send (proc string)

  "Override of the comint-simple-send function, with logic
specifically designed for powershell.  This just sends STRING,
plus the prompt command.

When running as an inferior shell with stdin/stdout redirected,
powershell is in noninteractive mode. This means no prompts get
emitted when a PS command completes. This makes it difficult for
a comint mode to determine when the command has completed.
Therefore, we send an explicit request for the prompt, after
sending the actual (primary) command. When the primary command
completes, Powershell then responds to the \"prompt\" command,
and emits the prompt.

This insures we get and display the prompt.
"
  ;; Tell PowerShell to resize its virtual window, if necessary. We do
  ;; this by calling a resize function in the PowerShell, before sending
  ;; the user-entered command to the shell.
  ;;
  ;; Powershell keeps track of its \"console\", and formats its output
  ;; according to the width it thinks it is using.  This is true even when
  ;; powershell is invoked with the - argument, which tells it to use
  ;; stdin as input.

  ;; Therefore, if the user has resized the emacs window since the last
  ;; PowerShell command, we need to tell PowerShell to change the size
  ;; of its virtual window. Calling that function does not change the
  ;; size of a window that is visible on screen - it only changes the
  ;; size of the virtual window that PowerShell thinks it is using.  We
  ;; do that by invoking the PowerShell function that this module
  ;; defined for that purpose.
  ;;
  (if powershell--need-rawui-resize
      (progn
        (powershell--set-window-width proc)
        (setq powershell--need-rawui-resize nil)))
  (comint-simple-send proc (concat string "\n"))
  (comint-simple-send proc "prompt\n"))




;; Notes on TAB for completion.
;; -------------------------------------------------------
;; Emacs calls comint-dynamic-complete when the TAB key is pressed in a shell.
;; This is set up in shell-mode-map.
;;
;; comint-dynamic-complete calls the functions in  comint-dynamic-complete-functions,
;; until one of them returns non-nil.
;;
;; comint-dynamic-complete-functions is a good thing to set in the mode hook.
;;
;; The default value for that var in a powershell shell is:
;; (comint-replace-by-expanded-history
;;    shell-dynamic-complete-environment-variable
;;    shell-dynamic-complete-command
;;    shell-replace-by-expanded-directory
;;    comint-dynamic-complete-filename)



;; (defun powershell-dynamic-complete-command ()
;;   "Dynamically complete the command at point.
;; This function is similar to `comint-dynamic-complete-filename', except that it
;; searches the commands from powershell and then the `exec-path' (minus the
;; trailing Emacs library path)  for completion
;; candidates.

;; Completion is dependent on the value of `shell-completion-execonly', plus
;; those that effect file completion.  See `powershell-dynamic-complete-as-command'.

;; Returns t if successful."
;;   (interactive)
;;   (let ((filename (comint-match-partial-filename)))
;;     (if (and filename
;;              (save-match-data (not (string-match "[~/]" filename)))
;;              (eq (match-beginning 0)
;;                  (save-excursion (shell-backward-command 1) (point))))
;;         (prog2 (message "Completing command name...")
;;             (powershell-dynamic-complete-as-command)))))


;; (defun powershell-dynamic-complete-as-command ()
;;   "Dynamically complete at point as a command.
;; See `shell-dynamic-complete-filename'.  Returns t if successful."
;;   (let* ((filename (or (comint-match-partial-filename) ""))
;;          (filenondir (file-name-nondirectory filename))
;;          (path-dirs (cdr (reverse exec-path)))
;;          (cwd (file-name-as-directory (expand-file-name default-directory)))
;;          (ignored-extensions
;;           (and comint-completion-fignore
;;                (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
;;                           comint-completion-fignore "\\|")))
;;          (dir "") (comps-in-dir ())
;;          (file "") (abs-file-name "") (completions ()))

;;     ;; Go thru each cmd in powershell's lexicon, finding completions.

;;     ;; Go thru each dir in the search path, finding completions.
;;     (while path-dirs
;;       (setq dir (file-name-as-directory (comint-directory (or (car path-dirs) ".")))
;;             comps-in-dir (and (file-accessible-directory-p dir)
;;                               (file-name-all-completions filenondir dir)))
;;       ;; Go thru each completion found, to see whether it should be used.
;;       (while comps-in-dir
;;         (setq file (car comps-in-dir)
;;               abs-file-name (concat dir file))
;;         (if (and (not (member file completions))
;;                  (not (and ignored-extensions
;;                            (string-match ignored-extensions file)))
;;                  (or (string-equal dir cwd)
;;                      (not (file-directory-p abs-file-name)))
;;                  (or (null shell-completion-execonly)
;;                      (file-executable-p abs-file-name)))
;;             (setq completions (cons file completions)))
;;         (setq comps-in-dir (cdr comps-in-dir)))
;;       (setq path-dirs (cdr path-dirs)))
;;     ;; OK, we've got a list of completions.
;;     (let ((success (let ((comint-completion-addsuffix nil))
;;                      (comint-dynamic-simple-complete filenondir completions))))
;;       (if (and (memq success '(sole shortest)) comint-completion-addsuffix
;;                (not (file-directory-p (comint-match-partial-filename))))
;;           (insert " "))
;;       success)))


(provide 'powershell)

;; End of powershell.el

