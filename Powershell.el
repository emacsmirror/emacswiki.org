;;; powershell.el, version 0.1
;;
;; Author: Dino Chiesa
;; Thu, 10 Apr 2008  11:10
;;
;; Copyright (c) 2008-2010, Dino Chiesa
;;
;;
;; Run Windows PowerShell v1.0 as an inferior shell within emacs. Tested with emacs v22.2.
;; This is NOT a powershell editing mode.  
;;
;; TODO:
;;
;; - test what happens when you expand the window size beyond the maxWindowWidth for the RawUI
;;   make everything configurable (Powershell exe, initial args, powershell prompt regexp)
;;   implement powershell launch hooks
;;
;; - prevent backspace from deleting the powershell prompt? (do other shells do this?)
;;   maybe see comit-prompt-read-only in comint.el
;;
;; - get TAB to do proper completion for powershell commands, filenames, etc
;;
;; Licensed under MS Public License
;;    http://www.opensource.org/licenses/ms-pl.html
;;



(require 'shell)

;; apparently the max WindowSize.Width for powershell is 210 
(defun powershell-gen-window-width-string ()
  (concat  "$a = (Get-Host).UI.RawUI\n" 
            "$b = $a.WindowSize\n"
            "$c = $a.WindowSize\n"
            ;;"$b.Width = 19999\n"
            "$b.width = " (number-to-string  (min 210 (window-width))) "\n"
            "$c.width = " (number-to-string  (min 210 (window-width))) "\n"
            "$a.BufferSize = $b\n"
            "$a.WindowSize = $c")
  )


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



(defvar powershell-prompt-pattern  "PS [^#$%>]+>" 
  "Regexp for powershell prompt.  This isn't really used, because I couldn't figure out how to get it to work."
  )

(defgroup powershell nil
  "Running shell from within Emacs buffers."
  :group 'processes
  )


(defcustom powershell-need-rawui-resize t
  "set when powershell needs to be resized"
  :group 'powershell
)

;;;###autoload
(defun powershell (&optional buffer)
  "Run Powershell, by invoking the shell function. See the help for shell for more details.
\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*PowerShell*")))))
  ;; get a name for the buffer
  (setq buffer (get-buffer-create (or buffer "*PowerShell*")))

  (let (
        (tmp-shellfile explicit-shell-file-name)
        )
    ;; set arguments for the powershell exe.
    ;; This needs to be tunable.
    (setq explicit-shell-file-name "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe")  
    (setq explicit-powershell.exe-args '("-Command" "-" )) ; interactive, but no command prompt
  
    ;; launch the shell
    (shell buffer)

    ; restore the original shell
    (if explicit-shell-file-name
        (setq explicit-shell-file-name tmp-shellfile)
      )
    )

  
  (let (
        (proc (get-buffer-process buffer))
        )
    
    ;; This sets up the powershell RawUI screen width. By default,
    ;; the powershell v1.0 assumes terminal width of 80 chars.
    ;; This means input gets wrapped at the 80th column.  Here, we reset the
    ;; width of the PS terminal to the window width. 
    (add-hook 'window-size-change-functions 'powershell-window-size-changed)
    (powershell-window-size-changed)
    
    
    ;; ask for initial prompt
    (comint-simple-send proc "prompt")
    )

  ;; hook the kill-buffer action so we can kill the inferior process?
  (add-hook 'kill-buffer-hook 'powershell-delete-process)

  ;; wrap the comint-input-sender with a PS version
  ;; must do this after launching the shell! 
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'powershell-simple-send)

  ;; set a preoutput filter for powershell.  This will trim newlines after the prompt.
  (add-hook 'comint-preoutput-filter-functions 'powershell-preoutput-filter-for-prompt)

  ;; why did I comment this out
  ;;(run-hooks 'powershell-launch-hook)

  ;; return the buffer created
  buffer
)


(defun powershell-window-size-changed (&optional frame)
  ;; do not actually resize here. instead just set a flag.
  ;; actual resize will happen.... elsewhere.
  (setq powershell-need-rawui-resize t)
)



(defun powershell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc))
  )



;; This function trims the newline from the prompt that we
;; get back from powershell.  It is set into the preoutput
;; filters, so the newline is trimmed before being put into
;; the output buffer.
(defun powershell-preoutput-filter-for-prompt (string)
   (if
       ;; not sure why, but I have not succeeded in using a variable here???  
       ;;(string-match  powershell-prompt-pattern  string)

       (string-match  "PS [^#$%>]+>" string)
       (substring string 0 -1)
     
     string

     )
   )



(defun powershell-simple-send (proc string)
  "Override of the comint-simple-send function, specific for powershell.
This just sends STRING, plus the prompt command. Normally powershell is in
noninteractive model when running as an inferior shell with stdin/stdout
redirected, which is the case when running as a shell within emacs.
This function insures we get and display the prompt. "
  ;; resize if necessary. We do this by sending a resize string to the shell,
  ;; before sending the actual command to the shell. 
  (if powershell-need-rawui-resize
      (and
       (comint-simple-send proc (powershell-gen-window-width-string))
       (setq powershell-need-rawui-resize nil)
       )
    )
  (comint-simple-send proc string)
  (comint-simple-send proc "prompt")
)


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

