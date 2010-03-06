[[de:cQnldvrRirlNHG]]
;;; exec-abbrev-cmd.el --- Execute commands by giving an abbreviation

;; Copyright 2007 Tassilo Horn
;;
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Homepage: http://www.tsdh.de
;; Contributors: Levin <zslevin@gmail.com>

;; This  program is free  software; you  can redistribute  it and/or  modify it
;; under the terms  of the GNU General Public License as  published by the Free
;; Software  Foundation;  either version  3,  or  (at  your option)  any  later
;; version.
;;
;; This program is distributed in the  hope that it will be useful, but WITHOUT
;; ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
;; FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to  the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file  includes the command  `exec-abbrev-cmd' which lets you  execute a
;; command by  giving it in an  abbreviated form, where  the abbreviation takes
;; the first character of each word in the command name.  For example "g" is an
;; abbreviation  for   the  command  `gnus',   "eb"  is  an   abbreviation  for
;; `emms-browser' and "omm" is an abbreviation for `outline-minor-mode'.  If an
;; abbrev contains hyphens the number of  words in the command match the number
;; of hyphens  plus one.   So the  abbrev "o-m-m" is  equivalent to  the abbrev
;; "omm".   Of course  it is  possible,  that an  abbreviation matches  several
;; commands, e.g. "g" matches not only  `gnus' but `grep', `gdb' and some more.
;; In such cases you will be queried, which command to use.
;;
;; When you hit TAB on the exec-abbrev-cmd prompt, you'll fall back to the
;; normal behavior of `execute-extended-command' with the current minibuffer
;; contents as initial input.
;;
;; To have this  functionality quickly accessible you might want  to bind it to
;; some key.  That's what I use:
;;
;;     (add-to-list 'load-path "~/elisp") ;; Where is exec-abbrev-cmd.el?
;;     (require 'exec-abbrev-cmd)         ;; Load it.
;;     (global-set-key (kbd "C-x x") 'exec-abbrev-cmd)
;;     ;; Since you can fall back to normal `execute-extended-command' behavior
;;     ;; with TAB, its perfectly ok to bind `exec-abbrev-cmd' to `M-x'.
;;     ;; (global-set-key (kbd "M-x") 'exec-abbrev-cmd)
;;
;; Now you'll say,  "Wow, what a nice feature!", but  it's even getting better.
;; Let's say  you often invoke  `customize-face' with `C-x  x cf RET'  and then
;; choosing from the completion  list between `copy-file' and `customize-face'.
;; Always `copy-file' is selected  first, because it's lexicographically before
;; `customize-face'.     As    a    solution    to   this    problem    there's
;; `exec-abbrev-cmd-mode', a global minor  mode that does bookkeeping how often
;; you invoke  a command with `exec-abbrev-cmd',  so that the  list of commands
;; you have  to choose from is  sorted by the frequency  of command invokation.
;; After a while in most cases `C-x x <abbrev> RET RET' will do what you want.
;;
;; If you want to enable this feature put this in your ~/.emacs:
;;
;;     (exec-abbrev-cmd-mode 1)
;;
;; Have fun!

;;; Notes:

;; You should have a look  at `partial-completion-mode' which comes with emacs,
;; too. I like this  mode better because `partial-completion-mode' doesn't play
;; well with `ido-mode', but your mileage may vary.

;;; Version:

;; <2007-10-05 Fri 09:58>

;;; Code:

(require 'cl)  ;; for `remove-if-not' and `push'

(defvar exec-abbrev-cmd-file "~/.emacs.d/exec-abbrev-cmd.el"
  "The file where `exec-abbrev-cmd-alist' will be saved.")

(defvar exec-abbrev-cmd-alist
  (with-temp-buffer
    (if (not (file-exists-p exec-abbrev-cmd-file))
        nil
      (insert-file-contents exec-abbrev-cmd-file)
      (read (current-buffer))))
  "An alist with items of the form (COMMAND . NO-OF-EXECUTIONS)
that will be used to sort the possible completions of
`exec-abbrev-cmd' so that most frequently commands come first.")

(defun exec-abbrev-cmd-record (command)
  "Record the execution of COMMAND to `exec-abbrev-cmd-alist'."
  (let ((pair (assq command exec-abbrev-cmd-alist)))
    (if (not pair)
        ;; Command was never used till now
        (push (cons command 1) exec-abbrev-cmd-alist)
      ;; Ok, it's in, so we need to increase NO-OF-EXECUTIONS.
      (setq exec-abbrev-cmd-alist
            (cons (cons command (1+ (cdr pair)))
                  (delete pair exec-abbrev-cmd-alist))))))

(defun exec-abbrev-cmd-save ()
  "Save `exec-abbrev-cmd-alist' to `exec-abbrev-cmd-file'."
  (with-temp-buffer
    (print exec-abbrev-cmd-alist (current-buffer))
    (write-file exec-abbrev-cmd-file)))

(defun exec-abbrev-cmd-more-frequently-used-p (c1 c2)
  "Return t if C1 should be sorted before C2.
That is if C1 was more frequently used than C2 or both were used
equally often."
  (>= (or (cdr (assq c1 exec-abbrev-cmd-alist)) 0)
      (or (cdr (assq c2 exec-abbrev-cmd-alist)) 0)))

(defun exec-abbrev-cmd-sort (commands)
  "Return the sorted list of COMMANDS.
Each command in COMMANDS is a string, not a symbol."
  (mapcar 'symbol-name
          (sort (mapcar 'intern commands)
                'exec-abbrev-cmd-more-frequently-used-p)))

;;;###autoload
(defun exec-abbrev-cmd (prefixarg)
  "Query for a command abbrev like \"mbm\" and execute the matching command.

If the list of matching commands has only one item, this command
will be executed directly.  If there a more choices, the user
will be queried which one to call.

The PREFIXARG is passed on to the invoked command.

With TAB on the input prompt fall back to normal
`execute-extended-command' behavior."
  (interactive "P")
  (let ((catch-val
         (catch 'escape-from-exec-abbrev-cmd
           (let* ((abbrev
                   (read-from-minibuffer
                    "Command Abbrev: "
                    nil
                    (let ((map (copy-keymap minibuffer-local-map)))
                      (define-key map "\C-i"
                        (lambda ()
                          (interactive)
                          (throw 'escape-from-exec-abbrev-cmd
                                 (minibuffer-contents))))
                      map)))
                  (regexp
                   (concat "^"
                           (let ((part-list (split-string abbrev "-")))
                             (if (= 1 (length part-list))
                                 ;; abc => a*-b*-c*
                                 (mapconcat #'list abbrev "[^-]*-")
                               ;; ahead-b-c => ahead*-b*-c*
                               (mapconcat #'identity part-list "[^-]*-")))
                           "[^-]*$"))
                  (commands
                   (exec-abbrev-cmd-sort
                    (remove-if-not (lambda (string)
                                     (string-match regexp string))
                                   (let (c)
                                     (mapatoms
                                      (lambda (a)
                                        (if (commandp a)
                                            (push (symbol-name a) c))))
                                     c)))))
             (if (not commands)
                 (message "No such command.")
               (let ((c (cond ((> (length commands) 1)
                               (intern
                                (if (and (featurep 'ido) ido-mode)
                                    ;; ido is available and enabled, so use it.
                                    (ido-completing-read "Command: " commands)
                                  ;; fallback to normal completion with the
                                  ;; most frequently used command as default.
                                  (completing-read
                                   (concat "Command (defaults to `"
                                           (car commands) "'): ")
                                   commands
                                   nil t nil nil (car commands)))))
                              (t (intern (car commands))))))
                 (call-interactively c t)
                 (when exec-abbrev-cmd-mode
                   (exec-abbrev-cmd-record c)))))
           ;; Always return nil so that an escape with TAB is the only thing
           ;; that makes `catch-val' non-nil.
           nil)))
    (when catch-val
      (let ((command (completing-read "M-x "
                                      (let (commands)
                                        (mapatoms (lambda (a)
                                                    (when (commandp a)
                                                      (push (symbol-name a)
                                                            commands))))
                                        commands)
                                      nil t catch-val)))
        (command-execute (intern command) t nil t)))))

(define-minor-mode exec-abbrev-cmd-mode
  "If enabled bookkeeping of command executions with
`exec-abbrev-cmd-execute' will be done, so that the list of
possible command completions is sorted by frequency of
invokations."
  :init-value nil
  :global t
  (if exec-abbrev-cmd-mode

      ;; Toggled on!
      (add-hook 'kill-emacs-hook 'exec-abbrev-cmd-save)

    ;; Toggled off!
    (remove-hook 'kill-emacs-hook 'exec-abbrev-cmd-save)))

(provide 'exec-abbrev-cmd)

;; Local Variables:
;; mode: outline-minor
;; end:

;;; exec-abbrev-cmd.el ends here
