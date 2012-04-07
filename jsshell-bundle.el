;;; jsshell-bundle.el -- JSShell generated bundle
;;
;; generated Fri Apr 06 23:45:02 2012
;;

;;; jsshell.el --- Run a javascript command interpreter in emacs on Windows.
;;
;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Created    : 26 March 2012
;; Modified   : March 2012
;; Version    : 2012.4.7
;; Keywords   : jScript javascript shell ms-windows
;; Copyright  : (c) 2012 Dino Chiesa
;; License    : New BSD
;; URL        : http://www.emacswiki.org/emacs/JSShell
;; Last-saved : <2012-April-06 23:44:06>
;;

;;; Commentary:

;; This is a Javascript REPL shell for emacs that depends on Windows and
;; its Cscript.exe host.

;; It was philosophically inspired by Paul Huff's js-comint.el .
;; The code here, though, is not related. It has been partially derived
;; from powershell.el, which is Copyright (C) 2008-2011 Dino Chiesa

;; To Use:
;;   Put jsshell-bundle.el in your load path
;;   Add (require 'jsshell-bundle) to your .emacs
;;   M-x jsshell
;;

;; There are a few customizations you may be interested in. Probably the
;; most interesting and useful is the `jsshell-profile', which is a list
;; of JS modules to load into every JSShell that starts.  You may want
;; to load in json2.js for example, or moment.js or some other library
;; or module. These modules are not supplied as part of jsshell.el.
;;


;;; More:
;;
;; -- Bundling --
;;
;; This module ships in two forms, "bundled" and "unbundled."  In
;; "unbundled" form, this module depends on on an external file named
;; jsshell.js, which provides the REPL. In bundled form, that code is
;; embedded in the elisp module. To determine if this is a bundled
;; version, scan to the bottom of the module and look for the assignment
;; to `jsshell-js-src'.  If the assignment is there, then this is the
;; bundled version. Usually the bundled version is called
;; jsshell-bundled.el while the core version is called jsshell.el.
;;
;;
;; -- Key mappings --
;;
;; Consider using these key mappnigs:
;;
;;    (local-set-key "\C-x\C-e" 'jsshell-send-last-sexp)
;;    (local-set-key "\C-\M-x"  'jsshell-send-last-sexp-and-pop)
;;    (local-set-key "\C-cb"    'jsshell-send-buffer)
;;    (local-set-key "\C-c\C-b" 'jsshell-send-buffer-and-pop)
;;    (local-set-key "\C-cl"    'jsshell-load-file-and-pop)
;;    (local-set-key "\C-c\C-e" 'jsshell-send-region)
;;
;;


;;; Revisions:
;;
;; 2012.4.7  2012-April-07  Dino Chiesa
;;    Added simple completion.
;;


;;; License:
;;
;; This code is distributed under the New BSD License.
;;
;; Copyright (c) 2008-2012, Dino Chiesa
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

;;; Bugs:

;; Literal strings typed into the shell need extra escaping.  This is
;; particularly a problem with regex strings. Eg, a \d in a JS regex gets
;; transformed into just a d when run through the jsshell.  I think this
;; is not a problem when loading files with `jsshell-load-file'. The
;; problem may not be worth fixing.
;;

;;; Code:

(require 'shell)
(eval-when-compile (require 'cl))  ;; lexical-let

(defcustom jsshell-location-of-cscript-exe
  ;; eg, "c:\\windows\\system32\\cscript.exe"
  (concat (getenv "WINDIR") "\\system32\\cscript.exe")
  "Path to the CSCRIPT.exe . You probably don't need to set this."
  :group 'jsshell)

(defcustom jsshell-profile
  (list "c:\\dev\\js\\json2.js")
  "List of filenames, each one a Javascript source file. This module
will load each JS module into the jsshell as the jsshell starts."
  :group 'jsshell)

(defgroup jsshell nil
  "Run a javascript process in a buffer."
  :group 'jsshell)

(defcustom jsshell-log-level 0
  "The current log level for jsshell internal operations.
0 = NONE, 1 = Info, 2 = VERBOSE, 3 = DEBUG. "
  :group 'jsshell)


;; The value of `jsshell-location-of-jsshell-js' is ignored when
;; run from within the bundle. The bundle includes the source code
;; for the JS, which is written out to a temporary file at runtime,
;; then used in the cscript.exe . This value is useful only when
;; in development of jsshell.js, or when using the unbundled
;; distribution.
(defvar jsshell-location-of-jsshell-js
  "c:\\dev\\js\\jsshell.js"
  "Path to the javascript REPL program")

(defvar jsshell--prompt-regex  "js> $"   "For internal use only")
(defvar jsshell--awaiting-command-prompt nil  "For internal use only. ")
(defvar jsshell--file-load-queue  nil  "For internal use only. ")
(defvar jsshell--load-path nil  "For internal use only. ")
(defvar jsshell-js-src nil  "For internal use only. ")
(defvar jsshell-js-tmpf nil  "For internal use only. ")
(defvar jsshell--silence nil "For internal use only. ")
(defvar jsshell--silent-output nil "For internal use only. ")

(defun jsshell-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `jsshell-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level jsshell-log-level)
      (let* ((msg (apply 'format text args)))
        (message "JSShell: %s" msg))))


;;;###autoload
(defun jsshell (&optional buffer)
  "Run an inferior Javascript shell, with I/O through tne named
BUFFER (which defaults to `*JSShell*').

Interactively, a prefix arg means to prompt for BUFFER.

If BUFFER exists but the shell process is not running, it makes a new shell.

If BUFFER exists and the shell process is running, just switch to BUFFER.

See the help for `shell' for more details.  \(Type
\\[describe-mode] in the shell buffer for a list of commands.)

The shell is CSCRIPT.exe (see `jsshell-location-of-cscript-exe'),
which runs a Javascript program that implements a
Read-Execute-Print Loop (REPL). This function looks for the
source for the REPL program first as a string stored in
`jsshell-js-src'. This variable will be non-nil when running the
shell from the bundle version of jsshell.  If that variable nil,
this function gets the source for the REPL from
`jsshell-location-of-jsshell-js'.

NB: Literal strings typed into the shell buffer need extra escaping.
This is espcially a problem with regex strings. eg, a \d in a JS
regex gets transformed into just a d when run through the
jsshell.  I think this is not a problem when loading files with
`jsshell-load-file'. The problem may not be worth fixing.

"
  (interactive
   (list
    (and current-prefix-arg
         (read-buffer "Shell buffer: "
                      (generate-new-buffer-name "*JSShell*")))))

  (setq buffer (get-buffer-create (or buffer "*JSShell*")))
  (jsshell--delete-old-tmp-js-files) ;; hygiene
  (let ((proc (get-buffer-process buffer)))
    (if (not proc)
        (progn
          (jsshell-log 1 "Javascript shell starting up...in buffer %s"
                       (buffer-name buffer))
          (let ((explicit-shell-file-name jsshell-location-of-cscript-exe)
            (explicit-cscript.exe-args
                  (list (cond
                         ((and jsshell-js-tmpf
                               (file-readable-p jsshell-js-tmpf))
                          jsshell-js-tmpf)

                         ((and jsshell-js-src
                               (progn
                                 (with-temp-file
                                    (setq jsshell-js-tmpf (make-temp-file "jsshell-" nil ".js"))
                                  (insert jsshell-js-src))
                                jsshell-js-tmpf)))

                         (t
                          jsshell-location-of-jsshell-js)))))

            (shell buffer))

          (setq proc (get-buffer-process buffer))

          (set (make-local-variable 'jsshell--awaiting-command-prompt) nil)
          (set (make-local-variable 'jsshell--silence) nil)
          (set (make-local-variable 'shell-dirtrack-mode) nil)
          (set (make-local-variable 'comint-prompt-read-only) t)
          (set (make-local-variable 'comint-input-sender-no-newline) nil)
          (set (make-local-variable 'jsshell--file-load-queue) nil)

          ;; fixup output hooks
          (make-local-variable 'comint-output-filter-functions)

          (add-hook 'comint-output-filter-functions
                    'jsshell--prompt-seeking-output-filter)

          (remove-hook 'comint-output-filter-functions
                       'comint-watch-for-password-prompt)

          (add-hook 'comint-preoutput-filter-functions
                    'jsshell--preoutput-filter-for-prompt)

          (add-hook 'comint-preoutput-filter-functions
                    'jsshell--maybe-enforce-silence)

          ;; for completion
          (set (make-local-variable 'comint-dynamic-complete-functions)
               '(comint-replace-by-expanded-history jsshell-complete-arg))


          ;; hook the kill-buffer action so we can kill the inferior process
          (add-hook 'kill-buffer-hook 'jsshell-delete-process)

          (accept-process-output proc)

          ;; profile
          (setq jsshell--input-recd-msg "")

          ;; record the list of files to be loaded...
          (mapc (lambda (filename)
                  (setq jsshell--file-load-queue
                        (cons filename jsshell--file-load-queue)))
                jsshell-profile)

          ;; not sure why lisp is ornery like this...
          (setq jsshell--file-load-queue (reverse jsshell--file-load-queue))

          ;; kick off the loading
          (comint-send-input t t)
          )

      ;; else, possibly pop to buffer?
      ))

  ;; return the buffer created
  buffer)


(defun jsshell--file-exists-and-is-old (file)
  "return t if the  file was modified witihn the last 24 hrs"
  (and (file-readable-p file)
       (let ((now (current-time))
             (filetime (nth 5 (file-attributes file)))
             s1 s2)
         (setq s1 (+ (* (nth 0 filetime) 65536)
                     (nth 1 filetime)))
         (setq s2 (+ (* (nth 0 now) 65536)
                     (nth 1 now)))
         (< 86400 (- s2 s1)))))

(defun jsshell--delete-old-tmp-js-files ()
  "remove old temporary Javascript source files, that
may exist from previous runs."
  (let ((spec (concat
               (file-name-as-directory temporary-file-directory)
               "jsshell-*.js")))
    (mapc (lambda (filename)
            (if (jsshell--file-exists-and-is-old filename)
                (delete-file filename)))
          (file-expand-wildcards spec))))


(defun jsshell-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))


(defvar jsshell--multiline-note-regex
      (concat
       ;;"^"
       (regexp-quote
        "Multi-line input. Use two consecutive blank lines to eval.")
       "\\(\x0A\\|\x0D\\)*$" )
    "Regexp for multiline note from JSShell. For internal use by jsshell.el")


(defvar jsshell--input-recd-msg ""
  "not documented.")


(defun jsshell--preoutput-filter-for-prompt (string)
  "Filter the 'Enter two consecutive blank lines...' from
the output, in some cases."
  (let ((old-msg jsshell--input-recd-msg))
    (if (and jsshell--awaiting-command-prompt
             (string-match jsshell--multiline-note-regex string))
        (progn
          (jsshell-log 4 "preoutput: seeking prompt and got multiline msg...(%s)" string)
          (setq jsshell--awaiting-command-prompt t
                jsshell--input-recd-msg "")
          (if (string= old-msg "")
              "\ninput received.\n"
            old-msg))

      string)))


(when (not (fboundp 'string/trim-trailing-newlines))
  (defun string/trim-trailing-newlines (string)
    (while (string-match "\\(.*\\)\\(\n\\|\r\\)$" string)
        (setq string (substring string 0 -1))) ;; remove newline
      string))


(defun jsshell--maybe-enforce-silence (string)
  "maybe suppress everything, if the buffer is in silent mode.
also maybe turn off silent mode if a prompt is recd."
  (let ((result ""))
    (if (not jsshell--silence) (setq result string)
      (when (string-match jsshell--prompt-regex string)
        (run-at-time 0.1 nil 'jsshell-silent-cmd-cb)
        (setq jsshell--silent-output
              (string/trim-trailing-newlines
               (substring string 0 (match-beginning 0)))
              jsshell--silence nil)))
    result))



(defun jsshell--prompt-seeking-output-filter (string)
  "This function is intended for use only internally to the jsshell
package.

It gets installed as a comint output filter upon initialization
of a jsshell. Its purpose is to negotiate the I/O protocol the
shell uses to manage multi-line input.

Normally, the Javascript REPL evals each line of input
independently. In some cases, such as with functions that are
defined over multiple lines, the user of the REPL wants to defer
eval until after all the input is ready.

For accepting multiline input, the Javascript REPL applies this
convention: one empty line signals the beginning of a multi-line
input block, and 2 empty lines signal the end of the block. This
multi-line input is what is used to load an entire JS file into
the shell, for example.

This filter function manages that protocol, keeping in mind the
state of this module, specifically whether there are additional
files waiting to be loaded.  When expecting a prompt after
loading a multi-line block, this filter send a nudge to the
shell (essentially a newline). When not expecting a prompt, and
there are files to be loaded, this filter loads the next file.

The nudging happens only when sending chunks of data via
`jsshell-send-region' or `jsshell-load-file', which are typically
bound to keyboard sequences.

When the shell is in actual interactive use - that is to say when
a person is typing input directly into the shell buffer - this
function does not get called. The human is expected to enter the
necessary double newlines at the appropriate times.

"
  (jsshell-log 3 "output filter (waiting? %s) (%s)"
               (prin1-to-string jsshell--awaiting-command-prompt)
               string)
  (cond

   (jsshell--awaiting-command-prompt
    (if (string= string "")
        (progn
          (jsshell-log 3 "empty output string...")
          ;;(comint-send-string (current-buffer) "")
          )
      (jsshell-log 3 "seek prompt")
      (let ((current (current-buffer)))
        (if (string-match jsshell--prompt-regex string)
            (progn
              (setq jsshell--awaiting-command-prompt nil
                    jsshell--input-recd-msg "")

              (jsshell-log 3 "the waiting is over")

              ;; more files to load?
              (if jsshell--file-load-queue
                  (progn
                    (jsshell-log 3 "next file")
                    ;;(comint-send-string current "") ;; newline?  two newlines? shit!
                    (jsshell--nudge)
                    )))

          (let ((proc (get-buffer-process current)))
            (if proc
                (progn
                  (jsshell-log 3 "no joy...")
                  (jsshell--nudge)
                  )))))))


   ;; not waiting. load a file, if available on the queue
   (jsshell--file-load-queue
    (let ((thisfile (car jsshell--file-load-queue)))
      (jsshell-log 3 "dequeue a file (%s)" thisfile)
      (setq jsshell--file-load-queue (cdr jsshell--file-load-queue))
      (jsshell-really-load-file thisfile (current-buffer))))

   (t
    (jsshell-log 3 "not waiting, no files to load"))))


(defun jsshell--nudge (&optional newline-count)
  "send a nudge to the shell in the current buffer"
  (if (and newline-count (> newline-count 0))
      (comint-send-string (current-buffer) (make-string newline-count ?\n)))
  (comint-send-input newline-count t))


(defun jsshell--squish-jscode (string)
  "Collapse double-newlines to single newlines.
Also strip newlines from the end of the code.
This is necessary because a double-newline tells the
JS REPL to stop parsing and evaluate. This is not
what we want."
  (while (string-match "\n\n" string)
    (setq string (replace-match "\n" nil nil string)))
  (while (string-match "^\n" string)
    (setq string (replace-match "" nil nil string)))
  (while (string-match "\n$" string)
    (setq string (replace-match "" nil nil string)))
  string)


(defun jsshell--enter-multiline-mode (buffer)
  "tell the shell to expect multiple lines of content.
Terminated by two successive newlines. "
  ;; in the shell buffer, set the local var that tells
  ;; this module to seek the prompt.
  (with-current-buffer buffer
    (jsshell-log 1 "enter multiline input mode: %s (%s)"
                           (buffer-name buffer)
                           jsshell--awaiting-command-prompt)
    (setq jsshell--awaiting-command-prompt t))
  (comint-send-string buffer "\n"))



(defun jsshell-send-cmd-silently (cmd fn &optional buffer)
  "Sends the command CMD to the shell without echoing input or output.

When output is available, FN will be invoked with one argument:
the output, a string.

The main purpose of this fn is to allow silent commands to
interrogate the JS environment, in support of completion.

"
  (let ((buffer (jsshell buffer))
        (comint-input-sender-no-newline nil))
    (with-current-buffer buffer
      (setq jsshell--silence t
            jsshell-silent-output-fn fn))
    (jsshell-log 2 "Silent cmd %s" cmd)
    (funcall comint-input-sender (get-buffer-process buffer) cmd)))


(defun jsshell-silent-cmd-cb ()
  "the fn invoked when the output of the silent cmd is ready.
It is available in `jsshell--silent-output'."
  (when jsshell-silent-output-fn
    (jsshell-log 2 "silent output: %s" jsshell--silent-output)
    (lexical-let ((s jsshell--silent-output) ;; closure
                  (f jsshell-silent-output-fn))
      (run-at-time 0.01 nil f s))
    (setq jsshell-silent-output-fn  nil)))



(defun jsshell-get-js-get-prop-list (identifier)
  "Returns a string containing Javascript code that, when
invoked, returns the properties of the object in the JS Shell
referred to as IDENTIFIER.

This is used to support completion in the shell.

"
  (concat "(function(){var r=[],m="
        identifier
        "; for(var p in m){r.push(p);}return r;}());"))



(defun jsshell-get-menu-position ()
  "get the position for the popup menu"
  (if (fboundp 'posn-at-point)
      (let ((x-y (posn-x-y (posn-at-point (point)))))
        (list (list (+ (car x-y) 10)
                    (+ (cdr x-y) 20))
              (selected-window)))
    t))


;; Emacs Lisp doesn’t come with a ‘filter’ function to keep elements that satisfy
;; a conditional and excise the elements that do not satisfy it. One can use ‘mapcar’
;; to iterate over a list with a conditional, and then use ‘delq’ to remove the ‘nil’
;; values.

(defun jsshell-filter (pred lst)
    (delq nil
           (mapcar (lambda (x) (and (funcall pred x) x)) lst)))


(when (not (fboundp 'string/starts-with))
  (defun string/starts-with (s arg)
    "returns t if string S starts with ARG.  Else nil."
    (cond ((>= (length s) (length arg))
           (string-equal (substring s 0 (length arg)) arg))
          (t nil))))


(defun jsshell--generate-menu (candidates stub)
  "Generate a menu suitable for use in `x-popup-dialog' from the
list of CANDIDATES. Each item in the list of candidates is a
string.  If STUB is non-nil, then include a candidate only if it
begins with STUB.

"
    (list "Complete with..."
          (cons "Ignored pane title"
                (mapcar '(lambda (elt)
                           (if stub
                               (and (string/starts-with elt stub)
                                    (cons elt elt))
                             (cons elt elt)))

                        candidates))))


(defun jsshell-dyn-jsfunc-snippet (s)
  "Dynamically produce a snippet for a js function arg list,
and expand it. "
  (when (and (fboundp 'yas/expand-snippet)
             (string-match "function *(\\([^)]+\\)) *{" s))
    (let ((c 0))
      (yas/expand-snippet
       (concat  "("
                (mapconcat '(lambda (elt)
                              (setq c (1+ c))
                              (concat "${" (number-to-string c)
                                      ":" elt "}"))
                           (split-string (match-string 1 s) ", " t)
                           ", ")
                ")")))))


(defun jsshell-expand-jsfunc (obj func)
  "Dynamically define a ya-snippet and expand it, for the
given function. "

  (when (fboundp 'yas/expand-snippet)
    (jsshell-send-cmd-silently
     (concat obj "." func)
     'jsshell-dyn-jsfunc-snippet)))



;; This function makes a function. It uses lexical-let because
;; it is invoked asynchronously, which means the arguments
;; will be out of scope by the time the function is invoked.
(defun jsshell-funcmaker1 (a b)
    (lexical-let ((a a) (b b)) #'(lambda (s)
                                   (when (string= s "function")
                                        (jsshell-expand-jsfunc a b)))))


(defun jsshell-choose-completion (str identifier stub)
  "Present possible completions."

  (let ((choice
         (x-popup-menu (jsshell-get-menu-position)
                       (jsshell--generate-menu (split-string str "," t) stub))))

    (when choice
      (when stub
        (backward-delete-char (length stub)))
      (insert choice)
      ;; possibly do a ya-snippet expansion on functions

      (when (fboundp 'yas/expand-snippet)
        (jsshell-send-cmd-silently (concat "typeof "
                                           identifier
                                           "."
                                           choice)
                                   (jsshell-funcmaker1 identifier choice))))))



;; This function makes a function. It uses lexical-let because
;; it is invoked asynchronously, which means the arguments
;; will be out of scope by the time the function is invoked.
(defun jsshell-funcmaker2 (a b)
  (lexical-let ((a a) (b b)) #'(lambda (s)
                                 (jsshell-choose-completion s a b))))



(defun jsshell-complete-arg ()
  "Do completion on the thing at point in the JSShell.

The normal way to do these completions is to cycle through or
present a dropdown.

Returns t if successful.
"
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\([_A-Za-z][_0-9A-Za-z\\.()]*\\)\\.\\([_A-Za-z][_0-9A-Za-z]*\\)?$"))
    (let ((identifier (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1)))
          (stub (and (match-beginning 2)
                      (buffer-substring-no-properties
                       (match-beginning 2)
                       (match-end 2)))))
      (jsshell-send-cmd-silently (jsshell-get-js-get-prop-list identifier)
                                 (jsshell-funcmaker2 identifier stub)))
    t))


;;;###autoload
(defun jsshell-send-region (start end &optional buffer)
  "Send the current region to the inferior Javascript process."
  (interactive "r")
  (let ((buffer (jsshell buffer))
        (jscode (jsshell--squish-jscode
                 (buffer-substring-no-properties start end))))

    (jsshell--enter-multiline-mode buffer)
    (comint-simple-send (get-buffer-process buffer)
                        jscode)))


;;;###autoload
(defun jsshell-send-region-and-pop (start end &optional buffer)
  "Send the contents of the current region to the inferior
Javascript shell."
  (interactive "r")
  (let ((buffer (jsshell buffer)))
    (jsshell-send-region start end buffer)
    (jsshell-switch-to-shell buffer)))

;;;###autoload
(defun jsshell-send-last-sexp-and-pop ()
  "Send the previous sexp to the inferior Js process, and pop to the buffer."
  (interactive)
  (jsshell-send-region-and-pop (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun jsshell-send-last-sexp ()
  "Send the previous sexp to the inferior Javascript process."
  (interactive)
  (jsshell-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun jsshell-send-buffer ()
  "Send the entire contents of the current buffer to
the inferior Javascript shell."
  (interactive)
  (jsshell-send-region (point-min) (point-max)))

;;;###autoload
(defun jsshell-send-buffer-and-pop ()
  "Send the buffer to the inferior Javascript process."
  (interactive)
  (jsshell-send-region-and-pop (point-min) (point-max)))

;;;###autoload
(defun jsshell-load-file (filename &optional buffer)
  "Enqueue the file named FILENAME to be loaded into the JSSHell
interpreter designated by BUFFER, or *JSShell* if no buffer supplied."
  (interactive "fjs file to load: ")
  (if (file-readable-p filename)
      (let ((buffer (jsshell buffer)))
        (with-current-buffer buffer
          (setq jsshell--file-load-queue
                (cons filename jsshell--file-load-queue))
          (comint-send-input t t)))
    (error "That file is not readable.")))


(defun jsshell-really-load-file (filename &optional buffer)
  "Load the file named FILENAME into the JSSHell interpreter designated by
BUFFER, or *JSShell* if no buffer supplied."
  (jsshell-log 2 "really load file (%s)" filename)
  (let ((jscode
         (if (file-readable-p filename)
             (jsshell--squish-jscode
              (with-temp-buffer
                (insert-file-contents filename)
                (buffer-substring-no-properties (point-min) (point-max))))
           ";"))

        (buffer (jsshell buffer)))

    (with-current-buffer buffer
      (jsshell--enter-multiline-mode buffer)
      (setq jsshell--input-recd-msg
            (format
             (if (file-readable-p filename)
                 "loading %s"
               "the file %s does not exist.") filename))
      (comint-simple-send (get-buffer-process buffer) jscode)
      (jsshell--nudge)
      (sleep-for 0.32))))

;;;###autoload
(defun jsshell-load-file-and-pop (filename &optional buffer)
  "Load the file named FILENAME into the JSSHell interpreter designated by
BUFFER, or *JSShell* if no buffer supplied. The pop to that buffer."
  (interactive "fjs file to load: ")
  (let ((buffer (jsshell buffer)))
    (jsshell-load-file filename buffer)
    (jsshell-switch-to-shell buffer)))


;;;###autoload
(defun jsshell-switch-to-shell (&optional buffer move-to-eob)
  "Switch to the javascript process buffer.
With non-nil MOVE-TO-EOB argument, position cursor at end of buffer."
  (interactive "P")
  (let ((buffer (jsshell buffer)))
    (if (not buffer)
        (error "Cannot find *JSShell* buffer.")
      (jsshell-log 2 "Switch to buffer '%s'" (buffer-name buffer))
      (pop-to-buffer buffer)
      (when move-to-eob
        (push-mark)
        (goto-char (point-max))))))


(defun jsshell--minimized-js-contents (file)
  "returns the minimized JS version of the contents of
the specified FILE."
  (and (file-readable-p file)
       (let ((re-pairs '(("[\s\t]*//.*$" "") ;; javascript comments
                         ("^\n" "")          ;; eliminate leading newlines
                         ("\n[\s\t]+" "\n")  ;; collapse whitespace
                         ("\n\n" "\n")       ;; collapse newlines
                         ("\n}" "}")
                         ("{\n" "{")
                         ("}\n" "}")
                         (";\n" ";")         ;; replace semi-newline with semi
                         ("\n$" ""))))       ;; trailing newline
         (with-temp-buffer
           (insert-file-contents file)
           (mapc (lambda (pair)
                   (goto-char (point-min))
                   (while (re-search-forward (car pair) nil t)
                     (replace-match (cadr pair) nil nil)))
                 re-pairs)
           (buffer-substring-no-properties (point-min) (point-max))))))


(defun jsshell-produce-bundle (&optional jsshell-el bundle-el jsshell-js)
  "Produce a new .el file, which contains all the jsshell.el
function and also embeds the jsshell.js source as a string. The
resulting .el file will then be suitable for a one-file
distribution of JSShell.

JSShell depends on two pieces: jsshell.el and jsshell.js. Rather
than distributing and installing two distinct files, the bundle
embeds the .js file into the .el file, for a one-file
distribution option. This function produces that one file.

Most people will never need to use this function. It's useful only
after modifying either the original jsshell.el or the jsshell.js file,
when you want to produce a new distributable bundle. In other words, it's
useful for the developer of jsshell.el.

"
  (let ((jsshell-el (or jsshell-el
                        (concat (file-name-directory jsshell--load-path) "jsshell.el")
                        "jsshell.el")))
    (let ((bundle-el  (or bundle-el
                          (concat (file-name-directory jsshell-el) "jsshell-bundle.el")))
          (jsshell-js (or jsshell-js
                          (and jsshell-js-tmpf
                               (file-readable-p jsshell-js-tmpf)
                               jsshell-js-tmpf)
                          jsshell-location-of-jsshell-js ;; orig dev wkstation
                          (concat (file-name-directory jsshell-el) "jsshell.js"))))
      (with-temp-file bundle-el
        (insert (concat
                 ";;; "
                 (file-name-nondirectory bundle-el)
                 " -- JSShell generated bundle\n"))
        (insert (concat ";;\n;; generated " (current-time-string) "\n;;\n\n"))
        (insert-file-contents jsshell-el)
        (goto-char (point-max))
        (insert "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
        (insert ";; this is the embedded Javascript code for the JS REPL\n\n")
        (goto-char (point-max))
        (insert (concat "(setq jsshell-js-src "
                        (pp-to-string (jsshell--minimized-js-contents jsshell-js))
                        ")\n"
                        "\n(provide '"
                        (file-name-sans-extension (file-name-nondirectory bundle-el))
                        ")\n"
                        "\n;;; "
                        (file-name-nondirectory bundle-el)
                        " ends here\n"))))))


(defun jsshell-produce-new-bundle ()
  "Produces a new bundle with the default settings."
   (jsshell-produce-bundle "~/jsshell.el"
                         "~/jsshell-bundle.el"
                         jsshell-location-of-jsshell-js))

;; (jsshell-produce-new-bundle)

;; remember load time path
(setq jsshell--load-path load-file-name)

(provide 'jsshell)

;;; jsshell.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the embedded Javascript code for the JS REPL

(setq jsshell-js-src "/*  jslint wsh: true */\n(function(globalScope) {'use strict';var line, trimline, scriptText, previousLine, result, count = 0;if (typeof String.prototype.trim != 'function') {String.prototype.trim = function() {return this.replace(/^\\s\\s*/, '').replace(/\\s\\s*$/, '');};}function scriptEngineInfo() {var s = ScriptEngine() + \" Version \" +\nScriptEngineMajorVersion() + \".\" +\nScriptEngineMinorVersion() + \".\" +\nScriptEngineBuildVersion();return(s);}function numberToHexString (n) {if (n >= 0) {return n.toString(16);}else {n += 0x100000000;return n.toString(16);}}function gshell() {while(true) {if (count === 0) {WScript.StdOut.WriteLine(\"Welcome to the JScript shell.\\n\" +\n\"Running \" + scriptEngineInfo() +\n\"\\n'exit' to exit.\\n\");}count++;WScript.StdOut.Write(\"js> \");if (WScript.StdIn.AtEndOfStream) {WScript.Echo(\"Bye.\");break;}line = WScript.StdIn.ReadLine();scriptText = line + \"\\n\";if (line === \"\") {WScript.Echo(\n\"Multi-line input. Use two consecutive blank lines to eval.\");do {if (WScript.StdIn.AtEndOfStream) {break;}previousLine = line;line = WScript.StdIn.ReadLine();line += \"\\n\";scriptText += line;} while(previousLine != \"\\n\" || line != \"\\n\");}trimline = scriptText.trim();if (trimline == \"exit\" || trimline == \"quit\") {WScript.Quit(0);}try {result = eval(scriptText);}catch (error) {WScript.Echo(\"0x\" + numberToHexString(error.number) + \" \" + error.name + \": \" +\nerror.message);}if (result) {try {WScript.Echo(result);}catch (e) {WScript.Echo(\"<<>>\");}}result = null;}}gshell();}(this));")

(provide 'jsshell-bundle)

;;; jsshell-bundle.el ends here
