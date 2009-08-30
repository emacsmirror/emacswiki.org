;;; perl-completion.el - minor mode provides useful features for editing perl codes

;; Copyright (c) 2009 by KAYAC Inc.

;; Author: IMAKADO <ken.imakado@gmail.com>
;; Keywords: perl

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

;; Prefix: plcmp-

;;; Commentary:
;; Tested on Emacs 22
;;
;; Thanks to rubikitch for integration with anything-show-completion.


;;; Commands:
;;
;; Below are complete command list:
;;
;; -Completion Commands-
;;
;; [C-c C-c a] `plcmp-cmd-complete-all'
;;       invoke anything with all completion sources.
;;       difference between this command and `plcmp-cmd-smart-complete'
;;       is this command doesn't sort candidates smartly.

;; [C-RET] `plcmp-cmd-smart-complete'
;; [<C-return>] `plcmp-cmd-smart-complete'
;; [M-TAB] `plcmp-cmd-smart-complete'
;;       complete all completion sources.
;;       completions are smartly sorted.

;;       e.x,
;;       when invoke command context like: (cursor is `!!')

;;      use LWP::UserAgent;
;;      my $ua = LWP::UserAgent->new;
;;      $ua->`!!'

;;       LWP::UserAgent's methods are appered at top of candidates.

;;       this is the main command of perl-completion.el!!.

;; [C-c v] `plcmp-cmd-complete-variables'
;;       invoke anything with only variables completion source.

;; [C-c a] `plcmp-cmd-complete-arrays'
;;       invoke anything with only array completion source.

;; [C-c h] `plcmp-cmd-complete-hashes'
;;       invoke anything with only hash completion source.

;; [C-c f] `plcmp-cmd-complete-functions'
;;       invoke anything with only function completion source.

;; [C-c m] `plcmp-cmd-complete-methods'
;;       invoke anything with only using module's methods completion source.

;; [C-c i] `plcmp-cmd-complete-modules'
;;       invoke anything with only installed modules completion source.


;; -Documents support commands-

;; [C-c d] `plcmp-cmd-show-doc'
;;       invoke anything with installed modules and man pages about perl.

;;       default action is "Show doc" that open selected condidate's document.

;;       e.x,
;;       [module]
;;       If selected DBIx::Class::ResultSet, open perldoc.
;;       [manpage]
;;       If selected perlop, open manpage using `woman'

;;       press TAB to select other Actions.


;; [C-c s] `plcmp-cmd-show-doc-at-point'
;;       same as `plcmp-cmd-show-doc'.
;;       only difference, word at point is used as initial-pattern.


;; -Other Commands-

;; [C-c c] `plcmp-cmd-clear-all-caches'
;;       remove all caches
;;       rebuild installed modules list asynchronously. as soon as called this command.
;;       caches are,
;;       - installed modules
;;       - methods
;;       - perl buffers last modified time. (used detect buffer is changed since last completion command is invoked
;;                                           other-perl-buffer-source doesn't try to get completions non modified buffer).

;;       this command usually used, when you install new module from something like CPAN after once perl-completion-mode is on.

;;       Internally, this command sets these variables to `nil'.
;;       -`plcmp-other-perl-buffers-cache-hash'
;;       -`plcmp-module-methods-alist'
;;       -`plcmp-installed-modules'


;; [C-c C-c s] `plcmp-cmd-show-environment'
;;       print infomations to *perl-completion show environment* buffer
;;       environment,
;;       perl-completion's customize variables,
;;       perl-completion's commands and where bound to and value of environment PERL5LIB.

;;       this command is good for debugging.

;; [C-c M] `plcmp-cmd-menu'
;;       show perl-completion's menu.

;;       this quite useful when before you remember perl-completion's keybinds.


;; -Commands support perl programing-

;; `plcmp-cmd-eval-buffer'
;;       Run current buffer string as Perl code asynchronously.
;;       when finished, popup result buffer.
;;       the result buffer is named *perl output*.

;; `plcmp-cmd-eval-on-region'
;;       Run selected region as Perl code asynchronously.
;;       when finished, popup result buffer.
;;       the result buffer is named *perl output*.

;;       If run perl process is exit with nonzero status, using `switch-to-buffer' instead of `pop-to-buffer'


;; Customize Variables

;; `plcmp-lib-directory-re'
;;      regexp to detect directory that automatically added to PERL5LIB when build completions.

;;      e.x,
;;      If you are editting file at "~/dev/SomeModule/lib/SomeModule/Hoge.pm",
;;      "~/dev/SomeModule/lib/" is added to PERL5LIB when invoke completion commands.

;;      default value is "lib/"

;; `plcmp-use-keymap'
;;      If this value is nil,
;;      perl-completion-mode doesn't use own key-map.

;;      it is useful if you want to use own keybind,
;;      or don't like default keybinds.

;;      Note, keymap is defined at library loading phase.
;;      so this variables's value should be set before `require'.

;;      e.x,
;;      ;; ok, dont use default keybinds.
;;      (setq plcmp-use-keymap nil)
;;      (require 'perl-completion)


;;      ;; NG when `plcmp-use-keymap' is set, plcmp-mode-map is defined already.
;;      (require 'perl-completion)
;;      (setq plcmp-use-keymap nil)



;; `plcmp-extra-using-modules'
;;      list of String or Alist

;;      If value is String(module name),
;;      module name is always appear in candidates.

;;      If value is Alist ("module-name" . "extra-module-name"),
;;      and module-name is using in current buffer,
;;      extra-module-name's methods are appear in candidates.

;;      e.x,
;;      (setq plcmp-extra-using-modules '("DBIx::Class::ResultSet"))
;;      (setq plcmp-extra-using-modules '(("LWP::UserAgent" . "HTTP::Response")))
;;      ;; also can both of them
;;      (setq plcmp-extra-using-modules '("DBIx::Class::ResultSet" ("LWP::UserAgent" . "HTTP::Response")))

;; `plcmp-perl-buffer-re'
;;      Regexp
;;      To Detect buffer is perl buffer or not.
;;      default value is "\\.[pP][lmLM]$"

;; `plcmp-other-perl-buffer-limit-number'
;;      Number
;;      how many buffers get buffer words `plcmp-get-sources-other-perl-buffers-words'.  Fixme
;;      default value is 30

;; `plcmp-module-filter-list'
;;      list of String(module name)
;;      module is not appear in method completion list.
;;      default value is '("strict" "warning")

;; `plcmp-additional-PERL5LIB-directories'
;;      list of String(directory)
;;      directory is added to PERL5LIB when invoke completion commands.

;; `plcmp-coding-system'
;;      If this value is not nil,
;;      value is bind to `coding-system-for-read' and `coding-system-for-write' temporary around these commands:
;;      `plcmp-cmd-eval-buffer'
;;      `plcmp-cmd-eval-buffer-and-go'
;;      `plcmp-cmd-eval-on-region'

;; To customize
;; M-x customize-group RET perl-completion RET



;;;code:
(require 'cl)
(require 'anything) ; perl-completion.el uses `anything-aif' macro.
(require 'cperl-mode)
(require 'dabbrev)
(require 'rx)
(require 'regexp-opt)
(require 'ffap)


;;; customize-variables
(defgroup perl-completion nil
  ""
  :group 'perl-completion)

(defcustom plcmp-lib-directory-re "lib/"
  "regexp to detect directory that automatically added to PERL5LIB when build completions.
e.x,
If you are editting file at \"~/dev/SomeModule/lib/SomeModule/Hoge.pm\",
\"~/dev/SomeModule/lib/\" is added to PERL5LIB when invoke completion commands.

default value is \"lib/\""
  :type 'regexp
  :group 'perl-completion)

(defcustom plcmp-use-keymap t
  "If this value is nil,
perl-completion-mode doesn't use own key-map.

it is useful if you want to use own keybind,
or don't like default keybinds.

Note, keymap is defined at library loading phase.
so this variables's value should be set before `require'.

e.x,
;; ok, dont use default keybinds.
(setq plcmp-use-keymap nil)
(require 'perl-completion)


;; NG when `plcmp-use-keymap' is set, plcmp-mode-map is defined already.
(require 'perl-completion)
(setq plcmp-use-keymap nil)"
  :group 'perl-completion)

(defcustom plcmp-extra-using-modules nil
  "list of String or Alist

If value is String(module name),
module name is always appear in candidates.

If value is Alist (\"module-name\" . \"extra-module-name\"),
and module-name is using in current buffer,
extra-module-name's methods are appear in candidates.

e.x,
(setq plcmp-extra-using-modules '(\"DBIx::Class::ResultSet\"))
(setq plcmp-extra-using-modules '((\"LWP::UserAgent\" . \"HTTP::Response\")))
;; also can both of them
(setq plcmp-extra-using-modules '(\"DBIx::Class::ResultSet\" (\"LWP::UserAgent\" . \"HTTP::Response\")))"
  :group 'perl-completion)

(defcustom plcmp-method-inspecter nil
  "Detect how to get methods. 
variable is one of the following values:
'class-inspector
'scrape

'class-inspector, use Class::Inspector
'scrape, open module, then scrape subs. this way is slow.
otherwise, try both"
  :group 'perl-completion)

(defcustom plcmp-perl-buffer-re "\\.[pP][lmLM]$"
  "Regexp
To Detect buffer is perl buffer or not.
default value is \"\\.[pP][lmLM]$\""
  :type 'regexp
  :group 'perl-completion)

(defcustom plcmp-other-perl-buffer-limit-number 30
  "Number
how many buffers get buffer words `plcmp-get-sources-other-perl-buffers-words'.  Fixme
default value is 30"
  :type 'number
  :group 'perl-completion)

(defcustom plcmp-module-filter-list '("strict" "warning")
  "list of String(module name)
module is not appear in method completion list.
default value is '(\"strict\" \"warning\")"
  :type '(repeat (string :tag "Module name"))
  :group 'perl-completion)

(defcustom plcmp-additional-PERL5LIB-directories nil
  "list of String(directory)

directory is added to PERL5LIB when invoke completion commands."
  :type '(repeat (string :tag "perl lib directory"))
  :group 'perl-completion)


;;; log util
(defvar plcmp-debug nil)
(defvar plcmp-log-buf-name "*plcmp debug*")
(defun plcmp-log-buf ()
  (get-buffer-create plcmp-log-buf-name))
(defsubst plcmp-log (&rest messages)
  (ignore-errors
    (when plcmp-debug
      (require 'pp)
      (let* ((str (or (ignore-errors (apply 'format messages))
                      (pp-to-string (car messages))))
             (strn (concat str "\n")))
        (with-current-buffer (plcmp-log-buf)
          (goto-char (point-max))
          (insert strn))
        str))))
(defun plcmp-message (&rest args)
  (when plcmp-debug
    (prog1 (apply 'message args)
      (apply 'plcmp-log args))))


;;; variables
(defvar plcmp-version 1.10)

(defvar plcmp-default-lighter  " PLCompletion")

(defvar plcmp-perl-ident-re "[a-zA-Z_][a-zA-Z_0-9]*")

(defvar plcmp-sub-re (rx-to-string `(and "sub"
                                        (+ space)
                                        (group
                                         (regexp ,plcmp-perl-ident-re)))))

(defvar plcmp-perl-package-re "[a-zA-Z_][a-zA-Z0-9_:]*")

(defvar plcmp-builtin-functions
  '("abs" "exec" "glob" "order" "seek" "symlink" "accept" "exists" "gmtime"
    "our" "seekdir" "syscall" "alarm" "exit" "goto" "pack" "select" "sysopen"
    "atan" "exp" "grep" "package" "semctl" "sysread" "bind" "fcntl" "hex"
    "pipe" "semget" "sysseek" "binmode" "fileno" "import" "pop" "semop"
    "system" "bless" "flags" "index" "pos" "send" "syswrite" "caller" "flock"
    "int" "precision" "setgrent" "tell" "chdir" "fork" "ioctl" "print" "sethostent"
    "telldir" "chmod" "format" "join" "printf" "setnetent" "tie" "chomp" "formline"
    "keys" "prototype" "setpgrp" "tied" "chop" "getc" "kill" "push" "setpriority"
    "time" "chown" "getgrent" "last" "q" "setprotoent" "times" "chr" "getgrgid"
    "lc" "qq" "setpwent" "tr" "chroot" "getgrnam" "lcfirst" "qr" "setservent"
    "truncate" "close" "gethostbyaddr" "length" "quotemeta" "setsockopt" "uc"
    "closedir" "gethostbyname" "link" "qw" "shift" "ucfirst" "connect" "gethostent"
    "listen" "qx" "shmctl" "umask" "continue" "getlogin" "local" "rand" "shmget"
    "undef" "cos" "getnetbyaddr" "localtime" "read" "shmread" "unlink" "crypt"
    "getnetbyname" "lock" "readdir" "shmwrite" "unpack" "dbmclose" "getnetent"
    "log" "readline" "shutdown" "unshift" "dbmopen" "getpeername" "lstat" "readlink"
    "sin" "untie" "defined" "getpgrp" "m" "readpipe" "size" "use" "delete" "getppid"
    "map" "recv" "sleep" "utime" "die" "getpriority" "mkdir" "redo" "socket" "values"
    "do" "getprotobyname" "msgctl" "ref" "socketpair" "vec" "dump" "getprotobynumber"
    "msgget" "rename" "sort" "vector" "each" "getprotoent" "msgrcv" "require" "splice"
    "wait" "endgrent" "getpwent" "msgsnd" "reset" "split" "waitpid" "endhostent"
    "getpwnam" "my" "return" "sprintf" "wantarray" "endnetent" "getpwuid" "next"
    "reverse" "sqrt" "warn" "endprotoent" "getservbyname" "no" "rewinddir" "srand"
    "write" "endpwent" "getservbyport" "oct" "rindex" "stat" "y" "endservent" "getservent"
    "open" "rmdir" "study" "eof" "getsockname" "opendir" "s" "sub" "eval" "getsockopt"
    "ord" "scalar" "substr"))

(defvar plcmp-builtin-variables
  '("$SIG{expr}" "%SIG" "$ENV{expr}" "%ENV" "%INC" "@_" "@INC" "@F" "ARGVOUT"
    "@ARGV" "$ARGV" "ARGV" "$^X" "$EXECUTABLE_NAME" "${^WARNING_BITS}" "$^W"
    "$WARNING" "$^V" "$PERL_VERSION" "${^UTF8LOCALE}" "${^UNICODE}" "${^TAINT}"
    "$^T" "$BASETIME" "$^S" "$EXCEPTIONS_BEING_CAUGHT" "$^R"
    "$LAST_REGEXP_CODE_RESULT" "$^P" "$PERLDB" "${^OPEN}" "$^O" "$OSNAME" "$^M" "$^I" "$INPLACE_EDIT"
    "%^H" "$^H" "$^F" "$SYSTEM_FD_MAX" "$^D" "$DEBUGGING" "$^C" "$COMPILING" "$]"
    "$[" "$0" "$PROGRAM_NAME" "$)" "$EGID" "$EFFECTIVE_GROUP_ID" "$(" "$GID" "$REAL_GROUP_ID"
    "$>" "$EUID" "$EFFECTIVE_USER_ID" "$<" "$UID" "$REAL_USER_ID" "$$" "$PID" "$PROCESS_ID"
    "$@" "$EVAL_ERROR" "$^E" "$EXTENDED_OS_ERROR" "%!" "$!" "$ERRNO" "$OS_ERROR" "${^ENCODING}"
    "$?" "$CHILD_ERROR" "$^A" "$ACCUMULATOR" "$^L" "$FORMAT_FORMFEED" "IO::Handle->format_formfeed" "$:"
    "$FORMAT_LINE_BREAK_CHARACTERS" "IO::Handle->format_line_break_characters" "$^"
    "$FORMAT_TOP_NAME" "HANDLE->format_top_name(EXPR)" "$~"
    "$FORMAT_NAME" "HANDLE->format_name(EXPR)" "@-" "@LAST_MATCH_START"
    "$-" "$FORMAT_LINES_LEFT" "HANDLE->format_lines_left(EXPR)" "$="
    "$FORMAT_LINES_PER_PAGE" "HANDLE->format_lines_per_page(EXPR)" "$%"
    "$FORMAT_PAGE_NUMBER" "HANDLE->format_page_number(EXPR)" "$#" "$;"
    "$SUBSEP" "$SUBSCRIPT_SEPARATOR" "$\"" "$LIST_SEPARATOR" "$\\" "$ORS"
    "$OUTPUT_RECORD_SEPARATOR" "IO::Handle->output_record_separator" "$," "$OFS"
    "$OUTPUT_FIELD_SEPARATOR" "IO::Handle->output_field_separator" "$|"
    "$OUTPUT_AUTOFLUSH" "HANDLE->autoflush(EXPR)" "$/" "$RS"
    "$INPUT_RECORD_SEPARATOR" "IO::Handle->input_record_separator(EXPR)" "$."
    "$NR" "$INPUT_LINE_NUMBER" "HANDLE->input_line_number(EXPR)" "$*" "@+"
    "@LAST_MATCH_END" "$^N" "$+" "$LAST_PAREN_MATCH" "$'" "$POSTMATCH" "$`"
    "$PREMATCH" "$&" "$MATCH" "$<digits>" "$b" "$a" "$_" "$ARG"))

(defvar plcmp--command-cleanup-hook nil "hook run when completion command finished")

(defvar plcmp--cached-variables nil "list of cached variable. each variable is cleared by function `plcmp-cmd-clear-all-caches'")

(defvar plcmp-clear-all-caches-hook nil
  "hook run when invoke `plcmp-cmd-clear-all-caches'")

;;; keymap
(defvar plcmp-mode-map
  (let ((map (make-sparse-keymap)))
    (when plcmp-use-keymap
      ;; completion
      (define-key map (kbd "C-RET") 'plcmp-cmd-smart-complete)
      (define-key map (kbd "C-<return>") 'plcmp-cmd-smart-complete)
      (define-key map (kbd "C-M-i") 'plcmp-cmd-smart-complete)
      (define-key map (kbd "C-c a") 'plcmp-cmd-complete-arrays)
      (define-key map (kbd "C-c i") 'plcmp-cmd-complete-modules)
      (define-key map (kbd "C-c v") 'plcmp-cmd-complete-variables)
      (define-key map (kbd "C-c f") 'plcmp-cmd-complete-functions)
      (define-key map (kbd "C-c h") 'plcmp-cmd-complete-hashes)
      (define-key map (kbd "C-c m") 'plcmp-cmd-complete-methods)
      (define-key map (kbd "C-c C-c a") 'plcmp-cmd-complete-all)

      ;; doc
      (define-key map (kbd "C-c d") 'plcmp-cmd-show-doc)
      (define-key map (kbd "C-c s") 'plcmp-cmd-show-doc-at-point)
      (define-key map (kbd "C-c M") 'plcmp-cmd-menu)

      ;; other
      (define-key map (kbd "C-c c") 'plcmp-cmd-clear-all-caches)
      (define-key map (kbd "C-c C-f") 'plcmp-cmd-project-files)
      (define-key map (kbd "C-c C-c s") 'plcmp-cmd-show-environment)
      (define-key map (kbd "C-c C-c u") 'plcmp-cmd-update-check)
      (define-key map (kbd "C-c C-c d") 'plcmp-cmd-set-additional-lib-directory))
    
    map))

(defvar plcmp-anything-map
  (let ((map (make-sparse-keymap)))
      (define-key map (kbd "O") 'plcmp-acmd-occur)
      (define-key map (kbd "D") 'plcmp-acmd-show-doc)
      (define-key map (kbd "F") 'plcmp-acmd-open-related-file)
      (define-key map (kbd "G") 'plcmp-acmd-goto-looking-point)
      (define-key map (kbd "J") 'scroll-other-window)
      (define-key map (kbd "K") 'scroll-other-window-down)
      (define-key map (kbd "L") 'plcmp-acmd-persistent-look)
      (set-keymap-parent map anything-map)

      map))


;;; macros

;; perl5 lib
;; idea: http://svn.coderepos.org/share/lang/elisp/set-perl5lib/set-perl5lib.el
;;       http://d.hatena.ne.jp/sun-basix/20080117/1200528765 (Japanese)

(defvar plcmp--PERL5LIB-directories nil
  "list of String(directory)
directory is added to PERL5LIB when invoke completion command.

this variable is Internal.
so should not change this variable.")


(defun plcmp--get-lib-path ()
  "return string(additional library path)"
  (let ((dir (plcmp-get-current-directory))
        (lib-re (rx-to-string `(and (group
                                     bol
                                     (* not-newline)
                                     ,plcmp-lib-directory-re)))))
    (when (string-match lib-re dir)
      (let ((lib-dir (match-string 1 dir)))
        (and (stringp lib-dir)
             (file-exists-p lib-dir)
             (directory-file-name lib-dir))))))

(defun* plcmp--get-lib-path-list-liberal (&optional (dir (plcmp-get-current-directory))
                                                    (libdir-names '("extlib" "lib")))
  "return list of string"
  (flet ((aux (libdir-name updirs)
              (loop for updir in updirs
                    for dir = (expand-file-name
                               (concat updir libdir-name))
                    when (and (file-exists-p dir)
                              (file-directory-p dir))
                    collect dir)))
    (let* ((updirs (loop for updir in '("../" "../../" "../../../")
                         collect (concat dir updir)))
           (updirs (mapcar 'expand-file-name updirs))
           (updirs (mapcar 'file-name-directory updirs)))
      (loop for libdir-name in libdir-names
            append (aux libdir-name updirs)))))



(defmacro plcmp-with-set-perl5-lib (&rest body)
  "Set each path that value of `plcmp--get-lib-path' to PERL5LIB.
then execute BODY"
  `(let ((process-environment (copy-sequence process-environment)))
     (require 'env)
     (let* ((additional-lib-list (append (ignore-errors (mapcar 'expand-file-name plcmp-additional-PERL5LIB-directories))
                                         plcmp--PERL5LIB-directories
                                         (when (plcmp--get-lib-path)
                                           (list (plcmp--get-lib-path)))))
            (old-perl5lib (or (getenv "PERL5LIB") "")))
       (when additional-lib-list
         (let* ((additional-lib-str (mapconcat 'identity additional-lib-list path-separator))
                (current-perl5lib (concat additional-lib-str path-separator old-perl5lib))
                (current-perl5lib (replace-regexp-in-string ":$" "" current-perl5lib)))
           (when (and (stringp current-perl5lib)
                      (not (equal "" current-perl5lib)))
             (setenv "PERL5LIB" current-perl5lib)
             (plcmp-log "plcmp-with-set-perl5-lib PERL5LIB: %s" current-perl5lib)))))
     (progn
       ,@body)))


(defmacro define-plcmp-command (command-name-with-no-prefix args &rest body)
  (let* ((prefix "plcmp-cmd-")
         (command-str (symbol-name command-name-with-no-prefix))
         (command-name (concat prefix command-str)))
    `(defun* ,(intern command-name) ,args
       (interactive)
       (unwind-protect
           (let ((anything-map plcmp-anything-map))
             (plcmp-with-set-perl5-lib
              (progn (plcmp-initialize-variables)
                     ,@body)))
         (plcmp-cleanup)))))
(put 'define-plcmp-command 'lisp-indent-function 'defun)
(def-edebug-spec define-plcmp-command defun*)

(defmacro plcmp-ignore-errors (&rest body)
  `(condition-case e (progn ,@body)
     (error (plcmp-log "Error plcmp-ignore-errors :  %s" (error-message-string e)))))
(def-edebug-spec plcmp-ignore-errors ignore-errors)

;;; Util functions
(defsubst plcmp-trim (s)
  "strip space and newline"
  (replace-regexp-in-string
   "[ \t\n]*$" "" (replace-regexp-in-string "^[ \t\n]*" "" s)))

(defsubst plcmp-bit-regexp-p (s)
  (string-match "^[/:$@&%(),.?<>+!|^*';\"\\]+$" s))

(defsubst plcmp-module-p (s)
  (string-match (format "^%s$" plcmp-perl-package-re) s))

(defsubst plcmp-perl-identifier-p (s)
  (string-match (concat "^" plcmp-perl-ident-re "$") s))

(defsubst plcmp-notfound-p (s)
  (anything-aif (string-match "^Can't locate [^ \t]+ in" s)
      (prog1 it
        (plcmp-log "module notfound errmsg: %s" s))))

(defsubst plcmp-tramp-p ()
  (when (and (featurep 'tramp)
             (fboundp 'tramp-tramp-file-p))
    (let* ((dir (plcmp-get-current-directory))
           (tramp? (tramp-tramp-file-p dir)))
      (when tramp?
        (prog1 tramp?
          (plcmp-log "plcmp-tramp-p return non-nil: %s" dir))))))

(defsubst plcmp-insert-each-line (los)
  (insert (mapconcat 'identity los "\n")))

(defun plcmp-get-current-directory ()
  (file-name-directory
   (expand-file-name
    (or (buffer-file-name)
        default-directory))))

;; TODO: need test.
(defsubst plcmp--re-match-sources1 (regexps source)
  (when source
    (let ((source (if (listp source) source (symbol-value source))))
      (some (lambda (re)
              (string-match re (assoc-default 'name source 'eq)))
            regexps))))

(defun plcmp-re-sort-sources (regexps sources &optional reverse)
  (condition-case e
      (let* ((regexps (if (stringp regexps) (list regexps) regexps))
             (match-sources)
             (unmatch-sources)
             (sorted-sources
              (loop for source in sources
                    if (plcmp--re-match-sources1 regexps source)
                    collect source into match-sources
                    else
                    collect source into unmatch-sources
                    finally return (if reverse
                                       (nconc unmatch-sources match-sources)
                                     (nconc match-sources unmatch-sources)))))
        sorted-sources)
    (error (plcmp-log "Error: plcmp-re-sort-sources\nregexps: %s\nsources: %s"
                      regexps
                      sources)
           sources)))

(defsubst* plcmp-collect-matches
    (re &optional (count 0) (match-string-fn 'match-string)
        (point-min (point-min)) (point-max (point-max)))
  (save-excursion
    (loop initially (goto-char point-min)
          while (re-search-forward re point-max t)
          collect (funcall match-string-fn count))))

(defun plcmp-get-occur-fn ()
  "return `occur-by-moccur if installed color-moccur.el otherwise return `occur"
  (if (require 'color-moccur nil t)
      'occur-by-moccur
    'occur))

(defun plcmp-update-lighter (str)
  (when (and (boundp 'perl-completion-mode)
             (assq 'perl-completion-mode minor-mode-alist))
    (setcar (cdr (assq 'perl-completion-mode minor-mode-alist)) str)))



;;; initial-input
(defvar plcmp-initial-input "")
(defvar plcmp-real-initial-input "real initial-input if required `anything-match-plugin' initial-input is not real initial-input")

(defun plcmp--fullname ()
  (save-excursion
    (let ((start (point)))
      (skip-chars-backward "a-zA-Z0-9_")
      (let ((str (plcmp-preceding-string 2)))
        (when (string-equal str "::")
          (buffer-substring-no-properties start (point)))))))


(defun plcmp-get-initial-real-input-list ()
  "return list (initial-input real-initial-input)"
  (save-excursion
    (let* ((start (point))
           (real-initial-input
            (cond
             ((plcmp--fullname))
             (t
              (skip-syntax-backward "w_")
              (let* ((preceding-string (char-to-string (preceding-char)))
                     (end (condition-case e
                              (cond
                               ((some (lambda (s) (string-equal s preceding-string)) '("$" "@" "%" "&"))
                                (backward-char)
                                (point))
                               (t
                                (point)))
                            (error (point)))))
                (buffer-substring-no-properties start end)))))
           (initial-input
            (regexp-quote
             (concat real-initial-input
                     (if (and (featurep 'anything-match-plugin)
                              (not (string-equal real-initial-input "")))
                         " "
                       "")))))
      (prog1 (values initial-input real-initial-input)
        (plcmp-log "plcmp-get-initial-real-input-list:\ninitial-input: %s\nreal-initial-input: %s"
                   initial-input
                   real-initial-input)))))


;;; installed modules
(defvar plcmp-installed-modules nil)
(add-to-list 'plcmp--cached-variables 'plcmp-installed-modules)

(defun plcmp-get-installed-modules ()
  (unless (plcmp-tramp-p)
    (prog1 plcmp-installed-modules
      (or plcmp-installed-modules
          (plcmp--installed-modules-asynchronously)))))

(defun plcmp--installed-modules-synchronously ()
  (message "fetching installed modules...")
  (let* ((modules-str (shell-command-to-string
                       (concat
                        "find `perl -e 'pop @INC; print join(q{ }, @INC);'`"
                        " -name '*.pm' -type f "
                        "| xargs egrep -h -o 'package [a-zA-Z0-9:]+;' "
                        "| perl -nle 's/package\s+(.+);/$1/; print' "
                        "| sort "
                        "| uniq "
                        )))
         (modules (split-string modules-str "\n")))
    (message "done")
    (remove-if (lambda (module)
                 (string-match "No such file or directory$" module))
               modules)))

(defvar plcmp-installed-modules-buffer-name " *perl-completion installed modules*")
(defun plcmp--installed-modules-set-cache (process event)
  "process-sentinel"
  (when (string-equal "finished\n" event)
    (with-current-buffer plcmp-installed-modules-buffer-name
      (unless (zerop (buffer-size))
        (setq plcmp-installed-modules (plcmp-collect-matches plcmp-perl-package-re))
        ;; (message "finished getting installed modules asynchronously.")
        (plcmp-update-lighter plcmp-default-lighter)
        (plcmp-log "cached installed modules %s" plcmp-installed-modules)))))

(defun plcmp--installed-modules-asynchronously ()
  "start process, set sentinel, return process."
  (let ((proc-run?
         (lambda ()
           (eq 'run
               (let ((proc (get-buffer-process plcmp-installed-modules-buffer-name)))
                 (when (processp proc)
                   (process-status proc)))))))
    (unless (or (plcmp-tramp-p)
                (funcall proc-run?))
      ;; (message "fetching installed modules...")
      (plcmp-update-lighter (format "%s[getting modules]" plcmp-default-lighter))
      (with-current-buffer (get-buffer-create plcmp-installed-modules-buffer-name)
        (erase-buffer))
      (let* ((command "find")
             (args (concat "`perl -e 'pop @INC; print join(q{ }, @INC);'`"
                           " -name '*.pm' -type f "
                           "| xargs grep -E -h -o 'package [a-zA-Z0-9:]+;' "
                           "| perl -nle 's/package\s+(.+);/$1/; print' "
                           "| sort "
                           "| uniq "))
             (proc (start-process-shell-command "installed perl modules"
                                                plcmp-installed-modules-buffer-name
                                                command
                                                args)))
        (set-process-sentinel proc 'plcmp--installed-modules-set-cache)
        ;; return process
        proc))))



;;; current package
(defvar plcmp-current-package-name "")
(defun plcmp-get-current-package-name ()
  "nil or string"
  (let ((re (rx-to-string `(and bol
                                (* space)
                                "package"
                                (* space)
                                (group
                                 (regexp ,plcmp-perl-package-re))
                                (* not-newline)
                                ";"))))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward re nil t)
        (match-string-no-properties 1)))))

;;; using-modules
(defvar plcmp-using-modules nil)
(defun plcmp-get-using-modules ()
  "return modules"
  (let* ((modules (plcmp--get-using-modules-uses-and-requires))
         (modules (plcmp--using-modules-marge-extra-modules modules))
         (modules (plcmp--using-modules-filter modules)))
    modules))

(defun plcmp--get-using-modules-uses-and-requires ()
  "list of string"
  (let ((re (rx-to-string ` (and bol
                                 (* space)
                                 "use"
                                 (+ space)
                                 (group ;1 package
                                  (regexp ,plcmp-perl-package-re))
                                 (* not-newline)
                                 ";")))
        (require-re (rx-to-string `(and "require"
                                        (+ space)
                                        (group
                                         (regexp ,plcmp-perl-package-re))))))
    (nunion (plcmp-collect-matches re 1 'match-string-no-properties)
            (delete-if-not (lambda (s)
                             (member s plcmp-installed-modules))
                           (plcmp-collect-matches require-re 1 'match-string-no-properties))
            :test 'string-equal)))

(defun plcmp--using-modules-marge-extra-modules (modules)
  (condition-case err
        (dolist (ext-module plcmp-extra-using-modules modules)
          (cond
           ((stringp ext-module)
            (add-to-list 'modules ext-module))
           ((consp ext-module) ; alist ("module-name" . "extra-module-name")
            (when (and (stringp (car ext-module))
                       (stringp (cdr ext-module))
                       (member (car ext-module) modules))
              (add-to-list 'modules (cdr ext-module))))))
      (error (prog1 modules
               (plcmp-log "Error in plcmp-get-using-modules %s"
                          (error-message-string err))))))



(defun plcmp--using-modules-filter (modules)
  (set-difference modules
                  plcmp-module-filter-list
                  :test 'string-equal))


;;; methods
(defvar plcmp-obj-instance-of-module-maybe-alist nil)
(defun plcmp-get-obj-instance-of-module-maybe-alist (using-modules)
  (let* ((using-module-re (regexp-opt using-modules))
         (re (rx-to-string `(and (group "$" ;1 var name
                                  (regexp ,plcmp-perl-ident-re))
                                 (* space)
                                 "="
                                 (* space)
                                 (group      ;2 module-name
                                  (regexp ,using-module-re))))))
    (save-excursion
      (loop initially (goto-char (point-min))
            while (re-search-forward re nil t)
            collect `(,(match-string-no-properties 1) . ,(match-string-no-properties 2))))))

(defsubst plcmp--make-los (str)
  (with-temp-buffer
    (insert str)
    (plcmp-collect-matches plcmp-perl-ident-re)))

(defsubst plcmp--inspect-module-class-inspector (module-name)
  "Inspect module for getting methods.

this function returns alist (module-name . list of methods)
If MODULE-NAME is not valid, returns `nil'"
  (when (plcmp-module-p module-name)
    (let ((modules-str
           (shell-command-to-string
            (concat "perl -MClass::Inspector -e'use "
                    module-name
                    "; print join \"\n\"=>@{Class::Inspector->methods("
                    module-name
                    ")} '"))))
      (when (and (not (plcmp-notfound-p modules-str))
                 (stringp modules-str))
        (let ((modules (plcmp--make-los modules-str)))
          `(,module-name . ,modules))))))

(defsubst plcmp-get-buffer-subs ()
  (plcmp-collect-matches plcmp-sub-re 1 'match-string-no-properties))

(defun plcmp-get-module-file-path (module-name)
  (plcmp-with-set-perl5-lib
   (let* ((path (shell-command-to-string (concat "perldoc -ml " (shell-quote-argument module-name))))
          (path (plcmp-trim path)))
     (and (stringp path)
          (file-exists-p path)
          (expand-file-name path)))))

(defun plcmp--inspect-module-scrape (module-name)
  (when (and (stringp module-name)
             (plcmp-module-p module-name))
    (let* ((path (plcmp-get-module-file-path module-name)))
      (when (and (stringp path)
                 (file-exists-p path)
                 (file-readable-p path))
        (let ((modules (with-temp-buffer
                         (insert-file-contents path)
                         (plcmp-get-buffer-subs))))
          `(,module-name . ,modules))))))

(defsubst plcmp--inspect-module (module-name)
  (message "getting methods %s ..." module-name)
  (funcall (plcmp--get-inspect-fn) module-name))


(defun plcmp--get-inspect-fn ()
  (case plcmp-method-inspecter
    (class-inspector 'plcmp--inspect-module-class-inspector)
    (scrape 'plcmp--inspect-module-scrape)
    (otherwise (lambda (module-name)
                 (or (plcmp--inspect-module-class-inspector module-name)
                     (plcmp--inspect-module-scrape module-name))))))

(defvar plcmp-module-methods-alist nil
  "alist, (module-name . (list of methods))")
(add-to-list 'plcmp--cached-variables 'plcmp-module-methods-alist)


(defun plcmp-get-module-methods-alist (using-modules)
  (dolist (module-name using-modules plcmp-module-methods-alist)
    (unless (assoc module-name plcmp-module-methods-alist)
      (add-to-list 'plcmp-module-methods-alist
                   (plcmp--inspect-module module-name)))))


;; module-name -> source
(defvar plcmp--mk-module-source-name " Methods")
(defsubst plcmp--mk-module-source (module-name)
  (anything-aif (assoc-default module-name plcmp-module-methods-alist)
      `((name . ,(concat module-name plcmp--mk-module-source-name))
        (action . (("Insert" . plcmp-insert)
                   ("Show doc" .
                    (lambda (candidate)
                      (let* ((module (plcmp-get-current-module-name))
                             (buf (plcmp-get-man-buffer module 'module)))
                        (save-selected-window
                          (pop-to-buffer buf)))))
                   ("Show doc and go" .
                    (lambda (candidate)
                      (let* ((module (plcmp-get-current-module-name))
                             (buf (plcmp-get-man-buffer module 'module)))
                        (pop-to-buffer buf))))
                   ("Open module file" .
                    (lambda (method)
                      (let ((module (plcmp-get-current-module-name)))
                        (plcmp-open-module-file module))))
                   ("Open module file other window" .
                    (lambda (method)
                      (let ((module-name (plcmp-get-current-module-name)))
                        (plcmp-open-module-file module-name 'pop-to-buffer))))
                   ("Open module file other frame" .
                    (lambda (candidate)
                      (let ((module-name (plcmp-get-current-module-name)))
                        (plcmp-open-module-file module-name
                                                'switch-to-buffer-other-frame))))
                   ("Occur module file" .
                    (lambda (candidate)
                      (let ((module-name (plcmp-get-current-module-name)))
                        (plcmp-open-module-file module-name
                                                'switch-to-buffer)
                        (funcall (plcmp-get-occur-fn)
                                 candidate
                                  nil))))
                   ("Add to kill-ring" . kill-new)
                   ("Insert source name" .
                    (lambda (candidate)
                      (let ((name (plcmp-anything-get-current-source-name)))
                        (and (stringp name)
                             (insert name)))))
                   ))
        (init . (lambda ()
                  (with-current-buffer (anything-candidate-buffer 'global)
                    (plcmp-insert-each-line ',it))))
        (candidates-in-buffer)
        (persistent-action . (lambda (candidate)
                               (let ((module-name (plcmp-get-current-module-name)))
                                 (plcmp-open-doc module-name)
                                 (plcmp-re-search-forward-fontify (regexp-quote candidate))))))))


;; plcmp-using-modules -> sources
(defun plcmp-get-sources-methods (using-modules)
  (loop for module-name in using-modules
        collect (plcmp--mk-module-source module-name)))


;;; dabbrev
(defvar plcmp-buffer-dabbrevs-re
  (rx (>= 4 (or (syntax word)
                (syntax symbol)))))

(defsubst* plcmp-get-buffer-dabbrevs ()
  (plcmp-collect-matches plcmp-buffer-dabbrevs-re 0 'match-string-no-properties))

;;; current buffer words
(defsubst* plcmp--check-face (face-names &optional (point (point)))
  (let* ((face (get-text-property point 'face))
         (faces (if (listp face) face (list face))))
    (some (lambda (face-sym)
            (memq face-sym faces))
          face-names)))

(defsubst* plcmp-get-face-words (&optional (faces '(font-lock-variable-name-face
                                                    font-lock-function-name-face)))
  (let ((hash (make-hash-table :test 'equal))
        (ret nil))
    (save-excursion
      (loop initially (goto-char (point-min))
            for next-change = (or (next-property-change (point) (current-buffer))
                                  (point-max))
            until (eobp)
            do (progn (when (plcmp--check-face faces)
                        (anything-aif (cperl-word-at-point)
                            (puthash it nil hash)))
                      (goto-char next-change)))
      (maphash (lambda (k v) (push k ret)) hash) ; remove-dups
      (nreverse ret))))

;;; other buffer words

(defvar plcmp-other-perl-buffers-words-faces
  '(font-lock-function-name-face
    font-lock-variable-name-face
    font-lock-keyword-face
    font-lock-builtin-face
    font-lock-type-face
    cperl-array-face
    cperl-hash-face))

;; cache
(defvar plcmp-buffer-tick-hash (make-hash-table :test 'equal))
(defun* plcmp-buffer-is-modified (&optional (buffer (current-buffer)))
  "Return non-nil when BUFFER is modified since `anything' was invoked."
  (let* ((key (concat (buffer-name buffer)
                      "/"
                      (anything-attr 'name)))
         (source-tick (or (gethash key plcmp-buffer-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick buffer)))
    (prog1 (/= source-tick buffer-tick)
      (puthash key buffer-tick plcmp-buffer-tick-hash)
      (plcmp-log "plcmp-buffer-is-modified")
      (plcmp-log "current-buffer: %s" buffer)
      (plcmp-log "source-tick: %s\nbuffer-tick: %s" source-tick buffer-tick))))


(defvar plcmp-other-perl-buffers-cache-hash (make-hash-table :test 'equal))
(add-hook 'plcmp-clear-all-caches-hook
          (lambda ()
            (setq plcmp-other-perl-buffers-cache-hash
                  (make-hash-table :test 'equal))))

(defun plcmp-add-to-other-perl-buffers-cache-hash (source-name faces buffer-name)
  (let ((hash (or (gethash source-name plcmp-other-perl-buffers-cache-hash)
                  (puthash source-name
                           (make-hash-table :test 'equal)
                           plcmp-other-perl-buffers-cache-hash)))
        (words (plcmp-get-face-words faces)))
    (assert (hash-table-p hash))
    (puthash buffer-name words hash)
    (prog1 words
      (plcmp-log "\nplcmp-add-to-other-perl-buffers-cache-hash: %s"
                 words))))

(defun plcmp-get-other-perl-buffers-cache (source-name buffer-name)
  "return los or nil if not cache ready"
  (let ((hash (gethash source-name plcmp-other-perl-buffers-cache-hash)))
    (and (hash-table-p hash)
         (prog1 (gethash buffer-name hash)
           (plcmp-log "plcmp-get-other-perl-buffers-cache: %s"
                      (gethash buffer-name hash))))))

(defun plcmp-other-perl-buffers-get-buffer-name ()
  "return buffer or nil"
  (let ((source-name (anything-attr 'name)))
    (when (string-match (rx " *" (group (* not-newline)) "*" eol)
                        source-name)
      (let ((buffer-name (match-string 1 source-name)))
        (when (stringp buffer-name)
          (let ((buffer (get-buffer buffer-name)))
            (and (bufferp buffer)
                 buffer)))))))

(defun plcmp-other-perl-buffers-action-open-related-buffer (candidate)
  (let ((buffer (plcmp-other-perl-buffers-get-buffer-name)))
    (switch-to-buffer buffer)))

(defun plcmp-other-perl-buffers-action-occur (candidate)
  (let ((buffer (plcmp-other-perl-buffers-get-buffer-name)))
    (switch-to-buffer buffer)
    (funcall (plcmp-get-occur-fn)
             (regexp-quote candidate)
             nil)))

(defun plcmp--mk-other-perl-buffer-source (source-name faces buffer-name)
  `((name
     . ,(concat source-name " *" buffer-name "*"))
    (action
     . (("Insert" . plcmp-insert)
        ("Open related buffer" . plcmp-other-perl-buffers-action-open-related-buffer)
        ("Occur" . plcmp-other-perl-buffers-action-occur)))
    (init
     . (lambda ()
         (let ((words
                (with-current-buffer (get-buffer ,buffer-name)
                  (anything-aif (and (not (plcmp-buffer-is-modified))
                                     (plcmp-get-other-perl-buffers-cache ,source-name ,buffer-name))
                      (prog1 it
                        (plcmp-log "return cache buffer: %s source-name: %s " ,buffer-name ,source-name))
                    (prog1 (plcmp-add-to-other-perl-buffers-cache-hash ,source-name ',faces ,buffer-name)
                      (plcmp-log "modified: %s" ,buffer-name))))))
           (with-current-buffer (anything-candidate-buffer 'global)
             (plcmp-insert-each-line words)))))
    (candidates-in-buffer)
    (persistent-action
     . (lambda (candidate)
         (let ((buffer (plcmp-other-perl-buffers-get-buffer-name)))
           (when (bufferp buffer)
             (switch-to-buffer buffer)
             (plcmp-re-search-forward-fontify (regexp-quote candidate))))))))

(defun plcmp--sources-other-perl-buffers (source-name faces)
  (let* ((perl-buffers (remove-if-not (lambda (buf)
                                        (string-match plcmp-perl-buffer-re (buffer-name buf)))
                                      (buffer-list)))
         (perl-buffers (subseq perl-buffers 0 plcmp-other-perl-buffer-limit-number))
         (sources (loop for buffer in perl-buffers
                        when (bufferp buffer)
                        collect (with-current-buffer buffer
                                  (plcmp--mk-other-perl-buffer-source
                                   source-name
                                   faces
                                   (buffer-name buffer))))))
    (prog1 sources
      (plcmp-log "plcmp--sources-other-perl-buffers:")
      (plcmp-log sources))))

(defun plcmp-get-sources-other-perl-buffers-words ()
  (plcmp--sources-other-perl-buffers "words" plcmp-other-perl-buffers-words-faces))

(defun plcmp-get-sources-other-perl-buffers-variable ()
  (plcmp--sources-other-perl-buffers "variables" '(font-lock-variable-name-face)))

(defun plcmp-get-sources-other-perl-buffers-hashes ()
  (plcmp--sources-other-perl-buffers "hashes" '(cperl-hash-face)))

(defun plcmp-get-sources-other-perl-buffers-arrays ()
  (plcmp--sources-other-perl-buffers "arrays" '(cperl-array-face)))

(defun plcmp-get-sources-other-perl-buffers-functions ()
  (plcmp--sources-other-perl-buffers "functions" '(font-lock-function-name-face)))

;;; man
(defvar plcmp-man-pages
  (ignore-errors
    (let ((pages (progn
                   (require 'woman)
                   (woman-file-name "")
                   (mapcar 'car woman-topic-all-completions))))
      (remove-if-not (lambda (s)
                       (or (member s plcmp-installed-modules)
                           (string-match "perl" s)))
                     pages))))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Document
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defvar plcmp-look-overlay nil "overlay")
(add-hook 'plcmp--command-cleanup-hook
          (lambda ()
            (when (overlayp plcmp-look-overlay)
              (delete-overlay plcmp-look-overlay))))

(defvar plcmp-look-current-positions nil
  "list of (point buffer)
point, after `plcmp-re-search-forward-fontify'")
(add-hook 'plcmp--command-cleanup-hook
          (lambda ()
            (setq plcmp-look-current-positions nil)))

(defun plcmp-re-search-forward-fontify (regexp)
  (if (re-search-forward regexp nil t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        ;; remember positions
        (setq plcmp-look-current-positions (list (point) (current-buffer)))
        (plcmp-log "plcmp-look-current-positions: %s" (list (point) (current-buffer)))
        (when (and beg end)
          (if (overlayp plcmp-look-overlay)
              (move-overlay plcmp-look-overlay beg end (current-buffer))
            (setq plcmp-look-overlay (make-overlay beg end)))
          (overlay-put plcmp-look-overlay 'face 'highlight)
          (recenter 5)))
    (goto-char (point-min))))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Initialize, Cleanup
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defun plcmp-initialize-variables ()
  (setq plcmp-installed-modules (plcmp-get-installed-modules)
        plcmp-current-package-name (plcmp-get-current-package-name)
        plcmp-using-modules (plcmp-get-using-modules)
        plcmp-module-methods-alist (plcmp-get-module-methods-alist plcmp-using-modules)
        plcmp-obj-instance-of-module-maybe-alist (plcmp-get-obj-instance-of-module-maybe-alist plcmp-using-modules))
  (multiple-value-setq
      (plcmp-initial-input plcmp-real-initial-input) (plcmp-get-initial-real-input-list)))

(defun plcmp-cleanup ()
  (run-hooks 'plcmp--command-cleanup-hook))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Anything
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun plcmp--anything-get-current-source-name ()
  "Return the source name for the current selection."
  (declare (special source))
  ;; The name `anything-get-current-source' should be used in init function etc.
  (if (and (boundp 'anything-source-name) (stringp anything-source-name))
      anything-source-name
    (with-current-buffer (anything-buffer-get)
      ;; This goto-char shouldn't be necessary, but point is moved to
      ;; point-min somewhere else which shouldn't happen.
      (goto-char (overlay-start anything-selection-overlay))
      (let* ((header-pos (anything-get-previous-header-pos)))
        (save-excursion
          (assert header-pos)
          (goto-char header-pos)
          (prog1 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))
            (plcmp-log "plcmp-anything-get-current-source-name: %s"
                       (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))))))))

(defun plcmp-get-current-module-name ()
  (let* ((module (plcmp--anything-get-current-source-name)))
    (when (string-match
           (rx-to-string `(and
                           bol
                           (group
                            (regexp ,plcmp-perl-package-re))
                           ,plcmp--mk-module-source-name))
           module)
      (match-string 1 module))))

;;;TODO
(defun plcmp-completion-get-man-buffer (candidate)
  "return manpage buffer ,called in completion source action"
  (let* ((name (plcmp--anything-get-current-source-name))
         (ret (cond
               ((string-equal name "builtin functions")
                (plcmp-get-man-buffer candidate 'function))
               ((or (string-equal name "using modules")
                    (string-equal name "installed modules"))
                (plcmp-get-man-buffer candidate))
               ((string-equal name "builtin variables")
                (plcmp-get-man-buffer "" 'variables))
               ((string-match (concat plcmp--mk-module-source-name "$")
                              name)
                (plcmp-get-current-module-name)))))
    (prog1 ret
      (plcmp-log "plcmp-completion-get-man-buffer candidate: %s return: %s"
                 candidate
                 ret))))


(defvar plcmp-type-completion
    '(plcmp-completion
      (action . (("Insert" . plcmp-insert)
                 ("Show man" .
                  (lambda (candidate)
                    (anything-aif (plcmp-completion-get-man-buffer candidate)
                        (switch-to-buffer it))))
                 ("Add to kill-ring" . kill-new)
                 ("Insert source name" .
                  (lambda (candidate)
                    (let ((name (plcmp-anything-get-current-source-name)))
                      (and (stringp name)
                           (insert name)))))
                 ))))
(add-to-list 'anything-type-attributes plcmp-type-completion)

(defvar plcmp-type-completion-method
    '(plcmp-completion-method
      (action . (("Insert" . plcmp-insert)
                 ("Open module file" .
                  (lambda (method)
                    (let ((module (plcmp-get-current-module-name)))
                      (plcmp-open-module-file module))))
                 ("Open module file other window" .
                  (lambda (method)
                    (let ((module-name (plcmp-get-current-module-name)))
                      (plcmp-open-module-file module-name 'pop-to-buffer))))
                 ("Open module file other frame" .
                  (lambda (candidate)
                    (let ((module-name (plcmp-get-current-module-name)))
                      (plcmp-open-module-file module-name
                                              'switch-to-buffer-other-frame))))
                 ("Add to kill-ring" . kill-new)
                 ("Insert source name" .
                  (lambda (candidate)
                    (let ((name (plcmp-anything-get-current-source-name)))
                      (and (stringp name)
                           (insert name)))))
                 ))))
(add-to-list 'anything-type-attributes plcmp-type-completion-method)

(defvar plcmp-type-man
  '(plcmp-doc
    (action . (("Show man" .
                (lambda (candidate)
                  (plcmp-open-doc candidate 'man)))
               ("Show man other window" .
                (lambda (candidate)
                  (plcmp-open-doc candidate 'man 'pop-to-buffer)))
               ("Show man other window and go" .
                (lambda (candidate)
                  (plcmp-open-doc candidate 'man 'switch-to-buffer-other-window)))
               ("Show man other frame and go" .
                (lambda (candidate)
                  (plcmp-open-doc candidate 'man 'switch-to-buffer-other-frame)))
               ("Occur man buffer" .
                (lambda (candidate)
                  (when (plcmp-open-doc candidate 'man)
                    (call-interactively (plcmp-get-occur-fn)
                                        (regexp-quote candidate)))))
               ("Insert man name" . insert)
               ("Add man name to kill-ring" . kill-new)))))

(defvar plcmp-type-perldoc
  '(plcmp-perldoc
    (action . ())))
(add-to-list 'anything-type-attributes plcmp-type-man)

(defun plcmp-insert (candidate)
  (delete-backward-char (length plcmp-real-initial-input))
  (insert candidate))

;;; perldoc
(defun* plcmp-get-man-buffer (topic &optional (type 'module))
  "like `Man-getpage-in-background' but call process synchronously.
return buffer or nil unless process return 0"
  (require 'man)
  (let* ((manual-program (ecase type
                           (module "perldoc")
                           (man manual-program)
                           (function "perldoc -f")
                           (variable "perldoc perlvar")))
         (command (if (eq type 'variable)
                      manual-program
                    (concat manual-program " " topic)))
         (bufname (concat "*perldoc " topic "*"))
         (buffer  (get-buffer-create bufname)))
    (require 'env)
    (let ((process-environment (copy-sequence process-environment))
            ;; The following is so Awk script gets \n intact
            ;; But don't prevent decoding of the outside.
            (coding-system-for-write 'raw-text-unix)
            ;; We must decode the output by a coding system that the
            ;; system's locale suggests in multibyte mode.
            (coding-system-for-read
             (if default-enable-multibyte-characters
                 locale-coding-system 'raw-text-unix))
            ;; Avoid possible error by using a directory that always exists.
            (default-directory
              (if (and (file-directory-p default-directory)
                       (not (find-file-name-handler default-directory
                                                    'file-directory-p)))
                  default-directory
                "/")))
        ;; Prevent any attempt to use display terminal fanciness.
        (setenv "TERM" "dumb")
        (save-window-excursion (shell-command command bufname))
        (get-buffer bufname))))

(defun* plcmp-open-doc (topic &optional (type 'module) (show-fn 'switch-to-buffer))
  (require 'man)
  (let ((manbuf (plcmp-get-man-buffer topic type)))
    (when (and (bufferp manbuf)
               (functionp show-fn))
      (funcall show-fn manbuf))))


;;; open module file
(defun plcmp--find-module-file-no-select (module-name)
  (when (plcmp-module-p module-name)
    (let ((path (plcmp-get-module-file-path module-name)))
      (when (and (stringp path)
                 (file-exists-p path)
                 (file-readable-p path))
        (find-file-noselect path)))))

(defun* plcmp-open-module-file (module-name &optional (show-buffer-fn 'switch-to-buffer))
  (anything-aif (plcmp--find-module-file-no-select module-name)
      (funcall show-buffer-fn it)
    (message "can't find %s" module-name)))

;;; sources
;; completion
(defvar plcmp-anything-source-completion-using-modules
  `((name . "using modules")
    (action . (("Insert" . plcmp-insert)
               ("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (pop-to-buffer buf))))
               ("Occur" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf)
                    (call-interactively (plcmp-get-occur-fn)))))
               ("Open module file" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate)))
               ("Open module file other window" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate 'pop-to-buffer)))
               ("Open module file other frame" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate
                                          'switch-to-buffer-other-frame)))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (when plcmp-using-modules
                  (plcmp-insert-each-line plcmp-using-modules)))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))

(defvar plcmp-anything-source-completion-builtin-functions
  `((name . "builtin functions")
    (action . (("Insert" . plcmp-insert)
               ("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'function)))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'function)))
                    (switch-to-buffer-other-window buf))))
               ("Occur" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'function)))
                    (switch-to-buffer buf)
                    (funcall (plcmp-get-occur-fn)
                             (regexp-quote candidate)
                             nil))))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (plcmp-insert-each-line plcmp-builtin-functions))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate 'function)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
     ))

(defvar plcmp-anything-source-completion-builtin-variables
  `((name . "builtin variables")
    (action . (("Insert" . plcmp-insert)
               ("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'variable))
                        (re (rx-to-string `(and bol (= 4 space)
                                                (group (eval ,candidate))
                                                (syntax whitespace)))))
                    (with-current-buffer buf
                      (re-search-forward re nil t))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'variable))
                        (re (rx-to-string `(and bol (= 4 space)
                                                (group (eval ,candidate))
                                                (syntax whitespace)))))
                    (switch-to-buffer-other-window buf)
                    (re-search-forward re nil t))))
               ("Occur" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'variable)))
                    (switch-to-buffer buf)
                    (funcall (plcmp-get-occur-fn)
                             (regexp-quote candidate)
                             nil))))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (plcmp-insert-each-line plcmp-builtin-variables))))
    (candidates-in-buffer)
    (search . ((lambda (re arg1 arg2)
                 (re-search-forward (regexp-quote re) nil t))))
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate 'variable)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))

(defvar plcmp-anything-source-completion-installed-modules
  `((name . "installed modules")
    (action . (("Insert" . plcmp-insert)
               ("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (pop-to-buffer buf))))
               ("Occur" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf)
                    (call-interactively (plcmp-get-occur-fn)))))
               ("Open module file" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate)))
               ("Open module file other window" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate 'pop-to-buffer)))
               ("Open module file other frame" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate
                                          'switch-to-buffer-other-frame)))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (let ((installed-modules (plcmp-get-installed-modules)))
                  (when installed-modules
                    (plcmp-insert-each-line installed-modules))))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))

(defvar plcmp-anything-source-completion-buffer-dabbrevs
  `((name . "buffer dabbrevs")
    (action . (("Insert" . plcmp-insert)
               ("Occur" .
                (lambda (candidate)
                  (funcall (plcmp-get-occur-fn)
                           (regexp-quote candidate)
                           nil)))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (let* ((words (plcmp-get-buffer-dabbrevs))
                     (words (delete plcmp-real-initial-input words)))
                (with-current-buffer (anything-candidate-buffer 'global)
                  (plcmp-insert-each-line words)))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (switch-to-buffer anything-current-buffer)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))


;; man, perldoc
(defvar plcmp-anything-source-doc-man-pages
  '((name . "perl man pages")
    (action . (("Show man" . woman)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (plcmp-insert-each-line plcmp-man-pages))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (woman candidate)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))

(defvar plcmp-anything-source-doc-using-modules
  '((name . "using modules")
    (action . (("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf))))
               ("Occur doc buffer" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf)
                    (call-interactively (plcmp-get-occur-fn)
                                        (regexp-quote candidate)))))
               ("Open module file" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate)))
               ("Open module file other window" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate 'pop-to-buffer)))
               ("Open module file other frame" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate
                                          'switch-to-buffer-other-frame)))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (plcmp-insert-each-line plcmp-using-modules))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))

(defvar plcmp-anything-source-doc-installed-modules
  '((name . "installed modules")
    (action . (("Show doc" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (save-selected-window
                      (pop-to-buffer buf)))))
               ("Show doc and go" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf))))
               ("Occur doc buffer" .
                (lambda (candidate)
                  (let ((buf (plcmp-get-man-buffer candidate 'module)))
                    (switch-to-buffer buf)
                    (call-interactively (plcmp-get-occur-fn)))))
               ("Open module file" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate)))
               ("Open module file other window" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate 'pop-to-buffer)))
               ("Open module file other frame" .
                (lambda (candidate)
                  (plcmp-open-module-file candidate
                                          'switch-to-buffer-other-frame)))
               ("Add to kill-ring" . kill-new)))
    (init . (lambda ()
              (with-current-buffer (anything-candidate-buffer 'global)
                (plcmp-insert-each-line plcmp-installed-modules))))
    (candidates-in-buffer)
    (persistent-action . (lambda (candidate)
                           (plcmp-open-doc candidate)
                           (plcmp-re-search-forward-fontify (regexp-quote candidate))))
    ))


;; menu
(defun plcmp-menu-cands ()
  (with-temp-buffer
    (let ((re (rx bol
                  (group (+ not-newline))
                  (group "plcmp-cmd-"
                         (+ not-newline)
                         eol))))
      (insert (substitute-command-keys (format "\\{%s}" "plcmp-mode-map")))
      (loop initially (goto-char (point-min))
            while (re-search-forward re nil t)
            collect (let* ((key (plcmp-trim (match-string 1)))
                           (command (plcmp-trim (match-string 2)))
                           (display (concat "[" key "] "  command))
                           (real command))
                      `(,display . ,real))))))

(defvar plcmp-anything-source-menu
  `((name . "perl-completion menu")
    (action ("Call interactively" . (lambda (command-name)
                                      (call-interactively (intern command-name))))
            ("Add command to kill ring" . kill-new)
            ("Go to command's definition" . (lambda (command-name)
                                              (find-function
                                               (intern command-name)))))
    (candidates . plcmp-menu-cands)
    ))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; prefix: plcmp-cmd-

;;; complete-all
(defvar plcmp-completion-all-static-sources
  '(
    plcmp-anything-source-completion-buffer-dabbrevs
    plcmp-anything-source-completion-builtin-variables
    plcmp-anything-source-completion-builtin-functions    
    plcmp-anything-source-completion-using-modules    
    plcmp-anything-source-completion-installed-modules
    ))

(defun plcmp-get-sources-for-complete-all ()
  (append
   (plcmp-get-sources-methods plcmp-using-modules)
   (plcmp-get-sources-other-perl-buffers-variable)
   (plcmp-get-sources-other-perl-buffers-hashes)
   (plcmp-get-sources-other-perl-buffers-arrays)
   (plcmp-get-sources-other-perl-buffers-functions)
   plcmp-completion-all-static-sources))

(define-plcmp-command complete-all ()
  (anything (plcmp-get-sources-for-complete-all) plcmp-initial-input))


;;; smart-complete

(defvar plcmp-completion-smart-complete-static-sources
  '(
    plcmp-anything-source-completion-buffer-dabbrevs
    plcmp-anything-source-completion-builtin-variables
    plcmp-anything-source-completion-builtin-functions
    plcmp-anything-source-completion-using-modules
    plcmp-anything-source-completion-installed-modules
    ))

(defun plcmp--sources-for-smart-complete ()
  (append
   (plcmp-get-sources-methods plcmp-using-modules)
   (plcmp-get-sources-other-perl-buffers-words)
   plcmp-completion-smart-complete-static-sources))

(defsubst* plcmp-preceding-string (&optional (count 1))
  "Return before COUNT character preceding point as String.
If COUNT is omitted, COUNT is set to 1.

don't throw error evan if point is at beggning of buffer."
  (buffer-substring-no-properties
   (point)
   (condition-case nil
       (save-excursion (backward-char count) (point))
     (error (point)))))

(defsubst plcmp-method-p ()
  (string-equal "->" (plcmp-preceding-string 2)))

(defun plcmp--get-context-symbol ()
  "return list (context-symbol module-name-string) or list (context-symbol) if not module context.
context-symbol is one of the following values:
self
method
variable
array
hash
function
installed-module
otherwise
"
  (ignore-errors
    (save-excursion
      (let* ((start (point))
             (start-input (progn (skip-syntax-backward "w_") ;move point
                                 (buffer-substring-no-properties (point) start)))
             (obj-str (buffer-substring-no-properties ;string
                       (or (ignore-errors (save-excursion (forward-char -2) (point)))
                           (point))
                       (save-excursion (or (ignore-errors (backward-sexp)
                                                          (point))
                                           (point)))))
             (context-sym-str-list
              (cond
               ;; fullname
               ;; File::Copy::`!!'
               ((save-excursion
                  (goto-char start)
                  (skip-chars-backward "a-zA-Z0-9_")
                  (let ((str (plcmp-preceding-string 2)))
                    (when (string-equal str "::")
                      (backward-char 2)
                      (let* ((start (point))
                             (end (progn (skip-syntax-backward "w_")
                                         (point)))
                             (obj-str (buffer-substring-no-properties
                                       start
                                       end)))
                        (values 'method obj-str))))))
               ;; package
               ;; $self->`!!'
               ;; __PACKAGE__->`!!'
               ((and (plcmp-method-p)
                     (string-match (rx bol
                                       (or "$self"
                                           "__PACKAGE__")
                                       eol)
                                   obj-str))
                (list 'package))
               ;; method
               ;; Foo->`!!'
               ((and (plcmp-method-p)
                     (stringp obj-str))
                (list 'method obj-str))
               ;; variable
               ;; $foo`!!'
               ((string-equal "$" (plcmp-preceding-string 1))
                (list 'variable))
               ;; array
               ((string-equal "@" (plcmp-preceding-string 1))
                (list 'array))
               ;; hash
               ((string-equal "%" (plcmp-preceding-string 1))
                (list 'array))
               ;; function
               ((string-equal "&" (plcmp-preceding-string 1))
                (list 'function))
               ;; installed-module
               ;; use `!!'
               ((string-match (rx bol (* space) "use" (+ space))
                              (buffer-substring (point-at-bol) (point)))
                (list 'installed-module))
               (t
                (list 'otherwise)))))
        (prog1 context-sym-str-list
          (plcmp-log "plcmp--get-context-symbol: %s" context-sym-str-list))))))

;; TODO
(defun plcmp-get-sources-for-smart-complete ()
  "return sources"
  (let* ((context-sym-str-list (plcmp--get-context-symbol))
         (ctx-sym (first context-sym-str-list))
         (module-name (second context-sym-str-list)))
    (let ((all-sources (plcmp--sources-for-smart-complete)))
      (case ctx-sym
        (method (let ((sources (plcmp-re-sort-sources "method"
                                                      all-sources)))
                  (if (stringp module-name)
                      (let ((obj (assoc-default module-name plcmp-obj-instance-of-module-maybe-alist)))
                        (if (and obj
                                 (stringp obj))
                            (plcmp-re-sort-sources obj sources)
                          (plcmp-re-sort-sources module-name sources)))
                    sources)))
        (variable (plcmp-re-sort-sources "variable"
                                         all-sources))
        (array (plcmp-re-sort-sources  "array"
                                       all-sources))
        (hash (plcmp-re-sort-sources "hash"
                                     all-sources))
        (function (plcmp-re-sort-sources "function"
                                         all-sources))
        (installed-module (plcmp-re-sort-sources "installed modules"
                                                 all-sources))
        (otherwise (let* ((sources (plcmp-re-sort-sources "method"
                                                         all-sources
                                                         t))
                          (sources (plcmp-re-sort-sources "dabbrev"
                                                          sources)))
                     sources))))))

(define-plcmp-command smart-complete ()
  (plcmp-log "smart-complete called line: %s`!!'"
             (buffer-substring (point-at-bol) (point)))
  (anything (plcmp-get-sources-for-smart-complete) plcmp-initial-input))


;;; complete-variables
(defvar plcmp-completion-variable-static-sources
  '(
    plcmp-anything-source-completion-builtin-variables
    ))

(defun plcmp-get-sources-for-complete-variables ()
  (append
   (plcmp-get-sources-other-perl-buffers-variable)
   plcmp-completion-variable-static-sources
   ))

(define-plcmp-command complete-variables ()
  (anything (plcmp-get-sources-for-complete-variables) plcmp-initial-input))

;;; complete-arrays
(define-plcmp-command complete-arrays ()
  (anything (plcmp-get-sources-other-perl-buffers-arrays) plcmp-initial-input))

;;; complete-hashes
(define-plcmp-command complete-hashes ()
  (anything (plcmp-get-sources-other-perl-buffers-hashes) plcmp-initial-input))

;;; complete-functions
(defun plcmp-get-sources-for-complete-functions ()
  (append
   (list plcmp-anything-source-completion-builtin-functions)
   (plcmp-get-sources-other-perl-buffers-functions)
   (plcmp-get-sources-methods plcmp-using-modules)))

(define-plcmp-command complete-functions ()
  (anything (plcmp-get-sources-for-complete-functions) plcmp-initial-input))

;;; complete-methods
(define-plcmp-command complete-methods ()
  (anything (plcmp-get-sources-methods plcmp-using-modules) plcmp-initial-input))

;;; complete-modules
(define-plcmp-command complete-modules ()
  (anything '(plcmp-anything-source-completion-using-modules
              plcmp-anything-source-completion-installed-modules)
            plcmp-initial-input))


;;; document
(defvar plcmp-show-doc-sources
  '(
    plcmp-anything-source-doc-using-modules
    plcmp-anything-source-doc-man-pages
    plcmp-anything-source-doc-installed-modules
    ))
(define-plcmp-command show-doc ()
  (anything plcmp-show-doc-sources))

(define-plcmp-command show-doc-at-point ()
  (anything plcmp-show-doc-sources (or (thing-at-point 'symbol) "")))
;;; other commands
(defun plcmp-cmd-menu ()
  (interactive)
  (anything '(plcmp-anything-source-menu)))

(defun plcmp-cmd-clear-all-caches ()
  (interactive)
  (dolist (variable plcmp--cached-variables)
    (set variable nil))
  (run-hooks 'plcmp-clear-all-caches-hook)
  (or plcmp-installed-modules
      (plcmp-with-set-perl5-lib
       (plcmp--installed-modules-asynchronously)))
  (message "cleared all caches and getting installed modules asynchronously"))

;; TODO
(defun plcmp-cmd-show-environment ()
  (interactive)
  (require 'custom)
  (let* ((decode-fn (lambda (s)
                      (if enable-multibyte-characters
                          (decode-coding-string s locale-coding-system t)
                        s)))
         (buf (get-buffer-create "*perl-completion show environment*"))
         (customs)
         (commands)
         (plcmp-symbols (loop for sym being the symbols
                              for s = (symbol-name sym)
                              when (and (string-match "^plcmp-" s)
                                        (custom-variable-p sym))
                              collect s into custom-syms
                              when (string-match (concat "^" "plcmp-cmd-") s)
                              collect s into command-syms
                              finally do (setq customs custom-syms
                                               commands command-syms)))
         (perl5-lib (format "this buffer's PERL5LIB: %s\n\n"
                            (plcmp-with-set-perl5-lib
                             (require 'env)
                             (or (getenv "PERL5LIB") ""))))
         (additional-lib-directories (format "plcmp-additional-lib-directories: %S\n"
                                             plcmp--PERL5LIB-directories)))
    (with-current-buffer buf
      (erase-buffer)
      ;; set perl5lib
      (insert additional-lib-directories)
      (insert perl5-lib)
      ;; customize-variables
      (insert "customize-variables:\n\n")
      (plcmp-insert-each-line customs)
      (insert "\n\n")
      ;; commands
      (insert "commands:\n\n")
      (plcmp-insert-each-line commands)
      (insert "\n\n")
      ;; process-environment
      (insert "process-environment:\n\n")
      (loop for env in process-environment
            do (progn (insert (funcall decode-fn (or env "")))
                      (insert "\n")))
      (goto-char (point-min))
      (switch-to-buffer buf))))

(defun plcmp-cmd-update-check ()
  (interactive)
  (when (require 'url nil t)
    (let* ((uri "http://svn.coderepos.org/share/lang/elisp/perl-completion/trunk/perl-completion.el")
           (buf (url-retrieve-synchronously uri))
           (re (rx "plcmp-version " (group (+ (or (any digit) "."))))))
      (with-current-buffer buf
        (goto-char (point-min))
        (let ((trunk-version (prog1 (when (re-search-forward re nil t)
                                      (string-to-number (match-string-no-properties 1)))
                               (kill-buffer buf))))
          (if (< plcmp-version trunk-version)
              (when (y-or-n-p "new version available. Open URL in the default browser? ")
                (browse-url uri))
            (message "%s is currently the newest version" plcmp-version)))))))


;; set-perl5lib
(defun plcmp-cmd-set-additional-lib-directory ()
  "ask directory, then set directory to `plcmp--PERL5LIB-directories'"
  (interactive)
  (let* ((dir (read-directory-name "set to PERL5LIB(this buffer only): " nil nil t))
         (dir (expand-file-name dir))
         (dir (directory-file-name dir)))
    (when (and (stringp dir)
               (file-exists-p dir))
      (add-to-list 'plcmp--PERL5LIB-directories dir)
      (message "added %s to PERL5LIB" dir))))



;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Anything commands
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; prefix: plcmp-acmd-
;; plcmp-acmd == plcmp-anything-command

;; util
(defun plcmp-re-select-action (action-name-re)
  (let* ((actions (anything-attr 'action))
         (action (assoc-if (lambda (s) (string-match action-name-re s))
                           actions)))
    (cond
     ((and action (cdr action))
      (setq anything-saved-action (cdr action))
      (anything-exit-minibuffer))
     (t
      (message "no action-name %s current source %s" action-name-re (anything-attr 'name))))))

(defun plcmp-acmd-occur ()
  (interactive)
  (plcmp-re-select-action "^Occur"))

(defun plcmp-acmd-show-doc ()
  (interactive)
  (plcmp-re-select-action "^show"))

(defun plcmp-acmd-open-related-file ()
  (interactive)
  (plcmp-re-select-action "^open"))

(defun plcmp-acmd-persistent-look ()
  (interactive)
  (anything-execute-persistent-action))

(defun plcmp-acmd-goto-looking-point (&optional pop-to-buffer)
  (interactive "P")
  (lexical-let ((looking-point (first plcmp-look-current-positions))
                (buffer (second plcmp-look-current-positions))
                (show-fn (or pop-to-buffer 'pop-to-buffer 'switch-to-buffer)))
    (if (and (numberp looking-point)
             (bufferp buffer))
        (progn (setq anything-saved-action
                     (lambda (ignore)
                       (funcall show-fn buffer)
                       (goto-char looking-point)))
               (anything-exit-minibuffer))
      (message "not looking buffer"))))


;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; Mode
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define-minor-mode perl-completion-mode "" nil plcmp-default-lighter plcmp-mode-map
  (or plcmp-installed-modules
      (plcmp-with-set-perl5-lib
       (plcmp--installed-modules-asynchronously))))


;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; auto-complete.el
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; ac == auto-complete
(defvar plcmp-ac-candidates-limit 1000)
(defvar ac-source-perl-completion
  '((candidates . plcmp-ac-candidates)))

(defvar ac-source-perl-completion-patial
  '((candidates . plcmp-ac-candidates-patial)))

(defun plcmp-ac-candidates ()
  (plcmp-ignore-errors
   (when (and (eq major-mode 'cperl-mode)
              (boundp 'perl-completion-mode)
              perl-completion-mode)
     (plcmp-initialize-variables)
     (let ((words (plcmp-ac-make-cands)))
       (plcmp-log "plcmp-ac-candidates words: %S" words)
       (let ((cands (loop with count = 0
                          with ret
                          for w in words
                          when (and (stringp w)
                                    (string-match (concat "^" ac-target) w)
                                    (not (string= ac-target w)))
                          do (progn (push w ret)
                                    (incf count)
                                    (when (= count plcmp-ac-candidates-limit)
                                      (return (nreverse ret))))
                          finally return (nreverse ret))))
         (prog1 cands
           (plcmp-log "plcmp-ac-candidates return: %s" cands)))))))

(defun plcmp-ac-make-cands ()
  (multiple-value-bind (ctx module-name) (plcmp--get-context-symbol)
    (case ctx
      (method
       (append
        (plcmp-ac-methods)
        (plcmp-get-buffer-dabbrevs)
        plcmp-builtin-functions
        plcmp-builtin-variables
        plcmp-using-modules
        plcmp-installed-modules))
      (otherwise
       (append
        (plcmp-get-buffer-dabbrevs)
        plcmp-builtin-functions
        plcmp-builtin-variables
        plcmp-using-modules
        plcmp-installed-modules
        (plcmp-ac-methods))))))

(defun plcmp-ac-methods ()
  (loop for module-name in plcmp-using-modules
        append (assoc-default module-name plcmp-module-methods-alist)))

;;; patial
(defun plcmp-ac-candidates-patial ()
  (plcmp-ignore-errors
   (when (and (eq major-mode 'cperl-mode)
              (boundp 'perl-completion-mode)
              perl-completion-mode)
     (plcmp-initialize-variables)
     (let ((words (plcmp-ac-make-cands)))
       (plcmp-log "plcmp-ac-candidates words: %S" words)
       (let ((cands (loop with count = 0
                          with ret
                          for w in words
                          when (and (stringp w)
                                    (string-match ac-target w)
                                    (not (string= ac-target w)))
                          do (progn (push w ret)
                                    (incf count)
                                    (when (= count plcmp-ac-candidates-limit)
                                      (return (nreverse ret))))
                          finally return (nreverse ret))))
         (prog1 cands
           (plcmp-log "plcmp-ac-candidates return: %s" cands)))))))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Extend find-file
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(add-to-list 'ffap-alist
             '(cperl-mode . plcmp-ffap-perl))

(defun plcmp-ffap-perl (module)
  ;; dont use argument MODULE
  ;; because `ffap-file-at-point' returns wrong word.
  ;; e.x, (cursor is `!!')
  ;; Net::CI`!!'DR::MobileJP
  ;; `ffap-file-at-point' returns CIDR
  (unless (plcmp-tramp-p)
    (let ((module (thing-at-point 'symbol)))
      (ignore-errors
        (and (plcmp-module-p module)
             (plcmp-get-module-file-path module))))))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Utils for editting perl
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
(defcustom plcmp-coding-system nil
  "if this variable's value is non-nil,
value is let-bound for `coding-system-for-read' and `coding-system-for-write'.
must be one of coding-system."
  :type 'symbol
  :group 'perl-completion)

(defvar plcmp-eval-output-buf-name "*perl output*")

(defmacro plcmp-with-specify-coding-system (&rest body)
  `(let ((coding-system-for-write plcmp-coding-system)
         (coding-system-for-read plcmp-coding-system))
     (progn ,@body)))
(def-edebug-spec plcmp-with-specify-coding-system t)


(defun* plcmp-async-do
    (&key command args buffer-name
          (callback 'identity)
          (errorback (lambda() (message (buffer-string)))))
  (plcmp-with-specify-coding-system
   (lexical-let ((buf (with-current-buffer (get-buffer-create buffer-name)
                        (erase-buffer)
                        (current-buffer)))
                 (callback callback)
                 (errorback errorback))
     (lexical-let
         ((sentinel (lambda (proc event)
                      (cond ((and (string= event "finished\n")
                                  (= (process-exit-status proc) 0))
                             (with-current-buffer buf
                               (funcall callback)))
                            ((and (string= event "finished\n")
                                  (/= (process-exit-status proc) 0))
                             (with-current-buffer buf
                               (funcall errorback)))
                            (t
                             (funcall errorback))))))
       (set-process-sentinel (apply 'start-process command buf command args) sentinel)))))


(defun* plcmp--eval-async (&key callback perl-code)
  (assert (and (functionp callback) (stringp perl-code)))
  (lexical-let ((buf-name plcmp-eval-output-buf-name)
                (perl-code perl-code))
    (plcmp-async-do
     :buffer-name buf-name
     :command (plcmp-get-perl-command)
     :args `("-e"
             ,perl-code)
     :callback callback
     :errorback (lambda ()
                  (switch-to-buffer buf-name))
     )))

(defun plcmp-cmd-eval-buffer ()
  (interactive)
  (plcmp--eval-async
   :callback (lambda ()
               (save-selected-window
                 (pop-to-buffer plcmp-eval-output-buf-name)
                 (goto-char (point-min))))
   :perl-code (buffer-string)))

(defun plcmp-cmd-eval-buffer-and-go ()
  (interactive)
  (plcmp--eval-async
   :callback (lambda ()
               (let ((b plcmp-eval-output-buf-name))
                 (with-current-buffer b (goto-char (point-min)))
                 (switch-to-buffer b)))
   :perl-code (buffer-string)))

(defun plcmp-cmd-eval-on-region (beg end)
  "Run selected region as Perl code asynchronously"
  (interactive "r")
  (plcmp--eval-async
   :callback (lambda ()
               (save-selected-window
                 (pop-to-buffer plcmp-eval-output-buf-name)))
   :perl-code (buffer-substring-no-properties
               beg end)))


;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Integration with anything-show-completion
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; rubikitch's patch. thanks!! :)
(when (require 'anything-show-completion nil t)
  (dolist (f '(plcmp-cmd-smart-complete
               plcmp-cmd-complete-all
               plcmp-cmd-complete-methods
               plcmp-cmd-complete-hashes
               plcmp-cmd-complete-functions
               plcmp-cmd-complete-variables
               plcmp-cmd-complete-modules
               plcmp-cmd-complete-arrays))
    (use-anything-show-completion f '(length plcmp-real-initial-input))))

;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;;; Test
;;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "plcmp-get-face-words")
      (expect nil
        (let ((b (get-buffer-create "*plcmp-test*"))
              (cperl-mode-hook nil))
          (with-current-buffer b
            (cperl-mode)
            (plcmp-get-face-words))))
      (expect "$test"
        (let ((b (get-buffer-create "*plcmp-test*"))
              (cperl-mode-hook nil))
          (with-current-buffer b
            (erase-buffer)
            (insert "my $test = 'hoge';\n")
            (cperl-mode)
            (font-lock-mode t)
            (font-lock-fontify-region (point-min) (point-max))
            (prog1 (car (plcmp-get-face-words))
              (kill-buffer b)))))
      (desc "plcmp--mk-module-source")
      (expect nil
        (plcmp--mk-module-source nil))
      (expect nil
        (let ((plcmp-module-methods-alist nil))
          (plcmp--mk-module-source "test")))
      (expect '((name . "module Methods") (action ("Insert" . plcmp-insert) ("Show doc" lambda (candidate) (let* ((module (plcmp-get-current-module-name)) (buf (plcmp-get-man-buffer module (quote module)))) (save-selected-window (pop-to-buffer buf)))) ("Show doc and go" lambda (candidate) (let* ((module (plcmp-get-current-module-name)) (buf (plcmp-get-man-buffer module (quote module)))) (pop-to-buffer buf))) ("Open module file" lambda (method) (let ((module (plcmp-get-current-module-name))) (plcmp-open-module-file module))) ("Open module file other window" lambda (method) (let ((module-name (plcmp-get-current-module-name))) (plcmp-open-module-file module-name (quote pop-to-buffer)))) ("Open module file other frame" lambda (candidate) (let ((module-name (plcmp-get-current-module-name))) (plcmp-open-module-file module-name (quote switch-to-buffer-other-frame)))) ("Occur module file" lambda (candidate) (let ((module-name (plcmp-get-current-module-name))) (plcmp-open-module-file module-name (quote switch-to-buffer)) (funcall (plcmp-get-occur-fn) candidate nil))) ("Add to kill-ring" . kill-new) ("Insert source name" lambda (candidate) (let ((name (plcmp-anything-get-current-source-name))) (and (stringp name) (insert name))))) (init lambda nil (with-current-buffer (anything-candidate-buffer (quote global)) (plcmp-insert-each-line (quote ("method" "method2"))))) (candidates-in-buffer) (persistent-action lambda (candidate) (let ((module-name (plcmp-get-current-module-name))) (plcmp-open-doc module-name) (plcmp-re-search-forward-fontify (regexp-quote candidate)))))
        (let ((plcmp-module-methods-alist '(("module" . ("method" "method2")))))
          (plcmp--mk-module-source "module")))

      (desc "plcmp-tramp-p")
      (expect t
        (require 'tramp)
        (stub plcmp-get-current-directory => "/tramp:path/to/")
        (when (plcmp-tramp-p)
          t))

      (desc "plcmp-sort-sources")
      (expect 'plcmp-anything-source-completion-builtin-variables
        (car (plcmp-re-sort-sources "variables" plcmp-completion-all-static-sources)))

      (desc "plcmp--get-lib-path")
      (expect "~/c/test/lib"
        (stub file-exists-p => t)
        (stub plcmp-get-current-directory => "~/c/test/lib/Test/TT/Test/")
        (plcmp--get-lib-path))
      (expect "~/c/test/lib"
        (stub file-exists-p => t)
        (stub plcmp-get-current-directory => "~/c/test/lib/Test/TT/Test")
        (plcmp--get-lib-path))
      (expect "~/c/hoge/test/lib/Test/TT/Test/lib"
        (stub plcmp-get-current-directory => "~/c/hoge/test/lib/Test/TT/Test/lib/test")
        (stub file-exists-p => t)
        (plcmp--get-lib-path))
      (expect "~/c/hoge/test/lib/Test/TT/Test/lib"
        (stub plcmp-get-current-directory => "~/c/hoge/test/lib/Test/TT/Test/lib/test/")
        (stub file-exists-p => t)
        (plcmp--get-lib-path))
      (expect nil
        (stub plcmp-get-current-directory => "")
        (plcmp--get-lib-path))

      (desc "plcmp--get-lib-path-list-liberal")
      (expect (mapcar 'file-relative-name '("~/c/remedie/extlib" "~/c/remedie/lib"))
        (stub plcmp-get-current-directory => "~/c/remedie/bin/")
        (stub file-exists-p => t)
        (mapcar 'file-relative-name  (plcmp--get-lib-path-list-liberal)))

      (desc "plcmp-extra-using-modules")
      (expect '("LWP::UserAgent")
        (stub plcmp--get-using-modules-uses-and-requires => nil)
        (let ((plcmp-extra-using-modules '("LWP::UserAgent")))
          (plcmp--using-modules-marge-extra-modules (plcmp--get-using-modules-uses-and-requires))))
      (expect '("HTTP::Response" "LWP::UserAgent")
        (stub plcmp--get-using-modules-uses-and-requires => '("LWP::UserAgent"))
        (let ((plcmp-extra-using-modules '(("LWP::UserAgent" . "HTTP::Response"))))
          (plcmp--using-modules-marge-extra-modules (plcmp--get-using-modules-uses-and-requires))))
      (desc "plcmp--using-modules-filter")
      (expect '("b" "a")
        (let ((ls '("a" "strict" "b"))
              (plcmp-module-filter-list '("strict")))
          (plcmp--using-modules-filter ls)))
      (expect nil
        (let ((ls '("strict"))
              (plcmp-module-filter-list '("strict")))
          (plcmp--using-modules-filter ls)))
      (expect '("LWP::UserAgent")
        (let ((ls '("strict" "LWP::UserAgent"))
              (plcmp-module-filter-list '("strict" "warning")))
          (plcmp--using-modules-filter ls)))
      (desc "plcmp-ffap-perl tramp")
      (expect "called"
        (stub plcmp-get-module-file-path => "called")
        (stub plcmp-tramp-p => nil)
        (stub thing-at-point => "Some::Module")
        (plcmp-ffap-perl "dummy"))
      (expect nil
        (stub plcmp-get-module-file-path => "called")
        (stub plcmp-tramp-p => t)
        (stub thing-at-point => "Some::Module")
        (plcmp-ffap-perl "dummy"))
      )))

(provide 'perl-completion)
;;; perl-completion.el ends here
