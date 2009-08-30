;;; mcskels.el --- C skeletons.

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: 5 Jan 2003
;; Maintainer: none, if you want be a maintainer please e-mail me.
;; Keywords: c
;; Version: 0.1
;; X-CVS: $Id$

;; This file is NOT part of XEmacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;   Use this to quickly insert C statements.
;;
;; Notes about `mc-comp-list':
;;
;;   `mc-comp-list' is a list of completions for c statements after
;;   statements is passed to `mc-insert-stat-complete' it will look-up
;;   for a symbol mc-<stat>-skel. If it is variable, then
;;   `mc-insert-skeleton' will be called, else if it is function then
;;   it will be called interactively.
;;
;; Notes about `mc-push-stack':
;;
;;   `mc-push-stack' is a stack of cursor points. You can use it to
;;   enhance speed of cursor moving inside skeleton.
;;

;;; Customisation:
;;
;; You can add your stats to `mc-comp-list', Do not forget to describe
;; action for it.
;;

;;; TODO:
;;
;; * Add templates
;;

;;; Code:
;;

(require 'cc-mode)

(defcustom mc-use-iswitchb t
  "*Non-nil meat use iswitchb package for completing-read.")

(defmacro mc-define-command (cmd skel)
  "Defines mc command with CMD name, which inserts SKEL."
  `(progn (fset ,cmd
		(lambda () 
		  (interactive)
		  (mc-insert-skeleton ,skel)))
	  ,cmd))

(defvar mc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'mc-insert-stat-complete) ; Syn: 'insert'
    (define-key map (kbd "c") (mc-define-command 'mc-insert-if mc-if-skel))	; Syn: 'condition'
    (define-key map (kbd "j") (mc-define-command 'mc-insert-just-check mc-if-check-skel)) ; Syn: 'just check'
    (define-key map (kbd "e") (mc-define-command 'mc-insert-if-else mc-if-check-skel)) ; Syn: 'else stat'
    (define-key map (kbd "l") (mc-define-command 'mc-insert-for mc-for-skel)) ; Syn 'for loop'
    (define-key map (kbd "r") (mc-define-command 'mc-insert-errcheck-skel mc-errck-skel)) ; Syn: 'eRr check'
    (define-key map (kbd "w") (mc-define-command 'mc-insert-while mc-while-skel)) ; Syn: 'while'
    (define-key map (kbd "s") (mc-define-command 'mc-insert-switch mc-switch-skel))	; Syn: 'switch'
    (define-key map (kbd "m") 'mc-insert-malloc) ; Syn: 'malloc'
    (define-key map (kbd "p") 'mc-insert-printf) ; Syn: 'printf'
    (define-key map (kbd "d") 'mc-insert-function) ; Syn: 'define function'
    (define-key map (kbd "o") 'mc-insert-open) ; Syn: 'open'
    (define-key map (kbd "f") 'mc-insert-fcntl) ; Syn: 'fcntl'
    (define-key map (kbd "x") 'mc-insert-exit) ; Syn: 'eXit'
    map)
  "Keymap for mc skeletons.")

;; Errors in format [code name short-desc long-desc]
(defvar mc-errors-list
  (list [0 "Undefined error." "Not used."]

	[1 "EPERM" "Operation not permitted." "An attempt was made to
perform an operation limited to processes with appropriate privileges
or to theowner of a file or other resources."]

	[2 "ENOENT" "No such file or directory."  "A component of a
specified pathnamedid not exist, or the pathname was an empty
string."]

	[3 "ESRCH" "No such process."  "No process could be found
corresponding to thatspecified by the given process ID."]

	[4 "EINTR" "Interrupted function call."  "An asynchronous
signal (such asSIGINT or SIGQUIT) was caught by the process during the
execution of an interruptible function.  If the signal handler
performs anormal return, the interrupted function call will seem to
have returned the error condition."]

	[5 "EIO" "Input/output error."  "Some physical input or output
error occurred. This error will not be reported until a subsequent
operation on the same file descriptor and may be lost (over written)
by any subsequent errors."]

	[6 "ENXIO" "No such device or address."  "Input or output on a
special file referred to a device that did not exist, or made a
request beyond the limits of the device.  This error may also occur
when, for example, a tape drive is not online or no disk pack is
loaded on a drive."]

	[7 "E2BIG" "Arg list too long."  "The number of bytes used for
the argument and environment list of the new process exceeded the
current limit of 65536 bytes (NCARGS in <sys/param.h>)."]

	[8 "ENOEXEC" "Exec format error."  "A request was made to
execute a file that, although it has the appropriate permissions, was
not in the format required for an executable file."]

	[9 "EBADF" "Bad file descriptor."  "A file descriptor argument
was out of range, referred to no open file, or a read (write) request
was made to a file that was only open for writing (reading)."]

	[10 "ECHILD" "No child processes." "A wait(2) or waitpid(2)
function was executed by a process that had no existing or
unwaited-for child processes."]

	[11 "EDEADLK" "Resource deadlock avoided." "An attempt was
made to lock a system resource that would have resulted in a deadlock
situation."]

	[12 "ENOMEM" "Cannot allocate memory." "The new process image
required more memory than was allowed by the hardware or by
system-imposed memory management constraints.  A lack of swap space is
normally temporary; however, a lack of core is not.  Soft limits may
be increased to their corresponding hard limits."]

	[13 "EACCES" "Permission denied." "An attempt was made to
access a file in a way forbidden by its file access permissions."]

	[14 "EFAULT" "Bad address." "The system detected an invalid
address in attempting to use an argument of a call."]

	[15 "ENOTBLK" "Not a block device." "A block device operation
was attempted on a non-block device or file."]

	[16 "EBUSY" "Resource busy." "An attempt to use a system
resource which was in use at the time in a manner which would have
conflicted with the request."]

	[17 "EEXIST" "File exists." "An existing file was mentioned in
an inappropriate context, for instance, as the new link name in a
link(2) function."]

	[18 "EXDEV" "Improper link." "A hard link to a file on another
file system was attempted."]

	[19 "ENODEV" "Operation not supported by device." "An attempt
was made to apply an inappropriate function to a device, for example,
trying to read a write-only device such as a printer."]

	[20 "ENOTDIR" "Not a directory." "A component of the specified
pathname existed, but it was not a directory, when a directory was
expected."]

	[21 "EISDIR" "Is a directory." "An attempt was made to open a
directory with write mode specified."]

	[22 "EINVAL" "Invalid argument." "Some invalid argument was
supplied.  (For example, specifying an undefined signal to a signal(3)
or kill(2) function)."]

	[23 "ENFILE" "Too many open files in system." "Maximum number
of file descriptors allowable on the system has been reached and a
requests for an open cannot be satisfied until at least one has been
closed."]

	[24 "EMFILE" "Too many open files." "<As released, the limit
on the number of open files per process is 64.> The getdtablesize(2)
function will obtain the current limit."]

	[25 "ENOTTY" "Inappropriate ioctl for device." "A control
function (see ioctl(2)) was attempted for a file or special device for
which the operation was inappropriate."]

	[26 "ETXTBSY" "Text file busy." "The new process was a pure
procedure (shared text) file which was open for writing by another
process, or while the pure procedure file was being executed an
open(2) call requested write access."]

	[27 "EFBIG" "File too large." "The size of a file exceeded the
maximum (about 2.1E9 bytes)."]

	[28 "ENOSPC" "Device out of space." "A write(2) to an ordinary
file, the creation of a directory or symbolic link, or the creation of
a directory entry failed because no more disk blocks were available on
the file system, or the allocation of an inode for a newly created
file failed because no more inodes were available on the file
system."]

	[29 "ESPIPE" "Illegal seek." "An lseek(2) function was issued
on a socket, pipe or FIFO."]

	[30 "EROFS" "Read-only file system." "An attempt was made to
modify a file or directory on a file system that was read-only at the
time."]

	[31 "EMLINK" "Too many links." "Maximum allowable hard links
to a single file has been exceeded (limit of 32767 hard links per
file)."]

	[32 "EPIPE" "Broken pipe." "A write on a pipe, socket or FIFO
for which there is no process to read the data."]

	[33 "EDOM" "Numerical argument out of domain." "A numerical
input argument was outside the defined domain of the mathematical
function."]

	[34 "ERANGE" "Numerical result out of range." "A numerical
result of the function was too large to fit in the available space
\\(perhaps exceeded precision\\)."]

	[35 "EAGAIN" "Resource temporarily unavailable." "This is a
temporary condition and later calls to the same routine may complete
normally."]

	[36 "EINPROGRESS" "Operation now in progress." "An operation
that takes a long time to complete (such as a connect(2)) was
attempted on a non-blocking object (see fcntl(2))."]

	[37 "EALREADY" "Operation already in progress." "An operation
was attempted on a non-blocking object that already had an operation
in progress."]

	[38 "ENOTSOCK" "Socket operation on non-socket."
"Self-explanatory."]

	[39 "EDESTADDRREQ" "Destination address required." "A required
address was omitted from an operation on a socket."]

	[40 "EMSGSIZE" "Message too long." "A message sent on a socket
was larger than the internal message buffer or some other network
limit."]

	[41 "EPROTOTYPE" "Protocol wrong type for socket." "A protocol
was specified that does not support the semantics of the socket type
requested.  For example, you cannot use the ARPA Internet UDP protocol
with type SOCK_STREAM."]

	[42 "ENOPROTOOPT" "Protocol not available." "A bad option or
level was specified in a getsockopt(2) or setsockopt(2) call."]

	[43 "EPROTONOSUPPORT" "Protocol not supported." "The protocol
has not been configured into the system or no implementation for it
exists."]

	[44 "ESOCKTNOSUPPORT" "Socket type not supported." "The
support for the socket type has not been configured into the system or
no implementation for it exists."]

	[45 "EOPNOTSUPP" "Operation not supported." "The attempted
operation is not supported for the type of object referenced.  Usually
this occurs when a file descriptor refers to a file or socket that
cannot support this operation, for example, trying to accept a
connection on a datagram socket."]

	[46 "EPFNOSUPPORT" "Protocol family not supported." "The
protocol family has not been configured into the system or no
implementation for it exists."]

	[47 "EAFNOSUPPORT" "Address family not supported by protocol
family." "An address incompatible with the requested protocol was
used.  For example, you shouldn't necessarily expect to be able to use
NS addresses with ARPA Internet protocols."]

	[48 "EADDRINUSE" "Address already in use." "Only one usage of
each address is normally permitted."]

	[49 "EADDRNOTAVAIL" "Cannot assign requested address."
"Normally results from an attempt to create a socket with an address
not on this machine."]

	[50 "ENETDOWN" "Network is down." "A socket operation
encountered a dead network."]

	[51 "ENETUNREACH" "Network is unreachable." "A socket
operation was attempted to an unreachable network."]

	[52 "ENETRESET" "Network dropped connection on reset." "The
host you were connected to crashed and rebooted."]

	[53 "ECONNABORTED" "Software caused connection abort." "A
connection abort was caused internal to your host machine."]

	[54 "ECONNRESET" "Connection reset by peer." "A connection was
forcibly closed by a peer.  This normally results from a loss of the
connection on the remote socket due to a timeout or a reboot."]

	[55 "ENOBUFS" "No buffer space available." "An operation on a
socket or pipe was not performed because the system lacked sufficient
buffer space or because a queue was full."]

	[56 "EISCONN" "Socket is already connected." "A connect(2)
request was made on an already connected socket; or, a sendto(2) or
sendmsg(2) request on a connected socket specified a destination when
already connected."]

	[57 "ENOTCONN" "Socket is not connected." "An request to send
or receive data was disallowed because the socket was not connected
and (when sending on a datagram socket) no address was supplied."]

	[58 "ESHUTDOWN" "Cannot send after socket shutdown." "A
request to send data was disallowed because the socket had already
been shut down with a previous shutdown(2) call."]

	[60 "ETIMEDOUT" "Operation timed out." "A connect(2) or
send(2) request failed because the connected party did not properly
respond after a period of time.  (The timeout period is dependent on
the communication protocol.)"]

	[61 "ECONNREFUSED" "Connection refused." "No connection could
be made because the target machine actively refused it.  This usually
results from trying to connect to a service that is inactive on the
foreign host."]

	[62 "ELOOP" "Too many levels of symbolic links." "A path name
lookup involved more than 32 (MAXSYMLINKS) symbolic links."]

	[63 "ENAMETOOLONG" "File name too long." "A component of a
path name exceeded 255 (MAXNAMELEN) characters, or an entire path name
exceeded 1023 (MAXPATHLEN-1) characters."]

	[64 "EHOSTDOWN" "Host is down." "A socket operation failed
because the destination host was down."]

	[65 "EHOSTUNREACH" "No route to host." "A socket operation was
attempted to an unreachable host."]

	[66 "ENOTEMPTY" "Directory not empty." "A directory with
entries other than `.' and `..' was supplied to a remove directory or
rename call."]

	[67 "EPROCLIM" "Too many processes." "Too many processes."]

	[68 "EUSERS" "Too many users." "The quota system ran out of
table entries."]

	[69 "EDQUOT" "Disc quota exceeded." "A write(2) to an ordinary
file, the creation of a directory or symbolic link, or the creation of
a directory entry failed because the user's quota of disk blocks was
exhausted, or the allocation of an inode for a newly created file
failed because the user's quota of inodes was exhausted."]

	[70 "ESTALE" "Stale NFS file handle." "An attempt was made to
access an open file (on an NFS filesystem) which is now unavailable as
referenced by the file descriptor.  This may indicate the file was
deleted on the NFS server or some other catastrophic event occurred."]

	[72 "EBADRPC" "RPC struct is bad." "Exchange of RPC
information was unsuccessful."]

	[73 "ERPCMISMATCH" "RPC version wrong." "The version of RPC on
the remote peer is not compatible with the local version."]

	[74 "EPROGUNAVAIL" "RPC prog."n"ot avail.  The requested
program is not registered on the remote host."]

	[75 "EPROGMISMATCH" "Program version wrong." "The requested
version of the program is not available on the remote host (RPC)."]

	[76 "EPROCUNAVAIL" "Bad procedure for program." "An RPC call
was attempted for a procedure which doesn't exist in the remote
program."]

	[77 "ENOLCK" "No locks available." "A system-imposed limit on
the number of simultaneous file locks was reached."]

	[78 "ENOSYS" "Function not implemented." "Attempted a system
call that is not available on this system."]

	[79 "EFTYPE" "Inappropriate file type or format." "The file
was the wrong type for the operation, or a data file had the wrong
format."]

	[80 "EAUTH" "Authentication error." "Attempted to use an
invalid authentication ticket to mount a NFS filesystem."]

	[81 "ENEEDAUTH" "Need authenticator." "An authentication
ticket must be obtained before the given NFS filesystem may be
mounted."]

	[82 "EIDRM" "Identifier removed." "An IPC identifier was
removed while the current process was waiting on it."]

	[83 "ENOMSG" "No message of desired type." "An IPC message
queue does not contain a message of the desired type, or a message
catalog does not contain the requested message."]

	[84 "EOVERFLOW" "Value too large to be stored in data type."
"A numerical result of the function was too large to be stored in the
caller provided space."]

	[85 "ECANCELED" "Operation canceled." "The scheduled operation
was canceled."]

	[86 "EILSEQ" "Illegal byte sequence." "While decoding a
multibyte character the function came along an invalid or an
incomplete sequence of bytes or the given wide character is invalid."]
))

;; Exit codes in format [code name short-desc]
(defvar mc-exitcodes-list
  (list [0 "EX_OK" "Successful termination."]

	[64 "EX_USAGE" "The command was used incorrectly, e.g., with
the wrong number of arguments, a bad flag, a bad syntax in a
parameter, or whatever."]

	[65 "EX_DATAERR" "The input data was incorrect in some way.
This should only be used for user's data & not system files."]

	[66 "EX_NOINPUT" "An input file (not a system file) did not
exist or was not readable.  This could also include errors like \"No
message\" to a mailer (if it cared to catch it)."]

	[67 "EX_NOUSER" "The user specified did not exist.  This might
be used for mail addresses or remote logins."]

	[68 "EX_NOHOST" "The host specified did not exist.  This is
used mail addresses or network requests."]

	[69 "EX_UNAVAILABLE" "A service is unavailable.  This can
occur if a support program or file does not exist.  This can also be
used as a catchall message when something you wanted to do doesn't
work, but you don't know why."]

	[70 "EX_SOFTWARE" "An internal software error has been
detected.  This should be limited to non-operating system related
errors as possible."]

	[71 "EX_OSERR" "An operating system error has been detected.
This is intended to be used for such things as \"cannot fork\",
\"cannot create pipe\", or the like.  It includes things like getuid
returning a user that does not exist in the passwd file."]

	[72 "EX_OSFILE" "Some system file (e.g., /etc/passwd,
/etc/utmp, etc.) does not exist, cannot be opened, or has some sort of
error (e.g., syntax error)."]

      [73 "EX_CANTCREAT" "A (user specified) output file cannot be
created."]

      [74 "EX_IOERR" "An error occurred while doing I/O on some
file."]

      [75 "EX_TEMPFAIL" "Temporary failure, indicating something that
is not really an error.  In sendmail, this means that a mailer (e.g.)
could not create a connection, and the request should be reattempted
later."]

      [76 "EX_PROTOCOL" "The remote system returned something that was
\"not possible\" during a protocol exchange."]

      [77 "EX_NOPERM" "You did not have sufficient permission to
perform the operation.  This is not intended for file system problems,
which should use NOINPUT or CANTCREAT, but rather for higher level
permissions."]  ))

(defvar mc-comp-list
  '(
    ;; C statements
    ("if") ("if-else") ("if-check") ("for")
    ("errck") ("while") ("switch") ("case")
    ("default")
    ;; commonly used stats
    ("getopt") ("socket") ("malloc") ("open")
    ("defun") ("printf") ("fcntl") ("exit")
    )
  "Completion list")

(defvar mc-push-stack nil
  "Where all internal pushes stored")

(defvar mc-if-skel
   '(nil
     > "if (" _ ") {" \n
     > '(progn (mc-push-mark) nil) \n
     > "}" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil)
     nil)
   "*Skeleton for if statement")

(defvar mc-if-else-skel
   '(nil
     > "if (" _ ") {" \n
     > '(progn (mc-push-mark) nil) \n
     > "} else {" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil) \n
     > "}" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil)
     nil)
   "*Skeleton for if-else statement")

(defvar mc-if-check-skel
   '(nil
     > "if (" _ ")" \n
     > '(progn (mc-push-mark) nil) \n
     nil)
   "*Skeleton for simple if-check statement")

(defvar mc-for-skel
   '(nil
     > "for (i = " _ "; i < " '(progn (mc-push-mark) nil) "; i++) {" \n
     > '(progn (mc-push-mark) nil) \n
     > "}" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil)
     nil)
   "*Skeleton for for statement")

(defvar mc-errck-skel
   '(nil
     > "err = " _ ";" \n
     > "if (err < 0) {" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil) \n
     > "}" '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil)
     nil)
   "*Skeleton for err-check statement")

(defvar mc-while-skel
  '(nil
    > "while (" _ ") {" \n
    > '(progn (mc-push-mark) nil) \n
    > "}" '(progn (indent-according-to-mode) nil)
    nil)
   "*Skeleton for while statement")

(defvar mc-switch-skel
  '(nil
    > "switch (" _ ") {" \n
    > '(progn (mc-push-mark) nil) \n
    > "}" '(progn (indent-according-to-mode) nil)
    nil)
   "*Skeleton for switch statement")

(defvar mc-case-skel
  '(nil
    > "case 0" _ ":"  '(progn (indent-according-to-mode) nil) \n
    > "break;" '(progn (indent-according-to-mode) nil)
    ;; Little bit hackerish
    '(progn (goto-char (- skeleton-point 1)) (delete-backward-char 1) (setq skeleton-point (- skeleton-point 2)) nil)
    nil)
  "*Skeleton for case statement")

(defvar mc-default-skel
  '(nil
    > "default:" '(progn (indent-according-to-mode) nil) \n 
    > _ \n
    > "break;" '(progn (indent-according-to-mode) nil)
    nil)
   "*Skeleton for default case statement")

(defun mc-push-mark ()
  "Just push mark at (point), then you can jump to it using \\[universal-argument] \\[set-mark-command]."
  (interactive)
  (add-to-list 'mc-push-stack (point)))

(defun mc-rpush-all-marks ()
  "Real push off all marks stored in mc-push-stack"

  (message (format "Marks set: %d, use C-u C-SPC to jump next."
                   (length mc-push-stack)))

  (while mc-push-stack
      (push-mark (car mc-push-stack) t nil (current-buffer))
      (setq mc-push-stack (cdr mc-push-stack))))

(defun mc-value-from-marker (var &optional marknum)
  "Get current word at most top marker.
VAR should be quoted variable."

  (when (< (length (eval var)) 1)
    (let ((old (get-char-table ?\_ c-mode-syntax-table)))

      (modify-syntax-entry ?\_ "w" c-mode-syntax-table) ; make ?\_ to be part of a word in c-mode

      ;; Try to get value from last mark
      (set var nil)
      (save-excursion
	(goto-char (marker-position (if marknum (nth (1- marknum) mark-ring) (mark-marker t))))
	(when (looking-at "\\sw")
	  (set var (current-word t))))
      (put-char-table ?\_ old c-mode-syntax-table))

    (when (null (eval var))
      (set var (symbol-name var)))))

  
;; Insert skel and push marks if any
(defun mc-insert-skeleton (skel)
  "Insert provided skeleton and push all marks"
  (skeleton-insert skel)
  (mc-rpush-all-marks))

;;; ACTIONS: functions

;; malloc() statement
(defalias 'mc-printf-skel 'mc-insert-printf)

(defun mc-insert-printf (prfx)
  "Insert [f]printf() statement."
  (interactive "P")
  
  (let ((stderr "stderr"))
    (when prfx
      (mc-value-from-marker 'stderr))

    (skeleton-insert
     '(nil
       > (if prfx (concat "fprintf(" stderr ", \"") "printf(\"") _ "\");"
       nil))))

(defalias 'mc-malloc-skel 'mc-insert-malloc) ; so mc-insert-stat-complete will intern properly

(defun mc-insert-malloc (malvar)
  "Insert malloc() statement"
  (interactive "sMalloc to variable: ")

  (mc-value-from-marker 'malvar)

  (skeleton-insert
   '(nil
     > malvar " = " _ "malloc(" '(progn (mc-push-mark) nil) ");" \n
     > (format "if (%s == NULL)" malvar) '(progn (indent-according-to-mode) nil) \n
     > '(progn (mc-push-mark) nil) \n
     nil))
  (mc-rpush-all-marks))

(defalias 'mc-function-skel 'mc-insert-function)

(defun mc-insert-function (arg funame retype)
  "Inserts function which FUNAME which returns value of RETYPE.
Prefix arg specifies how many arguments function accepts."
  (interactive (list
		current-prefix-arg
		(read-from-minibuffer "Function name: ")
		(mc-completing-read "Return type: " '(("int") ("void") ("short")) nil nil "int")))

  (skeleton-insert
   '(nil
     > retype \n
     > funame "(" '(let ((args (prefix-numeric-value arg)))
		     (while (> args 1)
		       (insert " ,")
		       (setq args (1- args))))
     ")" \n
     > "{" '(indent-according-to-mode) \n
     > _ \n
     > "}" '(indent-according-to-mode) \n
     nil)))

;; getopt() stat
(defun mc-getopt-skel (optstr)
  "Insert getop routine"
  (interactive "sGetopt string: ")

  (let ((go-chars (string-to-list optstr)))
    (skeleton-insert
     '(nil
       > (format "while ((ch = getopt(argc, argv, \"%s\")) != -1) {" optstr) \n
       > "switch (ch) {" '(progn (indent-according-to-mode) nil) \n
       > '(progn 
	    (let ((cpnt (point)))
	      (while go-chars
		   (insert (format "case '%c':\n" (car go-chars)))
		   (if (eq (cadr go-chars) ?\:)
		       (progn 
			 (insert (format "strncpy(%c_string, optarg, CHANGEME_LEN);\n"
					 (car go-chars)))
			 (setq go-chars (cddr go-chars)))
		     (progn
		       (insert (format "%c_flag = 1;\n" (car go-chars)))
		       (setq go-chars (cdr go-chars))))
		   (insert "break;")
		   (insert "\n"))
	      
	      (insert "case '?':\n")
	      (insert "default:\n")
	      (insert "usage();\n")
	      (indent-region cpnt (point) nil))
	      nil)
       > "}" '(progn (indent-according-to-mode) nil) \n
       > "}" '(progn (indent-according-to-mode) nil) \n
       > "argc -= optind;" '(progn (indent-according-to-mode) nil) \n
       > "argv += optind;" '(progn (indent-according-to-mode) nil) \n
       > _))))

(defun mc-completing-read-iswitchb (prompt cmpls &optional predicate reqmatch init hist def)
  "Like `completing-read' but using iswitchb package."
  (flet ((iswitchb-make-buflist
	  (default)
	  (setq iswitchb-buflist (mapcar 'car cmpls))))
    (setq iswitchb-buflist (mapcar 'car cmpls))
    (iswitchb-read-buffer prompt init reqmatch)))

(defun mc-completing-read (prompt cmpls &optional predicate reqmatch init hist def)
  "Generic completing-read for mc."
  (funcall (if mc-use-iswitchb 'mc-completing-read-iswitchb 'completing-read)
	   prompt cmpls predicate reqmatch init hist def))
    
(defun mc-socket-skel (type &optional sockname)
  "Insert socket creation skeleton:

type - socket type (tcp, udp, raw [default is tcp]).
sockname - socket variable.
"
  (interactive 
   (list (mc-completing-read "Socket type: " '(("tcp") ("udp") ("raw")) nil t "tcp")
	 (read-from-minibuffer "Socket variable: ")))

  (mc-value-from-marker 'sockname)

  (skeleton-insert
   '(nil
     > (format "%s = socket(AF_INET, %s, 0);" sockname
	       (cond ((string= type "tcp") "SOCK_STREAM")
		     ((string= type "udp") "SOCK_DGRAM")
		     ((string= type "raw") "SOCK_RAW")
		     (t "SOCK_STREAM"))) \n
     > (format "if (%s < 0) {" sockname) \n
     > _ \n
     > "}" '(progn (indent-according-to-mode) nil)
     )))

(defalias 'mc-open-skel 'mc-insert-open)

(defun mc-insert-open (arg fd file &optional type)
  "Insert opening file skeleton:

FD   - file descriptor name.
FILE - filename to open.
TYPE - type of opening (f.i.: O_RDONLY or O_RDWR).
When entering type you may press \\[keyboard-quit] to exit.

If used with prefix arg, insert if-else instead of if statement."
  (interactive
   (list current-prefix-arg
	 (let ((defd ""))
	   (mc-value-from-marker 'defd)
	   (read-from-minibuffer (format "Descriptor variable [%s]: " defd)))
	 (read-from-minibuffer "File to open: ")
	 (let ((rv "")
	       (nt ""))
	   (while (not (string=
			(setq
			 nt
			 (condition-case err
			     (mc-completing-read (format "Open flags (now: %s): " rv)
						 '(("O_RDONLY") ("O_WRONLY") ("O_RDWR") ("O_CREAT") ("O_APPEND")
						   ("O_NONBLOCK") ("O_TRUNC") ("O_EXCL") ("O_SHLOCK") ("O_DIRECT")
						   ("O_FSYNC") ("O_NOFOLLOW"))
						 nil t "O_")
			   (quit "")))
			""))
	     (if (string= rv "")
		 (setq rv nt)
	       (setq rv (concat rv "|" nt))))
	   rv)))

  (mc-value-from-marker 'fd)
  
  (skeleton-insert
   '(nil
     > (format "%s = open(%s, %s);" fd file type) \n
     > (format "if (%s < 0) {" fd) \n
     > _ \n
     > "}" '(progn (indent-according-to-mode) nil)
     > '(progn
	  (if arg
	      (let ((pnt (point)))
		(insert " else {\n")
		(indent-according-to-mode)
		(mc-push-mark)
		(insert "\n}\n")
		(indent-region pnt (point) nil))
	    (insert "\n"))
	  nil)
     > '(progn (indent-according-to-mode) (mc-push-mark) nil)
     ))
  (mc-rpush-all-marks))

(defalias 'mc-fcntl-skel 'mc-insert-fcntl)

(defun mc-insert-fcntl (prefix-arg fd cmd &optional arg)
  "Insert opening file skeleton:

FD   - file descriptor name.
CMD  - fcntl command.
ARG  - fcntl addition argument.
When entering arg you may press \\[keyboard-quit] to exit.

If used with prefix arg, insert if-else instead of if statement."
  (interactive
   (list current-prefix-arg
	 (read-from-minibuffer "Descriptor variable: ")
	 (mc-completing-read "fcntl cmd: "
			     '(("F_DUPFD") ("F_GETFD") ("F_SETFD") ("F_GETFL") ("F_SETFL")
			       ("F_GETOWN") ("F_SETOWN") ("F_GETLK") ("F_SETLK") ("F_SETLKW"))
			     nil t "F_")
	 (read-from-minibuffer "Additional argument (NULL): ")))

  (mc-value-from-marker 'fd)
  
  (skeleton-insert
   '(nil
     > (format "err = fcntl(%s, %s, %s);" fd cmd (if (string= arg "") "NULL" arg)) \n
     > "if (err < 0) {" \n
     > _ \n
     > "}" '(progn (indent-according-to-mode) nil)
     > '(progn
	  (if prefix-arg
	      (let ((pnt (point)))
		(insert " else {\n")
		(indent-according-to-mode)
		(mc-push-mark)
		(insert "\n}\n")
		(indent-region pnt (point) nil))
	    (insert "\n"))
	  nil)
     > '(progn (indent-according-to-mode) (mc-push-mark) nil)
     ))
  (mc-rpush-all-marks))

(defalias 'mc-exit-skel 'mc-insert-exit)

(defun mc-insert-exit (status)
  "Insert exit with STATUS statement."
  (interactive (list (mc-completing-read
		      "Exit status :"
		      (mapcar (lambda (est) (list (aref est 1))) mc-exitcodes-list)
		      nil t "EX_")))

  (skeleton-insert
   '(nil
     > (format "exit(%s);" status)
     )))

;; Interactive action selector
(defun mc-insert-stat-complete (stat)
  "Reads statement or keyword from minibuffer and act according to it.
Default action is to insert stat."
  (interactive
   (let ((val))
     (setq val (mc-completing-read (format "C Statement: ")
				   mc-comp-list nil t))
     (list val)))

  ;; Actions
  (let ((csym (intern-soft (format "mc-%s-skel" stat))))
    (cond ((null csym) (message "Statement `%s' has no action" stat))
	  ((boundp csym) (mc-insert-skeleton (symbol-value csym))) ; call ins skel-func
	  ((functionp csym) (call-interactively csym))
	  (t (message "Statement `%s' has UNKNOWN action" stat)))))


;;; Abbrevs

(defun mc-skel-c-stat (stat)
  "Inserts a statement (outside of a comment).
This should be called from an abbrev."
  (if (nth 4 (parse-partial-sexp 1 (point)))
      (insert stat)
      (skeleton-insert
       '(nil
	 > (format "%s (" stat) _ ") {" \n
	 > '(progn (mc-push-mark) nil) \n
	 > "}" '(progn (indent-according-to-mode) nil)))
      ))

(defun mc-install-keyboard ()
  "Installs keyboard for using mcskels in c-mode with C-M-i as prefix."
  (define-key c-mode-map (kbd "C-M-i") mc-mode-map)
  )

;; use abbrevs C-x ' to expand some C statements
(defun mc-install-abbrevs ()
  "Defines abbrev table for c-mode"
  (define-abbrev-table 'c-mode-abbrev-table 
    '(( "while" "" (lambda () (mc-skel-c-stat "while")) 0 )
      ( "for" "" (lambda () (mc-skel-c-stat "for")) 0 )
      ( "if" "" (lambda () (mc-skel-c-stat "if")) 0 )
      ( "switch" "" (lambda () (mc-skel-c-stat "switch")) 0))))

(provide 'mcskels)

;;; mcskels.el ends here
