;;; cscope.el --- Interface to cscope browser
;; Author: Bill Carpenter.

;; [Note by Stephen Eglen, Tue 19 Aug 2003.  I am placing this on
;; emacswiki.org with Bill's permission so that it gets archived.
;; Note that I have edited the original version of
;; cscope-bind-keys-3deep to allow C-c C-s <key> to work with modern
;; (> 1996) versions of the default C mode.  See
;; http://cscope.sourceforge.net for details on cscope and another
;; Emacs interface.

;; This is free software.
;; The above line was added to satisfy emacswiki.org; see below for an
;; elaboration on what can be done with this source.]

;; original: /anonymous@src.doc.ic.ac.uk:/gnu/EmacsBits/elisp-archive/interfaces/cscope.el.Z
;; From billc@pegasus.ATT.COM Mon Nov 11 20:53:08 1991
;; Xref: cbnewsh comp.emacs:7861 gnu.emacs.sources:558
;; Newsgroups: comp.emacs,gnu.emacs.sources
;; Path: cbnewsh!hos1cad!billc
;; From: billc@pegasus.ATT.COM (Bill Carpenter)
;; Subject: GNUemacs-to-cscope interface, cscope.el
;; Organization: AT&T Bell Laboratories
;; Date: Tue, 12 Nov 1991 01:51:45 GMT
;; Message-ID: <1991Nov12.015145.9840@cbnewsh.cb.att.com>
;; Sender: bill@cbnewsh.cb.att.com (william.j.carpenter)
;; Lines: 909

;; This is an interface from GNUemacs to Joe Steffen's "cscope" C browser.  
;; With this code, you can do things like put your cursor on some C
;; symbol, hit a couple keys and be transported anyplace special that
;; cscope knows about.  See the variable cscope-blurb for more general info.
;;
;; If you use this, I am interested in hearing from you how it went,
;; especially if you find bugs or if you use cscope in some way that
;; this interface doesn't support (or doesn't support very well).
;;
;;  Bill                      William_J_Carpenter@ATT.COM        or
;;  (908) 576-2932            attmail!bill   or   att!pegasus!billc
;;  AT&T Bell Labs / AT&T EasyLink Services               LZ 1E-207

;;cscope.el
;;
;;;
;;; LCD Archive Entry:
;;; cscope|Bill Carpenter|William_J_Carpenter@ATT.COM
;;; |Interface to cscope browser
;;; |91-11-11|1|~/interfaces/cscope.el.Z|
;;;
;;; patchlevel 0,   13-Oct-91  (beta)
;;; patchlevel 1,   11-Nov-91  first general release
;;;
;;; As far as I'm concerned, anyone can do anything they want with
;;; this specific piece of code.  No warranty or promise of support is
;;; offered.  I am interested in hearing reports of bugs or interesting
;;; uses.  Suggestions for interesting enhancements are welcome.

;;; My thanks to the beta testers.  I am especially appreciative of
;;; the helpful feedback received about earlier versions of this from
;;; Mike Balenger and Neal McBurnett.

(defconst cscope-version "patchlevel 1, 11 Nov 91")

(defvar cscope-bindings-2deep nil
  "*If non-nil, then two character bindings are applied when 
\"cscope-bind-keys\" is called.  The two character bindings are mostly
of the form \"C-c letter\".  The reason they are optional is that there
is an elisp coding convention which suggests that those kinds of bindings
should be reserved for users.  The binding of \"C-c C-c\" is not controlled
by this user option variable; it is always applied.")

(defvar cscope-bindings-3deep t
  "*If non-nil, then three character bindings are applied when
\"cscope-bind-keys\" is called.  The three character bindings are mostly
of the form \"C-c C-s letter\".")

(defvar cscope-bind-keys-hook nil
  "At the end of the function \"cscope-find-keys\", this hook is run.
This provides an opportunity for custom keybinding schemes as well as any 
other buffer-specific set-up.  In cscope output buffers, this hook is run
before the extra bindings are applied; however, since those extra bindings
are all in \"cscope-keymap\", they can be modified directly by the user
to affect all cscope output buffers.")

(defvar cscope-quit-hook nil
  "Called after a cscope subprocess is told to exit.  If called as part of
a command that also kills the buffer, the hook is run before the buffer is
killed.")

(defvar cscope-b-and-f-hook nil
  "Run after the user bounces back to the cscope output buffer from a
source file.  This hook is run after the cursor has been positoned.  It gives
the user an opportunity to use some other cursor positioning strategy instead
of just advancing to the next line.")

(defvar cscope-interpret-output-hook nil
  "Run after the user moves from a cscope output line to the referenced
source file.  This hook is run after the cursor has been positoned.  It gives
the user an opportunity to use some other cursor positioning strategy.")

(defvar cscope-query-hook nil
  "Run after a query has been made of the cscope subprocess.  Normally, the
cscope interface tries to position the cursor at the first cscope
result line, but it is possible for it to miss.  This hook allows an
alternate cursor positioning strategy or any other after-the-query
processing.  For example, if you felt like it, you could \"pre-visit\"
all the files mentioned in output lines.  More usefully, you might
like to automatically visit a referenced source file
line if there is only one output line from cscope.")

(defvar cscope-file-not-found-hook nil
  "If defined, this behaves slightly differently than a standard emacs
hook function.  It is run instead of (not in addition to) the normal
action taken if some referenced source file can't be found.  Normally,
(if this hook is not defined) the cscope interface will signal an
error and give up looking.")

(defvar cscope-filename-fixxer-raw nil
  "If defined as a function, called to generate a filename.  The single
argument is a raw filename reference as taken from a cscope output
buffer.  The expected return value is something that the cscope
interface will try to resolve into a full pathname (using mechanisms
desribed elsewhere).")

(defvar cscope-filename-fixxer-cooked nil
  "If defined as a function, called to generate a filename.  The
single argument is a cooked filename reference, meaning that the
cscope interface has already tried to resolve it into a full pathname
(using mechanisms described elsewhere).  Since the resolution doesn't
always succeed, the argument might not be a full pathname.  The
expected return value is something that the cscope interface will try
to visit with \"find-file\".")

(defvar cscope-blurb nil
  "This is an interface from GNUemacs to the line-oriented mode of Joe
Steffen's cscope, a C code browser (cscope itself is available from
the AT&T Toolchest).  The interface includes provisions for having
multiple concurrent unrelated cscope sessions.  For casual use,
arrange for this file to be loaded and call the function
\"cscope-bind-keys\".  See the documentation for \"cscope-bind-keys\"
for more information about that.

If you are inclined to have multiple cscope sessions, possibly with different
invocation command lines or using pre-built databases, then see the
documentation for cscope-master-info-table and cscope-master-info-default.

The general method of using this is to arrange for the cscope-related
bindings to be made, place the cursor over some symbol or filename in
question, and invoke the appropriate cscope function.  This will invoke
a cscope subprocess (if it's not already running) and perform the query.
The results of the query are presented in a cscope output buffer.  The
user can place the cursor over a cscope reference line and type \"C-c C-c\"
to move to the referenced location.  After that, the user can type
\"C-c C-c\" again and move back to the cscope output buffer, automatically
advancing the cursor to the next line.  Functions that cause other buffers
to be displayed generally pop them up in another window.  If those functions
are called with prefix arguments, then the summoned buffer is put in the
currently selected window.

A good way to arrange for this file to be loaded is via c-mode-hook.  This
file \"provides\" cscope, so you can use \"require\" directives and/or
autoloading.  Here's a example:

	(autoload 'cscope-bind-keys \"cscope\" nil t)

	(defun wjc:c-mode-hook () \"my C mode hook\"
		;; only bother doing the bindings first time ... they'll stick
		(or (where-is-internal 'cscope-find-c-symbol (current-local-map))
			(cscope-bind-keys))
		;; (and (boundp 'cscope-blurb) (makunbound 'cscope-blurb))
		(local-set-key \"\\M-?\" 'cscope-find-c-symbol))

	(setq c-mode-hook 'wjc:c-mode-hook)

If you've seen the information in the description of this user option
variable enough times, you can let emacs reclaim the string space by
doing (makunbound 'cscope-blurb) after the cscope interface is loaded 
(shown as a commented line in the above example).  Even if you don't
have handy the source file, cscope.el, you'll still be able to read the
docstrings in cscope.elc.")

(defvar cscope-master-info-table nil
  "*A list-of-lists telling how to run cscope for a given buffer.
If you're not doing anything fancy with the cscope interface, like browsing
multiple databases concurrently, then you probably don't need to set this.
Each item in \"cscope-master-info-table\" is a list.  Trailing nil items from
the sublists may be omitted.

The first item in each sublist is a string which acts as the key for that
sublist.  If the value of the buffer-local variable \"cscope-id\" matches
the key, then that sublist is used to decide how to run cscope for that
buffer.  If no sublist in the entire table matches \"cscope-id\", then
\"cscope-master-info-default\" is used.

The second item in each sublist is itself a list.  It's a list of strings
which comprise the command line and arguments for invoking cscope.  See
the cscope man page for valid command line arguments.  Don't forget to make
sure that cscope and/or user-specified cscope commands are available via
the PATH environment variable.

The third item in each sublist is an optional \"cd place\".  If a non-nil
string, the current directory will be temporarily changed to the directory
named while cscope is being invoked (ie, while the cscope subprocess is
being spawned).  After cscope is invoked, the current directory is returned
to whatever it was before.

The fourth item in each sublist is a user-specified path prefix.  A
cscope database might only know relative pathnames.  Some versions of
cscope can be queried for the path prefix to use with relative
pathnames, but this does not work with all versions.  If cscope tells
emacs about a filename that is a relative pathname, the automatically
known path prefix is prepended.  If there is no automatically known
path prefix, the user-specified path prefix is used.  If there is no
user-specified path prefix, the \"cd place\" is used.  Otherwise, the
unprefixed relative pathname is used.

Here's an example of a personal setting for this variable:

	(setq cscope-master-info-table
	  '(
		(\"projA\" (\"cscope\" \"-l\" \"-d\" \"-f\" \"/projA/src/cscope.out\"))
		(\"projB\" (\"cscope\" \"-l\" \"-d\" \"-f\" \"/projB/src/cscope.out\"))
	  ))

The example uses separate pre-built cscope databases for projects
\"projA\" and \"projB\".  The last two items in the sublists are nil
(because they're not specified), which is a good clue that the
databases were built knowing full pathnames.  For buffers that are not
\"projA\" or \"projB\", the default cscope invocation will be used.")

(defvar cscope-master-info-default '("CSCOPE" ("cscope" "-l") nil nil)
  "*When a search of \"cscope-master-info-table\" is done and no match
is found, the list specified by this variable is returned instead.  See
the documentation for \"cscope-master-info-table\" for an explanation of
the items in the list.")

(defvar cscope-id nil
  "*Used as a key into \"cscope-master-info-table\".  This is a buffer-local
variable and could be set manually or by some mode-specific hook function.
If this variable is not explicitly set, it will generally result in the
use of \"cscope-master-info-default\".  The value of \"cscope-id\"
follows from buffer to buffer, but will not override any previously
set values.  That is, if a given buffer has a cscope-id of \"foo\", a
cscope output buffer for a cscope process started from that buffer
will also get a cscope-id of \"foo\".  Any source files newly visited
as a result of cscope queries from that cscope process will also get a
cscope-id of \"foo\".  Users can overcome that by explicitly setting
cscope-id via some hook or other means.  \"cscope-id\" is
buffer-local, so if you kill-all-local-variables, the value will be
lost.")

(make-variable-buffer-local 'cscope-id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; most users need not explore below here except to read function
; documentation strings; you can just as well describe-function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cscope-output-line-regexp
  "\\s-*\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\([0-9]+\\)"
"*This regular expression is used to recognize valid reference lines of 
output from the output of the line-oriented mode of cscope.  It must include
subexpressions which obtain the filename, function name, and line number.")

(defvar cscope-output-file-place 1
  "*Position number of the \"cscope-output-line-regexp\" subexpression
which locates the filename.")

(defvar cscope-output-func-place 2
  "*Position number of the \"cscope-output-line-regexp\" subexpression
which locates the function name.")

(defvar cscope-output-line-place 3
  "*Position number of the \"cscope-output-line-regexp\" subexpression
which locates the line number.")

(defvar cscope-c-symbol-regexp "[a-zA-Z0-9_]+"
  "*A regular expression specifying a legitimate C symbol.  Used for
finding a default symbol for minibuffer prompting.  User input need
not conform to this regular expression.")

(defvar cscope-filename-regexp "[a-zA-Z0-9_.-]+"
  "*A regular expression specifying a legitimate file name.  Used for
finding a default filename for minibuffer prompting.  User input need
not conform to this regular expression.")

; macros for pulling items out of sublists from cscope-master-info-table
(defmacro cscope:label-of-list      (cl) (list 'nth 0 cl))
(defmacro cscope:command-of-list    (cl) (list 'nth 1 cl))
(defmacro cscope:cdplace-of-list    (cl) (list 'nth 2 cl))
(defmacro cscope:pathprefix-of-list (cl) (list 'nth 3 cl))

(defun cscope-bind-keys ()
  "Establish the key bindings for cscope queries and interactions.
A reasonable thing to do is to call this function from \"c-mode-hook\".
However, if you use a minor mode which changes the keymap, you may
have to take extra steps.  For example, \"view-mode\" installs its own
keymap and is often called via \"find-file-hooks\".  For that
particular case, it may be helpful to have a line something like this
automatically invoked after \"view-mode\" has been installed:

	  (if (eq major-mode 'c-mode) (cscope-bind-keys))

Keys bound by invoking this function can be listed by going to a
buffer where the bindings are active and doing \\[describe-bindings].
They usually begin with a C-c C-s prefix.  If \"cscope-bindings-2deep\"
is non-nil, then you will see bindings of the form \"C-c letter\".  If
\"cscope-bindings-3deep\" is non-nil (the default), you will see
bindings of the form \"C-c C-s letter\".  These settings are
independent of one another, and by default the three character
bindings are provided and the two character bindings are not.  The
bindings are put in place by modifying whatever keymaps happen to be in
effect whenever you invoke this function.  For most users, this will mean
it's a more or less global change (e.g., to \"c-mode-map\").

In any case, the binding of \"C-c C-c\" is provided.  After the bindings
have been made, the optional user-supplied \"cscope-bind-keys-hook\" is run.

In cscope output buffers, there are additional single character
bindings (not controlled by any user option variable).  Further, the
two and/or three character bindings are provided, depending on the
values of the \"cscope-bindings-2deep\" and \"cscope-bindings-3deep\"
at the time the cscope interface is first loaded.  A different binding is
given to \"C-c C-c\" in cscope output buffers.

Within each set of bindings, more than one binding is made for some
functions for convenience.  Use \\[describe-bindings] to get a
complete list.  The intent of the alternate bindings of
cscope-interpret-output-line and cscope-goback-and-goforth in
different buffer types is so that an interested user can step through
references in a cscope output buffer by repeatedly typing the same
keys."
  (interactive)
  (if cscope-bindings-2deep (cscope-bind-keys-2deep))
  (if cscope-bindings-3deep (cscope-bind-keys-3deep))
  (local-set-key "\C-c\C-c" 'cscope-goback-and-goforth)
  (run-hooks 'cscope-bind-keys-hook)
)

(defun cscope-bind-keys-2deep ()
  (interactive)
  "Apply two character cscope bindings to the currently active keymap.
The bindings are of the form \"C-c letter\"  See variable
\"cscope-bindings-2deep\"."
  (local-set-key "\C-cc" 'cscope-find-c-symbol)
  (local-set-key "\C-cC" 'cscope-find-c-symbol)
  ;; (local-set-key "\C-c\C-c" 'cscope-find-c-symbol)

  (local-set-key "\C-cd" 'cscope-find-global-definition)
  (local-set-key "\C-cD" 'cscope-find-global-definition)
  (local-set-key "\C-c\C-d" 'cscope-find-global-definition)

  (local-set-key "\C-cv" 'cscope-find-functions-called)
  (local-set-key "\C-cV" 'cscope-find-functions-called)
  (local-set-key "\C-c\C-v" 'cscope-find-functions-called)

  (local-set-key "\C-c^" 'cscope-find-functions-calling)
  (local-set-key "\C-c6" 'cscope-find-functions-calling)

  (local-set-key "\C-ct" 'cscope-find-text-string)
  (local-set-key "\C-cT" 'cscope-find-text-string)
  (local-set-key "\C-c\C-t" 'cscope-find-text-string)

  (local-set-key "\C-cg" 'cscope-find-grep-pattern)
  (local-set-key "\C-cG" 'cscope-find-grep-pattern)
  (local-set-key "\C-c\C-g" 'cscope-find-grep-pattern)

  (local-set-key "\C-ce" 'cscope-find-egrep-pattern)
  (local-set-key "\C-cE" 'cscope-find-egrep-pattern)
  (local-set-key "\C-c\C-e" 'cscope-find-egrep-pattern)

  (local-set-key "\C-cf" 'cscope-find-file)
  (local-set-key "\C-cF" 'cscope-find-file)
  (local-set-key "\C-c\C-f" 'cscope-find-file)

  (local-set-key "\C-ci" 'cscope-find-files-including)
  (local-set-key "\C-cI" 'cscope-find-files-including)
  (local-set-key "\C-c\C-i" 'cscope-find-files-including)

  (local-set-key "\C-c#" 'cscope-find-files-including)
  (local-set-key "\C-c3" 'cscope-find-files-including)
  (local-set-key "\C-c*" 'cscope-find-all)
  (local-set-key "\C-c8" 'cscope-find-all)

  (local-set-key "\C-ca" 'cscope-admin-toggle-case)
  (local-set-key "\C-cA" 'cscope-admin-toggle-case)
  (local-set-key "\C-c\C-a" 'cscope-admin-toggle-case)

  (local-set-key "\C-cr" 'cscope-admin-rebuild-db)
  (local-set-key "\C-cR" 'cscope-admin-rebuild-db)
  (local-set-key "\C-c\C-r" 'cscope-admin-rebuild-db)

  (local-set-key "\C-cp" 'cscope-query-path-prefix)
  (local-set-key "\C-cP" 'cscope-query-path-prefix)
  (local-set-key "\C-c\C-p" 'cscope-query-path-prefix)

  (local-set-key "\C-cx" 'cscope-admin-quit)
  (local-set-key "\C-cX" 'cscope-admin-quit)
  (local-set-key "\C-c\C-x" 'cscope-admin-quit)

  (local-set-key "\C-cq" 'cscope-admin-quit-and-kill-buffer)
  (local-set-key "\C-cQ" 'cscope-admin-quit-and-kill-buffer)
  (local-set-key "\C-c\C-q" 'cscope-admin-quit-and-kill-buffer)
)

(defun cscope-bind-keys-3deep ()
  (interactive)
  "Apply three character cscope bindings to the currently active keymap.
The bindings are of the form \"C-c C-s letter\"  See variable
\"cscope-bindings-3deep\"."

  (local-unset-key ""); SJE MOD Mon Jun 24 1996
  ; to stop cc-mode getting at c-c c-s
  (local-set-key "\C-c\C-sc" 'cscope-find-c-symbol)
  (local-set-key "\C-c\C-sC" 'cscope-find-c-symbol)
  (local-set-key "\C-c\C-s\C-c" 'cscope-find-c-symbol)

  (local-set-key "\C-c\C-sd" 'cscope-find-global-definition)
  (local-set-key "\C-c\C-sD" 'cscope-find-global-definition)
  (local-set-key "\C-c\C-s\C-d" 'cscope-find-global-definition)

  (local-set-key "\C-c\C-sv" 'cscope-find-functions-called)
  (local-set-key "\C-c\C-sV" 'cscope-find-functions-called)
  (local-set-key "\C-c\C-s\C-v" 'cscope-find-functions-called)

  (local-set-key "\C-c\C-s^" 'cscope-find-functions-calling)
  (local-set-key "\C-c\C-s6" 'cscope-find-functions-calling)

  (local-set-key "\C-c\C-st" 'cscope-find-text-string)
  (local-set-key "\C-c\C-sT" 'cscope-find-text-string)
  (local-set-key "\C-c\C-s\C-t" 'cscope-find-text-string)

  (local-set-key "\C-c\C-sg" 'cscope-find-grep-pattern)
  (local-set-key "\C-c\C-sG" 'cscope-find-grep-pattern)
  (local-set-key "\C-c\C-s\C-g" 'cscope-find-grep-pattern)

  (local-set-key "\C-c\C-se" 'cscope-find-egrep-pattern)
  (local-set-key "\C-c\C-sE" 'cscope-find-egrep-pattern)
  (local-set-key "\C-c\C-s\C-e" 'cscope-find-egrep-pattern)

  (local-set-key "\C-c\C-sf" 'cscope-find-file)
  (local-set-key "\C-c\C-sF" 'cscope-find-file)
  (local-set-key "\C-c\C-s\C-f" 'cscope-find-file)

  (local-set-key "\C-c\C-si" 'cscope-find-files-including)
  (local-set-key "\C-c\C-sI" 'cscope-find-files-including)
  (local-set-key "\C-c\C-s\C-i" 'cscope-find-files-including)

  (local-set-key "\C-c\C-s#" 'cscope-find-files-including)
  (local-set-key "\C-c\C-s3" 'cscope-find-files-including)
  (local-set-key "\C-c\C-s*" 'cscope-find-all)
  (local-set-key "\C-c\C-s8" 'cscope-find-all)

  (local-set-key "\C-c\C-sa" 'cscope-admin-toggle-case)
  (local-set-key "\C-c\C-sA" 'cscope-admin-toggle-case)
  (local-set-key "\C-c\C-s\C-a" 'cscope-admin-toggle-case)

  (local-set-key "\C-c\C-sr" 'cscope-admin-rebuild-db)
  (local-set-key "\C-c\C-sR" 'cscope-admin-rebuild-db)
  (local-set-key "\C-c\C-s\C-r" 'cscope-admin-rebuild-db)

  (local-set-key "\C-c\C-sp" 'cscope-query-path-prefix)
  (local-set-key "\C-c\C-sP" 'cscope-query-path-prefix)
  (local-set-key "\C-c\C-s\C-p" 'cscope-query-path-prefix)

  (local-set-key "\C-c\C-sx" 'cscope-admin-quit)
  (local-set-key "\C-c\C-sX" 'cscope-admin-quit)
  (local-set-key "\C-c\C-s\C-x" 'cscope-admin-quit)

  (local-set-key "\C-c\C-sq" 'cscope-admin-quit-and-kill-buffer)
  (local-set-key "\C-c\C-sQ" 'cscope-admin-quit-and-kill-buffer)
  (local-set-key "\C-c\C-s\C-q" 'cscope-admin-quit-and-kill-buffer)
)

(if (not (boundp 'cscope-keymap))
	(progn
	  (setq cscope-keymap (copy-keymap text-mode-map))
	  (let ((real-keymap (current-local-map)))
		(use-local-map cscope-keymap)
		(cscope-bind-keys)
		(use-local-map real-keymap))

	  (define-key cscope-keymap "c" 'cscope-find-c-symbol)
	  (define-key cscope-keymap "C" 'cscope-find-c-symbol)
	  
	  (define-key cscope-keymap "d" 'cscope-find-global-definition)
	  (define-key cscope-keymap "D" 'cscope-find-global-definition)
	  
	  (define-key cscope-keymap "v" 'cscope-find-functions-called)
	  (define-key cscope-keymap "V" 'cscope-find-functions-called)
	  
	  (define-key cscope-keymap "^" 'cscope-find-functions-calling)
	  
	  (define-key cscope-keymap "t" 'cscope-find-text-string)
	  (define-key cscope-keymap "T" 'cscope-find-text-string)
	  
	  (define-key cscope-keymap "g" 'cscope-find-grep-pattern)
	  (define-key cscope-keymap "G" 'cscope-find-grep-pattern)
	  
	  (define-key cscope-keymap "e" 'cscope-find-egrep-pattern)
	  (define-key cscope-keymap "E" 'cscope-find-egrep-pattern)
	  
	  (define-key cscope-keymap "f" 'cscope-find-file)
	  (define-key cscope-keymap "F" 'cscope-find-file)
	  
	  (define-key cscope-keymap "i" 'cscope-find-files-including)
	  (define-key cscope-keymap "I" 'cscope-find-files-including)
	  
	  (define-key cscope-keymap "#" 'cscope-find-files-including)
	  (define-key cscope-keymap "*" 'cscope-find-all)
	  
	  (define-key cscope-keymap "a" 'cscope-admin-toggle-case)
	  (define-key cscope-keymap "A" 'cscope-admin-toggle-case)
	  
	  (define-key cscope-keymap "r" 'cscope-admin-rebuild-db)
	  (define-key cscope-keymap "R" 'cscope-admin-rebuild-db)
	  
	  (define-key cscope-keymap "p" 'cscope-query-path-prefix)
	  (define-key cscope-keymap "P" 'cscope-query-path-prefix)
	  
	  (define-key cscope-keymap "x" 'cscope-admin-quit)
	  (define-key cscope-keymap "X" 'cscope-admin-quit)
	  
	  (define-key cscope-keymap "q" 'cscope-admin-quit-and-kill-buffer)
	  (define-key cscope-keymap "Q" 'cscope-admin-quit-and-kill-buffer)

	  (define-key cscope-keymap "\C-c\C-c" 'cscope-interpret-output-line)
	  (define-key cscope-keymap "\C-m" 'cscope-interpret-output-line)
	  ))
  

(defun cscope-find-c-symbol (symbol)
  "Query cscope for the whereabouts of the given symbol.
When called interactively, the user is prompted for the symbol name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-c-symbol "0"))
  (cscope:query-omnibus "0" symbol))
;; 0 0

(defun cscope-find-global-definition (symbol)
  "Query cscope for the global definition of the given symbol.
When called interactively, the user is prompted for the symbol name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-c-symbol "1"))
  (cscope:query-omnibus "1" symbol))
;; 1 x

(defun cscope-find-functions-called (symbol)
  "Query cscope for the names of functions called by a function.
When called interactively, the user is prompted for the function name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-c-symbol "1"))
  (cscope:query-omnibus "1" symbol))
;; 2 1

(defun cscope-find-functions-calling (symbol)
  "Query cscope for the names of functions calling a function.
When called interactively, the user is prompted for the function name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-c-symbol "2"))
  (cscope:query-omnibus "2" symbol))
;; 3 2

(defun cscope-find-text-string (symbol)
  "Query cscope for the whereabouts of the given text string.
When called interactively, the user is prompted for the string.  A
prefix argument causes the current window to be used for the output."
  (interactive (cscope:gather-text-string "3"))
  (cscope:query-omnibus "3" symbol))
;; 4 3


(defun cscope-find-grep-pattern (symbol)
  "Query cscope for the whereabouts of the given grep pattern.  When
called interactively, the user is prompted for the pattern.  (The
line-oriented mode of cscope does not support changing occurrences of
the given pattern; the full-screen version of cscope does.)  A prefix
argument causes the current window to be used for the output."
  (interactive (cscope:gather-text-string "5"))
  (cscope:query-omnibus "5" symbol))

;; 5 x

(defun cscope-find-egrep-pattern (symbol)
  "Query cscope for the whereabouts of the given egrep pattern.  When
called interactively, the user is prompted for the pattern.  A prefix
argument causes the current window to be used for the output."
  (interactive (cscope:gather-text-string "6"))
  (cscope:query-omnibus "6" symbol))
;; 6 x

(defun cscope-find-file (symbol)
  "Query cscope for the whereabouts of the given file.
When called interactively, the user is prompted for the file name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-file-name "5"))
  (cscope:query-omnibus "5" symbol))
;; 7 5

(defun cscope-find-files-including (symbol)
  "Query cscope for the files including a given file.
When called interactively, the user is prompted for the file name,
with a symbol near point being the default.  A prefix argument causes
the current window to be used for the output."
  (interactive (cscope:gather-file-name "4"))
  (cscope:query-omnibus "4" symbol))
;; 8 4 

(defun cscope-find-all (&rest symbol)
  "Query cscope for the definitions of all functions and C++ classes.
Any arguments to this function are ignored.  A prefix argument causes
the current window to be used for the output."
  (interactive)
  (cscope:query-omnibus "9" "fake"))
;; 9 x

(defun cscope-admin-toggle-case (&rest symbol)
  "Toggle case-folding for subsequent cscope searches.  Since
case-folding can also be affected by a command line option when cscope
is invoked, it's up to the user to keep track of the state of the
toggle.  Any arguments to this function are ignored.  A prefix
argument causes the current window to be used for the output."
  (interactive)
  (cscope:query-omnibus "c"))
;; c x

(defun cscope-admin-rebuild-db (&rest symbol)
  "Tell the cscope subprocess to rebuild its database.
Any arguments to this function are ignored.  A prefix argument causes
the current window to be used for the output."
  (interactive)
  (cscope:query-omnibus "r"))
;; r r

(defun cscope-query-path-prefix (&rest symbol)
  "Query cscope for the relative filename path prefix.  Not all
versions of cscope support this feature; if yours doesn't, it will be
obvious from what you see in the cscope output buffer after running
this command.  Any arguments to this function are ignored.  A prefix
argument causes the current window to be used for the output."
  (interactive)
  (cscope:query-omnibus "P"))
;; P x

(defun cscope-admin-quit (&rest symbol)
  "Tell the cscope subprocess to terminate.
Any arguments to this function are ignored.  Afterwards, run the 
optional user-supplied \"cscope-quit-hook\".  A prefix argument causes
the current window to be used for the output."
  (interactive)
  (prog1 (cscope:query-omnibus "q")
	(run-hooks 'cscope-quit-hook)
	))
;; q q

(defun cscope-admin-quit-and-kill-buffer (&rest symbol)
  "Tell the cscope subprocess to terminate.
You can achieve the same effect by simply killing the affiliated cscope
output buffer or by exiting emacs.  Any arguments to this function are
ignored."
  (interactive)
  (cscope-admin-quit)
  (let* (
		 (cscope-list (cscope:pick-description-list cscope-id))
		 (cscope-label (cscope:label-of-list cscope-list))
		 (cscope-buffer-name (cscope:bname-from-label cscope-label))
		 )
	(if (get-buffer cscope-buffer-name)
		(kill-buffer cscope-buffer-name)))
)

(defun cscope-goback-and-goforth ()
  "Return to the affiliated cscope buffer and advance the cursor by one line.
The affect of this will usually be to help in stepping through references
in the cscope output buffer.  See also \"cscope-interpret-output-line\".
After the cursor has been positioned, the optional user-supplied
\"cscope-b-and-f-hook\" is run.  A prefix argument causes
the current window to be used for the cscope output buffer."
  (interactive)
  (if (and cscope:affiliated-buffer (get-buffer cscope:affiliated-buffer))
	  (if (and current-prefix-arg
			   (not (get-buffer-window cscope:affiliated-buffer)))
		  (switch-to-buffer cscope:affiliated-buffer)
		(pop-to-buffer cscope:affiliated-buffer))
	(error "CSCOPE: No affiliated cscope output buffer"))
  (forward-line)
  (run-hooks 'cscope-b-and-f-hook))

(defun cscope-interpret-output-line ()
  "Parse the line under the cursor as a cscope output reference line.
Try to visit the named file and place the cursor on the mentioned line number.
After the cursor has been positioned, run the optional user-supplied
\"cscope-interpret-output-hook\".  A prefix argument causes
the current window to be used for the visited file."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at cscope-output-line-regexp))
	  (error "CSCOPE: Line not understood as a cscope reference line")
	(let (
		  (cscope:filename (buffer-substring
					 (match-beginning cscope-output-file-place)
					 (match-end cscope-output-file-place)))
		  (linenumb (buffer-substring
					 (match-beginning cscope-output-line-place)
					 (match-end cscope-output-line-place)))
		  (function (buffer-substring
					 (match-beginning cscope-output-func-place)
					 (match-end cscope-output-func-place)))
		  (find-file-not-found-hooks (list 'cscope:file-not-found))
		  (affiliated-buffer (buffer-name))
		  (cscope-id-affiliated cscope-id)
		  )
	  (and (fboundp 'cscope-filename-fixxer-raw)
		   (setq cscope:filename (funcall cscope-filename-fixxer-raw cscope:filename)))
	  (if (not (file-name-absolute-p cscope:filename))
		  (setq cscope:filename (cscope:fulfill-relative-path cscope:filename)))
	  (and (fboundp 'cscope-filename-fixxer-cooked)
		   (setq cscope:filename (funcall cscope-filename-fixxer-cooked cscope:filename)))
	  (if current-prefix-arg
		  (find-file cscope:filename)
		(find-file-other-window cscope:filename))
	  (setq cscope:affiliated-buffer affiliated-buffer)
	  (if (not cscope-id) (setq cscope-id cscope-id-affiliated))
	  (goto-line (string-to-int linenumb))
	  (message "CSCOPE: Function: %s" function)
	  (run-hooks 'cscope-interpret-output-hook)
	  ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; only implementation stuff below here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A buffer-local internal variable that is used to navigate back to the
; most recent previous cscope buffer.  Used by cscope-goback-and-goforth.
(defvar cscope:affiliated-buffer nil)
(make-variable-buffer-local 'cscope:affiliated-buffer)

; Gets a piece of menu text and strips off the last two characters, "%s"
(defun cscope:get-menu-subtext (menu-item-key)
  (substring (cdr (assoc menu-item-key cscope:menu-text-list)) 0 -2))

; Used to pick up buffer contents near point that match regexp.
; Works by backing up until no longer looking at something matching
; the regular expression, then finding the exact bounds by searching
; forward.
(defun cscope:looking-at (regexp)
  (let ((fore-marker nil))
	(save-excursion
	  (if (or (looking-at regexp) (re-search-backward regexp nil t))
		  (progn
			(while (and (looking-at regexp) (not (bobp))) (backward-char 1))
			(if (re-search-forward regexp nil t)
				(buffer-substring (match-beginning 0) (point-marker))
			  (identity "")))
		(identity "")))))

(defun cscope:gather-c-symbol (menu-item-key)
  (list (read-string (cscope:get-menu-subtext menu-item-key)
					 (cscope:looking-at cscope-c-symbol-regexp))))

(defun cscope:gather-file-name (menu-item-key)
  (list (read-string (cscope:get-menu-subtext menu-item-key)
					 (cscope:looking-at cscope-filename-regexp))))

; no default used for text string, grep/egrep patterns
(defun cscope:gather-text-string (menu-item-key)
  (list (read-string (cscope:get-menu-subtext menu-item-key))))
  
; Low-level function for passing queries to the "cscope" subprocess.
; "menu-item-key" and "item-argument" are what "cscope" is
; expecting (see the "cscope" man page).
(defun cscope:query-omnibus (menu-item-key &optional item-argument)
  (let* (symbol do-this cscope-process)
	(setq symbol (if item-argument item-argument ""))
	(setq do-this  ;; string to identify (in the cscope buffer) what's asked
		  (format
		   (concat (cdr (assoc menu-item-key cscope:menu-text-list)) "\n")
		   symbol))
	(setq cscope-process
		  (cscope:pname-from-label
		   (cscope:label-of-list
			(cscope:pick-description-list cscope-id))))
	(if (or (eq (process-status cscope-process) 'run)
			(not (string-equal menu-item-key "q")))
		(progn
		  (setq cscope-process (cscope:guarantee-process do-this))
		  (process-send-string
		   cscope-process (concat menu-item-key symbol "\n"))
		  (accept-process-output cscope-process)
		  (goto-char (point-min))
		  (forward-line 1)
		  (accept-process-output)
		  (if (looking-at "cscope: ") (forward-line 1))
		  (run-hooks 'cscope-query-hook)
		  ))))

; Obviously, these are intended to match the text of menu items in
; in the fullscreen version of cscope.  I've also added the "admin"
; items available under line-oriented mode.  "P" is used automatically
; (and invisibly) by the interface.

(defconst cscope:menu-text-list
	'(("0" . "Find this C symbol: %s")
	  ("1" . "Find functions called by this function: %s")
	  ("2" . "Find functions calling this function: %s")
	  ("3" . "Find this text string: %s")
	  ("6" . "Find POSIX naming violation: %s")
	  ("5" . "Find this file: %s")
	  ("4" . "Find files #including this file: %s")
	  ;;("9" . "Find all functions and C++ classes")
	  ;;("c" . "Toggle ignore/use letter case when searching")
	  ("r" . "Rebuild the symbol database")
	  ;;("P" . "Print relative-path prefix")
	  ("q" . "Quit cscope"))
)

; cscope version:
;(defconst cscope:menu-text-list
;	'(("0" . "Find this C symbol: %s")
;	  ("1" . "Find this global definition: %s")
;	  ("2" . "Find functions called by this function: %s")
;	  ("3" . "Find functions calling this function: %s")
;	  ("4" . "Find this text string: %s")
;	  ("5" . "Find this grep pattern: %s")
;	  ("6" . "Find this egrep pattern: %s")
;	  ("7" . "Find this file: %s")
;	  ("8" . "Find files #including this file: %s")
;	  ("9" . "Find all functions and C++ classes")
;	  ("c" . "Toggle ignore/use letter case when searching")
;	  ("r" . "Rebuild the symbol database")
;	  ("P" . "Print relative-path prefix")
;	  ("q" . "Quit cscope"))
;)

; mapping back and forth between label and buffer/process names
(defconst cscope:bname-prefix "cscope: ")
(defconst cscope:bname-prefix-length (length cscope:bname-prefix))
(defconst cscope:bname-suffix " Output")
(defconst cscope:bname-suffix-length (length cscope:bname-suffix))
(defun cscope:bname-from-label (label)
  (concat cscope:bname-prefix label cscope:bname-suffix))
(defun cscope:label-from-bname (name)
  (substring
   (substring name 0 (- cscope:bname-suffix-length))
   cscope:bname-prefix-length))

(defconst cscope:pname-prefix "cscope: ")
(defconst cscope:pname-prefix-length (length cscope:pname-prefix))
(defconst cscope:pname-suffix " Process")
(defconst cscope:pname-suffix-length (length cscope:pname-suffix))
(defun cscope:pname-from-label (label)
  (concat cscope:pname-prefix label cscope:pname-suffix))
(defun cscope:label-from-pname (name)
  (substring
   (substring name 0 (- cscope:pname-suffix-length))
   cscope:pname-prefix-length))

; if the appropriate cscope subprocess is not already running, start it
; up, initialize buffers, etc;  if it is running, just do the buffer biz
(defun cscope:guarantee-process (do-this)
  (let* (
		 (cscope-list (cscope:pick-description-list cscope-id))
		 (cscope-label (cscope:label-of-list cscope-list))
		 (cscope-buffer-name (cscope:bname-from-label cscope-label))
		 (cscope-process-name (cscope:pname-from-label cscope-label))
		 (cscope-process nil)
		 (cd-place (if (cscope:cdplace-of-list cscope-list) (cscope:cdplace-of-list cscope-list) default-directory))
		 )
	(if (eq (process-status cscope-process-name) 'run)
		(save-excursion (set-buffer cscope-buffer-name) (erase-buffer))
	  (message "CSCOPE: Starting process for %s ..." cscope-label)
	  (and (get-buffer cscope-buffer-name) (kill-buffer cscope-buffer-name))
	  (setq cscope-process
			(let (
			      ;; sje was  a pipe (nil) now a pty (t)
			      ;; Sun 16 Nov 97
			      (process-connection-type t) ; use a pipe
			      (default-directory cd-place))
			  (apply 'start-process cscope-process-name cscope-buffer-name (cscope:command-of-list cscope-list))))
	  (process-kill-without-query cscope-process)
	  (cscope:figure-out-path-prefix cscope-label cscope-buffer-name cscope-process)
	  (message "CSCOPE: Starting process for %s ... started" cscope-label)
	  (run-hooks 'cscope-start-process-hook)
	  )
	(if (and current-prefix-arg
			 (not (get-buffer-window cscope-buffer-name)))
		(switch-to-buffer cscope-buffer-name)
	  (pop-to-buffer cscope-buffer-name))
	(setq cscope-process (get-process cscope-process-name))
	(if (not cscope-id) (setq cscope-id cscope-label))
	(use-local-map cscope-keymap)
	(insert do-this)
	(set-marker (process-mark cscope-process) (point-max))
	(identity cscope-process)
))

; keep a list of path prefixes that can be discovered by asking cscope
(defvar cscope:path-prefix-table nil)

(defun cscope:figure-out-path-prefix (cscope-label cscope-buffer-name cscope-process)
  (set-buffer cscope-buffer-name)
  (goto-char (point-min))
  (while (and
		  (eq (process-status cscope-process) 'run)
		  (not (re-search-forward "^>>" nil t)))
	(goto-char (point-min))
	(accept-process-output))
  (if (not (eq (process-status cscope-process) 'run))
	  (message "CSCOPE: Dead process for %s" cscope-label)
	(erase-buffer)
	(process-send-string cscope-process "P\n")
	(while (and
			(eq (process-status cscope-process) 'run)
			(not (re-search-forward "^>>" nil t)))
	  (goto-char (point-min))
	  (accept-process-output))
	(goto-char (point-min))
	(if (not (looking-at ".*cscope.* unknown ")) ;; sometimes no P support
		(progn
		  (end-of-line)
		  (let* (
				 (path-prefix (buffer-substring (point-min) (point)))
				 (list-item
				  (if cscope:path-prefix-table
					  (assoc cscope-label cscope:path-prefix-table) nil))
				 )
			(if list-item
				(setcdr list-item path-prefix)
			  (setq cscope:path-prefix-table
					(cons '(cscope-label . path-prefix)
						  cscope:path-prefix-table)))
				 )))
	)
  (erase-buffer)
  )

(defun cscope:pick-description-list (cscope-id)
  (if (and cscope-master-info-table (assoc cscope-id cscope-master-info-table))
	  (assoc cscope-id cscope-master-info-table)
	(identity cscope-master-info-default)))

; catch the file-not-found situation so that a new empty file isn't just
; opened up as a routine matter; instead, give a message
(defun cscope:file-not-found ()
  (if cscope-file-not-found-hook
	  (run-hooks 'cscope-file-not-found-hook)
	(kill-buffer (get-file-buffer cscope:filename))
	(error "CSCOPE: Can't find file %s" cscope:filename)))

; convert cscope's relative pathnames into absolute pathnames;
; try computed path prefix, user-specified path prefix, and user-specified
; "cd place"
(defun cscope:fulfill-relative-path (filename)
  (let* (
		(list-item
		 (if cscope:path-prefix-table
			 (assoc cscope-id cscope:path-prefix-table) nil))
		(prefix (if list-item (cdr list-item) nil))
		(cscope-list (cscope:pick-description-list cscope-id))
		)
	(if (not prefix) (setq prefix (cscope:pathprefix-of-list cscope-list)))
	(if (not prefix) (setq prefix (cscope:cdplace-of-list cscope-list)))
	(if prefix
		(format "%s/%s" prefix filename)
	  (identity filename))
	)
)

(provide 'cscope)
;;; cscope.el ends here

