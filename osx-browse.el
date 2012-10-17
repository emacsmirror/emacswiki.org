;;; osx-browse.el --- Web browsing helpers for OS X
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/osx-browse
;; URL: http://raw.github.com/rolandwalker/osx-browse/master/osx-browse.el
;; Version: 0.8.2
;; Last-Updated: 16 Oct 2012
;; EmacsWiki: OSXBrowse
;; Keywords: hypermedia, external
;; Package-Requires: ((browse-url-dwim "0.6.2"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'osx-browse)
;;
;;     (osx-browse-mode 1)
;;
;;     ⌘-b      ; browse to URL in foreground
;;     C-- ⌘-b  ; browse to URL in background
;;
;;     ⌘-i      ; search Google in foreground
;;     C-- ⌘-i  ; search Google in background
;;
;;     ;; position cursor on a URL
;;     ⌘-b
;;
;;     ;; select a region
;;     ⌘-i
;;
;;     ;; to turn off confirmations
;;     (setq browse-url-dwim-always-confirm-extraction nil)
;;
;; Explanation
;;
;; This package helps Emacs run Safari, Google Chrome, and Firefox
;; on OS X.  It uses `browse-url', but is somewhat more friendly and
;; configurable than the OS X functions provided there.
;;
;; Default values for URLs or search text are deduced from the region
;; or from context around the point, according to the heuristics in
;; browse-url-dwim.el.
;;
;; To use osx-browse, place the osx-browse.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'osx-browse)
;;     (osx-browse-mode 1)
;;
;; The following interactive commands are provided:
;;
;;     `osx-browse-mode'
;;     `osx-browse-url'
;;     `osx-browse-search'
;;     `osx-browse-guess'
;;     `osx-browse-url-safari'
;;     `osx-browse-url-chrome'
;;     `osx-browse-url-firefox'
;;
;; When `osx-browse-install-aliases' is set (the default) and
;; `osx-browse-mode' is turned on, aliases are added for the commands
;;
;;     `browse'
;;     `google'
;;     `browse-url-chromium'
;;
;; See Also
;;
;;     M-x customize-group RET osx-browse RET
;;     M-x customize-group RET browse-url-dwim RET
;;     M-x customize-group RET browse-url RET
;;
;; Notes
;;
;;     This library uses browse-url-dwim.el, but does not require that
;;     `browse-url-dwim-mode' be turned on.  If both modes are turned
;;     on, keybindings from both modes will be active.
;;
;;     When `osx-browse-mode' is turned on, `browse-url-browser-function'
;;     is set to `osx-browse-url', meaning that your default browsing
;;     facilities will be provided by this library.  `osx-browse-url-safari'
;;     and friends are provided in the event that you wish to set
;;     `browse-url-browser-function' by hand.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, at the time of writing
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3 and lower : no
;;
;;     Requires: browse-url-dwim.el
;;
;;     Uses if present: string-utils.el
;;
;; Bugs
;;
;;     Keybindings don't work out of the box with Aquamacs, which
;;     does not think that ⌘ is the Super modifier.
;;
;;     OS X makes an iconified application visible, even when opening
;;     a URL in the background.
;;
;;     New-window parameter is not respected, just as in browse-url.el
;;     Could use AppleScript as follows
;;
;;         osascript -e 'tell application id "com.apple.Safari" to make new document with properties {URL:"%s"}'
;;
;;         osascript -e 'tell application id "com.google.Chrome"' -e 'set win to make new window' -e 'set URL of active tab of win to "%s"' -e 'end tell'
;;
;;         there should be a similar AppleScript solution for Firefox
;;
;; TODO
;;
;;     Respect new-window parameter.
;;
;;     If new-window worked, could make two universal prefix args
;;     mean: foreground + new window.
;;
;;     Examine the process object returned by start-process.
;;
;;; License
;;
;; Simplified BSD License
;;
;; Copyright (c) 2012, Roland Walker
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for callf, callf2
(require 'cl)

(require 'string-utils nil t)

(autoload 'browse-url                         "browse-url"       "Ask a WWW browser to load a URL." t)

(autoload 'browse-url-dwim-find-search-text   "browse-url-dwim"  "Find some text on which to conduct a search.")
(autoload 'browse-url-dwim-coerce-to-web-url  "browse-url-dwim"  "Coerce URL to a string representing a valid web address.")
(autoload 'browse-url-dwim-get-url            "browse-url-dwim"  "Find a Web URL by context or user input.")

;;; declarations

(declare-function string-utils-has-darkspace-p "string-utils.el")
(declare-function string-utils-squeeze-url     "string-utils.el")

(eval-when-compile
  (defvar browse-url-dwim-search-url))

;;; customizable variables

;;;###autoload
(defgroup osx-browse nil
  "Web browsing helpers for OS X."
  :version "0.8.2"
  :link '(emacs-commentary-link "osx-browse")
  :prefix "osx-browse-"
  :group 'external
  :group 'hypermedia)

(defcustom osx-browse-prefer-browser nil
  "Preferred external browser.

Nil means use the system default.

If a custom value is given, it should be a string in the form of
an application name or OS X bundle identifier as documented in
\"man open\"."
  :type '(choice
          (const  :tag "System Default" nil)
          (const  :tag "Safari"         "com.apple.Safari")
          (const  :tag "Chrome"         "com.google.Chrome")
          (const  :tag "Firefox"        "org.mozilla.Firefox")
          (string :tag "Custom"))
  :group 'osx-browse)

(defcustom osx-browse-prefer-background nil
  "Open new browser windows in the background by default."
  :type 'boolean
  :group 'osx-browse)

(defcustom osx-browse-prefer-new-window nil
  "Create new browser windows by default.

BUG: this value is currently not respected."
  :type 'boolean
  :group 'osx-browse)

(defcustom osx-browse-install-aliases t
  "Whether to install command aliases for `osx-browse'.

If this option is set, aliases are installed for the commands
`browse-url-chromium', `browse', and `google'."
  :type 'boolean
  :group 'osx-browse)

(defcustom osx-browse-less-feedback nil
  "Give less echo area feedback."
  :type 'boolean
  :group 'osx-browse)

;;;###autoload
(defgroup osx-browse-keys nil
  "Key bindings for `osx-browse-mode'."
  :group 'osx-browse)

(defcustom osx-browse-url-keystrokes '("s-b")
  "Key sequences to activate an external web browser.

These key sequences will invoke `osx-browse' when
`osx-browse-mode' is active.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'osx-browse-keys)

(defcustom osx-browse-guess-keystrokes '("s-i")
  "Key sequences to activate an Internet search in an external browser.

These key sequences will invoke `osx-browse-guess' when
`osx-browse-mode' is active.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'osx-browse-keys)

(defcustom osx-browse-search-keystrokes nil
  "Key sequences to activate an Internet search in an external browser.

These key sequences will invoke `osx-browse-search' when
`osx-browse-mode' is active.

The format for key sequences is as defined by `kbd'."
  :type '(repeat string)
  :group 'osx-browse-keys)

;;; variables

(defvar osx-browse-open-command "/usr/bin/open"
  "Path to the OS X \"open\" command.")

(defvar osx-browse-osascript-command "/usr/bin/osascript"
  "Path to the OS X \"osascript\" command.")

(defvar osx-browse-saved-browse-url-browser-function nil
  "Saved value of `browse-url-browser-function'.")

(defvar osx-browse-mode nil
  "Mode variable for `osx-browse-mode'.")

;;; keymaps

(defvar osx-browse-mode-map (make-sparse-keymap)
  "Keymap for `osx-browse-mode' minor-mode.")

(dolist (cmd '(osx-browse-url osx-browse-guess osx-browse-search))
  (dolist (k (symbol-value (intern (concat (symbol-name cmd) "-keystrokes"))))
    (define-key osx-browse-mode-map (read-kbd-macro k) cmd)))

;;; macros

(defmacro osx-browse-called-interactively-p (&optional kind)
  "A backward-compatible version of `called-interactively-p'.

Optional KIND is as documented at `called-interactively-p'
in GNU Emacs 24.1 or higher."
  (if (eq 0 (cdr (subr-arity (symbol-function 'called-interactively-p))))
      '(called-interactively-p)
    `(called-interactively-p ,kind)))

;;; compatibility functions

(unless (fboundp 'string-utils-has-darkspace-p)
  ;; simplified version of function from string-utils.el
  (defun string-utils-has-darkspace-p (obj)
  "Test whether OBJ, when coerced to a string, has any non-whitespace characters.

Returns the position of the first non-whitespace character
on success."
  (let ((str-val (if (stringp obj) obj (format "%s" obj))))
    (string-match-p "[^ \t\n\r\f]" str-val))))

;;; utility functions

(defun osx-browse-truncate-url (url maxlen)
  "Truncate URL to MAXLEN for use in feedback messages."
  (cond
    ((fboundp 'string-utils-squeeze-url)
     (string-utils-squeeze-url url maxlen))
    ((<= (length url) maxlen)
     url)
    (t
     (concat (substring url 0 maxlen) "..."))))

(defun osx-browse-bundle-name-p (bundle-or-app)
  "Whether string BUNDLE-OR-APP looks like a bundle id to the OS X \"open\" command."
  (and (string-match-p "\\..*\\." bundle-or-app)
       (not (string-match-p "/" bundle-or-app))))

(defun osx-browse-interactive-form (&optional target new-window browser)
  "Return a list to be used as an `interactive' form.

TARGET is a URL or text to search.

NEW-WINDOW and BROWSER set default values."
  (list
   (or target (browse-url-dwim-get-url))
   (or new-window osx-browse-prefer-new-window)
   (or browser osx-browse-prefer-browser)
   (cond
     ((and current-prefix-arg
           (< (prefix-numeric-value current-prefix-arg) 0))
      'background)
     (current-prefix-arg
      'foreground)
     (osx-browse-prefer-background
      'background)
     (t
      'foreground))))

;;; minor mode definition

;;;###autoload
(define-minor-mode osx-browse-mode
  "Turn on `osx-browse-mode'.

Turning on osx-browse-mode will direct `browse-url' to use this
package when opening external browsers, and activate keybindings
as defined in `customize'.  It may also set up certain aliases
when `osx-browse-install-aliases' is set.

When called interactively with no prefix argument, this command
toggles the mode.  With a prefix argument, it enables the mode
if the argument is positive and otherwise disables the mode.

When called from Lisp, this command enables the mode if the
argument is omitted or nil, and toggles the mode if the argument
is 'toggle."
  :keymap osx-browse-mode-map
  :group 'osx-browse
  :global t
  (cond
   (osx-browse-mode
    (setq osx-browse-saved-browse-url-browser-function browse-url-browser-function)
    (setq browse-url-browser-function 'osx-browse-url)
    (when osx-browse-install-aliases
      (defalias 'browse-url-chromium 'osx-browse-url-chrome)
      (defalias 'browse 'osx-browse-url)
      (defalias 'google 'osx-browse-guess))
    (when (and (osx-browse-called-interactively-p 'interactive)
               (not osx-browse-less-feedback))
      (message "osx-browse mode enabled")))
   (t
    (setq browse-url-browser-function osx-browse-saved-browse-url-browser-function)
    (when osx-browse-install-aliases
      (when (eq (symbol-function 'browse-url-chromium) 'osx-browse-url-chrome)
        (fmakunbound 'browse-url-chromium))
      (when (eq (symbol-function 'browse) 'osx-browse-url)
        (fmakunbound 'browse))
      (when (eq (symbol-function 'google) 'osx-browse-guess)
        (fmakunbound 'google)))
    (when (and (osx-browse-called-interactively-p 'interactive)
               (not osx-browse-less-feedback))
      (message "osx-browse mode disabled")))))

;;; interactive commands

;; todo - text for the docstring whenever NEW-WINDOW is respected:
;;
;; If optional NEW-WINDOW is non-nil, a new browser window will be
;; created.  When NEW-WINDOW is not set, the customizable variable
;; `osx-browse-prefer-new-window' is consulted.
;;

;;;###autoload
(defun osx-browse-url (url &optional new-window browser focus)
  "Open URL in an external browser on OS X.

When called interactively, `browse-url-dwim-get-url' is used
to detect URL from the edit context and prompt for user input
as needed.

Optional NEW-WINDOW is not currently respected.

Optional BROWSER requests a specific browser, using an Apple
bundle ID, eg \"com.apple.Safari\" or application name, eg
\"Webkit.app\".  When BROWSER is not set, the customizable
variable `osx-browse-prefer-browser' is consulted, and if that
value is nil, the system default is used.

Optional FOCUS can be set to 'foreground or 'background to
control whether the external process changes the focus.  If
BACKGROUND is not set, the customizable variable
`osx-browse-prefer-background' is consulted.

When called interactively, specifying a negative prefix argument
is equivalent to setting FOCUS to 'background.  Any other prefix
argument is equivalent to setting FOCUS to 'foreground."
  (interactive (osx-browse-interactive-form))
  (unless (stringp url)
    (error "No valid URL"))
  (let ((args (list url))
        (proc nil))
    (when browser
      (callf2 append (list (if (osx-browse-bundle-name-p browser) "-b" "-a") browser) args))
    (when (eq focus 'background)
      (push "-g" args))
    (setq proc (apply 'start-process "osx-browse-url" nil osx-browse-open-command args))
    (set-process-query-on-exit-flag proc nil))
  (let ((width (- (frame-width) 25)))
    (when (and (eq focus 'background)
               (not osx-browse-less-feedback)
               (>= width 10))
      (message "opened in background: %s" (osx-browse-truncate-url url width)))))

;;;###autoload
(defun osx-browse-search (text &optional new-window browser focus search-url)
  "Perform an Internet search for TEXT, or region, or interactive input.

If TEXT is a URL, browse to page directly.  Otherwise invoke an
Internet search using TEXT.  When called interactively, TEXT may
be taken from the region or entered at a prompt.

NEW-WINDOW, BROWSER, and FOCUS are as documented for
`osx-browse-url'.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is consulted."
  (interactive (osx-browse-interactive-form :passthrough))
  (require 'browse-url-dwim)
  (callf or search-url browse-url-dwim-search-url)
  (when (or (null text)
            (eq text :passthrough))
    ;; here rather than in interactive form so that search-url value is visible
    (setq text (browse-url-dwim-find-search-text search-url)))
  (cond
    ((not (string-utils-has-darkspace-p text))
     (error "No valid query or URL"))
    ((browse-url-dwim-coerce-to-web-url text)
     (osx-browse-url (browse-url-dwim-coerce-to-web-url text) new-window browser focus))
    (t
     (osx-browse-url (concat search-url (url-hexify-string text)) new-window browser focus))))

;;;###autoload
(defun osx-browse-guess (text &optional new-window browser focus search-url)
  "Perform an Internet search or browses to URL according to context.

Identical to `osx-browse-url-search' except that an attempt will
be made to extract a URL from TEXT or edit context before
prompting the user.

When a region is not active and the point is on a probable URL,
that value will be used and the user will not be prompted.

NEW-WINDOW, BROWSER, and FOCUS are as documented for
`osx-browse-url'.

Optional SEARCH-URL specifies the URL fragment used to construct
the search request.  If not specified, the customizable variable
`browse-url-dwim-search-url' is consulted."
  (interactive (osx-browse-interactive-form :passthrough))
  (require 'browse-url-dwim)
  (callf or search-url browse-url-dwim-search-url)
  (when (or (null text)
            (eq text :passthrough))
    ;; here rather than in interactive form so that search-url value is visible
    (setq text (browse-url-dwim-find-search-text search-url 'guess)))
  (cond
    ((not (string-utils-has-darkspace-p text))
     (error "No valid query or URL"))
    ((browse-url-dwim-coerce-to-web-url text)
     (osx-browse-url (browse-url-dwim-coerce-to-web-url text) new-window browser focus))
    (t
     (osx-browse-url (concat search-url (url-hexify-string text)) new-window browser focus))))

;;;###autoload
(defun osx-browse-url-safari (url &optional new-window browser focus)
  "Open URL in Safari on OS X.

BROWSER defaults to \"com.apple.Safari\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'."
  (interactive (osx-browse-interactive-form nil nil "com.apple.Safari"))
  (osx-browse-url url new-window browser focus))

;;;###autoload
(defun osx-browse-url-chrome (url &optional new-window browser focus)
  "Open URL in Google Chrome on OS X.

BROWSER defaults to \"com.google.Chrome\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'."
  (interactive (osx-browse-interactive-form nil nil "com.google.Chrome"))
  (osx-browse-url url new-window browser focus))

;;;###autoload
(defun osx-browse-url-firefox (url &optional new-window browser focus)
  "Open URL in Firefox on OS X.

BROWSER defaults to \"org.mozilla.Firefox\".

URL, NEW-WINDOW, and FOCUS are as documented for
`osx-browse-url'."
  (interactive (osx-browse-interactive-form nil nil "org.mozilla.Firefox"))
  (osx-browse-url url new-window browser focus))

(provide 'osx-browse)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: OSXBrowse dwim utils osascript iconified google args
;; LocalWords: callf MAXLEN Webkit

;;; osx-browse.el ends here
