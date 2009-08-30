;;; paste2.el --- Simple interface for paste2.org

;; Filename: paste2.el
;; Description: Simple interface for paste2.org
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-12-09 04:01:16
;; Version: 1.1.3
;; Last-Updated: 2009-04-11 01:53:51
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/paste2.org
;; Keywords: paste2.org, paste
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;; `url' `erc' `rcirc' `thingatpt'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Simple interface for paste2.org
;;
;; This extension to offer a quickly paste on `http://paste2.org'.
;;
;; * Send paste:
;;      Switch `erc' (or `rcirc') channel buffer, then M-x paste2-buffer-create,
;;      will popup a buffer named `*paste2-send*'.
;;
;;      Then write everything you want paste to buffer `*paste2-send*',
;;      type "C-c C-c" when you complete.
;;
;;      Then will prompt for paste title, type title name,
;;      then will upload your paste to `paste2.org',
;;      and will show paste title and paste link in `erc' (or `rcirc') channel buffer.
;;
;;      You can use command `paste2-send-paste-with-command-output' send special
;;      command output to the server.
;;
;; * Get paste:
;;      If someone paste something in `paste2.org'.
;;      You can use command `paste2-get-paste' to get paste,
;;      then will prompt paste number id.
;;      Example someone paste thing at `http://paste2.org/p/123456'
;;      or `http://paste2.org/get/123456',
;;      you can input `123456', `http://paste2.org/p/123456' or
;;      `http://paste2.org/get/123456' to
;;      get paste through `paste2-get-paste'.
;;
;; * Tips:
;;      It's not necessary to use `paste2-buffer-create' with `erc' (or `rcirc') channel
;;      buffer, you can use it with any buffer, but it will prompt for irc
;;      channel name if option `paste2-blank-channel' is `nil'.
;;
;;      Default, if current buffer is mark, then `paste2-buffer-create'
;;      will insert mark region to buffer `paste2-send-buffer'.
;;
;;      You can switch any buffer after use `paste2-buffer-create', then
;;      use command `paste2-buffer-append' will append current buffer or
;;      mark region to buffer `paste2-send-buffer',
;;      If you use `C-u' before `paste2-buffer-append', will switch to
;;      buffer `paste2-send-buffer' after insert.
;;
;;      You can move cursor to paste link, example `http://paste2/p/123456'
;;      or `http://paste2.org/get/123456', then use command `paste2-get-paste',
;;      `paste2-get-paste' will pick up paste id around point `123456',
;;      you just need type RET, everything is complete.
;;
;;      You can type `C-u' before command `paste2-get-paste', the it will
;;      load `emacs-lisp-mode' syntax highlight for paste buffer.
;;
;;      By default, all send paste function will check send string, if string
;;      is null, will ignore and stop send.
;;

;;; Installation:
;;
;; Put paste2.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'paste2)
;;
;; And this have below command for use:
;;
;;      `paste2-send-paste'                     send paste current buffer content quickly.
;;      `paste2-send-paste-with-command-output' send paste with command output.
;;      `paste2-get-paste'                      get paste with special paste-id.
;;      `paste2-buffer-create'                  create a buffer and then send content of buffer.
;;      `paste2-buffer-append'                  append content to buffer `paste2-buffer'.
;;
;; Enjoy!
;;

;;; Customize:
;;
;; Option `paste2-blank-announce' default nil for prompt announce.
;; If you don't send announce, setup it with t.
;;
;; Option `paste2-blank-channel' default nil for prompt channel.
;; Then i will use `paste2-buffer-channel' to get channel from current buffer
;; if current major-mode is `erc-mode' (or `rcirc-mode').
;; And it will try to show link in corresponding IRC channel after paste completed.
;; If you don't want to use those, set it to t.
;;
;; Option `paste2-user' is user name for paste, default is nil.
;;
;; Option `paste2-send-buffer' is the name of buffer that send paste content,
;; you can customize the name of buffer.
;;
;; Option `paste2-get-buffer' is the name of buffer that get paste content,
;; you can customize the name of buffer.
;;
;; And all above option can customize easy through:
;;      M-x RET customize-group RET paste2 RET
;;

;;; Change log:
;;
;; 2009/04/11
;;      * Add new command `paste2-send-paste-with-command-output'
;;      * Add new function `paste2-send-paste-string'
;;      * Check all send paste string, won't send null string.
;;
;; 2009/03/11
;;      * Fix bugs.
;;
;; 2009/02/17
;;      * Load `emacs-lisp-mode' syntax highlight if type `C-u'
;;        before command `paste2-get-paste'.
;;
;; 2009/02/16
;;      * Don't depend `w3m' anymore.
;;
;; 2009/01/30
;;      * Support `rcirc'.
;;
;; 2009/01/08
;;      * Improve coding transform algorithm.
;;      * Fix bug.
;;
;; 2009/01/05
;;      * Remove function `paste2-get-paste-with-w3m', it's not
;;        necessary.
;;      * Make function `paste2-get-paste' can accept paste2.org link
;;        or link id.
;;      * Fix doc.
;;
;; 2008/12/30
;;      * Add new option `paste2-get-temp-format' for format
;;        temp file that storage paste.
;;      * Fix the coding problem with `paste2-get-paste'.
;;
;; 2008/12/27
;;      * Add argument `prefix' to function `paste2-buffer-append'.
;;        If use `C-u' with `paste2-buffer-append', will switch to
;;        buffer `paste2-send-buffer' after append.
;;      * Fix doc.
;;
;; 2008/12/12
;;      * Add `paste2-get-buffer', and make `paste2-get-paste' asynchronous.
;;      * Use `paste2-send-buffer' replace `paste2-buffer'.
;;      * Modified some functions name.
;;      * Make function `paste2-get-paste' pick-up content instead return HTML.
;;      * Use `paste2-send-paste-region' replace `paste2-send-paste'.
;;      * Use `paste2-send-paste' replace `paste2-paste'.
;;      * Fix doc.
;;
;; 2008/12/10
;;      * When use command `paste2-buffer-create', add mark region
;;        to buffer `paste2-buffer' when current buffer is mark active.
;;
;; 2008/12/09
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      Richard Riley           <rileyrgdev@googlemail.com>     for advice.
;;      QinGW (IRC nickname)    <qingwuking@gmail.com>          for advice.
;;

;;; TODO
;;
;;

;;; Require
(require 'url)
(require 'erc)
(require 'rcirc)
(require 'thingatpt)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup paste2 nil
  "Simple interface for paste2.org"
  :group 'external)

(defcustom paste2-server "http://paste2.org"
  "Base URL for the paste2 server."
  :type 'string
  :group 'paste2)

(defcustom paste2-blank-announce nil
  "If non-nil, don't send a title to the server.
Otherwise, prompt announce every time."
  :type 'boolean
  :group 'paste2)

(defcustom paste2-blank-channel nil
  "If non-nil, don't send paste link to IRC channel.
Otherwise, prompt announce every time.

If nil, will try to get channel name from current buffer.
And will try to show paste link in corresponding channel."
  :type 'boolean
  :group 'paste2)

(defcustom paste2-user nil
  "The user name for paste."
  :type 'string
  :group 'paste2)

(defcustom paste2-send-buffer "*paste2-send*"
  "The buffer for send paste content."
  :type 'string
  :group 'paste2)

(defcustom paste2-get-buffer "*paste2-get*"
  "The buffer for get paste content."
  :type 'string
  :group 'paste2)

(defcustom paste2-buffer-help
  ";; Enter you paste below, and press C-c C-c to send.\n;; Press C-c C-d to cancel this paste.\n\n"
  "Paste buffer help text."
  :type 'string
  :group 'paste2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar paste2-last-paste-id nil
  "Numerical ID of the last paste.")

(defvar paste2-last-paste-announce nil
  "The announce of the last paste.")

(defvar paste2-last-paste-channel nil
  "The channel of the last paste.")

(defvar paste2-last-irc-client nil
  "The last irc client you use.")

(defvar paste2-buffer-channel nil
  "Try to get channel from current buffer.
When option `paste2-blank-channel' is nil.")

(defvar paste2-window-configuration nil
  "The window configuration that save before create paste.")

(defvar paste2-after-send-paste-hook nil
  "Run after `paste2-after-send-paste'.")

(defvar paste2-irc-client nil
  "The irc client you last use.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun paste2-send-paste ()
  "Send paste content region that you select.
If select nothing, paste entire buffer."
  (interactive)
  (let ((paste-start (point-min))
        (paste-end (point-max)))
    ;; Get paste content.
    (when mark-active
      (setq paste-start (region-beginning))
      (setq paste-end (region-end))
      (deactivate-mark))
    ;; Try to get channel name.
    (paste2-buffer-get-channel)
    ;; Read only
    (setq buffer-read-only t)
    ;; Paste content.
    (paste2-send-paste-region paste-start paste-end)))

(defun paste2-send-paste-with-command-output (command)
  "Read COMMAND and paste command result to server."
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (and buffer-file-name
                             (file-relative-name buffer-file-name)))))
  ;; Try to get channel name.
  (paste2-buffer-get-channel)
  ;; Send command output to the server.
  (paste2-send-paste-string
   (flet ((message (&rest args)))       ;won't output in minibuffer
     (with-temp-buffer
       (shell-command command (buffer-name))
       (buffer-string)))))

(defun paste2-get-paste (&optional id-or-url)
  "Fetch the contents of the paste from paste2.org with ID-OR-URL.
You can input link id or link.
Example, for the link `http://paste2.org/p/12345' or
`http://paste2.org/get/123456'
You can input `12345' `http://paste2.org/p/12345' or
`http://paste2.org/get/123456'.

If you type `C-u' before this command, it will load emacs-lisp
syntax highlight with paste buffer."
  (interactive)
  (let (bounds current-url id url)
    ;; Get id or url when `id-or-url' is `nil'.
    (unless id-or-url
      (setq bounds (bounds-of-thing-at-point 'url))
      (if bounds
          (setq current-url (buffer-substring-no-properties (car bounds) (cdr bounds))))
      (setq id-or-url (read-string (format "Input ID or URL: (%s) " (or current-url ""))
                                   nil nil current-url)))
    ;; Filter `id-or-url' make get valid link id.
    (cond ((string-match (format "^%s/\\(p\\|get\\)/[0-9]+$" paste2-server) id-or-url)
           (string-match "/\\([0-9]*\\)\\(#.*\\)?$" id-or-url)
           (setq id (match-string 1 id-or-url)))
          ((string-match "^[0-9]+$" id-or-url)
           (setq id id-or-url))
          (t
           (error "Please enter a valid 'link id' or 'link'")))
    ;; Generate paste url.
    (setq url (format "%s/get/%s" paste2-server id))
    ;; Get paste.
    (let* ((url-request-method "GET")
           (url-request-extra-headers nil)
           (url-mime-accept-string "*/*")
           (url-parsed (url-generic-parse-url url)))
      (setq paste2-get-buffer (url-retrieve url-parsed 'paste2-after-get-paste
                                            (list id url current-prefix-arg))))))

(defun paste2-buffer-create ()
  "Create a paste buffer for handle paste content.
If current buffer is mark, will insert mark region
to paste buffer `paste2-send-buffer'."
  (interactive)
  (let (mark-region)
    ;; Get mark region if mark active
    (when mark-active
      (setq mark-region (buffer-substring-no-properties (region-beginning) (region-end)))
      (deactivate-mark))
    ;; Remember current window configuration for revert after paste.
    (setq paste2-window-configuration (current-window-configuration))
    ;; Try to get channel name.
    (paste2-buffer-get-channel)
    ;; Create paste buffer.
    (switch-to-buffer (get-buffer-create paste2-send-buffer))
    (erase-buffer)
    ;; Insert help
    (insert paste2-buffer-help)
    ;; Insert mark if mark active
    (if (stringp mark-region)
        (insert mark-region))
    ;; Key bind setup
    (local-set-key (kbd "C-c C-d") 'paste2-buffer-quit)
    (local-set-key (kbd "C-c C-c") 'paste2-buffer-send)))

(defun paste2-buffer-append (&optional prefix)
  "Append content to `paste2-buffer'.

If select region is active, append region to `paste2-send-buffer',
otherwise append entire buffer.

If `PREFIX' is `non-nil', will switch to `paste2-send-buffer' after
append."
  (interactive "P")
  (if (bufferp (get-buffer paste2-send-buffer))
      (let ((append-start (point-min))
            (append-end (point-max))
            append-content)
        ;; Get append content.
        (when mark-active
          (setq append-start (region-beginning))
          (setq append-end (region-end))
          (deactivate-mark))
        (setq append-content (buffer-substring-no-properties append-start append-end))
        ;; Append to buffer `paste2-send-buffer'.
        (with-current-buffer paste2-send-buffer
          (goto-char (point-max))
          (insert append-content))
        ;; Whether switch to buffer `paste2-send-buffer'
        ;; after append.
        (if prefix
            (switch-to-buffer paste2-send-buffer)))
    (message "Buffer %s is not exist." paste2-send-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun paste2-send-paste-region (beg end)
  "Send region that from `BEG' to `END' the server specified in `paste2-server'."
  (paste2-send-paste-string (buffer-substring-no-properties beg end)))

(defun paste2-send-paste-string (str)
  "Send string `STR' to the server specified in `paste2-server'.

If option `paste2-blank-announce' is nil, the announce will be prompted.
If option `paste2-blank-channel' is nil,
and variable `paste2-buffer-channel' is nil, the channel name will be prompted.
The variable `paste2-buffer-channel' will try to setup
in `paste2-send-paste' or `paste2-buffer-create'.
If current buffer is at IRC buffer, setup `paste2-buffer-channel'
with current channel name, otherwise, ignore it.

For more information on paste2, see http://paste2.org"
  (if (null str)
      ;; Don't send null string.
      (message "Paste content is null, ignored.")
    ;; Send paste.
    (paste2-prompt-for-channel)         ;try to prompt for channel
    (let* ((announce (paste2-prompt-for-announce))
           ;; Need `/command-line' paste to paste2.org.
           ;; Don't use `/new-paste' to do that, can't effect.
           ;; `/new-paste' just works with web-page.
           (url (concat paste2-server "/command-line"))
           (url-request-method "POST")
           (url-request-extra-headers
            '(("Content-Type" . "application/x-www-form-urlencoded")))
           (url-mime-accept-string "*/*")
           (url-request-data
            (format "user=%s&filename=%s&code=%s"
                    (url-hexify-string (or paste2-user ""))
                    (url-hexify-string (buffer-name))
                    (url-hexify-string str))))
      (url-retrieve url 'paste2-after-send-paste))))

(defun paste2-after-send-paste (&optional redirect)
  "Callback that run after send paste is made.
Messages the user and tell them that
everything went smoothly, and save the paste
ID for use as a default ID for annotations.
REDIRECT is default argument for check status."
  (let ((url (cadr redirect)))
    (if (eq (car redirect) ':redirect)
        (let (id)
          (message "Paste successful: %s" url)
          (kill-new url)                ; kill to copy
          ;; Get Number ID
          (string-match "/\\([0-9]*\\)\\(#.*\\)?$" url)
          (setq id (match-string 1 url))
          (if id
              (setq paste2-last-paste-id id))
          ;; Run hook
          (run-hooks 'paste2-after-send-paste-hook))
      ;; Send paste failed.
      (message "Paste failed: %s" url))))

(defun paste2-after-get-paste (&optional redirect id url load-elisp-highlight)
  "Callback that run after get paste is made.
REDIRECT is default argument for check status.
Messages the user and tell them that everything went smoothly,
and save the paste ID for use as a default ID for annotations.
URL is fetch url.
If LOAD-ELISP-HIGHLIGHT is non-nil, load emacs-lisp mode syntax highlight."
  (if (eq (car redirect) ':error)
      ;; Notify user and kill buffer when occur error.
      (with-current-buffer paste2-get-buffer
        (message "Get paste from %s failed." url)
        (kill-buffer paste2-get-buffer))
    ;; Otherwise display paste.
    (let (paste-string)
      ;; Record last paste id.
      (setq paste2-last-paste-id id)
      (declare (special url-http-end-of-headers))
      ;; Get paste.
      (with-current-buffer paste2-get-buffer
        ;; Decode information.
        (set-buffer-multibyte t)                 ;enable multibyte flag.
        (goto-char (1+ url-http-end-of-headers)) ;skip http header
        (decode-coding-region                    ;decode region
         (point) (point-max)
         (coding-system-change-eol-conversion 'utf-8 'dos))
        (setq paste-string (buffer-substring (point) (point-max)))
        ;; Insert decoded information.
        (erase-buffer)
        (insert paste-string)
        (goto-char (point-min))
        ;; Load `emacs-lisp' syntax highlight,
        ;; if option `load-elisp-highlight' is non-nil.
        (when load-elisp-highlight
          (set-syntax-table emacs-lisp-mode-syntax-table)
          (lisp-mode-variables)
          (setq font-lock-mode t)
          (font-lock-fontify-buffer))
        ;; Switch to buffer.
        (switch-to-buffer (current-buffer))
        (message "Get paste from %s successful." url)))))

(defun paste2-prompt-for-announce ()
  "Prompt user input annotate.
When option `paste2-blank-announce' is nil."
  (if paste2-blank-announce
      (setq paste2-last-paste-announce "") ;blank announce
    (setq paste2-last-paste-announce       ;read from input
          (read-string (format "Announce (%s): " (or paste2-last-paste-announce "")) nil nil paste2-last-paste-announce))))

(defun paste2-prompt-for-channel ()
  "Prompt user input channel name.
When option `paste2-blank-channel' is nil.

And clear the value of `paste2-buffer-channel'."
  (if paste2-blank-channel
      (setq paste2-last-paste-channel nil)
    (if paste2-buffer-channel
        (setq paste2-last-paste-channel paste2-buffer-channel)
      ;; Get paste channel.
      (setq paste2-last-paste-channel
            (read-string (format "Channel <%s>: " (or paste2-last-paste-channel "")) nil nil paste2-last-paste-channel))
      ;; Get irc client.
      (setq paste2-irc-client (completing-read (format "IRC client (%s): " (or paste2-last-irc-client ""))
                                               (list "erc" "rcirc")
                                               nil nil paste2-last-irc-client))
      (setq paste2-last-irc-client paste2-irc-client)
      ;; Adjust buffer name when irc client is `rcirc'.
      (if (string-equal paste2-irc-client "rcirc")
          (catch 'match
            (dolist (buffer (buffer-list))
              (set-buffer buffer)
              (when (and (equal major-mode 'rcirc-mode)
                         (string-match paste2-last-paste-channel (buffer-name)))
                (setq paste2-last-paste-channel (buffer-name))
                (throw 'match "Match rcirc buffer.")))))))
  (setq paste2-buffer-channel nil))

(defun paste2-buffer-get-channel ()
  "Try to get channel name from current buffer.
When option `paste2-blank-channel' is nil."
  (unless paste2-blank-channel
    ;; Get buffer channel from `erc' or `rcirc'
    (setq paste2-buffer-channel (buffer-name))
    ;; Setup irc client.
    (setq paste2-irc-client
          (cond ((eq major-mode 'erc-mode)
                 "erc")
                ((eq major-mode 'rcirc-mode)
                 "rcirc")
                (t
                 "")))))

(defun paste2-buffer-send ()
  "Send content in buffer `paste2-send-buffer'."
  (interactive)
  (if (bufferp (get-buffer paste2-send-buffer))
      (with-current-buffer paste2-send-buffer
        (let (paste-content)
          (goto-char (point-min))
          (setq paste-content (buffer-string))
          (string-match paste2-buffer-help paste-content)
          (setq paste-content (replace-match "" nil nil paste-content 0))
          (erase-buffer)
          (insert paste-content)
          (setq buffer-read-only t)
          (paste2-send-paste-region (point-min) (point-max))
          (paste2-buffer-quit)))
    (message "Buffer %s is not exist." paste2-send-buffer)))

(defun paste2-buffer-quit ()
  "Quit from buffer `paste2-send-buffer'."
  (interactive)
  (if (bufferp (get-buffer paste2-send-buffer))
      (with-current-buffer paste2-send-buffer
        (kill-buffer paste2-send-buffer)
        (when (and (boundp 'paste2-window-configuration)
                   paste2-window-configuration)
          (set-window-configuration paste2-window-configuration)
          (setq paste2-window-configuration nil)))
    (message "Buffer %s is not exist." paste2-send-buffer)))

(defun paste2-irc-show-link ()
  "Show paste link in corresponding channel.
When option `paste2-blank-channel' is nil.

If `paste2-last-paste-channel' is non-nil,
show paste link in channel buffer automatically,
and don't need switch.

Otherwise, prompt user that can't find buffer `paste2-send-buffer'."
  (unless paste2-blank-channel
    (if (and paste2-last-paste-channel
             (bufferp (get-buffer paste2-last-paste-channel)))
        (with-current-buffer paste2-last-paste-channel
          (let ((irc-notify-msg (format "pasted \"%s\" at %s/get/%s"
                                        paste2-last-paste-announce
                                        paste2-server
                                        paste2-last-paste-id))
                (echo-notify-msg (format "Have show link %s/get/%s in %s successfully."
                                         paste2-server
                                         paste2-last-paste-id
                                         paste2-last-paste-channel)))
            (cond ((string-equal paste2-irc-client "erc")
                   (erc-send-action (erc-default-target) irc-notify-msg))
                  ((string-equal paste2-irc-client "rcirc")
                   (rcirc-send-message (rcirc-buffer-process) rcirc-target irc-notify-msg)))
            (message echo-notify-msg)))
      (message "Buffer '%s' is not exist, so show link failed." paste2-last-paste-channel))))

(add-hook 'paste2-after-send-paste-hook 'paste2-irc-show-link)

(provide 'paste2)

;;; paste2.el ends here

;;; LocalWords:  el erc thingatpt irc QinGW tmp www urlencoded eol msg fontify
