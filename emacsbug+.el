;;; emacsbug+.el --- Extensions to `emacsbug.el'.
;;
;; Filename: emacsbug+.el
;; Description: Extensions to `emacsbug.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2013-2015, Drew Adams, all rights reserved.
;; Created: Sat Jan 19 15:24:48 2013 (-0800)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Jan  1 10:37:18 2015 (-0800)
;;           By: dradams
;;     Update #: 367
;; URL: http://www.emacswiki.org/emacsbug%2b.el
;; Keywords: report bug
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `emacsbug', `sendmail'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Extensions to `emacsbug.el'.
;;
;;  If you have customized option `sendmail-query-once-function' then
;;  you probably want to load this after, not before, you load your
;;  `custom-file'.  Or if you customize `sendmail-query-once-function'
;;  in your `~/.emacs' then you probably want to load this after doing
;;  that.  See Emacs bug #13506.
;;
;;
;;  User options defined here:
;;
;;    `ebp-report-emacs-bug-included-fields'.
;;
;;  Commands defined here:
;;
;;    `ebp-insert-all', `ebp-insert-features',
;;    `ebp-insert-load-path-shadows', `ebp-insert-major-mode',
;;    `ebp-insert-minor-modes', `ebp-insert-recent-input',
;;    `ebp-insert-recent-messages', `ebp-insert-settings',
;;    `ebp-insert-version'.
;;
;;  Internal variables defined here:
;;
;;    `ebp-from-buffer', `ebp-insert-option-map',
;;    `ebp-message-end-point', `ebp-recent-keys'.
;;
;;
;;  ***** NOTE: The following function defined in `emacsbug.el' has
;;              been REDEFINED HERE:
;;
;;  `report-emacs-bug' - Fit `ebp-report-emacs-bug-included-fields'.
;;                       Bind `ebp-insert-*' commands to keys.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2013/01/20 dadams
;;     Added: ebp-insert-all, ebp-insert-features, ebp-insert-load-path-shadows,
;;            ebp-insert-major-mode, ebp-insert-minor-modes, ebp-insert-recent-input,
;;            ebp-insert-recent-messages, ebp-insert-settings, ebp-insert-version, ebp-from-buffer,
;;            ebp-insert-option-map, ebp-message-end-point, ebp-recent-keys, defgroup emacsbug-plus.
;;     Renamed: report-emacs-bug-included-fields to ebp-report-emacs-bug-included-fields.
;;     report-emacs-bug: Bind ebp-insert-* commands to keys.
;; 2013/01/19 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'emacsbug)


(defvar emacs-bzr-version)
(defvar locale-coding-system)
(defvar message-strip-special-text-properties)
(defvar minor-mode-list)
(defvar report-emacs-bug-send-command)
(defvar report-emacs-bug-send-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ebp-from-buffer nil
  "Buffer from which `report-emacs-bug' is called.")

(defvar ebp-insert-option-map ()
  "Keymap for optional-info insertion keys, for `report-emacs-bug'.")

(defvar ebp-message-end-point nil
  "`point-max-marker' in `*Messages*' when `report-emacs-bug' is called.")

(defvar ebp-recent-keys nil
  "Value from function `recent-keys' when `report-emacs-bug' is called.")

;;;###autoload
(defgroup emacsbug-plus nil
  "Inclusion of optional bug-reporting information."
  :prefix "ebp"
  :group 'emacsbug :group 'mail
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
emacsbug+.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Other Libraries by Drew"
          "http://www.emacswiki.org/cgi-bin/wiki/DrewsElispLibraries")
  :link '(url-link :tag "Download"
          "http://www.emacswiki.org/cgi-bin/wiki/emacsbug+.el"))

;;;###autoload
(defcustom ebp-report-emacs-bug-included-fields '(version settings major-mode
                                                  minor-modes recent-input
                                                  recent-messages load-shadows
                                                  features)
  "Fields to include by default for command `report-emacs-bug'.
The fields are included automatically in the buffer where you edit
your bug report."
  :type '(set
          (const :tag "Emacs version info" version)
          (const :tag "Important settings" settings)
          (const :tag "Major mode"         major-mode)
          (const :tag "Minor modes"        minor-modes)
          (const :tag "Recent input"       recent-input)
          (const :tag "Recent messages"    recent-messages)
          (const :tag "Load-path shadows"  load-shadows)
          (const :tag "Features"           features))
  :group 'emacsbug-plus :group 'convenience)

;;;###autoload
(defun ebp-insert-all ()
  "Insert all optional info at point in bug report buffer.
This is the same as using all of the other `ebp-insert-*' commands.
It has the same effect as the default value of option
`ebp-report-emacs-bug-included-fields'."
  (interactive)
  (ebp-insert-version)
  (ebp-insert-settings)
  (ebp-insert-major-mode)
  (when (boundp 'minor-mode-list) (ebp-insert-minor-modes))
  (ebp-insert-recent-input)
  (ebp-insert-recent-messages)
  (when (or (> emacs-major-version 23)  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
    (ebp-insert-load-path-shadows))
  (ebp-insert-features))

;;;###autoload
(defun ebp-insert-version ()
  "Insert Emacs version info at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'version ebp-report-emacs-bug-included-fields))
    (error "Version info is already included"))
  (insert "\n\nIn " (emacs-version) "\n")
  (when (and (boundp 'emacs-bzr-version)  (stringp emacs-bzr-version))
    (insert "Bzr revision: " emacs-bzr-version "\n"))
  (when (fboundp 'x-server-vendor)
    (condition-case nil
        ;; This is used not only for X11 but also W32 and others.
        (insert "Windowing system distributor `" (x-server-vendor)
                "', version " (mapconcat 'number-to-string (x-server-version) ".") "\n")
      (error t)))
  (let ((lsb  (with-temp-buffer (and (eq 0 (condition-case nil
                                               (call-process "lsb_release" nil '(t nil) nil "-d")
                                             (error nil)))
                                     (buffer-string)))))
    (when (stringp lsb) (insert "System " lsb "\n")))
  (when (and system-configuration-options  (not (equal system-configuration-options "")))
    (insert "Configured using:\n `configure " system-configuration-options "'\n\n")
    (fill-region (line-beginning-position -1) (point))))

;;;###autoload
(defun ebp-insert-settings ()
  "Insert important Emacs settings info at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'settings ebp-report-emacs-bug-included-fields))
    (error "Settings info is already included"))
  (insert "Important settings:\n")
  (mapc (lambda (var)
          (let ((val  (getenv var))) (when val (insert (format "  value of $%s: %s\n" var val)))))
        '("EMACSDATA" "EMACSDOC" "EMACSLOADPATH" "EMACSPATH" "LC_ALL" "LC_COLLATE" "LC_CTYPE"
          "LC_MESSAGES" "LC_MONETARY" "LC_NUMERIC" "LC_TIME" "LANG" "XMODIFIERS"))
  (when (boundp 'locale-coding-system)
    (insert (format "  locale-coding-system: %s\n" locale-coding-system)))
  (insert (format "  default enable-multibyte-characters: %s\n"
                  (default-value 'enable-multibyte-characters)))
  (insert "\n"))

;;;###autoload
(defun ebp-insert-major-mode ()
  "Insert major-mode info at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'major-mode ebp-report-emacs-bug-included-fields))
    (error "Major-mode info is already included"))
  (insert (format "Major mode: %s\n"
                  (if (fboundp 'format-mode-line)
                      (format-mode-line (buffer-local-value 'mode-name ebp-from-buffer)
                                        nil nil ebp-from-buffer)
                    (with-current-buffer ebp-from-buffer mode-name))))
  (insert "\n"))

;;;###autoload
(defun ebp-insert-minor-modes ()
  "Insert info about enabled minor modes at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'minor-modes ebp-report-emacs-bug-included-fields))
    (error "Minor-modes info is already included"))
  (insert "Minor modes in effect:\n")
  (dolist (mode  minor-mode-list)
    (when (and (boundp mode)  (buffer-local-value mode ebp-from-buffer))
      (insert (format "  %s: %s\n" mode (buffer-local-value mode ebp-from-buffer)))))
  (insert "\n"))

;;;###autoload
(defun ebp-insert-recent-input ()
  "Insert recent input log at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'recent-input ebp-report-emacs-bug-included-fields))
    (error "Recent input info is already included"))
  (insert "Recent input:\n")
  (let ((before-keys  (point)))
    (insert (mapconcat (lambda (key)
                         (if (or (integerp key)  (symbolp key)  (listp key))
                             (single-key-description key)
                           (prin1-to-string key nil)))
                       (or ebp-recent-keys  (recent-keys))
                       " "))
    (save-restriction (narrow-to-region before-keys (point))
                      (goto-char before-keys)
                      (while (progn (move-to-column 50) (not (eobp)))
                        (search-forward " " nil t)
                        (insert "\n")))))

;;;###autoload
(defun ebp-insert-recent-messages ()
  "Insert log of recent Emacs messages at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'recent-messages ebp-report-emacs-bug-included-fields))
    (error "Recent messages are already included"))
  (let ((message-buf  (get-buffer "*Messages*")))
    (when message-buf
      (let ((end-pos  ebp-message-end-point)
            beg-pos)
        (with-current-buffer message-buf
          (goto-char end-pos)
          (forward-line -10)
          (setq beg-pos  (point)))
        (insert "\n\nRecent messages:\n")
        (insert-buffer-substring message-buf beg-pos ebp-message-end-point))))
  ;; After `Recent messages', to avoid the messages produced by `list-load-path-shadows'.
  (unless (eq ?\n (char-before)) (insert "\n"))
  (insert "\n"))

;;;###autoload
(defun ebp-insert-load-path-shadows ()
  "Insert info about `load-path' shadows at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'load-shadows ebp-report-emacs-bug-included-fields))
    (error "`load-path' shadowing info is already included"))
  (insert "Load-path shadows:\n")
  (let* ((msg      "Checking for load-path shadows...")
         (result   "done")
         (shadows  (progn (message "%s" msg)
                          (condition-case nil
                              (list-load-path-shadows t)
                            (error (setq result  "error") "Error during checking")))))
    (message "%s%s" msg result)
    (insert (if (zerop (length shadows))  "None found.\n"  shadows))))

;;;###autoload
(defun ebp-insert-features ()
  "Insert the list of loaded features at point in bug report buffer."
  (interactive)
  (when (and (interactive-p)  (member 'featuers ebp-report-emacs-bug-included-fields))
    (error "Loaded-features list is already included"))
  (insert (format "\nFeatures:\n%s\n" features))
  (fill-region (line-beginning-position 0) (point)))

;;;###autoload
(defun report-emacs-bug (topic &optional recent-keys)
  "Report a bug in GNU Emacs.
Prompts for bug subject.  Leaves you in a mail buffer."
  ;; This strange form ensures that (recent-keys) is the value before
  ;; the bug subject string is read.
  (interactive (reverse (list (recent-keys) (read-string "Bug Subject: "))))
  ;; The syntax `version;' is preferred to `[version]' because the latter could be
  ;; mistakenly stripped by mailing software.
  (if (eq system-type 'ms-dos)
      (setq topic  (concat emacs-version "; " topic))
    (when (string-match "^\\(\\([.0-9]+\\)*\\)\\.[0-9]+$" emacs-version)
      (setq topic  (concat (match-string 1 emacs-version) "; " topic))))
  (let* ((from-buffer      (current-buffer))
         (can-insert-mail  (or (and (fboundp 'report-emacs-bug-can-use-xdg-email)
                                    (report-emacs-bug-can-use-xdg-email))
                               (and (fboundp 'report-emacs-bug-can-use-osx-open)
                                    (report-emacs-bug-can-use-osx-open))))
         (ins-features     (member 'features ebp-report-emacs-bug-included-fields))
         (ins-input        (member 'recent-input ebp-report-emacs-bug-included-fields))
         (ins-loads        (and (member 'load-shadows ebp-report-emacs-bug-included-fields)
                                (or (> emacs-major-version 23)  (and (= emacs-major-version 23)
                                                                     (> emacs-minor-version 1)))))
         (ins-major        (member 'major-mode ebp-report-emacs-bug-included-fields))
         (ins-minor        (and (member 'minor-modes ebp-report-emacs-bug-included-fields)
                                (boundp 'minor-mode-list)))
         (ins-messages     (member 'recent-messages ebp-report-emacs-bug-included-fields))
         (ins-settings     (member 'settings ebp-report-emacs-bug-included-fields))
         (ins-version      (member 'version ebp-report-emacs-bug-included-fields))
         (ins-all          (and ins-version  ins-settings  ins-major  ins-minor
                                ins-input    ins-messages  ins-loads  ins-features))
         (ins-none         (not (or ins-version  ins-settings  ins-major  ins-minor
                                    ins-input    ins-messages  ins-loads  ins-features)))
         user-point)
    (compose-mail report-emacs-bug-address topic)
    (set (make-local-variable 'ebp-from-buffer) from-buffer)
    (set (make-local-variable 'ebp-message-end-point)
         (with-current-buffer (get-buffer-create "*Messages*") (point-max-marker)))
    (set (make-local-variable 'ebp-recent-keys) recent-keys)
    ;; The rest of this does not execute if the user was asked to confirm and said no.
    (when (and (eq major-mode 'message-mode)  (fboundp 'message-sort-headers))
      ;; Message-mode sorts the headers before sending.  We sort now so that
      ;; `report-emacs-bug-orig-text' remains valid.  (Bug#5178)
      (message-sort-headers)
      ;; Stop message-mode stealing the properties we will add.
      (set (make-local-variable 'message-strip-special-text-properties) nil))
    (rfc822-goto-eoh)
    (forward-line 1)
    ;; Move the mail signature to the proper place.
    (let ((signature          (buffer-substring (point) (point-max)))
	  (inhibit-read-only  t))
      (delete-region (point) (point-max))
      (insert signature)
      (backward-char (length signature)))
    (unless report-emacs-bug-no-explanations
      ;; Insert warnings for novice users.
      (cond ((not (equal "bug-gnu-emacs@gnu.org" report-emacs-bug-address))
             (insert (format "The report will be sent to %s.\n\n" report-emacs-bug-address)))
            ((not (fboundp 'insert-text-button))
             (insert "This bug report will be sent to the Free Software Foundation.\n\n"))
            (t
             (insert "This bug report will be sent to the ")
             (insert-text-button
              "Bug-GNU-Emacs"
              'face 'link
              'help-echo (concat "mouse-2, RET: Follow this link")
              'action (lambda (button)
                        (browse-url "http://lists.gnu.org/archive/html/bug-gnu-emacs/"))
              'follow-link t)
             (insert " mailing list\nand the GNU bug tracker at ")
             (insert-text-button
              "debbugs.gnu.org"
              'face 'link
              'help-echo (concat "mouse-2, RET: Follow this link")
              'action (lambda (button)
                        (browse-url "http://debbugs.gnu.org/"))
              'follow-link t)
             (insert ".\n\n")))
      (insert "Please ensure that the `From:' line contains a valid email address.\n")
      (insert "After a delay of up to one day, you should receive an acknowledgment
at that address.

Please write in English if possible, as the Emacs maintainers
usually do not have translators for other languages.\n\n"))

    (insert "Please describe exactly what actions triggered the bug, and\n"
	    "the precise symptoms of the bug.  If you can, give a recipe\n"
	    "to reproduce it, starting from `emacs -Q':\n\n")
    (when (fboundp 'delete-and-extract-region)
      (let ((txt  (delete-and-extract-region
                   (save-excursion (rfc822-goto-eoh) (line-beginning-position 2))
                   (point))))
        (insert (propertize "\n" 'display txt))))
    (setq user-point (point))
    (insert "\n\n")
    (insert "If Emacs crashed, and you have the Emacs process in the gdb debugger,\n"
	    "please include the output from the following gdb commands:\n"
	    "    `bt full' and `xbacktrace'.\n")
    (let ((debug-file  (expand-file-name "DEBUG" data-directory)))
      (when (file-readable-p debug-file)
        (insert "For information about debugging Emacs, please read the file\n" debug-file ".\n")))
    (when (fboundp 'delete-and-extract-region)
      (let ((txt  (delete-and-extract-region (1+ user-point) (point))))
        (insert (propertize "\n" 'display txt))))

    (when ins-version  (ebp-insert-version))
    (when ins-settings (ebp-insert-settings))
    (when ins-major    (ebp-insert-major-mode))
    (when ins-minor    (ebp-insert-minor-modes))
    (when ins-input    (ebp-insert-recent-input))
    (when ins-messages (ebp-insert-recent-messages))
    (when ins-loads    (ebp-insert-load-path-shadows))
    (when ins-features (ebp-insert-features))

    (unless (and ins-version  ins-settings  ins-major  ins-minor
                 ins-input    ins-messages  ins-loads  ins-features)
      (setq ebp-insert-option-map  (make-sparse-keymap))
      (define-key (current-local-map) "\C-o" ebp-insert-option-map)
      (when   ins-none     (define-key ebp-insert-option-map "a" 'ebp-insert-all))
      (unless ins-features (define-key ebp-insert-option-map "f" 'ebp-insert-features))
      (unless ins-input    (define-key ebp-insert-option-map "i" 'ebp-insert-recent-input))
      (unless ins-major    (define-key ebp-insert-option-map "j" 'ebp-insert-major-mode))
      (unless ins-loads    (define-key ebp-insert-option-map "l" 'ebp-insert-load-path-shadows))
      (unless ins-messages (define-key ebp-insert-option-map "m" 'ebp-insert-recent-messages))
      (unless ins-minor    (define-key ebp-insert-option-map "n" 'ebp-insert-minor-modes))
      (unless ins-settings (define-key ebp-insert-option-map "s" 'ebp-insert-settings))
      (unless ins-version  (define-key ebp-insert-option-map "v" 'ebp-insert-version)))

    ;; This is so the user has to type something in order to send easily.
    (use-local-map (nconc (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-c\C-i" (if (fboundp 'info-emacs-bug)
                                                   'info-emacs-bug
                                                 'report-emacs-bug-info))
    (when (and can-insert-mail  (fboundp 'report-emacs-bug-insert-to-mailer))
      (define-key (current-local-map) "\C-cm" 'report-emacs-bug-insert-to-mailer))
    (when (boundp 'report-emacs-bug-send-command)
      (setq report-emacs-bug-send-command  (get mail-user-agent 'sendfunc)
            report-emacs-bug-send-hook     (get mail-user-agent 'hookvar))
      (when report-emacs-bug-send-command
        (setq report-emacs-bug-send-command  (symbol-name report-emacs-bug-send-command))))
    (unless report-emacs-bug-no-explanations
      (with-output-to-temp-buffer "*Bug Help*"
	(princ "While in the mail buffer:\n\n")
        (let ((report-emacs-bug-send-command  (if (boundp 'report-emacs-bug-send-command)
                                                  report-emacs-bug-send-command
                                                'mail-send-and-exit)))
          (if report-emacs-bug-send-command
              (princ (substitute-command-keys
                      (format "  Type \\[%s] to send the bug report.\n"
                              report-emacs-bug-send-command)))))
	(princ (substitute-command-keys
		"  Type \\[kill-buffer] RET to cancel (don't send it).\n"))

        (when (and (keymapp ebp-insert-option-map)  (not ins-all))
          (princ (substitute-command-keys
                  "\n  These keys insert optional information:\n"))
          (when ins-none
            (princ (substitute-command-keys
                    "    \\[ebp-insert-all]\t - ALL optional information (see below)\n")))
          (unless ins-version
            (princ (substitute-command-keys
                    "    \\[ebp-insert-version]\t - Version information\n")))
          (unless ins-settings
            (princ (substitute-command-keys
                    "    \\[ebp-insert-settings]\t - Important settings\n")))
          (unless ins-major
            (princ (substitute-command-keys
                    "    \\[ebp-insert-major-mode]\t - Major-mode information\n")))
          (unless ins-minor
            (princ (substitute-command-keys
                    "    \\[ebp-insert-minor-modes]\t - Minor-modes information\n")))
          (unless ins-input
            (princ (substitute-command-keys
                    "    \\[ebp-insert-recent-input]\t - Recent inputs\n")))
          (unless ins-messages
            (princ (substitute-command-keys
                    "    \\[ebp-insert-recent-messages]\t - Recent messages (see also buffer \
`*Messages*')\n")))
          (unless ins-loads
            (princ (substitute-command-keys
                    "    \\[ebp-insert-load-path-shadows]\t - Information about `load-path' \
shadows\n")))
          (unless ins-features
            (princ (substitute-command-keys
                    "    \\[ebp-insert-features]\t - The list of loaded features\n")))
          (princ (substitute-command-keys
                  "\n    You can customize option `ebp-report-emacs-bug-included-fields'
    to insert any or all such information automatically.\n")))

	(when (and can-insert-mail  (fboundp 'report-emacs-bug-insert-to-mailer))
          (princ (substitute-command-keys
                  "  Type \\[report-emacs-bug-insert-to-mailer] to copy text to your \
preferred mail program.\n")))
	(terpri)
	(princ (substitute-command-keys
                (format
                 "  Type \\[%s] to visit, in Info, the Emacs Manual section about when
    and how to write a bug report, and what information you should
    include to help fix the bug."
                 (if (fboundp 'info-emacs-bug)
                     'info-emacs-bug
                   'report-emacs-bug-info))))
        (unless (and (keymapp ebp-insert-option-map)  (not ins-all))
          (princ (substitute-command-keys
                  "\n\nYou can customize option `ebp-report-emacs-bug-included-fields' to
choose the types of information included automatically in your report."))))
      (shrink-window-if-larger-than-buffer (get-buffer-window "*Bug Help*")))
    ;; Make it less likely people will send empty messages.
    (if (boundp 'report-emacs-bug-send-hook)
        (when report-emacs-bug-send-hook
          (add-hook report-emacs-bug-send-hook 'report-emacs-bug-hook nil t))
      (make-local-variable 'mail-send-hook)
      (add-hook 'mail-send-hook 'report-emacs-bug-hook))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (make-local-variable 'report-emacs-bug-orig-text)
    (setq report-emacs-bug-orig-text
          (buffer-substring-no-properties (point-min) (point)))
    (goto-char user-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'emacsbug+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacsbug+.el ends here
