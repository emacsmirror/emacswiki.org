;;; text-translator.el --- Text Translator

;; Copyright (C) 2007-2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;;         plus   <MLB33828@nifty.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Translates character strings on Emacs.
;; This package use the text translation service that exists on the internet.

;; Read README.en (English) or README.ja (Japanese).

;;; Code:

(require 'text-translator-vars)

(defun text-translator (arg &optional last engine-or-func)
  "The function which does text translation.
Use Excite, Google and so translation site.
1. Mark is active
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate by type which you selected.
 - Prefix was not supplied.
   Translate range of region that you selected by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)
2. Mark is deactive
 - Prefix was supplied.
   1. Choose translation site which you use.
   2. Translate value which you input from minibuffer by type you selected.
 - Prefix was not supplied.
   Translate value which you input from minibuffer by
   first element of `text-translator-engine-history'.
   (If `text-translator-engine-history' is nil,
    use `text-translator-default-engine'.)"
  (interactive "P")
  (add-to-list 'text-translator-engine-history text-translator-default-engine)
  (let ((minibuffer-history text-translator-engine-history)
        (engine (text-translator-check-valid-translation-engine
                 engine-or-func (car text-translator-engine-history)))
        str)
    ;; If prefix-arg is non-nil, change translation type.
    (when (or arg last)
      (setq engine (completing-read
                    (format "Select translation engine (default %s): " engine)
                    text-translator-site-data-alist nil t nil nil engine)))
    (setq str
          (cond
           (last
            text-translator-last-string)
           (mark-active
            (buffer-substring-no-properties (region-beginning) (region-end)))
           (t
            (read-string
             (format "Enter string translated by %s: " engine)))))
    (text-translator-client
     (text-translator-check-valid-translation-engine
      (and (functionp engine-or-func) (funcall engine-or-func engine str))
      engine)
     str)))

(defun text-translator-translate-by-auto-selection (arg)
  "Function that translates by auto selection of translation engine.
Function that select automatically is value of `text-translator-auto-selection-func'."
  (interactive "P")
  (text-translator arg nil text-translator-auto-selection-func))

(defun text-translator-translate-by-auto-selection-enja (engine str)
  "Automatic selection function for English to Japanese(or Japanese to English)
translation.
If alphabet ratio is over 40%, select engine which is translating from English to Japanese.
Otherwise, from Japanese to English."
  (setq str (or str ""))
  (format
   "%s_%s"
   (text-translator-get-engine-type-or-site engine t)
   (if (> (/ (* (length (replace-regexp-in-string "[^A-Za-z]+" "" str)) 100)
             (length str))
          40)
       "enja" "jaen")))

(defun text-translator-translate-last-string ()
  "The function to translate in the translation site that
I choose with the character string that I translated in the last time."
  (interactive)
  (text-translator nil t))

(defun text-translator-client (engine str)
  "Function that throws out words and phrases that want to translate into
specified site, and receives translation result."
  (let* ((history-delete-duplicates t)
         (buf text-translator-work-buffer)
         (alist
          (cond
           ((not text-translator-do-fill-region)
            text-translator-pre-string-replace-alist)
           ;; for example, if engine is "excite.co.jp_enja",
           ;; this code returns "en".
           ((member (substring
                     (text-translator-get-engine-type-or-site engine) 0 2)
                    text-translator-space-division-languages)
            ;; replace "\n" to " ".
            (append '(("\n" . " ") ("\r" . ""))
                    text-translator-pre-string-replace-alist))
           (t
            ;; replace "\n" to "".
            (append '(("\n" . "") ("\r" . ""))
                    text-translator-pre-string-replace-alist))))
         (str (text-translator-replace-string str alist))
         (type (assoc engine text-translator-site-data-alist))
         (proc (open-network-stream "Web Connection" buf
                                    (or text-translator-proxy-server
                                        (nth 1 type))
                                    (or (and text-translator-proxy-server
                                             text-translator-proxy-port)
                                        80)))
         ;;(process-connection-type nil)
         (enc-str (text-translator-url-encode-string str (nth 4 type)))
         (post-str (format (nth 3 type) enc-str))
         (truncate-partial-width-windows nil))
    (add-to-history 'text-translator-engine-history engine)
    (setq text-translator-search-regexp-or-func (nth 5 type))
    (setq text-translator-last-string str)
    (with-current-buffer (get-buffer-create buf)
      (erase-buffer)
      (set-process-coding-system proc (nth 4 type) 'binary)
      (set-process-filter proc 'text-translator-client-filter)
      (process-send-string
       proc
       (concat
        "POST " "http://" (nth 1 type) (nth 2 type) "\r\n"
        (and text-translator-proxy-server
             text-translator-proxy-user
             text-translator-proxy-password
             (format "Proxy-Authorization: Basic %s \r\n"
                     (base64-encode-string
                      (concat text-translator-proxy-user ":"
                              text-translator-proxy-password))))
        "HOST: " (nth 1 type) "\r\n"
        "User-Agent: " text-translator-user-agent "\r\n"
;;        "Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5" "\r\n"
;;        "Accept-Language: ja,en-us;q=0.7,en;q=0.3" "\r\n"
        "Accept-Encoding: identity\r\n"
        "Accept-Charset: Shift_JIS,utf-8;q=0.7,*;q=0.7\r\n"
        "Keep-Alive: 300" "\r\n"
        "Connection: keep-alive" "\r\n"
        "Content-Type: application/x-www-form-urlencoded\r\n"
        "Content-Length: "
        (number-to-string (string-bytes post-str)) "\r\n"
        "\r\n"
        post-str "\r\n"
        "\r\n"))
      (message "Translating...")
      (save-selected-window
        (pop-to-buffer text-translator-buffer)
        (setq buffer-read-only nil)
        (erase-buffer)
        (text-translator-mode)
        (setq mode-line-buffer-identification
              `("%b [" ,(car text-translator-engine-history) "]"))))))

(defun text-translator-client-filter (proc str)
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (insert (format "%s" str))
    (set-marker (process-mark proc) (point))
    (setq str (text-translator-replace-string
               (or (cond
                    ((functionp text-translator-search-regexp-or-func)
                     (funcall text-translator-search-regexp-or-func))
                    ((re-search-backward
                      text-translator-search-regexp-or-func nil t)
                     (match-string 1)))
                   "")
               text-translator-post-string-replace-alist))
    (unless (string= "" str)
      (delete-process proc)
      (let ((window (get-buffer-window text-translator-buffer))
            (window-min-height
             (if (> text-translator-window-min-height (/ (frame-height) 2))
                 (/ (frame-height) 2)
               (1+ text-translator-window-min-height))))
        (set-buffer text-translator-buffer)
        (setq buffer-read-only nil)
        (when text-translator-leave-string
          (insert
           (concat
            (propertize "-----   Original  -----\n"
                        'face font-lock-keyword-face)
            text-translator-last-string
            "\n\n"
            (propertize "***** Translation *****\n"
                        'face font-lock-keyword-face))))
        (insert (concat str "\n"))
        (when text-translator-do-fill-region
          (goto-char (- (point) (/ (length str) 2)))
          (call-interactively 'fill-paragraph))
        (set-buffer-modified-p nil)
        ;; adjust window height
        (when (and text-translator-auto-window-adjust
                   (window-live-p window))
          (balance-windows)
          (shrink-window-if-larger-than-buffer window))
        (message "") ; prevent minibuffer from becoming two line.
        (ding)
        (message "Translating...done")))))

(defun text-translator-replace-string (str replace)
  "Function that converts character string specified for argument STR
according to rule REPLACE."
  (with-temp-buffer
    (insert str)
    ;; convert unusable string
    (format-replace-strings replace)
    (buffer-string)))

(defun text-translator-extract-tag-exclusion-string (regex &optional dont-convert-br)
  (when (re-search-backward regex nil t)
    ;; first: convert <br> tag to '\n' (when variable dont-convert-br is nil)
    ;; second: convert any another tags to empty string.
    (replace-regexp-in-string
     "<.*?>" "" (if dont-convert-br
                    (match-string 1)
                  (replace-regexp-in-string
                   "<[bB][rR]\\( /\\)?>" "\n" (match-string 1))))))

;;;; major-mode text-translator-mode

;; variables for major mode
(defvar text-translator-mode nil)
(defvar text-translator-mode-map nil)
(defvar text-translator-mode-pkey-map nil)
(defvar text-translator-mode-syntax-table nil)
(defvar text-translator-mode-abbrev-table nil)
(define-abbrev-table 'text-translator-mode-abbrev-table ())

;; keymap definition
(unless text-translator-mode-map
  (setq text-translator-mode-map (make-sparse-keymap))
  (define-prefix-command 'text-translator-mode-pkey-map)
  (let ((map text-translator-mode-pkey-map))
    (define-key map "\C-q" 'text-translator-quit)
    (define-key map "\C-a" 'text-translator-translate-recent-type)
    (define-key map "\C-l" 'text-translator-display-last-string)
    (define-key map "\C-d" 'text-translator-translate-default)
    (define-key map "\C-s" 'text-translator-toggle-leave-string)))

;; major-mode
(defun text-translator-mode ()
  "Major mode for text-translator."
  (kill-all-local-variables)
  (setq local-abbrev-table text-translator-mode-abbrev-table)
  (set-syntax-table text-translator-mode-syntax-table)
  (setq mode-name text-translator-mode-name)
  (setq major-mode 'text-translator-mode)
  (define-key text-translator-mode-map
    text-translator-prefix-key text-translator-mode-pkey-map)
  (use-local-map text-translator-mode-map)
  (run-hooks 'text-translator-mode-hook))

;; syntax-table
(unless text-translator-mode-syntax-table
  (setq text-translator-mode-syntax-table (make-syntax-table)))

;; functions for major-mode
(defun text-translator-quit ()
  "Function that closes buffer for text-translator.
If window only have *translated* buffer, change another buffer."
  (interactive)
  (bury-buffer)
  (unless (one-window-p)
    (delete-window)))

(defun text-translator-toggle-leave-string ()
  "Function that change value of `text-translator-leave-string'.
Toggle to display a translation result buffer of character
string that used last time."
  (interactive)
  (setq text-translator-leave-string (not text-translator-leave-string))
  (message "Pretranslational string switched%s to leave."
           (if text-translator-leave-string "" " not")))

(defun text-translator-display-last-string (arg)
  "Function that displays translated character string last time.
Default display to minibuffer.
With prefix-arg, insert buffer."
  (interactive "P")
  (if arg
      (insert text-translator-last-string)
    (message "%s" text-translator-last-string)))

(defun text-translator-translate-recent-type ()
  "Function that translates by type corresponding to the language
that used last time.
For example, last time, if you have used excite.co.jp_enja,
this time select from **_enja, and, translates."
  (interactive)
  (let* ((minibuffer-history text-translator-engine-history)
         (engine (car text-translator-engine-history))
         (last-type
          (concat "_" (text-translator-get-engine-type-or-site engine)))
         (type (completing-read
                (format "Select translation engine (last %s): " engine)
                (delq nil
                      (mapcar
                       (lambda (x)
                         (when (string-match last-type (car x))
                           x))
                       text-translator-site-data-alist))
                nil t)))
    (unless (string= "" type)
      (text-translator-client type text-translator-last-string))))

(defun text-translator-translate-default ()
  "Function that translates by default type only.
Default is value of `text-translator-default-engine'."
  (interactive)
  (text-translator nil nil text-translator-default-engine))

(defun text-translator-check-valid-translation-engine (engine valid-engine)
  "Check ENGINE that is registered in `text-translator-site-data-alist'.
Return ENGINE if it is already registered, otherwise return VALID-ENGINE."
  (or (car (member engine (mapcar 'car text-translator-site-data-alist)))
      valid-engine))

(defun text-translator-get-engine-type-or-site (engine &optional get-site)
  "Get a translation engine type or site name.
If optional argument GET-SITE is nil, return a translation engine type.
Otherwise return a translation site name."
  (nth (if get-site 0 1) (split-string engine "_")))

;; by google2.el
(defun text-translator-url-encode-string (str &optional coding)
  (apply (function concat)
         (mapcar
          (lambda (ch)
            (cond
             ((eq ch ?\n)               ; newline
              "%0D%0A")
             ((string-match "[-a-zA-Z0-9_:/]" (char-to-string ch)) ; xxx?
              (char-to-string ch))      ; printable
             ((char-equal ch ?\x20)     ; space
              "+")
             (t
              (format "%%%02X" ch))))   ; escape
          ;; Coerce a string to a list of chars.
          (append (encode-coding-string (or str "") (or coding 'iso-2022-jp))
                  nil))))

(provide 'text-translator)
;;; text-translator.el ends here

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:
