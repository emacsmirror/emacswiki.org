;;; w3m-extension.el --- Emacs-W3m extensions

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-06-07 22:06:58
;; Version: 0.2.5
;; Last-Updated: 2008-09-24 15:53:19
;; URL: not distributed yet
;; Keywords: Emacs-W3m, W3m, Google, search
;; Compatibility: GNU Emacs 23.0.60.1

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

;; Features that might be required by this library:
;;
;; `w3m-util' `w3m-proc' `w3m-form' `w3m-lnum' `w3m'
;; `basic-edit-toolkit'
;;

;;; Installation:
;;
;; Copy w3m-extension.el to your load-path and add to your ~/.emacs
;;
;;  (require 'w3m-extension)
;;
;; No more need

;;; Commentary:
;;
;; A collect functions that extension for Emacs-W3m.
;;

;;; Change log:
;;
;;      2008/09/24
;;              Remove function `w3m-search+' and replace with `w3m-search-advance'
;;
;;      2008/09/22
;;              Add functions: `w3m-goto-linknum' `w3m-gmail-toggle-mark'
;;              `w3m-gmail-mark-all' `w3m-gmail-unmark-all'.
;;
;;      2008/08/30
;;              Add function `w3m-search+', and modified some old search function
;;              to make these base on `w3m-search+'.
;;
;;      2008/08/28
;;              Add two function `w3m-search-slang' and `w3m-search-slang+'.
;;
;;      2008/07/20
;;              Modified some function about Google search.
;;
;;              Make all search function open new search result page in background session.
;;
;;              Extension function `w3m-search-dict-cn+' and `w3m-search-google-web+' to make
;;              them can search current mark sentence.
;;
;;      2008/06/18
;;              Replace some `w3m-goto-url-new-session' with `w3m-view-this-url-1'
;;              to make pages is open background and don't wink screen case by 'w3m-goto-url-new-session.
;;
;;              Add some function that make can jump between Google search titles.
;;
;;      2008/06/17
;;              Add function that return the random pages from EmacsWiki.
;;
;;      2008/06/07
;;              Create this collect of search function. ^_^
;;

;;; Acknowledgments:
;;
;; Emacs guys.
;;

;;; TODO
;;
;; None
;;

;;; Require
(require 'w3m-util)
(require 'w3m-proc)
(require 'w3m-form)
(require 'w3m-lnum)
(require 'w3m)
(require 'basic-edit-toolkit)

;;; Code:

(defvar google-desktop-search-url nil
  "The uniqure string per computer that Google Desktop Search need, can copy from address bar of web browser.")

(defvar w3m-search-advance-prettyfy-string-length 25
  "The length of `SEARCH-OBJECT' show in function `w3m-search-advance'.")

(defvar w3m-search-advance-search-object nil
  "The search object cache that `w3m-search-advance' use.")

(defvar w3m-gmail-login-string ""
  "The string for gmail login.")

(defun w3m-search-advance (search-url prompt-string
                                      &optional coding
                                      prefix-input-string postfix-input-string
                                      search-url-follow search-url-last
                                      foreground
                                      upcase-p downcase-p capitalize-p)
  "Advance w3m search function.
Default, if mark active, will set `SEARCH-OBJECT' with current mark region,
otherwise, set current word to `SEARCH-OBJECT'.

Set `SEARCH-URL' for special search.
Set `PROMPT-STRING' to prompt to user.
If `CODING' is set, encode `SEARCH-OBJECT' with this coding, default is nil.
`PREFIX-INPUT-STRING' is for add before `SEARCH-OBJECT'
`POSTFIX-INPUT-STRING' is for append after `SEARCH-OBJECT'
`SEARCH-URL-FOLLOW' is a url that follow `SEARCH-URL' for decorate
`SEARCH-URL-LAST' is a url that at last for decorate `SEARCH-URL'.
If `FOREGROUND' is non-nil, make search page open foreground, otherwise search in background.
If `UPCASE-P' is non-nil, upcase `SEARCH-OBJECT'.
If `downcase-p' is non-nil, downcase `SEARCH-OBJECT'.
If `capitalize-p' is non-nil, capitalize `SEARCH-OBJECT'."
  (let (search-string
        input-string)
    (setq search-string
          (if mark-active
              ;; when mark active get mark region
              (prog1
                  (buffer-substring (region-beginning) (region-end))
                (deactivate-mark))
            ;; or get current word
            (current-word)))
    ;; kill `search-string' for make user can edit it convenient
    ;; If you want to edit `search-string' by default when input in `read-string'
    ;; just use `w3m-search-advance-insert-search-object' yank and edit. :)
    (if search-string
        (setq w3m-search-advance-search-object search-string)
      (setq search-string ""))
    (setq input-string (read-string (concat prompt-string
                                            (format " (%-s): " (prettyfy-string
                                                                search-string w3m-search-advance-prettyfy-string-length)))))
    ;; set `input-string' with `search-string' if user input nothing
    (if (equal input-string "")
        (setq input-string search-string))
    ;; `input-string' transform.
    (setq input-string
          (cond (upcase-p
                 (upcase input-string))
                (downcase-p
                 (downcase input-string))
                (capitalize-p
                 (capitalize input-string))
                (t input-string)))
    ;; encode `input-string' with `coding'
    (or prefix-input-string (setq prefix-input-string ""))
    (or postfix-input-string (setq postfix-input-string ""))
    (or search-url-follow (setq search-url-follow ""))
    (or search-url-last (setq search-url-last ""))
    (setq input-string (w3m-url-encode-string (concat prefix-input-string input-string postfix-input-string) coding))
    (setq search-url (concat search-url search-url-follow input-string search-url-last))
    (if foreground
        (w3m-browse-url search-url t)         ;search foreground
      (w3m-view-this-url-1 search-url nil t)) ;search background
    ))

(defun w3m-search-advance-insert-search-object ()
  "Insert search object of `w3m-search-advance' into minibuffer."
  (interactive)
  (if w3m-search-advance-search-object
      (insert w3m-search-advance-search-object)))

(defun w3m-search-slang ()
  "Translate input word and search from urbandictionary.com."
  (interactive)
  (w3m-search-advance "http://www.urbandictionary.com/define.php?term=" "English Slang" 'utf-8))

(defun w3m-search-dict-cn ()
  "Translate input word and search from dict.cn."
  (interactive)
  (w3m-search-advance "http://dict.cn/search/?q=" "English Dict.cn" 'gbk))

(defun w3m-search-google-code ()
  "Use Google Code search for WHAT."
  (interactive)
  (w3m-search-advance "http://www.google.com/codesearch?hl=zh-CN&lr=&q=" "Google Code" 'utf-8))

(defun w3m-search-google-lucky ()
  "Use Google Lucky search for WHAT."
  (interactive)
  (w3m-search-advance "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&q=" "Google Lucky" 'utf-8))

(defun w3m-search-google-image ()
  "Use Google image search for WHAT."
  (interactive)
  (w3m-search-advance "http://images.google.com/images?sa=N&tab=wi&q=" "Google Image" 'utf-8))

(defun w3m-search-google-blog-cn ()
  "Use Google (Chinese) blog search for WHAT."
  (interactive)
  (w3m-search-advance "http://blogsearch.google.com/blogsearch?hl=zh-CN&ie=UTF-8&oe=UTF-8&q=" "Google Blog CN" 'utf-8))

(defun w3m-search-google-blog-en ()
  "Use Google (English) blog search for WHAT."
  (interactive)
  (w3m-search-advance "http://blogsearch.google.com/blogsearch?hl=en&ie=UTF-8&oe=UTF-8&q=" "Google Blog EN" 'utf-8))

(defun w3m-search-google-group ()
  "Use Google group search for WHAT."
  (interactive)
  (w3m-search-advance "http://groups.google.com/groups?hl=zh-CN&ie=UTF-8&oe=UTF-8&q=" "Google Group" 'utf-8))

(defun w3m-search-google-file ()
  "Use Google to search for a file named FILE.
This function add little Google search syntax, make search file simply.
Example, your want search pdf of chm about Emacs, you just type emacs pdf|chm."
  (interactive)
  (w3m-search-advance "http://www.google.com/search?&ie=UTF-8&oe=UTF-8&q=" "Google File" 'utf-8
                      "+intitle:(\"index of\"\|\"last modified\"\|\"parent of\") -inurl:htm -inurl:html -inurl:php -inurl:asp "))

(defun w3m-search-baidu-mp3 ()
  "Search mp3 from mp3.baidu.com."
  (interactive)
  (w3m-search-advance "http://mp3.baidu.com/m?f=ms&tn=baidump3&ct=134217728&lf=&rn=&lm=0&word=" "Baidu Mp3 Search" 'gbk))

(defun w3m-search-emacswiki ()
  "Search from EmacsWiki's Google Custom Search."
  (interactive)
  (w3m-search-advance "http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&q=" "Emacswiki" 'utf-8))

(defun w3m-search-emacswiki-random ()
  "Get the random pages from emacswiki."
  (interactive)
  (w3m-view-this-url-1 "http://www.emacswiki.org/cgi-bin/wiki?action=random" nil t))

(defun w3m-search-haskell-wiki ()
  "Search from HaskellWiki's Google Custom Search."
  (interactive)
  (w3m-search-advance "http://www.google.com/cse?cx=014102838545582129901%3Anhonl7a8bw8&q=" "Haskell Wiki" 'utf-8))

(defun w3m-search-rfc-number ()
  "Search RFC number from www.ietf.org."
  (interactive)
  (w3m-search-advance "http://www.ietf.org/rfc/rfc" "RFC Number" 'utf-8 nil ".txt"))

(defun w3m-search-lispdoc-basic ()
  "Search object in lispdoc.com with `basic-search'."
  (interactive)
  (w3m-search-advance "http://lispdoc.com?q=" "Lispdoc basic search" nil nil nil nil "&search=Basic+search/"))

(defun w3m-search-lispdoc-full ()
  "Search object in lispdoc.com with `full-search'."
  (interactive)
  (w3m-search-advance "http://lispdoc.com?q=" "Lispdoc basic search" nil nil nil nil "&search=Full+search/"))

(defun w3m-search-google-web-cn ()
  "Search object in www.google.cn."
  (interactive)
  (w3m-search-advance "http://www.google.cn/search?&hl=zh-CN&lr=lang_zh-CN%7Clang_zh-TW&inlang=zh-CN&q=" "Google Web CN" 'utf-8))

(defun w3m-search-google-web-en ()
  "Use Google (English) to search for WHAT."
  (interactive)
  (w3m-search-advance "http://www.google.com/search?&ie=UTF-8&oe=UTF-8&q=" "Google Web EN" 'utf-8))

(defun w3m-search-answers ()
  "Search object in www.answers.com."
  (interactive)
  (w3m-search-advance "http://www.answers.com/" "answers.com" 'utf-8))

(defun w3m-search-haskell-hoogle ()
  "Search object in haskell.org/hoogle/."
  (interactive)
  (w3m-search-advance "http://haskell.org/hoogle/?hoogle=" "Haskell Hoogle" 'utf-8))

(defun w3m-search-wikipedia-cn ()
  "Search object in zh.wikipedia.org."
  (interactive)
  (w3m-search-advance "http://zh.wikipedia.org/wiki/" "zh.wikipedia.org" 'utf-8))

(defun w3m-search-wikipedia-en ()
  "Search object in en.wikipedia.org."
  (interactive)
  (w3m-search-advance "http://en.wikipedia.org/wiki/" "en.wikipedia.org" 'utf-8))

(defun w3m-search-google-news-cn-Sci/Tech ()
  "Look up Google technology news."
  (interactive)
  (w3m-view-this-url-1 "http://news.google.cn/nwshp?tab=wn&ned=tcn&topic=t" nil t))

(defun w3m-search-google-news-en-Sci/Tech ()
  "Use Google news search for WHAT."
  (interactive)
  (w3m-view-this-url-1 "http://news.google.com/news?ned=tus&topic=t" nil t))

(defun w3m-download-with-wget-current-position()
  "Download current linked of W3m use Wget."
  (interactive)
  (if (and (require 'wget nil t)
           (require 'lazycat-toolkit nil t)
           (or (w3m-anchor)
               (w3m-image)))
      (progn
        (wget (or (w3m-anchor) (w3m-image)))
        (if wget-hide-status
            (wget-hide)))
    (message "Nothing at current point.")))

(defun w3m-search-google-desktop ()
  "Use Google Desktop search for WHAT.
The search url of Google Desktop Search is random create when first run.
So if you want to make this function works, you need replace the search url in yours browser address bar"
  (interactive)
  (w3m-view-this-url-1 google-desktop-search-url nil t))

(defun w3m-auto-logon-gmail ()
  "Auto logon gmail.
This url is bind with personal account, you just replace it with url that you have login in Gmail."
  (interactive)
  (w3m-view-this-url-1 w3m-gmail-login-string nil t))

(defun w3m-auto-install-elisp ()
  "Automatic download and install elisp."
  (interactive)
  (if (require 'auto-install nil t)
      (if (eq major-mode 'w3m-mode)
          (save-excursion
            (goto-char (point-min))
            (if (search-forward-regexp "^Download")
                (progn
                  (deactivate-mark)
                  (auto-install-download (w3m-anchor)))
              (message "Haven't found download anchor")))
        (message "Current mode is not `w3m-mode'."))))

(defun toggle-w3m-with-other-buffer ()
  "Switch to a w3m buffer or return to the previous buffer."
  (interactive)
  (if (derived-mode-p 'w3m-mode)
      ;; Currently in a w3m buffer
      ;; Bury buffers until you reach a non-w3m one
      (while (derived-mode-p 'w3m-mode)
        (bury-buffer))
    ;; Not in w3m
    ;; Find the first w3m buffer
    (let ((list (buffer-list)))
      (while list
        (if (with-current-buffer (car list)
              (derived-mode-p 'w3m-mode))
            (progn
              (switch-to-buffer (car list))
              (setq list nil))
          (setq list (cdr list))))
      (unless (derived-mode-p 'w3m-mode)
        (call-interactively 'w3m)))))

(defun w3m-open-rcirc-window ()
  "Open rcirc window in w3m."
  (interactive)
  (when (require 'rcirc-notify+ nil t)
    (split-window-vertically 10)
    (rcirc-notify+-jump-last-message-channel)
    (windmove-down)))

(defun w3m-startup-background ()
  "Startup w3m background."
  (interactive)
  (w3m-view-this-url-1 (w3m-input-url nil nil nil w3m-quick-start
                                      'feeling-lucky) nil t))

(defun w3m-google-desktop-url-open ()
  "Open file link that Google Desktop Search show."
  (interactive)
  (let ((file (w3m-print-this-url))
        (url (w3m-print-current-url))
        (google-search-url google-desktop-search-url)) ;google-search-url is unique string that generate by Google Desktop Search
    (string-match "/\\?.*" google-search-url)
    (setq google-search-url (replace-match "" nil nil google-search-url 0))
    (if (string-match google-search-url url) ;if is the result of Google Desktop Search
        (progn
          (string-match ".*&url=file://" file) ;cut front of file
          (setq file (replace-match "" nil nil file 0))
          (string-match "&s.*" file)                    ;cut behind of file
          (setq file (replace-match "" nil nil file 0)) ;get local file path
          (find-file file))             ;open file is my function for open diversified files
      (message "This not a valid Google Desktop Search result."))))

(defun w3m-delete-buffer-and-select-right ()
  "Delete current w3m buffer.
If current tab is at right side of tabs, select left tab, otherwise, select right tab."
  (interactive)
  (if (require 'tabbar nil t)
      (let* ((tabset (tabbar-current-tabset t))
             selected tab)
        (when tabset
          (setq selected (tabbar-selected-tab tabset))
          (setq tab (tabbar-tab-next tabset selected))
          (w3m-delete-buffer)
          (if tab                       ;if tab is not right side of tabs
              (tabbar-forward-tab))))))

(defun w3m-visual-scroll-up (&optional arg)
  "Visual scroll up with image and text."
  (interactive)
  (or arg (setq arg 1))
  (if (pos-visible-in-window-p (point-max))
      (message "End of buffer")
    (let ((cur (point))
          pos visible)
      (setq pos
            (save-excursion
              (while (and (search-forward "\n" nil t)
                          (= (length (pos-visible-in-window-p (point) nil t)) 2)))
              (1- (point))))
      (setq visible
            (pos-visible-in-window-p pos nil t))
      ;; if point is fully visible, we can go there
      (when (and (= (length visible) 2)
                 (not (= pos cur)))
        (goto-char pos))
      ;; if point is partly visible, we only go there if we absolutely
      ;; have to (point is already at the top)
      (when (and (= pos cur)
                 (null (pos-visible-in-window-p (1- (point)))))
        (forward-line 1))
      (set-window-vscroll nil (+ (window-vscroll) arg)))))

(defun w3m-visual-scroll-down (&optional arg)
  "Visual scroll down with image and text."
  (interactive)
  (or arg (setq arg 1))
  (if (pos-visible-in-window-p (point-min))
      (message "Beginning of buffer")
    (let ((cur (point))
          pos visible)
      (setq pos
            (save-excursion
              (while (and (search-backward "\n" nil t)
                          (= (length (pos-visible-in-window-p (point) nil t)) 2)))
              (+ 1 (point))))
      (setq visible
            (pos-visible-in-window-p pos nil t))
      (when (and (= (length visible) 2)
                 (not (= pos cur)))
        (goto-char pos))
      (when (and (= pos cur)
                 (null (pos-visible-in-window-p
                        (save-excursion (forward-line 1) (point)))))
        (goto-char (1- (point))))
      (when (zerop (window-vscroll))
        (message "vscroll is 0. Reverting to scroll-down.")
        (scroll-down arg))
      (set-window-vscroll nil (- (window-vscroll) arg)))))

(defun w3m-goto-linknum ()
  "Turn on link numbers and ask for one to go to."
  (interactive)
  (let ((active w3m-link-numbering-mode)
        action
        number)
    (when (not active) (w3m-link-numbering-mode))
    (unwind-protect
        (w3m-move-numbered-anchor (read-number "Anchor number: "))
      (when (not active) (w3m-link-numbering-mode)))))

(defun w3m-gmail-toggle-mark ()
  "Toggle form mark in Gmail web page."
  (interactive)
  (goto-char (point-min))
  (when (search-forward-regexp "\\[\\(\\*\\| \\)\\]" nil t)
    (backward-char 4)
    (w3m-form-goto-next-field)
    (while (or
            (looking-at "[*]")
            (looking-at "[ ]"))
      (w3m-view-this-url)
      (w3m-form-goto-next-field))))

(defun w3m-gmail-unmark-all ()
  "Unmark all form in Gmail web page."
  (interactive)
  (w3m-gmail-mark-all t))

(defun w3m-gmail-mark-all (unmark)
  "Mark all form in Gmail web page."
  (interactive "P")
  (goto-char (point-min))
  (when (search-forward (if unmark "[*]" "[ ]") nil t)
    (backward-char 4)
    (w3m-form-goto-next-field)
    (while (looking-at (if unmark "[*]" "[ ]"))
      (w3m-view-this-url)
      (w3m-form-goto-next-field))))

(defun w3m-open-dead-link-with-external-browser ()
  "Automatic open dead link."
  (interactive)
  (call-interactively 'w3m-process-stop)
  (if (search-forward-regexp "Reading " nil t)
      (browse-url-firefox (thing-at-point 'url))))

(defun w3m-emacswiki-view-diff ()
  "View different of current emacswiki page."
  (interactive)
  (w3m-emacswiki-view-regexp
   "^\\(Last edited\\|Edited\\) [0-9]\\{4\\}\\(-[0-9]\\{2\\}\\)\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\} UTC by .*(diff)$"
   "different"))

(defun w3m-emacswiki-view-other-version ()
  "View other version of current emacswiki page."
  (interactive)
  (w3m-emacswiki-view-regexp
   "^Edit this page View other revisions"
   "other version"))

(defun w3m-emacswiki-view-regexp (regexp echo-string)
  "View regexp link in emacswiki.org"
  (let ((remember-pos (point)))
    (w3m-redisplay-this-page)
    (goto-char (point-min))
    (if (search-forward-regexp
         regexp
         nil t)
        (progn
          (backward-char)
          (w3m-view-this-url t)         ;forces reload
          (message (format "Read %s with current wiki page." echo-string)))
      (goto-char remember-pos)
      (message (format "Don't %s in current wiki page." echo-string)))))

(defun w3m-emacswiki-recent-changes ()
  "View recent changes of EmacsWiki.org."
  (interactive)
  (w3m-goto-url-new-session "http://www.emacswiki.org/cgi-bin/wiki/RecentChanges" t))

(defun w3m-copy-link-in-region ()
  "Copy all link in yank at region with buffer."
  (interactive)
  (let* ((regionp (org-region-active-p))
         (transform-start (point-min))
         (transform-end (point-max))
         out-bound)
    (when regionp
      (setq transform-start (region-beginning))
      (setq transform-end (region-end))
      ;; Deactivate mark if current mark is activate.
      (if (fboundp 'deactivate-mark) (deactivate-mark)))
    (message "Copy links...")
    (save-excursion
      (goto-char transform-start)
      (while (and (not out-bound)             ; still inside region to copy
                  (not (w3m-no-next-link-p))) ; no next link current buffer
        ;; move to next anchor.
        (w3m-get-next-link-start)
        (if (<= (point) transform-end)  ; if point is inside transform bound
            ;; get link location at current point.
            (if (w3m-anchor (point))
                (kill-new (w3m-anchor (point))))
          (setq out-bound t)            ; for break out `while' loop
          ))
      (message "Copy links...done."))))

(defun w3m-get-anchor-start ()
  "Move cursor to the start of current anchor. Return point."
  (interactive)
  (goto-char (or (previous-single-property-change (point) 'w3m-anchor-sequence) ;get start position of anchor
                 (point))))

(defun w3m-get-anchor-end ()
  "Move cursor to the end of current anchor. Return point."
  (interactive)
  (goto-char (or (next-single-property-change (point) 'w3m-anchor-sequence) ;get end position of anchor
                 (point))))

(defun w3m-get-next-link-start ()
  "Move cursor to the start of next link. Return point."
  (interactive)
  (catch 'reach
    (while (next-single-property-change (point) 'w3m-anchor-sequence) ;jump to next anchor
      (goto-char (next-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun w3m-get-prev-link-start ()
  "Move cursor to the start of previous link. Return point."
  (interactive)
  (catch 'reach
    (while (previous-single-property-change (point) 'w3m-anchor-sequence) ;jump to previous anchor
      (goto-char (previous-single-property-change (point) 'w3m-anchor-sequence))
      (when (w3m-anchor (point))        ;return point when current is valid link
        (throw 'reach nil))))
  (point))

(defun w3m-no-next-link-p ()
  "Whether there is no next link after the cursor.
Return t if there is no next link; otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-next-link-start))))

(defun w3m-no-prev-link-p ()
  "Whether there is no previous link after the cursor.
Return t if there is no previous link; otherwise, return nil."
  (save-excursion
    (equal (point) (w3m-get-prev-link-start))))

(provide 'w3m-extension)

;;; LocalWords:  lnum utilties linknum unmark uniqure prettyfy logon login bw
;;; LocalWords:  postfix urbandictionary intitle inurl HaskellWiki's Anhonl txt
;;; LocalWords:  zh TW inlang tabset pos vscroll zerop UTC
