;;; sdcv.el --- Interface for sdcv (StartDict console version).

;; Filename: sdcv.el
;; Description: Interface for sdcv (StartDict console version).
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-05 22:04:02
;; Version: 2.9
;; Last-Updated: 2019-02-20 09:14:01
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/sdcv.el
;; Keywords: startdict, sdcv
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `posframe' `outline' `cl'
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
;; Interface for sdcv (StartDict console version).
;;
;; Translate word by sdcv (console version of Stardict), and display
;; translation use posframe or buffer.
;;
;; Below are commands you can use:
;;
;; `sdcv-search-pointer'
;; Search around word and display with buffer.
;; `sdcv-search-pointer+'
;; Search around word and display with `posframe'.
;; `sdcv-search-input'
;; Search input word and display with buffer.
;; `sdcv-search-input+'
;; Search input word and display with `posframe'.
;;
;; Tips:
;;
;; If current mark is active, sdcv commands will translate
;; region string, otherwise translate word around point.
;;

;;; Installation:
;;
;; To use this extension, you have to install Stardict and sdcv
;; If you use Debian, it's simply, just:
;;
;;      sudo aptitude install stardict sdcv -y
;;
;; And make sure have install `posframe.el',
;; this extension depend it.
;; You can install get it from:
;; https://raw.githubusercontent.com/tumashu/posframe/master/posframe.el
;;
;; Put sdcv.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'sdcv)
;;
;; And then you need set two options.
;;
;;  sdcv-dictionary-simple-list         (a simple dictionary list for posframe display)
;;  sdcv-dictionary-complete-list       (a complete dictionary list for buffer display)
;;
;; Example, setup like this:
;;
;; (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
;;       '(
;;         "懒虫简明英汉词典"
;;         "懒虫简明汉英词典"
;;         "KDic11万英汉词典"
;;         ))
;; (setq sdcv-dictionary-complete-list      ;; a complete dictionary list
;;       '("KDic11万英汉词典"
;;         "懒虫简明英汉词典"
;;         "朗道英汉字典5.0"
;;         "XDICT英汉辞典"
;;         "朗道汉英字典5.0"
;;         "XDICT汉英辞典"
;;         "懒虫简明汉英词典"
;;         "牛津英汉双解美化版"
;;         "stardict1.3英汉辞典"
;;         "英汉汉英专业词典"
;;         "CDICT5英汉辞典"
;;         "Jargon"
;;         "FOLDOC"
;;         "WordNet"
;;         ))
;; (setq sdcv-dictionary-data-dir "your_sdcv_dict_dir")   ;; set local sdcv dict to search word
;;

;;; Customize:
;;
;; `sdcv-buffer-name'
;; The name of sdcv buffer.
;;
;; `sdcv-dictionary-simple-list'
;; The dictionary list for simple describe.
;;
;; `sdcv-dictionary-complete-list'
;; The dictionary list for complete describe.
;;
;; `sdcv-dictionary-data-dir'
;; The directory to store stardict dictionaries.
;;
;; `sdcv-tooltip-face'
;; The foreground/background colors of sdcv tooltip.
;;
;; All of the above can customize by:
;;      M-x customize-group RET sdcv RET
;;

;;; Change log:
;;
;; 2019/02/20
;;      * Try pick word from camelcase string and translate again if no translate result for current string.
;;
;; 2018/12/09
;;      * Add command `sdcv-check' to help check invalid dictionaries.
;;
;; 2018/09/10
;;      * Add option `sdcv-say-word-p', just support OSX now, please send me PR if you want to support Linux. ;)
;;      * Make `sdcv-say-word' can work with `sdcv-search-pointer'.
;;      * Make `sdcv-say-word' support all platform.
;;      * Don't need `osx-lib' anymore.
;;
;; 2018/07/16
;;      * Fixed typo that uncomment setenv code.
;;
;; 2018/07/08
;;      * Add new option `sdcv-tooltip-border-width'.
;;
;; 2018/07/05
;;      * Use `posframe' for MacOS, bug has fixed at: https://www.emacswiki.org/emacs/init-startup.el
;;
;; 2018/07/01
;;      * Add support for MacOS, use `popup-tip'.
;;
;; 2018/06/23
;;      * Set LANG environment variable, make sure `shell-command-to-string' can handle CJK character correctly.
;;
;; 2018/06/20
;;      * Add `sdcv-dictionary-data-dir'
;;      * Use `posframe' instead `showtip' for better user experience.
;;      * Add new face `sdcv-tooltip-face' for customize.
;;      * Automatically hide sdcv tooltip once user move cursor of scroll window.
;;      * Make sure sdcv tooltip buffer kill after frame deleted.
;;      * Improve function `sdcv-hide-tooltip-after-move' performance.
;;      * Show tooltip above minibuffer if word is input from user.
;;
;; 2009/04/04
;;      * Fix the bug of `sdcv-search-pointer'.
;;      * Fix doc.
;;        Thanks "Santiago Mejia" report those problems.
;;
;; 2009/04/02
;;      * Remove unnecessary information from transform result.
;;
;; 2009/03/04
;;      * Refactory code.
;;      * Search region or word around point.
;;      * Fix doc.
;;
;; 2009/02/05
;;      * Fix doc.
;;
;; 2008/06/01
;;      * First released.
;;

;;; Acknowledgements:
;;
;;      pluskid@gmail.com   (Zhang ChiYuan)     for sdcv-mode.el
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'outline)
(eval-when-compile
  (require 'cl))
(require 'posframe)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup sdcv nil
  "Interface for sdcv (StartDict console version)."
  :group 'edit)

(defcustom sdcv-buffer-name "*SDCV*"
  "The name of the buffer of sdcv."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-tooltip-name "*sdcv*"
  "The name of sdcv tooltip name."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-program (if (string-equal system-type "darwin") "/usr/local/bin/sdcv" "sdcv")
  "The path of sdcv."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-tooltip-timeout 5
  "The timeout of sdcv tooltip show time, in seconds."
  :type 'integer
  :group 'sdcv)

(defcustom sdcv-dictionary-complete-list nil
  "The complete dictionary list for translate."
  :type 'list
  :group 'sdcv)

(defcustom sdcv-dictionary-simple-list nil
  "The simply dictionary list for translate."
  :type 'list
  :group 'sdcv)

(defcustom sdcv-dictionary-data-dir nil
  "Default, sdcv search word from /usr/share/startdict/dict/.
You can customize this value with local dir,
then you don't need copy dict data to /usr/share directory everytime when you finish system install."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-tooltip-border-width 10
  "The border width of sdcv tooltip, default is 10 px."
  :type 'integer
  :group 'sdcv)

(defcustom sdcv-say-word-p nil
  "Say word after search word if this option is non-nil.
Default is nil.

Voice will use system feature if you use OSX.
Voice will fetch from youdao.com if you use other system."
  :type 'integer
  :group 'sdcv)

(defface sdcv-tooltip-face
  '((t (:foreground "green" :background "gray12")))
  "Face for sdcv tooltip"
  :group 'sdcv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sdcv-previous-window-configuration nil
  "Window configuration before switching to sdcv buffer.")

(defvar sdcv-current-translate-object nil
  "The search object.")

(defvar sdcv-filter-string "^对不起，没有发现和.*\n"
  "The filter string that sdcv output.")

(defvar sdcv-fail-notify-string "没有发现解释也... \n用更多的词典查询一下吧! ^_^"
  "This string is for notify user when search fail.")

(defvar sdcv-tooltip-last-point 0
  "Hold last point when show tooltip, use for hide tooltip after move point.")

(defvar sdcv-tooltip-last-scroll-offset 0
  "Hold last scroll offset when show tooltip, use for hide tooltip after window scroll.")

(defvar sdcv-mode-font-lock-keywords    ;keyword for buffer display
  '(
    ;; Dictionary name
    ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
    ;; Search word
    ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
    ;; Serial number
    ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
    ;; Type name
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; Phonetic symbol
    ("^\\/\\([^>]*\\)\\/$" . (1 font-lock-string-face))
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
    )
  "Expressions to highlight in `sdcv-mode'.")

(defvar sdcv-mode-map                   ;key map
  (let ((map (make-sparse-keymap)))
    ;; Sdcv command.
    (define-key map "q" 'sdcv-quit)
    (define-key map "j" 'sdcv-next-line)
    (define-key map "k" 'sdcv-prev-line)
    (define-key map "J" 'sdcv-scroll-up-one-line)
    (define-key map "K" 'sdcv-scroll-down-one-line)
    (define-key map "d" 'sdcv-next-dictionary)
    (define-key map "f" 'sdcv-previous-dictionary)
    (define-key map "i" 'sdcv-search-input)
    (define-key map ";" 'sdcv-search-input+)
    (define-key map "p" 'sdcv-search-pointer)
    (define-key map "y" 'sdcv-search-pointer+)
    ;; Isearch.
    (define-key map "S" 'isearch-forward-regexp)
    (define-key map "R" 'isearch-backward-regexp)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    ;; Hideshow.
    (define-key map "a" 'show-all)
    (define-key map "A" 'hide-body)
    (define-key map "v" 'show-entry)
    (define-key map "V" 'hide-entry)
    ;; Misc.
    (define-key map "e" 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map "l" 'forward-char)
    (define-key map "h" 'backward-char)
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for `sdcv-mode'.")

(define-derived-mode sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq font-lock-defaults '(sdcv-mode-font-lock-keywords))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sdcv-search-pointer (&optional word)
  "Get current word.
And display complete translations in other buffer."
  (interactive)
  ;; Display details translate result.
  (sdcv-search-detail (or word (sdcv-region-or-word))))

(defun sdcv-search-pointer+ ()
  "Translate current point word.
And show information use tooltip.
But this function use a simple dictionary list."
  (interactive)
  ;; Display simple translate result.
  (sdcv-search-simple))

(defun sdcv-search-input (&optional word)
  "Translate current input WORD.
And show information in other buffer."
  (interactive)
  ;; Display details translate result.
  (sdcv-search-detail (or word (sdcv-prompt-input))))

(defun sdcv-search-input+ (&optional word)
  "Translate current point WORD.
And show information use tooltip."
  (interactive)
  ;; Display simple translate result.
  (sdcv-search-simple (or word (sdcv-prompt-input))))

(defun sdcv-quit ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-configuration)
      (progn
        (set-window-configuration sdcv-previous-window-configuration)
        (setq sdcv-previous-window-configuration nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-next-dictionary ()
  "Jump to next dictionary."
  (interactive)
  (show-all)
  (if (search-forward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (call-interactively 'previous-line)
        (recenter 0))
    (message "Reached last dictionary.")))

(defun sdcv-previous-dictionary ()
  "Jump to previous dictionary."
  (interactive)
  (show-all)
  (if (search-backward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (forward-char 1)
        (recenter 0))                   ;adjust position
    (message "Reached first dictionary.")))

(defun sdcv-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun sdcv-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun sdcv-next-line (arg)
  "Next ARG line and show item."
  (interactive "P")
  (ignore-errors
    (call-interactively 'next-line arg)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defun sdcv-prev-line (arg)
  "Previous ARG line."
  (interactive "P")
  (ignore-errors
    (call-interactively 'previous-line arg)))

(defun sdcv-check ()
  "This function mainly detects the StarDict dictionary that does not exist,
and eliminates the problem that cannot be translated."
  (interactive)
  (let* ((dict-name-infos
          (cdr (split-string
                (string-trim
                 (shell-command-to-string
                  (format "LANG=en_US.UTF-8 %s --list-dicts --data-dir=%s" sdcv-program sdcv-dictionary-data-dir)))
                "\n")))
         (dict-names (mapcar (lambda (dict) (car (split-string dict " "))) dict-name-infos))
         (have-invalid-dict nil))
    (if sdcv-dictionary-simple-list
        (dolist (dict sdcv-dictionary-simple-list)
          (unless (member dict dict-names)
            (setq have-invalid-dict t)
            (message
             "sdcv-dictionary-simple-list: dictionary '%s' does not exist, remove it from sdcv-dictionary-simple-list or download the corresponding dictionary file to %s"
             dict
             sdcv-dictionary-data-dir)))
      (setq have-invalid-dict t)
      (message "sdcv-dictionary-simple-list is empty, command sdcv-search-simple won't work as expected."))
    (if sdcv-dictionary-complete-list
        (dolist (dict sdcv-dictionary-complete-list)
          (unless (member dict dict-names)
            (setq have-invalid-dict t)
            (message
             "sdcv-dictionary-complete-list: dictionary '%s' does not exist, remove it from sdcv-dictionary-complete-list or download the corresponding dictionary file to %s"
             dict
             sdcv-dictionary-data-dir)))
      (setq have-invalid-dict t)
      (message "sdcv-dictionary-complete-list is empty, command sdcv-search-detail won't work as expected."))
    (unless have-invalid-dict
      (message "The dictionary's settings look correct, sdcv should work as expected."))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sdcv-search-detail (&optional word)
  "Search WORD in `sdcv-dictionary-complete-list'. The result
will be displayed in buffer named with `sdcv-buffer-name' with
`sdcv-mode'."
  (message "Searching...")
  (with-current-buffer (get-buffer-create sdcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq sdcv-current-translate-object word)
    (insert (sdcv-search-with-dictionary word
                                         sdcv-dictionary-complete-list))
    (sdcv-goto-sdcv)
    (sdcv-mode-reinit)))

(defun sdcv-search-simple (&optional word)
  "Search WORD simple translate result."
  (let ((result (sdcv-search-with-dictionary word sdcv-dictionary-simple-list)))
    ;; Show tooltip at point if word fetch from user cursor.
    (posframe-show
     sdcv-tooltip-name
     :string result
     :position (point)
     :timeout sdcv-tooltip-timeout
     :background-color (face-attribute 'sdcv-tooltip-face :background)
     :foreground-color (face-attribute 'sdcv-tooltip-face :foreground)
     :internal-border-width sdcv-tooltip-border-width
     )
    (add-hook 'post-command-hook 'sdcv-hide-tooltip-after-move)
    (setq sdcv-tooltip-last-point (point))
    (setq sdcv-tooltip-last-scroll-offset (window-start))
    ))

(defun sdcv-say-word (word)
  (if (featurep 'cocoa)
      (call-process-shell-command
       (format "say %s" word) nil 0)
    (let ((player (or (executable-find "mpv")
                      (executable-find "mplayer")
                      (executable-find "mpg123"))))
      (if player
          (start-process
           player
           nil
           player
           (format "http://dict.youdao.com/dictvoice?type=2&audio=%s" (url-hexify-string word)))
        (message "mpv, mplayer or mpg123 is needed to play word voice")))))

(defun sdcv-hide-tooltip-after-move ()
  (ignore-errors
    (when (get-buffer sdcv-tooltip-name)
      (unless (and
               (equal (point) sdcv-tooltip-last-point)
               (equal (window-start) sdcv-tooltip-last-scroll-offset))
        (posframe-delete sdcv-tooltip-name)
        (kill-buffer sdcv-tooltip-name)))))

(defun sdcv-search-with-dictionary (word dictionary-list)
  "Search some WORD with dictionary list.
Argument DICTIONARY-LIST the word that need transform."
  (let (translate-result)
    ;; Get translate object.
    (or word (setq word (sdcv-region-or-word)))
    ;; Record current translate object.
    (setq sdcv-current-translate-object word)
    ;; Get translate result.
    (setq translate-result (sdcv-translate-result word dictionary-list))

    (if (string-equal translate-result "")
        ;; Try pick word from camelcase string and translate again if no translate result for current string.
        (progn
          (setq word (sdcv-pick-word word))
          (if sdcv-say-word-p (sdcv-say-word word))
          (sdcv-translate-result word dictionary-list))
      ;; Otherwise return translate result of current word.
      (if sdcv-say-word-p (sdcv-say-word word))
      translate-result)))

(defun sdcv-pick-word (str)
  (let ((case-fold-search nil)
        (search-index 0)
        words
        char-offset)
    (setq char-offset
          (- (point)
             (save-excursion
               (backward-word)
               (point)
               )))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str))
    (setq str (replace-regexp-in-string "_+" "_" str))
    (setq words (s-split "_" (downcase str)))
    (dolist (word words)
      (if (and (>= char-offset search-index)
               (<= char-offset (+ search-index (length word))))
          (return word)
        (setq search-index (+ search-index (length word))))
      )))

(defun sdcv-translate-result (word dictionary-list)
  "Call sdcv to search word in dictionary list, return filtered
string of results."
  (sdcv-filter
   (shell-command-to-string
    ;; Set LANG environment variable, make sure `shell-command-to-string' can handle CJK character correctly.
    (format "LANG=en_US.UTF-8 %s -n %s %s --data-dir=%s"
            sdcv-program
            (mapconcat (lambda (dict)
                         (concat "-u \"" dict "\""))
                       dictionary-list " ")
            word
            sdcv-dictionary-data-dir))))

(defun sdcv-filter (sdcv-string)
  "This function is for filter sdcv output string,.
Argument SDCV-STRING the search string from sdcv."
  (setq sdcv-string (replace-regexp-in-string sdcv-filter-string "" sdcv-string))
  (if (equal sdcv-string "")
      sdcv-fail-notify-string
    (with-temp-buffer
      (insert sdcv-string)
      (goto-char (point-min))
      (kill-line 1)                   ;remove unnecessary information.
      (buffer-string))))

(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (setq sdcv-previous-window-configuration (current-window-configuration))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entry but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only t)
    (goto-char (point-min))
    (sdcv-next-dictionary)
    (show-all)
    (message "Finished searching `%s'." sdcv-current-translate-object)))

(defun sdcv-prompt-input ()
  "Prompt input object for translate."
  (read-string (format "Word (%s): " (or (sdcv-region-or-word) ""))
               nil nil
               (sdcv-region-or-word)))

(defun sdcv-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))

(provide 'sdcv)

;;; sdcv.el ends here

;;; LocalWords:  sdcv StartDict startdict posframe stardict KDic XDICT CDICT
;;; LocalWords:  FOLDOC WordNet ChiYuan Hideshow reinit
