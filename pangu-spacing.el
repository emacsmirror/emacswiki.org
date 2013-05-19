;;; pangu-spacing.el --- minor-mode to add space between Chinese and English characters.

;; Copyright (C) 2013 Yen-Chin, Lee.

;; Author: coldnew <coldnew.tw@gmail.com>
;; Kyewords: converience
;; Version: 0.1
;; X-URL: http://github.com/coldnew/pangu-spacing

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Commentary (English):

;; pangu-spacing-mode is an minor-mode to auto add space between Chinese
;; and English characters.

;; Take following sentance for example:
;;
;;      ä½ å¥½ï¼ææ¯coldnewï¼æåæ­¡ä½¿ç¨emacsã
;;
;; After you use pangu-spacing-mdoe, you will see
;;
;;      ä½ å¥½ï¼ææ¯ coldnewï¼æåæ­¡ä½¿ç¨ emacsã

;; This space between English and Chinese characters is called
;; pangu-spacing by sinologist. Since it separate the chaos between
;; full-width and half-width characters.

;;; Commentary (Chinese):

;; pangu-spacing-mode æ¯ä¸åå¯ä»¥èªåå¹«ä½ å°ä¸­æèè±æä¹éå ä¸`ç©ºç½'ä½çºå
;; éç minor-mode, ä»çåç¨±ä¾èªæ¼ paranoid-auto-spacing ä¸ç READMEã
;;
;;      å¼è¿°èª paranoid-auto-spacing README [1]
;;
;;      å¦æä½ è·æä¸æ¨£ï¼æ¯æ¬¡çå°ç¶²é ä¸çä¸­æå­åè±æãæ¸å­ãç¬¦èæ å¨ä¸å¡ï¼å°±æ
;;      åç«é£å®ï¼å¿ä¸ä½æ³å¨å®åä¹éå åç©ºæ ¼ãéåå¤æï¼æ¯æ´ Chrome å Firefoxï¼
;;      æ­£æ¯ä½ å¨ç¶²è·¯ä¸çèµ°è·³æéè¦çæ±è¥¿ï¼å®æèªåæ¿ä½ å¨ç¶²é ä¸­ææçä¸­æå­åå
;;      å½¢çè±æãæ¸å­ãç¬¦èä¹éæå¥ç©ºç½ã
;;
;;      æ¼¢å­¸å®¶ç¨±éåç©ºç½å­åçºãç¤å¤ä¹ç½ãï¼å çºå®åéäºå¨å½¢å­ååå½¢å­ä¹éçæ··
;;      æ²ãå¦æç ç©¶é¡¯ç¤ºï¼æå­çæåä¸åæ­¡å¨ä¸­æåè±æä¹éå ç©ºæ ¼çäººï¼ææè·¯é½
;;      èµ°å¾å¾è¾è¦ï¼æä¸æçæ¯ä¾æå¨ 34 æ­²çæåè·èªå·±ä¸æçäººçµå©ï¼èå¶é¤ä¸æ
;;      çäººæå¾åªè½æéºç¢ççµ¦èªå·±çè²ãç¢ç«ææè·æ¸å¯«é½éè¦é©æå°çç½ã
;;
;;      èå¤§å®¶å±åä¹ãori test

;;      [1] https://github.com/gibuloto/paranoid-auto-spacing

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install pangu-spacing
;;
;; In your .emacs
;;
;;      (require 'pangu-spacing)
;;      (global-pangu-spacing-mode 1)
;;
;; pangu-spacing-mode do not really insert space between English and
;; Chinese by defaut, you should enable this option manually.
;;
;;      (setq pangu-spacing-real-insert-separtor t)

;;; Code:

;;;; Custom Variables

(defcustom pangu-spacing-separator " "
  "String to be display between Chinese and English."
  :group 'pangu-spacing
  :type 'string
  :initialize 'custom-initialize-default)

(defcustom pangu-spacing-real-insert-separtor nil
  "Set t or nil to make space show only on overlay or insert in file."
  :group 'pangu-spacing
  :type 'boolean)

(defface pangu-spacing-separator-face nil
  "Face for pangu-spacing-mode separator."
  :group 'pangu-spacing)

;;;; Local variables

(defvar pangu-spacing-chinese-before-english-regexp
  (rx (group-n 1 (category chinese))
      (group-n 2 (in "a-zA-Z0-9")))
  "Regexp to find Chinese character before English character.")

(defvar pangu-spacing-chinese-after-english-regexp
  (rx (group-n 1 (in "a-zA-Z0-9"))
      (group-n 2 (category chinese)))
  "Regexp to find Chinese character after English character.")

(defvar pangu-spacing-chinese-before-english-regexp-exclude
  (rx (group-n 1 (in "[ãï¼ï¼ï¼ï¼ï¼ããï¼ï¼ã]"))
      (group-n 2 (in "a-zA-Z0-9")))
  "Excluded regexp to find Chinese character before English character.")

(defvar pangu-spacing-chinese-after-english-regexp-exclude
  (rx (group-n 1 (in "a-zA-Z0-9"))
      (group-n 2 (in "[ãï¼ï¼ï¼ï¼ï¼ããï¼ï¼ã]")))
  "Excluded regexp to find Chinese character after English character.")

;;;; Functions

(defmacro pangu-spacing-search-buffer (regexp func)
  "Helper macro to search buffer and do func according regexp for
pangu-spacing-mode."
  `(let ((start (window-start))
         (end (window-end)))
     (goto-char start)
     (while (re-search-forward ,regexp end t) ,func)))

(defmacro pangu-spacing-search-overlay (func regexp)
  "Helper macro to search and update overlay according func and regexp for
pangu-sapce-mode."
  `(pangu-spacing-search-buffer ,regexp
                                (,func (match-beginning 1) (match-end 1))))

(defun pangu-spacing-search-and-replace (match regexp)
  "Replace regexp with match in buffer."
  (pangu-spacing-search-buffer regexp (replace-match match nil nil)))

(defun pangu-spacing-overlay-p (ov)
  "Determine whether overlay OV was created by space-between."
  (and (overlayp ov) (overlay-get ov 'pangu-spacing-overlay)))


(defun pangu-spacing-check-overlay ()
  "Insert a space between English words and Chinese charactors in overlay."
  (pangu-spacing-delete-all-overlays)
  (save-excursion
    (pangu-spacing-search-overlay pangu-spacing-make-overlay
                                  pangu-spacing-chinese-before-english-regexp)

    (pangu-spacing-search-overlay pangu-spacing-make-overlay
                                  pangu-spacing-chinese-after-english-regexp)

    (pangu-spacing-search-overlay pangu-spacing-delete-overlay
                                  pangu-spacing-chinese-before-english-regexp-exclude)

    (pangu-spacing-search-overlay pangu-spacing-delete-overlay
                                  pangu-spacing-chinese-after-english-regexp-exclude)))

(defun pangu-spacing-check-buffer ()
  "Real insert separator between English words and Chinese charactors in buffer."
  (save-excursion
    (pangu-spacing-search-and-replace "\\1 \\2"
                                      pangu-spacing-chinese-before-english-regexp)

    (pangu-spacing-search-and-replace "\\1 \\2"
                                      pangu-spacing-chinese-after-english-regexp)

    (pangu-spacing-search-and-replace "\\1\\2"
                                      (replace-regexp-in-string "\\\\)\\\\(" "\\\\) \\\\("
                                                                pangu-spacing-chinese-before-english-regexp-exclude))

    (pangu-spacing-search-and-replace "\\1\\2"
                                      (replace-regexp-in-string "\\\\)\\\\(" "\\\\) \\\\("
                                                                pangu-spacing-chinese-after-english-regexp-exclude))))

(defun pangu-spacing-region-has-pangu-spacing-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
  Return t if it has at least one pangu-spacing overlay, nil if no overlay."
  (let ((ov (overlays-in beg end))
        (has-pangu-spacing-overlays nil))
    (while (consp ov)
      (when (pangu-spacing-overlay-p (car ov))
        (setq has-pangu-spacing-overlays t))
      (setq ov (cdr ov))
      has-pangu-spacing-overlays)))

(defun pangu-spacing-make-overlay (beg end)
  "Allocate a pangu-spacing overlay in range."
  (when (not (pangu-spacing-region-has-pangu-spacing-overlays beg end))
    (let ((ov (make-overlay beg end nil t t))
          (face 'pangu-spacing-separator-face))
      (overlay-put ov 'pangu-spacing-overlay t)
      (overlay-put ov 'after-string (propertize pangu-spacing-separator 'face face))
      ov)))

(defun pangu-spacing-delete-overlay (beg end)
  "Delete all pangu-spacing-overlays in BUFFER."
  (dolist (ov (overlays-in beg end))
    (when (pangu-spacing-overlay-p ov)
      (delete-overlay ov))))

(defun pangu-spacing-delete-all-overlays ()
  "Delete all pangu-spacing-overlays in BUFFER."
  (pangu-spacing-delete-overlay (point-min) (point-max)))

;;;###autoload
(define-minor-mode pangu-spacing-mode
  "Toggle pangu-spacing-mode"
  :group 'pangu-spacing
  :global nil
  :init-value nil
  :lighter "Î¡"
  (make-variable-buffer-local 'post-command-hook)
  (save-restriction
    (widen)
    (if pangu-spacing-mode
        (add-hook 'post-command-hook (if pangu-spacing-real-insert-separtor
                                         'pangu-spacing-check-buffer
                                       'pangu-spacing-check-overlay))
      (progn
        (remove-hook 'post-command-hook (if pangu-spacing-real-insert-separtor
                                            'pangu-spacing-check-buffer
                                          'pangu-spacing-check-overlay))
        (pangu-spacing-delete-all-overlays))))
  pangu-spacing-mode)

;;;###autoload
(define-globalized-minor-mode global-pangu-spacing-mode
  pangu-spacing-mode pangu-spacing-mode)


(provide 'pangu-spacing)
;; pangu-spacing.el ends here
