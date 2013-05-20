;;; pangu-spacing.el --- Minor-mode to add space between Chinese and English characters.

;; Copyright (C) 2013 Yen-Chin, Lee.

;; Author: coldnew <coldnew.tw@gmail.com>
;; Kyewords: converience
;; Version: 0.3
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
;;      你好，我是coldnew，我喜歡使用emacs。
;;
;; After you use pangu-spacing-mdoe, you will see
;;
;;      你好，我是 coldnew，我喜歡使用 emacs。

;; This space between English and Chinese characters is called
;; pangu-spacing by sinologist. Since it separate the chaos between
;; full-width and half-width characters.

;;; Commentary (Chinese):

;; pangu-spacing-mode 是一個可以自動幫你將中文與英文之間加上`空白'作為分
;; 隔的 minor-mode, 他的名稱來自於 paranoid-auto-spacing 上的 README。
;;
;;      引述自 paranoid-auto-spacing README [1]
;;
;;      如果你跟我一樣，每次看到網頁上的中文字和英文、數字、符號擠在一塊，就會
;;      坐立難安，忍不住想在它們之間加個空格。這個外掛（支援 Chrome 和 Firefox）
;;      正是你在網路世界走跳所需要的東西，它會自動替你在網頁中所有的中文字和半
;;      形的英文、數字、符號之間插入空白。
;;
;;      漢學家稱這個空白字元為「盤古之白」，因為它劈開了全形字和半形字之間的混
;;      沌。另有研究顯示，打字的時候不喜歡在中文和英文之間加空格的人，感情路都
;;      走得很辛苦，有七成的比例會在 34 歲的時候跟自己不愛的人結婚，而其餘三成
;;      的人最後只能把遺產留給自己的貓。畢竟愛情跟書寫都需要適時地留白。
;;
;;      與大家共勉之。ori test

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
;;
;; If you enable this, space will be inserted before you save file.
;;

;;; Code:

;;;; Custom Variables

(defcustom pangu-spacing-separator " "
  "String to be display between Chinese and English."
  :group 'pangu-spacing
  :type 'string
  :initialize 'custom-initialize-default)

(defcustom pangu-spacing-real-insert-separtor nil
  "Set t or nil to make space show only on overlay or insert in file.
When you set t here, the space will be insert when you save file."
  :group 'pangu-spacing
  :type 'boolean)

(defface pangu-spacing-separator-face nil
  "Face for pangu-spacing-mode separator."
  :group 'pangu-spacing)

(defvar pangu-spacing-inhibit-mode-alist
  '(eshell-mode shell-mode term-mode)
  "Inhibit mode alist for pangu-spacing-mode.")

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
  (rx (group-n 1 (in "。，！？；：「」（）、"))
      (group-n 2 (in "a-zA-Z0-9")))
  "Excluded regexp to find Chinese character before English character.")

(defvar pangu-spacing-chinese-after-english-regexp-exclude
  (rx (group-n 1 (in "a-zA-Z0-9"))
      (group-n 2 (in "。，！？；：「」（）、")))
  "Excluded regexp to find Chinese character after English character.")

;;;; Functions

(defmacro pangu-spacing-search-buffer (regexp start end func)
  "Helper macro to search buffer and do func according regexp for
pangu-spacing-mode."
  `(let ((start ,start) (end ,end))
     (save-excursion
       (goto-char start)
       (while (re-search-forward ,regexp end t) ,func))))

(defmacro pangu-spacing-search-overlay (func regexp)
  "Helper macro to search and update overlay according func and regexp for
pangu-sapce-mode."
  `(pangu-spacing-search-buffer ,regexp ;;(window-start (selected-window))  (window-end (selected-window) t)
				(point-min) (point-max)
                                (,func (match-beginning 1) (match-end 1))))

(defun pangu-spacing-search-and-replace (match regexp)
  "Replace regexp with match in buffer."
  (pangu-spacing-search-buffer regexp (point-min) (point-max)
                               (replace-match match nil nil)))

(defun pangu-spacing-overlay-p (ov)
  "Determine whether overlay OV was created by space-between."
  (and (overlayp ov) (overlay-get ov 'pangu-spacing-overlay)))


(defun pangu-spacing-check-overlay ()
  "Insert a space between English words and Chinese charactors in overlay."
  (pangu-spacing-delete-all-overlays)
  (pangu-spacing-search-overlay pangu-spacing-make-overlay
                                pangu-spacing-chinese-before-english-regexp)

  (pangu-spacing-search-overlay pangu-spacing-make-overlay
                                pangu-spacing-chinese-after-english-regexp)

  (pangu-spacing-search-overlay pangu-spacing-delete-overlay
                                pangu-spacing-chinese-before-english-regexp-exclude)

  (pangu-spacing-search-overlay pangu-spacing-delete-overlay
                                pangu-spacing-chinese-after-english-regexp-exclude))

(defun pangu-spacing-modify-buffer ()
  "Real insert separator between English words and Chinese charactors in buffer."
  (when pangu-spacing-real-insert-separtor
    (pangu-spacing-search-and-replace "\\1 \\2"
                                      pangu-spacing-chinese-before-english-regexp)

    (pangu-spacing-search-and-replace "\\1 \\2"
                                      pangu-spacing-chinese-after-english-regexp)

    (pangu-spacing-search-and-replace "\\1\\2"
                                      (replace-regexp-in-string "\\\\)\\\\(" "\\\\) \\\\("
                                                                pangu-spacing-chinese-before-english-regexp-exclude))

    (pangu-spacing-search-and-replace "\\1\\2"
                                      (replace-regexp-in-string "\\\\)\\\\(" "\\\\) \\\\("
                                                                pangu-spacing-chinese-after-english-regexp-exclude)))
  ;; nil must be returned to allow use in write file hooks
  nil)

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

(defun turn-on-pangu-spacing (beg end)
  (pangu-spacing-check-overlay))

;;;###autoload
(define-minor-mode pangu-spacing-mode
  "Toggle pangu-spacing-mode"
  :group 'pangu-spacing
  :global nil
  :init-value nil
  :lighter " Ρ"
  (unless (or (member major-mode pangu-spacing-inhibit-mode-alist)
              (minibufferp (current-buffer)))
    (save-restriction
      (widen)
      (if pangu-spacing-mode
          (progn
            (jit-lock-register 'turn-on-pangu-spacing)
            (add-hook 'local-write-file-hooks 'pangu-spacing-modify-buffer))
        (progn
          (jit-lock-unregister 'turn-on-pangu-spacing)
          (remove-hook 'local-write-file-hooks 'pangu-spacing-modify-buffer)
          (pangu-spacing-delete-all-overlays)))))
  pangu-spacing-mode)

;;;###autoload
(define-globalized-minor-mode global-pangu-spacing-mode
  pangu-spacing-mode pangu-spacing-mode)


(provide 'pangu-spacing)
;;; pangu-spacing.el ends here
