;;; sdic-inline.el --- Program to view dictionary.

;; Copyright (C) 2010  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: dictionary

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; カーソル下の単語の意味をミニバッファへと表示するマイナーモード。

;; 設定:
;;
;; まず、sdic と sdic 形式の辞書ファイルが必要。次に、このファ
;; イルを `load-path' の通った場所へと置き、以下の設定を加え
;; る。
;;
;; (require 'sdic-inline)
;; (sdic-inline-mode t)
;;
;; また、辞書ファイルの設定を行う。
;;
;; (setq sdic-inline-eiwa-dictionary "/home/khiker/lib/dict/eijirou.sdic")
;; (setq sdic-inline-waei-dictionary "/home/khiker/lib/dict/waeijirou.sdic")
;;
;; 適宜、sdic-inline を使用したいモードを、
;; `sdic-inline-enable-modes' に加える。
;;
;; ミニバッファへと表示する特性上、フレームの横幅よりも長くなっ
;; てしまうテキスト(辞書の意味)については、ミニバッファに1単
;; 語につき1行で収まるように途中でカットしてしまっています。
;; 詳細な意味を表示したい場合、`sdic-inline-display-popup' 関
;; 数を呼び出して下さい。キーは C-cC-p に割り当てられています。
;; なお、使用するには、popup ライブラリが必要です。
;; # http://github.com/m2ym/auto-complete
;;
;; (require 'popup)

;; Todo:
;;
;; * 辞書ファイルを sdic とは別に指定しなくても良くする事。
;; * 複数の辞書ファイルを指定できるようにする事。
;; * sdic-mode を起動するかどうかをファイル名からも設定できるようにする事。
;; * docstring の追加
;; * translate document to English.

;;; Code:

(require 'sdic)


;;; Variables:

(defconst sdic-inline-version "0.1")

(defvar sdic-inline-eiwa-dictionary nil)

(defvar sdic-inline-waei-dictionary nil)

(defvar sdic-inline-dictionary-encoding 'euc-jp)

(defvar sdic-inline-search-method 'grep)

(defvar sdic-inline-display-func
  'sdic-inline-display-minibuffer)

(defvar sdic-inline-delay 0.50)

(defvar sdic-inline-enable-modes
  '(text-mode outline-mode fundamental-mode))

(defvar sdic-inline-display-popup-key "\C-c\C-p")


(defvar sdic-inline-last-entry nil)

(defvar sdic-inline-timer nil)

(defvar sdic-inline-display-popup-now nil)

(defvar sdic-inline-map (make-sparse-keymap))
(define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-display-popup)


;; Functions:

(define-minor-mode sdic-inline-mode
  "sdic-inline-mode"
  :init-value t
  :global nil
  :keymap sdic-inline-map
  :lighter " SDIC"
  (if sdic-inline-mode
      (sdic-inline-start-timer)
    (sdic-inline-stop-timer)))

(defun sdic-inline-start-timer ()
  "start timer."
  (when sdic-inline-timer
    (cancel-timer sdic-inline-timer))
  (setq sdic-inline-timer
        (run-with-idle-timer sdic-inline-delay t 'sdic-inline-hook)))

(defun sdic-inline-stop-timer ()
  "stop timer"
  (when sdic-inline-timer
    (cancel-timer sdic-inline-timer)
    (setq sdic-inline-timer nil)))

(defun sdic-inline-hook ()
  (cond
   ((and (not (minibufferp))
         (member major-mode sdic-inline-enable-modes))
    (condition-case err
        (sdic-inline-function)
      (error
       (unwind-protect
           (message "Error: %S; sdic-inline-mode now disabled." err)
         (progn
           (setq sdic-inline-mode nil))))))
   (t
    (setq sdic-inline-mode nil))))

(defun sdic-inline-word-at-point ()
  (let ((cw (current-word))
        (sw (sdic-word-at-point)))
    (when (and cw sw)
      sw)))

(defun sdic-inline-function ()
  (let ((w (sdic-inline-word-at-point))
        jp)
    (when w
      (setq jp (string-match "\\cj" w))
      (let ((entry (sdic-inline-search-word w jp)))
        (when entry
          (setq sdic-inline-last-entry entry)
          (funcall sdic-inline-display-func entry))))))

(defun sdic-inline-search-word (word jp)
  (when (or (and (not jp) sdic-inline-eiwa-dictionary)
            (and jp sdic-inline-waei-dictionary))
    (unwind-protect
        (let ((dic (sdicf-open (if jp
                                   sdic-inline-waei-dictionary
                                 sdic-inline-eiwa-dictionary)
                               sdic-inline-dictionary-encoding
                               sdic-inline-search-method)))
          (sdicf-search dic 'exact word))
      (when (boundp 'dic)
        (sdicf-close dic)))))

(defun sdic-inline-display-minibuffer (entry)
  (let ((msg "")
        (num 0)
        head text cut)
    (dolist (i entry)
      ;; (setq head (sdicf-entry-headword (nth i entry)))
      (setq text (sdicf-entry-text i))
      (while (> (string-width text) (- (frame-width) 15))
        (setq text (substring text 0 (- (length text) 10))
              cut t))
      (when cut
        (setq text (concat text "...")))
      (setq msg (concat msg (if (> num 0) "\n") text))
      (setq num (1+ num)))
    (message "%s" msg)))

(defun sdic-inline-display-popup ()
  (interactive)
  (when (and (fboundp 'popup-cascade-menu)
             'sdic-inline-last-entry)
    (let ((cnt 0)
          head text lst)
      (dolist (i sdic-inline-last-entry)
        (setq head (sdicf-entry-headword i))
        (setq text (sdicf-entry-text i))
        (setq lst (cons (list head text) lst))
        (setq cnt (1+ cnt)))
      (cond
       ((> cnt 1)
        (message "Select item to show detail.")
        (popup-tip (popup-cascade-menu lst)))
;;        (popup-cascade-menu lst))
       (t
        (popup-tip text))))))

(provide 'sdic-inline)

;;; sdic-inline.el ends here
