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

;; The minor-mode for show the meaning of word under the
;; point to minibuffer.

;; Config:
;;
;; You have to have the `sdic' elisp program and dictionary
;; files of sdic type.  Next, you place this file to
;; `load-path' passing directory.  And, add following
;; setting.
;;
;; (require 'sdic-inline)
;; (sdic-inline-mode t)   ; enable sdic-inline.
;;
;; ;; Setting dictionary path.
;; (setq sdic-inline-eiwa-dictionary "/home/khiker/lib/dict/eijirou.sdic")
;; (setq sdic-inline-waei-dictionary "/home/khiker/lib/dict/waeijirou.sdic")
;;
;; You can select enabling `sdic-inline' by following variables.
;;
;; * sdic-inline-enable-modes
;; * sdic-inline-enable-faces
;; * sdic-inline-enable-filename-regex
;; * sdic-inline-enable-func
;;
;; If you want to do stemming (for example, "enabled" -> "enable"),
;; set following variable.
;;
;; (setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
;;
;; This package can popup the detailed meaning of word under
;; the point by C-cC-p. It's necessary to load `popup' library.
;; ;; http://github.com/m2ym/auto-complete
;;
;; (require 'popup)
;;
;; Todo:
;;
;; * Integrate sdic-inline dictionary setting and sdic dictionary setting.
;; * Specify multiple dictionary files.

;;; Code:

(require 'sdic)


;;; Variables:

(defconst sdic-inline-version "0.4.5.1"
  "Version of sdic-inline.")


(defvar sdic-inline-eiwa-dictionary nil
  "*Path of EIWA (English to Japanese) Dictionary.")

(defvar sdic-inline-waei-dictionary nil
  "*Path of WAEI (Japanese to English) Dictionary.")

(defvar sdic-inline-dictionary-encoding 'euc-jp
  "*Encoding of Dictionary.")

(defvar sdic-inline-search-method 'grep
  "*Method of sdic search type.
Detail is docstring of `sdicf-open'.")

(defvar sdic-inline-get-word-func
  'sdic-inline-word-at-point-or-region
  "*Function that get the word at point.")

(defvar sdic-inline-search-func
  'sdic-inline-search-word
  "*Function that search specified word")

(defvar sdic-inline-display-func
  'sdic-inline-display-minibuffer
  "*Function that show the meaning of word.")

(defvar sdic-inline-not-search-style
  'point
  "*Word search style.
Set `word', `sdic-inline-last-word' == current word -> do not search.
Set `point', `sdic-inline-last-point' == current point -> do not search.
Others, Always search.")

(defvar sdic-inline-delay 0.50
  "*Time in seconds to delay before showing a meaning of word.")

(defvar sdic-inline-enable-modes
  '(text-mode outline-mode fundamental-mode)
  "*Major-mode that enable the `sdic-inline'.")

(defvar sdic-inline-enable-faces
  '(font-lock-string-face font-lock-comment-face)
  "*Faces that enable the `sdic-inline'.")

(defvar sdic-inline-enable-filename-regex
  ".*\\.txt$"
  "*Filename regexp that enable the `sdic-inline'.")

(defvar sdic-inline-enable-func nil
  "*Specify user definition function.
If specified function returns t, sdic-inline-mode is enabled.")

(defvar sdic-inline-word-at-point-strict nil
  "*If this variable is non-nil, return nil unless point is within
or adjacent to a symbol or word.
Option for 1st argument of `current-word'.")


(defvar sdic-inline-last-word nil)

(defvar sdic-inline-last-point nil)

(defvar sdic-inline-last-entry nil)

(defvar sdic-inline-timer nil)

(defvar sdic-inline-display-popup-now nil)

(defvar sdic-inline-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\@\C-c" 'sdic-inline-clear-last-word)
    (define-key keymap "\C-c\C-p" 'sdic-inline-display-popup)
    keymap))


;; Functions:

(define-minor-mode sdic-inline-mode
  "sdic-inline-mode. Display the meaning of word under the point."
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
  "running or not running `sdic-inline'."
  (cond
   ((and (not (minibufferp))
         (or (member major-mode sdic-inline-enable-modes)
             (sdic-inline-enable-regex)
             (sdic-inline-enable-face)
             (sdic-inline-enable-func)))
    (condition-case err
        (progn
          (unless sdic-inline-mode
            (setq sdic-inline-mode t))
          (sdic-inline-function))
      (error
       (unwind-protect
           (message "Error: %S; sdic-inline-mode now disabled." err)
         (setq sdic-inline-mode nil)))))
   (t
    (setq sdic-inline-mode nil))))

(defun sdic-inline-enable-regex ()
  (and sdic-inline-enable-filename-regex
       (buffer-file-name)
       (string-match sdic-inline-enable-filename-regex
                     (buffer-file-name))))

(defun sdic-inline-enable-face ()
  (when sdic-inline-enable-faces
    (let ((prop (text-properties-at (point))))
      (catch 'ok
        (dolist (i sdic-inline-enable-faces)
          (when (member i prop)
            (throw 'ok t)))))))

(defun sdic-inline-enable-func ()
  (and sdic-inline-enable-func
       (funcall sdic-inline-enable-func)))

(defun sdic-inline-word-at-point-or-region ()
  (or (sdic-inline-word-region)
      (sdic-inline-word-at-point)))

(defun sdic-inline-word-region ()
  (when (and transient-mark-mode
             mark-active)
    (buffer-substring-no-properties
     (region-beginning) (region-end))))

(defun sdic-inline-word-at-point ()
  "Get word under the point."
  (let ((cw (current-word sdic-inline-word-at-point-strict))
        (sw (sdic-word-at-point)))
    (when (and cw
               (string-match "\\w" cw)
               sw)
      sw)))

(defun sdic-inline-do-search (w)
  (or (null sdic-inline-last-word)
      (null sdic-inline-last-point)
      (cond
       ((eq sdic-inline-not-search-style 'word)
        (not (string= sdic-inline-last-word w)))
       ((eq sdic-inline-not-search-style 'point)
        (not (eq sdic-inline-last-point (point))))
       (t
        t))))

(defun sdic-inline-function ()
  "Get entry of the word under the point.
and call `sdic-inline-display-func'."
  (let ((w (funcall sdic-inline-get-word-func))
        jp)
    (when (and w
               (sdic-inline-do-search w))
      (setq jp (string-match "\\cj" w))
      (let ((entry (funcall sdic-inline-search-func w jp)))
        (when entry
          (setq sdic-inline-last-word w)
          (setq sdic-inline-last-point (point))
          (setq sdic-inline-last-entry entry)
          (funcall sdic-inline-display-func entry))))))

(defun sdic-inline-search-word (word jp)
  "Get entry of the specified word."
  (when (or (and (not jp) sdic-inline-eiwa-dictionary)
            (and jp sdic-inline-waei-dictionary))
    (let (dic)
      (unwind-protect
          (progn
            (setq dic
                  (sdicf-open (expand-file-name (if jp
                                                    sdic-inline-waei-dictionary
                                                  sdic-inline-eiwa-dictionary))
                              sdic-inline-dictionary-encoding
                              sdic-inline-search-method))
            (sdicf-search dic 'exact word))
        (when (boundp 'dic)
          (sdicf-close dic))))))

(defun sdic-inline-search-word-with-stem (word jp)
  "Get entry of the specified word with stemming"
  (cond
   ((not jp)
    (let* ((stemed-list (stem-english word))
           entry entries)
      (dolist (i stemed-list)
        (setq entry (sdic-inline-search-word i jp))
        (when (and entry
                   (not (member (car entry) entries)))
          (setq entries (append entries entry))))
      entries))
   (t
    ;; Japanese was specified.
    (sdic-inline-search-word word jp))))

(defun sdic-inline-cut-string-1line (text)
  "if width of `text' > (- (frame-wdith) 15) cut `text' to one line"
  (let ((w (- (frame-width) 15))
        (cutted-p nil))
    (while (> (string-width text) w)
      (setq text (substring text 0 (- (length text) 10))
            cutted-p t))
    (when cutted-p
      (setq text (concat text "..."))))
  text)

(defun sdic-inline-display-minibuffer (entry)
  "Display meaning of word to the minibuffer."
  (let ((msg "") multiline-p)
    (dolist (i entry)
      (setq msg (concat
                 msg
                 (if multiline-p "\n")
                 (sdic-inline-cut-string-1line
                  (concat (sdicf-entry-headword i)
                          "："
                          (sdicf-entry-text i)))))
      (unless multiline-p
        (setq multiline-p t)))
    (message "%s" msg)))

(defun sdic-inline-clear-last-word ()
  "Set nil to `sdic-inline-last-word'"
  (interactive)
  (setq sdic-inline-last-word nil))

(defun sdic-inline-display-popup ()
  "Popup detailed meaning of word."
  (interactive)
  (when (and (fboundp 'popup-cascade-menu)
             'sdic-inline-last-entry)
    (let ((len (length sdic-inline-last-entry))
          lst)
      (dolist (i sdic-inline-last-entry)
        (setq lst (cons (list (sdicf-entry-headword i)
                              (sdicf-entry-text i))
                        lst)))
      (cond
       ((> len 1)
        (message "Select item to show detail.")
        (popup-tip (popup-cascade-menu (nreverse lst))))
;;        (popup-cascade-menu lst))
       ((= len 1)
        (popup-tip (nth 1 (car lst))))
       (t
        nil)))))

(provide 'sdic-inline)

;;; sdic-inline.el ends here

