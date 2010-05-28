;;; eim.el --- emacs input method
;; Last changed Time-stamp: <2006-04-10 23:58:44 Wenbin Ye>

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Ye Wenbin<wenbinye@163.com>, ChunYe Wang <CharlesWang@peoplemail.com.cn>
;; Keywords: 

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;;  Change Log:

;;; Feature:
;; 1. 导入速度快，节约内存
;; 2. 可以较多可定制的地方

;;; TODO:
;; 1. 每次选择的结果可以在退出时保存在文件中。启动时导入。
;; 2. 测试其它输入法。
;; 3. eim-make-database 和 eim-make-char-database 中的列表是有重复的。怎样
;;    既能快速导入又能不重复。

(eval-when-compile
  (require 'cl))

(defvar eim-version "1.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database relate
(defvar eim-database nil
  "All word is store here as a symbol")

(defvar eim-char-database nil
  "it is used to map a character into its code.")

(defvar eim-packages nil
  "A list of input methods.
An input method has these variables:
     char-database, word-database, validate-function, invalid-functions,
     handle-functions, keymap")
(defvar eim-current-package nil)

;; wcy-py-put-a-word-into-database
(defun eim-intern-word (code word &optional db)
  "Add the WORD to the symbol in `eim-database' which name is CODE"
  (or db (setq db eim-database))
  (let* ((s (intern-soft code db))
         (ws (symbol-value s)))
    ;;    (message "add %s => %s" code word)
    (set (intern code db) (add-to-list 'ws word))))

;; these two function should call after the database obarray create immediately
;; otherwise the data will be erase.
(defun eim-make-database (words)
  "Set `eim-database'"
  (dolist (word words)
    (set (intern (car word) eim-database) (cdr word))))

(defun eim-make-char-database (chars)
  "Set `eim-char-database'"
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c eim-char-database) code)))))

(defun eim-get (code &optional db)
  "Get the symbol value whose name is CODE"
  (or db (setq db eim-database))
  (symbol-value (intern-soft code db)))

(defun eim-get-completion (prefix)
  "Get all completions of the PREFIX in `eim-database'"
  (all-completions prefix eim-database))

(defun eim-get-char-code (char &optional db)
  "Get the code of the character CHAR"
  (or db (setq db eim-char-database))
  (symbol-value (intern-soft (char-to-string char) db)))

;;; package function
(defsubst eim-char-database ()
  (nth 2 eim-current-package))
(defsubst eim-database ()
  (nth 3 eim-current-package))
(defsubst eim-docstring ()
  (nth 1 eim-current-package))
(defsubst eim-params ()
  (nth 4 eim-current-package))
(defsubst eim-package-name ()
  (nth 0 eim-current-package))

(defun eim-install-package (package docstr char-db word-db &optional params)
  "Install the package to `eim-packages'"
  (setq eim-char-database char-db
        eim-database word-db)
  (dolist (p params)
    (set (car p) (cdr p)))
  (add-to-list 'eim-packages (list package docstr char-db word-db params)))

(defun eim-uninstall-package (package-name)
  (setq eim-packages (remove (assoc package-name eim-packages)
                             eim-packages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input method relate function
;; validate the input char
(defvar eim-start nil)
(defvar eim-end nil)
(defvar eim-activate nil)
(defvar eim-choices-string "")
(defvar eim-display-string "")
(defvar eim-current-string "")
(defvar eim-choices nil)
(defvar eim-completions nil)
(defvar eim-history-file nil)
(defvar eim-select-pos nil)
(defvar eim-overlay nil
  "Overlay to display `eim-display-string'")
(defvar eim-curse-overlay nil
  "Overlay to set keymap")

(defvar eim-stop-length 10000
  "When input so much char, the current display string will insert, leaving the
last input character.")
(defvar eim-page-length 7
  "The number of choice to display per page.")
(defvar eim-inactivate-hook nil)
(defvar eim-activate-hook nil)
(defvar eim-after-load-hook nil)
(defvar eim-invalid-function 'eim-punctuation-handle-function)
(defvar eim-handle-function 'eim-default-handle)
(defvar eim-validate-function (lambda (char)
                                (let ((c (string-to-char char)))
                                  (and (> c 96) (< c 123)))))

(defvar eim-default-map nil)
(when (null eim-default-map)
  (setq eim-default-map
        (let ((map (make-sparse-keymap)))
          (dotimes (i 10)
            (define-key map (number-to-string i) 'eim-select))
          (define-key map " " 'eim-select-current)
          (define-key map "\C-m" 'eim-no-clear-select)
          (define-key map "\C-n" 'eim-next-page)
          (define-key map "\C-c" 'eim-clear-select)
          (define-key map "\C-p" 'eim-prev-page)
          ;; how to disable movement
          (define-key map "\C-b" 'ignore)
          (define-key map "\C-f" 'ignore)
          (define-key map "\C-a" 'ignore)
          (define-key map "\C-e" 'ignore)
          map)))

(defvar eim-mode-map eim-default-map)

(defvar eim-default-setting `((eim-handle-function . eim-default-handle)
                              (eim-invalid-function . eim-punctuation-handle-function)
                              (eim-validate-function . eim-lower-case-letterp)
                              (eim-stop-length . 10000)
                              (eim-page-length . 7)
                              (eim-history-file . nil)
                              (eim-mode-map . ,eim-default-map))
  "Default settings")

;; what to do when the input char is not valid
(defvar eim-punctuation-list 
  '(("{" . "『")
    ("|" . "÷")
    ("}" . "』")
    ("~" . "～")
    ("`" . "·")
    ("!" . "！")
    ("#" . "＃")
    ("$" . "￥")
    ("%" . "％")
    ("&" . "※")
    ("(" . "（")
    (")" . "）")
    ("*" . "×")
    ("+" . "＋")
    ("," . "，")
    ("-" . "－")
    ("." . "。")
    ("/" . "、")
    (":" . "：")
    (";" . "；")
    ("<" . "《")
    ("=" . "＝")
    (">" . "》")
    ("?" . "？")
    ("@" . "◎")
    ("[" . "【")
    ("]" . "】")
    ("^" . "……")
    ("_" . "——")
    ))

(defvar eim-quote-punctuation '(("\"" nil "“" "”")
                                ("'"  nil "‘" "’")))

(defun eim-subseq (list from &optional to)
  (butlast (nthcdr from list) (- (length list) to)))

(defun eim-mod (x y)
  "like `mod', but when result is 0, return Y"
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun eim-reset ()
  (setq eim-start nil)
  (setq eim-end nil)
  (setq eim-activate nil)
  (setq eim-choices-string "")
  (setq eim-display-string "")
  (setq eim-current-string "")
  (setq eim-choices nil)
  (setq eim-completions nil)
  (setq eim-completion-pos 0)
  (setq eim-select-pos 1)
  (if (and eim-overlay (overlayp eim-overlay))
      (delete-overlay eim-overlay))
  (if (and eim-curse-overlay (overlayp eim-curse-overlay))
      (delete-overlay eim-curse-overlay)))

;; this need to improve
(defun eim-inactivate ()
  (interactive)
  (run-hooks 'eim-inactivate-hook)
  (remove-hook 'after-change-functions 'eim-after-change t))

(defun eim-use-package (package-name &rest libraries)
  (interactive)
  (make-local-variable 'eim-handle-function)
  (make-local-variable 'eim-invalid-function)
  (make-local-variable 'eim-validate-function)
  (make-local-variable 'eim-stop-length)
  (make-local-variable 'eim-page-length)
  (make-local-variable 'eim-mode-map)
  (make-local-variable 'eim-start)
  (make-local-variable 'eim-end)
  (make-local-variable 'eim-choices)
  (make-local-variable 'eim-select-pos)
  (make-local-variable 'eim-display-string)
  (make-local-variable 'eim-choices-string)
  (make-local-variable 'eim-current-string)
  (make-local-variable 'eim-activate)
  (make-local-variable 'eim-overlay)
  (make-local-variable 'eim-curse-overlay)
  (make-local-variable 'eim-completions)
  (make-local-variable 'eim-inactivate-hook)
  (make-local-variable 'eim-activate-hook)
  (make-local-variable 'eim-current-package)
  (make-local-variable 'inactivate-current-input-method-function)
  (setq inactivate-current-input-method-function 'eim-inactivate)
  (make-local-variable 'describe-current-input-method-function)
  (setq describe-current-input-method-function 'eim-help)
  ;; if package-name not in eim-packages, load lib. Otherwise set
  ;; variable from eim-packages and eim-default-setting
  (if (assoc package-name eim-packages)
      (apply 'eim-install-package (assoc package-name eim-packages))
    (load (car libraries))
    (run-hooks eim-after-load-hook)
    (eim-load-history))
  (setq eim-current-package (assoc package-name eim-packages))
  (let ((params (eim-params)))
    (dolist (var eim-default-setting)
      (if (not (assoc (car var) params))
          (set (car var) (cdr var)))))
  (eim-reset)
  (add-hook 'after-change-functions 'eim-after-change nil t)
  (run-hooks 'eim-activate-hook))

;;; The setting to the method
;;;###autoload
(defun eim-lower-case-letterp (char)
  "Default `eim-validate-function'"
  (let ((c (string-to-char char)))
    (and (> c 96) (< c 123))))

;; this is not good. because `eim-select-current' function maybe diffent
(defun eim-punctuation-handle-function (start end)
  "Handle punctuation"
  (let ((c (buffer-substring-no-properties start end))
        punc)
    (when eim-activate
      (eim-select-current)
      (forward-char 1))
    (cond ((setq punc (assoc c eim-punctuation-list))
           (delete-backward-char 1)
           (insert (cdr punc)))
          ((setq punc (assoc c eim-quote-punctuation))
           (delete-backward-char 1)
           (if (cadr punc)
               (progn
                 (setcar (cdr punc) nil)
                 (insert (nth 3 punc)))
             (setcar (cdr punc) t)
             (insert (nth 2 punc))))
          ((string= c "\\")
           (delete-backward-char 1)
           (call-interactively 'eim-insert-ascii)))))

;; this is the whole of the eim
(defun eim-after-change (start end length)
  ;; if it is user typing
  (if (and (= (- end start) 1)
           (= length 0))
      (if eim-activate
          ;; if active
          ;; 判断插入的位置。如果插入的位置不在 eim-start 和 eim-end 之间，退出
          (progn
            (if (and (<= eim-start start) (<= end (1+ eim-end)))
                (let ((char (buffer-substring-no-properties start end)))
                  (if (funcall eim-validate-function char)
                      (progn
                        (setq eim-end (1+ eim-end))
                        (funcall eim-handle-function))
                    (funcall eim-invalid-function start end)))
              (eim-reset)))
        ;; if not active
        (let ((char (buffer-substring-no-properties start end)))
          (if (funcall eim-validate-function char)
              (progn
                (setq eim-activate t)
                (setq eim-start start)
                (setq eim-end end)
                (funcall eim-handle-function))
            (funcall eim-invalid-function start end))))
    ;; if not user typing 
    ;; if ime activate and is a deletion, need to update
    (when eim-activate
      (if (and (= start end) (= length 1) ; a deletion
               (<= eim-start start) (< end eim-end))
          (progn
            (setq eim-end (1- eim-end))
            (funcall eim-handle-function))
        (eim-reset)))))

(defun eim-update-display ()
  "Show `eim-display-string' and `eim-choices-string'"
  (if eim-curse-overlay (delete-overlay eim-curse-overlay))
  (when (eobp)
    (let (after-change-functions)
      (insert (propertize " " 'eim-ime t))
      (backward-char 1)))
  (setq eim-curse-overlay (make-overlay (1- (point)) (1+ (point))))
  ;; the next line is for debug
  ;; (overlay-put eim-curse-overlay 'face '(:background "yellow"))
  (overlay-put eim-curse-overlay 'keymap eim-mode-map)
  (if eim-overlay (delete-overlay eim-overlay))
  (setq eim-overlay (make-overlay eim-start eim-end))
  (overlay-put eim-overlay 'display eim-display-string)
  (overlay-put eim-overlay 'face 'underline)
  ;; (overlay-put eim-overlay 'face '(:foreground "red" underline))
  (message eim-choices-string))

(defun eim-help (&optional package)
  "Show input method docstring"
  (let ((docstr (concat "Input method: " (eim-package-name) "\n"
                        (eim-docstring) "\n\n"
                        (substitute-command-keys "\\{eim-mode-map}"))))
    (with-current-buffer (help-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert docstr)
        (run-hooks 'temp-buffer-show-hook))
      (display-buffer (current-buffer)))))

;;; 下面的函数可能只能针对少数输入法有效
;; how to do with the input string
(defun eim-default-handle ()
  "The default handle set `eim-choices-string' and `eim-display-string'.
When arrive the `eim-stop-length', insert current `eim-display-string', leaving
the last input character."
  (let (str message-log-max)            ; inhibit message log
    ;; if arrive the eim-stop-length, insert current diplay string,
    ;; leaving the last input character
    (if (> (- eim-end eim-start) eim-stop-length)
        (let (after-change-functions)
          (goto-char eim-start)
          (delete-region eim-start (1- eim-end))
          (insert eim-display-string)
          (setq eim-start (point))
          (forward-char 1)
          (setq eim-end (point))))
    (setq eim-current-string (buffer-substring-no-properties eim-start eim-end))
    (setq str eim-current-string)
    ;; if current string is more than 1 charater, the completion will find
    (when (> (length str) 1)
      (setq eim-completions (cdr (sort (all-completions str eim-database)
                                       'string<)))
      (setq eim-completion-pos 0))
    (if (string< "" str)                ; 这个判断是针对把所有字符全部删除的情况
        (progn
          (if (setq eim-choices (mapcar (lambda (w) (list w)) (eim-get str)))
              (eim-update-pos (get (intern str eim-database) 'pos))
            (if (and (> (length str) 1) eim-completions)
                (setq eim-choices-string (format "%s[%s] " str (eim-next-char)))
              (setq eim-choices-string (format "%s[]" str)))
            (setq eim-display-string ""))
          (eim-update-display))
      (setq eim-display-string "")
      (eim-select-current))))

(defun eim-next-char ()
  "The possible next char in the `eim-completions'"
  (let (chars
        (len (length eim-current-string)))
    (dolist (code eim-completions)
      (if chars
          (add-to-list 'chars (substring code len (1+ len)))
        (setq chars (list (substring code len (1+ len))))))
    (mapconcat 'identity (sort chars 'string<) "")))

(defun eim-update-pos (pos)
  "Change `eim-display-string' and `eim-choices-string' to the
nth POS of `eim-choices'"
  (let ((str (buffer-substring-no-properties eim-start eim-end))
        (mes (eim-format-choice eim-choices pos)))
    (when (string< "" mes)
      (setq eim-choices-string (format "%s: %s" str mes)
            eim-display-string (car (nth (1- eim-select-pos) eim-choices))))
    (eim-update-display)))

(defun eim-format-choice (choices &optional pos)
  "format the current choice to string"
  (let ((len eim-page-length)
        (mes "")
        (i 1) base
        newlist)
    (or pos (setq pos 1))
    (setq base (eim-mod pos len))
    (setq newlist (eim-get-page pos))
    (when newlist
      (setq eim-select-pos pos)
      (dolist (c newlist)
        (let ((char (car c))
              (code (if (cdr c)
                        (substring (cdr c) (length eim-current-string))
                      "")))
          (setq mes (format "%s %d.%s%s" mes i
                            (if (= i base) (propertize char 'face 'underline) char)
                            code)
                i (1+ i)))))
    mes))

;;;###autoload
(defun eim-get-page (pos)
  "get the page of the POS"
  (let* ((whole (length eim-choices))
         (len eim-page-length)
         (last (+ (- pos (eim-mod pos len)) len))
         (comp eim-completions)
         (all (length comp))
         code word)
    ;; if not enough, get from completion
    (while (and (< whole last)
                (< eim-completion-pos all))
      (setq word (eim-get (setq code (nth eim-completion-pos comp)))
            eim-completion-pos (1+ eim-completion-pos))
      (mapcar (lambda (w)
                (if (and (= (length w) 1)
                         (not (assoc w eim-choices)))
                    (add-to-list 'eim-choices (cons w code) t)))
              word)
      (setq whole (length eim-choices)))
    (eim-subseq eim-choices (- last len) last)))

;;;###autoload
(defun eim-load-history ()
  (when (and eim-history-file (file-exists-p eim-history-file))
    (let ((buf (find-file-noselect eim-history-file))
          s word)
      (with-current-buffer buf
        (goto-char (point-min))
        (while (not (eobp))
          (setq word (split-string (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
          (if (setq s (intern-soft (car word) eim-database))
              (put s 'pos (string-to-number (cadr word))))
          (forward-line 1)))
      (kill-buffer buf))))

;;;###autoload
(defun eim-save-history ()
  (dolist (package eim-packages)
    (apply 'eim-install-package package)
    (when (and eim-history-file (file-exists-p eim-history-file))
      (let ((buf (find-file-noselect eim-history-file)))
        (message "Save history to %s..." eim-history-file)
        (with-current-buffer buf
          (erase-buffer)
          (mapatoms (lambda (s)
                      (let ((pos (get s 'pos)))
                        (if pos
                            (insert (format "%s %d\n" (symbol-name s) pos)))))
                    eim-database)
          (save-buffer)
          (kill-buffer (current-buffer)))
        (message "Save history to %s... done" eim-history-file)))))

(add-hook 'kill-emacs-hook 'eim-save-history)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
;;;###autoload
(defun eim-insert-ascii ()
  (interactive)
  (let (after-change-functions)
    (insert (read-from-minibuffer ""))))

;;;###autoload
(defun eim-select ()
  (interactive)
  (let ((index (- last-input-char 48)))
    (setq eim-select-pos
          (+ index
             (* eim-page-length
                (/ (1- eim-select-pos) eim-page-length))))
    (eim-update-pos eim-select-pos)))

;;;###autoload
(defun eim-select-current ()
  (interactive)
  (let (after-change-functions
        (str (buffer-substring-no-properties eim-start eim-end)))
    (when eim-start
      (goto-char eim-start)
      (delete-region eim-start eim-end)
      (if eim-overlay (delete-overlay eim-overlay))
      (insert eim-display-string)
      (if (and eim-history-file (string< "" str))
          (put (intern-soft str eim-database) 'pos eim-select-pos)))
    (when (get-char-property (point) 'eim-ime)
      (delete-char 1))
    (eim-reset)))

;;;###autoload
(defun eim-next-page (arg)
  (interactive "p")
  (let* ((len eim-page-length)
         (pos eim-select-pos)
         (newpos (+ pos (* len arg) 1))
         newlist)
    (setq pos (- newpos (eim-mod pos len)))
    (if (> pos 0)
        (eim-update-pos pos)
      (message eim-choices-string))))

;;;###autoload
(defun eim-prev-page (arg)
  (interactive "p")
  (eim-next-page (- arg)))

;;;###autoload
(defun eim-clear-select ()
  "Erase current input"
  (interactive)
  (let (after-change-functions)
    (if eim-start
        (delete-region eim-start eim-end))
    (eim-reset)))

;;;###autoload
(defun eim-no-clear-select ()
  "Insert current input"
  (interactive)
  (let (after-change-functions)
    (when eim-start
      (if eim-overlay (delete-overlay eim-overlay))
      (if eim-curse-overlay (delete-overlay eim-curse-overlay)))
    (eim-reset)))

;;; debug functions
(defun eim-reset-vars ()
  (setq eim-validate-function (lambda (char)
                                (let ((c (string-to-char char)))
                                  (and (> c 96) (< c 123)))))
  (setq eim-invalid-function 'eim-punctuation-handle-function)
  (setq eim-handle-function 'eim-default-handle)
  (setq eim-mode-map eim-default-map))

(defvar eim-debug nil)
(defun eim-dprintf (fmt &rest args)
  (if eim-debug
      (apply 'message fmt args)))

(provide 'eim)
;;; eim.el ends here
