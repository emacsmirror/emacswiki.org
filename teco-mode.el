;;; teco-mode.el --- Emacs mode for TECO macro -*- lexical-binding: t; -*-
;; Copyright (C) 2024 LdBeth
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; If not, see <https://www.gnu.org/licenses/>.
;;
;; This teco mode mainly targets TECO-64 which has slightly different
;; implementation on how to read in commands compared to traditional
;; tecoes.
(eval-when-compile
  (require 'rx))

(defgroup teco nil
  "A mode for editing TECO macros"
  :group 'languages
  :prefix "teco-")

(defcustom teco-atsign-use-braces 'ignore
  "Controls if @ delimiters use special treatment for braces.
 This is for TECO-64 when E1&4 is set. If set to `ignore', braces
 enclosed string are not font-locking. Set to `nil' to follow
 traditional TECO convention. When the value is true, whenever
 thedelimiter after @ modified command is brace the other
 delimiter must be paired."
  :type 'symbol
  :group 'teco)
(put 'teco-atsign-use-braces 'safe-local-variable #'symbolp)

(defcustom teco-atsign-ignore-spaces t
  "Controls if @ delimiters ignores whitespaces.
TECO-64 looks for non whitespaces."
  :type 'boolean
  :group 'teco)
(put 'teco-atsign-ignore-spaces 'safe-local-variable #'booleanp)

(defconst teco-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?! "! 12b" table)

    (modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?\' "." table)
    (modify-syntax-entry ?\[ "." table) ; Q-reg push
    (modify-syntax-entry ?\] "." table) ; Q-reg pop
    (modify-syntax-entry ?*  "." table)
    (modify-syntax-entry ?/  "." table)
    (modify-syntax-entry ?&  "."  table)
    (modify-syntax-entry ?#  "."  table)
    (modify-syntax-entry ?+  "."  table)
    (modify-syntax-entry ?-  "."  table)
    (modify-syntax-entry ?*  "."  table)
    (modify-syntax-entry ?.  "."  table)
    (modify-syntax-entry ?:  "."  table)
    (modify-syntax-entry ?@  "."  table)
    (modify-syntax-entry ?=  "."  table)
    (modify-syntax-entry ?<  "."  table)
    (modify-syntax-entry ?>  "."  table)
    (modify-syntax-entry ?|  "."  table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(eval-when-compile
  (rx-define teco-rx-regiser
    (seq (? ".") (any "A-Z0-9")))
  (rx-define teco-rx-atsign-prefix
    (seq "@" (* (any "\s\t\n\r,:0-9"))))
  (rx-define teco-rx-atsign-1-arg
    (or (seq "E" (or (any "%BGILNRW_")
                     (seq (any "QM") teco-rx-regiser)))
        (regex "F[BDMKR]")
        "^A"
        (any "\C-a=!INOS_")
        (seq (or "^U" "\C-u") teco-rx-regiser)))
  (rx-define teco-rx-1-arg
     (or (seq "E" (or (any "%BGILNRW_")
                      (seq (any "QM") teco-rx-regiser)))
         (regex "F[BDMKR]")
         (any "=INOS_")
         (seq (or "^U" "\C-u") teco-rx-regiser)))
  (rx-define teco-rx-atsign-2-arg
    (seq "F" (any "1-4CNS_"))))

(defun teco-font-lock-delim (group)
  (let* ((beg (match-beginning group))
         (end (match-end group))
         (ppss (syntax-ppss end)))
    (unless (eql (char-after (1- beg)) ?^)
      (unless (nth 4 ppss)
        (compose-region beg end ?$ 'decompose-region))
      font-lock-keyword-face)))

(defmacro teco-font-lock-define-matcher (fn cmds delim)
  `(defalias ',fn
     (lambda (limit)
       (when (re-search-forward (rx (group ,cmds)
                                    (group (* (not ,delim)))
                                    ,delim)
                                limit t)
         (let ((beg (match-beginning 0))
               (end (match-end 0))
               (pair (cons (match-string-no-properties 1)
                           ,delim)))
           (put-text-property (match-beginning 2) (match-end 2)
                              'teco-delim-pair pair)
           (put-text-property beg end 'font-lock-multiline 't)
           (goto-char end))))))

(teco-font-lock-define-matcher
 teco-font-lock-control-out (or "^A" "\C-a") "\C-a")
(teco-font-lock-define-matcher
 teco-font-lock-1-arg teco-rx-1-arg "\e")

(defun teco-font-lock-2-arg (limit)
  (when (re-search-forward (rx (group teco-rx-atsign-2-arg)
                               (group (* (not "\e")))
                               "\e"
                               (group (* (not "\e"))))
                           limit t)
    (let ((beg (match-beginning 0))
          (end (match-end 0))
          (pair1 (cons (match-string-no-properties 1)
                       "\e"))
          (pair2 (cons "\e" "\e")))
      (put-text-property (match-beginning 2) (match-end 2)
                         'teco-delim-pair pair1)
      (put-text-property (match-beginning 3) (match-end 3)
                         'teco-delim-pair pair2)
      (put-text-property beg end 'font-lock-multiline 't)
      (goto-char end))))

(defvar teco-font-lock-keywords
  `((teco-font-lock-control-out (2 font-lock-string-face))
    (teco-font-lock-1-arg (2 font-lock-string-face))
    (teco-font-lock-2-arg (2 font-lock-string-face) (3 font-lock-string-face))
    ("[:<>=]?==?\\|<>\\|//\\|<<\\|>>\\|\\^_\\|\\\\/"
     (0 'font-lock-operator-face))
    ("[#&*+/!~:@-]" (0 'font-lock-operator-face))
    ("F?['<>|]\\|:?;" (0 font-lock-keyword-face))
    ("\e\\|\\^\\[" 0 (teco-font-lock-delim 0) prepend)
    ("\\^[][_\\@A-Za-z]" 0 font-lock-constant-face prepend)
    ("\\^\\^." 0 'font-lock-preprocessor-face prepend)
    (,(rx (or "^U"
              (any "[]UXQGM*%\C-u"))
          (group teco-rx-regiser))
     (1 font-lock-function-name-face))
    ("\"[ACDEFGLNRSTUVW<>=]" (0 font-lock-keyword-face))
    ("F[1-4BCDKMNRS_]" (0 font-lock-builtin-face))
    ("E[ABCFGIJKLMNPQRWXYZ%_]" (0 font-lock-builtin-face))
    ("E[1-4DEHOSTUV]" (0 'font-lock-variable-name-face))
    ("F?[HZ]\\|[B.]\\|F0" (0 'font-lock-variable-name-face))
    ))

(defun teco-mode-syntax-propertize (start end)
  (goto-char start)
  (let (ppss string-start)
    (while (setq ppss (syntax-ppss)
                 string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
      (goto-char string-start)
      (search-backward "@" nil t)))
  (while (and (< (point) end)
              (re-search-forward (rx (or (seq teco-rx-atsign-prefix
                                              (group teco-rx-atsign-1-arg))
                                         (seq teco-rx-atsign-prefix
                                              (group teco-rx-atsign-2-arg))))
                                 end t))
    (when (save-excursion
            (null (nth 4 (syntax-ppss (match-beginning 0)))))
      (let* ((cmd (match-beginning 1))
             (delimiter
              (if teco-atsign-ignore-spaces
                  (prog1 (and (looking-at "[\s\t\n\r]*\\([^\C-@]\\)")
                              (char-after (match-beginning 1)))
                    (goto-char (match-beginning 1)))
                (char-after (point))))
             (string-start (point)))
        (unless (eq t (nth 3 (syntax-ppss)))
          (when (and cmd (eql (char-after cmd) ?!))
            ;; Cancel syntax class of `!'
            (put-text-property cmd (1+ cmd) 'syntax-table '(1)))
          (if (and (eq teco-atsign-use-braces 'ignore)
                   (eql delimiter ?{))
              (setq delimiter nil))
        (setq cmd (if cmd 1 2))
        (when delimiter
          (put-text-property string-start (1+ string-start) 'syntax-table
                             '(15))
          (let ((search
                 (cond
                  ((and teco-atsign-use-braces (eql delimiter ?\[))
                   "]")
                  ((and teco-atsign-use-braces (eql delimiter ?{))
                   "}")
                  (t
                   (string delimiter)))))
            (forward-char)
            (when (search-forward search end t cmd)
              (put-text-property (1- (point)) (point) 'syntax-table
                                 '(15))))))))))

(defun teco-fontify-extend-region (beg end _old-len)
  (let ((str-beg (car (get-text-property beg 'teco-delim-pair)))
        (str-end (cdr (get-text-property end 'teco-delim-pair)))
        case-fold-search)
    (save-excursion
      (goto-char beg)
      (cons (or (and str-beg (search-backward str-beg nil t)) beg)
            (or (and str-end (search-forward str-end nil t)) end)))))

(defun teco-font-lock-syntactic-face-function (state)
  "Function for detection of string vs. Comment."
  (let ((start-pos (nth 8 state)))
    (cond
     ((nth 3 state)
      (if (save-excursion
            (goto-char start-pos)
            (search-backward "@" nil t)
            (looking-at-p (rx teco-rx-atsign-prefix "!")))
          font-lock-comment-face
        font-lock-string-face))
     (t
      font-lock-comment-face))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.te[sc]\\'"  . teco-mode))
;;;###autoload
(add-to-list 'file-coding-system-alist '("\\.te[sc]\\'"  . utf-8))

;;;###autoload
(define-derived-mode teco-mode prog-mode "TECO"
  :syntax-table teco-mode-syntax-table
  (setq font-lock-defaults '(teco-font-lock-keywords
                             nil t nil nil
                             (font-lock-syntactic-face-function
                              . teco-font-lock-syntactic-face-function))
        font-lock-multiline t
        tab-width 8)
  (setq-local comment-start "! "
              comment-end " !"
              syntax-propertize-function #'teco-mode-syntax-propertize
              font-lock-extend-after-change-region-function
              #'teco-fontify-extend-region
              indent-line-function #'insert-tab
              electric-indent-mode nil))
