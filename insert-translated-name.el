;;; insert-translated-name.el --- Insert translated string as variable or function name  -*- lexical-binding: t; -*-

;; Filename: insert-translated-name.el
;; Description: Insert translated string as variable or function name
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-22 10:54:16
;; Version: 1.2
;; Last-Updated: 2018-09-26 13:44:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/insert-translated-name.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;;
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
;; Insert translated string as variable or function name
;;

;;; Installation:
;;
;; Put insert-translated-name.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'insert-translated-name)
;;
;; No need more.

;;; Customize:
;;
;; `insert-translated-name-translate-engine'
;; `insert-translated-name-line-style-mode-list'
;; `insert-translated-name-underline-style-mode-list'
;; `insert-translated-name-camel-style-mode-list'
;; `insert-translated-name-font-lock-mark-word'
;;

;;; Change log:
;;
;; 2018/09/26
;;      * Add `insert-translated-name-use-original-translation'.
;;      * Nothing happen if input word is empty.
;;      * Make `insert-translated-name-insert' support prefix arg.
;;
;; 2018/09/25
;;      * Add `insert-translated-name-in-commit-buffer-p' option to make english assistants available in magit.
;;      * Make english assistants available in minibuffer.
;;
;; 2018/09/24
;;      * Add option `insert-translated-name-translate-engine' and default use Google.
;;      * Support pyim now.
;;
;; 2018/09/23
;;      * Store placeholder in buffer local's hash instead insert placeholder uuid in buffer.
;;      * Make `insert-translated-name-replace-*' functions support region.
;;      * Call `insert-translated-name-insert-original-translation' when cursor in comment or string.
;;
;; 2018/09/22
;;      * First released.
;;      * Change query translation asynchronous, don't insert buffer if query duration more than 2 seconds.
;;      * Use overlay as input way, and add `insert-translated-name-replace-*' functions.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

;;; Code:

;;;;;;;;;;;;;;;;;;;;; Customize options ;;;;;;;;;;;;;;;;;;;;;
(defgroup insert-translated-name nil
  "Search and refacotry code base on ripgrep."
  :group 'insert-translated-name)

(defface insert-translated-name-font-lock-mark-word
  '((t (:foreground "White" :background "SystemBlueColor" :bold t)))
  "Face for keyword match."
  :group 'insert-translated-name)

(defvar insert-translated-name-line-style-mode-list
  '(web-mode emacs-lisp-mode))

(defvar insert-translated-name-camel-style-mode-list
  '(js-mode))

(defvar insert-translated-name-underline-style-mode-list
  '(ruby-mode))

(defvar insert-translated-name-translate-engine "google"
  "The translate engine can use \"google\" or \"youdao\".")

;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-insert (arg)
  (interactive "p")
  (if (or
       (equal arg 4)
       (and (boundp 'insert-translated-name-original-translation)
            insert-translated-name-original-translation)
       (insert-translated-name-in-string-p)
       (insert-translated-name-in-comment-p)
       (insert-translated-name-in-commit-buffer-p)
       (minibuffer-window-active-p (get-buffer-window)))
      (insert-translated-name-insert-original-translation)
    (insert-translated-name-active
     (cond ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
            "line")
           ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
            "camel")
           ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
            "underline")
           (t
            "underline")))))

(defun insert-translated-name-insert-original-translation ()
  (interactive)
  (insert-translated-name-active "comment"))

(defun insert-translated-name-insert-with-line ()
  (interactive)
  (insert-translated-name-active "line"))

(defun insert-translated-name-insert-with-underline ()
  (interactive)
  (insert-translated-name-active "underline"))

(defun insert-translated-name-insert-with-camel ()
  (interactive)
  (insert-translated-name-active "camel"))

(defun insert-translated-name-replace ()
  (interactive)
  (insert-translated-name-replace-symbol
   (cond ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
          "line")
         ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
          "camel")
         ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
          "underline")
         (t
          "underline"))))

(defun insert-translated-name-replace-with-line ()
  (interactive)
  (insert-translated-name-replace-symbol "line"))

(defun insert-translated-name-replace-with-underline ()
  (interactive)
  (insert-translated-name-replace-symbol "underline"))

(defun insert-translated-name-replace-with-camel ()
  (interactive)
  (insert-translated-name-replace-symbol "camel"))

;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-replace-symbol (style)
  (let ((word (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (thing-paste-symbol))
    (insert-translated-name-query-translation word style)))

(defun insert-translated-name-match-modes (mode-list)
  (cl-remove-if 'null (mapcar #'(lambda (mode) (derived-mode-p mode)) mode-list)))

(defun insert-translated-name-use-original-translation ()
  (set (make-local-variable 'insert-translated-name-original-translation) t))

(defun insert-translated-name-active (style)
  ;; Enable pyim if user has load it.
  (when (featurep 'pyim)
    (activate-input-method "pyim"))

  ;; Add monitor hook.
  (add-hook 'after-change-functions 'insert-translated-name-monitor-after-change nil t)
  (add-hook 'pre-command-hook #'insert-translated-name-monitor-pre-command)

  ;; Make sure build hash to contain placeholder.
  (unless (boundp 'insert-translated-name-placeholder-hash)
    (set (make-local-variable 'insert-translated-name-placeholder-hash) (make-hash-table :test 'equal)))

  ;; Make sure clean active overlay first.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Reset active local variables
  (set (make-local-variable 'insert-translated-name-active-point) (point))
  (set (make-local-variable 'insert-translated-name-active-style) style)
  (set (make-local-variable 'insert-translated-name-active-overlay) (make-overlay (point) (point)))

  ;; Active new overlay from current point.
  (overlay-put insert-translated-name-active-overlay 'face 'insert-translated-name-font-lock-mark-word)

  ;; Print play hint.
  (if (string-equal insert-translated-name-active-style "comment")
      (message "Type Chinese and press SPACE to translate, type TAB to stop translate.")
    (message "Type Chinese and press SPACE to translate.")))

(defun insert-translated-name-inactive (&optional keep-style)
  (interactive)
  ;; Disable pyim if user has load it.
  (when (featurep 'pyim)
    (deactivate-input-method))

  ;; Delete active overlay.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Clean active local variables.
  (set (make-local-variable 'insert-translated-name-active-point) nil)
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (set (make-local-variable 'insert-translated-name-active-overlay) nil))

  ;; Clean style.
  (unless keep-style
    (set (make-local-variable 'insert-translated-name-active-style) nil)))

(defun insert-translated-name-monitor-pre-command ()
  (when (and (boundp 'insert-translated-name-active-point))
    (let* ((event last-command-event)
           (key (make-vector 1 event))
           (key-desc (key-description key)))
      (cond
       ;; End translate if press TAB with comment style.
       ((and (string-equal insert-translated-name-active-style "comment")
             (equal key-desc "TAB"))
        ;; Inactive
        (insert-translated-name-inactive nil)

        (message "End translate comment."))))))

(defun insert-translated-name-monitor-after-change (start end len)
  (when (and (boundp 'insert-translated-name-active-point))
    (if insert-translated-name-active-point
        (cond
         ;; Translate current Chinese words after press SPACE.
         ((string-equal (buffer-substring-no-properties start end) " ")
          (let ((word (buffer-substring-no-properties insert-translated-name-active-point (- (point) 1))))
            ;; Delete Chinese words.
            (kill-region insert-translated-name-active-point (point))

            ;; Query translation.
            (insert-translated-name-query-translation word insert-translated-name-active-style)

            ;; Inactive.
            (insert-translated-name-inactive t)
            ))
         ;; Update active overlay bound if user press any other non-SPACE character.
         (t
          (move-overlay insert-translated-name-active-overlay insert-translated-name-active-point (point))))
      ;; Continue translate if styel is comment and press SPACE.
      (when (string-equal insert-translated-name-active-style "comment")
        (let ((insert-char (buffer-substring-no-properties start end)))
          (when (string-equal insert-char " ")
            (insert-translated-name-active "comment")
            ))))))

(defun insert-translated-name-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun insert-translated-name-in-commit-buffer-p ()
  (and (string-equal (buffer-name) "COMMIT_EDITMSG")
       (save-excursion
         (goto-char (point-min))
         (search-forward-regexp "#\\s-Please\\s-enter\\s-the\\s-commit\\s-message\\s-for\\s-your\\s-changes." nil t))))

(defun insert-translated-name-in-string-p (&optional state)
  "True if the parse state is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  (and (nth 3 (or state (insert-translated-name-current-parse-state)))
       t))

(defun insert-translated-name-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 4. nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state (insert-translated-name-current-parse-state)))
       t))

(defun insert-translated-name-convert-translation (translation style)
  (let ((words (split-string translation " ")))
    (cond ((string-equal style "line")
           (string-join (mapcar 'downcase words) "-"))
          ((string-equal style "underline")
           (string-join (mapcar 'downcase words) "_"))
          ((string-equal style "camel")
           (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words)))))
          ((string-equal style "comment")
           translation))))

(defun insert-translated-name-update-translation-in-buffer (word style translation insert-buffer placeholder)
  (let ((result (insert-translated-name-convert-translation translation style)))
    (save-excursion
      (with-current-buffer insert-buffer
        (let ((placeholder-point (gethash placeholder insert-translated-name-placeholder-hash)))
          (if placeholder-point
              (progn
                ;; Insert result at placeholder point .
                (goto-char placeholder-point)
                (insert result)

                ;; Remove placeholder from hash.
                (remhash placeholder insert-translated-name-placeholder-hash))
            (message (format "Something wrong that we can't found placeholder for %s: %s" word translation))))))))

(defun insert-translated-name-generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(defun insert-translated-name-query-translation (word style)
  (if (string-equal word "")
      (message "Nothing input, cancel translate.")
    (let ((placeholder (insert-translated-name-generate-uuid)))
      ;; Store placeholder in hash.
      (puthash placeholder (point) insert-translated-name-placeholder-hash)

      ;; Query translation.
      (insert-translated-name-retrieve-translation word style placeholder)
      )))

(defun insert-translated-name-retrieve-translation (word style placeholder)
  (cond ((string-equal insert-translated-name-translate-engine "youdao")
         (url-retrieve
          (insert-translated-name-youdao-build-url word)
          'insert-translated-name-youdao-retrieve-callback
          (list word style (current-buffer) placeholder)))
        ((string-equal insert-translated-name-translate-engine "google")
         (url-retrieve
          (insert-translated-name-google-build-url word)
          'insert-translated-name-google-retrieve-callback
          (list word style (current-buffer) placeholder)))
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;; Google API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst insert-translated-name-google-api-url
  "http://translate.google.cn/translate_a/single")

(defconst insert-translated-name-google-translate-bit-v-len 32)

(defun insert-translated-name-google-retrieve-callback (&optional redirect word style insert-buffer placeholder)
  (let (json word translation result)
    ;; Get translation.
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward (format "\n\n"))
    (delete-region (point-min) (point))
    (prog1
        (setq translation (elt (elt (elt (json-read-from-string (buffer-string)) 0) 0) 0))
      (kill-buffer))

    ;; Insert result with placeholder point.
    (insert-translated-name-update-translation-in-buffer word style translation insert-buffer placeholder)))

(defun insert-translated-name-google-translate-prepare-text-for-request (text)
  "Make TEXT as clean as possible berofe sending it in the
request."
  (insert-translated-name-google-translate-trim-string
   (insert-translated-name-google-translate-strip-string text)))

(defun insert-translated-name-google-translate-trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

(defun insert-translated-name-google-translate-strip-string (string)
  "Replace spaces, tabs, line feeds (ASCII 10) and carridge
returns (ASCII 13) by a single space symbol."
  (replace-regexp-in-string "[[:space:]\n\r]+" " " string))

(defun insert-translated-name-google-build-url (text)
  (insert-translated-name-google-translate-format-request-url
   `(("client" . "t")
     ("ie"     . "UTF-8")
     ("oe"     . "UTF-8")
     ("sl"     . "zh-CN")
     ("tl"     . "en")
     ("q"      . ,text)
     ("dt"     . "bd")
     ("dt"     . "ex")
     ("dt"     . "ld")
     ("dt"     . "md")
     ("dt"     . "qc")
     ("dt"     . "rw")
     ("dt"     . "rm")
     ("dt"     . "ss")
     ("dt"     . "t")
     ("dt"     . "at")
     ("pc"     . "1")
     ("otf"    . "1")
     ("srcrom" . "1")
     ("ssel"   . "0")
     ("tsel"   . "0")
     ("tk"     . ,(insert-translated-name-google-translate-gen-tk text)))))

(defun insert-translated-name-google-translate-format-request-url (query-params)
  "Format QUERY-PARAMS as a Google Translate HTTP request URL.

QUERY-PARAMS must be an alist of field-value pairs."
  (concat insert-translated-name-google-api-url
          "?"
          (insert-translated-name-google-translate-format-query-string query-params)))

(defun insert-translated-name-google-translate-gen-tk (text &optional b-d1)
  (setq b-d1 (or b-d1 (insert-translated-name-google-translate-get-b-d1)))
  (let* ((b (cl-first b-d1))
         (d1 (cl-second b-d1))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-reduce (lambda (a e) (insert-translated-name-google-translate-gen-rl (+ a e) vb))
                       (encode-coding-string text 'utf-8) :initial-value b)))
    (setq a (insert-translated-name-google-translate-gen-rl a ub))
    (setq a (insert-translated-name-google-translate-logxor a d1))
    (when (< a 0) ;; (abs a) + 2^31
      (setq a (+ (insert-translated-name-google-translate-logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string (insert-translated-name-google-translate-logxor a b)) "\\.")))))

(defun insert-translated-name-google-translate-get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

(defun insert-translated-name-google-translate-gen-rl (a b)
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (insert-translated-name-google-translate-lsh a (- d))
                     (insert-translated-name-google-translate-lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (insert-translated-name-google-translate-logand (+ a d) 4294967295.0)
                     (insert-translated-name-google-translate-logxor a d))))
  a)

(defun insert-translated-name-google-translate-lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (insert-translated-name-google-translate-number-to-bit-v n))
        (v-result (make-vector insert-translated-name-google-translate-bit-v-len 0)))
    (if (< d 0) ;; Shift Right Logical
        ;; [x0 x1 ... xn-d ... xn] => [0 ... 0 x0 x1 ... xn-d]
        (cl-loop for i from (abs d) below insert-translated-name-google-translate-bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      ;; [x0 x1 ... xd ... xn] => [xd ... xn 0 ... 0]
      (cl-loop for i from d below insert-translated-name-google-translate-bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (insert-translated-name-google-translate-bit-v-to-number v-result)))

(defun insert-translated-name-google-translate-number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (insert-translated-name-google-translate-bit-v-2comp
               (insert-translated-name-google-translate-number-to-bit-v (abs n)))
    (let ((v (make-vector insert-translated-name-google-translate-bit-v-len 0)))
      (cl-loop for i downfrom (1- insert-translated-name-google-translate-bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun insert-translated-name-google-translate-bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below insert-translated-name-google-translate-bit-v-len
                    thereis (> (aref v i) 0)))
      (- (insert-translated-name-google-translate-bit-v-to-number (insert-translated-name-google-translate-bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun insert-translated-name-google-translate-logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (insert-translated-name-google-translate-logfn #'logand n1 n2))

(defun insert-translated-name-google-translate-logfn (fn n1 n2)
  "Helper function for logical FN."
  (let ((v1 (insert-translated-name-google-translate-number-to-bit-v n1))
        (v2 (insert-translated-name-google-translate-number-to-bit-v n2))
        (v (make-vector insert-translated-name-google-translate-bit-v-len 0)))
    (cl-loop for i from 0 below insert-translated-name-google-translate-bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (insert-translated-name-google-translate-bit-v-to-number v)))

(defun insert-translated-name-google-translate-logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (insert-translated-name-google-translate-logfn #'logxor n1 n2))

(defun insert-translated-name-google-translate-bit-v-2comp (v)
  "Return the two's complement of V."
  (let* ((vc (vconcat v))
         (len (length vc)))
    ;; Complement of v
    (cl-loop for i from 0 below len do
             (aset vc i (logxor (aref vc i) 1)))
    ;; vc = complement of v + 1
    (cl-loop for i downfrom (1- len) to 0
             do (aset vc i (logxor (aref vc i) 1))
             when (> (aref vc i) 0) return nil)
    vc))

(defun insert-translated-name-google-translate-format-query-string (query-params)
  "Format QUERY-PARAMS as a query string.

QUERY-PARAMS must be an alist of field-value pairs."
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

;;;;;;;;;;;;;;;;;;;;;;;;;; Youdao API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst insert-translated-name-youdao-api-url
  "http://fanyi.youdao.com/openapi.do?keyfrom=YouDaoCV&key=659600698&type=data&doctype=json&version=1.1&q=%s"
  "Youdao dictionary API template, URL `http://dict.youdao.com/'.")

(defun insert-translated-name-youdao-build-url (word)
  (format insert-translated-name-youdao-api-url (url-hexify-string word)))

(defun insert-translated-name-youdao-retrieve-callback (&optional redirect word style insert-buffer placeholder)
  (let (json word translation result)
    ;; Get translation.
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (not (string-match "200 OK" (buffer-string)))
      (error "Problem connecting to the server"))
    (re-search-forward "^$" nil 'move)
    (setq json (json-read-from-string
                (buffer-substring-no-properties (point) (point-max))))
    (kill-buffer (current-buffer))
    (setq translation (elt (assoc-default 'translation json) 0))

    ;; Insert result with placeholder point.
    (insert-translated-name-update-translation-in-buffer word style translation insert-buffer placeholder)))

(provide 'insert-translated-name)

;;; insert-translated-name.el ends here
