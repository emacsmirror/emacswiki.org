;;; -*- coding: utf-8 -*-
;;; flydoc.el --- on-the-fly documentation

;; Copyright (C) 2008  MATSUYAMA Tomohiro

;; Author: MATSUYAMA Tomohiro <t.matsuyama.pub@gmail.com>
;; Keywords: convenience

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

;; To use flydoc, put the following code into your ~/.emacs:
;;
;; (require 'flydoc)
;; (flydoc-default-setup)
;; (global-flydoc-mode 1)
;; (global-set-key "\C-ce" 'flydoc-explain)

;; TODO more general
;; TODO remove require
;; TODO documentation
;; TODO multi document support
;; TODO semantic support
;; TODO autoload
;; TODO hightlight (partially done)
;; TODO refactoring
;; TODO celdoc

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup flydoc nil
  ""
  :prefix "flydoc-"
  :group 'convenience)

(defcustom flydoc-documentation-delay 0.5
  ""
  :group 'flydoc)

(defcustom flydoc-mode-hook nil
  ""
  :type 'hook
  :group 'flydoc)

(defcustom flydoc-explain-mode-hook nil
  ""
  :type 'hook
  :group 'flydoc)

 

(defvar flydoc-document-cache (make-hash-table :test 'equal))
(defvar flydoc-documentation-timer (run-with-idle-timer flydoc-documentation-delay t 'flydoc-tip))

(defvar flydoc-documentation-function 'flydoc-default-documentation)
(defvar flydoc-decorate-tip-function nil)
(make-variable-buffer-local 'flydoc-documentation-function)
(make-variable-buffer-local 'flydoc-decorate-tip-function)

(defvar flydoc-explain-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)

    (define-key map "q" 'flydoc-explain-quit)
    map))

 
;;; Utilities

(defun flydoc-trim-string (string)
  (if (string-match "^[ \n\t]*\\(.*?\\)[ \t]*$" string)
      (match-string 1 string)
    string))

(defun flydoc-strip ()
  ;; remove ^H like col -b
  (goto-char (point-min))
  (while (re-search-forward "\b" nil t)
    (backward-delete-char 2))

  (goto-char (point-min))
  ;; the following regexp is borrowed from man.el
  (while (re-search-forward  "\e\\[0?\\([1470]\\|2\\([247]\\)\\)m" nil t)
    (replace-match "")))

(defmacro flydoc-cache (name &rest body)
  (declare (indent defun))
  (let ((namesym (gensym))
        (docsym (gensym)))
    `(let* ((,namesym ,name)
            (,docsym (gethash ,namesym flydoc-document-cache)))
       (if ,docsym
           (if (not (eq ,docsym 'notfound))
               ,docsym)
         (puthash ,namesym (or (progn ,@body) 'notfound) flydoc-document-cache)))))

 
;;; Looking up Manual Pages

(defcustom flydoc-man-key-translation-alist
  '(("名前" . "NAME")
    ("書式" . "SYNOPSIS")
    ("説明" . "DESCRIPTION")
    ("SYNTAX" . "SYNOPSIS"))
  ""
  :type 'alist
  :group 'flydoc)

(defun flydoc-man-translate-key (key)
  (let (result)
    (while
        (progn
          (setq result key)
          (setq key (cdr-safe (assoc key flydoc-man-key-translation-alist)))))
    result))

(defun flydoc-man-lookup-1 (name &optional section)
  (with-temp-buffer
    (if (= (if section
               (call-process "man" nil t nil section name)
             (call-process "man" nil t nil name))
           0)
        (let (begin end key new-key document)
          (flydoc-strip)
          (goto-char (point-min))
          (forward-line)
          (while (re-search-forward "^\\(\\w+\\)$" nil t)
            (setq new-key (match-string 1))
            (setq end (match-beginning 0))
            (if begin
                (push (cons key
                            (buffer-substring-no-properties begin end))
                      document))
            (forward-line)
            (setq begin (point))
            (setq key (flydoc-man-translate-key new-key)))
          (nreverse document)))))

(defun flydoc-man-lookup (name &rest section)
  (flydoc-cache (format "man %s" name)
    (flydoc-man-lookup-1 name section)))

(defun flydoc-man-lookup-command (name)
  (flydoc-cache (format "man 1,1p %s" name)
     (or
      (flydoc-man-lookup-1 name "1")
      (flydoc-man-lookup-1 name "1p"))))

(defun flydoc-man-lookup-c-function (name &optional level)
  (flydoc-cache (format "man 3,2 %s" name)
    (let ((document (or
                     (flydoc-man-lookup-1 name "3")
                     (flydoc-man-lookup-1 name "2")))
          synopsis
          begin)
      (if (and document
               (setq synopsis (cdr-safe (assoc "SYNOPSIS" document))))
          (with-temp-buffer
            (insert synopsis)
            (goto-char (point-min))
            (setq name (regexp-quote name)
                  synopsis nil)
            (while (and (null synopsis)
                        (re-search-forward name nil t))
              (setq begin (line-beginning-position))
              (forward-line 0)
              (if (and (not (looking-at "^ *#.*"))
                       (re-search-forward ";" nil t))
                  (setq synopsis (buffer-substring-no-properties begin (match-beginning 0))))
              (forward-line 1))
            (if synopsis
                (setq document
                      (append document
                              (list
                               (cons "*TIP-KEY*" "*PROTOTYPE*")
                               (cons "*PROTOTYPE*" (flydoc-trim-string synopsis))))))))
      document)))

 
;;; Looking up by Perldoc

(defun flydoc-perldoc-lookup-function (name)
  (flydoc-cache (format "perldoc -f %s" name)
    (with-temp-buffer
      (when (= (call-process "perldoc" nil t nil "-f" name)
               0)
        (flydoc-strip)
        (goto-char (point-min))
        (let ((indent (current-indentation)))
          (while
              (progn
                (forward-line 1)
                (eq indent (current-indentation))))
          (list
           (cons "SYNOPSIS" (buffer-substring-no-properties (point-min) (point)))
           (cons "DESCRIPTION" (buffer-substring-no-properties (point) (point-max)))))))))

 
;;; Extracting Document Names

(defun flydoc-default-extract-document-name ()
  ;; TODO
  (require 'thingatpt)
  (thing-at-point 'symbol))

 
;;; Documentation

(defun flydoc-default-documentation ()
  (let (name)
    (and (setq name (flydoc-default-extract-document-name))
         (flydoc-man-lookup name))))

(defun flydoc-documentation ()
  (funcall (or flydoc-documentation-function
               'flydoc-default-documentation)))

(defun flydoc-get-explain-buffer (document)
  (let ((buffer (get-buffer-create "*Flydoc-Explain*")))
    (with-current-buffer buffer
      (erase-buffer)
      (mapcar
       (lambda (pair)
         (let ((key (car pair))
               (value (cdr pair))
               (begin (point)))
           (when (not (eq (elt key 0) ?*))
             (insert key "\n")
             (setq begin (point))
             (insert value "\n")
             (goto-char begin)
             (while
                 (progn
                   (indent-to 8)
                   (eq (forward-line 1) 0)))
             (insert "\n"))))
       document)
      (goto-char (point-min))
      (flydoc-explain-mode))
    buffer))

(defun flydoc-tip ()
  (interactive)
  (if flydoc-mode
      (let (document tip-key synopsis)
        (and (setq document (flydoc-documentation))
             (setq tip-key (or (cdr-safe (assoc "*TIP-KEY*" document))
                               "SYNOPSIS"))
             (setq synopsis (cdr-safe (assoc tip-key document)))
             (when synopsis
               (setq synopsis (flydoc-trim-string synopsis))
               (if flydoc-decorate-tip-function
                   (setq synopsis (funcall flydoc-decorate-tip-function synopsis)))
               (if synopsis
                   (message "%s" synopsis)))))))

(defun flydoc-explain ()
  (interactive)
  (if flydoc-mode
      (let ((document (flydoc-documentation)))
        (when document
          (select-window (get-lru-window))
          (switch-to-buffer (flydoc-get-explain-buffer document))))))

 
;;; Various major-mode supports

(defun flydoc-extract-c-function ()
  ;; TODO
  (require 'thingatpt)
  (save-excursion
    (let (name (try 8))
      (while (and (null name)
                  (not (zerop try)))
        (if (null
             (condition-case nil
                 (prog1 t
                   (backward-up-list))
               (error . nil)))
            (setq try 0) ;; give up
          (setq name (thing-at-point 'symbol))
          (setq try (1- try))))
      (cons name (point)))))

(defun flydoc-extract-c-function-name ()
  (car (flydoc-extract-c-function)))

(defun flydoc-highlight-c-function (prototype)
  (let (begin
        end
        (arg 0)
        result)
    (if (setq begin (cdr (flydoc-extract-c-function)))
        (progn
          (setq offset (- (point) begin 1))
          (save-excursion
            (setq begin (1+ begin)
                  end (point))
            (goto-char begin)
            (while (and begin (< begin end))
              (setq c (char-after begin))
              (cond
               ((eq c ?\()
                (goto-char begin)
                (setq begin (condition-case nil
                                (forward-list)
                              (error . nil))))
               ((eq c ?\))
                (setq begin nil))
               ((eq c ?,)
                (setq arg (1+ arg))
                (setq begin (1+ begin)))
               (t
                (setq begin (1+ begin))))))
          (with-temp-buffer
            (insert prototype)
            (goto-char (point-min))
            (condition-case nil
                (progn
                  (down-list)
                  (setq begin (point))
                  (while (null result)
                    (setq end (if (re-search-forward "[,)]" nil t)
                                  (match-beginning 0)))
                    (if (zerop arg)
                        (progn
                          (setq result prototype)
                          (if end
                              (put-text-property (1- begin) (1- end)
                                                 'face 'bold
                                                 result)))
                      (if (not (eobp))
                          (forward-char)))
                    (setq begin (1+ end))
                    (setq arg (1- arg))))
              (error . nil))))
      (setq result prototype))
    result))

(defun flydoc-sh-mode-setup ()
  (setq flydoc-documentation-function
        (lambda ()
          (let (name)
            (and (setq name (flydoc-default-extract-document-name))
                 (flydoc-man-lookup-command name))))))

(defun flydoc-c-mode-setup ()
  (setq flydoc-documentation-function
        (lambda ()
          (let (name)
            (and (setq name (flydoc-extract-c-function-name))
                 (flydoc-man-lookup-c-function name)))))
  (setq flydoc-decorate-tip-function
        'flydoc-highlight-c-function))

(defun flydoc-perl-mode-setup ()
  (setq flydoc-documentation-function
        (lambda ()
          (let (name)
            (and (setq name (flydoc-default-extract-document-name))
                 (flydoc-perldoc-lookup-function name))))))

 
;;; Flydoc Explain mode

(defun flydoc-explain-quit ()
  (interactive)
  (quit-window))

(defun flydoc-explain-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'flydoc-explain-mode
        mode-name "Flydoc-Explain")
  (buffer-disable-undo)
  (use-local-map flydoc-explain-mode-map)
  (run-hooks 'flydoc-explain-mode))

 
;;; Flydoc mode

(require 'easy-mmode)

(defun flydoc-mode-maybe ()
  (if (not (minibufferp (current-buffer)))
      (flydoc-mode 1)))

(defun flydoc-default-setup ()
  (add-hook 'sh-mode-hook 'flydoc-sh-mode-setup)
  (add-hook 'c-mode-common-hook 'flydoc-c-mode-setup)
  (add-hook 'perl-mode-hook 'flydoc-perl-mode-setup)
  (add-hook 'cperl-mode-hook 'flydoc-perl-mode-setup))

(define-minor-mode flydoc-mode
  "Flydoc mode."
  nil " Flydoc" nil
  :group 'flydoc
  (if flydoc-mode
      (run-hooks 'flydoc-mode-hook)))

(define-global-minor-mode global-flydoc-mode
  flydoc-mode flydoc-mode-maybe
  :group 'flydoc)

(provide 'flydoc)
;;; flydoc.el ends here
