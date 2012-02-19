;;; mistilteinn.el --- minor-mode for mistilteinn
;;; -*- coding: utf-8 -*-
;;
;;; Install:
;;
;; (1) Install mistilteinn system by rubygems.
;;
;;    gem install mistilteinn
;;
;; (2) Copy this file to load-path
;;
;; (3) Add following code to .emacs
;;
;;    (add-to-list 'load-path "~/workspaces/mistilteinn/")
;;    (require 'mistilteinn)
;;    ;; for minor mode
;;    (global-mistilteinn-mode t)
;;    ;; for anything
;;    (defun anything-for-mistiltein ()
;;      (interactive)
;;      (anything-other-buffer
;;        '(anything-c-source-git-ticket) "*mistiltein*"))
;;    (define-key global-map (kbd "C-t") 'anything-for-mistiltein))
;;
;;; Setup for each project:
;;
;; To use mistilteinn, run `mistilteinn init` at each project dirctory.
;;
;;    $ cd /path/to/project
;;    $ git init
;;    $ mistilteinn init
;;    $ vim .mistilteinn/config.yaml
;;
;;; Web sites:
;;
;;  stable verison: http://www.emacswiki.org/cgi-bin/wiki?mistilteinn.el
;;  development version: https://github.com/mistilteinn/mistilteinn-emacs
;;  mistilteinn website: http://mistilteinn.github.com
;;
;;; Author:
;;
;; @mzp
;; @mallowlabs
;; @suer
;;
;;; License:
;;
;; mistilteinn.el is free software; you can redistribute it and/or modify
;; it under the terms of MIT License.

;;; Code:
(require 'cl)

;;;; configure
;;; The definition of mistilteinn.
(defgroup mistilteinn nil
  "New style development with git and emacs"
  :group 'tools)

(defcustom mistilteinn-exclude-modes  '(dummy1-mode dummy2-mode)
  "Major modes `mistilteinn-minor-mode' cannot run."
  :type '(list symbol)
  :group 'mistilteinn)

(defcustom mistilteinn-inactive-ticket-regexp  "\\[解決\\]"
  "A regexp for inactive ticket regexp"
  :type 'string
  :group 'mistilteinn)

(defcustom mistilteinn-info-buffer "*mistilteinn-info*"
  "Buffer name for information"
  :type 'string
  :group 'mistilteinn)

(defcustom mistilteinn-message-buffer "*mistilteinn-message*"
  "Buffer name for message"
  :type 'string
  :group 'mistilteinn)

(defcustom mistilteinn-diff-buffer "*mistilteinn-diff*"
  "Buffer name for diff"
  :type 'string
  :group 'mistilteinn)

(defface mistilteinn-inactive-ticket-face
  '((t (:foreground "blue")))
  "*Face used for inactive ticket"
  :group 'mistilteinn)

(defface mistilteinn-active-ticket-face
  '()
  "*Face used for active ticket"
  :group 'mistilteinn)

;;;; message buffer function
;;; Create message buffer and popup for user to input commit message etc.
(defun mi:close-message-buffer ()
  "Close current message buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun mi:strip-comment (s)
  "strip comment heading #"
  (replace-regexp-in-string "^#.*\n" "" s))

(defvar mi:commit (lambda () nil))
(make-variable-buffer-local 'mi:commit)
(defun mi:commit-message-buffer ()
  "Commit current message buffer."
  (interactive)
  (funcall mi:commit (mi:strip-comment (buffer-string)))
  (mi:close-message-buffer))

(defvar mi:message-keymap
  (make-sparse-keymap)
  "A keymap for message buffer. ")

(define-key mi:message-keymap (kbd "C-c C-c") 'mi:commit-message-buffer)
(define-key mi:message-keymap (kbd "C-c C-q") 'mi:close-message-buffer)

(defconst mi:message-help
"
# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")

(defconst mi:message-font-locks
  '(("^\\(#.*\\)$" (1 font-lock-comment-face t))   ;; highlight comment
    ))

(defun mi:show-message-buffer (f)
  "Show message buffer and callback `f' when user input is completed."
  (let ((buffer (generate-new-buffer mistilteinn-message-buffer)))
    (with-current-buffer buffer
      ;; restore window configure at kill buffer
      (add-hook 'kill-buffer-hook
                (lexical-let ((wc (current-window-configuration)))
                  #'(lambda ()
                      (set-window-configuration wc))) nil t)
      (setq mi:commit f)
      ;; set keybind
      (use-local-map mi:message-keymap)
      ;; set fontlock
      (font-lock-add-keywords nil mi:message-font-locks)
      (when global-font-lock-mode (font-lock-mode t))
      ;; add help message
      (insert "# C-c C-c: commit; C-c C-q: close buffer\n")
      (save-excursion (insert mi:message-help)))
    (pop-to-buffer buffer)))

;;;; git command
(defun mistilteinn-git-now ()
  "run git-now to create temporary commit"
  (interactive)
  (shell-command "git now --compact"))

(defun mi:git-now ()
  (interactive)
  (when mistilteinn-minor-mode
    (mistilteinn-git-now)))

(defun mi:branch-list ()
  "Get branch list for current repository."
  (remove-if '(lambda (s) (string= "" s))
             (mapcar '(lambda (s) (replace-regexp-in-string "^*? *" "" s))
                     (split-string (shell-command-to-string "git branch")
                                   "\n"))))

(defun mistilteinn-git-master ()
  "run git-master to masterize current topic branch"
  (interactive)
  (let* ((branch (completing-read "git-master (default master): " (mi:branch-list) nil nil "master" nil "master"))
         (cmd    (format "git master %s" branch)))
    (shell-command cmd)))

(defun mistilteinn-git-ticket-create (subject)
  "Create ticket."
  (interactive "sSubject: ")
  (shell-command (format "mistilteinn create \"%s\"" subject)))

(defun mi:git-fixup (s)
  (shell-command (format "git now --fixup \"%s\"" s)))

(defun mi:git-diff (buf)
  (shell-command "git now --diff" buf))

(defun mistilteinn-git-fixup ()
  "run git-now --fixup to fixup now commit"
  (interactive)
  (mi:show-message-buffer 'mi:git-fixup))

(defun mistilteinn-git-diff ()
  (interactive)
  (when (buffer-live-p mistilteinn-diff-buffer)
    (kill-buffer mistilteinn-diff-buffer))
  (with-current-buffer (get-buffer-create mistilteinn-diff-buffer)
    (diff-mode)
    (mi:git-diff (current-buffer))))

;;;; anything
(defun mi:switch-topic-branch (str)
  (let ((id (car (split-string str " "))))
    (shell-command
     (format "git branch id/%s 2>/dev/null; git checkout id/%s" id id)
     "*git-ticket*")))

(defun mi:highlight-ticket (tickets)
  (loop for ticket in tickets
      collect
      (cond
       ((string-match mistilteinn-inactive-ticket-regexp ticket) (propertize ticket 'face 'mistilteinn-inactive-ticket-face))
       (t (propertize ticket 'face 'mistilteinn-active-ticket-face)))))

(defvar anything-c-source-git-ticket
  '((name . "Tickets")
    (candidates-in-buffer)
    (candidate-transformer mi:highlight-ticket)
    (init . (lambda () (call-process-shell-command "mistilteinn list" nil (anything-candidate-buffer 'git-ticket))))
    (action ("Switch topic branch" . mi:switch-topic-branch))))

;;;; minor mode
(defconst mistilteinn-minor-mode-map (make-sparse-keymap)
  "Keymap for the mistilteinn minor mode.")

(define-key mistilteinn-minor-mode-map (kbd "C-c # c") 'mistilteinn-git-ticket-create)
(define-key mistilteinn-minor-mode-map (kbd "C-c # m") 'mistilteinn-git-master)
(define-key mistilteinn-minor-mode-map (kbd "C-c # n") 'mistilteinn-git-now)
(define-key mistilteinn-minor-mode-map (kbd "C-c # f") 'mistilteinn-git-fixup)
(define-key mistilteinn-minor-mode-map (kbd "C-c # d") 'mistilteinn-git-diff)

(define-minor-mode mistilteinn-minor-mode
  "mistilteinn"
  :lighter " mi"
  :keymap mistilteinn-minor-mode-map
  :group mistilteinn-mode
  (funcall (if mistilteinn-minor-mode 'add-hook 'remove-hook)
           'after-save-hook
           'mi:git-now))

(defmacro mi:with-cd (path &rest body)
  (let ((var (make-symbol "path")))
    `(let ((,var default-directory))
       (unwind-protect
           (progn
             (cd ,path)
             ,@body)
         (cd ,var)))))

(defun mi:inside-p (path)
  (and (file-exists-p path)
       (file-directory-p path)
       (mi:with-cd path
                   (equal "0"
                          (shell-command-to-string "mistilteinn is-inside; echo -n $?")))))

(defun mi:mode-switch ()
  "Return t and enable mistilteinn-minor-mode if `mistilteinn-minor-mode' can called on current buffer."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode mistilteinn-exclude-modes))
             (mi:inside-p "."))
    (mistilteinn-minor-mode t)))

(define-global-minor-mode global-mistilteinn-mode
  mistilteinn-minor-mode mi:mode-switch)

(provide 'mistilteinn)
;;; mistilteinn.el ends here.
