;;; Emacs init file for Christian Rovner            -*- coding: utf-8 -*-
;; ----------------------------------------------------------------------

(defvar workspace-dir "~/workspace/")

(load "~/.emacs.d/ifind-mode.el")
(load "~/.emacs.d/mybuffers.el")
(visit-tags-table "~/workspace/TAGS")
(grep-compute-defaults)

(defun find-in-workspace (pattern)
  "Run `find-name-dired' in `workspace-dir' for the given PATTERN."
  (interactive "sFind file: ")
  (find-name-dired workspace-dir pattern))

(defun grep-in-workspace (pattern)
  "Run `rgrep' in all files of `wokspace-dir' for the given PATTERN."
  (interactive "sGrep pattern: ")
  (rgrep pattern "*" workspace-dir))

(defun search-at-point ()
  "Highlight word at point by starting an isearch on that word."
  (interactive)
  (isearch-mode nil)
  (let ((word (symbol-name (symbol-at-point))))
    (setq isearch-string word
          isearch-message word))
  (isearch-search-and-update))

(defun perltidy-region ()
  "Run perltidy on the active region (or the whole buffer by default)."
  (interactive)
  (save-excursion
    (let ((beg (if mark-active (point) (point-min)))
          (end (if mark-active (mark) (point-max)))) 
      (shell-command-on-region beg end "perltidy -q" nil t))))

(defun expand-region-to-whole-lines ()
  "Expand the region to make it encompass whole lines.
If the region is not active, activate the current line."
  (if (not mark-active)
      ;; Create region from current line
      (progn 
        (beginning-of-line)
        (set-mark (point))
        (end-of-line))
    ;; The mark is active, expand region
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char beg)
      (beginning-of-line)
      (set-mark (point))
      (goto-char end)
      (unless (bolp) (end-of-line)))))

(defun my-comment-region ()
  "Comment or uncomment region after expanding it to whole lines."
  (interactive)
  (save-excursion
    (expand-region-to-whole-lines)
    (comment-or-uncomment-region (region-beginning) (region-end))))

(defun my-increase-left-margin ()
  "Increase left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (expand-region-to-whole-lines)
    (increase-left-margin (region-beginning) (region-end) nil)))

(defun my-decrease-left-margin ()
  "Decrease left margin in region after expanding it to whole lines."
  (interactive)
  (let (deactivate-mark)
    (expand-region-to-whole-lines)
    (decrease-left-margin (region-beginning) (region-end) nil)))

(defun undo-all ()
  "Undo all changes.
This is equivalent to revert-buffer, except that it doesn't
re-read the file.  Useful when editing a remote file."
  (interactive)
  (undo-start)
  (undo-more (length pending-undo-list)))

(defun toggle-tab-width ()
  "Toggle variable tab-width between 8 and 4."
  (interactive)
  (set-variable 'tab-width (if (= tab-width 8) 4 8))
  (message "Tab width set to %d" tab-width))

(defun toggle-tabs-mode ()
  "Toggle variable indent-tabs-mode between t and nil."
  (interactive)
  (set-variable 'indent-tabs-mode (not indent-tabs-mode))
  (message "Tabs mode set to %s" indent-tabs-mode))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs,
indented text (quotes,code) and lines starting with an
asterix (lists) intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(defun smart-tab ()
  "This smart tab is minibuffer compliant: it acts as usual in
the minibuffer. Else, if mark is active, indents region. Else if
point is at the end of a symbol, expands it. Else indents the
current line."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if mark-active
        (indent-region (region-beginning)
                       (region-end))
      (if (looking-at "\\_>")
          (dabbrev-expand nil)
        (indent-for-tab-command)))))

(defalias 'fw 'find-in-workspace)
(defalias 'gw 'grep-in-workspace)
(defalias 'dt 'delete-trailing-whitespace)
(defalias 'fi 'ifind-mode)

;; ----------------------------------------------------------------------
;; Preferences

;; No me rompas las pelotas
(fset 'yes-or-no-p 'y-or-n-p)

;; Keyboard shortcuts
(global-set-key [(control .)]             'search-at-point)
(global-set-key [(control tab)]           'mybuffers-switch)
(global-set-key [(control ?')]            'my-comment-region)
(global-set-key [(control >)]             'my-increase-left-margin)
(global-set-key [(control <)]             'my-decrease-left-margin)
(global-set-key [(meta ?ñ)]               'ediff-buffers)
(global-set-key [tab]                     'smart-tab)
(global-set-key [(meta _)]                'undo-all)
(global-set-key [(control meta _)]        'revert-buffer)
(global-set-key [(control shift d)]       'kill-whole-line)
(global-set-key [f7]                      'find-variable-at-point)
(global-set-key [f8]                      'find-function-at-point)
(global-set-key [(control insert)]        'clipboard-kill-ring-save)
(global-set-key [(shift insert)]          'clipboard-yank)
(global-set-key [(control meta t)]        'toggle-tabs-mode)
(global-set-key [(control meta w)]        'toggle-tab-width)

;; Perl
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook 
          (lambda () 
			(require 'template)
			(template-initialize)
			(require 'perlnow)
            (local-set-key [f5] 'perlnow-run)
            (cperl-set-style "BSD")))

;; PHP
(defun my-php-mode-common-hook ()
  (set-variable 'indent-tabs-mode nil)
  (c-set-offset 'class-open 0)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (setq c-default-style "bsd"
        c-basic-offset 4))
(add-hook 'php-mode-hook 'my-php-mode-common-hook)

;; CSS
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

;; Enable recent files menu
(require 'recentf)
(setq recentf-auto-cleanup 'never)
(recentf-mode 1)

;; Interactively do things
(require 'ido)
(ido-mode t)
(add-hook 'ido-minibuffer-setup-hook
           (lambda ()
             (make-local-variable 'truncate-lines)
             (setq truncate-lines nil)))

;; File types
(setq auto-mode-alist 
      (append '(
                ("\\.tpl$"      . html-mode)
                ("\\.template$" . html-mode)
                ("\\.frag$"     . html-mode)
                ("\\.mako$"     . html-mode)
                ("\\.as[pc]x$"  . html-mode)
                ("\\.js$"       . js2-mode)
                ) auto-mode-alist))

;; ----------------------------------------------------------------------
;; Added by Emacs

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/tmp/save" t))))
 '(backup-directory-alist (quote (("." . "~/.backups"))))
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(cua-mode t nil (cua-base))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(fill-column 80)
 '(global-font-lock-mode t)
 '(grep-find-ignored-directories (quote ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "var")))
 '(grep-find-template "find . <X> -not -name TAGS -type f <F> -print0 | xargs -0 -e grep <C> -nH -e <R>")
 '(ido-enable-tramp-completion nil)
 '(ido-max-prospects 20)
 '(ido-max-window-height nil)
 '(ido-record-ftp-work-directories nil)
 '(ido-separator " | ")
 '(indent-tabs-mode t)
 '(js2-basic-offset 4)
 '(js2-highlight-level 2)
 '(max-mini-window-height 0.4)
 '(show-paren-mode t)
 '(tab-width 4)
 '(tramp-auto-save-directory "~/.backups")
 '(tramp-backup-directory-alist (quote (("." . "~/.backups"))))
 '(tramp-verbose 2)
 '(truncate-lines t)
 '(user-mail-address "crovner@...com")
 '(visible-bell t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mode-line ((((class color) (min-colors 88)) (:background "grey10" :foreground "white"))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:background "grey10" :foreground "grey50")))))
