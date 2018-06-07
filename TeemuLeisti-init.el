    ; -*- coding: utf-8  -*-
    
    ;;;; file: ~/.emacs.d/init.el
    ;;;; Teemu Leisti 2018-06-06
    
    
    ;;;; General settings.
    
    ;; Use UTF-8 for all character encoding.
    (prefer-coding-system 'utf-8)
    (set-locale-environment "en.UTF-8")
    (set-language-environment 'utf-8)
    (set-default-coding-systems 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)
    (setq-default buffer-file-coding-system 'utf-8)
    (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding
    
    ;; Added to work around a bug in Emacs 25.2.2 that breaks the setting of the font size in the
    ;; initialization file on hosts that use gconf, a system used by the GNOME desktop environment for
    ;; storing configuration settings.
    (define-key special-event-map [config-changed-event] #'ignore)
    
    ;; Added by Package.el. This must come before configurations of installed packages. Don't delete
    ;; this line. If you don't want it, just comment it out by adding a semicolon to the start of the
    ;; line. You may delete these explanatory comments.
    ;(package-initialize)
    
    ;; Add to load-path this directory containing local add-ons.
    (add-to-list 'load-path "~/.emacs.d/lisp/")
    
    ;; Enable the narrow-to-region command ("C-x n n").
    (put 'narrow-to-region 'disabled nil)
    
    ;; Always confirm exiting.
    (setq confirm-kill-emacs 'yes-or-no-p)
    
    ;; Make all yes-or-no questions accept typing just "y" or "n".
    (fset 'yes-or-no-p 'y-or-n-p)
    
    ;; Disable all version control backends.
    (setq vc-handled-backends (quote ()))
    
    ;; Remove all trailing whitespace when saving a buffer.
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    
    ;; Set the default major mode.
    (setq major-mode 'text-mode)
    
    ;; To make dead keys work again.
    (require 'iso-transl)
    
    ;; To see which key sequences are unassigned, command describe-unbound-keys.
    (require 'unbound)
    
    
    ;;;; Custom variable settings.
    
    (custom-set-variables
     ;; custom-set-variables was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(blink-cursor-mode nil)
     '(c-basic-indent 2)
     '(c-basic-offset 4)
     '(c-offsets-alist
       (quote
        ((inline-open . 0)
         (statement-case-intro . 0)
         (statement-case-open . 0)
         (substatement-open . 0)
         (case-label . +))))
     '(fci-handle-truncate-lines nil)
     '(fill-column 100)
     '(indent-tabs-mode nil)
     '(indicate-empty-lines t)
     '(inhibit-startup-screen t)
     '(js-indent-level 2)
     '(line-move-visual nil)
     '(list-colors-sort (quote hsv))
     '(search-whitespace-regexp nil)
     '(sentence-end-double-space nil)
     '(show-paren-mode t)
     '(tab-width 4)
     '(tool-bar-mode nil)
     '(transient-mark-mode nil)
     '(uniquify-buffer-name-style nil nil (uniquify))
     '(vc-make-backup-files t))
    
    
    ;;;; Settings for character style, colors, and other look-and-feel stuff.
    
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "Liberation Mono" :foundry "1ASC" :background "light yellow" :foreground "black" :slant normal :weight normal :height 90 :width normal))))
     '(highlight ((t (:background "moccasin"))))
     '(isearch ((t (:background "chocolate" :foreground "white"))))
     '(lazy-highlight ((t (:background "yellow2" :foreground "black"))))
     '(mode-line ((t (:background "darkorange4" :foreground "white smoke" :box (:line-width -1 :style released-button)))))
     '(mode-line-inactive ((t (:background "gray16" :foreground "white smoke" :box (:line-width -1 :style released-button)))))
     '(region ((t (:background "yellow" :foreground "black"))))
     '(secondary-selection ((t (:background "dark salmon"))))
     '(warning ((t (:foreground "orange red" :weight bold)))))
    
    ;; Turn on the highlight-current-line mode.
    (global-hl-line-mode 1)
    
    ;; Set the line number mode on by default.
    (global-linum-mode 1) (add-hook 'term-mode-hook (lambda () (linum-mode -1)))
    
    ;; Show a vertical line at the fill column, defined as a global minor mode.  Provided by
    ;; fill-column-indicator.el.
    (require 'fill-column-indicator)
    (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
    (global-fci-mode 1)
    
    ;; Make tab characters visible as a right-guillemet symbol.
    (setq whitespace-style '(tabs tab-mark))
    (global-whitespace-mode 1)
    
    ;; Tweak the mode line format.
    (setq default-mode-line-format
          (list "--:" 'mode-line-modified "- %30b %[(" 'mode-name 'minor-mode-alist
                "%n" 'mode-line-process ")%]--(%l,%c)--" '(-3 . "%p") "-%-"))
    (setq mode-line-format default-mode-line-format)
    
    ;; Set the window title format to show the full path of the file.
    (setq frame-title-format
          '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
    
    
    ;;;; Mode-specific settings.
    
    ;; In text mode, set fill column to 80, instead of the default 100 (as set by custom-set-variables
    ;; above).
    (defun fill-column-to-80 () (setq fill-column 80))
    (add-hook 'text-mode-hook 'fill-column-to-80)
    
    ;; When in the Java, Ruby, or HTML modes, use spaces instead of tabs.
    (defun use-spaces-instead-of-tabs () (setq indent-tabs-mode nil))
    (add-hook 'java-mode-hook 'use-spaces-instead-of-tabs)
    (add-hook 'ruby-mode-hook 'use-spaces-instead-of-tabs)
    (add-hook 'html-mode-hook 'use-spaces-instead-of-tabs)
    
    ;; No need for adding the encoding line to top of Ruby files in case the file contains non-ASCII
    ;; characters, since Ruby versions 2 and higher use UTF-8 by default.
    (setq ruby-insert-encoding-magic-comment nil)
    
    ;; Update ruby-mode to indent the access modifiers ('private', 'protected', 'public') on the same
    ;; level as the class or module name, and everything after them by one indent step.
    ;; 2018-04-20: Does not work. None of the debug messages are ever displayed.
    (defadvice ruby-indent-line (around outdent-modifiers activate)
      (message "ruby-indent-line 1")
      (if (save-excursion
            (message "ruby-indent-line 2"))
            (beginning-of-line)
            (message "beginning-of-line")
            (looking-at "\s*\\(private\\|protected\\|public\\)\s*$"))
          (save-excursion
            (message "ruby-indent-line 3"))
            (beginning-of-line)
            (just-one-space 0)
        ad-do-it
        (message "ruby-indent-line 4"))
    
    ;; Invoke the associated major mode when opening certain types of Rails files.
    (add-to-list 'auto-mode-alist '("\\.js\\.erb\\'" . js-mode))
    (add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-mode))
    
    
    ;;;; Define host-specific settings and key bindings.
    
    (defun open-energiavirasto-todo-file ()
      (interactive)
      (find-file "~/Documents/Energiavirasto/todo.txt"))
    
    (defun open-personal-todo-file ()
      (interactive)
      (find-file "~/Documents/personal/todo.txt"))
    
    (defun open-personal-passwords-file ()
      (interactive)
      (find-file "~/Documents/personal/passwords.txt"))
    
    (if (string= (system-name) "unikie")
        (progn
          (global-set-key (kbd "C-c t") 'open-energiavirasto-todo-file)
          (set-face-attribute 'default nil :height 100))
      (progn
        (global-set-key (kbd "C-c t") 'open-personal-todo-file)
        (global-set-key (kbd "C-c w") 'open-personal-passwords-file)))
    
    
    ;;;; Define key sequences for managing windows.
    
    ;; Key sequence "C-'" leaves only the active window open.
    (global-set-key (kbd "C-'") '(lambda () (interactive) (delete-other-windows)))
    
    ;; Key sequence "C-*" closes the active window.
    (global-set-key (kbd "C-*") '(lambda () (interactive) (delete-window)))
    
    ;; Key sequences "C-<tab>" and "M-p" switch to other window.
    (global-set-key (kbd "C-<tab>") 'other-window)
    (global-set-key (kbd "M-p") 'other-window)
    
    ;; Key sequence "C-å" shrinks the current window vertically by one row.
    (global-set-key (kbd "C-å") 'shrink-window)
    
    ;; Key sequence "C-+" enlarges the current window vertically by one row, providing an easier key
    ;; sequence than (C-x ^), which 'enlarge-window is bound to by default.
    (global-set-key (kbd "C-+") 'enlarge-window)
    
    ;; Key sequence "M-+" enlarges the current window horizontally by one row, providing an easier key
    ;; sequence than (C-x }), which 'enlarge-window-horizontally is bound to by default.
    (global-set-key (kbd "M-+") 'enlarge-window-horizontally)
    
    ;; Key sequence "M-å" shrinks the current window horizontally by one row, providing an easier key
    ;; sequence than (C-x {), which 'shrink-window-horizontally is bound to by default.
    (global-set-key (kbd "M-å") 'shrink-window-horizontally)
    
    
    ;;;; Define functions and key sequences for navigating by brackets. Modified from code written by
    ;;;; Emacs guru XAH.
    
    (defvar xah-brackets nil "string of left/right brackets pairs.")
    (setq xah-brackets "(){}[]<>“”‘’‹›«»")
    
    (defvar xah-left-brackets '("(" "{" "[" "<" "“" "‘" "‹" "«" )
      "List of left bracket chars.")
    (progn
      ;; make xah-left-brackets based on xah-brackets
      (setq xah-left-brackets '())
      (dotimes ($x (- (length xah-brackets) 1))
        (when (= (% $x 2) 0)
          (push (char-to-string (elt xah-brackets $x))
                xah-left-brackets)))
      (setq xah-left-brackets (reverse xah-left-brackets)))
    
    (defvar xah-right-brackets '(")" "}" "]" ">" "”" "’" "›" "»")
      "List of right bracket chars.")
    (progn
      (setq xah-right-brackets '())
      (dotimes ($x (- (length xah-brackets) 1))
        (when (= (% $x 2) 1)
          (push (char-to-string (elt xah-brackets $x))
                xah-right-brackets)))
      (setq xah-right-brackets (reverse xah-right-brackets)))
    
    (defun xah-backward-left-bracket ()
      "Move cursor to the previous occurrence of left bracket.
       The list of brackets to jump to is defined by
       `xah-left-brackets'."
      (interactive)
      (re-search-backward (regexp-opt xah-left-brackets) nil t))
    
    (defun xah-forward-right-bracket ()
      "Move cursor to the next occurrence of right bracket.
       The list of brackets to jump to is defined by
       `xah-right-brackets'."
      (interactive)
      (re-search-forward (regexp-opt xah-right-brackets) nil t))
    
    ;; Key sequences "M-<left>" and "M-<right>" move the cursor between matching brackets.
    (global-set-key (kbd "M-<left>")' xah-backward-left-bracket)
    (global-set-key (kbd "M-<right>") 'xah-forward-right-bracket)
    
    (defun xah-goto-matching-bracket ()
      "Move cursor to the matching bracket. If cursor is not on a
       bracket, call `backward-up-list'. The list of brackets to jump
       to is defined by `xah-left-brackets' and
       `xah-right-brackets'."
      (interactive)
      (if (nth 3 (syntax-ppss))
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
        (cond
         ((eq (char-after) ?\") (forward-sexp))
         ((eq (char-before) ?\") (backward-sexp ))
         ((looking-at (regexp-opt xah-left-brackets))
          (forward-sexp))
         ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
          (backward-sexp))
         (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))
    
    ;; Key sequence "M-<return>" moves the cursor to the matching brackets, and if the cursor is not on a
    ;; bracket, moves it to the previous bracket.
    (global-set-key (kbd "M-<return>")' xah-goto-matching-bracket)
    
    
    ;;;; Define functions and key sequences for moving lines.
    
    (defun move-line (n)
      "Move the current line up or down by N lines."
      (interactive "p")
      (setq col (current-column))
      (beginning-of-line) (setq start (point))
      (end-of-line) (forward-char) (setq end (point))
      (let ((line-text (delete-and-extract-region start end)))
        (forward-line n)
        (insert line-text)
        ;; restore point to original column in moved line
        (forward-line -1)
        (forward-char col)))
    
    (defun move-line-up (n)
      "Move the current line up by N lines."
      (interactive "p")
      (move-line (if (null n) -1 (- n))))
    
    (defun move-line-down (n)
      "Move the current line down by N lines."
      (interactive "p")
      (move-line (if (null n) 1 n)))
    
    ;; Key sequences "C-S-<up>" and "C-S-<down>" move the current line up and down.
    (global-set-key (kbd "C-S-<up>") 'move-line-up)
    (global-set-key (kbd "C-S-<down>") 'move-line-down)
    
    
    ;;;; Define key sequences for setting fill columns.
    
    ;; Key sequence "C-c ö" sets the fill column to 80.
    (global-set-key (kbd "C-c ö")
                    '(lambda ()
                       (interactive)
                       (setq fill-column 80)
                       (message "Fill column set to 80")))
    
    ;; Key sequence "C-c ä" sets the fill column to 100.
    (global-set-key (kbd "C-c ä")
                    '(lambda ()
                       (interactive)
                       (setq fill-column 100)
                       (message "Fill column set to 100")))
    
    ;; Key sequence "C-c å" sets the fill column to 120.
    (global-set-key (kbd "C-c å")
                    '(lambda ()
                       (interactive)
                       (setq fill-column 120)
                       (message "Fill column set to 120")))
    
    
    ;;;; Define key sequences for inserting strings.
    
    ;; Key sequence "C-c 7" inserts a TODO note with "Teemu Leisti" and the current date.
    (global-set-key (kbd "C-c 7") '(lambda () (interactive)
                                     (insert "TODO Teemu Leisti ")
                                     (insert (format-time-string "%Y-%m-%d"))
                                     (insert ": ")))
    
    ;; Key sequence "C-c 8" inserts "-- Teemu Leisti".
    (global-set-key (kbd "C-c 8") '(lambda () (interactive) (insert "-- Teemu Leisti")))
    
    ;; Key sequence "C-c 9" inserts "-- Teemu".
    (global-set-key (kbd "C-c 9") '(lambda () (interactive) (insert "-- Teemu")))
    
    ;; Key sequence "C-c 0" inserts the current date in ISO format.
    (global-set-key (kbd "C-c 0") '(lambda () (interactive) (insert (format-time-string "%Y-%m-%d"))))
    
    ;; Key sequence "C-c g" inserts "teemu.leisti@gmail.com".
    (global-set-key (kbd "C-c g") '(lambda () (interactive) (insert "teemu.leisti@gmail.com")))
    
    ;; Key sequence "C-c \!" inserts "● ".
    (global-set-key (kbd "C-c \!") '(lambda () (interactive) (insert "● ")))
    
    ;; Key sequence "C-c \"" inserts "► ".
    (global-set-key (kbd "C-c \"") '(lambda () (interactive) (insert "► ")))
    
    ;; Key sequence "C-c #" inserts "■ ".
    (global-set-key (kbd "C-c #") '(lambda () (interactive) (insert "■ ")))
    
    ;; Key sequence "C-c m" inserts the m-dash.
    (global-set-key (kbd "C-c m") '(lambda () (interactive) (insert "—")))
    
    ;; Key sequence "C-c n" inserts the n-dash.
    (global-set-key (kbd "C-c n") '(lambda () (interactive) (insert "–")))
    
    ;; Key sequence "C-c <left>" inserts the left-arrow (←).
    (global-set-key (kbd "C-c <left>") '(lambda () (interactive) (insert "←")))
    
    ;; Key sequence "C-c <right>" inserts the right-arrow (→).
    (global-set-key (kbd "C-c <right>") '(lambda () (interactive) (insert "→")))
    
    ;; Key sequence "C-c <up>" inserts the up-arrow (↑).
    (global-set-key (kbd "C-c <up>") '(lambda () (interactive) (insert "↑")))
    
    ;; Key sequence "C-c <down>" inserts the down-arrow (↓).
    (global-set-key (kbd "C-c <down>") '(lambda () (interactive) (insert "↓")))
    
    ;; Key sequence "C-c <return>" inserts the return-arrow (↵).
    (global-set-key (kbd "C-c <return>") '(lambda () (interactive) (insert "↵")))
    
    
    ;;;; Define key sequences for opening files.
    
    ;; Key sequence "C-c a" opens file ~/.bash_aliases.
    (global-set-key (kbd "C-c a") '(lambda () (interactive) (find-file "~/.bash_aliases")))
    
    ;; Key sequence "C-c u" opens file ~/.bash_aliases_unikie.
    (global-set-key (kbd "C-c u") '(lambda () (interactive) (find-file "~/.bash_aliases_unikie")))
    
    ;; Key sequence "C-c p" opens file ~/.bash_aliases_personal.
    (global-set-key (kbd "C-c p") '(lambda () (interactive) (find-file "~/.bash_aliases_personal")))
    
    ;; Key sequence "C-c b" opens file ~/.bashrc.
    (global-set-key (kbd "C-c b") '(lambda () (interactive) (find-file "~/.bashrc")))
    
    ;; Key sequence "C-c A" opens file /etc/bash.bash_aliases.
    (global-set-key (kbd "C-c A") '(lambda () (interactive) (find-file "/etc/bash.bash_aliases")))
    
    ;; Key sequence "C-c B" opens file /etc/bash.bashrc.
    (global-set-key (kbd "C-c B") '(lambda () (interactive) (find-file "/etc/bash.bashrc")))
    
    ;; Key sequence "C-c e" opens file ~/.emacs.d/init.el.
    (global-set-key (kbd "C-c e") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))
    
    ;; Key sequence "C-c 1" opens file ~/tmp/tmp-1.txt.
    (global-set-key (kbd "C-c 1") '(lambda () (interactive) (find-file "~/tmp/tmp-1.txt")))
    
    ;; Key sequence "C-c 2" opens file ~/tmp/tmp-2.txt.
    (global-set-key (kbd "C-c 2") '(lambda () (interactive) (find-file "~/tmp/tmp-2.txt")))
    
    ;; Key sequence "C-c 3" opens file ~/tmp/tmp-3.txt.
    (global-set-key (kbd "C-c 3") '(lambda () (interactive) (find-file "~/tmp/tmp-3.txt")))
    
    ;; Key sequence "C-c 4" opens file ~/tmp/tmp-4.txt.
    (global-set-key (kbd "C-c 4") '(lambda () (interactive) (find-file "~/tmp/tmp-4.txt")))
    
    ;; Key sequence "C-c 5" opens file ~/tmp/tmp-5.txt.
    (global-set-key (kbd "C-c 5") '(lambda () (interactive) (find-file "~/tmp/tmp-5.txt")))
    
    ;; Key sequence "C-c 6" opens file ~/tmp/tmp-6.txt.
    (global-set-key (kbd "C-c 6") '(lambda () (interactive) (find-file "~/tmp/tmp-6.txt")))
    
    ;; Key sequence "C-c d" starts opening a file in directory ~/Documents.
    (fset 'start-opening-documents-file "\C-x\C-f\C-a\C-k~/Documents/")
    (global-set-key (kbd "C-c d") '(lambda () (interactive)
                                     (execute-kbd-macro (symbol-function 'start-opening-documents-file))))
    
    ;; Key sequence "C-c c" starts opening a file in directory ~/code/.
    (fset 'start-opening-file-in-code "\C-x\C-f\C-a\C-k~/code/")
    (global-set-key (kbd "C-c c") '(lambda () (interactive)
                                     (execute-kbd-macro (symbol-function 'start-opening-file-in-code))))
    
    
    ;;;; Define other functions and key sequences.
    
    ;; Key sequences "C-S-a" and "S-<home>" invoke the function beginning-of-line-text.
    (global-set-key (kbd "C-S-a") 'beginning-of-line-text)
    (global-set-key (kbd "S-<home>") 'beginning-of-line-text)
    
    ;; Key sequence "C-," invokes the function just-one-space, which is bound to key sequence "M-SPC" by
    ;; default.
    (global-set-key (kbd "C-,") 'just-one-space)
    
    ;; Key sequence "C-." invokes the function delete-horizontal-space, which is bound to key sequence
    ;; "M-\" by default.
    (global-set-key (kbd "C-.") 'delete-horizontal-space)
    
    ;; Key sequence "C-w" kills the previous word.
    (global-set-key (kbd "C-w") 'backward-kill-word)
    
    ;; Key sequence "C-z" calls eval-defun. This has the additional benefit of overwriting the default
    ;; binding of "C-z", function suspend-frame, which causes the frame to be minimized.
    (global-set-key (kbd "C-z") 'eval-defun)
    
    ;; Key sequence "C-c l" calls goto-line.
    (global-set-key (kbd "C-c l") 'goto-line)
    
    ;; Key sequence "C-<return>" means calling split-line. (That function is bound to "C-M-o", but the key
    ;; sequence "C-M" is caught by the VMware virtual machine, to get focus away from the VM, so we have
    ;; to use another key sequence in order to be able to use Emacs in such a virtual machine.)
    (global-set-key (kbd "C-<return>") 'split-line)
    
    ;; Key sequence "C-c ." inserts the "<code> ... </code>" tags around the region.
    (fset 'insert-code-tags-around-region "\C-x\C-x<code>\C-x\C-x</code>")
    (global-set-key (kbd "C-c .") 'insert-code-tags-around-region)
    
    ;; Key sequence "C-c r" opens the calendar, customized here.
    (setq calendar-week-start-day 1)  ; the week starts on Monday
    (copy-face font-lock-constant-face 'calendar-iso-week-face)
    (set-face-attribute 'calendar-iso-week-face nil :height 0.8)
    (setq calendar-intermonth-text    ; show ISO week numbers
          '(propertize (format "%2d"
                               (car
                                (calendar-iso-from-absolute
                                 (calendar-absolute-from-gregorian (list month day year)))))
                       'font-lock-face 'calendar-iso-week-face))
    (setq calendar-intermonth-header  ; also show a "wk" header above the ISO week numbers
          (propertize "wk" 'font-lock-face 'calendar-iso-week-face))
    (global-set-key (kbd "C-c r") 'calendar)
    
    ;; Key sequence "C-x w" kills the the entire content of the buffer, or, if the buffer is narrowed,
    ;; its entire visible content.
    (defun kill-whole-buffer ()
      "Kill the entire content of the buffer, or of the part the buffer is currently narrowed to."
      (interactive)
      (kill-region (point-min) (point-max))
      (message "Buffer contents killed"))
    (global-set-key (kbd "C-x w") 'kill-whole-buffer)
    
    ;; Key sequence "C-c q" joins all the lines of a paragraph into one line. This works as the opposite
    ;; of fill-paragraph, key sequence "M-q".
    (defun unfill-paragraph ()
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))
    (global-set-key (kbd "C-c q") 'unfill-paragraph)
    
    ;; Key sequence "C-c DEL" kills the current buffer without questions, if it's not dirty.
    (defun kill-current-buffer-immediately ()
      (interactive)
      (kill-buffer (buffer-name)))
    (global-set-key (kbd "C-c <deletechar>") 'kill-current-buffer-immediately)
    
    ;; Key sequences "M-<up>" and "M-<down>" scroll the buffer up and down without moving the cursor.
    (defun scroll-down-keep-cursor () (interactive) (scroll-down 1))
    (defun scroll-up-keep-cursor () (interactive) (scroll-up 1))
    (global-set-key (kbd "M-<up>") 'scroll-down-keep-cursor)
    (global-set-key (kbd "M-<down>") 'scroll-up-keep-cursor)
    
    
    ;;;; Unset some key sequences.
    
    ;; Without this command, key sequence "s-q" would run the command save-buffers-kill-emacs.
    (global-unset-key (kbd "s-q"))
    
    ;; Without this command, key sequence "C-x C-z" would run the command suspend-frame.
    (global-unset-key (kbd "C-x C-z"))
    
    
    ;;;; Definitions appropriate for Emacs running as a desktop application, not in a terminal window.
    
    (when (display-graphic-p)
    
      ;;;; General graphic-mode settings.
    
      ;; Disable the toolbar.
      (tool-bar-mode -1)
    
      ;; Set up the frames.
      (setq default-frame-alist
            '((wait-for-wm . nil)          ; speed up initialization
              (top . 30) (left . 400)      ; pixels; this tells where on the display to place the frame
              (width . 150) (height . 60)  ; the width and height of the initial frame in characters
              (line-spacing . 0)
              (screen-gamma . 1.8)))
    
      ;; Key sequence "M-n" toggles the frame-maximized mode.
      (global-set-key (kbd "M-n") 'toggle-frame-maximized)
    
      ;;;; Define key sequences for setting the height of the default face.
    
      ;; Key sequence "C-kp-multiply" sets the default face height to 100.
      (global-set-key (kbd "<C-kp-multiply>")
                      '(lambda ()
                         (interactive)
                         (set-face-attribute 'default nil :height 100)
                         (message "Default face height set to 100")))
    
      ;; Key sequence "C-kp-divide" sets the default face height to 80.
      (global-set-key (kbd "<C-kp-divide>")
                      '(lambda ()
                         (interactive)
                         (set-face-attribute 'default nil :height 90)
                         (message "Default face height set to 90")))
    
      ;; Key sequence "C-kp-add" increases the default face height by 10 units.
      (global-set-key (kbd "<C-kp-add>")
                      '(lambda ()
                         (interactive)
                         (set-face-attribute 'default nil
                                             :height (+ (face-attribute 'default :height) 10))
                         (message "Default face height set to %d" (face-attribute 'default :height))))
    
      ;; Key sequence "C-kp-subtract" dereases the default face height by 10 units.
      (global-set-key (kbd "<C-kp-subtract>")
                      '(lambda ()
                         (interactive)
                         (set-face-attribute 'default nil
                                             :height (- (face-attribute 'default :height) 10))
                         (message "Default face height set to %d" (face-attribute 'default :height))))
    
      ;;;; Tabbar-related definitions.
    
      ;; Show the windows in a tab bar at the top of each window.  Provided by tabbar.el.
      (require 'tabbar)
      (tabbar-mode)                      ; Turn on the tabbar minor mode.
      (defun my-tabbar-buffer-groups ()  ; Customize to show all normal files in one group.
        "Returns the name of the tab group names the current buffer belongs to.
         There are two groups: Emacs buffers (those whose name starts with *, plus
         dired buffers), and the rest. The groups are called 'emacs' and 'user'."
        (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                    ((eq major-mode 'dired-mode) "emacs")
                    (t "user")))
        )
      (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
    
      ;; To prevent Emacs from slowing down, don't use images.
      (setq tabbar-use-images nil)
    
      ;; Tweak the color of the tabs' text and background.
      (set-face-attribute 'tabbar-unselected nil :background "khaki")
      (set-face-attribute 'tabbar-unselected nil :foreground "black")
      (set-face-attribute 'tabbar-selected nil :background "light yellow")
      (set-face-attribute 'tabbar-selected nil :foreground "blue")
    
      (defun tabbar-goto-tab-group (group-name)
        "Jump to a specific tabbar group."
        (unless (and (featurep 'tabbar) tabbar-mode) (error "Error: tabbar-mode not turned on."))
        (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh groups
        (let* ( (groups (mapcar #'(lambda (group) (format "%s" (cdr group)))
                                (tabbar-tabs tabbar-tabsets-tabset))))
          (mapc #'(lambda (group)
                    (when (string= group-name (format "%s" (cdr group)))
                      (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                      (switch-to-buffer (car group))))
                (tabbar-tabs tabbar-tabsets-tabset))))
    
      (defun tabbar-move-current-tab-one-place-left ()
        "Move current tab one place left, unless it's already the leftmost."
        (interactive)
        (let* ((bufset (tabbar-current-tabset t))
               (old-bufs (tabbar-tabs bufset))
               (first-buf (car old-bufs))
               (new-bufs (list)))
          (if (string= (buffer-name) (format "%s" (car first-buf)))
              old-bufs ; the current tab is the leftmost
            (setq not-yet-this-buf first-buf)
            (setq old-bufs (cdr old-bufs))
            (while (and
                    old-bufs
                    (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
              (push not-yet-this-buf new-bufs)
              (setq not-yet-this-buf (car old-bufs))
              (setq old-bufs (cdr old-bufs)))
            (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
                (progn
                  (push (car old-bufs) new-bufs) ; this is the tab that was to be moved
                  (push not-yet-this-buf new-bufs)
                  (setq new-bufs (reverse new-bufs))
                  (setq new-bufs (append new-bufs (cdr old-bufs))))
              (error "Error: current buffer's name was not found in Tabbar's buffer list."))
            (set bufset new-bufs)
            (tabbar-set-template bufset nil)
            (tabbar-display-update))))
    
      (defun tabbar-move-current-tab-one-place-right ()
        "Move current tab one place right, unless it's already the rightmost."
        (interactive)
        (let* ((bufset (tabbar-current-tabset t))
               (old-bufs (tabbar-tabs bufset))
               (first-buf (car old-bufs))
               (new-bufs (list)))
          (while (and
                  old-bufs
                  (not (string= (buffer-name) (format "%s" (car (car old-bufs))))))
            (push (car old-bufs) new-bufs)
            (setq old-bufs (cdr old-bufs)))
          (if old-bufs ; if this is false, then the current tab's buffer name is mysteriously missing
              (progn
                (setq the-buffer (car old-bufs))
                (setq old-bufs (cdr old-bufs))
                (if old-bufs ; if this is false, then the current tab is the rightmost
                    (push (car old-bufs) new-bufs))
                (push the-buffer new-bufs)) ; this is the tab that was to be moved
            (error "Error: current buffer's name was not found in Tabbar's buffer list."))
          (setq new-bufs (reverse new-bufs))
          (setq new-bufs (append new-bufs (cdr old-bufs)))
          (set bufset new-bufs)
          (tabbar-set-template bufset nil)
          (tabbar-display-update)))
    
      ;; Key sequences "C-PgUp" and "C-PgDn" backward and forward between tabs.
      (global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)
      (global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
    
      ;; Key sequences "C-S-PgUp" and "C-S-PgDn" move the current tab to the left and to the right.
      (global-set-key (kbd "C-S-<prior>") 'tabbar-move-current-tab-one-place-left)
      (global-set-key (kbd "C-S-<next>") 'tabbar-move-current-tab-one-place-right)
    
      ;; Key sequence "C-M-PgDn" navigates between Tabbar groups.
      (global-set-key (kbd "C-M-<next>") 'tabbar-forward-group)
    
      ;; On startup, display Emacs as maximized, and split into two side-by-side windows.
      (toggle-frame-maximized)
      (split-window-right)
    
      ) ; (when (display-graphic-p)
    
    
    ;;;; Examples.
    
    ;; An example of how to hook a function to the after-save-hook, and to test for the major mode being
    ;; java-mode inside the function.
    ;
    ; (defun a-test-save-hook-function ()
    ;   "Test of save hook for Java major mode."
    ;   (when (eq major-mode 'java-mode)
    ;     (message "This is the payload function.")))
    ; (add-hook 'after-save-hook 'a-test-save-hook-function)
    
    ;; Makes Emacs display a text of a huge size in a single window, maximized to fill the desktop, when
    ;; key sequence "C-c ?" is typed.
    ;
    ; (global-set-key (kbd "C-c ?") '(lambda () (interactive)
    ;                                  (delete-other-windows)
    ;                                  (toggle-frame-maximized)
    ;                                  (find-file "~/.emacs.d/lunch.txt")
    ;                                  (text-scale-adjust 19)))
