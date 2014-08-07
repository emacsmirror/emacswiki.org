{{{
;;;; file: ~/.emacs.d/init.el
;;;; Teemu Leisti 2014-08-07


;; To see which key sequences are unassigned, command describe-unbound-keys.

;; Use UTF-8 for all character encoding.
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en_GB.UTF-8")
(prefer-coding-system 'utf-8)
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding

(require 'iso-transl) ; to make dead keys work again

;; Select the package archives to use.  (To manage packages, call function list-packages.)
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Add to load-path this directory containing local add-ons.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Set the default major mode.
(setq major-mode 'text-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(c-basic-indent 2)
 '(c-basic-offset 4)
 '(js-indent-level 2)
 '(c-offsets-alist (quote ((inline-open . 0)
                           (statement-case-intro . 0)
                           (statement-case-open . 0)
                           (substatement-open . 0)
                           (case-label . +))))
 '(fci-handle-truncate-lines nil)
 '(fill-column 100)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(line-move-visual nil)
 '(tab-width 4)
 '(vc-make-backup-files t)
 '(list-colors-sort 'hsv)
 '(search-whitespace-regexp nil)
)

;; Turn on the highlight-current-line mode.  Its color is set later in the file, by the command
;; (set-face-background 'hl-line ...).
(global-hl-line-mode 1)

;; Show a vertical line at the fill column, defined as a global minor mode.  Provided by
;; fill-column-indicator.el.
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; Key sequence (C-c DEL) kills the current buffer without questions, if it's not dirty.
(defun kill-current-buffer-immediately ()
  (interactive)
  (kill-buffer (buffer-name)))
(global-set-key (kbd "C-c <deletechar>") 'kill-current-buffer-immediately)

;; Scroll up and down without moving the cursor by pressing the numeric keypad's "/" and "*" keys.
(defun scroll-down-keep-cursor () (interactive) (scroll-down 1))
(global-set-key [kp-divide] 'scroll-down-keep-cursor)
(defun scroll-up-keep-cursor () (interactive) (scroll-up 1))
(global-set-key [kp-multiply] 'scroll-up-keep-cursor)

;; Enable the narrow-to-region command.
(put 'narrow-to-region 'disabled nil)

;; Remove all trailing whitespace when saving buffer.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set the transient mark mode.
(transient-mark-mode 1)

;; Make tab characters visible as a right-guillemet symbol.
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark))
(global-whitespace-mode 1)

;; Set the window title format to show the full path of the file.
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))

;; Tweak the mode line format.
(setq default-mode-line-format
      (list "--:" 'mode-line-modified "- %30b %[(" 'mode-name 'minor-mode-alist
            "%n" 'mode-line-process ")%]--(%l,%c)--" '(-3 . "%p") "-%-"))
(setq mode-line-format default-mode-line-format)

;; Make all yes-or-no questions accept typing just "y" or "n".
(fset 'yes-or-no-p 'y-or-n-p)

;; Define a command to join all the lines of a paragraph into one line, that is, the opposite of
;; fill-paragraph (M-q).  Bind the command to key sequence (S-TAB).
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(global-set-key [backtab] 'unfill-paragraph)

;; No need for adding the encoding line to top of Ruby files in case the file contains non-ASCII
;; characters, since Ruby versions 2 and higher use UTF-8 by default.
(setq ruby-insert-encoding-magic-comment nil)

;; Key sequence (C-u C-SPC) pops the mark from the mark stack.  The following setting enables you to
;; keep unpopping marks by holding down the Ctrl and space keys.
(setq set-mark-command-repeat-pop t)

;; Turn on the Projectile global mode for all buffers.  Projectile helps navigation among the files
;; of a project, including those of Rails projects.
(add-to-list 'load-path "~/.emacs.d/elpa/projectile-0.9.2/")
(require 'projectile)
(projectile-global-mode)                ; turn it on everywhere
(setq projectile-enable-caching t)      ; so native indexing will be faster
(global-set-key (kbd "C-,") 'projectile-find-file)
(global-set-key (kbd "C-.") 'projectile-grep)
(add-hook 'projectile-mode-hook 'projectile-rails-on)  ; use the Projectile Rails minor mode

;; Key sequences (M-up) and (M-down) move the active region, or the current line, up or down.
(require 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

;; Key sequence (C-c c) inserts the "<code> ... </code>" tags around the region.
(fset 'insert-mediawiki-code-tags-around-region "\C-x\C-x<code>\C-x\C-x</code>")
(global-set-key (kbd "C-c c") 'insert-mediawiki-code-tags-around-region)

;; Customize the calendar.  Key sequence (C-c r) opens it.
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

;; This setting makes key sequence (C-w) kill the previous word, instead of killing the region.
;; (The latter can still be done via key sequence (S-DEL).)
(global-set-key (kbd "C-w") 'backward-kill-word)

;; Key sequence (C-c 6) inserts "-- Teemu".
(global-set-key (kbd "C-c 6") '(lambda () (interactive) (insert "-- Teemu")))

;; Key sequence (C-c 7) inserts "-- Teemu " and the current date in ISO format.
(global-set-key (kbd "C-c 7")
                '(lambda ()
                   (interactive)
                   (insert "-- Teemu " (format-time-string "%Y-%m-%d"))))

;; Key sequence (C-c 8) inserts "-- Teemu Leisti".
(global-set-key (kbd "C-c 8") '(lambda () (interactive) (insert "-- Teemu Leisti")))

;; Key sequence (C-c 9) inserts "-- Teemu Leisti " and the current date in ISO format.
(global-set-key (kbd "C-c 9")
                '(lambda ()
                   (interactive)
                   (insert "-- Teemu Leisti " (format-time-string "%Y-%m-%d"))))

;; Key sequence (C-c 0) inserts the current date in ISO format.
(global-set-key (kbd "C-c 0") '(lambda () (interactive) (insert (format-time-string "%Y-%m-%d"))))

;; Key sequence (C-c d) opens file ~/misc/todo.txt.
(global-set-key (kbd "C-c d") '(lambda () (interactive) (find-file "~/misc/todo.txt")))

;; Key sequence (C-c e) opens the initialization file,  ~/.emacs.d/init.el.
(global-set-key (kbd "C-c e") '(lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; Key sequence (C-c t) opens file ~/tmp/tmp.txt.
(global-set-key (kbd "C-c t") '(lambda () (interactive) (find-file "~/tmp/tmp.txt")))

;; Key sequence (C-c i) starts opening a file in directory ~/code/myproject.
(fset 'start-opening-myproject-file "\C-x\C-f\C-a\C-k~/code/myproject/")
(global-set-key (kbd "C-c i") '(lambda ()
                                 (interactive)
                                 (execute-kbd-macro (symbol-function 'start-opening-myproject-file))))

;; Key sequence (C-ö) sets the fill column to 80.
(global-set-key (kbd "C-ö")
                '(lambda ()
                   (interactive)
                   (setq fill-column 80)
                   (message "Fill column set to 80.")))

;; Key sequence (C-ä) sets the fill column to 100.
(global-set-key (kbd "C-ä")
                '(lambda ()
                   (interactive)
                   (setq fill-column 100)
                   (message "Fill column set to 100.")))

;; Key sequence (C-') leaves only the active window open.
(global-set-key (kbd "C-'") '(lambda () (interactive) (delete-other-windows)))

;; Key sequence (C-+) enlarges the current window by one row, providing an easier key sequence than
;; (C-x ^), which it's bound to by default.
(global-set-key (kbd "C-+") 'enlarge-window)

;; Key sequence (C-å) shrinks the current window by one row.
(global-set-key (kbd "C-å") 'shrink-window)

;; Redefine a couple of key sequences to simply display a message: C-z because otherwise it runs the
;; command suspend-frame, which annoyingly minimizes the window; and C-S-DEL because I need to
;; untrain myself from using it for triggering kill-current-buffer-immediately.
(defun echo-does-not-work-message () (interactive) (message "That button does not work."))
(global-set-key (kbd "C-z") 'echo-does-not-work-message)
(global-set-key [C-S-delete] 'echo-does-not-work-message)


;;;; The following definitions are appropriate for Emacs running on a window system, not in a
;;;; terminal window.

(when (display-graphic-p)

  (set-frame-size (selected-frame) 130 60) ; columns, lines

  ;; Set up the frames.
  (setq default-frame-alist
        '((wait-for-wm . nil)          ; speed up initialization
          (top . 30) (left . 150)      ; pixels; this tells where on the display to place the frame
          (background-color . "cornsilk")
          (foreground-color . "black")
          (line-spacing . 0)
          (screen-gamma . 1.8)
          (font . "-unknown-Liberation Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
           ; in case the preceding doesn't work, try one of these:
           ; (font . "-unknown-Droid Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
           ; (font . "-b&h-lucidatypewriter-bold-*-*-*-14-*-*-*-*-*-iso8859-*")
  ))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight ((t (:background "moccasin"))))
   '(isearch ((t (:background "chocolate" :foreground "white"))))
   '(lazy-highlight ((t (:background "chocolate" :foreground "white"))))
   '(mode-line ((t (:background "darkorange4" :foreground "white smoke" :box (:line-width -1 :style released-button)))))
   '(mode-line-inactive ((t (:background "gray16" :foreground "white smoke" :box (:line-width -1 :style released-button)))))
   '(region ((t (:background "yellow" :foreground "black"))))
   '(secondary-selection ((t (:background "dark salmon"))))
   '(warning ((t (:foreground "orange red" :weight bold)))))

  ;; Key sequence (C-?) toggles the frame between maximum and minimum, with a puzzling
  ;; almost-vertical-maximum mode in between.
  (defun x-maximize-frame ()
    "Maximize the current frame (to full screen)"
    (interactive)
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
    (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
  (global-set-key (kbd "C-?") 'x-maximize-frame)

  ;; Show the windows in a tab bar at the top of each window.  Provided by tabbar.el.
  (require 'tabbar)
  (tabbar-mode)                      ; Turn on the tabbar minor mode.
  (defun my-tabbar-buffer-groups ()  ; Customize to show all normal files in one group.
    "Returns the name of the tab group names the current buffer belongs to.
     There are two groups: Emacs buffers (those whose name starts with “*”, plus
     dired buffers), and the rest."
    (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
                ((eq major-mode 'dired-mode) "emacs")
                (t "user")))
    )
  (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
  ;; Tweak the text color of the tabs, for easier visibility.
  (set-face-attribute 'tabbar-default nil :height 1.1)
  (set-face-attribute 'tabbar-unselected nil :background "khaki")
  (set-face-attribute 'tabbar-unselected nil :foreground "black")
  (set-face-attribute 'tabbar-selected nil :background "cornsilk")
  (set-face-attribute 'tabbar-selected nil :foreground "black")
  ;; Bind tab navigation to key sequences C-PgUp and C-PgDn.
  (global-set-key [C-prior] 'tabbar-backward-tab)  ; previous tab: C-PgUp
  (global-set-key [C-next]  'tabbar-forward-tab)   ; next tab:     C-PgDn
  ;; Add this hook to get rid of the slight but annoying extra spacing between lines Tabbar
  ;; sometimes introduces when a file is opened.  Adding the hook is equivalent to always pressing
  ;; C-L after opening a file.
  (add-hook 'find-file-hook 'recenter-top-bottom)
)


;; This is an example of how to hook a function to the after-save-hook,
;; conditional on the major mode being java-mode.
;
; (defun a-test-save-hook()
;   "Test of save hook"
;   (when (eq major-mode 'java-mode)
;     (message "this is the payload function")))
; (add-hook 'after-save-hook 'a-test-save-hook)
;
;; Further examples of adding hooks.
;
; ;; Define functions to be used in individual mode definitions.
; (defun fill-column-to-100() (setq fill-column 100))
; (defun use-spaces-instead-of-tabs() (setq indent-tabs-mode nil))
; ;; When in the Java, Ruby, or HTML modes:
; ;; Set the maximum line length to 100.
; (add-hook 'java-mode-hook 'fill-column-to-100)
; (add-hook 'ruby-mode-hook 'fill-column-to-100)
; (add-hook 'html-mode-hook 'fill-column-to-100)
; ;; Use tabs instead of spaces.
; (add-hook 'java-mode-hook 'use-spaces-instead-of-tabs)
; (add-hook 'ruby-mode-hook 'use-spaces-instead-of-tabs)
; (add-hook 'html-mode-hook 'use-spaces-instead-of-tabs)
}}}
