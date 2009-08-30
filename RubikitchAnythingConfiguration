;;;
;; $Id: 99anything.el,v 1.429 2009/02/24 11:20:18 rubikitch Exp $

;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/RubikitchAnythingConfiguration
;; Site: http://www.emacswiki.org/cgi-bin/emacs/Anything

;; install-elisp.el is in the EmacsWiki.
;; http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el

;; This file is delimited by linkd tags.
;; http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el

;; The definition of with-new-window is here. It depends on windows.el.
;; (@* "with-new-window definition")
;; copy from 29windows.el
(unless (fboundp 'with-new-window)
  ;; (install-elisp "http://www.gentei.org/~yuuji/software/windows.el")
  (require 'windows)
  (defun win:insert-config (idx)
    (let (i (vector win:configs))
      (setq i 1)
      (while (and (< i (1- (length vector)))
                  (aref vector i))
        (setq i (1+ i)))
      (assert (eq (aref vector i) nil))
      (while (< idx i)
        (win:copy-config  (- i 1) i)
        (setq i (1- i)))
      (aset win:configs idx nil)
      (aset win:names idx nil)
      (aset win:names-prefix idx "")
      (aset win:sizes idx nil)
      (aset win:buflists idx (make-vector win:buffer-depth-per-win nil))
      (aset win:points idx nil)))
  (defmacro with-new-window (&rest body)
    `(let ((i (win:find-new-window)))
       (win:store-config win:current-config)
       (win:insert-config i)
       (win:switch-window i nil t)
       (delete-other-windows)
       ,@body
       (win:store-config i)))
  (defun win:squeeze-config ()
    (interactive)
    (let (idxs j k (limit win-rotate-by))
      (setq j 1)
      (while (<= j limit)
        (when (aref win:configs j)
          (setq  idxs (cons j idxs)))
        (and (eq j win:current-config) (setq win:current-config (length idxs)))
        (and (eq j win:last-config) (setq win:last-config (length idxs)))
        (setq j (1+ j)))
      (setq idxs (reverse idxs))

      (setq j 1)
      (mapcar (lambda (k)
                (win:copy-config k j)
                (setq j (1+ j)))
              idxs)
      (setq k j)
      (while (<= k limit)
        (aset win:configs k nil)
        (aset win:names k nil)
        (aset win:names-prefix k "")
        (aset win:sizes k nil)
        (aset win:buflists k (make-vector win:buffer-depth-per-win nil))
        (aset win:points k nil)
        (setq k (1+ k))
        )
      (win:store-config win:current-config)
      ))
  )


;; [2007/07/25]
;; (install-elisp-from-emacswiki "anything-config")
;; (install-elisp-from-emacswiki "anything")
(require 'anything-config)
(require 'anything)
(setq anything-save-configuration-functions
      '(set-window-configuration . current-window-configuration))
(setq anything-idle-delay 0.3)
(setq anything-input-idle-delay 0)
(setq anything-candidate-number-limit 100)
(setq anything-c-locate-db-file "/log/home.simple.locatedb")
(setq anything-c-locate-options `("locate" "-d" ,anything-c-locate-db-file "-i" "-r" "--"))
(setq anything-candidate-separator
      "------------------------------------------------------------------------------------")
(setq anything-persistent-action-use-special-display t)
(add-hook 'anything-after-persistent-action-hook 'which-func-update)
;; adaptive sort file is buggy
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
;; [2008/01/02]
;;(view-elinit "sticky" "anything-map")

;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
(require 'anything-c-yasnippet)         ;[2008/03/25]
;; (install-elisp-from-emacswiki "anything-gtags.el")
(require 'anything-gtags)               ;[2008/08/15]
(setq anything-gtags-classify t)

;; (install-elisp-from-emacswiki "anything-ipa.el")
(require 'anything-ipa)                 ;[2009/02/13]
;; (install-elisp-from-emacswiki "anything-etags.el")
(require 'anything-etags)               ;[2009/01/30]
;; (install-elisp-from-emacswiki "anything-match-plugin.el")
(require 'anything-match-plugin)        ;[2008/08/20]
(setq anything-mp-space-regexp "[\\ ] ")
;; (install-elisp-from-emacswiki "anything-auto-install.el")
(require 'anything-auto-install)
(require 'anything-yaoddmuse)
;;
(require 'anything-grep)
(setq anything-grep-goto-hook
      (lambda ()
        (when anything-in-persistent-action
          (anything-persistent-highlight-point (point-at-bol) (point-at-eol)))))
(defalias 'ag 'anything-grep)
(load "anything-grep-alist.el")
(setq anything-grep-filter-command  "ruby -rkconv -pe '$_.replace $_.toeuc'")
(setq anything-grep-find-file-function 'anything-grep-find-file)

;; [2008/10/30] improved version is in anything-complete.el
(defun anything-grep-find-file (&rest args)
  (if anything-in-persistent-action
      (apply 'find-file args)
    (with-new-window (apply 'find-file args))))
(setq anything-find-file-additional-sources
      '(anything-c-source-home-directory))
;; (read-directory-name "dir: " nil nil t)
;; (@* "eijiro")
;; depends anything-grep
(defvar anything-c-source-eijiro 
  '((name . "Eijiro")
    (query)
    (init . anything-eijiro-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (get-line . buffer-substring)
    (migemo)))

(defvar eijiro-command
  "egrep --color=always -ih %s eijiro75.euc otojiro.euc reiji75.euc ryaku75.euc | cut -c3-")
(defvar eijiro-directory "/home/dict")
(defun anything-eijiro-init ()
  (with-current-buffer
      (agrep-create-buffer
       (format eijiro-command (shell-quote-argument (anything-attr 'query)))
       eijiro-directory)
    (anything-eijiro-relocate)))

(defun anything-eijiro-relocate ()
  (goto-char 1)
  (insert "\n")
  (let ((s (make-marker))
        (e (make-marker))
        (pattern (concat "\n" (anything-attr 'query))))
    (when (search-forward pattern nil t)
      (set-marker s (point-at-bol))
      (goto-char (point-max))
      (search-backward pattern nil t)
      (forward-line 2)
      (set-marker e (point))
      (goto-char 2)
      (insert-buffer-substring (current-buffer) s e)
      (delete-region s e)
      (delete-region 1 2))))

(defun eijiro (query)
  (interactive "sEijiro: ")
  (let ((anything-input-idle-delay anything-idle-delay))
    (anything-attrset 'query query anything-c-source-eijiro)
    (anything 'anything-c-source-eijiro)))
(defun eijiro-at-point ()
  (interactive)
  (eijiro (word-at-point)))

;; (@* "reset anything.el state")
(defun anything-reset ()
  "Clear internal state of `anything'."
  (interactive)
  (loop for v in '(anything-candidate-buffer-alist anything-candidate-cache
                   anything-saved-selection anything-current-buffer
                   anything-saved-selection anything-saved-action
                   anything-buffer-file-name anything-current-position
                   anything-last-sources anything-saved-current-source
                   anything-compiled-sources)
        do (set v nil))
  (loop for b in (buffer-list)
        for name = (buffer-name b)
        when (string-match "^ ?\\*anything" name)
        do (kill-buffer b)))

;; (@* "keymaps")
;; (find-ekeymapdescr anything-map)
;;(define-key anything-map "\C-k" 'anything-select-action-in-minibuffer)
(define-key anything-map "\C-k" (lambda () (interactive) (delete-minibuffer-contents)))
(setq anything-map-C-j-binding 'anything-select-3rd-action)
(define-key anything-map "\C-j" anything-map-C-j-binding)
(define-key anything-map "\C-e" 'anything-select-2nd-action-or-end-of-line)
(define-key anything-map "\M-N" 'anything-next-source)
(define-key anything-map "\M-P" 'anything-previous-source)
(define-key anything-map "\C-\M-n" 'anything-next-source)
(define-key anything-map "\C-\M-p" 'anything-previous-source)
(define-key anything-map "\C-s" 'anything-isearch)
(define-key anything-map "\C-p" 'anything-previous-line)
(define-key anything-map "\C-n" 'anything-next-line)
(define-key anything-map "\C-v" 'anything-next-page)
(define-key anything-map "\M-v" 'anything-previous-page)
(define-key anything-map "\C-z" 'anything-execute-persistent-action)
(define-key anything-map "\C-i" 'anything-select-action)
(define-key anything-map "B" 'anything-insert-buffer-name)
(define-key anything-map "R" 'anything-show/rubyref)
(define-key anything-map "C" 'anything-show/create)
;;(define-key anything-map "\C-k" 'anything-show/create)
(define-key anything-map "\C-b" 'anything-backward-char-or-insert-buffer-name)
(define-key anything-map "\C-o" 'anything-next-source)
(define-key anything-map "\C-\M-v" 'anything-scroll-other-window)
(define-key anything-map "\C-\M-y" 'anything-scroll-other-window-down)
;; [2008/04/02]
(define-key anything-map [end] 'anything-scroll-other-window)
(define-key anything-map [home] 'anything-scroll-other-window-down)
(define-key anything-map [next] 'anything-next-page)
(define-key anything-map [prior] 'anything-previous-page)
(define-key anything-map [delete] 'anything-execute-persistent-action)
;; [2008/08/22]
(define-key anything-map (kbd "C-:") 'anything-for-create-from-anything)

;; (@> " frequently used commands - keymap")
(define-key anything-isearch-map "\C-m"  'anything-isearch-default-action)

(setq anything-enable-digit-shortcuts nil)
(define-key anything-map (kbd "M-1") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-2") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-3") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-4") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-5") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-6") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-7") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-8") 'anything-select-with-digit-shortcut)
(define-key anything-map (kbd "M-9") 'anything-select-with-digit-shortcut)

(define-key anything-map "N" 'anything-next-visible-mark)
(define-key anything-map "P" 'anything-prev-visible-mark)
(define-key anything-map (kbd "C-SPC") 'anything-toggle-visible-mark)
(define-key anything-map "M" 'anything-toggle-visible-mark)
(define-key anything-map "Y" 'anything-yank-selection)
(define-key anything-map "\M-[" 'anything-prev-visible-mark)
(define-key anything-map "\M-]" 'anything-next-visible-mark)

(define-key anything-map "\C-a" 'beginning-of-line)


;; [2008/07/28] (@* "anything-c-moccur")
;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
(require 'anything-c-moccur)
(setq anything-c-moccur-enable-initial-pattern t)
(setq anything-c-moccur-anything-idle-delay 0.1)
(defalias 'aoccur 'anything-c-moccur-occur-by-moccur)

;; (@* " mode-line indicator")
(defvar anything-c-moccur-original-mode-line-format nil)
(defadvice anything-c-moccur-initialize (after set-mode-line-format activate)
  ""
  (setq anything-c-moccur-original-mode-line-format mode-line-format)
  (with-current-buffer (get-buffer-create anything-buffer)
    (setq mode-line-format
          '(" Function Comment String D:Symbol(U:start I:end) Word(O:start P:end)"))))
;; (progn (ad-disable-advice 'anything-c-moccur-initialize 'after 'set-mode-line-format) (ad-update 'anything-c-moccur-initialize)) 

(defadvice anything-c-moccur-clean-up (after set-mode-line-format activate)
  (with-current-buffer anything-buffer
    (setq mode-line-format anything-c-moccur-original-mode-line-format)))
;; (progn (ad-disable-advice 'anything-c-moccur-clean-up 'after 'set-mode-line-format) (ad-update 'anything-c-moccur-clean-up)) 

;; (@* " isearch hack")
;; I do not prefer anything-c-moccur-enable-initial-pattern when anything-c-moccur-isearch-*
(defadvice anything-c-moccur-isearch-forward (around anything-c-moccur-enable-initial-pattern activate)
  (let (anything-c-moccur-enable-initial-pattern) ad-do-it))
;; (progn (ad-disable-advice 'anything-c-moccur-isearch-forward 'around 'anything-c-moccur-enable-initial-pattern) (ad-update 'anything-c-moccur-isearch-forward)) 
(defadvice anything-c-moccur-isearch-backward (around anything-c-moccur-enable-initial-pattern activate)
  (let (anything-c-moccur-enable-initial-pattern) ad-do-it))
;; (progn (ad-disable-advice 'anything-c-moccur-isearch-backward 'around 'anything-c-moccur-enable-initial-pattern) (ad-update 'anything-c-moccur-isearch-backward)) 

;; [2008/01/14]
;; (install-elisp-from-emacswiki "anything-dabbrev-expand.el")
(require 'anything-dabbrev-expand)
(define-key anything-dabbrev-map [(control ?@)] 'anything-dabbrev-find-all-buffers)
(setq anything-dabbrev-input-idle-delay 0.0)
(setq anything-dabbrev-idle-delay 1.0)
(setq anything-dabbrev-expand-candidate-number-limit 20)
(setq anything-dabbrev-expand-strategies
      '(;; anything-dabbrev-expand--first-partial-dabbrev
        anything-dabbrev-expand--anything))
(setq anything-dabbrev-sources
      '(anything-dabbrev-partial-source
        anything-c-source-complete-emacs-commands
        anything-c-source-complete-emacs-functions
        anything-c-source-complete-emacs-variables
        anything-c-source-complete-emacs-other-symbols
        anything-dabbrev-all-source))

;; (install-elisp-from-emacswiki "anything-complete.el")
(require 'anything-complete)
(anything-read-string-mode 1)
(anything-lisp-complete-symbol-set-timer 150)
(setq anything-lisp-complete-symbol-input-idle-delay 0.0)
;; (install-elisp "http://www4.atpages.jp/loveloveelisp/anything-c-source-buffers2.el")
(require 'anything-c-source-buffers2)
(setq anything-c-buffer-ignore-regexp-list
  '(anything-buffer "*Completions*" ))

;; (install-elisp "http://www4.atpages.jp/loveloveelisp/anything-goodies.el")
;; (install-elisp "http://www4.atpages.jp/loveloveelisp/lib/anything-c-linkd-tags.el")
(require 'anything-c-linkd-tags)
(setq anything-c-source-linkd-candidate-cache nil)
(make-variable-buffer-local 'anything-c-source-linkd-candidate-cache)
(defun anything-linkd-goto (selection)
  '(switch-to-buffer anything-current-buffer)
  (if (not (listp selection))
      (goto-char selection)
    (goto-char (car anything-current-position)))
  (set-window-start (get-buffer-window anything-current-buffer) (point))
  (when anything-in-persistent-action
    (anything-persistent-highlight-point
     (line-beginning-position)
     (line-end-position))))

'        (obsolete
         (persistent-action . (lambda (selection)
                                (switch-to-buffer anything-current-buffer)
                                (if (not (listp selection))
                                    (goto-char selection)
                                  (goto-char (car anything-current-position)))
                                (set-window-start (get-buffer-window anything-current-buffer) (point))
                                (anything-persistent-highlight-point
                                 (line-beginning-position)
                                 (line-end-position))))
         (action . (lambda (overlay)
                     (goto-char (overlay-start overlay))
                     (set-window-start (get-buffer-window anything-current-buffer) (point))))
                 (action-transformer . (lambda (actions selection)
                                (if (not (listp selection))
                                    (lambda (point)
                                      (goto-char point)
                                      (linkd-next-link))
                                  (lambda (tag)
                                    (goto-char (car anything-current-position))
                                    (call-interactively (key-binding "\M-\;"))
                                    (insert (format "(@%s \"%s\")"
                                                    (case (car tag)
                                                      ('link ">")
                                                      ('star "*"))
                                                    (cdr tag))))))))


(setq anything-c-source-linkd-tag
      '((name . "linkd tags")
        (init . (lambda ()
                  (when nil
                    (linkd-do-font-lock 'font-lock-add-keywords)
                    (font-lock-fontify-buffer)
                    (or buffer-read-only (linkd-mode -1))
                    (linkd-mode 1))))
        (action . anything-linkd-goto)
        (persistent-action . anything-linkd-goto)


        ;; I prefer to another source to create new linkd tag.
        ;; TODO candidate cache framework
        (candidates . (lambda ()
                        (with-current-buffer anything-current-buffer
                          (if (or (anything-current-buffer-is-modified)
                                  (not anything-c-source-linkd-candidate-cache))
                              (setq anything-c-source-linkd-candidate-cache
                                    (anything-c-linkd-tag-get-list))
                            anything-c-source-linkd-candidate-cache))))))

;; [2008/03/28]
;; TODO user-option
(defadvice anything-move-selection (after screen-top activate)
  "Display at the top of window when moving selection to the prev/next source."
  (if (eq unit 'source)
      (save-selected-window
        (select-window (get-buffer-window anything-buffer 'visible))
        (set-window-start (selected-window)
                          (save-excursion (forward-line -1) (point))))))
;; (progn (ad-disable-advice 'anything-move-selection 'after 'screen-top) (ad-update 'anything-move-selection))

;; [2007/09/28] (@* "skk")
;; (view-elinit "skk" "(defvar minibuffer-use-skk nil")
(defun anything-skk (with-skk)
  (interactive "P")
  (let ((minibuffer-use-skk with-skk))
    (define-key anything-map "\C-j" anything-map-C-j-binding)
    (anything)))

;; [2008/01/26]
(defadvice anything (before restore-C-j activate)
  (define-key anything-map "\C-j" anything-map-C-j-binding))


;; (@* "migemo")
;; (install-elisp-from-emacswiki "anything-migemo.el")
(require 'anything-migemo)

;; (@* "describe-bindings")
(require 'descbinds-anything)
(setq descbinds-anything-actions
      '(("Execute" . descbinds-anything-action:execute)
        ("Find Function" . descbinds-anything-action:find-func)
        ("Describe Function" . descbinds-anything-action:describe)))
;; REDEFINED
(defun descbinds-anything-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (list
      (cons 'name (car section))
      (cons 'candidates (cdr section))
      (cons 'candidate-transformer
	    (lambda (candidates)
	      (mapcar
	       (lambda (pair)
		 (cons (funcall descbinds-anything-candidate-formatter
				(car pair) (cdr pair))
		       (cons (car pair)
			     (intern-soft (cdr pair)))))
	       candidates)))
      (cons 'action descbinds-anything-actions)
      (cons 'action-transformer
	    (lambda (actions candidate)
	      (and (commandp (cdr candidate))
		   actions)))
      (cons 'persistent-action
            'descbinds-anything-action:describe)))
   (descbinds-anything-all-sections buffer prefix menus)))
(descbinds-anything-install)

;; (@* "candidate-transformer for buffers")
(defvar anything-c-boring-buffer-regexp
  (rx (or 
       ;; because of switch commands (@> "switch commands")
       "tvprog-keyword.txt" "tvprog.html" ".crontab" "+inbox"
       ;; internal use only
       "*windows-tab*"
       ;; caching purpose
       "*refe2x:" "*refe2:" "ri `"
       ;; echo area
       " *Echo Area" " *Minibuf")))

(defun anything-c-skip-boring-buffers (buffers)
  (remove-if (lambda (buf) (and (stringp buf) (string-match anything-c-boring-buffer-regexp buf)))
             buffers))

(defun anything-c-skip-current-buffer (buffers)
  (remove (buffer-name anything-current-buffer) buffers))

(defun anything-c-transform-navi2ch-article (buffers)
  (loop for buf in buffers collect
        (if (string-match "^\\*navi2ch article" buf)
            (cons (with-current-buffer buf
                    (format "[navi2ch:%s]%s" 
                            (cdr (assq 'id navi2ch-article-current-board))
                            (navi2ch-article-get-current-subject)))
                  buf)
          buf)))

;; (@* "candidate-transformer for files")
(defun anything-c-skip-opened-files (files)
  (set-difference files
                  (mapcan (lambda (file) (list file (abbreviate-file-name file)))
                          (delq nil (mapcar #'buffer-file-name (buffer-list))))
                  :test #'string=))

(add-to-list 'anything-c-source-file-cache '(candidate-transformer . anything-c-skip-opened-files))

;; [2007/12/25] (@* "action extension")
(defun anything-c-action-replace (source new-action)
  (setf (cdr (assq 'action (symbol-value source))) new-action)
  (symbol-value source))

(defun anything-c-action-extend (description function)
  `((,(concat description " (new window)") . (lambda (c) (with-new-window (,function c))))
    (,description . ,function)
    (,(concat description " (other window)") . (lambda (c)
                                                 (when (one-window-p)
                                                   (select-window (split-window)))
                                                 (,function c)))))

(anything-c-action-replace
 'anything-c-source-man-pages
 (anything-c-action-extend "Show with Woman" 'woman))

;; (anything 'anything-c-source-man-pages)

(defun anything-c-info (node-str)
  (info (replace-regexp-in-string "^[^:]+: " "" node-str)))

(anything-c-action-replace
 'anything-c-source-info-pages
 (anything-c-action-extend "Show with Info" 'anything-c-info))
;; (anything 'anything-c-source-info-pages)

' (anything-c-action-replace
 'anything-c-source-bookmarks
 (append                                ;[2007/12/30]
  (anything-c-action-extend "Jump to Bookmark" 'bookmark-jump)
  '(("Update Bookmark" . bookmark-set)
    ("Delete Bookmark" . bookmark-delete))))

;;;; (@* "modification of sources")
(dolist (v '(anything-c-source-apropos-emacs-commands
             anything-c-source-apropos-emacs-functions
             anything-c-source-apropos-emacs-variables))
  (add-to-list v '(requires-pattern . 4))
  (add-to-list v '(major-mode emacs-lisp-mode)))
(add-to-list 'anything-c-source-buffers '(migemo))
(add-to-list 'anything-c-source-buffers
             '(persistent-action . (lambda (name)
                           (flet ((kill (item)
                                    (kill-buffer item)
                                    (anything-delete-current-selection)
                                    (message "Buffer <%s> Killed" item))
                                  (goto (item)
                                    (switch-to-buffer item)))
                             (if current-prefix-arg
                                 (kill name)
                                 (goto name))))))


;; (position-if 'oddp '(2 3 4))

;; [2008/10/15]
(defun anything-yank-selection ()
  (interactive)
  (delete-minibuffer-contents)
  (insert (anything-get-selection)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  (@* "my extension: select other actions by key")                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar anything-select-in-minibuffer-keys "\C-m\C-a\C-s\C-k\C-d\C-f\C-j\C-l")


;; ;; It is based on anything-select-action, so it needs refactoring.
;; (defun anything-select-action-in-minibuffer ()
;;   "Select an action for the currently selected candidate in minibuffer."
;;   (interactive)
;;   (if anything-saved-sources
;;       (error "Already showing the action list"))

;;   (setq anything-saved-selection (anything-get-selection))
;;   (unless anything-saved-selection
;;     (error "Nothing is selected."))

;;   (let ((actions (anything-get-action)))
;;     (message "%s" (apply #'concat
;;                          (loop for action in actions
;;                                for i from 0 collecting
;;                                (format "[%c]%s\n"
;;                                        (elt anything-select-in-minibuffer-keys i)
;;                                        (car action)))))
;;     (let* ((key (read-char))
;;            (idx (rindex anything-select-in-minibuffer-keys key)))
;;       (or idx (error "bad selection"))
;;       (setq anything-saved-action (cdr (elt actions idx)))
;;       (anything-exit-minibuffer))))

(defun anything-select-2nd-action-or-end-of-line ()
  (interactive)
  (if (eolp)
      (anything-select-nth-action 1)
    (end-of-line)))

(define-key anything-map "\C-r" 'anything-call-source-from-anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; [2008/08/21] (@* "my plug-ins")                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; [2007/12/25] (@* "my sources")                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [2007/08/05] (@* " switch commands")
(defvar anything-c-switch-commands nil)
(defvar anything-c-source-switch-commands
  '((name . "Switch Commands")
    (candidates . anything-c-switch-commands)
    (type . command-ext)))

(defmacro define-switch-command (command &rest body)
  `(progn
     (defun ,command ()
       (interactive)
       ,@body)
     (defun ,(intern (concat "win-" (symbol-name command))) ()
       (interactive)
       (with-new-window (,command)))
     (add-to-list 'anything-c-switch-commands (symbol-name ',command))))

(define-switch-command tvprog-cron-all
  (find-file "/log/tvprog.html")
  (find-file-other-window "~/.crontab")
  (other-window 1))

(define-switch-command tvprog-cron
  (find-file "/log/tvprog-keyword.txt")
  (find-file-other-window "~/.crontab")
  (other-window 1))

(define-switch-command inbox
  (mew))

(define-switch-command navi2ch-session
  (unless navi2ch-opened-urls-loaded
    (navi2ch-load-opened-urls)
    (setq navi2ch-opened-urls-loaded t))
  (setq anything-c-switch-commands
        (delete "navi2ch-session" anything-c-switch-commands)))

(add-to-list 'anything-c-switch-commands "hatena")
(add-to-list 'anything-c-switch-commands "emacswiki")
(add-to-list 'anything-c-switch-commands "add-mode-specific-abbrev")
(add-to-list 'anything-c-switch-commands "add-global-abbrev")


;; [2008/01/07] (@* " candidates from file")
;; [2008/01/07] (@* "  rubylib-18")
;; (view-mybinfile "cronjob.sh.r" "/m/home/rubikitch/bin/ruby-libraries")
(defvar anything-c-source-rubylib-18
  '((name . "Ruby 1.8 libraries")
    (candidates-file "/log/ruby-libraries.18.filelist" updating)
    (requires-pattern . 4)
    (type . file)
    (major-mode ruby-mode el4r-mode)))
;; (anything 'anything-c-source-rubylib-18)
;; [2008/01/07] (@* "  rubylib-19")
(defvar anything-c-source-rubylib-19
  '((name . "Ruby 1.9 libraries")
    (candidates-file "/log/ruby-libraries.19.filelist" updating)
    (requires-pattern . 4)
    (type . file)
    (major-mode ruby-mode el4r-mode)))
;; [2008/08/05] (@* "  ruby18-src")
(defvar anything-c-source-ruby18-source
  '((name . "Ruby 1.8 source")
    (candidates-file "/log/rubysrc-files.18.filelist" updating)
    (requires-pattern . 4)
    (type . file)
    (major-mode ruby-mode el4r-mode)))
;; [2008/08/05] (@* "  ruby19-src")
(defvar anything-c-source-ruby19-source
  '((name . "Ruby 1.9 source")
    (candidates-file "/log/rubysrc-files.19.filelist" updating)
    (requires-pattern . 4)
    (type . file)
    (major-mode ruby-mode el4r-mode)))
;; [2008/08/14] (@* "  home-directory")
(defvar anything-c-source-home-directory
  '((name . "Home directory")
    (candidates-file "/log/home.filelist" updating)
    (requires-pattern . 5)
    (candidate-number-limit . 20)
    (type . file)))
;; [2008/09/04] (@* "  compile-directory")
(defvar anything-c-source-compile-directory
  '((name . "Compile directory")
    (candidates-file "/log/compile.filelist" updating)
    (requires-pattern . 8)
    (candidate-number-limit . 20)
    (delayed)
    (type . file)))
;; (@* "  find-library")
;; (anything 'anything-c-source-find-library)
(defvar anything-c-source-find-library
  '((name . "Elisp libraries")
    (candidates-file "/log/elisp.filelist" updating)
    (requires-pattern . 4)
    (type . file)
    (major-mode emacs-lisp-mode)))

;; (@* " directory-files")
(defun anything-c-transform-file-name-nondirectory (files)
  (mapcar (lambda (f) (cons (file-name-nondirectory f) f)) files))
(defun anything-c-source-files-in-dir (desc dir &optional match skip-opened-file)
  `((name . ,desc)
    (candidates . (lambda () (directory-files ,dir t ,match)))
    (candidate-transformer
     . (lambda (candidates)
         (anything-c-compose (list candidates)
                             '(,@(if skip-opened-file (list 'anything-c-skip-opened-files))
                               anything-c-transform-file-name-nondirectory))))
    (type . file)))

;; (@* "  elinit")
(setq anything-c-source-elinit
      (anything-c-source-files-in-dir
       "Emacs init files" "~/emacs/init.d/" "^_?[0-9]+.+\.el$"))
;; (anything 'anything-c-source-elinit)



;; (@* " escript-link")

;; (@* "  langhelp-ruby")
(defvar anything-c-source-langhelp-ruby
  '((name . "Langhelp ruby")
    (candidates-file . "~/.langhelp/ruby.e")
    (type . escript)
    (requires-pattern . 5)
    (category rubyref)
    (major-mode ruby-mode el4r-mode)))

;; (anything 'anything-c-source-langhelp-ruby)
;; (@* "  refe2x")
;; (anything 'anything-c-source-refe2x)
(defvar anything-c-source-refe2x
  '((name . "ReFe2x")
    (candidates-file . "~/compile/ruby-refm-1.9.0-dynamic/bitclust/refe2x.e")
    (type . escript)
    (requires-pattern . 4)
    (category rubyref)
    (major-mode ruby-mode el4r-mode)
    (candidate-number-limit . 800)))
;; (anything 'anything-c-source-refe2x)

;; (@* "ri")
(defvar anything-c-source-ri
  '((name . "Ri")
    (candidates-file . "/log/fastri.e")
    (type . escript)
    (requires-pattern . 4)
    (category rubyref)
    (major-mode ruby-mode el4r-mode)
    (candidate-number-limit . 800)))
;; (anything 'anything-c-source-ri)
;;(find-epp (anything-compile-sources '(anything-c-source-refe2x) anything-compile-source-functions))
;; (@* "  music")
(defvar anything-c-source-music
  '((name . "Music")
    (candidates-file . "~/.music.list")
    (type . escript)
    (migemo)
    (requires-pattern . 4)))
;; (anything 'anything-c-source-music)


;; [2008/01/07] (@* " imenu (improved)")
;; (install-elisp "http://www4.atpages.jp/loveloveelisp/anything-c-imenu.el")
(require 'anything-c-imenu)
;; (anything 'anything-c-source-imenu)

;; [2007/12/31] (@* " rake-task")
(defvar anything-c-source-rake-task
  '((name . "Rake Task")
    (candidates
     . (lambda ()
         (when (string-match "^rake" anything-pattern)
           (cons '("rake" . "rake")
                 (mapcar (lambda (line)
                           (cons line (car (split-string line " +#"))))
                         (with-current-buffer anything-current-buffer
                           (split-string (shell-command-to-string "rake -T") "\n" t)))))))
    (action ("Compile" . compile)
            ("Compile with command-line edit"
             . (lambda (c) (let ((compile-command (concat c " ")))
                             (call-interactively 'compile)))))
    (requires-pattern . 4)))
;; (anything 'anything-c-source-rake-task)

;; [2007/12/31] (@* " tvavi")
;; FIXME
(defvar anything-c-source-tvavi
  '((name . "TV avi")
    (candidates "tvavi" "tvavi:dvdr" "tvavi:dvdram")
    (action . identity)
    (action-transformer . anything-c-tvavi-get-actions)
    (requires-pattern . 4)))
(defun anything-c-tvavi-get-actions (actions candidates)
  (case (intern (anything-get-selection))
    ('tvavi (anything-c-tvavi-get-actions/tv "/tv/"))
    ('tvavi:dvdr (anything-c-tvavi-get-actions/filelist "~/filelist/dvdr/" "/dvdr/"))
    ('tvavi:dvdram (anything-c-tvavi-get-actions/filelist "~/filelist/dvdram/" "/d/"))))
(defun anything-c-tvavi-get-actions/tv (dir)
  (mapcar (lambda (avi)
            (let* ((attrib (file-attributes avi))
                   (mtime (nth 5 attrib))
                   (size (nth 7 attrib))
                   (basename (file-name-nondirectory avi)))
            `(,(format "%s %s %s" (format-time-string "%m/%d" mtime) size basename)
              . (lambda (c)
                  (start-process-shell-command "*tvavi*" "*tvavi*" 
                                               ,(format "m '%s' > /dev/null 2>&1" avi))))))
          (directory-files dir t ".avi$")))
(defun anything-c-tvavi-get-actions/filelist (dir mountdir)
  (loop for file in (directory-files dir t "[^.]$")
        append
        (with-current-buffer (find-file-noselect file)
          (goto-char 1)
          (let ((id (and (re-search-forward "^## id:\\(.+\\)$")
                         (match-string-no-properties 1)))
                actions)
            (while (re-search-forward "^ *\\([0-9]+\\) \\(.+avi\\)$" nil t)
              (push 
               `(,(format "#%s %s %s" id
                          (match-string-no-properties 1)
                          (match-string-no-properties  2))
                 . (lambda (c)
                     (start-process-shell-command
                      "*tvavi*" "*tvavi*" 
                      ,(format "mount %s ; m '%s%s' > /dev/null 2>&1; umount %s"
                               mountdir mountdir (match-string-no-properties 2) mountdir))))
               actions))
            actions))))
;; (anything-c-tvavi-get-actions/tv "/tv/")
;; (anything-c-tvavi-get-actions/filelist "~/filelist/dvdram/" "/d/")
;; (length (anything-c-tvavi-get-actions/filelist "~/filelist/dvdram/" "/dvdr/"))
;; (anything 'anything-c-source-tvavi)

;; [2008/01/04] by Matsuyama ](@* " bm")

;; [2008/01/12] (@* " frequently used commands")
(defvar anything-c-frequently-used-commands
  '("qr" "qrr"))
(defvar anything-c-source-frequently-used-commands
  '((name . "Frequently used commands")
    (candidates
     . (lambda ()
         (when (member anything-pattern anything-c-frequently-used-commands)
           (list anything-pattern))))
    (volatile)
    (action
     . anything-c-call-interactively)))
;; (anything 'anything-c-source-frequently-used-commands)
;; [2008/01/12] (@* "  frequently used commands - keymap")
' (loop for (command . key)
      in '(("anything-update-vars" . "U")
           ("icicle-execute-extended-command" . "\C-x"))
      do (define-key anything-map key
           `(lambda ()
              (interactive)
              (delete-minibuffer-contents)
              (insert ,command)
              (anything-check-minibuffer-input)
              (anything-exit-minibuffer))))

;; [2008/01/12] (@* " abbrev")
;; It depends on  structure of `insert-abbrev-table-description' output.
(defun anything-c-abbrev-candidates (table-sym)
  (with-temp-buffer
    (let ((table (with-current-buffer anything-current-buffer
                   (symbol-value table-sym))))
      (insert-abbrev-table-description
       (abbrev-table-name  table)
       nil)
      (goto-char 0)
      (let ((abbrevs (car (cdaddr (read (current-buffer))))))
        (mapcar
         (lambda (abb)
           (let ((name (car abb))
                 (desc (if (and (equal "" (cadr abb)) )
                           (setq desc (format "%s" (cadadr (caddr (caddr abb)))))
                         (setq desc (cadr abb)))))
             (list (format "%s: %s" name
                           (replace-regexp-in-string
                            "\n" " "
                            (truncate-string-to-width desc (- (window-width) 15))))
                   name table)))
         abbrevs)))))
;; (equal a (anything-c-abbrev-candidates 'local-abbrev-table))

(defvar anything-c-source-abbrev-local
  '((name . "Local Abbrev")
    (candidates . (lambda () (anything-c-abbrev-candidates 'local-abbrev-table)))
    (type . abbrev)))
;; (anything 'anything-c-source-abbrev-local)

(defvar anything-c-source-abbrev-global
  '((name . "Global Abbrev")
    (candidates . (lambda () (anything-c-abbrev-candidates 'global-abbrev-table)))
    (type . abbrev)))
;; (anything 'anything-c-source-abbrev-global)

(add-to-list 'anything-type-attributes
             '(abbrev
               (action ("expand" . (lambda (c)
                                     (let ((p (point)))
                                       (insert (car c))
                                       (expand-abbrev))))
                       ("undefine" . (lambda (c)
                                       (define-abbrev (cadr c) (car c) nil))))))
  
;; [2008/01/13] (@* " edit abbrev files")
(defvar anything-c-source-find-abbrev-file
  '((name . "Abbrev file")
    (init
     . (lambda ()
         (setq anything-c-abbrev-dir
               (expand-file-name
                (with-current-buffer anything-current-buffer
                  (symbol-name major-mode))
                abbrev-files-directory))))
    (candidates
     . (lambda ()
         (let ((abbrev-dir anything-c-abbrev-dir))
           (and (file-directory-p abbrev-dir)
                (directory-files abbrev-dir t "^[^\\.]")))))
    (filtered-candidate-transformer
     . (lambda (candidates source)
         `(,@candidates
           (,(concat "*New abbrev file* '" anything-input "'") .
            ,(expand-file-name (concat anything-input ".abbrev") anything-c-abbrev-dir)))))
    (type . file)))
;; (anything 'anything-c-source-find-abbrev-file)

;; [2008/01/13] (@* " dummy source")
;; (anything 'anything-c-source-buffer-not-found)


;; [2008/01/15] (@* "  define-global-abbrev")
(defun define-abbrev-interactively (abbrev &optional table)
  (let ((expansion (read-string (format "abbrev(%s) expansion: " abbrev))))
    (define-abbrev (or table global-abbrev-table) abbrev expansion)
    (insert expansion)))
(defun define-mode-abbrev-interactively (abbrev)
  (define-abbrev-interactively abbrev local-abbrev-table))

(defvar anything-c-source-define-global-abbrev
  '((name . "Define Global Abbrev")
    (dummy)
    (action . define-abbrev-interactively)))
;; (anything 'anything-c-source-define-global-abbrev)

;; [2008/01/15] (@* "  define-mode-abbrev")
(defvar anything-c-source-define-mode-abbrev
  '((name . "Define Mode-specific Abbrev")
    (dummy)
    (action . define-mode-abbrev-interactively)))
;; (anything 'anything-c-source-define-mode-abbrev)

;; [2008/08/22] (@* "  define-yasnippet-on-region")
(defvar anything-c-source-define-yasnippet-on-region
  '((name . "Create new YaSnippet on region")
    (dummy)
    (action . (lambda (c)
                (with-stub
                  (let* ((mode-name (symbol-name anything-c-yas-cur-major-mode))
                         (root-dir (expand-file-name
                                    (if (listp yas/root-directory)
                                        (car yas/root-directory)
                                      yas/root-directory)))
                         (default-snippet-dir (anything-c-yas-find-recursively mode-name root-dir 'dir))
                         (filename (concat default-snippet-dir "/" anything-pattern)))
                    (stub read-file-name => filename)
                    (anything-c-yas-create-new-snippet
                     (with-current-buffer anything-current-buffer
                       (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) "")))))))))

;; [2008/01/15] (@* "  M-x")
;; (install-elisp-from-emacswiki "anything-complete.el")
(defvar anything-c-source-M-x
  '((name . "M-x")
    ;; Use anything-lisp-complete-symbol's candidates
    (init . (lambda ()
              (anything-candidate-buffer (get-buffer alcs-commands-buffer))))
    (header-name . (lambda (name)
                     (if anything-current-prefix-arg
                         (format "%s (arg=%S)" name anything-current-prefix-arg)
                       name)))
    (candidates-in-buffer)
    (type . command)))
;; (anything 'anything-c-source-M-x)

;; [2008/01/14] helper
(defun anything-c-create-format-commands-with-description (alist)
  (loop for (cmd . desc) in alist
        collect (cons (format "%s: %s" cmd desc) cmd)))

;; [2008/01/14] (@* " commands for current buffer")
(defvar anything-c-commands-for-current-buffer
  (anything-c-create-format-commands-with-description
   '(;; column-marker
     ("column-marker-1" . "Highlight column with green")
     ("column-marker-2" . "Highlight column with blue")
     ("column-marker-3" . "Highlight column with red")
     ("column-marker-turn-off-all" . "Remove all highlight by column-marker")

     ;; markerpen
     ("markerpen-clear-all-marks" . "Clear all markerpens")
     ("markerpen-clear-region" . "Clear markerpen of region")
     ("markerpen-mark-region-1" . "Highlight region with color")
     ("markerpen-mark-region-2" . "Highlight region with color")
     ("markerpen-mark-region-3" . "Highlight region with color")
     ("markerpen-mark-region-4" . "Highlight region with color")
     ("markerpen-mark-region-5" . "Highlight region with color")
     ("markerpen-mark-region-6" . "Highlight region with color")
     ("markerpen-mark-region-7" . "Highlight region with color")
     ("markerpen-mark-region-8" . "Highlight region with color")
     ("markerpen-mark-region-9" . "Highlight region with color")
     ("markerpen-mark-region-10" . "Highlight region with color")

     )))
  
(defvar anything-c-source-commands-for-current-buffer
  '((name . "Commands for current buffer")
    (candidates . anything-c-commands-for-current-buffer)
    (type . command)))

;; [2008/01/14] (@* " commands for insertion")
(defvar anything-c-commands-for-current-insertion
  (anything-c-create-format-commands-with-description
   '()))
(defvar anything-c-source-commands-for-insertion
  '((name . "Commands for current buffer")
    (candidates . anything-c-commands-for-current-insertion)
    (type . command)))

;; [2008/01/14] (@* " KYR")
;; KYR = "Kuuki wo YomeRu" in Japanese (can read the atmosphere)
(require 'anything-kyr)
;; (defvar anything-kyr-candidates nil)
;; (defvar anything-kyr-functions nil)
;; (defvar anything-c-source-kyr
;;   '((name . "Context-aware Commands")
;;     (candidates . anything-kyr-candidates)
;;     (filtered-candidate-transformer)    ;no need to sort
;;     (type . command)))
;; (defvar anything-kyr-commands-by-major-mode nil)
;; ;; (anything 'anything-c-source-kyr)
;; (defun anything-kyr-candidates ()
;;   (loop for func in anything-kyr-functions
;;         append (with-current-buffer anything-current-buffer (funcall func))))
;; (defun anything-kyr-commands-by-major-mode ()
;;   (assoc-default major-mode anything-kyr-commands-by-major-mode))

(define-switch-command add-kyr-command
  (find-anchor "~/emacs/init.d/anything-private.el" "  KYR vars"))

' (setq anything-kyr-commands-by-major-mode
      '((ruby-mode "rdefs" "rcov" "rbtest")
        (emacs-lisp-mode "byte-compile-file"))
      ;;
      anything-kyr-functions
      '((lambda ()
          (save-excursion
            (when (and (re-search-backward (format ee-anchor-format "\\(.+\\)") nil t)
                       (string= (match-string-no-properties 1) "abbrevs"))
              (list "make-skeleton"))))
        anything-kyr-commands-by-major-mode))

;; (@* " rdefs")
(defvar rdefs-keyword-regexp
  (rx (or "def" "class" "module" "include" "extend" "alias"
          "attr" "attr_reader" "attr_writer" "attr_accessor"
          "public" "private" "private_class_method" "public_class_method"
          "module_function" "protected" "def_delegators")
      (1+ space)
      (? ":")
      (group (1+ (or alnum "_" "!" "?")))))

(defun anything-rdefs-goto-line (line)
  ;; go to definition line
  (goto-line line)
  ;; go to the top of RDoc for this definition.
  (while (progn
           (forward-line -1)
           (looking-at "^\s*\\(#.*\\)?$")))
  (forward-line 1)
  (while (looking-at "^\s*$")
    (forward-line 1))

  ;;(set-window-start (get-buffer-window anything-current-buffer) (point))
  (set-window-start (selected-window) (point))
  )

(defun anything-rdefs-goto-entry (candidate)
  (cond ((string-match "^\\([0-9]+\\)" candidate)
         (anything-rdefs-goto-line (string-to-number (match-string 1 candidate))))
        ((and rdefs-buffer (string-match "^\\(.+?\\):\\([0-9]+\\)" candidate))
         (let ((orig-rdefs-buffer rdefs-buffer)
               (file (match-string 1 candidate))
               (line (match-string 2 candidate)))
           (ps-push-context)
           (find-file (expand-file-name file (buffer-local-value 'default-directory rdefs-buffer)))
           (setq rdefs-buffer orig-rdefs-buffer)
           (anything-rdefs-goto-line  (string-to-number line))))))

;; new implementation
(defun anything-c-source-rdefs-base (attrs)
  (append attrs
          '((candidates-in-buffer)
            (major-mode ruby-mode el4r-mode)
            (action ("Goto line" . anything-rdefs-goto-entry)
                    ("Insert method" .
                     (lambda (candidate)
                       (when (string-match rdefs-keyword-regexp candidate)
                         (insert (match-string 1 candidate))))))
            (action-transformer
             . (lambda (actions candidate)
                 (if (eq anything-sources anything-for-insertion-sources)
                     (list (cadr actions) (car actions))
                   actions))))))

(defvar anything-c-source-rdefs
  (anything-c-source-rdefs-base
   '((name . "Ruby Defines")
     (init
      . (lambda ()
          (when (and buffer-file-name (memq major-mode '(ruby-mode el4r-mode))
                     (anything-current-buffer-is-modified))
            (call-process-shell-command
             (format "rdefs.rb -nP %s"
                     (buffer-local-value 'buffer-file-name anything-current-buffer))
             nil (anything-candidate-buffer 'local))))))))

(defvar anything-c-source-rdefs-from-rdefs-buffer
  (anything-c-source-rdefs-base
   '((name . "Ruby Defines (RDEFS)")
     (init
      . (lambda ()
          (when rdefs-buffer
            (with-current-buffer (anything-candidate-buffer rdefs-buffer)
              (setq rdefs-buffer (buffer-local-value 'rdefs-buffer anything-current-buffer))))))
     (requires-pattern . 4)
     ;;(type . file-line)
     (candidates-in-buffer
      . (lambda () (when (buffer-local-value 'rdefs-buffer anything-current-buffer)
                     (anything-candidates-in-buffer)))))))

;; (@* " tag jump by rdefs")
(defun search-rdefs-file ()
  (loop with dir = (substring default-directory 0 -1)
        until (member dir '("~" "/" ""))
        for rdefs = (expand-file-name "RDEFS" dir)
        when (file-readable-p rdefs)
        do (return rdefs)
        else do (setq dir (substring (file-name-directory dir) 0 -1))))

(defun search-rdefs-buffer ()
  (anything-aif (search-rdefs-file)
      (find-file-noselect it)))

(defun rdefs-current-buffer ()
  (interactive)
  (unless buffer-file-name
    (error "rdefs must be applied by file"))
  (let ((orig-buf (current-buffer))
        (bfn buffer-file-name))
    (with-current-buffer (get-buffer-create (concat " *rdefs:%s*" bfn))
      (buffer-disable-undo)
      (erase-buffer)
      (call-process "rdefs.rb" nil (current-buffer) nil "-nHPd" "--dev" bfn)
      (let ((rdefs-buf (current-buffer)))
        (with-current-buffer orig-buf (setq rdefs-buffer rdefs-buf))))))


;; (install-elisp-from-emacswiki "point-stack.el")
(require 'point-stack)

(defvar rdefs-buffer nil)
(make-variable-buffer-local 'rdefs-buffer)
(defun anything-rdefs-find-definition (&optional word)
  (interactive)
  (setq word (or word (thing-at-point 'symbol)))
  (let ((anything-quit-if-no-candidate
         (lambda () (message "No definition"))))
   (anything
    '(((name . "Find definition by rdefs")
       (init
        . anything-rdefs-init)
       (candidates-in-buffer)
       ;; override default file-line type
       ;;(display-to-real-old . anything-rdefs-display-to-real)
       (filtered-candidate-transformer . anything-rdefs-filtered-candidate-transformer)
       ;; require gtags.el
       (before-jump-hook . (lambda () (ps-push-context)))
       (after-jump-hook . (lambda ()
                            (setq rdefs-buffer (buffer-local-value 'rdefs-buffer
                                                                   anything-current-buffer))
                            (view-mode 1)))
       (adjust)
       (no-new-window)
       (type . file-line))))))

(defun anything-rdefs-init ()
  (anything-aif (or rdefs-buffer (search-rdefs-buffer))
      (with-current-buffer (anything-candidate-buffer 'local)
        (let ((wordq (concat (regexp-quote word) "[!\\?]?")))
          (with-current-buffer anything-current-buffer
            (setq rdefs-buffer it))
          (cond ((bufferp it)
                 (insert-buffer-substring it)
                 (setq default-directory
                       (buffer-local-value 'default-directory it)))
                ((stringp it)
                 (insert-file-contents it)
                 (setq default-directory (file-name-directory it)))
                (t
                 (error)))
          (keep-lines (concat "\\(class\\|module\\|def\\) +" wordq "\\([\\ (]\\|$\\)\\|"
                              "\\." wordq "\\|" ;singleton methods
                              ":" wordq "\\([, ]\\|$\\)"
                              )
                      (point-min)(point-max))))))

(defun anything-rdefs-filtered-candidate-transformer (candidates source)
  (mapcar
   (lambda (data)
     (destructuring-bind (candidate . (file lineno content)) data
       (setq content (with-temp-buffer
                       (insert content)
                       (if (search-backward " / " nil t)
                           (buffer-substring (match-end 0) (point-max))
                         (goto-char (point-min))
                         (re-search-forward " +" nil t)
                         (buffer-substring (point) (point-max)))))
       (setq file (expand-file-name file (buffer-local-value 'default-directory (anything-candidate-buffer))))
       (cons candidate (list file lineno content))))
   (anything-c-filtered-candidate-transformer-file-line candidates source)))


'(defun anything-rdefs-display-to-real (candidate)
  (destructuring-bind (file lineno content)
      (anything-c-display-to-real-file-line candidate)
    (setq content (with-temp-buffer
                    (insert content)
                    (if (search-backward " / " nil t)
                        (buffer-substring (match-end 0) (point-max))
                      (goto-char (point-min))
                      (re-search-forward " +" nil t)
                      (buffer-substring (point) (point-max)))))
    (setq file (expand-file-name file (buffer-local-value 'default-directory (anything-candidate-buffer))))
    (list file lineno content)))

;; [2008/02/12] << list-call-seq.rb>>
(defvar anything-c-source-list-call-seq
  '((name . "Ruby Source (call-seq)")
    (init
     . (lambda ()
         (when (and buffer-file-name
                    (anything-current-buffer-is-modified)
                    (eq major-mode 'c-mode))
           (call-process-shell-command
            (format "list-call-seq.rb -n %s" buffer-file-name)
            nil (anything-candidate-buffer 'local)))))
    (recenter)
    (candidates-in-buffer)
    (type . line)))
;; (anything 'anything-c-source-list-call-seq)
(defun anything-c-source-global-call-seq (args)
  `((init
     . (lambda ()
         (anything-candidate-buffer
          (find-file-noselect ,(assoc-default 'call-seq-file args)))))
    (default-directory . ,(file-name-directory (assoc-default 'call-seq-file args)))
    (candidates-in-buffer)
    (type . file-line)
    (requires-pattern . 4)
    (major-mode ruby-mode el4r-mode c-mode)
    (recenter)
    ,@args))

(defvar anything-c-source-call-seq-ruby18
  (anything-c-source-global-call-seq
   '((name . "Ruby 1.8 call-seq")
     (call-seq-file . "/log/compile/ruby18/CALLSEQ"))))
(defvar anything-c-source-call-seq-ruby19
  (anything-c-source-global-call-seq
   '((name . "Ruby 1.9 call-seq")
     (call-seq-file . "/log/compile/ruby19/CALLSEQ"))))
;; (anything 'anything-c-source-call-seq-ruby18)
;; (anything 'anything-c-source-call-seq-ruby19)

;; [2008/01/21] REDEFINED
(defun anything-c-buffer-list ()
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (append (cdr buffers) (list (car buffers)))))

;; [2008/02/04] (@* "special-display-buffer-history")
(defvar anything-c-source-special-display-buffer-history
  '((name . "Special Display Buffer History")
    (candidates
     . (lambda () (remove-if-not #'get-buffer special-display-buffer-history)))
    (action . display-buffer)))
;; (anything 'anything-c-source-special-display-buffer-history)

(defvar anything-c-source-w3m-defun
  '((name . "w3m DEFUN")
    (headline . "^DEFUN")
    (condition . (string-match "/w3m.+\.c$" (or buffer-file-name "")))))
;; (anything 'anything-c-source-w3m-defun)

(defvar anything-c-source-hatena-headline
  '((name . "Hatena HeadLine")
    (headline . "^\\*+")
    (migemo)
    (condition . (string-match "/memo/hatena/" (or buffer-file-name "")))))
;; (anything 'anything-c-source-hatena-headline)

(defvar anything-c-source-eev-anchor
  '((name . "Anchors!")
    (headline . (lambda () (format ee-anchor-format "\\(.+\\)")))
    (subexp . 1)))
;; (anything 'anything-c-source-eev-anchor)
;; [2008/08/12] <<< not-yet>>
(defvar anything-c-source-not-yet
  '((name . "Not Yet")
    (headline . "^# X.+$")
    (migemo)
    (condition . (string-match "index.e$" (or buffer-file-name "")))
    (adjust)
    (recenter)))
;; (anything 'anything-c-source-not-yet)

(setq anything-headline-sources
      '(anything-c-source-org-headline anything-c-source-oddmuse-headline
                                       anything-c-source-emacs-lisp-toplevels
                                       anything-c-source-rd-headline
                                       anything-c-source-w3m-defun
                                       anything-c-source-hatena-headline
                                       anything-c-source-emacs-source-defun
                                       anything-c-source-emacs-lisp-expectations))


;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(when (fboundp 'expectations)
  (expectations
    (desc "RD headers")
    (expect '(("RD HeadLine" (("H1-1" . 10) ("H1-1 / H2-1" . 20) ("H1-1 / H2-2" . 28)
                              ("H1-1 / H2-2 / H3-1" . 37) ("H1-1 / H2-3" . 45) ("H1-2" . 52))))
      (with-temp-buffer
        (insert-file-contents "~/emacs/etc/test.rd")
        (rd-mode)
        (anything-test-candidates (list anything-c-source-rd-headline) "")
        ))
    ))

;; [2008/08/24] (@* " grep buffer")
;; BUG 
(defvar anything-c-source-grep-buffer
  '((name . "*grep*")
    (init . (lambda () 
              (with-current-buffer (get-buffer-create anything-buffer)
                (if (anything-candidate-buffer (get-buffer-create "*grep*"))
                    (font-lock-mode 1)
                  (font-lock-mode -1)))))
    (candidates-in-buffer)
    (default-directory
      . (lambda ()
          (with-current-buffer (anything-candidate-buffer)
            (goto-char (point-min))
            (or ;; rake hack
                (and (re-search-forward "\n(in \\(.+\\))\n" nil t)
                     (match-string 1))
                default-directory))))
          
    (get-line . (lambda (s e) (unless (= s e) (buffer-substring s e))))
    (migemo)
    (recenter)
    (adjust)
    (type . file-line)))
;; (anything 'anything-c-source-grep-buffer)
;; (kill-buffer anything-buffer)
;; [2008/07/16] (@* " mew")
(defvar anything-c-source-mew-auto-refile-summary
  '((name . "Mew refile and summary")
    (candidates
     . (lambda ()
         (when (string= (buffer-name anything-current-buffer) "+inbox")
           auto-refile-alist)))
    (action
     ("read the folder" .
      (lambda (candidate)
        (apply 'auto-refile-summary candidate))))))
;; (anything 'anything-c-source-mew-auto-refile-summary)

;; [2008/08/22] (@* " source filter")
(defvar anything-c-source-set-source-filter
  '((name . "Anything Source")
    (candidates . (lambda ()
                    (mapcar (lambda (s) (assoc-default 'name s))
                            (anything-get-sources))))
    (persistent-action . (lambda (name)
                           (with-selected-window (minibuffer-window)
                             (delete-minibuffer-contents))
                           (anything-set-source-filter (list name))
                           (setq anything-candidate-number-limit nil)))
    (requires-pattern . 1)))
;; (find-epp0 anything-candidate-number-limit)
;; (setq anything-candidate-number-limit 100)
;; [2008/08/25] (@* " games")
(defvar anything-c-source-games
  '((name . "GAMES")
    (candidates-file . "~/games/GAMES")
    (candidates-in-buffer)
    (get-line . (lambda (s e) (cons (buffer-substring-no-properties s e) s)))
    (migemo)
    (requires-pattern . 1)
    (action
     ("select"
      . (lambda (c)
          (with-new-window (switch-to-buffer (anything-candidate-buffer)))
          (goto-char c)
          (set-window-start (selected-window) c))))))

;; [2008/09/02] (@* " background")
(defvar anything-c-source-background
  '((name . "Background")
    (candidates . anything-c-background-candidates)
    (action . pop-to-buffer)))
(defun anything-c-background-candidates ()
  (loop for b in (buffer-list)
        for name = (buffer-name b)
        when (string-match "^%" name)
        collecting
        (cons (with-current-buffer b
                (goto-char (point-min))
                (concat "Background: " (buffer-substring-no-properties (+ (point-min) 10) (point-at-eol))))
              name)))
;; (anything 'anything-c-source-background)
;; (anything '(((name . "test")(candidates ("hoge" . "boke")))))

;; (@* " navi2ch board")
;; navi2ch: http://navi2ch.sourceforge.net/
(defvar anything-c-source-navi2ch-board
  '((name . "Navi2ch board")
    (init . (lambda () (require 'navi2ch)))
    (candidates
     . (lambda () (mapcar (lambda (x) (cons (format "[%s]%s"
                                                    (assoc-default 'id x 'eq)
                                                    (assoc-default 'name x 'eq))
                                            (assoc-default 'id x 'eq)))
                          navi2ch-list-board-name-list)))
    (migemo)
    (action
     ("goto" . (lambda (candidate)
                 (with-new-window
                  (navi2ch-bm-select-board
                   (find candidate navi2ch-list-board-name-list
                         :test 'equal :key (lambda (x) (assoc-default 'id x))))))))))
;; (anything 'anything-c-source-navi2ch-board)

;; (@* "experiments")
;; (@* " persistent-action overlay test")
(defvar anything-c-source-persistent-action-overlay-test
  '((name . "test")
    (candidates . (lambda () (loop for i from 1 to 30 collect (int-to-string i))))
    (persistent-action . anything-c-source-persistent-action-overlay-test-doit)
    (cleanup . anything-c-source-persistent-action-overlay-test-delete-overlay)))
(defvar anything-persistent-action-overlay nil)
(defun anything-c-source-persistent-action-overlay-test-delete-overlay ()
  (and anything-persistent-action-overlay
       (delete-overlay anything-persistent-action-overlay)))
(defun anything-c-source-persistent-action-overlay-test-doit (cand)
  (with-anything-window
    (anything-c-source-persistent-action-overlay-test-delete-overlay)
    (save-excursion
      (goto-char (window-start))
      (setq anything-persistent-action-overlay
            (make-overlay (point-at-bol) (1+ (point-at-eol))))
      (overlay-put anything-persistent-action-overlay 'face 'highlight)
      (overlay-put anything-persistent-action-overlay 'display
                   (concat "{{{{" cand "}}}}" "\n"
                           "((((" cand "))))"
                           "\n")))
  (message cand)))
;; (anything 'anything-c-source-persistent-action-overlay-test)

(defvar anything-c-source-cheat
  '((name . "Cheat Sheets")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (with-current-buffer (anything-candidate-buffer 'global)
                  (call-process-shell-command
                   "cheat sheets" nil  (current-buffer))
                  (goto-char (point-min))
                  (forward-line 1)
                  (delete-region (point-min) (point))
                  (indent-region (point) (point-max) -2)))))
    (requires-pattern)
    (candidates-in-buffer)
    (action . (lambda (entry)
                (let ((buf (format "*cheat sheet:%s*" entry)))
                  (unless (get-buffer buf)
                    (call-process "cheat" nil (get-buffer-create buf) t entry))
                  (display-buffer buf)
                  (set-window-start (get-buffer-window buf) 1))))))
;; (anything 'anything-c-source-cheat)

;; [2008/09/28] (@* "mysql manual")
;; You must use rubikitch version of w3m
;; http://www.rubyist.net/~rubikitch/archive/w3m-rubikitch-071022-0.5.2-cvs.tar.gz
;; (shell-command "cd ~/doc; w3m -dump mysql-4.1.ja.html > mysql-4.1.ja.rd; egrep -n '^=' mysql-4.1.ja.rd > mysql-4.1.ja.rd.idx")
(defvar anything-c-source-mysql-manual
  '((name . "MySQL manual")
    (candidates-file . "~/doc/mysql-4.1.ja.rd.idx")
    (target-file . "~/doc/mysql-4.1.ja.rd")
    (major-mode sql-mode)
    (migemo)
    (type . line)))
;; (find-epp (anything-compile-sources '(anything-c-source-mysql-manual) anything-compile-source-functions))
;; (anything 'anything-c-source-mysql-manual)


;; [2008/09/29] (@* "mysql history")
(defvar anything-c-source-mysql-history
  '((name . "MySQL history")
    (candidates-file "~/.mysql_history" updating)
    (search-from-end)
    (type . complete)))
(defvar anything-c-source-postgresql-history
  '((name . "PostgreSQL history")
    (candidates-file "~/.psql_history" updating)
    (search-from-end)
    (type . complete)))
(defun anything-complete-sql-history ()
  "Select a command from sql history and insert it."
  (interactive)
  (anything-complete '(anything-c-source-sql-history-in-buffer
                       anything-c-source-mysql-history
                       anything-c-source-postgresql-history)
                     (or (word-at-point) "")
                     50))

(defvar anything-c-source-sql-history-in-buffer
  '((name . "SQL history in *SQL*")
    (candidates
     . (lambda ()
         (anything-aif (and (get-buffer "*SQL*")
                            (with-current-buffer "*SQL*"
                              (assoc-default mode-name sql-prompt-alist)))
             (apply 'sql-history-in-buffer "*SQL*" it))))
    (action . ac-insert)))
;; (anything 'anything-c-source-sql-history-in-buffer)

(defvar sql-prompt-alist
  '(("SQLi[mysql]" "^\C-g?mysql> " "\n    -> ")
    ("SQLi[postgres]" "^[^ ]+=# " "\n[^ ]+-# ")))

(defun sql-history-in-buffer (buf prompt-first-re prompt-rest-re)
  (with-current-buffer buf
    (goto-char (point-max))
    (or (search-backward "Process SQL finished\n" nil t)
        (goto-char (point-min)))
    (nreverse
     (loop with str
           while (re-search-forward prompt-first-re nil t)
           for s = (match-end 0)
           for e = (re-search-forward "\\(;\\|\\[a-zA-Z]\\)$" nil t)
           when (and e (setq str (buffer-substring-no-properties s e))
                     (not (string-match "^ *help " str)))
           collecting (replace-regexp-in-string prompt-rest-re " "
                                                (buffer-substring-no-properties s e))))))
;; (sql-history-in-buffer "*SQL*" "^rubikitch=# " "\nrubikitch-# ")
;; (sql-history-in-buffer "*SQL*" "^[^ ]+=# " "\n[^ ]+-# ")
;; (sql-history-in-buffer "*SQL*" "^\C-g?mysql> " "\n    -> ")
;; (anything 'anything-c-source-mysql-history)

;; (@* "anything-grep fallback")
(defvar anything-c-source-anything-grep-fallback
  '((name . "anything-grep fallback")
    (dummy)
    (action . (lambda (query) (anything-grep-by-name nil query)))))
;; (anything 'anything-c-source-anything-grep-fallback)

;; (@* "google fallback")
(defvar anything-c-source-anything-google-fallback
  '((name . "Google fallback")
    (dummy)
    (action . google)))
;; (anything 'anything-c-source-anything-google-fallback)


;; (@* "kill-ring")
(setq anything-kill-ring-threshold 1)


;; [2009/01/08]
(defun anything-c-kill-ring-action (str)
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (anything-attr 'last-command) 'yank))
      (insert-for-yank str)
    ;; from `yank-pop'
    (let ((inhibit-read-only t)
          (before (< (point) (mark t))))
      (if before
          (funcall (or yank-undo-function 'delete-region) (point) (mark t))
        (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (insert-for-yank str)
      ;; Set the window start back where it was in the yank command,
      ;; if possible.
      (set-window-start (selected-window) yank-window-start t)
      (if before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))
  (kill-new str))

;; (@* "emacswiki pages") [2008/10/21]
;; I no longer use oddmuse.el.
' (defvar anything-c-source-emacswiki-pages
    '((name . "EmacsWiki")
      (init . (lambda () (oddmuse-make-completion-table "EmacsWiki")))
      (candidates
       . (lambda () (gethash "EmacsWiki" oddmuse-pages-hash)))
      (action
       ("open EmacsWiki page" . (lambda (candidate)
                                  (with-new-window
                                   (oddmuse-edit "EmacsWiki" candidate)
                                   (delete-other-windows))))
       ("open EmacsWiki page" . (lambda (candidate)
                                  (oddmuse-edit "EmacsWiki" candidate))))
      (persistent-action
       . (lambda (candidate)
           (oddmuse-edit "EmacsWiki" candidate)))))

;; (anything 'anything-c-source-emacswiki-pages)

;; (@* "call tree") [2008/10/21]
(defvar anything-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init
     . (lambda ()
         (require 'simple-call-tree)
         (when (anything-current-buffer-is-modified)
           (simple-call-tree-analyze)
           (let ((list (simple-call-tree-invert simple-call-tree-alist)))
             (with-current-buffer (anything-candidate-buffer 'local)
               (dolist (entry list)
                 (let ((callers (mapconcat #'identity (cdr entry) ", ")))
                   (insert (car entry) " is called by "
                           (if (string= callers "")
                               "no functions."
                             callers)
                           ".\n"))))))))
    (delayed)
    (candidates-in-buffer)))
;; (anything 'anything-c-source-simple-call-tree-functions-callers)
(defvar anything-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init
     . (lambda ()
         (require 'simple-call-tree)
         (when (anything-current-buffer-is-modified)
           (simple-call-tree-analyze)
           (let ((list simple-call-tree-alist))
             (with-current-buffer (anything-candidate-buffer 'local)
               (dolist (entry list)
                 (let ((functions (mapconcat #'identity (cdr entry) ", ")))
                   (insert (car entry) " calls "
                           (if (string= functions "")
                               "no functions"
                             functions)
                           ".\n"))))))))
    (delayed)
    (candidates-in-buffer)))
;; (anything 'anything-c-source-simple-call-tree-callers-functions)

(defvar anything-c-source-windows-select
  '((name . "windows.el")
    (configs . 20)
    (init . (lambda () (win:switch-window win:current-config)))
    (candidates
     . (lambda ()
         (loop for i from 1 to (anything-attr 'configs)
               for key = (+ win:base-key i)
               for name-prefix = (aref win:names-prefix i)
               for name = (aref win:names i)
               when (and name (aref win:configs i))
               collecting (cons (format "[%c]%s:%s" key name-prefix name) i))))
    (action
     ("select" .
      (lambda (n)
        (cond ((aref win:configs (1+ win:current-config))
               (win:switch-window (1+ win:current-config))
               (unless (= win:current-config n) (win-swap-with (+ win:base-key n))))
              (t
               (let ((cc win:current-config))
                 (win:switch-window (1+ cc) nil t)
                 (win:set-wc n)
                 (win:switch-window n)
                 (win-delete-current-window nil)
                 (win:squeeze-config)
                 (win:switch-window cc)
                 (win-tab-updater)))))))))
;; (anything 'anything-c-source-windows-select)

;; (@* "buffer-list")
(defvar anything-c-source-buffer-list
  '((name . "Buffer List")
    (init
     . (lambda ()
         (setq abl-buffer-list (abl-buffer-list)
               abl-candidates (abl-format-buffer-list abl-buffer-list))))
    (candidates . abl-candidates)
    (multiline)
    (sort-by)
    (volatile)
    (persistent-action
     . (lambda (item)
         (kill-buffer item)
         (anything-delete-current-selection)
         (message "Buffer <%s> Killed" item)))))
(defvar abl-buffer-list nil)
(defvar abl-candidates nil)

(defun abl-sort-func-key (symbol)
  (case symbol
    ('name '(string< car))
    ('size '(> cadr))
    ('mode '(string< caddr))))
(defun abl-sort-by (key)
  (anything-attrset 'sort-by key)
  (setq abl-candidates
        (abl-format-buffer-list 
         (destructuring-bind (func key) (abl-sort-func-key key)
           (if func
               (sort* abl-buffer-list func :key key)
             abl-buffer-list))))
  (anything-update))

(defun abl-sort-by-size ()
  (interactive)
  (abl-sort-by 'size))
(defun abl-sort-by-name ()
  (interactive)
  (abl-sort-by 'name))
(defun abl-sort-by-mode ()
  (interactive)
  (abl-sort-by 'mode))

(defun abl-buffer-list ()
  (mapcar (lambda (b)
            (list (buffer-name b)
                  (buffer-size b)
                  (buffer-local-value 'mode-name b)
                  (buffer-file-name b)))
          (buffer-list)))

(defun abl-format-buffer-list (blist)
  (loop for (name size mode file-name) in blist
        collect
        (cons (concat (format "%-40s %8d bytes <%s>"
                              name size mode)
                      (if file-name (format "\n  %s" file-name)))
              name)))

(defvar anything-buffer-list-map
  (let ((map (copy-keymap anything-map)))
    (define-key map "S" 'abl-sort-by-size)
    (define-key map "N" 'abl-sort-by-name)
    (define-key map "M" 'abl-sort-by-mode)
    map))

(defun anything-buffer-list ()
  (interactive)
  (let ((anything-map anything-buffer-list-map)
        (anything-candidate-number-limit 9999))
    (define-key anything-map "S" 'abl-sort-by-size)
    (define-key anything-map "N" 'abl-sort-by-name)
    (define-key anything-map "M" 'abl-sort-by-mode)
    (anything 'anything-c-source-buffer-list)))


;; (anything 'anything-c-source-buffer-list)


;; (@* "org-gtd-started-waiting")
;(defvar anything-c-org-todo-states "^\\*+ \\<\\(STARTED\\|WAITING\\)\\> ")
(defvar anything-c-org-todo-states '("STARTED" "WAITING"))
(defvar anything-c-source-org-todo-state
  '((name . "Started and Waiting tasks")
    (candidates
     . (lambda ()
         (with-current-buffer (find-file-noselect org-default-notes-file)
           (loop for state in anything-c-org-todo-states
                 do (goto-char (point-min))
                 append
                 (loop while (re-search-forward (format "^\\*+ \\<%s\\> " state) nil t)
                       for bol = (point-at-bol)
                       collect (cons (buffer-substring bol (point-at-eol)) bol))))))
    (action
     ("Go to the location" . (lambda (p)
                               (find-file org-default-notes-file)
                               (goto-char p))))))
;; (anything 'anything-c-source-org-todo-state)

(add-to-list 'anything-c-source-org-todo-state
             '(action
               ("Go to the location"
                . (lambda (p)
                    (with-new-window (find-file org-default-notes-file))
                    (goto-char p)))))


;; (@* "new sources")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  (@* "type attribute helper")                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-type-attribute/command-local
  '((action 
     ("Call interactively" . anything-c-call-interactively)
     ("Call interactively" . anything-c-call-interactively)
     ("Describe Function" . alcs-describe-function))
    ;; Sort commands according to their usage count.
    ;;(filtered-candidate-transformer . anything-c-adaptive-sort)
    (persistent-action . alcs-describe-function)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  (@* "type attributes")                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (find-epp anything-type-attributes)
(setq anything-type-attributes
      `((buffer
         (action
          ("Switch to Buffer (next curwin)" . win-switch-to-buffer)
          ("Switch to buffer" . switch-to-buffer)
          ("Switch to buffer other window" . switch-to-buffer-other-window)
          ("Kill buffer"      . kill-buffer)
          ("Switch to buffer other frame" . switch-to-buffer-other-frame)
          ("Display buffer"   . display-buffer))
         (candidate-transformer . (lambda (candidates)
                                    (anything-c-compose
                                     (list candidates)
                                     '(anything-c-skip-boring-buffers
                                       anything-c-skip-current-buffer
                                       anything-c-transform-navi2ch-article))))
         (persistent-action . switch-to-buffer))
        (file
         (action
          ("Find File (next curwin)" . win-find-file)
          ("Find file" . find-file)
          ("Find file other window" . find-file-other-window)
          ("Delete File" . anything-c-delete-file)
          ("Find file other frame" . find-file-other-frame)
          ("Open dired in file's directory" . anything-c-open-dired)
          ("Delete file" . anything-c-delete-file)
          ("Open file externally" . anything-c-open-file-externally)
          ("Open file with default tool" . anything-c-open-file-with-default-tool))
         (action-transformer . (lambda (actions candidate)
                                 (anything-c-compose
                                  (list actions candidate)
                                  '(anything-c-transform-file-load-el
                                    anything-c-transform-file-browse-url
                                    anything-c-transform-w3m-remote))))
         (candidate-transformer . (lambda (candidates)
                                    (anything-c-compose
                                     (list candidates)
                                     '(anything-c-skip-boring-files
                                       anything-c-shorten-home-path))))
         (persistent-action . find-file))
        (command-ext (action ("Call Interactively (new window)"
                              . (lambda (command-name)
                                  (with-new-window (anything-c-call-interactively command-name))))
                             ("Call interactively" . anything-c-call-interactively)
                             ("Describe command" . alcs-describe-function)
                             ("Add command to kill ring" . kill-new)
                             ("Go to command's definition" . alcs-find))
                     ;; Sort commands according to their usage count.
                     ;; (filtered-candidate-transformer . anything-c-adaptive-sort)
                     (persistent-action . alcs-describe-function)
                     )
        (command-local  ,@anything-type-attribute/command-local)
        (command  ,@anything-type-attribute/command-local)
        (function (action ("Describe function" . alcs-describe-function)
                          ("Add function to kill ring" . kill-new)
                          ("Go to function's definition" . alcs-find-function))
                  (action-transformer . (lambda (actions candidate)
                                          (anything-c-compose
                                           (list actions candidate)
                                           '(anything-c-transform-function-call-interactively)))))
        (sexp (action ("Eval s-expression" . (lambda (c)
                                               (eval (read c))))
                      ("Add s-expression to kill ring" . kill-new))
              (action-transformer . (lambda (actions candidate)
                                      (anything-c-compose
                                       (list actions candidate)
                                       '(anything-c-transform-sexp-eval-command-sexp)))))
        (escript (action ("Eval it" . anything-c-action-escript-eval)))
        (line (display-to-real . anything-c-display-to-real-line)
              (action ("Go to Line" . anything-c-action-line-goto)))
        (file-line (filtered-candidate-transformer . anything-c-filtered-candidate-transformer-file-line)
                   (multiline)
                   ;; (display-to-real . anything-c-display-to-real-file-line)
                   (action ("Go to (next curwin)"
                            . (lambda (file-line)
                                (with-new-window (anything-c-action-file-line-goto file-line))))
                           ("Go to" . anything-c-action-file-line-goto))
                   (action-transformer
                    . (lambda (actions sel)
                        (if (anything-attr-defined 'no-new-window)
                            (cdr actions)
                          actions)))
                   (persistent-action . anything-c-action-file-line-goto))
        (apropos-function
         (persistent-action . alcs-describe-function)
         (action
          ("Find Function (next window)"
           . (lambda (f) (with-new-window (alcs-find-function f))))
          ("Find Function" . alcs-find-function)
          ("Describe Function" . alcs-describe-function)
          ))
        (apropos-variable
         (persistent-action . alcs-describe-variable)
         (action
          ("Find Variable (next window)"
           . (lambda (v) (with-new-window (alcs-find-variable v))))
          ("Find Variable" . alcs-find-variable)
          ("Describe Variable" . alcs-describe-variable)))
        (complete-function
         (action . ac-insert)
         (persistent-action . alcs-describe-function))
        (complete-variable
         (action . ac-insert)
         (persistent-action . alcs-describe-variable))
        (complete
         (candidates-in-buffer . ac-candidates-in-buffer)
         (action . ac-insert))
        (bookmark
         (action
          ,@(anything-c-action-extend "Jump to Bookmark" 'bookmark-jump)
          ("Update Bookmark" . bookmark-set)
          ("Delete Bookmark" . bookmark-delete)
          ))
        ,@anything-type-attributes
        ))

(defun anything-c-transform-w3m-remote (actions candidate)
  (if (string-match "^h?ttp://" candidate)
      (cons '("w3m-remote" . w3mremote)
            actions)
    actions))

;; (@* "action function")
(defun anything-c-action-escript-eval (cand)
  (with-current-buffer (anything-candidate-buffer)
    (let ((max-mini-window-height
           (if anything-in-persistent-action 1 0.9)))
      (goto-char (point-min))
      (search-forward (concat cand "\n"))
      (eek-eval-last-sexp anything-current-prefix-arg))))



;; REDEFINED!
(defun alcs-describe-variable (v)
  (describe-hash-table (intern v)))


;; (@* "anything-set-source-filter")
(defvar anything-c-categories '(rubyref create))
(dolist (category anything-c-categories)
  (let ((funcsym (intern (format "anything-show/%s" category))))
    (eval `(defun ,funcsym ()
             (interactive)
             (anything-set-source-filter
              (mapcar (lambda (src) (assoc-default 'name src))
                      (remove-if-not (lambda (src) (memq ',category (assoc-default 'category src)))
                                     anything-sources)))))))

;; [2007/12/30] (@* "anything-insert-buffer-name")
(defun anything-insert-buffer-name ()
  (interactive)
  (delete-minibuffer-contents)
  (insert (with-current-buffer anything-current-buffer
            (if buffer-file-name (file-name-nondirectory buffer-file-name)
              (buffer-name)))))
;; [2007/12/30]
(defun anything-backward-char-or-show/create ()
  (interactive)
  (if (string= "" anything-pattern)
      (anything-show/create)
    (call-interactively 'backward-char)))
(defun anything-backward-char-or-insert-buffer-name ()
  (interactive)
  (if (string= "" anything-pattern)
      (anything-insert-buffer-name)
    (call-interactively 'backward-char)))

(defvar anything-function 'anything-at-point)

(defun anything-sort-sources-by-major-mode (sources)
  (loop for src in sources
        for modes = (anything-attr 'major-mode (symbol-value src))
        if (memq major-mode modes)
        collect src into prior
        else
        collect src into rest
        finally (return (append prior rest))))

;; [2008/09/21] (@* "anything default")
(defun anything-default ()
  (interactive)
  (let ((anything-input-idle-delay 0.2)
        (anything-idle-delay 0.7)
        (anything-quick-update t)
        (anything-sources
         (append
          '( ;;prior
             anything-c-source-mew-auto-refile-summary
             anything-c-source-rdefs-from-rdefs-buffer
             anything-c-source-emacs-variable-at-point
             anything-c-source-emacs-function-at-point
             anything-c-source-ffap-guesser
             ;;anything-c-source-files-in-current-dir/rtb
             anything-c-source-rake-task
             anything-c-source-tvavi
             ;;anything-c-source-buffers2
             anything-c-source-org-todo-state
             anything-c-source-windows-select
             anything-c-source-recentf
             anything-c-source-buffers
             anything-c-source-special-display-buffer-history
             anything-c-source-bookmarks
             anything-c-source-file-cache
             ;;anything-c-source-set-source-filter
             anything-c-source-elinit
             anything-c-source-switch-commands
             anything-c-source-background
             anything-c-source-cheat
             ;;anything-c-source-grep-buffer
             ;;anything-c-source-rtb
             anything-c-source-navi2ch-board
             )
          (anything-sort-sources-by-major-mode
           '(;; major-mode oriented sources
             anything-c-source-refe2x
             anything-c-source-ri
             anything-c-source-find-library
             anything-c-source-rubylib-18
             anything-c-source-rubylib-19
             anything-c-source-call-seq-ruby18
             anything-c-source-call-seq-ruby19
             anything-c-source-ruby18-source
             anything-c-source-ruby19-source
             anything-c-source-apropos-emacs-commands
             anything-c-source-apropos-emacs-functions
             anything-c-source-apropos-emacs-variables
             ;;anything-c-source-mysql-manual
             ))
          '(;; lower priority
             anything-c-source-ipa-global
             anything-c-source-info-pages
             ;;anything-c-source-man-pages
             ;;anything-c-source-extended-command-history
             anything-c-source-games
             anything-c-source-music
             ;;anything-c-source-faces
             ;;anything-c-source-colors
             anything-c-source-home-directory
             anything-c-source-compile-directory
             anything-c-source-auto-install-from-emacswiki
             anything-c-source-yaoddmuse-emacswiki-edit-or-view
             anything-c-source-file-name-history
             anything-c-source-anything-grep-fallback
             anything-c-source-anything-google-fallback
             ))))
    (call-interactively anything-function)))
;; [2008/01/04] (@* "anything for current-buffer")
(defvar anything-for-current-buffer-sources nil)
(defun anything-for-current-buffer ()
  (interactive)
  (let* ((anything-buffer "*anything current buffer*")
         (anything-sources
          (append
           '( ;; prior
             anything-c-source-not-yet
             anything-c-source-ipa
             anything-c-source-bm
             anything-c-source-fixme
             anything-c-source-linkd-tag
             anything-c-source-eev-anchor
             )
           anything-headline-sources
           '( ;; mode-oriented
             anything-c-source-rdefs
             anything-c-source-list-call-seq
             ;; minor
             anything-c-source-ctags
             ;;anything-c-source-imenu
             anything-c-source-commands-for-current-buffer
             anything-c-source-anything-grep-fallback
             anything-c-source-anything-google-fallback
             ;;anything-c-source-simple-call-tree-functions-callers
             ;;anything-c-source-simple-call-tree-callers-functions
             )))
         (anything-candidate-number-limit 2000))
    (call-interactively anything-function)))

;; [2008/01/12] (@* "anything for insert")
(defvar anything-for-insertion-sources nil)
(defun anything-for-insertion ()
  (interactive)
  (let* ((anything-buffer "*anything insert exec*")
         (anything-sources 
          '(anything-c-source-frequently-used-commands
            anything-c-source-kyr
            anything-c-source-extended-command-history
            anything-c-source-yasnippet
            anything-c-source-abbrev-local
            anything-c-source-abbrev-global
            ;;anything-c-source-rdefs
            anything-c-source-register
            anything-c-source-M-x
            anything-c-source-lacarte
            anything-c-source-commands-for-insertion
            anything-c-source-define-mode-abbrev
            anything-c-source-define-global-abbrev
            ))
         (anything-candidate-number-limit 200))
    (setq anything-current-prefix-arg current-prefix-arg)
    (anything)))

;; [2008/06/13] (@* "anything for langhelp")
;; temporary implementation
(defvar anything-for-langhelp-sources nil)
(defun anything-for-langhelp ()
  (interactive)
  (let ((anything-buffer "*anything langhelp*")
        (anything-sources '(anything-c-source-langhelp-ruby))
        (anything-candidate-number-limit 10000))
    (call-interactively anything-function)))

;; (@* "anything for create")
(defun anything-for-create-from-anything ()
  (interactive)
  (setq anything-input-idle-delay 0)
  (anything-set-sources
   '(((name . "Create")
      (dummy)
      (action
       ("find-file new window" . (lambda (x) (with-new-window (find-file x))))
       ("find-file" . find-file)
       ("find-file other window" . find-file-other-window)
       ("New buffer new window" . (lambda (x) (with-new-window (switch-to-buffer x))))
       ("New buffer" . switch-to-buffer)
       ("New buffer other window" . switch-to-buffer-other-window)
       ("YaSnippet on region"
        . (lambda (c)
                (with-stub
                  (let* ((mode-name (symbol-name anything-c-yas-cur-major-mode))
                         (root-dir (expand-file-name
                                    (if (listp yas/root-directory)
                                        (car yas/root-directory)
                                      yas/root-directory)))
                         (default-snippet-dir (anything-c-yas-find-recursively (symbol-name major-mode) root-dir 'dir))
                         (filename (concat default-snippet-dir "/" anything-input)))
                    (stub read-file-name => filename)
                    (anything-c-yas-create-new-snippet
                     (with-current-buffer anything-current-buffer
                       (if mark-active (buffer-substring-no-properties (region-beginning) (region-end)) "")))))))
       ("New Linkd Tag" . anything-c-insert-linkd-action)
       ("Bookmark Set" . bookmark-set)
       ("Define mode abbrev" . define-mode-abbrev-interactively)
       ("Define global abbrev" . define-abbrev-interactively)
       
      
;;                           anything-c-source-buffer-not-found
;;                           anything-c-source-insert-linkd-tag
;;                           anything-c-source-define-yasnippet-on-region
;;                           anything-c-source-bookmark-set
;;                           anything-c-source-define-mode-abbrev
;;                           anything-c-source-define-global-abbrev
;;                           anything-c-source-find-abbrev-file
                          )))))

;; (@* "anything for history") [2008/10/18] 
(defun anything-for-history ()
  (interactive)
  (anything-complete
   '(anything-c-source-kill-ring
     anything-c-source-complete-shell-history)
   ""))


;; [2008/01/15] (@* "abbrev or anything-for-insertion")
(defun expand-abbrev-or-anything-for-insertion ()
  (interactive)
  (or (and (not buffer-read-only) (expand-abbrev)) (anything-for-insertion)))

;; [2009/01/29]  (@* "disable pushy completion")
;; I do not want to do pushy completion with anything-related commands.
(dolist (cmd (all-completions "anything" obarray 'commandp))
  (put (intern cmd) 'pushy-completion 'disabled))

(load "anything-private.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  (@* "sources")                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (emacswiki-post "RubikitchAnythingConfiguration")
;; Local Variables:
;; linkd-mode: t
;; End:
