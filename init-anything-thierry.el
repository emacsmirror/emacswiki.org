;;; init-anything-thierry.el --- My startup file for anything. 
;; 
;; Author: Thierry Volpiatto
;; Maintainer: Thierry Volpiatto
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Personal configuration to start anything.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (tv-ee-index-create)

;;;;«INDEX»
;;; «.key-map»                          (to "key-map")
;;; «.My-Sources»                       (to "My-Sources")
;;; «.w3m-meteo-yahoo»                  (to "w3m-meteo-yahoo")
;;; «.Extensions»                       (to "Extensions")
;;; «.Mercurial-Qpatchs»                (to "Mercurial-Qpatchs")
;;; «.Delicious-bookmarks-tv»           (to "Delicious-bookmarks-tv")
;;; «.Traverse-in-anything»             (to "Traverse-in-anything")
;;; «.anything-etags»                   (to "anything-etags")
;;; «.Mark-rings»                       (to "Mark-rings")
;;; «.anything-sources»                 (to "anything-sources")
;;; «.Anything-set-source-filter»       (to "Anything-set-source-filter")
;;; «.Anything-faces»                   (to "Anything-faces")
;;; «.User-config»                      (to "User-config")
;;; «.Start-anything-in-miniwindow»     (to "Start-anything-in-miniwindow")
;;; «.Global-keymap»                    (to "Global-keymap")
;;; «.Info-at-point»                    (to "Info-at-point")
;;; «.only-etags»                       (to "only-etags")
;;; «.set-delicious-passwords»          (to "set-delicious-passwords")
;;; «.only-gentoo»                      (to "only-gentoo")
;;; «.only-kill-ring»                   (to "only-kill-ring")
;;; «.only-qpatchs»                     (to "only-qpatchs")
;;; «.only-register»                    (to "only-register")
;;; «.only-locate»                      (to "only-locate")
;;; «.only-semantic»                    (to "only-semantic")
;;; «.Surfraw-config»                   (to "Surfraw-config")
;;; «.anything-google-suggest»          (to "anything-google-suggest")
;;; «.anything-traverse-fontify-buffer» (to "anything-traverse-fontify-buffer")
;;; «.buffers-only»                     (to "buffers-only")
;;; «.yaoddmuse»                        (to "yaoddmuse")
;;; «.anything-regexp»                  (to "anything-regexp")
;;; «.anything-for-files»               (to "anything-for-files")
;;; «.xfonts»                           (to "xfonts")
;;; «.Provide-anything-config»          (to "Provide-anything-config")
;;; «.END»                              (to "END")

;;;==UPDATE-EEV-BOUNDARY== ;; (Don't delete this line!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;(require 'anything-config "~/labo/anything-config-dev-hg/anything-config.el")
(require 'anything-config)
(setq anything-c-use-standard-keys t)

;; «key-map»                            (to ".key-map")
(when anything-c-use-standard-keys
  (setq anything-map
        (let ((map (copy-keymap minibuffer-local-map)))
          (define-key map (kbd "<down>") 'anything-next-line)
          (define-key map (kbd "<up>") 'anything-previous-line)
          (define-key map (kbd "C-n")     'anything-next-line)
          (define-key map (kbd "C-p")     'anything-previous-line)
          (define-key map (kbd "<prior>") 'anything-previous-page)
          (define-key map (kbd "<next>") 'anything-next-page)
          (define-key map (kbd "M-v")     'anything-previous-page)
          (define-key map (kbd "C-v")     'anything-next-page)
          (define-key map (kbd "<right>") 'anything-next-source)
          (define-key map (kbd "<left>") 'anything-previous-source)
          (define-key map (kbd "<RET>") 'anything-exit-minibuffer)
          (define-key map (kbd "C-1") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-2") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-3") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-4") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-5") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-6") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-7") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-8") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "C-9") 'anything-select-with-digit-shortcut)
          (define-key map (kbd "<tab>")   'anything-select-action)
          (define-key map (kbd "C-z") 'anything-execute-persistent-action)
          (define-key map (kbd "C-k") 'anything-delete-minibuffer-content)
          (define-key map (kbd "C-o") 'anything-next-source)
          (define-key map (kbd "<C-M-up>")  'anything-scroll-other-window)
          (define-key map (kbd "<C-M-down>")  'anything-scroll-other-window-down)
          (define-key map (kbd "C-SPC") 'anything-toggle-visible-mark)
          (define-key map (kbd "M-[") 'anything-prev-visible-mark)
          (define-key map (kbd "M-]") 'anything-next-visible-mark)

          (define-key map (kbd "C-s") 'anything-isearch)
          (define-key map (kbd "C-r") 'undefined)
          (define-key map (kbd "C-x C-f") 'anything-quit-and-find-file)
          (define-key map (kbd "C-c C-f") 'anything-follow-mode)
          (define-key map (kbd "C-c C-d") 'anything-delete-current-selection)
          (define-key map (kbd "C-c C-y") 'anything-yank-selection)
          (define-key map (kbd "C-c C-k") 'anything-kill-selection-and-quit)

          ;; the defalias is needed because commands are bound by name when
          ;; using iswitchb, so only commands having the prefix anything-
          ;; get rebound
          (defalias 'anything-previous-history-element 'previous-history-element)
          (defalias 'anything-next-history-element 'next-history-element)
          (define-key map (kbd "M-p") 'anything-previous-history-element)
          (define-key map (kbd "M-n") 'anything-next-history-element)
          map))

  (setq anything-isearch-map
        (let ((map (copy-keymap (current-global-map))))
          (define-key map (kbd "<return>")    'anything-isearch-default-action)
          (define-key map (kbd "<tab>")       'anything-isearch-select-action)
          (define-key map (kbd "C-g")         'anything-isearch-cancel)
          (define-key map (kbd "C-s")         'anything-isearch-again)
          (define-key map (kbd "C-r")         'undefined)
          (define-key map (kbd "<backspace>") 'anything-isearch-delete)
          ;; add printing chars
          (let ((i ?\s))
            (while (< i 256)
              (define-key map (vector i) 'anything-isearch-printing-char)
              (setq i (1+ i))))
          map)))

;;;; «My-Sources»                       (to ".My-Sources")
;;; «w3m-meteo-yahoo»                   (to ".w3m-meteo-yahoo")

(defvar anything-c-source-yahoo-meteo
  '((name . "Yahoo Meteo")
    (candidates . (lambda ()
                    (get-meteo-keys)))
    (action . (("W3m search meteo" . (lambda (item)
                                       (w3m-frweather item)))))
    (volatile)
    (delayed)))

;; (anything 'anything-c-source-yahoo-meteo)

(defvar anything-bookmark-regions-alist nil)
(defvar anything-c-source-bookmark-regions
  '((name . "Bookmark Regions")
    (init . (lambda ()
              (condition-case nil
              (setq anything-bookmark-regions-alist
                    (bookmark-region-alist-only))
                (error nil))))
    (candidates . anything-c-bookmark-region-setup-alist)
    (action . (("Goto Bookmark" . (lambda (elm)
                                    (let ((bmk (car (split-string elm " => "))))
                                      (bookmark-jump bmk))))))))

;; (anything 'anything-c-source-bookmark-regions)

(defun anything-c-bookmark-region-setup-alist ()
  (loop for i in anything-bookmark-regions-alist
     for b = (car i)
     collect (concat
                 b
                 " => "
                 (bookmark-get-buffer-name b))))


;;;; «Extensions»                       (to ".Extensions")

;;;; «Mercurial-Qpatchs»                (to ".Mercurial-Qpatchs")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/anything-mercurial.el")
(require 'anything-mercurial)

;;;; «Delicious-bookmarks-tv»           (to ".Delicious-bookmarks-tv")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/anything-delicious.el")
(require 'anything-delicious)

;;;; «Traverse-in-anything»             (to ".Traverse-in-anything")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/anything-traverse.el")
(require 'anything-traverse)

;;;; «anything-etags»                   (to ".anything-etags")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/emacs/download/anything-etags.el)
(require 'anything-etags)

;;;; «Mark-rings»                       (to ".Mark-rings")
;(require 'anything-mark-rings)

;;;; «anything-sources»                 (to ".anything-sources")

(setq anything-sources
      (list anything-c-source-buffers+
            anything-c-source-bookmarks-local
            anything-c-source-bookmarks-su
            anything-c-source-bookmarks-ssh
            anything-c-source-eev-anchor
            ;anything-c-source-qapplied-patchs
            ;anything-c-source-qunapplied-patchs
            anything-c-source-files-in-current-dir+
            anything-c-source-file-name-history
            anything-c-source-info-pages
            anything-c-source-info-cl
            anything-c-source-info-elisp
            anything-c-source-man-pages
            anything-c-source-locate
            anything-c-source-recentf
            anything-c-source-emacs-commands
            ;anything-c-source-emacs-functions-with-abbrevs
            anything-c-source-imenu
            anything-c-source-org-headline
            anything-c-source-semantic
            ;anything-c-source-complex-command-history
            anything-c-source-bbdb
            ;anything-c-source-google-suggest
            ;anything-c-source-surfraw
            anything-c-source-yahoo-meteo
            ;anything-c-source-traverse-occur
	    anything-c-source-emms-dired
            anything-c-source-emms-streams
            ;anything-c-source-emms-playlist
            ;anything-c-source-delicious-tv
            anything-c-source-w3m-bookmarks
            anything-c-source-yaoddmuse-emacswiki-edit-or-view
            anything-c-source-yaoddmuse-emacswiki-post-library
            anything-c-source-lacarte
            anything-c-source-calculation-result
            anything-c-source-emacs-process
            ;anything-c-source-faces
            anything-c-source-customize-face
            anything-c-source-colors
            anything-c-source-kill-ring
            ;anything-c-source-global-mark-ring
            ;anything-c-source-mark-ring
            anything-c-source-register
            ;anything-c-source-xrandr-change-resolution
            ;anything-c-source-xfonts
            ;anything-c-source-icicle-region
            anything-c-source-bookmark-regions))
            ;anything-c-source-gentoo
            ;anything-c-source-use-flags
            ;anything-c-source-simple-call-tree-callers-functions
            ;anything-c-source-simple-call-tree-functions-callers))

;;; «Anything-set-source-filter»        (to ".Anything-set-source-filter")

(define-key anything-map (kbd "M-c") 'anything-tv-show-calcul-only)
(define-key anything-map (kbd "C-b") 'anything-tv-show-bbdb-only)
(define-key anything-map (kbd "C-i") 'anything-tv-show-info-only)
(define-key anything-map (kbd "M-m") 'anything-tv-show-man-only)
(define-key anything-map "I" 'anything-tv-show-imenu-and-eev-only)
(define-key anything-map (kbd "M-x") 'anything-tv-show-emacs-commands-only)
(define-key anything-map (kbd "M-g") 'anything-tv-show-gentoo-only)
;(define-key anything-map "G" 'anything-tv-show-google-only)
(define-key anything-map "Y" 'anything-tv-show-meteo-only)
(define-key anything-map "M" 'anything-tv-show-music-only)
(define-key anything-map "R" 'anything-tv-show-registers-only)
(define-key anything-map "K" 'anything-tv-show-kill-ring-only)
(define-key anything-map (kbd "C-F") 'anything-tv-show-colors-only)
(define-key anything-map "A" 'anything-tv-show-all)
(define-key anything-map "W" 'anything-tv-show-w3m-bookmarks-only)
(define-key anything-map "D" 'anything-tv-show-delicious-only)
(define-key anything-map (kbd "M-<f10>") 'anything-tv-show-alacarte-only)
(define-key anything-map "Q" 'anything-tv-show-qpatchs-only)
(define-key anything-map "T" 'anything-tv-show-traverse-only)
(define-key anything-map "B" 'anything-tv-show-bookmarks-only)
(define-key anything-map "F" 'anything-tv-show-files-only)
(define-key anything-map "O" 'anything-tv-show-org-only)
(define-key anything-map "L" 'anything-tv-show-locate-only)
(define-key anything-map (kbd "M-s") 'anything-tv-show-surfraw-only)
(define-key anything-map (kbd "M-b") 'anything-tv-show-buffers-only)
(define-key anything-map (kbd "M-I") 'anything-tv-show-icy-region-only)
(define-key anything-map (kbd "S") 'anything-tv-show-semantic-only)
;(define-key anything-map (kbd "C-m") 'anything-tv-show-mark-ring-only)

;; (defun anything-tv-show-mark-ring-only ()
;;   (interactive)
;;   (anything-set-source-filter '("mark-ring"
;;                                 "global-mark-ring")))
  
(defun anything-tv-show-semantic-only ()
  (interactive)
  (anything-set-source-filter '("Semantic Tags")))

(defun anything-tv-show-icy-region-only ()
  (interactive)
  (anything-set-source-filter '("Icicle Regions")))

(defun anything-tv-show-buffers-only ()
  (interactive)
  (anything-set-source-filter '("Buffers")))

(defun anything-tv-show-surfraw-only ()
  (interactive)
  (anything-set-source-filter '("Surfraw")))

(defun anything-tv-show-org-only ()
  (interactive)
  (anything-set-source-filter '("Org HeadLine")))

(defun anything-tv-show-calcul-only ()
  (interactive)
  (anything-set-source-filter '("Calculation Result")))

(defun anything-tv-show-bbdb-only ()
  (interactive)
  (anything-set-source-filter '("BBDB")))

(defun anything-tv-show-locate-only ()
  (interactive)
  (anything-set-source-filter '("Locate")))

(defun anything-tv-show-info-only ()
  (interactive)
  (anything-set-source-filter '("Info Pages"
                                "Info Elisp"
                                "Info Common-Lisp")))

(defun anything-tv-show-man-only ()
  (interactive)
  (anything-set-source-filter '("Manual Pages")))

(defun anything-tv-show-imenu-and-eev-only ()
  (interactive)
  (anything-set-source-filter '("Anchors"
                                ;"Semantic Tags"
                                "Imenu")))

(defun anything-tv-show-emacs-commands-only ()
  (interactive)
  (anything-set-source-filter '("Emacs Commands")))

(defun anything-tv-show-gentoo-only ()
  (interactive)
  (anything-set-source-filter '("Portage sources"
                                "Use Flags")))

(defun anything-tv-show-google-only ()
  (interactive)
  (anything-set-source-filter '("Google Suggest")))

(defun anything-tv-show-meteo-only ()
  (interactive)
  (anything-set-source-filter '("Yahoo Meteo")))


(defun anything-tv-show-files-only ()
  (interactive)
  (anything-set-source-filter '("File Name History"
                                "Files from Current Directory"
                                "Recentf"
                                "Locate")))

(defun anything-tv-show-music-only ()
  (interactive)
  (anything-set-source-filter '("Music Directory"
                                "Emms Streams")))

(defun anything-tv-show-bookmarks-only ()
  (interactive)
  (anything-set-source-filter '("Bookmarks-Local"
                                "Bookmarks-root"
                                "Bookmarks-ssh")))

(defun anything-tv-show-traverse-only ()
  (interactive)
  (anything-set-source-filter '("Traverse Occur")))

(defun anything-tv-show-qpatchs-only ()
  (interactive)
  (anything-set-source-filter '("Hg Qapplied Patchs"
                                "Hg Qunapplied Patchs")))

(defun anything-tv-show-alacarte-only ()
  (interactive)
  (anything-set-source-filter '("Lacarte")))

(defun anything-tv-show-delicious-only ()
  (interactive)
  (anything-set-source-filter '("Del.icio.us")))


(defun anything-tv-show-w3m-bookmarks-only ()
  (interactive)
  (anything-set-source-filter '("W3m Bookmarks")))

(defun anything-tv-show-all ()
  (interactive)
  (anything-set-source-filter nil))

(defun anything-tv-show-colors-only ()
  (interactive)
  (anything-set-source-filter '("Colors"
                                "Customize Face")))


(defun anything-tv-show-kill-ring-only ()
  (interactive)
  (anything-set-source-filter '("Kill Ring")))


(defun anything-tv-show-registers-only ()
  (interactive)
  (anything-set-source-filter '("Registers")))


;;;; «Anything-faces»                   (to ".Anything-faces")

(defface anything-delicious-tag-face '((t (:foreground "VioletRed4" :weight bold)))
  "Face for w3m bookmarks" :group 'anything)

(defface anything-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'anything)

(defface anything-tv-header '((t (:background "#22083397778B" :foreground "white" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

(setq anything-header-face 'anything-tv-header)

;;; «User-config»                       (to ".User-config")

(require 'anything-match-plugin)
(require 'anything-complete)

;; Automatically collect symbols by 150 secs
(anything-lisp-complete-symbol-set-timer 150)
(require 'shell-history)
(setq anything-input-idle-delay 0.6)
(setq anything-idle-delay 1.3)
(setq anything-candidate-number-limit 9999)
(setq anything-c-bookmarks-face1 'diredp-dir-priv)
(setq anything-c-buffers-face1 'diredp-dir-priv)
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(setq anything-quick-update t)
(setq anything-candidate-separator (propertize (make-string 42 ?-)
                                               'face 'traverse-match-face))

;; «Start-anything-in-miniwindow»       (to ".Start-anything-in-miniwindow")
(defun tv-anything-in-miniwindows (fn)
  (delete-other-windows)
  (split-window-horizontally 45)
  (other-window 1)
  (funcall fn)
  (save-excursion
    (other-window 1)
    (when (> (count-windows) 1)
      (delete-window))))

;; «Global-keymap»                      (to ".Global-keymap")
(global-set-key (kbd "<f5> A") 'anything)
(global-set-key (kbd "<f11> a") #'(lambda ()
                                    (interactive)
                                    (tv-anything-in-miniwindows 'anything)))
(global-set-key (kbd "<M-tab>") 'anything-lisp-complete-symbol)
(global-set-key (kbd "<C-M-tab>") 'anything-lisp-complete-symbol-partial-match)
(global-set-key (kbd "<f5> a p") 'anything-apropos)
(global-set-key (kbd "<f5> a d") #'(lambda ()
                                     (interactive)
                                     (tv-anything-in-miniwindows 'anything-delicious)))
(define-key anything-map "\C-r" 'anything-select-source)


;; «Info-at-point»                      (to ".Info-at-point")

(global-set-key (kbd "C-c h f") 'anything-info-at-point)

(require 'anything-dabbrev-expand)
(global-set-key "\M-/" 'anything-dabbrev-expand)
(define-key anything-dabbrev-map "\M-/" 'anything-dabbrev-find-all-buffers)

;; «only-etags»                         (to ".only-etags")
(defun anything-etags-maybe-at-point ()
  (interactive)
  (if current-prefix-arg
      (anything-etags-select-from-here)
      (anything-etags-select)))
(global-set-key (kbd "<f5> a e") 'anything-etags-maybe-at-point)


;; «only-gentoo»                        (to ".only-gentoo")
(global-set-key (kbd "<f5> a g") 'anything-gentoo)

;; «only-kill-ring»                     (to ".only-kill-ring")
(defun anything-kill-ring-only ()
  (interactive)
  (anything 'anything-c-source-kill-ring))

(global-set-key (kbd "C-M-y") 'anything-kill-ring-only)

;; «only-qpatchs»                       (to ".only-qpatchs")
(defun anything-qpatchs-only ()
  (interactive)
  (anything '(anything-c-source-qapplied-patchs
              anything-c-source-qunapplied-patchs)))

(global-set-key (kbd "<f5> a q") 'anything-qpatchs-only)

;; «only-register»                      (to ".only-register")
(defun anything-register-only ()
  (interactive)
  (anything 'anything-c-source-register))

(global-set-key (kbd "C-x a r") 'anything-register-only)

;; «only-locate»                        (to ".only-locate")
(defun anything-locate-only ()
  (interactive)
  (tv-anything-in-miniwindows #'(lambda ()
                                  (anything 'anything-c-source-locate))))

(global-set-key (kbd "<f5> a l") 'anything-locate-only)

;; «only-semantic»                      (to ".only-semantic")
(defun anything-semantic ()
  (interactive)
  (tv-anything-in-miniwindows
   #'(lambda ()
       (anything 'anything-c-source-semantic))))

(global-set-key (kbd "C-c C-b") 'anything-semantic)

;; «Surfraw-config»                     (to ".Surfraw-config")
(setq anything-c-surfraw-use-only-favorites t)
(setq anything-c-surfraw-favorites '("google" "wikipedia" "fast"
                                     "yahoo" "codesearch" "foldoc"
                                     "thesaurus" "translate"
                                     "genpkg" "genportage"
                                     "currency" "stockquote"))

(global-set-key (kbd "C-c g") 'anything-surfraw-only)

;; «anything-google-suggest» (to ".anything-google-suggest")
(global-set-key (kbd "<f5> g") 'anything-google-suggest)

;; «anything-traverse-fontify-buffer»   (to ".anything-traverse-fontify-buffer")
(setq anything-c-traverse-fontify-buffer t)
(setq anything-scroll-amount 1)

(defun anything-file-in-current-dir ()
  (interactive)
  (anything 'anything-c-source-files-in-current-dir+))
(global-set-key (kbd "C-x C-d") 'anything-file-in-current-dir)

(setq anything-c-files-in-current-tree-ignore-files '(".elc$" ".pyc$"
                                                      ".orig$" ".rej$"
                                                      "ANYTHING-TAG-FILE"))

(global-set-key (kbd "C-c C-d") 'anything-files-in-current-tree)

;; «buffers-only»                       (to ".buffers-only")
(defun anything-buffers-only ()
  (interactive)
  (anything 'anything-c-source-buffers+))
(global-set-key (kbd "<f5> a b") 'anything-buffers-only)

;; «yaoddmuse»                          (to ".yaoddmuse")
(setq anything-yaoddmuse-use-cache-file t)

;; «anything-regexp»                    (to ".anything-regexp")
(global-set-key (kbd "<f5> a r") 'anything-regexp)

;; «anything-for-files»                 (to ".anything-for-files")
(setq anything-for-files-prefered-list '(anything-c-source-recentf
                                         anything-c-source-files-in-current-dir+
                                         anything-c-source-locate
                                         anything-c-source-bookmarks-local))
                                         
(global-set-key (kbd "<f5> a f") 'anything-for-files)

;; «xfonts»                             (to ".xfonts")

(defun anything-xfonts-only ()
  (interactive)
  (anything 'anything-c-source-xfonts))

;;; «Provide-anything-config» (to ".Provide-anything-config")
(provide 'init-anything-thierry)

;; «END» (to ".END")
;; (to "INDEX")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-anything-thierry.el ends here
;; Local Variables:
;; eev-comment-prefix: ";"
;; ee-anchor-format: "«%s»"
;; End:
