;;;----------------------------------------------------------------------
;; template profile
;;----------------------------------------------------------------------

(defvar templates-enabled nil)
(defun templates-enabled-p ()
  "templates-enabled-p

   return non-nil when templates are enabled.
  "
  (when templates-enabled t))

(defvar templates-loaded nil "a list of the languages with templates loaded")

(grail-load 'yasnippet (grail-define-installer
                         "yasnippet" "svn"
                         "http://yasnippet.googlecode.com/svn/trunk/"))


;; don't automatically activate.
(setq yas/dont-activate t)

;; This is a really ugly situation. The default non-nil behavior is to
;; unbind the yasnippet keymap and call the previous keymap function
;; which is of course dwim-tab. When calling yas/expand from inside
;; dwim-tab you get an infinite loop. setting this to nil fixes this.
(setq yas/fallback-behavior nil)

(defvar template-group-yasnippet-tree
  (expand-file-name (concat grail-elisp-root "templates/yasnippet/"))
  "the yasnippet tree path relative to grail-elisp-root")

;; these have the generic template reference so I can switch or mix
;; template systems without going on a grep marathon.

(defun template-in-reference ()
  (when (mode-overlay-at-point-p 'yas/snippet-reference) t))

(defun template-next-next-field ()
  (interactive)
  (when (template-in-reference)
    (yas/next-field-group)
    t))

(defun templates-load-for-language ( language )
  (let
    ((templates (concat template-group-yasnippet-tree language)))

    (when (file-accessible-directory-p templates)
      (yas/load-directory templates)
      (add-to-list 'templates-loaded language)) ))

(defun template-expand ()
  (interactive)
  (when (equal 'expanded (yas/expand)) (throw 'terminate-complete t))
  nil)

;; signal that a template system is activated
(setq templates-enabled t)

;; always load the lisp templates.
(templates-load-for-language "lisp")

(eval-after-load 'cperl-mode
  '(templates-load-for-language "perl"))

(eval-after-load 'nxml-mode
  '(templates-load-for-language "web"))




