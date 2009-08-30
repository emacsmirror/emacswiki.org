;;; minibuf-electric-gnuemacs.el

;;---------------------------------------------------------------------
;; Teaches `read-file-name' under GNU-Emacs to be as smart as it
;; already is under XEmacs: that is, typing " / " or " ~ " as the
;; first character after C-x C-f will empty the minibuffer.
;;
;; This gem of unparalled value was uncovered by
;; dom@kilimandjaro.dyndns.org in the help-emacs-windows mailing
;; list archive,
;; http://lists.gnu.org/archive/html/help-emacs-windows/2001-12/msg00183.html
;;
;; `minibuffer-electric-slash' stolen from XEmacs.
;; Minor-mode:ized by Anders Lindgren.
;;
;; Since some of the code comes from XEmacs, and lacking a contact
;; address for this Mr Anders Lindgren, I'm assuming the GPL license
;; applies, hence the following mantra (to please the Emacs-Wiki
;; upload janitor):
;;
;; minibuf-electric-gnuemacs.el is free software, and may be distributed
;; under the terms of the GNU General Public License version 2.

(if (not (fboundp 'minibuffer-electric-slash))
    (progn
      (defvar my-electric-file-mode nil)
      (defvar my-electric-file-mode-map (make-sparse-keymap))
      (make-local-variable 'my-electric-file-mode)

      ;; Backward compatibility.
      (if (not (fboundp 'minibuffer-prompt-end))
          (defun minibuffer-prompt-end ()
            (point-min)))

      (defun minibuffer-electric-slash ()
        ;; by address@bogus.example.com
        (interactive)
        (and (eq ?/ (preceding-char))
             ;; permit `//hostname/path/to/file'
             (not (eq (point) (1+ (minibuffer-prompt-end))))
             ;; permit `http://url/goes/here'
             (not (eq ?: (char-after (- (point) 2))))
             (delete-region (minibuffer-prompt-end) (point)))
        (insert ?/))

      (defun minibuffer-electric-tilde ()
        (interactive)
        (if (eq ?/ (preceding-char))
            (delete-region (minibuffer-prompt-end) (point)))
        (insert ?~))

      (defun minibuffer-electric-dollar ()
        (interactive)
        (if (eq ?/ (preceding-char))
            (delete-region (minibuffer-prompt-end) (point)))
        (insert ?$))

      ;; TODO: Check the "special syntax" list (maybe colon is used
      ;; by ange-ftp?)
      (defun minibuffer-electric-colon ()
        (interactive)
        (let ((pos nil))
          (save-excursion
            (unless (equal (skip-chars-backward "a-zA-Z") 0)
              (setq pos (point))
              ;; Allow //http:
              (if (equal (skip-chars-backward "/") -2)
                  (setq pos (point)))))
          (if pos
              (delete-region (minibuffer-prompt-end) pos))
          (insert ?:)))

      (define-key my-electric-file-mode-map "/" 'minibuffer-electric-slash)
      (define-key my-electric-file-mode-map "~" 'minibuffer-electric-tilde)
      (define-key my-electric-file-mode-map "$" 'minibuffer-electric-dollar)
      (define-key my-electric-file-mode-map ":" 'minibuffer-electric-colon)

      (setq minor-mode-map-alist (cons (cons 'my-electric-file-mode
                                             my-electric-file-mode-map)
                                       minor-mode-map-alist))

      (defun my-electric-minibuffer-setup ()
        (setq my-electric-file-mode
              (and (boundp 'minibuffer-completion-table)
                   (eq minibuffer-completion-table
        'read-file-name-internal))))
      (defun my-electric-minibuffer-exit ()
        (setq my-electric-file-mode nil))

      (add-hook 'minibuffer-setup-hook 'my-electric-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook  'my-electric-minibuffer-exit)))

(provide 'minibuf-electric-gnuemacs)

;;; minibuf-electric-gnuemacs.el ends here
