;;; ac-sources:
;; '(ac-source-ghc-module ac-source-ghc-symbol ac-source-ghc-pragmas ac-source-ghc-langexts
;;; usage:
;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (mapc (lambda (x) (add-to-list 'ac-sources x))
;;                   (ac-source-ghc-module
;;                    ac-source-ghc-symbol
;;                    ac-source-ghc-pragmas
;;                    ac-source-ghc-langexts))))

(provide 'ac-ghc-mod)

(require 'cl)
(require 'rx)

(require 'haskell-doc)
(require 'ghc)

(defvar ac-source-ghc-module nil "ac-source for import statement.")
(setq ac-source-ghc-module
      `((candidates . (lambda () ghc-module-names))
        (prefix . "^import +\\(.*\\)")
        (requires . 0)
        (symbol . "m")))

(defvar ac-source-ghc-symbol nil "ac-source by ghc-merged-keyword with quickhelp.")
(defvar ac-source-ghc-symgol-enable-quickhelp t)
(defvar ac-source-ghc-symbol-enable-quickhelp-by-hoogle nil)
(defun ac-source-ghc-symbol-quickhelp ()
  (if ac-source-ghc-symgol-enable-quickhelp
      (or (haskell-doc-sym-doc x)
          (and ac-source-ghc-symbol-enable-quickhelp-by-hoogle
               (concat (haskell-type-by-hoogle x) "\n\nby Hoogle"))
          (not '(with-current-buffer inferior-haskell-buffer
                  (save-excursion (let ((start (point))
                                        (res (inferior-haskell-info x)))
                                    (delete-region start (point-max))
                                    (if (string-match "Not in Scope" res)
                                        nil
                                      res))))))))
(setq ac-source-ghc-symbol
      '((candidates . (lambda () ghc-merged-keyword))
        (document . ac-source-ghc-symbol-quickhelp)))

(defvar ac-source-ghc-pragmas)
(setq ac-source-ghc-pragmas (lexical-let* ((ghc-pragmas '("LANGUAGE" "OPTIONS_GHC" "INCLUDE" "WARNING" "INLINE" "ANN" "LINE" "RULES" "SPECIALIZE" "SPECIALIZE instance" "UNPACK" "SOURCE" ))
                     (prefix (rx (and bol (* blank) "{-" (* blank) "#" (* blank) (group (* (any "_"  alnum)))))))
        `((candidates . (lambda () ',ghc-pragmas))
          (prefix . ,prefix)
          (symbol . "pragma")
          (requires . 0))))

(defun haskell-type-by-hoogle (sym)
  (shell-command-to-string (format "hoogle %s" sym)))

(defvar ac-source-ghc-langexts)
(setq ac-source-ghc-langexts
      `((candidates . (lambda () ghc-language-extensions))
        (prefix . ac-source-ghc-langexts-prefix)
        (requires . 0)
        (symbol . "LANGUAGE")))

(defvar ac-source-ghc-langexts-pattern
  (rx bol (* blank) "{-" (* blank) "#" (* blank) "LANGUAGE" (+ blank)
              (or (and (+ (and (+ alnum) (* blank) "," (* blank)))  (group (* alnum)))
                  (and (group (* alnum))))))

(defun ac-source-ghc-langexts-prefix ()
  (save-excursion
    (if (re-search-backward (concat ac-source-ghc-langexts-pattern "\\=") nil t) 
        (or (match-beginning 1) (match-beginning 2))
      (if (ac-source-ghc-find-begininng-lang-pragma)
          (- (point) (length (thing-at-point 'word))))) ))

(defun ac-source-ghc-find-begininng-lang-pragma ()
  (save-excursion
    (goto-char (line-beginning-position))
    (block search-langexts-line
    (while (re-search-forward (rx (+ blank) (? ",") (* blank) (* alnum)) (line-end-position) t)
      (if (= -1 (forward-line -1))
          (return-from search-langexts-line nil))
      (if (re-search-forward  (rx bol (* blank) "{-" (* blank) "#" (* blank) "LANGUAGE") (line-end-position) t)
          (return-from search-langexts-line (point))
        )))))
