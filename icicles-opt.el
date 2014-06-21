(defcustom icicle-completion-key-bindings
  `((,(icicle-kbd "M-return")  icicle-candidate-read-fn-invoke t)                     ;`M-RET'
                                                                                      ; (`M-return')
    (,(icicle-kbd "C-M-m")     icicle-candidate-read-fn-invoke t)                     ;`M-RET'
                                                                                      ; (`ESC RET')
    (,(icicle-kbd "C-S-return") icicle-candidate-alt-action t)                        ; `C-S-RET'
                                                                                      ; (`C-S-return')
    (,(icicle-kbd "delete")    icicle-remove-candidate t)                             ; `delete'
    (,(icicle-kbd "S-delete")  icicle-delete-candidate-object t)                      ; `S-delete'
    (,(icicle-kbd "C-w")       icicle-kill-region t)                                  ; `C-w'
    (,(icicle-kbd "C-!")       icicle-all-candidates-action t)                        ; `C-!'
    (,(icicle-kbd "C-|")       icicle-all-candidates-alt-action t)                    ; `C-|'
    (,(icicle-kbd "M-!")       icicle-all-candidates-list-action t)                   ; `M-!'
    (,(icicle-kbd "M-|")       icicle-all-candidates-list-alt-action t)               ; `M-|'
    (,(icicle-kbd "M-h")       icicle-history t)                                      ; `M-h'
    (,(icicle-kbd "M-pause")   icicle-keep-only-past-inputs t) ; `M-pause'
    (,(icicle-kbd "C-pause")   icicle-toggle-highlight-historical-candidates t)       ; `C-pause'
    (,(icicle-kbd "S-pause")   icicle-toggle-highlight-saved-candidates t)            ; `S-pause'
    (,(icicle-kbd "C-S-pause") icicle-toggle-WYSIWYG-Completions t)                   ; `C-S-pause'
    ;;$$$$$$  (,(icicle-kbd "C-M-pause") 'icicle-other-history) ; `C-M-pause'
    (,(icicle-kbd "C-insert")  icicle-switch-to-Completions-buf t)                    ; `C-insert'
    (,(icicle-kbd "insert")    icicle-save/unsave-candidate t)                        ; `insert'

    ;; In Emacs 22+, local is parent of local-completion
    ;; Note: `setup-keys.el' binds `C-o' to `1on1-fit-minibuffer-frame' if defined.
    (,(icicle-kbd "C-a")     icicle-beginning-of-line+
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-a'
    (,(icicle-kbd "C-e")     icicle-end-of-line+
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-e'
    (,(icicle-kbd "C-M-v")   icicle-scroll-forward
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-v'
    (,(icicle-kbd "C-M-S-v") icicle-scroll-backward
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-V'
                                                                                      ; (`C-M-S-v')
    (,(icicle-kbd "C-=")     icicle-insert-string-from-variable
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-='
    ;; Replaces `tab-to-tab-stop':
    (,(icicle-kbd "M-i")     icicle-clear-current-history
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-i'
    ;; Replaces `kill-sentence':
    (,(icicle-kbd "M-k")     icicle-erase-minibuffer-or-history-element
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-k'
    (,(icicle-kbd "M-o")     icicle-insert-history-element
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-o'
    (,(icicle-kbd "M-.")     icicle-insert-string-at-point
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-.'
    (,(icicle-kbd "C-x C-f") icicle-resolve-file-name
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-x C-f'
    (,(icicle-kbd "M-:")     icicle-pp-eval-expression-in-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-:'
    (,(icicle-kbd "C-M-y") icicle-yank-secondary
     (and (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))
      (fboundp 'icicle-yank-secondary)))                                              ; `C-M-y'
    (,(icicle-kbd "C-M-pause")  icicle-other-history
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-M-pause'
    (,(icicle-kbd "M-S-backspace") icicle-erase-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-S-backspace'
    (,(icicle-kbd "M-S-delete") icicle-erase-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-S-delete'

    ;; Need `C-g', even if `minibuffer-local-completion-map' inherits from `minibuffer-local-map'.
    (,(icicle-kbd "C-g")       icicle-abort-recursive-edit t)                         ; `C-g'
    (,(icicle-kbd "M-q")       icicle-dispatch-M-q t)                                 ; `M-q'
    (,(icicle-kbd "C-l")       icicle-retrieve-previous-input t)                      ; `C-l'
    (,(icicle-kbd "C-S-l")     icicle-retrieve-next-input t)                          ; `C-L' (`C-S-l')
    (,(icicle-kbd "M-$")       icicle-candidate-set-truncate t)                       ; `M-$'
    (,(icicle-kbd "C-~")       icicle-candidate-set-complement t)                     ; `C-~'
    (,(icicle-kbd "C--")       icicle-candidate-set-difference t)                     ; `C--'
    (,(icicle-kbd "C-+")       icicle-candidate-set-union t)                          ; `C-+'
    (,(icicle-kbd "C-*")       icicle-candidate-set-intersection t)                   ; `C-*'
    (,(icicle-kbd "C->")       icicle-candidate-set-save-more t)                      ; `C->'
    (,(icicle-kbd "C-M->")     icicle-candidate-set-save t)                           ; `C-M->'
    (,(icicle-kbd "C-(")       icicle-next-TAB-completion-method t)                   ; `C-('
    (,(icicle-kbd "M-(")       icicle-next-S-TAB-completion-method t)                 ; `M-('
    (,(icicle-kbd "C-)")       icicle-candidate-set-save-more-selected t)             ; `C-)'
    (,(icicle-kbd "C-M-)")     icicle-candidate-set-save-selected t)                  ; `C-M-)'
    (,(icicle-kbd "C-M-<")     icicle-candidate-set-retrieve t)                       ; `C-M-<'
    (,(icicle-kbd "C-M-}")     icicle-candidate-set-save-to-variable t)               ; `C-M-}'
    (,(icicle-kbd "C-M-{")     icicle-candidate-set-retrieve-from-variable t)         ; `C-M-{'
    (,(icicle-kbd "C-}")       icicle-candidate-set-save-persistently t)              ; `C-}'
    (,(icicle-kbd "C-{")       icicle-candidate-set-retrieve-persistent t)            ; `C-{'
    (,(icicle-kbd "C-%")       icicle-candidate-set-swap t)                           ; `C-%'
    (,(icicle-kbd "M-%")       icicle-regexp-quote-input t)                           ; `M-%'
    (,(icicle-kbd "C-:")       icicle-candidate-set-define t)                         ; `C-:'
    (,(icicle-kbd "C-M-j")     icicle-insert-list-join-string t)                      ; `C-M-j'
    (,(icicle-kbd "C-,")       icicle-change-sort-order t)                            ; `C-,'
    (,(icicle-kbd "C-M-\;")     icicle-toggle-ignoring-comments t)                    ; `C-M-;'
    (,(icicle-kbd "C-`")       icicle-toggle-regexp-quote t)                          ; `C-`'
    (,(icicle-kbd "C-M-.")     icicle-toggle-dot t)                                   ; `C-M-.'
    (,(icicle-kbd "C-M-`")     icicle-toggle-literal-replacement t)                   ; `C-M-`'
    (,(icicle-kbd "C-<")       icicle-candidate-set-retrieve-more t)                  ; `C-<'
    (,(icicle-kbd "C-M-_")     icicle-toggle-proxy-candidates t)                      ; `C-M-_'
    (,(icicle-kbd "C-$")       icicle-toggle-transforming t)                          ; `C-$'
;;;     ;; In Emacs 22+, local is parent of local-completion
;;;     ;; $$$$$$ Keep `C-?' also for a while, undocumented, for backward compatibility only.
;;;     (,(icicle-kbd "C-?")     icicle-minibuffer-help
;;;      (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-?'
    (,(icicle-kbd "M-?")     icicle-minibuffer-help
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `M-?'
    (,(icicle-kbd "C-.")       icicle-dispatch-C-. t)                                 ; `C-.'
    (,(icicle-kbd "C-#")       icicle-cycle-incremental-completion t)                 ; `C-#'
    (,(icicle-kbd "C-M-#")     icicle-toggle-icomplete-mode t)                        ; `C-M-#'
    (,(icicle-kbd "C-\"")      icicle-toggle-expand-to-common-match t)                ; `C-"'
    (,(icicle-kbd "C-M-\"")    icicle-cycle-expand-to-common-match t)                 ; `C-M-"'
    (,(icicle-kbd "M-\;")      icicle-toggle-search-replace-common-match t)           ; `M-;'
    (,(icicle-kbd "C-^")       icicle-dispatch-C-^ t)                                 ; `C-^'
    (,(icicle-kbd "C-M-^")     icicle-toggle-completions-format t)                    ; `C-M-^'
    (,(icicle-kbd "C-S-a")     icicle-toggle-case-sensitivity t)                      ; `C-A' (`C-S-a')
    (,(icicle-kbd "M-~")       icicle-toggle-~-for-home-dir t)                        ; `M-~'
    (,(icicle-kbd "C-M-~")     icicle-toggle-search-complementing-domain t)           ; `C-M-~'
    (,(icicle-kbd "M-g")       icicle-toggle-C-for-actions t)                         ; `M-g'
    (,(icicle-kbd "M-,")       icicle-dispatch-M-comma t)                             ; `M-,'
    (,(icicle-kbd "C-M-,")     icicle-toggle-alternative-sorting t)                   ; `C-M-,'
    (,(icicle-kbd "C-M-+")     icicle-plus-saved-sort t)                              ; `C-M-+'
    (,(icicle-kbd "M-+")       icicle-widen-candidates t)                             ; `M-+'
    (,(icicle-kbd "M-*")       icicle-narrow-candidates t)                            ; `M-*'
    (,(icicle-kbd "M-&")       icicle-narrow-candidates-with-predicate t)             ; `M-&'
    (,(icicle-kbd "M-_")       icicle-dispatch-M-_ t)                                 ; `M-_'
    (,(icicle-kbd "C-M-&")     icicle-save-predicate-to-variable t)                   ; `C-M-&'
    (,(icicle-kbd "S-SPC")     icicle-apropos-complete-and-narrow t)                  ; `S-SPC'
    (,(icicle-kbd "S-return")  icicle-apropos-complete-and-exit t)                    ; `S-return'
    (,(icicle-kbd "S-backspace") icicle-apropos-complete-and-widen t)                 ; `S-backspace'
    (,(icicle-kbd "C-v")       icicle-scroll-Completions-forward t)                   ; `C-v'
    (,(icicle-kbd "M-v")       icicle-scroll-Completions-backward t)                  ; `M-v'
    (,(icicle-kbd ".")         icicle-insert-dot-command t)                           ; `.'
    (,(icicle-kbd "M-m")       icicle-toggle-show-multi-completion t)                 ; `M-m'
    (,(icicle-kbd "M-r")       icicle-roundup t)                                      ; `M-r'
    (,(icicle-kbd "C-x .")     icicle-dispatch-C-x. t)                                ; `C-x .'
    (,(icicle-kbd "C-x :")     icicle-toggle-network-drives-as-remote t)              ; `C-x :'
    (,(icicle-kbd "C-x /")     icicle-toggle-expand-directory t)                      ; `C-x /'
    (,(icicle-kbd "C-x C-a")   icicle-toggle-annotation t)                            ; `C-x C-a'
    (,(icicle-kbd "C-x C-0")   icicle-recomplete-from-original-domain t)              ; `C-x C-0'
    (,(icicle-kbd "C-x t")     icicle-cycle-image-file-thumbnail                      ; `C-x t'
     (fboundp 'icicle-cycle-image-file-thumbnail))
    (,(icicle-kbd "C-x w")     icicle-doremi-candidate-width-factor+                  ; `C-x w'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x |")     icicle-doremi-inter-candidates-min-spaces+             ; `C-x |'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x #")     icicle-doremi-increment-max-candidates+                ; `C-x #'
     (fboundp 'doremi))
    (,(icicle-kbd "C-x -")     icicle-doremi-zoom-Completions+                        ; `C-x -'
     (and (fboundp 'doremi)
      (fboundp 'text-scale-increase)))
    (,(icicle-kbd "C-x 1")     icicle-doremi-increment-swank-timeout+
     (and (fboundp 'doremi)  (eq (icicle-current-TAB-method) 'swank)))
    ;; NO - NEED TO DO THE SWANK PART AT RUNTIME, in icicles-mode.el
    (,(icicle-kbd "C-x 2") icicle-doremi-increment-swank-prefix-length+
     (and (fboundp 'doremi)  (eq (icicle-current-TAB-method) 'swank)))
    (,(icicle-kbd "C-x C-M->") bmkp-set-icicle-search-hits-bookmark                   ; `C-x C-M->'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    (,(icicle-kbd "C-x C-M-<") bmkp-retrieve-icicle-search-hits                       ; `C-x C-M-<'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    (,(icicle-kbd "C-x C-<")   bmkp-retrieve-more-icicle-search-hits                  ; `C-x C-<'
      (fboundp 'bmkp-set-icicle-search-hits-bookmark))
    ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
    (,(icicle-kbd "?")         icicle-self-insert t)                                  ; `?
    (,(icicle-kbd "SPC")       icicle-self-insert t)                                  ; " "
    ;; In Emacs 22+, local is parent of local-completion
    (,(icicle-kbd "C-j")     icicle-insert-newline-in-minibuffer
     (not (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map)))) ; `C-j
    )
  "*List of minibuffer key bindings during completion in Icicle mode.
The option value has the same form as that of option
`icicle-top-level-key-bindings' (which see).
Each list element is of custom type `icicle-key-definition' and has
the form (KEY COMMAND CONDITION).

If you customize this option then you must exit and re-enter Icicle
mode to ensure that the change takes effect.  This is really necessary
only if your changes would undefine a key.

For this option to have an effect upon startup, it must be set before
you enter Icicle mode.  This means that you must ensure that the code
that sets it is invoked before you enter Icicle mode.  If you use
Customize to change this option, then ensure that the code inserted by
Customize into your `user-init-file' or your `custom-file' is invoked
before you enter Icicle mode."
  :type (if (> emacs-major-version 21)
            '(repeat icicle-key-definition)
          '(repeat
            (list
             (choice
              (restricted-sexp :tag "Key"
               :match-alternatives ((lambda (x) (or (stringp x)  (vectorp x)))) :value [ignore])
              (restricted-sexp :tag "Command to remap"
               ;; Use `symbolp' instead of `commandp', in case the library defining the
               ;; command is not loaded.
               :match-alternatives (symbolp) :value ignore))
             ;; Use `symbolp' instead of `commandp'...
             (restricted-sexp :tag "Command"
              :match-alternatives (symbolp) :value ignore)
             (sexp :tag "Condition"))))
  :group 'Icicles-Key-Bindings)
