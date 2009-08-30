;;; customize of local grep command.
(require 'find-dired+)
(require 'w32-find-dired)
(require 'grep+)

(autoload 'igrep "igrep"
  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep"
  "*Run `grep` via `find`..." t)
;; 使用gnu的grep和find
;; 注意，需要拷贝grep和find到可执行的目录
(setq igrep-program "ggrep")
(setq grep-program "ggrep")
(setq find-program "gfind")
(setq igrep-find-program "gfind")

(setq igrep-find-use-xargs nil)
(setq igrep-case-fold-search nil)
(setq igrep-find t)
(setq igrep-menu-bar t)

(setq grepp-default-regexp-fn nil)

;; ;; f3用于C/C++文件的符号查找
;; (global-set-key [(f3)] 'igrep)

;; (global-set-key [(f3)]  'wuxch-find-dired)
;; (global-set-key [(control f3)] 'wuxch-find-grep-dired)

(defun wuxch-find-dired (dir args)
  "Run `find' and put its output in a buffer in Dired Mode.
Then run `find-dired-hook' and `dired-after-readin-hook'.
The `find' command run (after changing into DIR) is:

    find . \\( ARGS \\) -ls"
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
     (list (read-file-name "Run `find' in directory: " nil "" t)
           (read-from-minibuffer "Run `find' (with regex)): " default
                                 nil nil 'find-args-history default t))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (abbreviate-file-name
               (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p dir)      ; Ensure that it's really a directory.
      (error "Command `find-dired' needs a directory: `%s'" dir))
    ;; (switch-to-buffer (create-file-buffer (directory-file-name dir)))
    (if (get-buffer "<find result>")
        (kill-buffer "<find result>"))
    (switch-to-buffer (create-file-buffer "find result"))
    ;; 考虑出去目录名中的<>
    ;; (rename-buffer (concat "find:" (substring (buffer-name) 0 (- (length (buffer-name)) 3)))))
    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir)
    ;; (setq args (concat
    ;;                 "gfind . " (if (string= "" args) "" (concat "\\( " args " \\) "))
    ;;                 (car find-ls-option)))
    (setq args (concat
                "gfind . -regex " args " -ls "))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr find-ls-option))
    ;; This really should rerun the find command, but I don't
    ;; have time for that.
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "g" 'undefined)

    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
        ;; will work even with nested dired format (dired-nstd.el,v 1.15
        ;; and later)
        (dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker)))))
    (setq buffer-read-only nil)
    ;; Subdir headerline must come first because the first marker in
    ;; `subdir-alist' points there.
    (insert "  " dir ":\n")
    ;; Make second line a "find" line in analogy to the "total" or
    ;; "wildcard" line.
    (insert "  " args "\n")
    ;; Start the `find' process.
    (let ((proc (start-process-shell-command "gfind" (current-buffer) args)))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer))
      (setq mode-line-process '(": %s `gfind'"))
      (run-hooks 'find-dired-hook 'dired-after-readin-hook))
    )

  )


(defun wuxch-find-grep-dired (dir file-regexp grep-regexp)
  "Find files in DIR containing a regexp REGEXP.
The output is in a Dired buffer.
The `find' command run (after changing into DIR) is:

    find . -exec grep -s REGEXP {} \\\; -ls

Thus REGEXP can also contain additional grep options."
  (interactive
   (let ((default (and find-dired-default-fn
                       (funcall find-dired-default-fn))))
     (list (read-file-name "Find-grep (directory): " nil "" t)
           (read-from-minibuffer "Find-grep (file regex): " default
                                 nil nil 'find-args-history default t)
           (read-from-minibuffer "Find-grep (grep regex): " default
                                 nil nil 'dired-regexp-history default t))))
  ;; find -exec doesn't allow shell i/o redirections in the command,
  ;; or we could use `grep -l >/dev/null'
  ;; We use -type f, not ! -type d, to avoid getting screwed
  ;; by FIFOs and devices.  I'm not sure what's best to do
  ;; about symlinks, so as far as I know this is not wrong.
  (wuxch-find-dired dir
                    (concat "-type f -regex " file-regexp " -exec grep " find-grep-options " "
                            grep-regexp " {} ; ")))

(provide 'wuxch-find-grep)
