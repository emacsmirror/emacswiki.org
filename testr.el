;;; testr-minor-mode - Quick and easy Ruby TDD

;; Copyright (C) Jose Pablo Barrantes 2012 <xjpablobrx@gmail.com>

;; Licensed under the same terms as Emacs.

;; Keywords: test ruby
;; Created: 2012-01-28
;; Author: Jose Pablo Barrantes 2012 <xjpablobrx@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/jpablobr/testr

;; Mostly based off Jim Weirich emacs-setup-esk/testing.el:
;; https://github.com/jimweirich/emacs-setup-esk/blob/master/testing.el

;;; Installation

;; see README.md

(require 'compile)

;;; Name of the asynchronous test process.
(defconst testr-process-name "*test-process*")

;;; Name of the test process output buffer.
(defconst testr-buffer-name "*testing*")

;;; Path to Ruby
(defconst testr-ruby-program "ruby")

;;; Name of the ruby debugging command to run the tests in debug mode.
(defconst testr-rdebug-command "rdebug")

;;; Name of the command to remove ANSI terminal cruft
(defconst testr-noansi-command  "noansi")

;;; NOANSI option string (may be empty, or a pipe to the noansi command)
;;;(defconst testr-noansi-option  (concat " | " testr-noansi-command))
(defconst testr-noansi-option  "")

;;; Name of the rake command ot run the rake based tests.
(defconst testr-rake-command "rake")

;;; Options to be added to the ruby based test commands.
(defconst testr-options "-Ilib:test:.")

;;; Options to be added to the spec command.
(defconst testr-spec-options "")

;;; If true, run the tests with warnings turned on.
(defvar testr-warnings t)

;;; If true, display the testing buffer in a single window (rather
;;; than a split window).
(defvar testr-single-window nil)

;;; If true, keep the mode line in the compilation buffer.
(defvar testr-keep-mode-line nil)

;;; Name of the last buffer running a file or method style test.
(defvar testr-last-test-buffer nil)

;;; Regexp for matching test unit test names
(defvar testr-test-unit-pattern "^ *def *\\(test_[a-zA-Z0-9_]+\\(!\\|\\?\\)?\\)")

;;; Regexp for matching test unit test names
(defvar testr-shoulda-pattern "^ *should +\\('[^']+'\\|\"[^\"]+\"\\)")

(defvar testr-name-pattern
  "^ *\\(def\\|should\\|context\\|test\\) +\\(\\(test_[a-zA-Z0-9_]+[!?]?$\\)\\|'\\([^']+\\)'\\|\"\\([^\"]+\\)\"\\)" )

(defvar testr-all-pattern testr-name-pattern )

(set-face-attribute (make-face 'test-heading1) nil
                    :family "arial"
                    :height 240
                    :background "#000000"
                    :foreground "#9999ff"
                    :weight 'bold)

(set-face-attribute (make-face 'test-heading2) nil
                    :family "arial"
                    :height 180
                    :background "#000000"
                    :foreground "#9999ff"
                    :weight 'bold)

(set-face-attribute (make-face 'test-success) nil
                    :family "arial"
                    :height 240
                    :background (if window-system "black" "#001100")
                    :foreground (if window-system "#33ff33" "white")
                    :weight 'bold)

(set-face-attribute (make-face 'test-failure) nil
                    :family "arial"
                    :height 240
                    :background (if window-system "black" "#110000")
                    :foreground (if window-system "ff3333" "white")
                    :weight 'bold)

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\([0-9]+ examples?, 0 failures?.*\n\\)"
               (1 'test-success)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(.* 0 failures, 0 errors.*\n\\)"
               (1 'test-success)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^\\(.* [1-9][0-9]* \\(failures?\\|errors?\\).*\n\\)"
               (1 'test-failure)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^= \\(.*\n\\)"
               (1 'test-heading1)))

(add-to-list 'compilation-mode-font-lock-keywords
             '("^==+ \\(.*\n\\)"
               (1 'test-heading2)))

(defun testr-warning-options()
  (if testr-warnings "-w " "") )

(defun testr-option-string()
  (concat (testr-warning-options) testr-options))

(defun testr-remove-stupid-messages ()
  (save-excursion
    (goto-char (max (- (point) 10) (point-min)))
    (while (re-search-forward "\\(WARNING\\|CONTEXT\\|NOTICE\\):.*
" nil t)
      (replace-match "_"))))

(defun testr-remove-crud ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(
\\| \\[[0-9]+[a-zA-Z]\\)" nil t)
      (replace-match "") )))

(defun testr-compilation-buffer-hook-function ()
  "Remove carriage returns that occasionally pollute the compile buffer."
  (save-current-buffer
    (set-buffer (get-buffer testr-buffer-name))
    (let ((buffer-read-only nil))
      (testr-remove-crud))))

(add-hook 'compilation-filter-hook 'testr-compilation-buffer-hook-function)

(defun testr-build-command-line (args)
  "Define the command line needed to run the given command and arguments."
  (let ((proj-env (testr-project-env-file default-directory))
        (command  (mapconcat (lambda (x) x) args " ")))
    (if proj-env
        (concat ". " proj-env "; " command testr-noansi-option)
      (concat command testr-noansi-option))))

(defun testr-start-process (&rest args)
  "Start the test process using the compilation package."
  (compilation-start
   (testr-build-command-line args)
   nil
   (lambda (x) testr-buffer-name)))

(defun testr-start-debugging (&rest args)
  (rdebug (mapconcat (lambda (x) x) args " ")) )

(defun testr-prep-test-buffer ()
  "Prepare the test buffer for inserting output from the test process."
  (let ((buf (get-buffer testr-buffer-name)))
    (if buf (kill-buffer buf))
    (setq buf (get-buffer-create testr-buffer-name))
    (testr-push-buffer buf)))

(defun testr-spec-file-name-p (file-name)
  "Is the given file name a spec file?"
  (string-match "\\bspec\\b" (file-name-nondirectory file-name)) )

(defun testr-unit-file-name-p (file-name)
  "Is the given file name a test file?"
  (string-match "\\btest\\b" (file-name-nondirectory file-name)) )

(defun testr-runnable-test-file-p (file-name)
  "Is the given file name a test or spec file?"
  (or (testr-unit-file-name-p file-name)
      (testr-spec-file-name-p file-name)))

(defun testr-target-file-name (file-name)
  "Return the test file name associated with the given file name."
  (cond ((testr-runnable-test-file-p file-name) file-name)
        ((toggle-filename file-name toggle-mappings))
        (t file-name) ))

(defun testr-extract-name ()
  "Extract the name of the test from the match."
  (cond
   ((match-beginning 3)
    (buffer-substring (match-beginning 3) (match-end 3)))
   ((match-beginning 4)
    (buffer-substring(match-beginning 4) (match-end 4)))
   ((match-beginning 5)
    (buffer-substring (match-beginning 5) (match-end 5))) ))

(defun testr-find-test-method-name ()
  "Return the name of the current test method."
  (save-excursion
    (forward-line)
    (re-search-backward testr-name-pattern)
    (testr-extract-name)))

(defun testr-find-existing-file (files)
  "Return the first file name in the list of files that exists, or nil."
  (cond ((null files) ())
        ((file-exists-p (car files)) (car files))
        (t (testr-find-existing-file (cdr files)))))

(defun testr-spec-command (buffer)
  "Return the name of the appropriate spec command to run for the given buffer."
  (let* ((default-directory (testr-find-project-top (buffer-file-name buffer))))
    (or (msg (concat "---: '" (getenv "HOME") "' ---"))
        (getenv "RSPEC_CMD")
        (testr-find-existing-file
         (list (concat default-directory "script/spec")
               (concat default-directory "vendor/plugins/rspec/bin/spec")))
        "$SPEC_COMMAND")))

(defun testr-find-spec-name ()
  "Return the name of the current test method."
  (save-excursion
    (forward-line)
    (re-search-backward "^ *it +['\"]\\([^\"]*\\)['\"] +do")
    (buffer-substring (match-beginning 1) (match-end 1))))

(defun testr-take-down-test-buffer ()
  "If the test buffer is in the front, take it down.
Make an attempt to get back to the last buffer that was used in a
test."
  (if (string-equal testr-buffer-name (buffer-name))
      (progn
        (kill-buffer testr-buffer-name)
        (if testr-last-test-buffer (pop-to-buffer testr-last-test-buffer)) )))

(defun testr-deal-with-mode-line ()
  "Remove the mode line if so configured.
The compilation buffer by default gets a mode line.  Remove it
unless the testr-keep-mode-line variable is true.  Otherwise
just skip past it and insert an extra line in preparation for the
test headers."
  (if (and (looking-at "-*-") (not testr-keep-mode-line))
      (let
          ((bol (save-excursion (beginning-of-line)(point)))
           (eol (save-excursion (end-of-line)(point))))
        (delete-region bol (+ eol 1)))
    (forward-line)
    (insert "\n")) )

(defun testr-insert-headers (buffer-name &rest headers)
  "Insert the given strings into the test buffer."
  (save-current-buffer
    (set-buffer (get-buffer buffer-name))
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (testr-deal-with-mode-line)
    (apply 'insert headers)
    (setq buffer-read-only t)
    (goto-char (point-max))
    (if testr-single-window (delete-other-windows)) ))

;;; ------------------------------------------------------------------
;;; -- Test Run Commands ---------------------------------------------
(defun testr-run-test-rake ()
  "Run the default rake command as a test."
  (interactive)
  (testr-prep-test-buffer)
  (testr-start-process testr-rake-command)
  (testr-insert-headers
   testr-buffer-name
   "= Test Rake\n"
   "== Target: default\n\n") )

(defun testr-run-test-units ()
  "Run the test:units rake command as a test."
  (interactive)
  (testr-prep-test-buffer)
  (testr-start-process testr-rake-command "test:units")
  (testr-insert-headers
   testr-buffer-name
   "= Test Rake\n"
   "== Target: test:units\n\n") )

(defun testr-run-test-functionals ()
  "Run the test:functionals rake command as a test."
  (interactive)
  (testr-prep-test-buffer)
  (testr-start-process testr-rake-command "test:functionals")
  (testr-insert-headers
   testr-buffer-name
   "= Test Rake\n"
   "== Target: test:functionals\n\n") )

(defun testr-run-test-integration ()
  "Run the test:integration rake command as a test."
  (interactive)
  (testr-prep-test-buffer)
  (testr-start-process testr-rake-command "test:integration")
  (testr-insert-headers
   testr-buffer-name
   "= Test Rake\n"
   "== Target: test:integration\n\n") )

(defun testr-run-spec-method (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test")
  (testr-take-down-test-buffer)
  (let* ((file-name (buffer-file-name))
         (default-directory (testr-find-project-top file-name))
         (test-buffer (current-buffer)) )
    (if (not (testr-spec-file-name-p file-name))
        (progn
          (testr-toggle-buffer)
          (setq file-name (buffer-file-name)) ))
    (save-buffer)
    (setq testr-last-test-buffer (buffer-name))
    (let ((line-number (int-to-string (line-number-at-pos))))
      (cond ((null default-directory) (message "Cannot find project top"))
            ((null arg)
             (testr-prep-test-buffer)
             (testr-start-process
              (testr-spec-command test-buffer) testr-spec-options
              (concat file-name ":" line-number))
             (testr-insert-headers
              testr-buffer-name
              "= Individual Spec ...\n"
              "== In:    " default-directory "\n"
              "== File:  " (file-name-nondirectory file-name) "\n"
              "== Line:  " line-number "\n\n"))
            (t (testr-prep-test-buffer)
               (testr-start-debugging
                testr-rdebug-command (testr-option-string)
                file-name "--" (concat "--name " method-name))) ))))

(defun testr-run-spec-file (arg)
  "Run the current file as a spec.
If this file name does not include the string 'spec' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (testr-take-down-test-buffer)
  (if (string-equal testr-buffer-name (buffer-name))
      (kill-buffer testr-buffer-name))
  (let* ((file-name (testr-target-file-name (buffer-file-name)))
         (default-directory (testr-find-project-top file-name))
         (test-buffer (current-buffer)) )
    (cond ((null default-directory) (message "Cannot find project top"))
          (t
           (save-buffer)
           (setq testr-last-test-buffer (buffer-name))
           (testr-prep-test-buffer)
           (cond ((null arg)
                  (testr-start-process
                   (testr-spec-command test-buffer) testr-spec-options file-name)
                  (testr-insert-headers
                   testr-buffer-name
                   "= Spec File ...\n"
                   "== In:   " default-directory "\n"
                   "== File: " (file-name-nondirectory file-name) "\n\n") )
                 (t (testr-start-debugging
                     testr-rdebug-command (testr-option-string) file-name)) )))))


(defun testr-run-test-method (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test")
  (testr-take-down-test-buffer)
  (let* ((file-name (buffer-file-name))
         (default-directory (testr-find-project-top file-name)) )
    (if (not (testr-unit-file-name-p file-name))
        (progn
          (testr-toggle-buffer)
          (setq file-name (buffer-file-name)) ))
    (save-buffer)
    (setq testr-last-test-buffer (buffer-name))
    (let ((invoke-given (save-excursion
                          (re-search-backward testr-all-pattern)
                          (looking-at " *"))))
        (let ((method-name (testr-find-test-method-name)))
          (testr-invoking-test-by-name arg
                                            file-name
                                            (testr-fixup-method-name method-name))))))

(defun testr-fixup-method-name (name)
  "Add .* to method names with spaces"
  (while (string-match " " name)
    (setq name (replace-match ".*" nil nil name)))
  name)

(defun testr-run-test-command ()
  testr-ruby-program)

(defun testr-invoking-test-by-name (arg file-name method-name)
  (cond ((null default-directory) (message "Cannot find project top"))
        ((null arg)
         (testr-prep-test-buffer)
         (testr-start-process
          (testr-run-test-command) (testr-option-string)
          file-name (concat "--name \"/" method-name "/\""))
         (testr-insert-headers
          testr-buffer-name
          "= Test Method ...\n"
          "== In:     " default-directory "\n"
          "== File:   " (file-name-nondirectory file-name) "\n"
          "== Method: " method-name "\n\n"))
        (t (testr-prep-test-buffer)
           (testr-start-debugging
            testr-rdebug-command (testr-option-string)
            file-name "--" (concat "--name " method-name)))))

(defun testr-run-test-file (arg)
  "Run the current file as a test.
If this file name does not include the string 'test' and there is
a toggle mapping for this file, then run the test on the toggled
test file."
  (interactive "P")
  (bookmark-set "test-file")
  (testr-take-down-test-buffer)
  (if (string-equal testr-buffer-name (buffer-name))
      (kill-buffer testr-buffer-name))
  (let* ((file-name (testr-target-file-name (buffer-file-name)))
         (default-directory (testr-find-project-top file-name)) )
    (cond ((null default-directory) (message "Cannot find project top"))
          (t
           (save-buffer)
           (setq testr-last-test-buffer (buffer-name))
           (testr-prep-test-buffer)
           (cond ((null arg)
                  (testr-start-process
                   (testr-run-test-command) (testr-option-string) file-name)
                  (testr-insert-headers
                   testr-buffer-name
                   "= Test File ...\n"
                   "== In:   " default-directory "\n"
                   "== File: " (file-name-nondirectory file-name) "\n\n") )
                 (t (testr-start-debugging
                     testr-rdebug-command (testr-option-string) file-name)) )))))

(defun testr-run-test-or-spec-method (args)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (cond ((testr-unit-file-name-p file-name) (testr-run-test-method args))
          ((testr-spec-file-name-p file-name) (testr-run-spec-method args))
          (t (error "not a test nor a spec")) )))

(defun testr-run-last-test-or-spec-method (args)
  (interactive "P")
  (bookmark-jump "test")
  (testr-run-test-or-spec-method args))

(defun testr-run-test-or-spec-file (args)
  (interactive "P")
  (let ((file-name (buffer-file-name)))
    (cond ((testr-unit-file-name-p file-name) (testr-run-test-file args))
          ((testr-spec-file-name-p file-name) (testr-run-spec-file args))
          (t (error "not a test nor a spec")) )))

(defun testr-run-last-test-or-spec-file (args)
  (interactive "P")
  (bookmark-jump "test-file")
  (testr-run-test-or-spec-file args))

(defun testr-mark-for-testing (n)
  (interactive "P")
  (cond ((null n) (bookmark-jump "test"))
        (t (bookmark-set "test")) ))

;;; ------------------------------------------------------------------
;;; -- Toggle Enhancements -------------------------------------------
(defvar testr-toggle-style nil
  "Buffer local variable describing the buffer's toggle style.")
(make-variable-buffer-local 'testr-toggle-style)

(defun testr-load-project-toggle-style ()
  "Set the buffer's toggle style from the project defaults."
  (let* ((togglerc (testr-find-in-parent-dir (buffer-file-name) ".togglerc")))
    (if (file-readable-p togglerc)
        (load-file togglerc))
    (if (null testr-toggle-style)
        (setq testr-toggle-style toggle-mapping-style))  ))

(defun testr-select-buffer-toggle-style ()
  "Set the buffer's toggle style.
If no style is currently selected, load the style from the
project .togglerc file."
  (if (null testr-toggle-style)
      (testr-load-project-toggle-style) )
  (setq toggle-mappings (toggle-style testr-toggle-style)) )

(defun testr-toggle-buffer ()
  "Enhanced version of the Ryan Davis's toggle-buffer function
Check for a .togglerc file at the top level of the project
directory.  If found, the file will be loaded before toggling,
allowing per-project toggle customizations."
  (interactive)
  (testr-select-buffer-toggle-style)
  (toggle-buffer) )

(defun testr-toggle-clear-buffer-styles ()
  "Clear all the buffer toggle style settings."
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (if (local-variable-p 'testr-toggle-style (car buffers))
          (save-current-buffer
            (set-buffer (car buffers))
            (setq testr-toggle-style nil) ))
      (setq buffers (cdr buffers)) )
    (message "All buffer toggle styles are reset") ))

(defun testr-add-or-replace (name pair)
  (let* ((key (car pair))
         (new-value (cdr pair))
         (alist (eval name))
         (old-pair (assoc key alist)))
    (cond ((null old-pair) (add-to-list name pair))
          ((equal (cdr old-pair) new-value) ())
          (t (set name (cons pair (assq-delete-all key alist)))) ))
  (eval name) )

(defun testr-buffer-p ()
  (string-match "_test\." (buffer-name)))

(defun testr-spec-buffer-p ()
  (string-match "_spec\." (buffer-name)))

(defun testr-or-spec-buffer-p ()
  (or (testr-buffer-p)
      (testr-spec-buffer-p)))

(defun testr-code-test-split ()
  "Horizontally split between the code and test"
  (interactive)
  (delete-other-windows)
  (if (testr-or-spec-buffer-p)
      (testr-toggle-buffer))
  (split-window-horizontally)
  (other-window 1)
  (testr-toggle-buffer)
  (other-window 1))

(defun testr-split-or-toggle (n)
  "Toggle code/test buffer, or split windows with code and test (if prefix)"
  (interactive "P")
  (cond ((null n) (testr-toggle-buffer))
        (t (testr-code-test-split))))

(defun buffer-toggle-style (style-name)
  "Set the testing toggle style for this buffer.
Normally called in the .togglerc file at the project level."
  (setq testr-toggle-style style-name) )

(defun buffer-toggle-mapping (mapping)
  "Define a project specific mapping.
Note: Make sure the mapping name is unique and doesn't class with
mappings from other projects."
  (testr-add-or-replace 'toggle-mapping-styles mapping))

(defun testr-toggle-warnings ()
  "Toggle the 'use warnings' flag for when testing"
  (interactive)
  (setq testr-warnings (not testr-warnings))
  (if testr-warnings
      (message "Warnings enabled in tests")
    (message "Warnings disabled in tests") ))

;;; Add the toggle command to the compilation mode, just make it
;;; delete the test buffer.
(defun testr-kill-test-buffer ()
  (interactive)
  (kill-buffer testr-buffer-name)
  (if testr-last-test-buffer
      (pop-to-buffer testr-last-test-buffer) ))

(define-key compilation-mode-map "\C-c\C-t" 'testr-kill-test-buffer)

;;; ------------------------------------------------------------------
;;; Window Swapping --------------------------------------------------

(defun testr-minibuffer-window-p (win)
  (and win
       (string-match "Minibuf" (buffer-name (window-buffer win)))))

(defun testr-window-at-origin ()
  (window-at 0 0))

(defun testr-neighbor-window (win)
  "Return a neighboring window to WIN.
Prefer windows on the right to those below.  Might return the minibuffer."
  (let* ((edges (window-edges win))
         (left (caddr edges))
         (bottom (cadddr edges)))
    (cond ((window-at left 0))
          ((window-at 0 bottom)))))

(defun testr-neighbor-edit-window (win)
  "Return a neighboring edit window to WIN.
Never returns the minibuffer."
  (let ((neighbor (testr-neighbor-window win)))
    (if (testr-minibuffer-window-p neighbor)
        nil
      neighbor)))

(defun testr-neighboring-windows (buf1 buf2)
  (let* ((w1 (testr-window-at-origin))
         (w2 (testr-neighbor-edit-window w1)))
    (set-window-buffer w1 buf1)
    (set-window-buffer w2 buf2)
    (select-window w2)))

(defun testr-push-buffer (buffer)
  "Push a new buffer onto the screen.
Current buffer goes to first position."
  (if (= 2 (count-windows))
      (testr-neighboring-windows
       (window-buffer (selected-window))
       buffer)
    (switch-to-buffer buffer)))

;;; ------------------------------------------------------------------
;;; finder -----------------------------------------------------------
(defun testr-dir-contains-p (path file)
  "Does the directory path contain the given file name?"
  (file-readable-p (concat (file-name-as-directory path) file)))

(defun testr-dir-contains-any-p (path files)
  "Does the directory path contain any of the given files?"
  (cond ((null files) nil)
        ((testr-dir-contains-p path (car files)) t)
        (t (testr-dir-contains-any-p path (cdr files)))))

(defconst testr-project-top-level-files
  '("Rakefile" ".gitignore"))

(defun testr-project-top-p (path)
  "Are we at the top of a project?"
  (testr-dir-contains-any-p path testr-project-top-level-files))

(defun testr-root-p (path)
  "Is this the root of the file system?"
  (string-equal "/" path))

(defun testr-parent-dir (path)
  "Return the parent directory of path.  The parent of / is nil."
  (cond ((testr-root-p path) nil)
        (t (file-name-directory (directory-file-name path))) ))

;;; ------------------------------------------------------------------
;;; Public functions -------------------------------------------------
(defun testr-find-project-top (path)
  "Find the top level directory of a project starting with path.
Return original path if not project top not found."
  (setq dir path)
  (while (and dir (not (testr-project-top-p dir)))
    (setq dir (testr-parent-dir dir)))
  (if dir
      (file-name-as-directory (directory-file-name dir))
    path))

(defun testr-find-in-parent-dir (path file)
  "Find an improper parent of path that contains file."
  (while (and path
              (not (testr-dir-contains-p path file)))
    (setq path (testr-parent-dir path)))
  (if path
      (concat (file-name-as-directory path) file)
    nil))

(defun testr-project-env-file (path)
  "Find the project environment shell file, nil if none found."
  (testr-find-in-parent-dir path ".env.rc"))

;; toggle.el --- quickly open corresponding file (eg test vs impl).
;; Copyright (C) 2006-2007 by Ryan Davis
;; Author: Ryan Davis <ryand-ruby@zenspider.com>
(defcustom toggle-mapping-styles
  '((zentest . (("app/controllers/\\1.rb" . "test/controllers/\\1_test.rb")
                ("app/views/\\1.rb"       . "test/views/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (rspec   . (("app/models/\\1.rb"      . "spec/models.\\1_spec.rb")
                ("app/controllers/\\1.rb" . "spec/controllers/\\1_spec.rb")
                ("app/views/\\1.rb"       . "spec/views/\\1_spec.rb")
                ("app/helpers/\\1.rb"     . "spec/helpers/\\1_spec.rb")))
    (rails   . (("app/controllers/\\1.rb" . "test/functional/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (ruby    . (("lib/\\1.rb"             . "test/test_\\1.rb")
                ("\\1.rb"                 . "test_\\1.rb"))))
  "A list of (name . toggle-mapping) rules used by toggle-filename."
  :group 'toggle
  :type '(repeat (cons string string)))

(defcustom toggle-mapping-style
  'rails
  "The default toggle mapping style to load when initialized."
  :group 'toggle
  :type '(symbol))

(defun toggle-style (name)
  (interactive (list (completing-read "Style: "
                                      (mapcar
                                       #'symbol-name
                                       (mapcar #'car toggle-mapping-styles))
                                      nil t "")))
  (let* ((style (if (stringp name) (intern name) name))
         (pairs (cdr (assoc style toggle-mapping-styles))))
    (if pairs
        (let ((mappings
               (mapcar (lambda (pair)
                         (cons
                          (replace-regexp-in-string
                           "\\\\1" "\\\\(.*\\\\)"
                           (replace-regexp-in-string ; special case for "\\1.ext"
                            "^\\\\1" "\\\\([^/]*\\\\)" (car pair)))
                          (cdr pair)))
                       (mapcan 'list
                               pairs
                               (mapcar (lambda (pair)
                                         (cons (cdr pair) (car pair)))
                                       pairs)))))
          (if (interactive-p)
              (setq toggle-mappings mappings)
            mappings))
      nil)))

(defvar toggle-mappings (toggle-style toggle-mapping-style)
  "*The current file mappings for `toggle-filename' to use.")

(defun toggle-filename (path rules)
  "Transform a matching subpath in PATH as given by RULES.
Each element in RULES is a pair (RE . TRANS). If the regular
expression RE matches PATH, then replace-match is invoked with
TRANS. After the first successful match, this returns. If no rule
matches, it returns nil"
  (cond ((null rules) nil)
        ((string-match (caar rules) path)
         (replace-match (cdar rules) nil nil path))
        (t (toggle-filename path (rest rules)))))

(defun toggle-buffer ()
  "Opens a related file to the current buffer using matching rules.
Matches the current buffer against rules in toggle-mappings. If a
match is found, switches to that buffer."
  (interactive)
  (let ((new-name (toggle-filename (buffer-file-name) toggle-mappings)))
    (if new-name
        (find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

;;; ----------------------------------------------------------------------------
;;; - TestR minor mode
(defvar testr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-t")  'testr-toggle-buffer)
    (define-key map (kbd "C-c tf")  'testr-run-test-file)
    (define-key map (kbd "C-c tm")  'testr-run-test-method)
    (define-key map (kbd "C-c sf")  'testr-run-spec-file)
    (define-key map (kbd "C-c sm")  'testr-run-spec-method)
    (define-key map (kbd "C-c rr")  'testr-run-last-test-or-spec-file)
    (define-key map (kbd "C-c rm")  'testr-run-last-test-or-spec-method)
    map))

;;;###autoload
(define-minor-mode testr-mode "TestR Minor Mode"
  :lighter " TestR" :keymap testr-mode-map
  (use-local-map testr-mode-map))

(provide 'testr)
