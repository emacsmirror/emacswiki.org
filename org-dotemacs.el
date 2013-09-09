;;; org-dotemacs.el --- Store your emacs config as an org file, and choose which bits to load.

;; Filename: org-dotemacs.el
;; Description: Store your emacs config as an org file, and load code snippets based on tag matches.
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: 2013-04-27 20:19:18
;; Version: 0.2
;; Last-Updated: 2013-04-27 20:19:18
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/org-dotemacs
;; Keywords: local
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((org "7.9.3") (cl-lib "1.0"))
;;
;; Features that might be required by this library:
;;
;; org cl
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1Ph9srQBspJCDS9CnGUyWJPTrU4ydX9Aa3
;;
;; Keeping your emacs config in an org file makes it easier for you to keep your .emacs under control,
;; and avoid DotEmacsBankruptcy.
;; With your config code stored in an org file you can easily edit the structure and keep notes.
;; 
;; This library allows you to load elisp code from an org file on emacs startup.
;; You can also limit the code that is loaded to certain tagged headers using an org tag match,
;; and specify dependencies between code blocks.
;; Using tag matches you can also reuse the same org file for different emacs setups by specifying different
;; tag matches for each setup, or load parts of the file on demand.
;; 
;;; Commands/Usage 
;; 
;; The main command is `org-dotemacs-load-default' which loads your default org-dotemacs file (~/.dotemacs.org)
;; and prompts for a tag match to specify which code blocks to load. 
;; In this way you can load bits of config code when you need them.
;; 
;; You can also put this command in your InitFile (see Installation below) to load the code on startup.
;; To change the default org file use the `org-dotemacs-default-file' option.
;; If you want to load a different org file from your default one, use `org-dotemacs-load-file'.
;; 
;; For faster loading you may prefer to keep your config code in a separate elisp file, and just update this file now and again
;; by exporting the code from the org file.
;; Use the `org-dotemacs-load-file' command for this and specify a target file when prompted.
;; 
;;; Structure of the org file 
;; 
;; The elisp code should be contained in emacs-lisp code blocks, e.g:
;; 
;; #+BEGIN_SRC emacs-lisp
;; (setq line-number-mode t)
;; (setq column-number-mode t)
;; (setq frame-title-format "%b")
;; (set-background-color "Black")
;; (set-foreground-color "White")
;; (set-cursor-color "White")
;; #+END_SRC
;; 
;; Ideally you should have each code block under a separate org subtree, then you can use properties to
;; name the blocks and define dependencies, and tags and todo states to specify which blocks
;; should be loaded (see below).
;; 
;; I prefer to keep all my code block subtrees under a single header, and use other headers for keeping notes,
;; defining buffer-wide properties, etc. This way I can get a nice column view of the code blocks
;; (see the columns view section below).
;; 
;;;  Block dependencies 
;; 
;; You can enforce dependencies between code blocks by defining NAME & DEPENDS properties for the subtrees containing the
;; blocks (preferred). The NAME property should contain the name of the block, and the DEPENDS property should contain a space
;; separated list of block names that this block depends on.
;; These properties will be applied to all code blocks in the subtree (see "Properties and Columns" in the org manual for
;; more details).
;; 
;; The NAME property can be overridden on a per block basis by adding a :name header arg to a block, and dependencies can be
;; augmented by adding a :depends header arg (see "Header arguments" in the org manual).
;; However it is recommended to keep a separate subtree for each code block and use properties for defining headers and names
;; since then you can get a column view of the dependencies (see below).
;; 
;; A block will not be loaded until all of its dependencies have been loaded.
;; 
;;;  Tags and TODO states 
;; 
;; You can tag your subtrees and use tag matches to specify which blocks to evaluate in calls to `org-dotemacs-load-file'
;; and `org-dotemacs-load-default'. See "Matching tags and properties" in the org manual for more information on tag matches.
;; 
;; Also, by default any blocks in a subtree marked with a todo state of BROKEN will not be evaluated.
;; You can specify which TODO states to include/exclude for evaluation by customizing the `org-dotemacs-include-todo' and
;; `org-dotemacs-exclude-todo' options.
;; 
;; To add the BROKEN state to the list of todo states for the file you need to add buffer-wide todo states by adding a line
;; like this somewhere in your org file (see "Per file keywords" in the org manual).
;; 
;; #+TODO: BROKEN CHECK TODO
;; 
;;;  Columns View 
;; 
;; If you use properties for defining names and dependencies then you can get a nice column view of your code subtrees
;; with the following columns view specification:
;; 
;; #+COLUMNS: %35ITEM %15NAME %35DEPENDS %15TAGS %TODO
;; 
;; This can be placed anywhere in your dotemacs org file.
;; Then if you press C-c C-x C-c on the toplevel header for your code blocks you'll get a column view that allows you
;; to easily change the names, dependencies, tags and todo states.
;; 
;;;  Error handling 
;; 
;; Error handling can be controlled by customizing `org-dotemacs-error-handling' or by setting the error-handling
;; command line option when starting emacs.
;; By default code blocks with unmet dependencies or errors are skipped over as soon as an error is encountered,
;; but you can also specify that org-dotemacs should halt or try to reload the blocks.
;; In the latter case each time a new block is successfully loaded, any unsuccessful blocks will be retried.
;; 
;;;  Command line options 
;; 
;; org-dotemacs.el will look for two command line options when loaded: error-handling (for setting the value of
;; `org-dotemacs-error-handling') and tag-match (for specifying which headers to load).
;; For example if you enter the following at the command line:
;; 
;;        emacs --error-handling retry --tag-match "settings-mouse"
;; 
;; Then only code blocks tagged "settings" but not "mouse" will be loaded, and org-dotemacs will try to reload any
;; blocks that have errors.
;; 
;;;  Installation 
;; 
;; Put org-dotemacs.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;; 
;; Then make sure you have an ~/.dotemacs.org file and add the following lines to
;; the end of your .emacs file:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-default)
;; 
;; or if you want to just load code blocks matching a tag match:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-default "<TAG-MATCH>")
;; 
;; To load a different org file either customize `org-dotemacs-default-file' or use the
;; `org-dotemacs-load-file' function, e.g:
;; 
;; (load-file "~/.emacs.d/org-dotemacs.el")
;; (org-dotemacs-load-file "~/.emacs.d/my_emacs_config.org" "<TAG-MATCH>")
;; 


;;; Customize:
;;
;; `org-dotemacs-default-file' : The default org file containing the code blocks to load when `org-dotemacs-load-file' is called.
;; `org-dotemacs-error-handling' : Indicates how errors should be handled by `org-dotemacs-load-blocks'.
;; `org-dotemacs-include-todo' : A regular expression matching TODO states to be included.
;; `org-dotemacs-exclude-todo' : A regular expression matching TODO states to be excluded.
;;
;; All of the above can customized by:
;;      M-x customize-group RET org-dotemacs RET
;;

;;; Change log:
;; 4-May-2013      
;;    Last-Updated: 2013-04-27 20:19:18 (Joe Bloggs)
;;    ;;    
;;	
;; 2013/04/27
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; 
;;

;;; TODO
;;
;; Option to show prominent warning message if some blocks didn't load (e.g. in large font in dedicated buffer after startup)
;; 

;;; Require
(eval-when-compile (require 'cl))
(require 'org)

;;; Code:



(defgroup org-dotemacs nil
  "Store your emacs config as an org file, and choose which bits to load."
  :group 'org)

(defcustom org-dotemacs-default-file "~/.dotemacs.org"
  "The default org file containing the code blocks to load when `org-dotemacs-load-file' is called."
  :group 'org-dotemacs
  :type '(file :must-match t))

(defcustom org-dotemacs-error-handling 'skip
  "Indicates how errors should be handled by `org-dotemacs-load-blocks'.
If eq to 'skip then errors will be skipped over (default).
If eq to 'retry then `org-dotemacs-load-blocks' will attempt to reload any blocks containing errors,
after each successfully loaded block.
In all other cases errors will cause evaluation to halt as normal.
In all cases errors will be reported in the *Messages* buffer as normal.

This variable can be set from the command line using the dotemacs-error-handling argument."
  :group 'org-dotemacs
  :type 'symbol)

(defcustom org-dotemacs-include-todo nil
  "A regular expression matching TODO states to be included.
If non-nil then only headers with TODO states matching this regexp will be checked for code blocks.
See also `org-dotemacs-exclude-todo'."
  :group 'org-dotemacs
  :type 'regexp)

(defcustom org-dotemacs-exclude-todo "BROKEN"
  "A regular expression matching TODO states to be excluded.
If non-nil then headers with TODO states matching this regexp will not be checked for code blocks.
See also `org-dotemacs-include-todo'."
  :group 'org-dotemacs
  :type 'regexp)

(defvar org-dotemacs-tag-match nil
  "An org tag match string indicating which code blocks to load with `org-dotemacs-load-file'.
If non-nil the value of this variable will override the match argument to `org-dotemacs-load-file'.")

;; This function was obtained from string-fns.el by Noah Friedman <friedman@splode.com>
;;;###autoload
(defun string-split (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

;;;###autoload
(defun* org-dotemacs-extract-subtrees (match &optional
                                             (exclude-todo-state org-dotemacs-exclude-todo)
                                             (include-todo-state org-dotemacs-include-todo))
  "Extract subtrees in current org-mode buffer that match tag MATCH.
MATCH should be a tag match as detailed in the org manual.
If EXCLUDE-TODO-STATE is non-nil then subtrees with todo states matching this regexp will be
excluding, and if INCLUDE-TODO-STATE is non-nil then only subtrees with todo states matching
this regexp will be included.
The copied subtrees will be placed in a new buffer which is returned by this function.
If called interactively MATCH is prompted from the user, and the new buffer containing
the copied subtrees will be visited."
  (interactive '(nil))
  (let ((buf (generate-new-buffer (buffer-name)))
        todo-only copied-areas)
    (org-scan-tags (lambda nil
		     (let ((todo-state (org-get-todo-state)))
		       (unless (or (and exclude-todo-state
					todo-state
					(string-match exclude-todo-state
						      todo-state))
				   (and include-todo-state
					todo-state
					(not (string-match include-todo-state
							   todo-state)))
                                   (loop for pair in copied-areas
                                         if (and (>= (point) (car pair))
                                                 (< (point) (cdr pair)))
                                         return t))
			 (let ((start (point)) end)
			   (org-copy-subtree)
			   (setq end (+ start (length (current-kill 0 t))))
			   (push (cons start end) copied-areas))
			 (with-current-buffer buf 
			   (goto-char (point-max))
			   (yank)))))
                   (cdr (org-make-tags-matcher match)) todo-only)
    (with-current-buffer buf (org-mode))
    (if (called-interactively-p 'any)
        (switch-to-buffer buf)
      buf)))

;;;###autoload
(defun* org-dotemacs-load-blocks (&optional target-file
                                            (error-handling org-dotemacs-error-handling))
  "Load the emacs-lisp code blocks in the current org-mode file.
Save the blocks to TARGET-FILE if it is non-nil.
See the definition of `org-dotemacs-error-handling' for an explanation of the ERROR-HANDLING
argument which uses `org-dotemacs-error-handling' for its default value."
  (run-hooks 'org-babel-pre-tangle-hook)
  (save-restriction
    (save-excursion
      (let* ((block-counter 1)
             (org-babel-default-header-args
              (org-babel-merge-params org-babel-default-header-args
                                      (list (cons :tangle (or target-file "yes")))))
             (specs (cdar (org-babel-tangle-collect-blocks 'emacs-lisp)))
             (get-spec (lambda (spec name) (cdr (assoc name (nth 4 spec)))))
             (try-eval (lambda (spec blockname blockdeps)
                         (let (fail)
                           (if (cl-subsetp blockdeps evaluated-blocks :test 'equal)
                               (with-temp-buffer
                                 (ignore-errors (emacs-lisp-mode))
                                 (org-babel-spec-to-string spec)
                                 ;; evaluate the code
                                 (message "org-dotemacs: Evaluating %s code block" blockname)
                                 (setq fail nil)
                                 (if (member error-handling '(skip retry))
                                     (condition-case err
                                         (eval-buffer)
                                       (error
                                        (setq fail (error-message-string err))
                                        (message "org-dotemacs: Error in %s code block: %s"
                                                 blockname fail)))
                                   (eval-buffer))
                                 (unless fail
                                   (setq evaluated-blocks (append evaluated-blocks (list blockname)))
                                   ;; We avoid append-to-file as it does not work with tramp.
                                   (when target-file
                                     ;; save source-block to file
                                     (let ((content (buffer-string)))
                                       (with-temp-buffer
                                         (if (file-exists-p target-file)
                                             (insert-file-contents target-file))
                                         (goto-char (point-max))
                                         (insert content)
                                         (write-region nil nil target-file)))))
                                 fail) 'unmet))))
             evaluated-blocks unevaluated-blocks unmet-dependencies)
        ;; delete any old versions of file
        (if (and target-file (file-exists-p target-file))
            (delete-file target-file))
        (mapc
         (lambda (spec)
           (let* ((linenum (car spec))
                  (start (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- linenum))
                           (point)))
                  (subtreedeps (org-entry-get start "DEPENDS" t))
                  (subtreename (org-entry-get start "NAME" t))
                  (blockname (or (funcall get-spec spec :name)
                                 subtreename
                                 (concat "block_" (number-to-string block-counter))))
                  (blockdeps (remove "" (string-split (concat (funcall get-spec spec :depends)
                                                              " "
                                                              subtreedeps)
                                                      "[ ,\f\t\n\r\v]+"))))
             (let ((fail (funcall try-eval spec blockname blockdeps)))
               (cond ((stringp fail)
                      (setq unevaluated-blocks (append unevaluated-blocks (list (list spec blockname blockdeps fail)))))
                     ((eq fail 'unmet)
                      (setq unmet-dependencies (append unmet-dependencies (list (list spec blockname blockdeps)))))
                     (t (if (eq error-handling 'retry)
                            (while (and (not fail) (or unevaluated-blocks unmet-dependencies))
                              (loop for blk in (append unevaluated-blocks unmet-dependencies)
                                    do (setq fail (funcall try-eval (car blk) (second blk) (third blk)))
                                    unless fail do (setq unevaluated-blocks (remove blk unevaluated-blocks)
                                                         unmet-dependencies (remove blk unmet-dependencies))
                                    and return t))))))
             (setq block-counter (+ 1 block-counter))))
         specs)
        (if (and (not unevaluated-blocks) (not unmet-dependencies))
            (message "\norg-dotemacs: All blocks evaluated successfully!")
          (if evaluated-blocks
              (message "\norg-dotemacs: Successfully evaluated the following %d code blocks: %s"
                       (length evaluated-blocks)
                       (mapconcat 'identity evaluated-blocks " ")))
          (if unevaluated-blocks
              (message "\norg-dotemacs: The following %d code block%s errors: \n %s\n"
                       (length unevaluated-blocks)
                       (if (= 1 (length unevaluated-blocks)) " has" "s have")
                       (mapconcat (lambda (blk) (concat "   " (second blk)
                                                        " block error: " (fourth blk) "\n"))
                                  unevaluated-blocks " ")))
          (if unmet-dependencies
              (message "\norg-dotemacs: The following %d code block%s unmet dependencies: \n %s\n"
                       (length unmet-dependencies)
                       (if (= 1 (length unmet-dependencies)) " has" "s have")
                       (mapconcat (lambda (blk) (concat "   " (second blk)
                                                        " block depends on blocks: " (third blk)))
                                  unmet-dependencies " ")))))
      ;; run `org-babel-post-tangle-hook' in tangled file
      (when (and org-babel-post-tangle-hook
                 target-file
                 (file-exists-p target-file))
        (org-babel-with-temp-filebuffer target-file
          (run-hooks 'org-babel-post-tangle-hook))))))

;;;###autoload
(defun* org-dotemacs-load-file (&optional match
                                          (file org-dotemacs-default-file)
                                          target-file
                                          (error-handling org-dotemacs-error-handling))
  "Load the elisp code from code blocks in org FILE under headers matching tag MATCH.
If TARGET-FILE is supplied it should be a filename to save the elisp code to, but it should
not be any of the default config files .emacs, .emacs.el, .emacs.elc or init.el
 (the function will halt with an error in those cases).
The optional argument ERROR-HANDLING determines how errors are handled and takes default value
`org-dotemacs-error-handling' (which see)."
  (interactive (list nil
                     (read-file-name (format "File to load (default %s): " org-dotemacs-default-file)
                                     (file-name-directory org-dotemacs-default-file)
                                     org-dotemacs-default-file
                                     t nil
                                     (lambda (file)
                                       (string-match "\\.org$" file)))
                     (if (y-or-n-p "Save elisp code to separate file?")
                         (read-file-name "Save to file: " user-emacs-directory))))
  (if (and target-file (string-match "\\(?:\\.emacs\\(?:\\.elc?\\)?\\|init\\.elc?\\)$" target-file))
      (error "org-dotemacs: Refuse to overwrite %s" target-file))
  (require 'ob-core)
  (cl-flet ((age (file) (float-time
                         (time-subtract (current-time)
                                        (nth 5 (or (file-attributes (file-truename file))
                                                   (file-attributes file)))))))
    (if (and target-file
             (file-exists-p target-file)
             (> (age file) (age target-file)))
        (load-file target-file)
      (let ((visited-p (get-file-buffer (expand-file-name file)))
            matchbuf to-be-removed)
        (save-window-excursion
          (find-file file)
          (setq to-be-removed (current-buffer))
          (setq matchbuf (org-dotemacs-extract-subtrees (or org-dotemacs-tag-match match)))
          (with-current-buffer matchbuf
            ;; Write the buffer out first to prevent org-babel-pre-tangle-hook
            ;; prompting for a filename to save it in.
            (write-file (concat temporary-file-directory (buffer-name)))
            (org-dotemacs-load-blocks target-file error-handling))
          (kill-buffer matchbuf))
        (unless visited-p
          (kill-buffer to-be-removed))))))

(defun* org-dotemacs-load-default (&optional (match ""))
  "Load code from `org-dotemacs-default-file' matching tag MATCH.
Unlike `org-dotemacs-load-file' the user is not prompted for the location of any files,
and no code is saved."
  (interactive (list nil))
  (org-dotemacs-load-file match org-dotemacs-default-file nil))

;; Code to handle command line arguments
(let* ((errpos (or (cl-position-if (lambda (x) (equal x "-error-handling")) command-line-args)
		   (cl-position-if (lambda (x) (equal x "--error-handling")) command-line-args)))
       (errval (if errpos (nth (+ 1 errpos) command-line-args)))
       (tagpos (or (cl-position-if (lambda (x) (equal x "-tag-match")) command-line-args)
		   (cl-position-if (lambda (x) (equal x "--tag-match")) command-line-args)))
       (tagval (if tagpos (nth (+ 1 tagpos) command-line-args))))
  (if errval 
      (setq org-dotemacs-error-handling (intern errval)))
  (if tagval (setq org-dotemacs-tag-match tagval)))

(message "org-dotemacs: error-handling = %s" (concat "'" (symbol-name org-dotemacs-error-handling)))
(message "org-dotemacs: tag-match = %s" org-dotemacs-tag-match)

(provide 'org-dotemacs)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "org-dotemacs.el" (buffer-name) (buffer-string) "update")

;;; org-dotemacs.el ends here
