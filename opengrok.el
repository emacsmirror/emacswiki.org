;;; opengrok.el --- Navigate source code with OpenGrok

;; Authors: Mike Veldink <mike.veldink@gmail.com>
;; Version: 1.0.0
;; Emacs: GNU Emacs 24 or later
;; Keywords: programming
;; EmacsWiki: OpenGrok

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows navigating source code with the help of
;; OpenGrok, the "wicked fast source browser" (see
;; http://hub.opensolaris.org/bin/view/Project+opengrok).
;;
;; By calling the Java CLI tool of OpenGrok, this package allows
;; indexing the symbols of a source code directory, searching
;; definitions and references of symbols, searching for files and
;; performing a full text search.
;;
;;
;; INSTALLATION
;;
;; 1. Add the following to your .emacs
;;    (require 'opengrok)
;;
;; 2. Turn on the opengrok minor mode
;;
;;    a. either manually in .emacs
;;
;;       (opengrok-minor-mode t)
;;
;;    b. or by customizing it in the group "opengrok"
;;
;; 3. Download a binary distribution of OpenGrok 0.11.1 from
;;    http://hub.opensolaris.org/bin/view/Project+opengrok and unpack
;;    it in a directory of your choice. Then, customize the variable
;;
;;    opengrok-jar
;;
;;    to specify the location of opengrok.jar.
;;
;; 4. Make sure that you have a Java Runtime Environment installed and
;;    that Emacs can invoke it, e.g. by looking if a
;;
;;    (compile "java -version")
;;
;;    shows reasonable output.
;;
;;
;; USAGE
;;
;; First, you need to index your source code base by creating a new
;; OpenGrok project:
;;
;; 1. Specify the location of the new OpenGrok project.
;;
;;    M-x opengrok-change-dir <source code base>
;;
;;    You use this command before you create a new OpenGrok project or
;;    when you switch projects.
;;
;; 2. Create an OpenGrok project with
;;
;;    M-x opengrok-create-project
;;
;;    This place an OpenGrok index into the new directory <source code
;;    base>/.opengrok. The buffer *opengrok-create-project* shows the
;;    output of the call to OpenGrok.
;;
;; You can now look up a file of your indexed source code base:
;;
;;    M-x opengrok-find-path <file name>                  (C-,)
;;
;; To search for the definition of a symbol, invoke the following
;; command directly, or place point on a reference of the desired
;; symbol and then call:
;;
;;    M-x opengrok-find-defs <symbol name>                (C-.)
;;
;; The result of the search is displayed in grep style in the buffer
;; *opengrok*. If all the hits of the search are found in a single file,
;; opengrok.el jumps to the location of the first hit.
;;
;; In a simillar way, you can look for symbol references:
;;
;;    M-x opengrok-find-refs <symbol name>                (C-,)
;;
;; You perform an OpenGrok full text search like this:
;;
;;    M-x opengrok-find-full <OpenGrok search expression>
;;
;; Like with Emacs Tags, you can jump back to the location where you
;; last invoked any of the above searches with
;;
;;    M-x opengrok-pop-find-mark                          (M-*)
;;
;;
;; Please don't hesitate to get back to me with bug reports, feature
;; requests or any other comment.
;;
;;
;; Enjoy!
;;
;; Mike Veldink, February 2012
;;
;; For God so loved the world that he gave his one and only Son, that
;; whoever believes in him shall not perish but have eternal life.
;;                                             --The Bible, John 3:16

;;; Code:

(require 'compile)
(require 'ring)

(defgroup opengrok nil
  "Global minor mode for source code indexer OpenGrok."
  :group 'programming)

;; idea of marker ring facility borrowed from etags.el
(defcustom opengrok-marker-ring-length 16
  "*Length of marker ring `opengrok-find-marker-ring'."
  :group 'opengrok
  :type 'integer)

(defcustom opengrok-dir nil
  "Source directory containing a directory in which an OpenGrok
project resides."
  :group 'opengrok
  :type 'directory)

(defcustom opengrok-project-dir-name ".opengrok"
  "Name of the directory containing an OpenGrok project."
  :group 'opengrok
  :type 'string)

(defcustom opengrok-jar "c:/home/bin/opengrok-0.11.1/lib/opengrok.jar"
  "The location of file `opengrok.jar' containing the OpenGrok byte code."
  :group 'opengrok
  :type 'directory)

(defcustom opengrok-ctags-command "c:/home/bin/ctags.exe"
  "Directory in which Exuberant Ctags lives."
  :group 'opengrok
  :type 'directory)

(defcustom opengrok-ctags-options "c:/home/etc/ctags.conf"
  "File containing Exuberant Ctags command line options."
  :group 'opengrok
  :type 'directory)

(defcustom opengrok-index-include "-I *.c -I *.cpp -I *.h -I *.py -I *.cs -I *.java"
  "OpenGrok option specifying file types to include in the index."
  :group 'opengrok
  :type 'string)

(defcustom opengrok-index-exclude "-i mweb_memdump.c"
  "OpenGrok option specifying file types to exinclude from the index."
  :group 'opengrok
  :type 'string)

(defvar opengrok-mode-font-lock-keywords
  '(;; Command output lines.
    (": \\(.+\\): \\(?:Permission denied\\|No such \\(?:file or directory\\|device or address\\)\\)$"
     1 grep-error-face)
    ;; remove match from opengrok-regexp-alist before fontifying
    ("^OpenGrok[/a-zA-z]* started.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t))
    ("^OpenGrok[/a-zA-z]* finished \\(?:(\\(matches found\\))\\|with \\(no matches found\\)\\).*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 compilation-info-face nil t)
     (2 compilation-warning-face nil t))
    ("^OpenGrok[/a-zA-z]* \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (0 '(face nil compilation-message nil help-echo nil mouse-face nil) t)
     (1 grep-error-face)
     (2 grep-error-face nil t))
    ("^.+?-[0-9]+-.*\n" (0 grep-context-face)))
  "Additional things to highlight in opengrok output.
This gets tacked on the end of the generated expressions.")

(defvar opengrok-create-project-buffer-name "*opengrok-create-project*"
  "Name of the buffer which contains the output of an OpenGrok
create project invocation.")

(defvar opengrok-project-dir nil
  "Directory containing an OpenGrok project.")

(defvar opengrok-configuration-file nil)

(defvar opengrok-find-marker-ring (make-ring opengrok-marker-ring-length)
  "Ring of markers for locations at which \\[opengrok-find-path], \\[opengrok-find-defs],
or \\[opengrok-find-refs] was invoked.")

;; override compilation-last-buffer
(defvar opengrok-last-buffer nil
  "The most recent OpenGrok buffer.
A opengrok buffer becomes most recent when you select OpenGrok mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`compilation-last-buffer' rather than `opengrok-last-buffer'.")

(defun opengrok-find-path (str)
  (interactive (opengrok-prompt "File"))
  (opengrok-perform-search "-p" str))

(defun opengrok-find-path-sync (str)
  (interactive (opengrok-prompt "File"))
  (opengrok-perform-search "-p" str t))

(defun opengrok-find-defs (str)
  (interactive (opengrok-prompt "Symbol definition"))
  (opengrok-perform-search "-d" str))

(defun opengrok-find-refs (str)
  (interactive (opengrok-prompt "Symbol reference"))
  (opengrok-perform-search "-r" str))

(defun opengrok-find-full (str)
  (interactive (opengrok-prompt "Full text"))
  (opengrok-perform-search "-f" str))

;; These two pieces of advice are necessary to temporarily prevent
;; compilation-start to open a buffer.

(defadvice display-buffer (around prohibit-call-to-display-buffer)
  "Prohibits the call to display-buffer"
  (setq ad-return-value (selected-window)))
;; prohibits calling the advised function by obmitting 'ad-do-it'

(defadvice set-window-start (around prohibit-call-to-set-window-start)
  "Prohibits the call to set-window-start"
  (setq ad-return-value 0))
;; prohibits calling the advised function by obmitting 'ad-do-it'

(defun opengrok-perform-search (search-type search-str)
  (or (file-exists-p opengrok-configuration-file) (error "No OpenGrok project found"))
  (let ((cmd (concat "java -Xms128m -cp " opengrok-jar
                            " org.opensolaris.opengrok.search.Search"
                            " -R " opengrok-configuration-file
                            " " search-type " \"" search-str "\"")))
    (ring-insert opengrok-find-marker-ring (point-marker))
    (ad-activate 'display-buffer)
    (ad-activate 'set-window-start)
    (unwind-protect
        (compilation-start cmd 'opengrok-mode)
      (ad-deactivate 'display-buffer)
      (ad-deactivate 'set-window-start))))

(defun opengrok-create-project ()
  (interactive)
  (or (file-exists-p opengrok-dir) (error "No source code directory specified"))
  ;; Creating the index from certain standard directories does not
  ;; succeed. I could not figure out why this is. to work-around this
  ;; issue, set the standard directory to a directory that is known to
  ;; work.
  (set-process-sentinel
   (start-process-shell-command
    "opengrok-create-project" opengrok-create-project-buffer-name
    (concat "java -Xms512m -cp " opengrok-jar
            " org.opensolaris.opengrok.index.Indexer"
            " -d " opengrok-project-dir
            " -W " opengrok-configuration-file
            " -s " opengrok-dir
            " -c " opengrok-ctags-command
          ;;" -o " opengrok-ctags-options
            " " opengrok-index-include
            " " opengrok-index-exclude)) 'opengrok-create-project-cb))

(defun opengrok-create-project-cb (process event)
  "Sentinel called when the process that created a project signals."
  (with-current-buffer (get-buffer opengrok-create-project-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (previous-line 15)
        (if (re-search-forward "Optimizing the index" nil t)
            (message "Successfully created OpenGrok project.")
          (message "Failed to create OpenGrok project.")))))

;; similar to grep-filter
(defun opengrok-filter ()
  "Handle highlighting hits marked by XHTML tags in the OpenGrok output.
This function is called from `compilation-filter-hook'."
  (save-excursion
    (forward-line 0)
    (let ((end (point)) beg)
      (goto-char compilation-filter-start)
      (forward-line 0)
      (setq beg (point))
      ;; Only operate on whole lines so we don't get caught with part of an
      ;; escape sequence in one chunk and the rest in another.
      (when (< (point) end)
        (setq end (copy-marker end))
        ;;filter out line endings
        (while (re-search-forward "
" end 1)
          (replace-match "" t t))
        (goto-char beg)
        ;; highlight grep matches and delete marking sequences.
        (while (re-search-forward "<b>\\(.*?\\)</b>" end 1)
          (replace-match (propertize (match-string 1)
                                     'face nil 'font-lock-face grep-match-face)
                         t t))
        ;; delete all remaining XHTML tags and escape sequences
        (goto-char beg)
        (while (re-search-forward "</?b>" end 1)
          (replace-match "" t t))
        (goto-char beg)
        ;; adjust OpenGrok path search output
        (while (re-search-forward ": \\[...\\]" nil t)
          (replace-match ":1:"))
        (goto-char beg)
        ;; remove HTML escape sequences and square brackets
        (while (re-search-forward " \\[" nil t)
          (replace-match ":"))
        (goto-char beg)
        (while (re-search-forward "\\]$" nil t)
          (replace-match ""))
        (goto-char beg)
        (while (re-search-forward "&lt;" nil t)
          (replace-match "<"))
        (goto-char beg)
        (while (re-search-forward "&gt;" nil t)
          (replace-match ">"))))))

(defun opengrok-number-of-hits ()
  (with-current-buffer opengrok-last-buffer
    (save-excursion
      (goto-char (point-min))
        (if (re-search-forward "of \\([0-9]+\\) total matching documents collected" nil t)
            (string-to-number (match-string 1))
          0))))

(defun opengrok-exit-message-function (status code msg)
  (let ((hits 0))
    (if (eq status 'exit)
        ;; This relies on the fact that `compilation-start'
        ;; sets buffer-modified to nil before running the command,
        ;; so the buffer is still unmodified if there is no output.
        (cond ((and (zerop code) (buffer-modified-p) (> (setq hits (opengrok-number-of-hits)) 0))
               (if (eq hits 1) (opengrok-goto-first-hit)
                 (display-buffer opengrok-last-buffer))
               '("finished (matches found)\n" . "matched"))
              ((or (not (buffer-modified-p)) (eq hits 0))
               '("finished with no matches found\n" . "no match"))
              (t
               (cons msg code)))
      (cons msg code))))

(defun opengrok-goto-first-hit ()
  (ad-activate 'display-buffer)
  (ad-activate 'set-window-start)
  (unwind-protect
      (next-error 1 t)
    (ad-deactivate 'display-buffer)
    (ad-deactivate 'set-window-start)))

(defun opengrok-pop-find-mark ()
  "Pop back to where \\[opengrok-find-path], \\[opengrok-find-defs],
or \\[opengrok-find-refs] was last invoked."
  (interactive)
  (if (ring-empty-p opengrok-find-marker-ring)
      (error "No previous locations for opengrok-find-* invocation"))
  (let ((marker (ring-remove opengrok-find-marker-ring 0)))
    (switch-to-buffer (or (marker-buffer marker)
                          (error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    (set-marker marker nil nil)))

(defun opengrok-prompt (str)
  (let* ((default (grep-tag-default))
         (prompt (if (string= default "")
                     (format "%s: " str)
                   (format "%s (default %s): " str default))))
    (list (read-string prompt nil nil default))))

(defun opengrok-change-dir (dir)
  (interactive "DDirectory: ")
  (setq opengrok-dir (convert-standard-filename (directory-file-name dir)))
  (setq opengrok-project-dir (convert-standard-filename
                              (concat (file-name-as-directory opengrok-dir)
                                      opengrok-project-dir-name)))
  (setq opengrok-configuration-file (convert-standard-filename
                                     (concat (file-name-as-directory opengrok-project-dir)
                                             "configuration.xml")))
  (setq opengrok-project-drive (substring opengrok-project-dir 0 2)))

(defvar opengrok-key-map (make-sparse-keymap)
  "Keymap used in OpenGrok minor mode.")
(define-key opengrok-key-map "\e*" 'opengrok-pop-find-mark)
(define-key opengrok-key-map (kbd "C-,") 'opengrok-find-path)
(define-key opengrok-key-map "\e." 'opengrok-find-defs)
(define-key opengrok-key-map "\e," 'opengrok-find-refs)

;;;###autoload
(define-compilation-mode opengrok-mode "OpenGrok"
  (setq opengrok-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-disable-input) t)
  (add-hook 'compilation-filter-hook 'opengrok-filter nil t)
  (set (make-local-variable 'compilation-exit-message-function)
       'opengrok-exit-message-function)
  ;; OpenGrok sometimes delivers some ending in its output (^M,
  ;; i.e. Carriage Return). To avoid undesired output in the opengrok
  ;; buffer, let the compilation mode not treats line endings by
  ;; turning of comint-inhibit-carriage-motion and let the
  ;; opengrok-filter filter the line endings out.
  (set (make-local-variable 'comint-inhibit-carriage-motion) t))

;;;###autoload
(define-minor-mode opengrok-minor-mode
  "Navigate source code with OpenGrok."
  :global t
  :init-value nil
  :keymap opengrok-key-map
  :group 'opengrok
  (and opengrok-dir (file-exists-p opengrok-dir) (opengrok-change-dir opengrok-dir)))

(provide 'opengrok)

;;; opengrok.el ends here
