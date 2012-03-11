;;; flymake-dino-phpcs.el --- Flymake for PHP via PHP-CodeSniffer
;;
;; Inspired by the similar function flymake-phpcs.el, by Sam Graham, I think.
;; But this is different. Simpler and more tuned to what I need to do.
;;
;; This software is in the public domain.
;;
;; To use a custom coding standard, you can create a subdir in the phpcs/Standards
;; directory, and place within that subdir a file named ruleset.xml, with contents
;; like this:
;;
;; <?xml version="1.0"?>
;; <ruleset name="Custom Standard">
;;   <description>My custom coding standard</description>
;;
;;   <rule ref="PEAR">
;;     <exclude name="PEAR.Commenting.ClassComment"/>
;;     <exclude name="PEAR.Commenting.FileComment"/>
;;     <exclude name="PEAR.Commenting.FunctionComment"/>
;;     <exclude name="PEAR.Commenting.InlineComment"/>
;;     <exclude name="PEAR.Classes.ClassDeclaration"/>
;;     <exclude name="PEAR.Functions.FunctionDeclaration.BraceOnSameLine"/>
;;     <exclude name="Generic.Files.LineEndings"/>
;;     <exclude name="Generic.Files.LineLength.TooLong"/>
;;     <exclude name="PEAR.ControlStructures.ControlSignature"/>
;;   </rule>
;;
;;
;;   <rule ref="PEAR.WhiteSpace.ScopeIndent">
;;     <properties>
;;       <property name="indent" value="2"/>
;;     </properties>
;;   </rule>
;;
;; </ruleset>
;;
;;
;;; Commentary:
;;
;; flymake-dino-phpcs.el adds support for running PHP_CodeSniffer
;; (http://pear.php.net/package/PHP_CodeSniffer/) to perform static
;; analysis of a PHP file.
;;
;;; Usage:
;; (require 'flymake-dino-phpcs)

(eval-when-compile (require 'flymake))

(defvar fly/phpcs-phpcs-dir "c:\\dev\\phpcs"
  "Location of the PHP CodeSniffer installation.")

(defvar fly/phpcs-phpinc "c:\\dev\\phplibs"
  "Location of any PHP include dir.")

(defvar fly/phpcs-phpexe "c:\\php\\php.exe"
  "Location of the PHP EXE or program.")

(defvar fly/phpcs-standard "PEAR" ;; Zend, PEAR, PHPCS, etc
  "The coding standard CodeSniffer should apply.")


;; "%phpexe%" -d auto_append_file="" -d auto_prepend_file="" -d include_path="%phpinc%" "%phpcs%\scripts\phpcs" %*


(defun fly/phpcs-get-cmdline (source base-dir)
  "Gets the cmd line for running a flymake session in a PHP buffer.
This gets called by flymake itself. The output is a list of two elements:
the command to run, and a list of arguments.  The resulting command is like:

  php.exe -d auto_append_file="" -d auto_prepend_file="" phpcs\scripts\phpcs --report=emacs file.php

"
  ;;(dino-log "PHP" "flymake cmdline for %s" source)
        (list fly/phpcs-phpexe
              (list
               "-d" "auto_append_file=''"
               "-d" "auto_prepend_file=''"
               (concat fly/phpcs-phpcs-dir "\\scripts\\phpcs")
               (concat "--standard="  fly/phpcs-standard)
               "--report=emacs"
               "-s" ;; show the fullname of the rule being violated
               (expand-file-name source))))


(defun fly/phpcs-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

This won't always work; it will fail if the source module
refers to relative paths.
"
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                prefix "-"
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "-"))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (make-temp-file name nil ext))
         )
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


(defun fly/phpcs-cleanup ()
     (flymake-simple-cleanup))


(defun fly/phpcs-init ()
  "initialize flymake for PHP using the PHP CodeSniffer tool."
  (let ((create-temp-f 'fly/phpcs-create-temp-intemp)
        (use-relative-base-dir t)
        (use-relative-source t)
        (get-cmdline-f 'fly/phpcs-get-cmdline)
        args
        temp-source-file-name)
    (setq temp-source-file-name (flymake-init-create-temp-buffer-copy create-temp-f)
          args (flymake-get-syntax-check-program-args
                temp-source-file-name "."
                use-relative-base-dir use-relative-source
                get-cmdline-f))
    args))


(defvar fly/phpcs-error-pattern
    "^[ \t]*\\([\._A-Za-z0-9][^(\n]+\\.php\\):\\([0-9]+\\):\\([0-9]+\\)[ \t]+\\(\\(error\\|warning\\) - \\(.+\\)\\)$"
  "The regex pattern for PHP CodeSniffer error or warning messages. For use as
an entry in `flymake-err-line-patterns'. ")


(defun fly/phpcs-install ()
  "install flymake stuff for PHP CodeSniffer files."
  (add-to-list
   'flymake-err-line-patterns
   (list fly/phpcs-error-pattern 1 2 3 4))

  (let* ((key "\\.php\\'")
         (phpentry (assoc key flymake-allowed-file-name-masks)))
    (if phpentry
        (setcdr phpentry '(fly/phpcs-init fly/phpcs-cleanup))
      (add-to-list
       'flymake-allowed-file-name-masks
       (list key 'fly/phpcs-init 'fly/phpcs-cleanup)))))

(eval-after-load "flymake"
  '(progn
     (fly/phpcs-install)))


(provide 'flymake-dino-phpcs)
;;; flymake-dino-phpcs.el ends here
