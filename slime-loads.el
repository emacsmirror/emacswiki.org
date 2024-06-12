;;; ==============================
;;; SLIME w32 setup - 
;;; GNU setup should _always_ be maintained seperately.
;;; If not, problems inevitable arise whenever we synch up the HG repos.
;;; ==============================
;;; :SBCL-W32
;;; sb-win32::*core-pathname*
;;; sb-win32::*tty*
;;; sb-win32::directory 
;;; (sb-win32::directory-namestring 
;;; (sb-win32::*ansi-codepage*)
;;; (sb-win32::get-system-info)
;;;  (sb-ext::posix-getenv "SBCL_HOME")
;;; (getenv "SBCL_HOME")
;;; (setenv "SBCL_HOME" "C:/home/sp/bin/sbcl")
;;; ==============================


;;; (unless (equal current-language-environment "UTF-8")
;;;   (set-language-environment "UTF-8"))

;;; ==============================
;; (setq slime-net-coding-system (car (slime-find-coding-system 'utf-8-unix)))
(setq slime-net-coding-system  'utf-8-unix)

;;; :NOTE On w32 when using the above coding system I had to change the value of 
;;;  keyword :coding-system in `swank:create-server' from 
;;;  :coding-system "iso-latin-1-unix" -> :coding-system "utf-8-unix"
;;  to get `slime-repl' working.

;;; ==============================
(add-to-list 'load-path (mon-build-path-for-load-path *mon-site-lisp-root* "slime"))
(add-to-list 'load-path (mon-build-path-for-load-path *mon-site-lisp-root* "slime/contrib"))

;;; ==============================
;; :LOAD-SLIME
(load (concat *mon-site-lisp-root* "/slime/slime"))

;;; ==============================
;; :SET-INFERIOR-LISP

;;; (setq inferior-lisp-program "allegro-express")
;;; (setq inferior-lisp-program "clisp")
;;; (setq inferior-lisp-program "wx86cl")

(let ((ilp (executable-find (executable-find "sbcl"))))
  (setq inferior-lisp-program
        (concat ilp " --core " (file-name-directory ilp) "sbcl.core")))

;;; ==============================
;; :SET-SLIME-LISP-IMPLEMENTATIONS
;;
(let ((ilp-sbcl  (executable-find "sbcl"))
      (ilp-clisp (executable-find "clisp")))
  (setq slime-lisp-implementations
        `((sbcl (,ilp-sbcl "--core " ,(concat (file-name-directory ilp-sbcl) "sbcl.core"))
                :coding-system utf-8-unix)
          (clisp (,ilp-clisp)))))
;;
;;; :TEST-ME slime-lisp-implementations
;;
;; (setq slime-default-lisp

;;; ==============================
;; :CONFIG-LOCAL-SLIME

;;; ==============================
;; (require 'slime)
;; (require 'slime-autoloads)
;; (require 'slime-asdf)
;; (require 'slime-scratch)
;; (require 'slime-repl)
;; (require 'slime-autodoc)
;; (require 'slime-c-p-c)
;; (require 'slime-editing-commands)
;; (require 'slime-fancy-inspector)
;; (require 'slime-fuzzy)
;; (require 'slime-fontifying-fu)
;; (require 'slime-package-fu)
;; (require 'slime-mdot-fu)
;; (require 'slime-references)
;; (require 'slime-xref-browser)
;; (require 'slime-highlight-edits)
;; ;; WHEN sbcl
;; (require 'slime-sbcl-exts)
;; (require 'slime-references)
;; (require 'slime-presentations)
;; (require 'slime-presentation-streams)
;;; ==============================
;;
;;(when (equal (getenv "LISPTYPE") "sbcl")
;;               slime-sbcl-exts ;)
;;               slime-repl
;;               slime-scratch
;;               slime-asdf
;;               slime-tramp
;;               )) 
;;
;;; ==============================
(require 'slime-autoloads)
(slime-setup '(slime-fancy
               slime-repl
               slime-scratch
               slime-sbcl-exts
               slime-tramp
               slime-asdf))
;;
(slime-require :swank-listener-hooks) ; :NOTE This was in the clbuild script

;;; ==============================
;; :SLIME-HOOKS

;; (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
;; (remove-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))

(add-hook 'lisp-mode-hook #'(lambda () (slime-mode t)))

(add-hook 'slime-mode-hook
	  (function (lambda () 
            (set (make-local-variable 'lisp-indent-function)  'common-lisp-indent-function))))

;;; ==============================
;;; :NOTE slime-autodoc is easier on the eyes by simply calling C-c C-d C-a (slime-autodoc-manually)
;; (setq slime-autodoc-mode nil)
;;; (when (featurep 'slime-autodoc) (setq slime-autodoc-delay 3.0))
;;; (setq slime-autodoc-delay 0.2)
;;; (setq asdf-install:*verify-gpg-signatures* nil)
;;; ==============================


;;; ==============================
;;;Use this with same steingold's CLOCC
;;;(URL `http://clocc.sourceforge.net/')
;;(setenv "LISPTYPE" inferior-lisp-program) 
;;(getenv "LISPTYPE")

;;; ==============================
;;; ==============================
;;; Allegro
;; (push "c:/home/sp/bin/acl81-express/eli" load-path)
;; (load "fi-site-init.el")

;; (setq fi:common-lisp-image-name "c:/home/sp/bin/acl81-express/allegro-express.exe")
;; (setq fi:common-lisp-image-file "c:/home/sp/bin/acl81-express/allegro-express.dxl")
;; (setq fi:common-lisp-directory  "c:/home/sp/bin/acl81-express")

;; (defun run-allegro-lisp ()
;;   (interactive)
;;   (fi:common-lisp "*common-lisp*"
;;                   "c:/home/sp/bin/acl81-express/"                     
;;                   "c:/home/sp/bin/acl81-express/allegro-express.exe" 
;;                   '("+B" "+cn")
;;                   "localhost"
;;                   "c:/home/sp/bin/acl81-express/allegro-express.dxl" 
;;                   ))
;;; ==============================

;;; ==============================
;; (URL `http://article.gmane.org/gmane.lisp.slime.devel/8712')
;; (URL `http://thread.gmane.org/gmane.lisp.slime.devel/8442')
;; >> *STANDARD-OUTPUT* of functions called from the REPL is going to
;; >> *inferior-lisp* not to the REPL as it used to.  I noticed this when
;; >> calling DISASSEMBLE; the disassembly appears in the *inferior-lisp*
;; >> buffer.
;; > This is a known bug, but unfortunately it's still not fixed. As a quick
;; > workaround you can evaluate:
;
; (setf *standard-output*
;       (swank::connection.user-output swank::*emacs-connection*)) 
;
; > at the REPL.
;;; ==============================

;;; ==============================
(provide 'slime-loads)
;;; ==============================

;;; ==============================
;;; EOF
