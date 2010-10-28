;;; yas-jit.el --- Loads Yasnippets on demand (makes start up faster)
;; 
;; Filename: yas-jit.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Oct 27 08:14:43 2010 (-0500)
;; Version: 0.1
;; Last-Updated: Wed Oct 27 22:32:45 2010 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 111
;; URL: http://www.emacswiki.org/emacs/download/yas-jit.el
;; Keywords: Yasnippet fast loading. 
;; Compatibility: Emacs 23.2 with Yasnippet on svn trunk as of release date.
;; 
;; Features that might be required by this library:
;;
;;   `assoc', `button', `cl', `dropdown-list', `easymenu',
;;   `help-fns', `help-mode', `view', `yasnippet'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Usage:
;;   Instead of using 
;;
;;   (require 'yasnippet)
;;   (setq yas/root-directory snippet-directory)
;;   (yas/load-directory yas/root-directory)
;;
;;   which takes some time on initial loading, use
;;
;;   (require 'yas-jit)
;;   (setq yas/root-directory snippet-directory)
;;   (yas/jit-load)
;;
;;   Note this works assuming that each load directory contains a list
;;   of modes. Currently the release version of yasnippet does not use
;;   this paradigm. However the svn trunk does.
;;
;;   Also note that yasnippet requires something in the hash,
;;   otherwise it loads everything.  Therefore text-mode snippets are
;;   loaded by default.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 22:31:46 2010 (-0500) #107 (Matthew L. Fidler)

;;    Changed JIT reload-all to load the modes of all open buffers
;;    instead of just the currently open mode

;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 11:45:14 2010 (-0500) #95 (Matthew L. Fidler)
;;    Added hook.  Should (in theory) work well.
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 10:43:47 2010 (-0500) #74 (Matthew L. Fidler)
;;    Changed yas/minor-mode-on definition
;; 27-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct 27 10:25:30 2010 (-0500) #65 (Matthew L. Fidler)

;;    Tried setting root directory to text-mode so that yas will not
;;    load anything until the hook is called

;; 27-Oct-2010    Matthew L. Fidler  
;;    Initial Release
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl)
(require 'yasnippet)
(defvar yas/jit-loads '()
  "Alist of JIT loads for yas.")
(defvar yas/get-jit-loads-again 't)
(defun yas/get-jit-loads ()
  "* Loads Snippet directories just in time.  Should speed up the start-up of Yasnippet"
  (when yas/get-jit-loads-again
    (let* (
           (dirs (yas/snippet-dirs))
           files
           modes
           (files '())
           (debug-on-error 't)
           jit
           )
      (when dirs
        (mapc (lambda(x)
                (setq files (append files (directory-files x 't)))
                )
              (yas/snippet-dirs)
              )
        (setq modes 
              (remove-if-not
               #'(lambda(file)
                   (and (file-directory-p file)
                        (not (string-match "^[.]" (file-name-nondirectory file)))
                        )
                   )
               (directory-files (pop dirs) 't)
               )
              )
        (setq jit (mapcar (lambda(x) (list (intern (file-name-nondirectory x)) x) ) modes))
        ;; Now add more directories.
        (when (> (length dirs) 0)
          (mapc
           (lambda(dir)
             (let (
                   (modes (remove-if-not
                           #'(lambda(file)
                               (and (file-directory-p file)
                                    (not (string-match "^[.]" (file-name-nondirectory file)))
                                    )
                               )
                           (directory-files dir 't)
                           ))
                   )
               (mapc (lambda(mode)
                       (if (not (assoc (intern (file-name-nondirectory mode)) jit))
                           (add-to-list 'jit (list (intern (file-name-nondirectory mode)) mode))
                         (setq jit (mapcar
                                    (lambda(m)
                                      (if (eq (intern (file-name-nondirectory mode)) (car m))
                                          (append m (list mode))
                                        m
                                        )
                                      )
                                    jit)
                               )
                         )
                       )
                     modes
                     )
               )
             )
           dirs
           )
          )
        (setq yas/jit-loads jit)
        (let ((major-mode 'text-mode)
              )
          (yas/jit-hook)
          )
        )
      (setq yas/get-jit-loads-again nil)
      )
    ))
(defun yas/jit-hook ()
  "Have Yas load directories as needed. Derived from `yas/direct-keymaps-set-vars'"
  (interactive)
  (let ((modes-to-activate (list major-mode))
        (mode major-mode)
        (debug-on-error 't)
        (debug-on-quit 't)
        )
    (while (setq mode (get mode 'derived-mode-parent))
      (push mode modes-to-activate))
    (dolist (mode (yas/extra-modes))
      (push mode modes-to-activate))
    (dolist (mode modes-to-activate)
      (let (
            (test-mode mode)
            (other-modes '())
            cur-mode
            tmp
            )
        (setq tmp (assoc test-mode yas/jit-loads))
        (while tmp
          (setq yas/get-jit-loads-again 't) ;; Get loads since some of the JIT loads have left.
          (setq cur-mode (pop tmp))
          (setq yas/jit-loads
                (remove-if 
                 #'(lambda(x)
                     (eq cur-mode (car x))
                     )
                 yas/jit-loads
                 )
                )
          (mapc (lambda(dir)
                  (message "Loading snippet directory %s" dir)
                  (yas/load-directory-1 dir cur-mode
                                        (if (not (file-readable-p (concat dir "/.yas-parents")))
                                            nil
                                          (mapcar #'intern
                                                  (split-string
                                                   (with-temp-buffer
                                                     (insert-file-contents (concat dir "/.yas-parents"))
                                                     (buffer-substring-no-properties (point-min)
                                                                                     (point-max))))))
                                        )
                  (when (file-exists-p (concat dir "/.yas-parents"))
                    (with-temp-buffer
                      (insert-file-contents (concat dir "/.yas-parents"))
                      (mapc (lambda(x)
                              (add-to-list 'other-modes x)
                              )
                            (split-string (buffer-substring (point-min) (point-max)) nil 't)
                            )
                      )
                    )
                  )
                tmp)
          (setq other-modes (remove-if-not #'(lambda(x) (assoc (intern x) yas/jit-loads)) other-modes))
          (setq tmp nil)
          (when (> (length other-modes) 0)
            (setq test-mode (intern (pop other-modes)))
            (setq tmp (assoc test-mode yas/jit-loads))
            )
          )
        )
      )
    )
  ) 
(defalias 'yas/jit-load 'yas/get-jit-loads)

(defun yas/jit-hook-run ()
  "* Run yas/jit-hook and setup hooks again..."
  (add-hook 'after-change-major-mode-hook 'yas/jit-hook-run)
  (add-hook 'find-file-hook 'yas/jit-hook-run)
  (add-hook 'change-major-mode-hook 'yas/jit-hook-run)
  (yas/jit-hook)
  )
(add-hook 'after-change-major-mode-hook 'yas/jit-hook-run)
(add-hook 'find-file-hook 'yas/jit-hook-run)
(add-hook 'change-major-mode-hook 'yas/jit-hook-run)

(defun yas/load-snippet-dirs ()
  "Reload the directories listed in `yas/snippet-dirs' or
   prompt the user to select one."
  (if yas/snippet-dirs
      (progn 
        (yas/get-jit-loads)
        (let ( (modes '())
                (bufs (buffer-list))
                )
           ;; Load snippets for major modes of all open buffers
           (mapc (lambda(x)
                   (save-excursion
                     (set-buffer x)
                     (yas/jit-hook) ;; Load current mode's snippets.
                     )
                   )
                 bufs
                 )
           )
        )
    (call-interactively 'yas/load-directory)))
(provide 'yas-jit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yas-jit.el ends here
