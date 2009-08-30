;;; mgrep.el
;; -*- Mode: Emacs-Lisp -*-

;;  $Id: mgrep.el,v 1.8.2.1 2003/10/24 10:28:46 akihisa Exp $

;; Author: Matsushita Akihisa <akihisa@mail.ne.jp>
;; Keywords: grep memo

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Probably, you will search a word under same directory many
;; times. mgrep can search under the directory you specified
;; previously.

;; The latest version of this program can be downloaded from
;; http://www.bookshelf.jp/elc/mgrep.el

;;; Usage

;; M-x mgrep

;; Run grep under your favorite directory

;;; Variables

;; mgrep-list : Your favorite directory list. This valiable is used
;; as bellow.

;; (setq mgrep-list
;;       '(
;;          name   directory        mask   option
;;         ("dir" default-directory "*.el" dir)
;;         ("config" "~/mylisp/"  "*.el" nil)
;;         ("1.99" "d:/unix/Meadow2/1.99a6/" "*.*" sub)
;;         ))

;; name : Input your favorite name
;; directory : Directory you'd like to search
;; mask : file-mask for grep command.
;; option : usually option is nil. If option is t, mgrep uses find
;; command. If option is "dir", you can select directory like
;; find-file. If option is "sub", you can select sub directory to
;; search. "dirfind" , "subfind" are the options to use find command
;; of "dir" , "sub"

(require 'compile)

(defvar mgrep-list
      '(
        ;; name directory filemask subdirectory
        ("site" "C:/unix/Meadow2/1.99a6/site-lisp" "*.el" t)
        ))

;;; Internal variables
(defvar mgrep-word-history nil)
(defvar mgrep-directory-history nil)
(defvar mgrep-find-command nil)
(defvar mgrep-find-grep-command nil)
(defvar mgrep-find-grep-command-last nil)

(defun mgrep-set-find-command ()
  (setq grep-find-command
        (cond ((eq grep-find-use-xargs 'gnu)
               (setq mgrep-find-command
                     (format "%s . -name "
                             find-program))
               (setq mgrep-find-grep-command
                     (format " -type f -print0 | xargs -0 -e %s"
                             grep-command))
               (setq mgrep-find-grep-command-last nil)
               )
              (grep-find-use-xargs
               (setq mgrep-find-command
                     (format "%s . -name "
                             find-program))
               (setq mgrep-find-grep-command
                     (format " -type f -print | xargs %s"
                             grep-command))
               (setq mgrep-find-grep-command-last nil))
              (t
               (setq mgrep-find-command
                     (format "%s . -name "
                             find-program))
               (setq mgrep-find-grep-command
                     (format " -type f -exec %s"
                             grep-command null-device))
               (setq mgrep-find-grep-command-last
                     (format " {} %s \\;"
                             null-device))))))

(defun mgrep (arg)
  (interactive "p")
  (unless grep-command
    (grep-compute-defaults))
  (unless grep-find-command
    (grep-compute-defaults))
  (unless mgrep-find-command
    (mgrep-set-find-command))
  (let (name regexp dir lst)
    (setq name (completing-read
                (concat
                 "mgrep directory "
                 (when (car mgrep-directory-history)
                   (format "(default %s)"
                           (car mgrep-directory-history)))
                 " : ")
                mgrep-list
                nil nil nil 'mgrep-directory-history
                (if (car mgrep-directory-history)
                    (car mgrep-directory-history)
                  nil)
                ))
    (if (assoc name mgrep-list)
        ()
      (setq mgrep-directory-history (cdr mgrep-directory-history))
      (error "Input correct name!"))

    (setq dir (file-name-as-directory
               (eval (nth 1 (assoc name mgrep-list)))))

    (if (or (string= 'sub (nth 3 (assoc name mgrep-list)))
            (string= 'subfind (nth 3 (assoc name mgrep-list))))
        (progn
          ;; directory list
          (setq lst (mapcar '(lambda (file)
                               (if (and (not (string-match "\\.+$" file))
                                        (file-directory-p file))
                                   (file-name-nondirectory file)))
                            (directory-files (eval (nth 1 (assoc name mgrep-list))) t)))
          (setq lst (delq nil lst))

          (setq dir (concat
                     (file-name-as-directory dir)
                     (completing-read
                      "mgrep sub directory : "
                      (mapcar 'list lst)
                      nil t)
                     "/"))
                ))
    
    (if (or (string= 'dir (nth 3 (assoc name mgrep-list)))
            (string= 'dirfind (nth 3 (assoc name mgrep-list))))
        (progn
          (setq dir (expand-file-name (read-file-name "Directory: ")))
          (if (file-directory-p dir)
              (setq dir (file-name-as-directory dir))
            (setq dir (file-name-directory dir)))))

    (setq regexp (read-from-minibuffer "mgrep word : "
                                       nil nil nil
                                       'mgrep-word-history
                                       ))
    
    (let ((default-directory dir)
          (null-device nil)
          com)
      (with-temp-buffer
        (cond
         ((or (not (nth 3 (assoc name mgrep-list)))
              (string= 'sub (nth 3 (assoc name mgrep-list)))
              (string= 'dir (nth 3 (assoc name mgrep-list))))
          (setq com (concat grep-command "\"" regexp "\" "
                          (nth 2 (assoc name mgrep-list))))
          (grep com))
         (t
          (setq com
                (concat
                 mgrep-find-command
                 (regexp-quote
                  (nth 2 (assoc name mgrep-list))) ;; *.el
                 mgrep-find-grep-command
                 "\"" regexp "\""
                 mgrep-find-grep-command-last))
          (grep-find com)))
        ))))

(provide 'mgrep)
;;; mgrep.el ends here
