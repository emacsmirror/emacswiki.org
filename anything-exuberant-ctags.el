;;; anything-exuberant-ctags.el --- Exuberant ctags anything.el interface

;; Filename: anything-exuberant-ctags.el
;; Description: Exuberant ctags anything.el interface
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;; Maintainer: Kenichirou Oyama <k1lowxb@gmail.com>
;; Copyright (C) 2011, 101000code/101000LAB, all rights reserved.
;; Created: 2011-08-09
;; Version: 0.1.2
;; URL:
;; Keywords: anything, exuberant ctags
;; Compatibility: GNU Emacs 23
;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; Reference
;; Some code referenced from anything-etags.el, anything-find-project-resources.el
;;
;; anything-find-project-resources.el
;; Author: SAKURAI, Masashi <m.sakurai@kiwanami.net>
;;
;; anything-etags.el
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;;         Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package use `anything' as a interface to find tag with Exuberant ctags.
;;
;; Follow command is `anything' interface of find ctags.
;;
;; `anything-exuberant-ctags-select'
;; `anything-exuberant-ctags-select-from-here'
;;
;; If you want to use tag file in cache directory
;; instead current directory, example you have a
;; tag file in directory "~/MyEmacs/", and you want
;; always search tag in this directory.
;; You can setup like below:
;;
;; (setq anything-exuberant-ctags-enable-tag-file-dir-cache t)
;; (setq anything-exuberant-ctags-cache-tag-file-dir "~/MyEmacs/")
;;
;;; Installation:
;;
;; Put anything-exuberant-ctags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-exuberant-ctags)
;;
;; It is good to use anything-match-plugin.el to narrow candidates.
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin
;;
;; In your project root directory, do follow command to make tags file.
;;
;; ctags --verbose -R --fields="+afikKlmnsSzt"
;;
;; No need more.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-exuberant-ctags-select'
;;    Tag jump using `anything'.
;;  `anything-exuberant-ctags-select-from-here'
;;    Tag jump with current symbol using `anything'.
;;  `anything-exuberant-ctags-enable-cache'
;;    Enable use tag file in cache directory.
;;  `anything-exuberant-ctags-disable-cache'
;;    Disable use tag file in cache directory.
;;  `anything-exuberant-ctags-toggle-cache'
;;    Toggle tag file cache directory status.
;;  `anything-exuberant-ctags-generate-tag-buffer'
;;    Do nothing. Only for compatibility.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-exuberant-ctags-tag-file-name'
;;    Exuberant ctags tag file name.
;;    default = "tags"
;;  `anything-exuberant-ctags-enable-tag-file-dir-cache'
;;    Whether use Exuberant ctags tag file in cache directory.
;;    default = nil
;;  `anything-exuberant-ctags-cache-tag-file-dir'
;;    The cache directory that storage Exuberant ctags tag file.
;;    default = nil
;;  `anything-exuberant-ctags-tag-file-search-limit'
;;    The limit level of directory that search tag file.
;;    default = 10
;;  `anything-exuberant-ctags-line-length-limit'
;;    The limit level of line length.
;;    default = 400
;;  `anything-exuberant-ctags-line-format-func'
;;    The limit level of line length.
;;    default = (\` anything-exuberant-ctags-line-format)

;;; Contributors:
;;
;;     mori_dev <mori.dev.asdf@gmail.com>
;;

;;; TODO
;;
;;      Select scope search

;;; Require
(require 'anything)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup anything-exuberant-ctags nil
  "Exuberant ctags anything.el interface."
  :prefix "anything-exuberant-ctags-"
  :group 'convenience)

(defcustom anything-exuberant-ctags-tag-file-name "tags"
  "Exuberant ctags tag file name."
  :type 'string
  :group 'anything-exuberant-ctags)

(defcustom anything-exuberant-ctags-enable-tag-file-dir-cache nil
  "Whether use Exuberant ctags tag file in cache directory.
If `non-nil', try to use `anything-exuberant-ctags-cache-tag-file-dir'.
Default is nil."
  :type 'boolean
  :group 'anything-exuberant-ctags)

(defcustom anything-exuberant-ctags-cache-tag-file-dir nil
  "The cache directory that storage Exuberant ctags tag file.
This value just use when you setup option
`anything-exuberant-ctags-enable-tag-file-dir-cache' with `non-nil'.
If is nil try to find tag file in current directory.
Default is nil."
  :type 'string-match
  :group 'anything-exuberant-ctags)

(defcustom anything-exuberant-ctags-tag-file-search-limit 10
  "The limit level of directory that search tag file.
Don't search tag file deeply if outside this value.
This value only use when option
`anything-exuberant-ctags-tag-file-dir-cache' is nil."
  :type 'number
  :group 'anything-exuberant-ctags)

(defcustom anything-exuberant-ctags-line-length-limit 400
  "The limit level of line length.
Don't search line longer if outside this value."
  :type 'number
  :group 'anything-exuberant-ctags)

(defcustom anything-exuberant-ctags-line-format-func `anything-exuberant-ctags-line-format
  "The limit level of line length.
Don't search line longer if outside this value."
  :type 'symbol
  :group 'anything-exuberant-ctags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-exuberant-ctags-tag-file-dir nil
  "Exuberant ctags file directory.")

(defvar anything-exuberant-ctags-tag-buffer nil
  "Exuberant ctags tag buffer.")

(defvar anything-exuberant-ctags-max-length 30
  "Max length for file path name.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-exuberant-ctags-select (&optional symbol-name)
  "Tag jump using `anything'.
If SYMBOL-NAME is non-nil, jump tag position with SYMBOL-NAME."
  (interactive)
  (let* ((initial-pattern               ; string or nil
          (if symbol-name
              (concat "\\_<" (regexp-quote symbol-name) "\\_>"
                      (if (featurep 'anything-match-plugin) " "))))
         (anything-quit-if-no-candidate
          (lambda () (if symbol-name
                         (message "No TAGS file or containing `%s'" symbol-name)
                       (message "No TAGS file"))))
         (anything-execute-action-at-once-if-one t))
    (anything '(anything-c-source-exuberant-ctags-select)
              ;; Initialize input with current symbol
              initial-pattern "Find Tag: " nil)))
;;(anything '(anything-c-source-exuberant-ctags-select) nil "Find Tag: " nil)

(defun anything-exuberant-ctags-select-from-here ()
  "Tag jump with current symbol using `anything'."
  (interactive)
  (anything-exuberant-ctags-select (thing-at-point 'symbol)))

(defun anything-exuberant-ctags-enable-cache ()
  "Enable use tag file in cache directory."
  (interactive)
  (setq anything-exuberant-ctags-enable-tag-file-dir-cache t)
  (message "Enable Exuberant ctags file cache directory."))

(defun anything-exuberant-ctags-disable-cache ()
  "Disable use tag file in cache directory."
  (interactive)
  (setq anything-exuberant-ctags-enable-tag-file-dir-cache nil)
  (message "Disable exuberant-ctags file cache directory."))

(defun anything-exuberant-ctags-toggle-cache ()
  "Toggle tag file cache directory status."
  (interactive)
  (if anything-exuberant-ctags-enable-tag-file-dir-cache
      (anything-exuberant-ctags-disable-cache)
    (anything-exuberant-ctags-enable-cache)))

(defun anything-exuberant-ctags-generate-tag-buffer ()
  "Do nothing. Only for compatibility."
  (interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-exuberant-ctags-get-tag-file ()
  "Get Exuberant ctags tag file."
  (cond (anything-exuberant-ctags-enable-tag-file-dir-cache
         ;; Get tag-file form directory `anything-exuberant-ctags-cache-tag-file-dir'
         ;; or `default-directory'.
         (setq anything-exuberant-ctags-tag-file-dir "") ;set tag file directory
         (expand-file-name anything-exuberant-ctags-tag-file-name
                           (or anything-exuberant-ctags-cache-tag-file-dir default-directory)))
        (t
         ;; Get tag file from `default-directory' or upper directory.
         (let ((current-dir (anything-exuberant-ctags-find-tag-file default-directory)))
           ;; Return nil if not find tag file.
           (when current-dir
             (setq anything-exuberant-ctags-tag-file-dir current-dir) ;set tag file directory
             (expand-file-name anything-exuberant-ctags-tag-file-name current-dir))))))

(defun anything-exuberant-ctags-find-tag-file (current-dir)
  "Find tag file.
Try to find tag file in upper directory if haven't found in CURRENT-DIR."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name anything-exuberant-ctags-tag-file-name dir)))
             (and (stringp tag-path)
                  (file-exists-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
       until (file-exists? current-dir)
       ;; Return nil if outside the value of
       ;; `anything-exuberant-ctags-tag-file-search-limit'.
       if (= count anything-exuberant-ctags-tag-file-search-limit)
       do (return nil)
       ;; Or search upper directories.
       else
       do (incf count)
         (setq current-dir (expand-file-name (concat current-dir "../")))
       finally return current-dir)))

(defun anything-exuberant-ctags-create-buffer ()
  "Create buffer from tag file."
  (anything-aif (anything-exuberant-ctags-get-tag-file)
      (with-current-buffer (anything-candidate-buffer 'global)
        (set-syntax-table (with-current-buffer anything-current-buffer
                            (syntax-table)))
        (insert-file-contents it))
    (message "Can't find tag file: %s" it)))

(defun anything-exuberant-ctags-find-tag (candidate)
  "Find tag that match CANDIDATE from `anything-exuberant-ctags-tag-buffer'.
And switch buffer and jump tag position.."
  (catch 'failed
    (let (file-name tag tag-info line)
      (set-buffer (anything-candidate-buffer))
      ;; Get tag.
      (goto-char (point-min))
      (search-forward candidate nil t)
      (beginning-of-line)
      ;(unless (setq tag-info (exuberant-ctags-snarf-tag))
      ; (message "failed")
      ;  (throw 'failed nil))
      (re-search-forward "^[^\t]+\t\\([^\t]+\\).+line:\\([0-9]+\\)\t" nil t)
      (setq file-name (expand-file-name (match-string 1)
                                        anything-exuberant-ctags-tag-file-dir))
      (setq line (string-to-number (match-string 2)))
      (unless (and file-name
                   (file-exists-p file-name))
        (message "Can't find target file: %s" file-name)
        (throw 'failed nil))
      ;; Jump to tag position when
      ;; tag file is valid.
      (find-file file-name)
      (goto-line line))))

(defun anything-exuberant-ctags-get-line (s e)
  (let ((substr (buffer-substring s e)))
    (unless (or (< anything-exuberant-ctags-line-length-limit (length substr)) (string-match "^!_" substr))
      substr
      )))

(defun anything-exuberant-ctags-transformer (tags)
  (let* (list
         format-func
         (name-max-len 0)
         (path-max-len 0)
         (entries
          (mapcar
           (lambda (tag)
             (let (entry name path kind class line)
             (if (string-match "^\\([^\t]+\\)\t\\([^\t]+\\)\t.*line:\\([0-9]+\\)" tag)
                 (progn
                   (setq name (match-string 1 tag))
                   (setq path (match-string 2 tag))
                   (setq line (match-string 3 tag))
                   (put-text-property 0 (length name) 'face 'bold name))
               (setq name "")
               (setq path "")
               (setq line ""))
             (if (string-match "kind:\\([^\t]+\\)\t" tag)
                 (setq kind (match-string 1 tag))
               (setq kind ""))
             (if (string-match "\\(class:[^\t]+\\)" tag)
                 (setq class (concat "[" (match-string 1 tag) "]"))
               (setq class ""))
             (setq name-max-len (max name-max-len (length path)))
             (setq path-max-len (max path-max-len (length path)))
             (setq entry (list path name kind class tag))
             )) tags)))
    (fset 'format-func (symbol-function anything-exuberant-ctags-line-format-func))
    (when (< anything-exuberant-ctags-max-length path-max-len)
      (setq path-max-len anything-exuberant-ctags-max-length))
    (loop for entry in entries
          do
          (push
           (cons
            (format-func entry path-max-len name-max-len)
            (nth 4 entry))
           list)
          finally (return (nreverse list)))))

(defun anything-exuberant-ctags-goto-location (candidate)
  (anything-exuberant-ctags-find-tag candidate)
  (when (and anything-in-persistent-action
             (fboundp 'anything-match-line-color-current-line))
    (anything-match-line-color-current-line)))

(defun anything-source-exuberant-ctags-header-name (x)
  (concat "Exuberant ctags in "
          (with-current-buffer anything-current-buffer
            (anything-exuberant-ctags-get-tag-file))))

(defun anything-exuberant-ctags-line-format (entry path-max-len name-max-len)
  "Format candidate line."
  (format (format "%%%ds : %%-%ds [%%s]%%s" path-max-len name-max-len)
          (let ((path (car entry)))
            (if (< path-max-len (length path))
                (substring path (- path-max-len)) path))
          (cadr entry) (caddr entry) (cadddr entry)))

(defvar anything-c-source-exuberant-ctags-select
  '((name . "Exuberant ctags")
    (header-name . anything-source-exuberant-ctags-header-name)
    (init . anything-exuberant-ctags-create-buffer)
    (candidates-in-buffer)
    (get-line . anything-exuberant-ctags-get-line)
    (action ("Goto the location" . anything-exuberant-ctags-goto-location))
    (candidate-number-limit . 9999)
    (candidate-transformer .
                           (lambda (candidates)
                             (anything-exuberant-ctags-transformer candidates)))))

(provide 'anything-exuberant-ctags)
;;; anything-exuberant-ctags.el ends here
