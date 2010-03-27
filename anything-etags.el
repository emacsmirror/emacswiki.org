;;; anything-etags.el --- Etags anything.el interface

;; Filename: anything-etags.el
;; Description: Etags anything.el interface
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;;         Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Kenichirou Oyama <k1lowxb@gmail.com>
;; Copyright (C) 2009, Kenichirou Oyama, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009, rubikitch, all rights reserved.
;; Copyright (C) 2009, Thierry Volpiatto, all rights reserved.
;; Created: 2009-01-29 18:19:38
;; Version: 1.1.6
;; URL: http://www.emacswiki.org/emacs/download/anything-etags.el
;; Keywords: anything, etags
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `anything' `etags'
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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package use `anything' as a interface to find tag with Etags.
;;
;;
;; You can also make this package integrate with `anything',
;; just setup like below:
;;
;; (setq anything-sources
;;       '(anything-etags-c-source-etags-select
;;        ))
;;
;; If you want to use tag file in cache directory
;; instead current directory, example you have a
;; tag file in directory "~/MyEmacs/", and you want
;; always search tag in this directory.
;; You can setup like below:
;;
;; (setq anything-etags-enable-tag-file-dir-cache t)
;; (setq anything-etags-cache-tag-file-dir "~/MyEmacs/")
;;
;;; Installation:
;;
;; Put anything-etags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-etags)
;;
;; It is good to use anything-match-plugin.el to narrow candidates.
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin
;;
;; No need more.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-etags-select'
;;    Tag jump using etags and `anything'.
;;  `anything-etags-select-from-here'
;;    Tag jump with current symbol using etags and `anything'.
;;  `anything-etags-enable-cache'
;;    Enable use tag file in cache directory.
;;  `anything-etags-disable-cache'
;;    Disable use tag file in cache directory.
;;  `anything-etags-toggle-cache'
;;    Toggle tag file cache directory status.
;;  `anything-etags-generate-tag-buffer'
;;    Do nothing. Only for compatibility.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-etags-tag-file-name'
;;    Etags tag file name.
;;    default = "TAGS"
;;  `anything-etags-enable-tag-file-dir-cache'
;;    Whether use Etags tag file in cache directory.
;;    default = nil
;;  `anything-etags-cache-tag-file-dir'
;;    The cache directory that storage Etags tag file.
;;    default = nil
;;  `anything-etags-tag-file-search-limit'
;;    The limit level of directory that search tag file.
;;    default = 10


;;; Change log:
;;
;; 2010/03/27
;;   * rubikitch:
;;      * Fix a bug of handling same tags. I tested in Ruby code.
;;      * With that, slight changes in display.
;;
;; 2009/04/04
;;   * Thierry Volpiatto:
;;      * Regexp change in `anything-c-etags-get-line'.
;;      
;; 2009/03/28
;;   * Thierry Volpiatto:
;;      * code cleanup.
;;   * rubikitch:
;;      * More beautiful line. (only show the line)
;;      * Show TAGS file name in header.
;;      
;; 2009/03/23
;;   * rubikitch:
;;      * Use `expand-file-name' instead of `concat' to make full path.
;;      * Optionally highlight in persistent-action.
;;      * Fix doc.
;;      
;; 2009/03/19
;;   * Kenichirou Oyama:
;;      * Fix bug.
;;
;; 2009/03/19
;;   * Kenichirou Oyama:
;;      * Use `etags-goto-tag-location'.
;;
;; 2009/03/18
;;   * Kenichirou Oyama:
;;      * Applied SUGAWARA's patch to make `anything-etags-generate-tag-buffer' to user command.
;;      * makes `anything-etags-generate-tag-buffer' to user command.
;;      * Fix doc.
;;
;; 2009/03/18
;;   * Kenichirou Oyama:
;;      * Applied SUGAWARA's patch to make multi TAGS buffer.
;;      * use separete tag buffer for each tag file.
;;      * create tag buffer only first visit time.
;;      * Fix doc.
;;      * Fix bug.
;;
;; 2009/03/07
;;   * rubikitch:
;;      * More kind error message.
;;
;; 2009/02/07
;;   * rubikitch:
;;      * Use `anything-quit-if-no-candidate' and
;;        `anything-execute-action-at-once-if-one' to suppress
;;        opening unneeded *anything* window.
;;      * New command `anything-etags-select-from-here'.
;;      * Suggestion of anything-match-plugin.el.
;;      * Fix bug.
;;      * Refactor code.
;;   * Andy Stewart
;;      * Remove option `anything-etags-enable-initial-pattern'
;;        not necessary.
;;      * Fix doc.
;;
;; 2009/02/01
;;   * Andy Stewart:
;;      * Applied IMAKADO's patch to fix the "infinite loop" bug
;;        when TAGS file in more than two levels upper directory.
;;
;; 2009/01/30
;;   * Andy Stewart:
;;      * Remove file patch that haven't contain tag information
;;        from tag buffer.
;;      * Don't show \x01 in *anything* buffer, beautiful <line>.
;;      * Fix bug.
;;
;; 2009/01/30
;;   * rubikitch:
;;      * Fix bug.
;;      * Refactor code.
;;
;; 2009/01/29
;;   * Andy Stewart:
;;      * Refactor source code and make it more readability.
;;      * Fix bug.
;;      * Fix doc.
;;      * Make user can use function `anything-etags-generate-tag-buffer'
;;        to generate tag buffer when startup.
;;        Then user doesn't need wait `anything' when first use.
;;
;; 2009/01/28
;;   * Kenichirou:
;;      * Patch from Andy Stewart.
;;        Refactor source code and speed up for
;;        `anything-c-source-etags-select' user.
;;      * New variable `anything-etags-enable-tag-file-dir-cache'.
;;
;; 2009/01/29
;;   * Kenichirou:
;;      * First released.
;;

;;; Acknowledgements:
;;
;;  ThierryVolpiatto for advice and highlight idea.
;;

;;; TODO
;;
;;      Highlight keyword and name.
;;      Highlight match type.
;;      Multiline: with filename.
;;

;;; Require
(require 'anything)
(require 'etags)
(require 'anything-match-plugin nil t)  ;optional
(require 'anything-config nil t)        ;optional

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup anything-etags nil
  "Etags anything.el interface."
  :prefix "anything-etags-"
  :group 'convenience)

(defcustom anything-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type 'string
  :group 'anything-etags)

(defcustom anything-etags-enable-tag-file-dir-cache nil
  "Whether use Etags tag file in cache directory.
If `non-nil', try to use `anything-etags-cache-tag-file-dir'.
Default is nil."
  :type 'boolean
  :group 'anything-etags)

(defcustom anything-etags-cache-tag-file-dir nil
  "The cache directory that storage Etags tag file.
This value just use when you setup option
`anything-etags-enable-tag-file-dir-cache' with `non-nil'.
If is nil try to find tag file in current directory.
Default is nil."
  :type 'string-match
  :group 'anything-etags)

(defcustom anything-etags-tag-file-search-limit 10
  "The limit level of directory that search tag file.
Don't search tag file deeply if outside this value.
This value only use when option
`anything-etags-enable-tag-file-dir-cache' is nil."
  :type 'number
  :group 'anything-etags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-etags-tag-file-dir nil
  "Etags file directory.")

(defvar anything-etags-tag-buffer nil
  "Etags tag buffer.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-etags-select (&optional symbol-name)
  "Tag jump using etags and `anything'.
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
    (anything '(anything-c-source-etags-select)
              ;; Initialize input with current symbol
              initial-pattern "Find Tag: " nil)))

(defun anything-etags-select-from-here ()
  "Tag jump with current symbol using etags and `anything'."
  (interactive)
  (anything-etags-select (thing-at-point 'symbol)))

(defun anything-etags-enable-cache ()
  "Enable use tag file in cache directory."
  (interactive)
  (setq anything-etags-enable-tag-file-dir-cache t)
  (message "Enable etags file cache directory."))

(defun anything-etags-disable-cache ()
  "Disable use tag file in cache directory."
  (interactive)
  (setq anything-etags-enable-tag-file-dir-cache nil)
  (message "Disable etags file cache directory."))

(defun anything-etags-toggle-cache ()
  "Toggle tag file cache directory status."
  (interactive)
  (if anything-etags-enable-tag-file-dir-cache
      (anything-etags-disable-cache)
    (anything-etags-enable-cache)))

(defun anything-etags-generate-tag-buffer ()
  "Do nothing. Only for compatibility."
  (interactive))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-etags-get-tag-file ()
  "Get Etags tag file."
  (cond (anything-etags-enable-tag-file-dir-cache
         ;; Get tag-file form directory `anything-etags-cache-tag-file-dir'
         ;; or `default-directory'.
         (setq anything-etags-tag-file-dir "") ;set tag file directory
         (expand-file-name anything-etags-tag-file-name
                           (or anything-etags-cache-tag-file-dir default-directory)))
        (t
         ;; Get tag file from `default-directory' or upper directory.
         (let ((current-dir (anything-etags-find-tag-file default-directory)))
           ;; Return nil if not find tag file.
           (when current-dir
             (setq anything-etags-tag-file-dir current-dir) ;set tag file directory
             (expand-file-name anything-etags-tag-file-name current-dir))))))

(defun anything-etags-find-tag-file (current-dir)
  "Find tag file.
Try to find tag file in upper directory if haven't found in CURRENT-DIR."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name anything-etags-tag-file-name dir)))
             (and (stringp tag-path)
                  (file-exists-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
       until (file-exists? current-dir)
       ;; Return nil if outside the value of
       ;; `anything-etags-tag-file-search-limit'.
       if (= count anything-etags-tag-file-search-limit)
       do (return nil)
       ;; Or search upper directories.
       else
       do (incf count)
         (setq current-dir (expand-file-name (concat current-dir "../")))
       finally return current-dir)))

(defun anything-etags-create-buffer ()
  "Create buffer from tag file."
  (anything-aif (anything-etags-get-tag-file)
      (with-current-buffer (anything-candidate-buffer 'global)
        (set-syntax-table (with-current-buffer anything-current-buffer
                            (syntax-table)))
        (insert-file-contents it))
    (message "Can't find tag file: %s" it)))
          

(defun anything-etags-find-tag (candidate)
  "Find tag that match CANDIDATE from `anything-etags-tag-buffer'.
And switch buffer and jump tag position.."
  (setq candidate (replace-regexp-in-string " +\177" "\177" candidate))
  (catch 'failed
    (let (file-name tag tag-info)
      (set-buffer (anything-candidate-buffer))
      ;; Get tag.
      (goto-char (point-min))
      (search-forward candidate nil t)
      (beginning-of-line)
      (unless (setq tag-info (etags-snarf-tag))
        (message "failed")
        (throw 'failed nil))
      (re-search-backward "\x0c\n\\(.+\\),[0-9]+\n" nil t)
      (setq file-name (and (match-string 1)
                           (expand-file-name (match-string 1)
                                             anything-etags-tag-file-dir)))
      (unless (and file-name
                   (file-exists-p file-name))
        (message "Can't find target file: %s" file-name)
        (throw 'failed nil))
      ;; Jump to tag position when
      ;; tag file is valid.
      (find-file file-name)
      (etags-goto-tag-location tag-info))))

;;; Rationale: ~/memo/junk/2010-03-27-053504.anything-etags-test.rb
(defun* anything-c-etags-get-line (s e &optional (width (- (window-width) 6)))
  (let ((substr (buffer-substring s e)))
    (unless (string-match "^/.*/.[^,]*\\|^\x0c\\|^\\<.*/.[^,]*" substr)
      (anything-aif (string-match "\177" substr)
          (concat (substring substr 0 (match-beginning 0))
                  (or (ignore-errors (make-string (- width (length substr)) ? )) "")
                  (substring substr (match-beginning 0)))
        substr))))

(defun anything-c-etags-goto-location (candidate)
  (ring-insert find-tag-marker-ring (point-marker))
  (anything-etags-find-tag candidate)
  (when (and anything-in-persistent-action
             (fboundp 'anything-match-line-color-current-line))
    (anything-match-line-color-current-line)))

(defun anything-c-source-etags-header-name (x)
  (concat "Etags in "
          (with-current-buffer anything-current-buffer
            (anything-etags-get-tag-file))))

(defvar anything-c-source-etags-select
  '((name . "Etags")
    (header-name . anything-c-source-etags-header-name)
    (init . anything-etags-create-buffer)
    (candidates-in-buffer)
    (get-line . anything-c-etags-get-line)
    (action ("Goto the location" . anything-c-etags-goto-location))))

(provide 'anything-etags)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-etags.el")
;;; anything-etags.el ends here

;;; LocalWords:  etags Kenichirou Oyama MyEmacs Refactor IMAKADO's plugin
;;; LocalWords:  Multiline featurep

