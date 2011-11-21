;;; anything-etags+.el ---Another Etags anything.el interface

;; Description: Another Etags anything.el interface
;; Filename: anything-etags+.el
;; Created: 2011-02-23
;; Last Updated: Joseph 2011-11-21 16:46:18 星期一
;; Version: 0.1.4
;; Author: 纪秀峰(Joseph) <jixiuf@gmail.com>
;; Maintainer: Joseph <jixiuf@gmail.com>
;; Copyright (C) 2011~, Joseph, all rights reserved.
;; URL:http://www.emacswiki.org/emacs/anything-etags+.el
;;     https://github.com/jixiuf/anything-etags-plus
;; screencast:http://screencast-repos.googlecode.com/files/emacs-anything-etags-puls.mp4.bz2
;; Keywords: anything, etags
;; Compatibility: (Test on GNU Emacs 23.2.1)
;;   I am trying to make it work with XEmacs ,
;;   but I haven't tested it on XEmacs.
;;  .
;;
;; Features that might be required by this library:
;;
;; `anything' `etags'
;;
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
;;  it support multiple tag files.
;;  and it can recursively searches each parent directory for a file named
;;  'TAGS'. so you needn't add this special file to `tags-table-list'
;;
;;  if you use GNU/Emacs ,you can set `tags-table-list' like this.
;;  (setq tags-table-list '("/java/tags/TAGS"
;;                          "/java/tags/linux.tag"
;;                          "/java/tags/tag3"))
;;
;;  if you use XEmacs ,you can set `tag-table-alist' like this.
;;  (setq tag-table-alist
;;        '(("/usr/src/public/perl/" . "/usr/src/public/perl/perl-3.0/")
;;          ("\\.el$" . "/usr/local/emacs/src/")
;;          ("/jbw/gnu/" . "/usr15/degree/stud/jbw/gnu/")
;;          ("" . "/usr/local/emacs/src/")
;;  ))
;;
;;  (global-set-key "\M-." 'anything-etags+-select-one-key)
;;       `M-.' call  anything-etags+-select-at-point
;;       `C-uM-.' call anything-etags+-select
;;   or
;; (define-key anything-command-map (kbd "e") 'anything-etags+-select-at-point)
;; (define-key anything-command-map (kbd "C-e") 'anything-etags+-select)
;;
;; anything-etags+.el also support history go back ,go forward and list tag
;; histories you have visited.(must use commands list here:)
;;  `anything-etags+-history'
;;    List all tag you have visited with `anything'.
;;  `anything-etags+-history-go-back'
;;    Go back cyclely.
;;  `anything-etags+-history-go-forward'
;;    Go Forward cyclely.
;;
;; if you want to work with `etags-table.el' ,you just need
;; add this line to to init file after loading etags-table.el
;;
;;     (add-hook 'anything-etags+-select-hook 'etags-table-recompute)
;;    (setq etags-table-alist
;;     (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '(".*\\.[ch]$" "/usr/local/include/TAGS")
;;        ))
;;
;;; Installation:
;;
;; Don't need anything-etags.el (another etags interface).
;; Just put anything-etags+.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-etags+)
;;
;; No need more.
;;
;; I use GNU/Emacs,and this is my config file about etags
;; (require 'anything-etags+)
;; (setq anything-etags+-use-short-file-name nil)
;; ;;you can use  C-uM-. input symbol (default thing-at-point 'symbol)
;; (global-set-key "\M-." 'anything-etags+-select-one-key)
;; ;;list all visited tags
;; (global-set-key "\M-*" 'anything-etags+-history)
;; ;;go back directly
;; (global-set-key "\M-," 'anything-etags+-history-action-go-back)
;; ;;go forward directly
;; (global-set-key "\M-/" 'anything-etags+-history-action-go-forward)
;;
;; and how to work with etags-table.el
;; (require 'etags-table)
;; (setq etags-table-alist
;;       (list
;;        '("/home/me/Projects/foo/.*\\.[ch]$" "/home/me/Projects/lib1/TAGS" "/home/me/Projects/lib2/TAGS")
;;        '("/home/me/Projects/bar/.*\\.py$" "/home/me/Projects/python/common/TAGS")
;;        '("/tmp/.*\\.c$"  "/java/tags/linux.tag" "/tmp/TAGS" )
;;        '(".*\\.java$"  "/opt/sun-jdk-1.6.0.22/src/TAGS" )
;;        '(".*\\.[ch]$"  "/java/tags/linux.ctags")
;;        ))
;; (add-hook 'anything-etags+-select-hook 'etags-table-recompute)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-etags+-select'
;;    Tag jump using etags and `anything'.
;;  `anything-etags+-select-at-point'
;;    Tag jump with current symbol using etags and `anything'.
;;  `anything-etags+-select-one-key'
;;    you can bind this to `M-.'
;;  `anything-etags+-history-go-back'
;;    Go Back.
;;  `anything-etags+-history-go-forward'
;;    Go Forward.
;;  `anything-etags+-history'
;;    show all tag historys using `anything'
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-etags+-use-short-file-name'
;;    Use short source file name as each candidate's display.
;;    default = t
;;  `anything-etags+-highlight-tag-after-jump'
;;    *If non-nil, temporarily highlight the tag
;;    default = t
;;  `anything-etags+-highlight-delay'
;;    *How long to highlight the tag.
;;    default = 0.2

;;; Code:

;; Some functions are borrowed from anything-etags.el and etags-select.el.

;;; Require
(require 'custom)
(require 'etags)
(require 'anything)
(require 'anything-config nil t)        ;optional
(require 'anything-match-plugin nil t)  ;optional

;;; Custom

(defgroup anything-etags+ nil
  "Another Etags anything.el interface."
  :prefix "anything-etags+-"
  :group 'convenience)

(defcustom anything-etags+-use-short-file-name t
  "Use short source file name as each candidate's display.
 search '(DISPLAY . REAL)' in anything.el for more info."
  :type 'boolean
  :group 'anything-etags+)

(defcustom anything-etags+-highlight-tag-after-jump t
  "*If non-nil, temporarily highlight the tag
  after you jump to it.
  (borrowed from etags-select.el)"
  :group 'anything-etags+-
  :type 'boolean)

(defcustom anything-etags+-highlight-delay 0.2
  "*How long to highlight the tag.
  (borrowed from etags-select.el)"
  :group 'anything-etags+-
  :type 'number)

(defface anything-etags+-highlight-tag-face
  '((t (:foreground "white" :background "cadetblue4" :bold t)))
  "Font Lock mode face used to highlight tags.
  (borrowed from etags-select.el)"
  :group 'anything-etags+-)

(defun anything-etags+-highlight (beg end)
  "Highlight a region temporarily.
   (borrowed from etags-select.el)"
  (if (featurep 'xemacs)
      (let ((extent (make-extent beg end)))
        (set-extent-property extent 'face 'anything-etags+-highlight-tag-face)
        (sit-for anything-etags+-highlight-delay)
        (delete-extent extent))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'face 'anything-etags+-highlight-tag-face)
      (sit-for anything-etags+-highlight-delay)
      (delete-overlay ov))))

;;; Hooks

(defvar anything-etags+-select-hook nil
  "hooks run before `anything' funcion with
   source `anything-c-source-etags+-select'")

;;; Variables
(defvar  anything-etags+-tag-marker-ring (make-ring 8))

(defvar anything-etags+-current-marker-in-tag-marker-ring nil
  "a marker in `anything-etags+-tag-marker-ring', going back and going
forward are related to this variable.")

;; (defvar anything-etags+-history-tmp-marker nil
;;   "this variable will remember current position
;;    when you call `anything-etags+-history'.
;;   after you press `RET' execute `anything-etags+-history-action'
;;  it will be push into `anything-etags+-tag-marker-ring'")
(defvar anything-etags+-tag-table-buffers nil
  "each time `anything-etags+-select' is executed ,it
will set this variable.")
(defvar anything-idle-delay-4-anything-etags+ 1.0
  "see `anything-idle-delay'. I will set it locally
   in `anything-etags+-select'")

(defvar previous-opened-buffer-in-persistent-action nil
  "record it to kill-it in persistent-action,in order to
   not open too much buffer.")

(defvar anything-etags+-use-xemacs-etags-p
  (fboundp 'get-tag-table-buffer)
  "Use XEmacs etags?")

(defvar anything-etags+-previous-matched-pattern nil
  "work with `anything-etags+-candidates-cache'.
  the value is (car (amp-mp-make-regexps anything-pattern))
:the first part of `anything-pattern', the matched
 candidates is saved in `anything-etags+-candidates-cache'. when current
'(car (amp-mp-make-regexps anything-pattern))' is equals to this value
then the cached candidates can be reused ,needn't find from the tag file.")

(defvar anything-etags+-candidates-cache nil
  "documents see `anything-etags+-previous-matched-pattern'")
(defvar anything-etags+-untransformed-anything-pattern
  "this variable is seted in func of transformed-pattern .and is used when
getting candidates.")

;;; Functions
(defun anything-etags+-match-string (num &optional string))
(defun anything-etags+-file-truename (filename))

(if (string-match "XEmacs" emacs-version)
    (fset 'anything-etags+-match-string 'match-string)
  (fset 'anything-etags+-match-string 'match-string-no-properties)
  (if anything-etags+-use-xemacs-etags-p
      (unless (fboundp 'symlink-expand-file-name)
        (fset 'symlink-expand-file-name 'file-truename))))

(if (fboundp 'symlink-expand-file-name)
    (fset 'anything-etags+-file-truename 'symlink-expand-file-name)
  (fset 'anything-etags+-file-truename 'file-truename)
  )

(defun anything-etags+-case-fold-search ()
  "Get case-fold search."
  (when (boundp 'tags-case-fold-search)
    (if (memq tags-case-fold-search '(nil t))
        tags-case-fold-search
      case-fold-search)))

(defun anything-etags+-etags-snarf-tag (&optional use-explicit)
  "borrowed from GNU/Emacs etags.el"
  (let (tag-text line startpos explicit-start)
    (if (save-excursion
          (forward-line -1)
          (looking-at "\f\n"))
        ;; The match was for a source file name, not any tag within a file.
        ;; Give text of t, meaning to go exactly to the location we specify,
        ;; the beginning of the file.
        (setq tag-text t
              line nil
              startpos (point-min))

      ;; Find the end of the tag and record the whole tag text.
      (search-forward "\177")
      (setq tag-text (buffer-substring (1- (point))
                                       (save-excursion (beginning-of-line)
                                                       (point))))
      ;; If use-explicit is non nil and explicit tag is present, use it as part of
      ;; return value. Else just skip it.
      (setq explicit-start (point))
      (when (and (search-forward "\001" (save-excursion (forward-line 1) (point)) t)
                 use-explicit)
        (setq tag-text (buffer-substring explicit-start (1- (point)))))


      (if (looking-at "[0-9]")
          (setq line (string-to-number (buffer-substring
                                        (point)
                                        (progn (skip-chars-forward "0-9")
                                               (point))))))
      (search-forward ",")
      (if (looking-at "[0-9]")
          (setq startpos (string-to-number (buffer-substring
                                            (point)
                                            (progn (skip-chars-forward "0-9")
                                                   (point)))))))
    ;; Leave point on the next line of the tags file.
    (forward-line 1)
    (cons tag-text (cons line startpos))))

(defun anything-etags+-goto-tag-location (tag-info)
  "Go to location of tag specified by TAG-INFO.
TAG-INFO is a cons (TEXT LINE . POSITION).
TEXT is the initial part of a line containing the tag.
LINE is the line number.
POSITION is the (one-based) char position of TEXT within the file.

If TEXT is t, it means the tag refers to exactly LINE or POSITION,
whichever is present, LINE having preference, no searching.
Either LINE or POSITION can be nil.  POSITION is used if present.

If the tag isn't exactly at the given position, then look near that
position using a search window that expands progressively until it
hits the start of file."
  (let ((startpos (cdr (cdr tag-info)))
        (line (car (cdr tag-info)))
        offset found pat)
    (if (eq (car tag-info) t)
        ;; Direct file tag.
        (cond (line (progn (goto-char (point-min))
                           (forward-line (1- line))))
              (startpos (goto-char startpos))
              (t (error "etags.el BUG: bogus direct file tag")))
      ;; This constant is 1/2 the initial search window.
      ;; There is no sense in making it too small,
      ;; since just going around the loop once probably
      ;; costs about as much as searching 2000 chars.
      (setq offset 1000
            found nil
            pat (concat (if (eq selective-display t)
                            "\\(^\\|\^m\\)" "^")
                        (regexp-quote (car tag-info))))
      ;; The character position in the tags table is 0-origin.
      ;; Convert it to a 1-origin Emacs character position.
      (if startpos (setq startpos (1+ startpos)))
      ;; If no char pos was given, try the given line number.
      (or startpos
          (if line
              (setq startpos (progn (goto-char (point-min))
                                    (forward-line (1- line))
                                    (point)))))
      (or startpos
          (setq startpos (point-min)))
      ;; First see if the tag is right at the specified location.
      (goto-char startpos)
      (setq found (looking-at pat))
      (while (and (not found)
                  (progn
                    (goto-char (- startpos offset))
                    (not (bobp))))
        (setq found
              (re-search-forward pat (+ startpos offset) t)
              offset (* 3 offset)))	; expand search window
      (or found
          (re-search-forward pat nil t)
          (error "Rerun etags: `%s' not found in %s"
                 pat buffer-file-name)))
    ;; Position point at the right place
    ;; if the search string matched an extra Ctrl-m at the beginning.
    (and (eq selective-display t)
         (looking-at "\^m")
         (forward-char 1))
    (beginning-of-line)))

(defun anything-etags+-file-of-tag(&optional relative)
  (save-excursion
    (re-search-backward "\f\n\\([^\n]+\\),[0-9]*\n")
    (let ((str (buffer-substring (match-beginning 1) (match-end 1))))
      (expand-file-name str
                        (anything-etags+-file-truename default-directory))
      )))

(defun anything-etags+-find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
    (defun find-tags-file-r (path)
      "find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
             (possible-tags-file (concat parent "TAGS")))
        (cond
         ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
         ((string-match "^/TAGS\\|^[a-zA-Z]:/TAGS" possible-tags-file) nil)
         (t (find-tags-file-r (directory-file-name parent))))))
    (if (buffer-file-name)
        (catch 'found-it
          (find-tags-file-r (buffer-file-name)))
      (message "buffer is not visiting a file") nil)))

(defun anything-etags+-get-tag-files()
  "Get tag files."
  (if anything-etags+-use-xemacs-etags-p
      (let ((tags-build-completion-table nil))
        (buffer-tag-table-list))
    (let ((local-tag  (anything-etags+-find-tags-file)))
      (when local-tag
        (add-to-list 'tags-table-list (anything-etags+-find-tags-file)))
      (dolist (tag tags-table-list)
        (when (not (file-exists-p tag))
          (setq  tags-table-list (delete tag tags-table-list))))
      (mapcar 'tags-expand-table-name tags-table-list))))

(defun anything-etags+-rename-tag-file-buffer-maybe(buf)
  (with-current-buffer buf
    (if (string-match "^ \\*Anything" (buffer-name))
        buf
      (rename-buffer (concat" *Anything etags+:" (buffer-name) "*") t)
      ))buf)

(defun anything-etags+-get-tag-table-buffer (tag-file)
  "Get tag table buffer for a tag file."
  (when (file-exists-p tag-file)
    (let ((tag-table-buffer) (current-buf (current-buffer))
          (tags-revert-without-query t)
          (large-file-warning-threshold nil)
          (tags-add-tables t))
      (if anything-etags+-use-xemacs-etags-p
          (setq tag-table-buffer (get-tag-table-buffer tag-file))
        (visit-tags-table-buffer tag-file)
        (setq tag-table-buffer (find-buffer-visiting tag-file)))
      (set-buffer current-buf)
      (anything-etags+-rename-tag-file-buffer-maybe tag-table-buffer))))

(defun anything-etags+-get-available-tag-table-buffers()
  "Get tag table buffer for a tag file."
  (setq anything-etags+-tag-table-buffers
        (delete nil (mapcar 'anything-etags+-get-tag-table-buffer
                            (anything-etags+-get-tag-files)))))

(defun anything-etags+-get-candidates-with-cache-support()
  "for example when the `anything-pattern' is 'toString System pub'
   only 'toString' is treated as tagname,and
`anything-etags+-get-candidates-from-all-tag-file'
will search `toString' in all tag files. and the found
 candidates is stored in `anything-etags+-candidates-cache'
'toString' is stored in `anything-etags+-previous-matched-pattern'
so when the `anything-pattern' become to 'toString System public'
needn't search tag file again."
  (let ((pattern anything-etags+-untransformed-anything-pattern));;default use whole anything-pattern to search in tag files
    ;; first collect candidates using first part of anything-pattern
    (when (featurep 'anything-match-plugin)
      ;;for example  (amp-mp-make-regexps "boo far") -->("boo" "far")
      (setq pattern (car (amp-mp-make-regexps anything-etags+-untransformed-anything-pattern))))
    (unless (string-equal anything-etags+-previous-matched-pattern pattern)
      ;;          (setq candidates anything-etags+-candidates-cache)
      (setq anything-etags+-candidates-cache (anything-etags+-get-candidates-from-all-tag-file pattern))
      (setq anything-etags+-previous-matched-pattern pattern))
    anything-etags+-candidates-cache))


(defun anything-etags+-get-candidates-from-all-tag-file(first-part-of-anything-pattern)
  (let (candidates)
    (dolist (tag-table-buffer anything-etags+-tag-table-buffers)
      (setq candidates
            (append
             candidates
             (anything-etags+-get-candidates-from-tag-file
              first-part-of-anything-pattern tag-table-buffer))))
    candidates))

(defun anything-etags+-get-candidates-from-tag-file (tagname tag-table-buffer)
  "find tagname in tag-table-buffer. "
  (catch 'failed
    (let ((case-fold-search (anything-etags+-case-fold-search))
          tag-info tag-line src-file-name full-tagname
          tag-regex
          tagname-regexp-quoted
          candidates)
      (if (string-match "\\\\_<\\|\\\\_>[ \t]*" tagname)
          (progn
            (setq tagname (replace-regexp-in-string "\\\\_<\\|\\\\_>[ \t]*" ""  tagname))
            (setq tagname-regexp-quoted (regexp-quote tagname))
            (setq tag-regex (concat "^.*?\\(" "\^?\\(.+[:.']"  tagname-regexp-quoted "\\)\^A"
                                    "\\|" "\^?"  tagname-regexp-quoted "\^A"
                                    "\\|" "\\<"  tagname-regexp-quoted "[ \f\t()=,;]*\^?[0-9,]"
                                    "\\)")))
        (setq tagname-regexp-quoted (regexp-quote tagname))
        (setq tag-regex (concat "^.*?\\(" "\^?\\(.+[:.'].*"  tagname-regexp-quoted ".*\\)\^A"
                                "\\|" "\^?.*"  tagname-regexp-quoted ".*\^A"
                                "\\|" ".*"  tagname-regexp-quoted ".*[ \f\t()=,;]*\^?[0-9,]"
                                "\\)")))
      (with-current-buffer tag-table-buffer
        (modify-syntax-entry ?_ "w")
        (goto-char (point-min))
        (while (search-forward  tagname nil t) ;;take care this is not re-search-forward ,speed it up
          (beginning-of-line)
          (when (re-search-forward tag-regex (point-at-eol) 'goto-eol)
            (setq full-tagname (or (anything-etags+-match-string 2) tagname))
            (beginning-of-line)
            (save-excursion (setq tag-info (anything-etags+-etags-snarf-tag)))
            (re-search-forward "\\s-*\\(.*?\\)\\s-*\^?" (point-at-eol) t)
            (setq tag-line (anything-etags+-match-string 1))
            (setq tag-line (replace-regexp-in-string  "/\\*.*\\*/" "" tag-line))
            (setq tag-line (replace-regexp-in-string  "\t" (make-string tab-width ? ) tag-line))
            (end-of-line)
            ;;(setq src-file-name (etags-file-of-tag))
            (setq src-file-name (anything-etags+-file-of-tag))
            (let ((display)(real (list  src-file-name tag-info full-tagname)))
              (let ((tag-table-parent (anything-etags+-file-truename (file-name-directory (buffer-file-name tag-table-buffer)))))
                (when (string-match  tag-table-parent (anything-etags+-file-truename src-file-name))
                  (setq src-file-name (substring src-file-name (length  tag-table-parent)))))
              (if anything-etags+-use-short-file-name
                  (setq src-file-name (file-name-nondirectory src-file-name)))
              (setq display (concat tag-line
                                    (or (ignore-errors
                                          (make-string (- (window-width) 6
                                                          (string-width tag-line)
                                                          (string-width src-file-name))
                                                       ? )) "")
                                    src-file-name))
              (add-to-list 'candidates (cons display real)))))
        (modify-syntax-entry ?_ "_"))
      candidates)))

(defun anything-etags+-find-tag(candidate)
  "Find tag that match CANDIDATE from `tags-table-list'.
   And switch buffer and jump tag position.."
  (let ((src-file-name (car candidate))
        (tag-info (nth 1 candidate))
        (tagname (nth 2 candidate))
        src-file-buf)
    (when (file-exists-p src-file-name)
      ;; Jump to tag position when
      ;; tag file is valid.
      (setq src-file-buf (find-file src-file-name))
      (anything-etags+-goto-tag-location  tag-info)

      (beginning-of-line)
      (when (search-forward tagname (point-at-eol) t)
        (goto-char (match-beginning 0))
        (setq tagname (thing-at-point 'symbol))
        (beginning-of-line)
        (search-forward tagname (point-at-eol) t)
        (goto-char (match-beginning 0))
        (when(and anything-etags+-highlight-tag-after-jump
                  (not anything-in-persistent-action))
          (anything-etags+-highlight (match-beginning 0) (match-end 0))))

      (when (and anything-in-persistent-action ;;color
                 (fboundp 'anything-match-line-color-current-line))
        (anything-match-line-color-current-line))

      (if anything-in-persistent-action ;;prevent from opening too much buffer in persistent action
          (progn
            (if (and previous-opened-buffer-in-persistent-action
                     (not (equal previous-opened-buffer-in-persistent-action src-file-buf)))
                (kill-buffer  previous-opened-buffer-in-persistent-action))
            (setq previous-opened-buffer-in-persistent-action src-file-buf))
        (setq previous-opened-buffer-in-persistent-action nil)))))

(defun anything-c-etags+-goto-location (candidate)
  (unless anything-in-persistent-action
    (when (and
           (not (ring-empty-p anything-etags+-tag-marker-ring))
           anything-etags+-current-marker-in-tag-marker-ring
           (not (equal anything-etags+-current-marker-in-tag-marker-ring (ring-ref anything-etags+-tag-marker-ring 0))))
      (while (not (ring-empty-p anything-etags+-tag-marker-ring ))
        (ring-remove anything-etags+-tag-marker-ring)
        ))
    ;;you can use `anything-etags+-history' go back
    (ring-insert anything-etags+-tag-marker-ring (point-marker))
    (setq anything-etags+-current-marker-in-tag-marker-ring (point-marker))
    )
  (anything-etags+-find-tag candidate);;core func.
  )

(defun anything-etags+-select-internal(init-pattern prompt)
  (run-hooks 'anything-etags+-select-hook)
  (anything '(anything-c-source-etags+-select)
            ;; Initialize input with current symbol
            init-pattern  prompt nil))

;;;###autoload
(defun anything-etags+-select()
  "Tag jump using etags and `anything'.
If SYMBOL-NAME is non-nil, jump tag position with SYMBOL-NAME."
  (interactive)
  (let ((anything-execute-action-at-once-if-one t)
        (anything-candidate-number-limit nil)
        (anything-idle-delay anything-idle-delay-4-anything-etags+))
    (anything-etags+-select-internal nil "Find Tag(require 3 char): ")))

;;;###autoload
(defun anything-etags+-select-at-point()
  "Tag jump with current symbol using etags and `anything'."
  (interactive)
  (let ((anything-execute-action-at-once-if-one t)
        (anything-candidate-number-limit nil)
        (anything-idle-delay 0))
    ;; Initialize input with current symbol
    (anything-etags+-select-internal
     (concat "\\_<" (thing-at-point 'symbol) "\\_>"
             (if (featurep 'anything-match-plugin) " "))
     "Find Tag: ")))

;;;###autoload
(defun anything-etags+-select-one-key (&optional args)
  "you can bind this to `M-.'"
  (interactive "P")
  (if args
      (anything-etags+-select)
    (anything-etags+-select-at-point)))

;;;###autoload
(defvar anything-c-source-etags+-select
  '((name . "Etags+")
    (init . anything-etags+-get-available-tag-table-buffers)
    (candidates . anything-etags+-get-candidates-with-cache-support)
    (volatile);;candidates
    (pattern-transformer (lambda (anything-pattern)
                           (setq anything-etags+-untransformed-anything-pattern anything-pattern)
                           (regexp-quote (replace-regexp-in-string "\\\\_<\\|\\\\_>" ""  anything-pattern))))
    (requires-pattern  . 3);;need at least 3 char
    (delayed);; (setq anything-idle-delay-4-anthing-etags+ 1)
    (action ("Goto the location" . anything-c-etags+-goto-location))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Go Back and Go Forward

;;util func

;;(anything-etags+-is-marker-avaiable (ring-ref anything-etags+-tag-marker-ring 0))
(defun anything-etags+-is-marker-available(marker)
  "return nil if marker is nil or  in dead buffer ,
   return marker if it is live"
  (if (and marker
           (markerp marker)
           (marker-buffer marker))
      marker
    ))
;;; func about history
(defun anything-etags+-history-get-candidate-from-marker(marker)
  "genernate candidate from marker candidate= (display . marker)."
  (let ((buf (marker-buffer marker))
        (pos (marker-position marker))
        line-num line-text candidate display
        file-name empty-string)
    (when  buf
      ;;      (save-excursion
      ;;        (set-buffer buf)
      (with-current-buffer buf
        (if anything-etags+-use-short-file-name
            (setq file-name (or (file-name-nondirectory (buffer-file-name)) (buffer-name)))
          (setq file-name (or (buffer-file-name) (buffer-name))))
        (goto-char pos)
        (setq line-num (int-to-string (count-lines (point-min) pos)))
        (setq line-text (buffer-substring-no-properties (point-at-bol)(point-at-eol)))
        (setq line-text (replace-regexp-in-string "^[ \t]*\\|[ \t]*$" "" line-text))
        (setq line-text (replace-regexp-in-string  "/\\*.*\\*/" "" line-text))
        (setq line-text (replace-regexp-in-string  "\t" (make-string tab-width ? ) line-text)))
      ;;          )
      (if (equal marker anything-etags+-current-marker-in-tag-marker-ring)
          ;;this one will be preselected
          (setq line-text (concat line-text "\t")))
      (setq empty-string  (or (ignore-errors
                                (make-string (- (window-width)  6
                                                (string-width  line-num)
                                                (string-width file-name)
                                                (string-width line-text))
                                             ? )) "  "))
      (setq display (concat line-text empty-string
                            file-name ":[" line-num "]"))
      (setq candidate  (cons display marker)))))

;;(anything-etags+-history-get-candidate-from-marker (ring-remove (ring-copy anything-etags+-tag-marker-ring)))
;; (ring-remove )
;; (ring-length anything-etags+-tag-marker-ring)
;; (anything-etags+-history-get-candidates)
;; time_init
(defun anything-etags+-history-candidates()
  "generate candidates from `anything-etags+-tag-marker-ring'.
  and remove unavailable markers in `anything-etags+-tag-marker-ring'"
  (let ((candidates (mapcar 'anything-etags+-history-get-candidate-from-marker (ring-elements anything-etags+-tag-marker-ring))))
    ;; (when anything-etags+-history-tmp-marker
    ;;   (setq candidates (append (list (anything-etags+-history-get-candidate-from-marker anything-etags+-history-tmp-marker)) candidates)))
    candidates))

(defun anything-etags+-history-init()
  "remove #<marker in no buffer> from `anything-etags+-tag-marker-ring'.
   and remove those markers older than #<marker in no buffer>."
  (let ((tmp-marker-ring))
    (while (not (ring-empty-p anything-etags+-tag-marker-ring))
      (anything-aif (anything-etags+-is-marker-available (ring-remove anything-etags+-tag-marker-ring 0))
          (setq tmp-marker-ring (append tmp-marker-ring (list it)));;new item first
        (while (not (ring-empty-p anything-etags+-tag-marker-ring));;remove all old marker
          (ring-remove anything-etags+-tag-marker-ring))))
    ;;reinsert all available marker to `anything-etags+-tag-marker-ring'
    (mapcar (lambda(marker) (ring-insert-at-beginning anything-etags+-tag-marker-ring marker)) tmp-marker-ring))
  ;; (when (not (ring-empty-p anything-etags+-tag-marker-ring))
  ;;   (let ((last-marker-in-anything-etags+-tag-marker-ring (ring-ref  anything-etags+-tag-marker-ring 0)))
  ;;     (when (and (equal anything-etags+-current-marker-in-tag-marker-ring  last-marker-in-anything-etags+-tag-marker-ring)
  ;;                (or (not (equal (marker-buffer last-marker-in-anything-etags+-tag-marker-ring) (current-buffer)))
  ;;                    (> (abs (- (marker-position last-marker-in-anything-etags+-tag-marker-ring) (point))) 350)))
  ;;       (setq anything-etags+-history-tmp-marker (point-marker)))))
  )

(defun anything-etags+-history-clear-all(&optional candidate)
  "param `candidate' is unused."
  (while (not (ring-empty-p anything-etags+-tag-marker-ring));;remove all marker
    (ring-remove anything-etags+-tag-marker-ring)))


;;;###autoload
(defun anything-etags+-history-go-back()
  "Go Back. "
  (interactive)
  (anything-etags+-history-init)
  (when (and
         (anything-etags+-is-marker-available anything-etags+-current-marker-in-tag-marker-ring)
         (ring-member anything-etags+-tag-marker-ring anything-etags+-current-marker-in-tag-marker-ring))
    (let* ((next-marker (ring-next anything-etags+-tag-marker-ring anything-etags+-current-marker-in-tag-marker-ring)))
      (anything-etags+-history-go-internel next-marker)
      (setq anything-etags+-current-marker-in-tag-marker-ring next-marker))))

;;;###autoload
(defun anything-etags+-history-go-forward()
  "Go Forward. "
  (interactive)
  (anything-etags+-history-init)
  (when (and
         (anything-etags+-is-marker-available anything-etags+-current-marker-in-tag-marker-ring)
         (ring-member anything-etags+-tag-marker-ring anything-etags+-current-marker-in-tag-marker-ring))
    (let* ((previous-marker (ring-previous anything-etags+-tag-marker-ring anything-etags+-current-marker-in-tag-marker-ring)))
      (anything-etags+-history-go-internel previous-marker)
      (setq anything-etags+-current-marker-in-tag-marker-ring previous-marker))))

(defun anything-etags+-history-go-internel (candidate-marker)
  "Go to the location depend on candidate."
  (let ((buf (marker-buffer candidate-marker))
        (pos (marker-position candidate-marker)))
    (when buf
      (switch-to-buffer buf)
      (set-buffer buf)
      (goto-char pos))))

;; (action .func),candidate=(Display . REAL), now in this func
;; param candidate is 'REAL' ,the marker.
(defun anything-etags+-history-action-go(candidate)
  "List all history."
  (anything-etags+-history-go-internel candidate)
  (unless  anything-in-persistent-action
    (setq anything-etags+-current-marker-in-tag-marker-ring candidate)
    ;; (when anything-etags+-history-tmp-marker
    ;;   (ring-insert anything-etags+-tag-marker-ring anything-etags+-history-tmp-marker)
    ;;   (setq anything-etags+-history-tmp-marker nil))
    )
  (when (and anything-in-persistent-action ;;color
             (fboundp 'anything-match-line-color-current-line))
    (anything-match-line-color-current-line)))

(defvar anything-c-source-etags+-history
  '((name . "Etags+ History: ")
    (header-name .( (lambda (name) (concat name "`RET': Go ,`C-z' Preview. `C-e': Clear all history."))))
    (init .  anything-etags+-history-init)
    (candidates . anything-etags+-history-candidates)
    ;;        (volatile) ;;maybe needn't
    (action . (("Go" . anything-etags+-history-action-go)
               ("Clear all history" . anything-etags+-history-clear-all)))))

;;;###autoload
(defun anything-etags+-history()
  "show all tag historys using `anything'"
  (interactive)
  (let ((anything-execute-action-at-once-if-one t)
        (anything-quit-if-no-candidate
         (lambda () (message "No history record in `anything-etags+-tag-marker-ring'"))))
    (anything '(anything-c-source-etags+-history)
              ;; Initialize input with current symbol
              ""  nil nil "\t")))

(provide 'anything-etags+)
;;;anything-etags+.el ends here.
