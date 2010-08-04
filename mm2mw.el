;;; mm2mw.el --- A MoinMoin 1.2.x -> MediaWiki 1.7.x converter

;;; Copyright: (C) 2008, 2009 Charles Sebold
;; 
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public License
;;     along with GNU Emacs; if not, write to the Free Software
;;     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;     02110-1301 USA
;;
;; Latest version should be available at:
;;    <URL:http://www.emacswiki.org/emacs/MoinMoinToMediaWiki>

;;; Commentary:

;; To use this, just find the base name of your MoinMoin Wiki and the
;; base name of your new MediaWiki installations, and feed them to
;; mm2mw-wiki.  Tested only on Emacs 22, requires at least a url library
;; with the ability to do HTTP POSTs.  Check the *Messages* buffer to
;; see how it did, per page.

;; My MoinMoin wiki was v1.2.x, so I have absolutely no idea if it will
;; work on later versions.  This might give somebody a good start,
;; though.  I don't claim that it supports everything; it definitely
;; does not handle attachments correctly.  There are probably better
;; ways to do everything but I built this in a couple of days from
;; scratch for my own purposes, and if God is good to me I will not have
;; to do it again.

;; I ripped the wikiname regexp from oddmuse.el and the url HTTP POST
;; code from an example on EmacsWiki; beyond that I wrote this on my
;; own.

;;; Code:

(require 'url)

(defun mm2mw-convert-wikiname-chars (old-string)
  (with-temp-buffer
    (insert old-string)
    (goto-char (point-min))
    (while (re-search-forward "_2f" nil t)
      (replace-match "/" t t))
    (goto-char (point-min))
    (while (re-search-forward "_2d" nil t)
      (replace-match "-" t t))
    (buffer-string)))

(defun mm2mw-convert-wikiname (my-string &optional underscore)
  "Convert CamelCaps MY-STRING into MediaWiki wiki name."
  (save-match-data
    (let ((my-case-fold case-fold-search))
      (setq case-fold-search nil)
      (prog1
          (concat (if underscore "" "[[")
                  (mapconcat 'identity
                             (let ((my-list nil)
                                   (this-word nil)
                                   (last-char "")
                                   (characters (split-string
                                                (mm2mw-convert-wikiname-chars
                                                 my-string) "" t)))
                               (dolist (i characters)
                                 (if (and (string-match "[A-Z]" i)
                                          (not (string-match "[A-Z]"
                                                             last-char))
                                          (not (string-match "/" last-char)))
                                     (progn
                                       (if my-list
                                           (setq my-list
                                                 (append my-list
                                                         (list (apply 'concat this-word))))
                                         (setq my-list
                                               (list (apply 'concat this-word))))
                                       (setq this-word (list i)))
                                   (setq this-word (append this-word (list
                                                                      i))))
                                 (setq last-char i))
                               (setq case-fold-search default-case-fold-search)
                               (cdr (append my-list (list (apply 'concat
                                                                 this-word)))))
                             (if underscore "_" " ")) (if underscore "" "]]"))
        (setq case-fold-search my-case-fold)))))

(defun mm2mw-convert-table (my-string)
  "Convert MoinMoin table MY-STRING to MediaWiki table."
  (save-match-data
    (with-temp-buffer
      (insert my-string)
    ; beginning of row
      (goto-char (point-min))
      (while (re-search-forward "^ *||" nil t)
        (replace-match "|"))
    ; end of row
      (goto-char (point-min))
      (while (re-search-forward "|| *$" nil t)
        (replace-match ""))
    ; row dividers
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
        (replace-match "\000e0x\000|-\000e0x\000"))
    ; column dividers
      (goto-char (point-min))
      (while (re-search-forward "||" nil t)
        (replace-match "\000e0x\000|"))
    ; beginning of table
      (goto-char (point-min))
      (insert "\000e0x\000{| border=\"1\"\000e0x\000")
    ; end of table
      (while (progn
               (goto-char (point-max))
               (forward-char -1)
               (looking-at "\n"))
        (replace-match ""))
      (goto-char (point-max))
      (insert "\000e0x\000|}")
    ; return result
      (buffer-string))))

(defun mm2mw ()
  "Convert current buffer contents from MoinMoin markup to MediaWiki markup."
  (interactive)
  (setq case-fold-search nil)
  (let ((pres (make-hash-table :test 'eql)))
    ; predefined token for a single carriage return is \000e0x\000
    (puthash (hash-table-count pres) "\n" pres)
    (save-excursion
      ; one-line preformatted
      (goto-char (point-min))
      (while (re-search-forward "{{{\\([^\n]+?\\)}}}" nil t)
        (replace-match (concat "<tt>" (match-string 1) "</tt>") t t))
      ; multiline preformatted
      (goto-char (point-min))
      (while (re-search-forward "{{{" nil t)
        (replace-match "")
        (let ((start (point))
              (end (progn
                     (if (search-forward "}}}" nil t)
                         (search-backward "}}}" nil t)))))
          (when end
            (replace-match "")
            (puthash (hash-table-count pres)
                     (concat "<pre>"
                             (buffer-substring-no-properties start end)
                             "</pre>")
                     pres)
            (delete-region start end)
            (goto-char start)
            (insert (format "\000e%dx\000" (- (hash-table-count pres) 1))))))
      ; tables
      (goto-char (point-min))
      (while (re-search-forward "^ *||" nil t)
        (let* ((start (save-excursion
                        (beginning-of-line)
                        (point)))
               (end (save-match-data
                      (while
                          (and (condition-case nil
                                   (progn
                                     (forward-line 1)
                                     (beginning-of-line)
                                     t)
                                 (error nil))
                               (looking-at "^ *||")))
                      (- (point) 1)))
               (table (buffer-substring-no-properties start end)))
          (goto-char start)
          (delete-region start end)
          (insert (mm2mw-convert-table table))))
      ; tokenize URL links behind text
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(?:http\\|ftp\\|mailto\\):[^\000[]+?\\]" nil t)
        (puthash (hash-table-count pres) (match-string 0) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres) 1))
                       t))
      ; attachments
      (goto-char (point-min))
      (while (re-search-forward "attachment:\\(.*?\\)\\([ \n]\\)" nil t)
        (replace-match (concat "[[" (match-string 1) "]]" (match-string 2)) t t))
      ; automatic links (don't wiki them, tokenize them to get by)
      (goto-char (point-min))
      (while (re-search-forward "\\(?:http\\|ftp\\|mailto\\|file\\):[^ \n]+ "
                                nil t)
        (puthash (hash-table-count pres) (match-string 0) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres) 1))
                       t))
      ; ordered lists
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\n\\|^\\)\\( +\\)\\(?:[0-9]+\\|[A-Za-z]\\)\\. " nil t)
        (replace-match (concat "\000e0x\000"
                               (make-string (length (match-string 1)) ?#)
                               " ")))
      ; #pragma lines - remove them
      (goto-char (point-min))
      (while (re-search-forward "^#pragma .*?\n" nil t)
        (replace-match ""))
      ; Anchor and search lines - remove them
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(?:Anchor\\|PageList\\|FullSearch\\).*?\\]\\]" nil t)
        (replace-match ""))
      ; horizontal rules
      (goto-char (point-min))
      (while (re-search-forward "\n----+\n" nil t)
        (puthash (hash-table-count pres) (match-string 0) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres) 1))))
      ; headings and subheadings
      (goto-char (point-min))
      (while (re-search-forward "\n *\\(=[^\n]+?=\n\\)" nil t)
        (puthash (hash-table-count pres) (concat "\n" (match-string 1)) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres) 1))
                       t))
      ; definition lists (dt::dl -> ;dt\n:dl)
      (goto-char (point-min))
      (while (re-search-forward "^\\( *\\)\\(.+?\\)::\\(.*\\)$" nil t)
        (replace-match (concat (match-string 1) ";" (match-string 2)
                               "\000e0x\000:" (match-string 3)) t t))
      ; multiple carriage returns
      (goto-char (point-min))
      (while (re-search-forward "\n\\{2,\\}" nil t)
        (puthash (hash-table-count pres) (match-string 0) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres) 1))))
      ; end preformatted areas (shouldn't be required)
      (goto-char (point-min))
      (while (re-search-forward "}}}" nil t)
        (replace-match "</pre>"))
      ; unordered lists
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\|x\000\\)\\( +\\)\\* " nil t)
        (replace-match (concat (match-string 1)
                               (make-string (length (match-string 2)) ?*)
                               " ")))
      ; six apostrophes to keep something from wikifying
      (goto-char (point-min))
      (while (re-search-forward "''''''" nil t)
        (replace-match ""))
      ; Turn special wiki links from MoinMoin to MediaWiki style
      (goto-char (point-min))
      (while (re-search-forward "\\[\"\\(.*?\\)\"\\]" nil t)
        (puthash (hash-table-count pres) (match-string 1) pres)
        (replace-match (format "[[\000e%dx\000]]" (- (hash-table-count pres)
                                                     1)) t))
      ; Catch backslash-prefix non-wiki names (like in Windows filespecs)
      (goto-char (point-min))
      (while (re-search-forward
              "\\\\\\<\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\(?:[0-9]+\\|[A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\)\\>"
              nil t)
        (puthash (hash-table-count pres) (match-string 0) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres)
                                                     1)) t))
      ; Catch bang-prefix non-wiki names
      (goto-char (point-min))
      (while (re-search-forward
              "!\\<\\([A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\(?:[0-9]+\\|[A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\)\\>"
              nil t)
        (puthash (hash-table-count pres) (match-string 1) pres)
        (replace-match (format "\000e%dx\000" (- (hash-table-count pres)
                                                     1)) t))
      ; convert standard wiki names, breaking on capital letters
      (goto-char (point-min))
      (while (re-search-forward
              "\\<[A-Z\xc0-\xde]+[a-z\xdf-\xff]+\\(?:[0-9]+\\|[A-Z\xc0-\xde]+[a-z\xdf-\xff]*\\)+\\>"
              nil t)
        (replace-match (mm2mw-convert-wikiname (match-string 0)) t))
      ; remove unnecessary carriage returns
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^\n*# ]\\)" nil t)
        (replace-match (concat " " (match-string 1))))
      ; untokenize tokens
      (dotimes (i 3)
        (goto-char (point-min))
        (while (re-search-forward "\000e\\([0-9]+\\)x\000" nil t)
        ; we're going to avoid the preformatted stuff for now
          (let ((my-token (gethash (string-to-number (match-string 1)) pres)))
            (unless (and (> (length my-token) 5)
                         (string= (substring my-token 0 5) "<pre>"))
              (replace-match my-token t t)))))
      ; indent
      (goto-char (point-min))
      (while (re-search-forward "^\\( +\\)" nil t)
        (replace-match (make-string (length (match-string 1)) ?:)))
      ; untokenize preformatted tokens
      (goto-char (point-min))
      (while (re-search-forward "\000e\\([0-9]+\\)x\000" nil t)
        (replace-match (gethash (string-to-number (match-string 1)) pres) t t)
        (goto-char (point-min)))
      ; put carriage returns around preformatted text
      (goto-char (point-min))
      (while (re-search-forward "\\([^\n]\\)<pre>" nil t)
        (replace-match (concat (match-string 1) "\n<pre>")))
      ; Mixed lists (just the one level though)
      (goto-char (point-min))
      (while (re-search-forward "^#" nil t)
        (while (progn
                 (forward-line 1)
                 (beginning-of-line)
                 (looking-at "[*]"))
          (replace-match "#")))
      ; category fix (what a hack)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\[\\[Category\\) \\(.*?\\]\\]\\)" nil t)
        (replace-match (concat (match-string 1) ":" (match-string 2))))
      ; more local-only changes
      (goto-char (point-min))
      (while (re-search-forward "Category:\\(Vms\\|Cph\\|Dns\\|Lhm\\|Ups\\)" nil t)
        (replace-match (concat "Category:" (upcase (match-string 1))) t t)))
    (setq case-fold-search default-case-fold-search)))

(defun mm2mw-url (my-mm-base-url my-mw-base-url page-name)
  "Convert the contents of MY-MM-BASE-URL to MY-MW-BASE-URL."
  (interactive "sMoinMoin URL? \nsMediaWiki editing URL? \nsPage? ")
  (catch 'mm2mw-out
    (save-excursion
      (switch-to-buffer (url-retrieve-synchronously
                         (concat my-mm-base-url "/" page-name
                                 "?action=edit")))
      (goto-char (point-min))
      (if (re-search-forward "<textarea name=\"savetext\"" nil t)
          (progn
            (re-search-forward ">" nil t)
            (let* ((start (point))
                   (wikitext (progn
                               (re-search-forward "</textarea>" nil t)
                               (re-search-backward "</textarea>" nil t)
                               (buffer-substring-no-properties start (point))))
                   (newtext (with-temp-buffer
                              (insert wikitext)
                              (mm2mw)
                              (buffer-string)))
                   (new-page-name (mm2mw-convert-wikiname page-name t))
                   (new-page-url (concat my-mw-base-url "/"
                                         new-page-name))
                   (new-edit-url (concat my-mw-base-url "?title="
                                         new-page-name
                                         "&action=edit"))
                   (new-submit-url (concat my-mw-base-url "?title="
                                           new-page-name
                                           "&action=submit"))
                   (form-values (mm2mw-process-mw-edit new-edit-url)))
              (goto-char (point-min))
              (if (re-search-forward (regexp-quote my-mw-base-url) nil t)
                  (throw 'mm2mw-out (not (kill-buffer nil)))
                (mm2mw-submit-mw-edit form-values newtext new-submit-url)
                (mm2mw-update-mm-edit (concat my-mm-base-url "/" page-name)
                                      wikitext new-page-url))))
        (throw 'mm2mw-out (not (kill-buffer nil))))
      (kill-buffer nil))))

(defun mm2mw-process-mw-edit (my-url)
  (let ((acc '()))
    (save-excursion
      (switch-to-buffer (url-retrieve-synchronously my-url))
      (goto-char (point-min))
      (while (re-search-forward
              "<input type='hidden' value=\"\\(.*?\\)\" name=\"\\(.*?\\)\""
              nil t)
        (setq acc (append (list (cons (match-string 2) (match-string 1))) acc)))
      (kill-buffer nil))
    acc))

(defun mm2mw-update-mm-edit (my-url old-text new-url)
  (let ((acc '()))
    (goto-char (point-min))
    (while (re-search-forward
            "<input type=\"hidden\" name=\"\\(.*?\\)\" value=\"\\(.*?\\)\""
            nil t)
      (setq acc (append (list (cons (match-string 1) (match-string 2))) acc)))
    (setq acc (append (list (cons "button_save" "Save Changes"))
                      (list (cons "rstrip" "1"))
                      (list (cons "savetext"
                                  (concat "This page has moved to "
                                          new-url " by mm2mw.el\n\n"
                                          old-text))) acc))
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data
           (mapconcat (lambda (arg)
                        (concat (url-hexify-string (car arg))
                                "="
                                (url-hexify-string (cdr arg))))
                      acc "&")))
      (kill-buffer (url-retrieve-synchronously (concat my-url "#preview"))))))

(defun mm2mw-submit-mw-edit (form-values text my-url)
  (setq form-values (append (list (cons "wpTextbox1" text)) form-values))
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (mapconcat (lambda (arg)
                      (concat (url-hexify-string (car arg))
                              "="
                              (url-hexify-string (cdr arg))))
                    form-values
                    "&")))
    (kill-buffer (url-retrieve-synchronously my-url))))

(defun mm2mw-wiki (old-wiki-url new-wiki-url)
  "Front end for `mm2mw' which will grab all the pages available and convert
them."
  (interactive "sMoinMoin Wiki base URL? \nsMediaWiki base URL? ")
  (let ((my-old-wiki-url (if (string=
                              (car (last (split-string old-wiki-url "" t)))
                              "/")
                             old-wiki-url
                           (concat old-wiki-url "/"))))
    (save-excursion
      (switch-to-buffer
       (url-retrieve-synchronously
        (concat my-old-wiki-url "TitleIndex")))
      (goto-char (point-min))
      (delete-region (point-min) (progn
                                   (re-search-forward "XML title index" nil t)
                                   (re-search-forward "<a href=" nil t)
                                   (forward-char -8)
                                   (point)))
      (delete-region (progn
                       (re-search-forward "</p>" nil t)
                       (forward-line -1)
                       (point)) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "^.*?<a href=\"/.*?/" nil t)
        (replace-match my-old-wiki-url t t))
      (goto-char (point-min))
      (while (re-search-forward "\".*$" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^http://.*/\\(.*\\)$" nil t)
        (if (save-match-data
              (mm2mw-url old-wiki-url new-wiki-url (match-string 1)))
            (message "mm2mw: converted %s." (match-string 1))
          (message "mm2mw: failed to convert %s." (match-string 1)))))))

(provide 'mm2mw)
