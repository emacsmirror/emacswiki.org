;;; html-parse.el --- html parser for Emacs

;; Copyright (C) 2009  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: html parse

;; This program is free software; you can redistribute it and/or modify
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

;; parse HTML(XHTML) to S-expression.

;; This library requires xml-parse.el.
;; - ftp://ftp.newartisans.com/pub/emacs/xml-parse.el

;;; Code:

(require 'xml-parse)

;; Variables:

(defvar html-parse-single-tags
  '("base" "link" "meta" "img" "br" "area" "param"
    "hr" "col" "option" "input" "wbr")
  "List of empty element tags.")


;; Functions:

(defun html-parse-read ()
  (save-excursion
    (save-restriction
      (cdr (html-parse-read-iter)))))

;; from xml-parse.el's xml-parse-read
(defun html-parse-read-iter ()
  (let ((beg (search-forward "<" nil t))
        after)
    ;; skip comments or custom tags
    (while (and beg
                (memq (setq after (char-after)) '(?! ??)))
      (xml-parse-skip-tag)
      (setq beg (search-forward "<" nil t)))
    (when beg
      (if (eq after ?/)
          ;; point is end tag.
          (progn
            (search-forward ">")
            ;; 1- beg is point of '<'
            (cons (1- beg) (buffer-substring (1+ beg) (1- (point)))))
        ;; point is start tag.
        (skip-chars-forward "^ \t\n/>")
        (cons
         (1- beg)
         (progn
           (setq after (point))
           (skip-chars-forward " \t\n")
           (let* ((tag (buffer-substring beg after))
                  (single (or (eq (char-after) ?/) ; example: <foo />
                              (member (downcase tag) html-parse-single-tags)))
                  attrs data-beg data)
             ;; handle the attribute list, if present
             (cond
              ;; no attributes
              ((eq (char-after) ?\>)
               (forward-char 1))
              ;; single or attribute is found.
              (t
               (let* ((attrs    (list t))
                      (lastattr attrs)
                      (end      (search-forward ">")))
                 (goto-char after)
                 (while (re-search-forward
                         "\\([^ \t\n=]+\\)=\"\\([^\"]+\\)\"" end t)
                   (let ((attr (cons (match-string 1) (match-string 2))))
                     (setcdr lastattr (list attr))
                     (setq   lastattr (cdr  lastattr))))
                 (goto-char end)
                 (when (cdr attrs)
                   (setq tag (cons tag (cdr attrs))
                         single (or single (eq (char-before (1- end)) ?/)))))))
             ;; tag: (tag_name (attr1 . val1) (attr2 . val2) ...) or (tag)
             ;; return the tag and its data
             (if single
                 (list tag)
               (setq tag (list tag))
               (let ((data-beg (point))
                     (tag-end  (last tag)))
                 (while (and (setq data (html-parse-read-iter))
                             (not (stringp (cdr data))))
                   (setq tag-end (xml-parse-concat data-beg
                                                   (car data)
                                                   tag-end)
                         data-beg (point))
                   (setcdr tag-end (list (cdr data)))
                   (setq tag-end (cdr tag-end)))
                 (xml-parse-concat data-beg (or (car data) (point-max))
                                   tag-end)
                 tag)))))))))


(provide 'html-parse)

;; Local Variables:
;; Coding: iso-2022-7bit
;; End:

;;; html-parse.el ends here
