;;; qq-mode.el --- Modes for creating valid XHTML 1.0 Transitional pages.

;; Author: Rhoderick Espineda <ekz@burlezke.com>
;; Version: 1.0.3
;; Keywords: XHTML
;; URL: http://www.emacswiki.org/elisp/qq-mode.el
;; Date: March 22, 2004

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Makes for creating valid XHTML 1.0 Transitional web pages.

;;; Acknowledgement:

;; This code is derived from NCSA html-mode.el made by MarcAndreessen <marca@ncsa.uiuc.edu>

;; Use M-x qq-mode RET  
;; Check the code below for available keybindings.

;;; Code:

(defvar qq-mode-map nil)

(if qq-mode-map()
        (setq qq-mode-map (make-sparse-keymap))
          (define-key qq-mode-map "\t" 'tab-to-tab-stop)
          (define-key qq-mode-map "\C-ca" 'qq-skeleton)
          (define-key qq-mode-map "\C-cs" 'qq-style)

          (define-key qq-mode-map "\C-cp" 'qq-p)
          (define-key qq-mode-map "\C-cb" 'qq-b)
          (define-key qq-mode-map "\C-cn" 'qq-br)
          (define-key qq-mode-map "\C-cl" 'qq-hl)

          (define-key qq-mode-map "\C-ci" 'qq-img)
          (define-key qq-mode-map "\C-cf" 'qq-font)
          (define-key qq-mode-map "\C-ch" 'qq-href)
          (define-key qq-mode-map "\C-ct" 'qq-table)

          (define-key qq-mode-map "\C-cm" 'qq-tm)
          (define-key qq-mode-map "\C-c&" 'qq-amp)
          (define-key qq-mode-map "\C-cc" 'qq-copy)
          (define-key qq-mode-map "\C-c*" 'qq-bullet)
          (define-key qq-mode-map "\C-c<" 'qq-less-than)
          (define-key qq-mode-map "\C-c>" 'qq-greater-than)

          (define-key qq-mode-map "\C-cr" 'qq-referer)
          (define-key qq-mode-map "\C-cv" 'qq-validator))

(defun qq-skeleton(title)
(interactive "sTITLE: ")
(let ((start (point)))
(insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n\t\t")
(insert "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n\n")
(insert "<html>\n\n<head>\n\t")
(insert "<title>" title "</title>\n\t")
(insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\'iso-8859-1\'\" />")
(insert "\n\t<meta name=\"webmaster\" content=\"YOUR-NAME\" />")
(insert "\n\t<link rel=\"alternate\" type=\"application/rss+xml\" href=\"YOUR-RSS\" />")
(insert "\n\t<link rel=\"stylesheet\" type=\"text/css\" href=\"YOUR-CSS\" />")
(insert "\n\t<link rel=\"icon\" type=\"image/x-icon\" href=\"YOUR-ICON\" />\n")
(insert "</head>\n\n<body>\n\t<!-- YOUR-CONTENT -->\n")
(insert "</body>\n\n</html>")))

(defun qq-style()
    (interactive)
    (insert "<style type=\"text/css\">\n\t")
    (insert "body {\n\t\t")
    (insert "color: #000000; background: #ffffff;\n\t\t")
    (insert "font-family: lucidatypewriter, georgia, sans-serif;\n\t}\n")
    (insert "</style>"))

(defun qq-open-field (tag)
        (let ((start (point)))
                (insert "<" tag ">")
                (setq start (point))
                (insert "</" tag ">")
                (push-mark)
                (goto-char start)))

(defun qq-p()
        (interactive)
                (qq-open-field "p"))

(defun qq-b()
     (interactive)
          (qq-open-field "b"))

(defun qq-br()
     (interactive)
          (insert "<br />"))

(defun qq-hl()
     (interactive)
          (insert "<hl />"))

(defun qq-img (href)
        (interactive "sURL-img: ")
        (let ((start (point)))
                (insert "<img src=\"" href "\" border=\"0\" alt=\"\" id=\"\" />")))

(defun qq-font()
     (interactive)
          (insert "<font color=\"#000000\">YOUR-TEXT</font>"))

(defun qq-href (href title)
     (interactive "sURL-href: \nsURL-title: ")
     (let ((start (point)))
          (insert "<a href=\"" href "\" border=\"0\" alt=\"\">"  title "</a>")))

(defun qq-table()
     (interactive)
          (insert "<table border=\"1\" width=\"100%\" cellspacing=\"0\" cellpadding=\"0\">\n")
          (insert "<tbody>\n<tr>\n\t<td>A-1</td>\n\t<td>A-2</td>\n</tr>\n<tr>\n\t")
          (insert "<td>B-1</td>\n\t<td>B-2</td>\n</tr>\n</tbody>\n</table>"))

(defun qq-bullet()
     (interactive)
          (insert "&#149;"))

(defun qq-amp()
        (interactive)
                (insert "&amp;"))

(defun qq-copy()
     (interactive)
          (insert "&copy;"))

(defun qq-tm()
     (interactive)
          (insert "&trade;"))

(defun qq-less-than()
     (interactive)
          (insert "&lt;"))

(defun qq-greater-than()
     (interactive)
          (insert "&gt;"))

(defun qq-referer()
     (interactive)
          (insert "<p>\n\t")
          (insert "<a href=\"http://validator.w3.org/check/referer\"><img\n\t\t")
          (insert "src=\"http://www.w3.org/Icons/valid-xhtml10\"\n\t\talt=\"Valid XHTML 1.0\"")
          (insert "height=\"31\" width=\"88\" /></a>\n</p>"))

(defun qq-validator()
     (interactive)
          (insert "<!--\nMarkUp Validation Service\nhttp://validator.w3.org/ \n-->"))

(defun qq-mode()
        (interactive)
        (kill-all-local-variables)
        (use-local-map qq-mode-map)
        (setq mode-name "XHTML 1.0 Transitional"))

(provide 'qq-mode)

;;; qq-mode.el ends here
