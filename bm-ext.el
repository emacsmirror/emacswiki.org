;;;
;; bm-ext.el
;;
;; An extension for bm.el that adds a function that lists all bookmarks in all
;; buffers. 
;;
;; By Dan McKinley, 2008 - http://mcfunley.com or mcfunley at gmail.
;;
;; For the latest version of bm.el, visit: 
;;    http://www.nongnu.org/bm/
;;
;; For the latest version of this extension, visit:
;;    http://www.emacswiki.org/cgi-bin/wiki/VisibleBookmarks
;;
;; Revision history:
;; 08/16/2008 - Created (Dan McKinley)
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
(eval-when-compile (require 'cl))
(require 'bm)

(defun bm-all ()
  "Displays a buffer listing the bookmarks in all open buffers."
  (interactive)
  (let* ((bookmarks (bm-all-bookmarks))
	 (lines (mapconcat 'bm-format-line bookmarks "")))
    (if (null bookmarks)
	(message "No bookmarks.")
      (with-output-to-temp-buffer "*bm-bookmarks*"
	(set-buffer standard-output)	
	(insert lines)
	(bm-show-mode)
	(setq buffer-read-only t))
      (let ((header (format "  %-30s %5s   %s" "Buffer" "Line" "Content")))
	  (put-text-property 0 (length header) 'face 'fixed-pitch header)
	  (with-current-buffer "*bm-bookmarks*"
	    (setq header-line-format header))))))

(defun bm-format-line (bm)
  (let ((buf (overlay-buffer bm)))
    (with-current-buffer buf
      (let ((string 
	     (format "%-30s %5s   %s"
		     buf
		     (count-lines (point-min) (overlay-start bm))
		     (buffer-substring (overlay-start bm) (overlay-end bm)))))
    (put-text-property 0 (length string) 'bm-buffer buf string)
    (put-text-property 0 (length string) 'bm-bookmark bm string)
    string))))
    
(defun bm-all-bookmarks ()
  (let (bookmarks)
    (mapcar #'(lambda (buf) 
		(mapcar #'(lambda (bm) (push bm bookmarks))
			(bm-bookmarks-in-buffer buf)))
	    (buffer-list))
    bookmarks))

(defun bm-bookmarks-in-buffer (buf)
  "Gets a list of bookmarks in `buf', which can be a string or a buffer."
  (flet ((mklist (x) (if (listp x) x (list x))))
    (mklist
     (with-current-buffer buf
       (apply 'append 
	      (mapcar 'mklist (remove nil (bm-lists))))))))

(provide 'bm-ext)
