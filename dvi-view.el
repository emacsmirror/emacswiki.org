;;;; dvi-view.el -- View a DVI file in an Emacs buffer
;;; Time-stamp: <2007-08-20 00:30:43 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;; Converter written 2007-08-18 by John C G Sturdy when he found
;; himself on a machine without xdvi, and major mode written the next
;; day.

;; todo: make this choose a format according to what converters are available

;; todo: find how to get page boundary information to make pages clickable -- I think there is some magic stuff you can get in DVI files

;; todo: sort out the two-page spreads

(defgroup dvi-view
  nil
  "In-buffer viewer for TeX's dvi output.")

(defcustom dvi-view-converter-program "/usr/bin/dvipng"
  "Program to convert dvi files to png."
  :type '(file :must-match t)
  :group 'dvi-view)

(defcustom dvi-view-converter-options nil
  "Options to pass to the dvi->png converter program."
  :type 'list
  :group 'dvi-view)

(defcustom dvi-view-image-options nil
  "How to display each image in dvi-view-file."
  :type 'plist
  :group 'dvi-view)

(defcustom dvi-view-page-header "Page %d:\n"
  "String to put before each image."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-page-footer "\n"
  "String to put after each image."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-even-page-header ""
  "String to put before each even-numbered image if doing two-page spreads."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-even-page-footer "\n"
  "String to put after each even-numbered image if doing two-page spreads."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-odd-page-header ""
  "String to put before each odd-numbered image if doing two-page spreads."
  :type 'string
  :group 'dvi-view)

(defcustom dvi-view-odd-page-footer ""
  "String to put after each odd-numbered image if doing two-page spreads."
  :type 'string
  :group 'dvi-view)

(defun dvi-view-file (dvi-file &optional two-page-spread)
  "View DVI-FILE in an Emacs buffer.
You can give the name of the TeX file, and it will use the DVI file anyway."
  (interactive "fView DVI file: 
P")
  (when (file-directory-p dvi-file)
    (setq dvi-file (buffer-file-name)))
  (let ((base-name (file-name-sans-extension (expand-file-name dvi-file)))
	(buffer (get-buffer-create " *dvipng output*")))
    (message "dvi file %s" dvi-file)
    (set-buffer buffer)
    (erase-buffer)
    (setq dvi-file (concat base-name ".dvi")
	  default-directory (file-name-directory dvi-file))
    (let ((png (apply 'call-process dvi-view-converter-program
		      nil buffer nil dvi-file
		      dvi-view-converter-options))
	  (pages nil))
      (if (/= png 0)
	  (error "Error %S in dvipng" png)
	(goto-char (point-min))
	;; first get all the page numbers from this buffer, rather
	;; than keep switching buffers
	(while (re-search-forward "\\[\\([0-9]+\\) ?\\(([0-9]+)\\)?\\]" (point-max) t)
	  (let* ((page (match-string-no-properties 1))
		 (page-file (concat base-name page ".png")))
	    (push (cons (string-to-int page) page-file) pages)))
	(let ((view-buffer (get-buffer-create (format "*View of %s*" (file-name-nondirectory base-name)))))
	  (set-buffer view-buffer)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (dolist (page (nreverse pages))
	    (message "Adding %S" page)
	    (let ((image (apply 'create-image (cdr page)
				nil nil
				dvi-view-image-options))
		  (start (point)))
	      (if two-page-spread
		  (if (evenp (car page))
		      (when dvi-view-even-page-header
			(insert (format dvi-view-even-page-header (car page))))
		    (when dvi-view-odd-page-header
		      (insert (format dvi-view-odd-page-header (car page)))))		      
		(when dvi-view-page-header
		  (insert (format dvi-view-page-header (car page)))))
	      (insert-image image)
	      (if two-page-spread
		  (if (evenp (car page))
		      (when dvi-view-even-page-footer
			(insert (format dvi-view-even-page-footer (car page))))
		    (when dvi-view-odd-page-footer
		      (insert (format dvi-view-odd-page-footer (car page)))))		      
		(when dvi-view-page-footer
		  (insert (format dvi-view-page-footer (car page)))))
	      (put-text-property start (point) 'page-number (car page))))
	  (goto-char (point-min))
	  (pop-to-buffer view-buffer)
	  (dvi-view-mode))))))

;;;; Major mode for viewing DVI files

(defun dvi-view-mode-goto-page (page)
  "Go to PAGE."
  (interactive "NGo to page: ")
  (unless (eq major-mode 'dvi-view-mode)
    (error "Not in dvi-view-mode"))
  (let ((current-page (get-text-property (point) 'page-number))
	(place (point)))
    (if (> page current-page)
	(while (and (> page current-page)
		    (setq place (next-single-property-change place 'page-number)))
	  (setq current-page (get-text-property place 'page-number)))
      (while (and (< page current-page)
		  (setq place (previous-single-property-change place 'page-number)))
	(setq current-page (get-text-property place 'page-number))))
    (if place
	(progn
	  (goto-char place)
	  (recenter 0))
      (error "Could not find page %d" page))))

(defun dvi-view-mode-previous-page ()
  "Go to the next page in the DVI file."
  (interactive)
  (let ((place (previous-single-property-change (point) 'page-number)))
    (if place
	(progn
	  (goto-char place)
	  (recenter 0))
      (goto-char (point-min)))))

(defun dvi-view-mode-next-page ()
  "Go to the next page in the DVI file."
  (interactive)
  (let ((place (next-single-property-change (point) 'page-number)))
    (if place
	(progn
	  (goto-char place)
	  (recenter 0))
      (error "Could not find next page"))))

(defun dvi-view-last-page ()
  "Go to the top of the last page in the DVI file."
  (interactive)
  (goto-char (point-max))
  (dvi-view-mode-previous-page))

(defvar dvi-view-mode-map (make-keymap "DVI view")
  "Keymap for dvi-view-mode.")

(suppress-keymap dvi-view-mode-map)
(define-key dvi-view-mode-map "g" 'dvi-view-mode-goto-page)
(define-key dvi-view-mode-map "p" 'dvi-view-mode-previous-page)
(define-key dvi-view-mode-map "-" 'dvi-view-mode-previous-page)
(define-key dvi-view-mode-map "n" 'dvi-view-mode-next-page)
(define-key dvi-view-mode-map "+" 'dvi-view-mode-next-page)
(define-key dvi-view-mode-map "^" 'beginning-of-buffer)
(define-key dvi-view-mode-map "<" 'beginning-of-buffer)
(define-key dvi-view-mode-map ">" 'dvi-view-last-page)
(define-key dvi-view-mode-map "$" 'dvi-view-last-page)
(define-key dvi-view-mode-map "\d" 'scroll-down)
(define-key dvi-view-mode-map " " 'scroll-up)
(define-key dvi-view-mode-map "q" 'bury-buffer)

(defun dvi-view-mode ()
  "Major mode for viewing DVI files.
Navigation commands according to various common conventions are provided.
Commands are:
\\{dvi-view-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dvi-view-mode
	mode-name "DVI view"
	buffer-read-only t)
  (use-local-map dvi-view-mode-map))

(provide 'dvi-view)

;;; end of dvi-view.el
