;;; Thumb-page.el -- create a muse file with thumbs
;; 
;; Filename: thumb-page.el
;; Description: Provide three functions to serial rename a directory,
;; create a thumb-directory, and finally create a muse file with url
;; of all thumbs.
;;
;; Author: ThierryVolpiatto 
;; Maintainer: ThierryVolpiatto
;; Contact: thierry dot volpiatto at gmail dot com
;;
;; Created: jeu déc 25 16:43:37 2008 (+0100)
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:

;;; Install:
;;  =======
;; Put this file somewhere in your path and:
;; (require 'thumb-page)
;; you can set different variables in customize ==> muse ==> thumb-page
;; or in your init file

;; Usage:
;; =====
;; To create a web page with a gallery of pictures, Proceed in three steps:
;; 1) If the files of your directory are not well sorted rename all the files:
;; Use M-x tv-serial-rename
;; 2) Convert all the files in a Thumb directory:
;; You don't have to mkdir the thumb_dir, just do M-x thumb-convert
;; NOTE: you can use image-dired for that also.
;; 3) Create the muse file and publish it:
;; M-x muse-write-thumb-table
;; if you say y to comment you will be prompted for each picture displayed
;; in the other window for a comment about this picture.
;; if you say n the comment will be added like photo 1 2 ... and 
;; you will not be prompted for comment.(better if you are impatient like me ;))
;; Save your muse file and C-c C-t ==> html RET

;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
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

(defgroup thumb-page nil
  "Write a muse file with thumbs linked to pictures, this program
provide 2 tools: 
 - one to create a directory of thumbs from the original dir of pictures
 - one to create the muse file"
  :group 'muse)

(defcustom host-url
  "http://localhost:8080"
  "the host url of this server"
  :type 'string
  :group 'thumb-page)

(defcustom mess-acc
  "Back to index page"
  "Message to come back to index page"
  :type 'string
  :group 'thumb-page)

(defcustom server-image-dir
  "/pics/"
  "The relative path to the directory containing pictures on root of your server"
  :type 'string
  :group 'thumb-page)

;;;###autoload
(defun muse-write-thumb-table (dir ficmuse thumbd comment nbrcol)
  "This function take two directory, one of images and an another one
with thumbs and write a muse file with thumbs linked to normal images.
If comment is (y) preview image and interactive add of command is provided
If comment is (n) comment will be added without any prompt and will be
Photo 1 Photo 2 ...etc...
Important: The two directory must be renamed sequentially and 
sorted correctly with serialrename.
Assume you are working in the directory where the pictures are."
  (interactive "fDir: \nFFicMuse: \nfThumbd: \nsComment(y/n): \nnNbrCol: ")
  (let ((dir-photos (cddr (directory-files dir)))
        (dir-thumbs (cddr (directory-files thumbd)))
        (relative-dir (file-relative-name dir default-directory))
        (relative-thumbd (file-relative-name thumbd default-directory))
        (n 0)
        (c 0)
        (flag-col 0)
        (url-fic "")
        (url-thumb "")
        (new-comment nil))
  (find-file ficmuse)
  (goto-char (point-min))
  (save-excursion
    (delete-region (point-min) (point-max)))
  (insert (concat "#title\n" relative-dir "\n\n\n" "[[" host-url "][" mess-acc "]]\n\n\n"))
  (dolist (i dir-photos)
    ; set the first line
    (setq url-fic (concat "[[" host-url server-image-dir relative-dir i "]")
          url-thumb (concat "[" server-image-dir relative-thumbd (nth n dir-thumbs) "]]" " | "))
    (if (string= comment "y") ;comment photo interactive with preview
        (progn
          (save-excursion
            (view-file-other-window (concat dir i))
            (push (format "%s" (read-string "Commentary: ")) new-comment)
            (delete-window)))
      (progn ; else (n) comment with Photo c
        (setq comment (format "Photo %s" c))
        (push comment new-comment)
        (setq c (+ c 1))))
    (if (= flag-col (- nbrcol 1)) ;flag-col is full, jump two line
        (progn 
          (insert (concat url-fic url-thumb "\n\n" "       "))
          (setq new-comment (reverse new-comment))
          (dolist (j new-comment)
            (insert (concat j " | ")))
          (insert "\n\n")
          (setq new-comment nil
                flag-col 0)
          (setq n (+ n 1)))
      (progn ;else: start here with empty flag-col
        (if (= flag-col 0) ;first line
            (insert (concat "       " url-fic url-thumb))
          (insert url-fic url-thumb)) ;continue until flag-col not full
        (setq flag-col (+ flag-col 1))
        (setq n (+ n 1)))))
  ;;TODO: use when here
  (if new-comment ;to write the last line 
      (progn
        (insert (concat "\n\n" "       "))
        (setq new-comment (sort new-comment 'string<))
        (dolist (j new-comment)
          (insert (concat j " | ")))
        (setq new-comment nil)))))

(defmacro mapc-with-progress-reporter (message func seq)
  `(let* ((max (length ,seq))
          (progress-reporter
           (make-progress-reporter
            (message ,message)
            0 max))
          (count 0))
     (mapc #'(lambda (x)
               (progress-reporter-update progress-reporter count)
               (funcall ,func x)
               (incf count))
           ,seq)
     (progress-reporter-done progress-reporter)))

;;;###autoload
(defun thumb-convert (dir thumbdir scale)
  "If thumb directory doesn't exist create it and
convert all files of input dir to thumbs"
  (interactive "fDir: \nFThumbDir: \nsScale: ")
  (when (not (file-exists-p thumbdir))
    (make-directory thumbdir))
  (setq thumbdir (file-name-as-directory thumbdir))
  (setq dir (file-name-as-directory dir))
  (let ((ls-dir (cddr (directory-files dir))))
    (mapc-with-progress-reporter (format "Wait, converting files to %s..." thumbdir)
                                 #'(lambda (x)
                                     (call-process-shell-command (format "convert %s -scale %s %s" 
                                                                         (concat dir x) 
                                                                         scale 
                                                                         (concat thumbdir x))))
                                 ls-dir))
  (message "%s Created Successfully!" thumbdir))

(defun* thumb-convert-current-dir (&optional (scale "60%"))
  (interactive)
  (let* ((cur-dir default-directory)
         (thumb-dir (concat cur-dir "save-resized"))
         (new-scale (if current-prefix-arg
                        (read-string "Scale: ")
                        scale))) 
  (thumb-convert cur-dir thumb-dir new-scale)))

;;;###autoload
(defun tv-serial-rename (dir ext name start)
  "rename all the files of dir matching regex ext with the name name
starting to number start - ex: file01.jpg"
  (interactive "fDir: \nsExt(no dot): \nsName: \nnStart: ")
  (find-file dir)
  (let (ls-dir
        new-ls-dir
        n
        c)
  (setq ls-dir (file-expand-wildcards (format "*.%s" ext) t))
  (setq new-ls-dir nil)
  (setq n 0)
  (while (< n (length ls-dir))
    (if (< start 10)
        (push (concat dir name (format "0%s" start) "." ext) new-ls-dir)
      (push (concat dir name (format "%s" start) "." ext) new-ls-dir))
    (setq start (+ start 1))
    (setq n (+ n 1)))
  (setq ls-dir (reverse ls-dir))
  (setq c 0)
  (dolist (i ls-dir)
    (rename-file i (nth c new-ls-dir))
    (setq c (+ c 1)))))

(provide 'thumb-page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; thumb-page.el ends here




    
      
      


  
    
