;;; eiv.el --- emacs image viewer

;; Copyright (C) 2008, 2009 Thierry Volpiatto
;; Author:     Thierry Volpiatto 
;; Maintainer: Thierry Volpiatto 
;; Keywords: image, picture

;; Created: lun fév  2 11:38:44 2009 (+0100)

;; X-URL: http://mercurial.intuxication.org/hg/emacs-image-viewer

;; This file is not part of GNU Emacs.

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

;;; Commentary: 

;; Dependencies: - traverselisp.el:
;;                 http://mercurial.intuxication.org/hg/traverselisp
;;               - iterator.el
;;                 that is part of traverselisp package.
;;               - the ImageMagick package that provide "mogrify":
;;                 http://www.imagemagick.org/

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `eiv-fit-image-to-window'
;;    Resize image to current window size.
;;  `eiv-rotate-current-image'
;;    Rotate current image at 90 degrees.
;;  `eiv-dec-or-inc-image'
;;    Resize image to current window size.
;;  `eiv-diaporama'
;;    Start a diaporama on tree.
;;  `eiv-viewer'
;;    The emacs-image-viewer. Allow to navigate in a Tree of dir and subdir
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Changelog:
;; See <http://mercurial.intuxication.org/hg/emacs-image-viewer>

;;; Code:

;;; Require

(require 'cl)
(require 'traverselisp)
(require 'iterator)

;;; User variables

(defvar eiv-default-dir "~/Pictures/")
(defvar eiv-default-save-dir-name "eiv-save")
(defvar eiv-default-diaporama-delay 2)

;;; Image modification

;;;###autoload
(defun eiv-fit-image-to-window (&optional arg quiet)
  "Resize image to current window size.
With prefix arg don't preserve the aspect ratio."
  (interactive)
  (lexical-let ((cur-fname-to-resize
                 (buffer-file-name (current-buffer)))
                (noverbose quiet))
    (let* ((edges  (window-inside-pixel-edges))
           (width  (- (nth 2 edges) (nth 0 edges)))
           (height (- (nth 3 edges) (nth 1 edges)))
           (asr    (when current-prefix-arg t)))
      (apply #'start-process "resize-image" nil "mogrify"
             (list "-resize"
                   (concat (format "%dx%d" width height)
                           (and asr "!"))
                   cur-fname-to-resize))
      (set-process-sentinel (get-process "resize-image")
                            #'(lambda (process event)
                                (quit-window t)
                                (view-file cur-fname-to-resize)
                                (unless noverbose
                                  (message "Ok %s on %s %s"
                                           process
                                           (file-name-nondirectory
                                            cur-fname-to-resize)
                                           event)))))))

;;;###autoload
(defun eiv-rotate-current-image (&optional num-arg)
  "Rotate current image at 90 degrees.
with prefix arg at -90 degrees"
  (interactive)
  (let ((fname (buffer-file-name (current-buffer))))
    (unless num-arg
      (setq num-arg (if current-prefix-arg
                        -90
                        90)))
    (shell-command (format "mogrify -rotate %s %s" num-arg fname))
    (quit-window t)
    (view-file fname)))

;;;###autoload
(defun eiv-dec-or-inc-image (n &optional quiet)
  "Resize image to current window size.
With prefix arg don't preserve the aspect ratio."
  (interactive)
  (lexical-let ((cur-fname-to-resize
                 (buffer-file-name (current-buffer)))
                (noverbose quiet))
    (let* ((amount   130)
           (com-args (if (< n 0)
                         (format "%d%%^" (* 100 (/ 100.0 amount)))
                         (format "%d%%^" amount))))
      (apply #'start-process "resize-image" nil "mogrify"
             (list "-resize"
                   com-args
                   "-gravity"
                   "center"
                   cur-fname-to-resize))
      (set-process-sentinel (get-process "resize-image")
                            #'(lambda (process event)
                                (quit-window t)
                                (view-file cur-fname-to-resize)
                                (unless noverbose
                                  (message "Ok %s on %s %s"
                                           process
                                           (file-name-nondirectory
                                            cur-fname-to-resize)
                                           event)))))))

;;;###autoload
(defun eiv-tag-image (text &optional quiet)
  (interactive "sText: ")
  (lexical-let* ((cur-fname-to-tag (concat (symbol-name (gensym "img")) ".jpg"))
                 (img              (buffer-file-name (current-buffer)))
                 (def-dir          (file-name-directory img))
                 (noverbose        quiet))
    (let* ((width-buf (eshell-command-result (concat "identify -format %w " img)))
           (size-buf  (concat width-buf "x" "24")))
      (apply #'start-process "tag-image" nil "convert"
             (list "-background"
                   "#0008"
                   "-fill"
                   "white"
                   "-gravity"
                   "center"
                   "-size"
                   size-buf
                   (format "caption:%s" text)
                   "+size"
                   (file-name-nondirectory img)
                   "+swap"
                   "-gravity"
                   "south"
                   "-composite"
                   cur-fname-to-tag)))
    (set-process-sentinel (get-process "tag-image")
                          #'(lambda (process event)
                              (quit-window t)
                              (rename-file (expand-file-name cur-fname-to-tag def-dir)
                                           (expand-file-name img def-dir) t)
                              (view-file img)
                              (unless noverbose
                                (message "Ok %s on %s %s"
                                         process
                                         (file-name-nondirectory
                                          cur-fname-to-tag)
                                         event))))))


(defun eiv-display-external (&optional fname)
  "Display FNAME or file at point using an external viewer."
  (interactive)
  (let ((file (or fname (dired-get-filename))))
    (call-process shell-file-name nil nil nil shell-command-switch
		  (format "%s \"%s\"" image-dired-external-viewer file))))

(defvar eiv-view-external nil)

;;; Image navigation

;;;###autoload
(defun eiv-viewer (tree &optional only)
  "Allow to navigate in a Tree of dir and subdir of pictures.
If prefix arg prompt for only file extensions to use.
An interactive diaporama is also available.

On each image, simple manipulations are possible:
- rotate left and right.
- resize image to window size.
- decrease image size
- increase image size
- tag image
- scroll image

NOTE: these manipulations are destructives on file
so when resizing you will be prompt to save image, if you DON'T save
your initial image will be LOST."
  (interactive (list (read-directory-name "DTree: ")
                     (when current-prefix-arg
                       (read-string "OnlyExt: "))))
  (let* ((save-dir       (concat eiv-default-save-dir-name "/"))
         (ignore-dirs    (cons eiv-default-save-dir-name traverse-ignore-dirs))
         (flist          (traverse-list-files-in-tree tree nil ignore-dirs
                                                      (when only (list only))))
         (flist-iterator (iter-list flist))
         (diapo-run      nil)
         (display-fn     (if eiv-view-external 'eiv-display-external 'view-file))
         (diapo-speed    eiv-default-diaporama-delay)
         action cur-elm flag-move fnext-elm bnext-elm)
    (flet ((eiv-viewer-goto-next-file ()
             (clear-image-cache t)
             (if (eq major-mode 'image-mode)
                 (quit-window t))
             (if flag-move ;; direction changed we use a new iterator
                 (progn
                   (setq flist-iterator (iter-sub-next flist cur-elm))
                   (setq fnext-elm (iter-next flist-iterator))
                   (setq cur-elm fnext-elm)
                   (if fnext-elm
                       (funcall display-fn fnext-elm)
                       (throw 'break
                         (message "Finish! no more images"))))
                 ;; Use initial iterator unless we change direction
                 (let ((next-elm (iter-next flist-iterator)))
                   (setq cur-elm next-elm)
                   (if next-elm
                       (funcall display-fn next-elm)
                       (throw 'break
                         (message "Finish! no more images"))))))
           (eiv-viewer-goto-prec-file ()
             (clear-image-cache t)
             (if (eq major-mode 'image-mode)
                 (quit-window t))
             (setq flist-iterator (iter-sub-prec flist cur-elm))
             (setq bnext-elm (iter-next flist-iterator))
             (setq cur-elm bnext-elm)
             (setq flag-move t)
             (if bnext-elm
                 (funcall display-fn bnext-elm)
                 (throw 'break
                   (message "Finish! no more images"))))
           (eiv-save-file (fn)
             (unless diapo-run
               (if (y-or-n-p "Save image?")
                   (let* ((fname     (buffer-file-name (current-buffer)))
                          (tmp-fname (concat (symbol-name (gensym "img"))
                                             (file-name-extension fname t)))
                          (tmp-dname (concat default-directory save-dir)))
                     (if (not (file-exists-p tmp-dname))
                         (make-directory tmp-dname)) 
                     (copy-file fname
                                (concat tmp-dname tmp-fname))
                     (funcall fn))
                   (funcall fn)))))
      (eiv-viewer-goto-next-file)
      (catch 'break
        (while t
          (catch 'continue
            (if diapo-run
                (setq
                 action
                 (or (read-event
                      (propertize
                       (concat
                        "(b)ack|"
                        "(a)stop|"
                        "(q)uit|"
                        "(<)slowDown"
                        "(>)speedUp")
                       'face '((t (:background "DarkSlateBlue" :foreground "AntiqueWhite"))))
                      nil
                      diapo-speed)
                     '?s))
                (setq action
                      (read-event
                       (propertize
                        (concat
                         "(n)ext|"
                         "(s)tartDiapo|"
                         "(b)ack|"
                         "(x)Down|"
                         "(y)Up|"
                         "(q)uit|"
                         "(t)ag)|"
                         "(l)Rotateleft|"
                         "(r)otateRight|"
                         "(f)itToWindow|"
                         "(d)ecrease|"
                         "(i)ncrease")
                        'face '((t (:background "DarkSlateBlue" :foreground "AntiqueWhite")))))))
            (case action
              ;; go forward
              ('?n
               (setq diapo-run nil)
               (eiv-viewer-goto-next-file))
              ('?s
               (setq diapo-run t)
               (eiv-viewer-goto-next-file)
               (throw 'continue nil))
              ;; stop diaporama
              ('?a
               (setq diapo-run nil))
              ;; slow down diaporama
              ('?<
               (incf diapo-speed))
              ;; speed up diaporama
              ('?>
               (unless (<= diapo-speed 2)
                 (decf diapo-speed)))
              ;; go backward
              ('?b
               (setq diapo-run nil)
               (eiv-viewer-goto-prec-file))
              ('?x
               (unwind-protect
                    (scroll-down -1)
                 (throw 'continue nil)))
              ('?y
               (unwind-protect
                    (scroll-down 1)
                 (throw 'continue nil)))
              ;; rotate right
              ('?r
               (unless diapo-run
                 (eiv-rotate-current-image)))
              ;; rotate left
              ('?l
               (unless diapo-run
                 (eiv-rotate-current-image -90)))
              ;; tag image
              ('?t
               (eiv-save-file #'(lambda ()
                                  (eiv-tag-image (read-string "Text: ") t))))
              ;; resize to window
              ('?f
               (eiv-save-file #'(lambda ()
                                  (eiv-fit-image-to-window nil t))))
              ;; reduce size
              ('?d
               (eiv-save-file #'(lambda ()
                                  (eiv-dec-or-inc-image -1 t))))
              ;; increase size
              ('?i
               (eiv-save-file #'(lambda ()
                                  (eiv-dec-or-inc-image 1 t))))
              ;; quit
              ('?q
               (clear-image-cache t)
               (quit-window t)
               (throw 'break nil)))))))))


(provide 'eiv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eiv.el ends here
