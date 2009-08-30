;;; thumbs-xmas.el --- Thumbnails previewer for images files
;; 
;;
;; This is xemacs version of original thumbs.el package.
;;
;;
;; Emacs Lisp Archive Entry
;; Filename: thumbs.el
;; Version: 0.7
;; Keywords: Multimedia
;; Author: Jean-Philippe Theberge <jphil@emacslisp.org>
;; Maintainer: Jean-Philippe Theberge <jphil@emacslisp.org>
;; Created: 15 Jan 2002
;; Last-Updated: 30 Jan 2002
;; Description: Thumbnails previewer for images files.
;; URL: http://www.emacslisp.org/mypackages/thumbs/
;; Compatibility: Emacs 21 and Unix
;; Incompatibility: Emacs < 21, Xemacs, non-unix OS

;; This is free software. The GPL applies.

;;; Commentary:
;; 
;; This package modify dired mode and create two new mode: thumbs-mode
;; and thumbs-view-image-mode. It is used for images browsing and
;; viewing from within emacs. Minimal Image manipulation functions are
;; also available via external programs.
;;
;; The 'convert' program from 'ImageMagick'
;; [URL:http://www.imagemagick.org/] is required.
;;
;; The 'Esetroot' program from Eterm [URL:http://www.eterm.org/] is
;; optional (need by the desktop wallpaper functionality)
;;
;;; Revision history
;;
;; 0.1  Initial version
;; 0.2  Minimal Dired interaction
;; 0.3  Some code optimisation - mini-doc - checking for thumbs-dir
;;      Andrew Scott <ascott@sedona.ch.intel.com>  send me a patched copy
;;      with theses changes:
;;      * I added a test for convert already being in someone's path but
;;        not in your default place.
;;      * I changed the defvar docstring to put the code first than the
;;        docstrings, not the other way around. This fixed my functionality
;;        problem with the concatenated directories, probably because
;;        thumbs-thumbsdir was getting munged.
;;      * I changed the docstrings to start with "*", which seems to indicate
;;        that this is a user-configurable variable.
;;      * I added a few clarifying comments.
;;      * I added a test for image-handling (basically to differential
;;        Emacs-20.7 vs 21.1; only the later can do images.)
;; 0.4  Updated archive_entry
;;      Auto-cleaning of thumbnails-dir
;;      Resizing in thumbs-view-image-mode with '+' and '-'
;; 0.5  Emboss in thumbs-view-image-mode with 'e'
;;      Some code rewrote.
;;      M-x thumbs-modify-image let you modify the image in any way convert can.
;;      auto-show image-name in minibuffer in thumbnail-mode.
;; 0.6  Some corrections, thanks to Andrew Scott.
;;      page-up, page-down in image view mode to move to next or previous image.
;;      Desktop wallpaper setting via dired.
;;      dired-find-file kill the preview buffer.
;;      thumbs-save-current-image, for saving a modified image.
;;      thumbs-make-html to generate a HTML preview page. 
;; 0.7  Solved problem with dired-buffer
;;      Solved startup problem when thumbs-thumbsdir do not exists.
;;      Some code rewrote (mostly style, no new functionality)
;;      Add thumbs-user-thumbsdir to replace thumbs-thumbsdir 
;;       (this one don't need to be expanded)
;;      Fix the call to Esetroot (it was bugging with some filenames)
;;
;;; THANKS TO:
;;
;; Andrew Scott, for suggestions and patches.
;; John Wiegley, for eshell from which I borrow codes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HOW TO USE
;;
;; 1- Install thumbs.el in your load-path and load it.
;; 
;; 2- Set the 'thumbs-thumbsdir' variable and make sure this directory
;;    exists, The default is ~/.emacs-thumb
;;
;; 3- This package assumes that ImageMagick's 'convert' package is on
;;    your binary search path (i.e. exec-path). If not, set
;;    'thumbs-conversion-program' to the full path to ImageMagick
;;    'convert'. For now the program MUST be 'convert'.
;;
;; then theses commands are available to you:
;;
;; M-x thumbs-show-all-from-dir
;;
;; Prompt for a directory then display a thumbnails page .
;;
;; From Dired
;;
;; T    Display the thumbnail for image at point.
;; C-t  Display a thumbnails page for all images in current directory
;; M-t  Display a thumbnails page for all marked images.
;; w    Set image at point as desktop wallpaper.
;;
;; M-x toggle-thumbs-show-preview
;;
;; toggle on/off auto-image thumbnail preview from dired buffer
;;
;; From the thumbnails preview page
;;
;; [return]    Display the image in the same emacs buffer
;; [M-return]  Display the image in a new window
;; [C-return]  Set the image as your desktop wallpaper I call for this
;;             the Esetroot program because this is the one I use.  If
;;             you hack thumbs.el to support another program, please
;;             sent me the patch!
;; [delete]    Delete the image.
;; s           Show the image name in the minibuffer
;; q           quit
;;
;; From the Image view page.
;;
;; WARNING: You are in image-view-mode when you select an image from
;;          the thumbnails preview page, NOT FROM DIRED.
;;
;; +         rescale the image bigger
;; -         rescale the image smaller
;; page-up   previous image
;; page-down next image
;; e         emboss the image
;; r         rescale the image, asking for width and height in minibuffer
;; q         quit
;; w         Set current image as desktop wallpaper.
;;
;;
;; M-x thumbs-monochrome-image to turn the image monochrome
;;
;; M-x thumbs-negate-image to reverse image colors
;;
;; M-x thumbs-modify-image <action> <argument>
;; see the convert man-page for possible actions and arguments.
;; for example: M-x thumbs-modify-image <return> swirl <return> 50 <return>
;; will swirl the current image by 50 degree.
;;
;; M-x thumbs-save-current-image save the modified image.
;;
;; M-x thumbs-make-html will generate a html preview page.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: I will not make a XV front-end (to replace ImageMagick)
;;       because XV is not free software.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'derived)
(require 'dired)
(require 'cl)
(require 'esh-util)


;;; User-configurable Code:

(defun image-file-name-regexp ()
  (let ((reg
	 "\\.\\(GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|NG\\|PM\\)\\|TIFF\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|ng\\|pm\\)\\|tiff\\|x\\(?:[bp]m\\)\\)\\'"))
    reg))


(defun thumbs-remove-image ()
  (map-extents (function
		(lambda (extent maparg)
		  (delete-extent extent)))
	       nil (point-min)(point-max)))

(defvar thumbs-user-thumbsdir
  "~/.emacs-thumbs"
  "*Directory to store thumbnails.  Make sure it is expanded.")

(defvar thumbs-geometry "100x100"
  "*Size of thumbnails")

(defvar thumbs-thumbsdir-max-size
  50000000
  "Max size for thumbnails directory.
When it reach that size (in bytes), a warning is send.
(to be replaced with an auto delete of older files.)"
)

(defvar thumbs-conversion-program
  (or (locate-library "convert" t exec-path)
      "/usr/X11R6/bin/convert")
  "*Name of conversion program for thumbnails generation.
It must be 'convert'.")

(defvar thumbs-setroot-program
  (or (locate-library "Esetroot" t exec-path)
      "/usr/X11R6/bin/Esetroot")
  "*Name of Esetroot program for setting desktop Wallpaper.
Only 'Esetroot' is supported now but hack for another
program are more than welcome!")

(defvar thumbs-relief
  5
  "*Size of border around thumbnails.")

(defvar thumbs-thumbsdir-auto-clean
  t
  "if true, auto-delete older file when the thumbnails directory
became bigger than 'thumbs-thumbsdir-max-size'.
If nil, just echo a warning.")

(defvar thumbs-image-resizing-step 10)
(defvar thumbs-temp-dir "/tmp/") ;; with ending slash
(defvar thumbs-temp-prefix "emacsthumbs") ;; without leading slash
(defvar thumbs-temp-prefix-regexp "emacsthumbs.*")
(defvar thumbs-temp-file (concat thumbs-temp-dir thumbs-temp-prefix))

(defvar thumbs-html-width 6
  "* number of column in html generation page")



;;; local 


(defvar thumbs-current-image-filename nil)
(defvar thumbs-current-tmp-filename nil)
(defvar thumbs-current-image-size nil)
(defvar thumbs-fileL nil)
(defvar thumbs-image-num nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end of user configurable code, don't edit below this line unless    ;;
;; you know what you are doing and, in this case, don't forget to send ;;
;; me your patches!                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq thumbs-thumbsdir (expand-file-name thumbs-user-thumbsdir))

;; Make sure auto-image-file-mode is ON.
;;(auto-image-file-mode t)

(defun thumbs-thumbsdir-size ()
  "Return the actual size (in bytes) of the thumbsnail dir."
  (apply '+ (mapcar
	     (lambda (x)
	       (nth 7 x))
	     (mapcar
	      (lambda(x)
		(file-attributes x))
	      (directory-files
	       thumbs-thumbsdir t
	       (image-file-name-regexp))))))

(defun thumbs-cleanup-thumbsdir (dir max reg)
  "Clean DIR.
Deleting oldest files matching REG until DIR is below MAX bytes."
  (let* ((L (thumbs-sort-entries
	     (mapcar
	      (lambda(x)
		(append (list (expand-file-name x)) 
			(file-attributes x)))
	      (directory-files
	       dir t
	       reg))))
	 (dirsize
	  (apply '+ (mapcar
		     (lambda (x)
		       (nth 8 x))
		     L))))
    (while (> dirsize max)
      (progn
	(message (concat "Deleting file " (caar L)))
	(delete-file (caar L))
	(setq dirsize (- dirsize (nth 8 (car L))))
	(setq L (cdr L))))))

;; Check the thumbsnail directory size and clean it if necessary.
(if (file-exists-p thumbs-thumbsdir)
    (if (> (thumbs-thumbsdir-size) thumbs-thumbsdir-max-size)
	(if thumbs-thumbsdir-auto-clean
	    (thumbs-cleanup-thumbsdir thumbs-thumbsdir
				      thumbs-thumbsdir-max-size
				      (image-file-name-regexp))
	  (message "Your thumbnails directory is huge!!"))))

(defun thumbs-create-thumbsdir ()
  "If's thumbsdir don't exist, create it."
  (if (not (file-directory-p thumbs-thumbsdir))
      (progn
	(make-directory thumbs-thumbsdir)
	(message "Creating thumbnails directory"))))

(defun thumbs-call-convert (filein fileout action 
				   &optional arg output-format action-prefix)
  (let ((command (format "%s %s%s %s \"%s\" \"%s:%s\""
			 thumbs-conversion-program
			 (or action-prefix "-")
			 action
			 (or arg "")
			 filein
			 (or output-format "jpeg")
			 fileout)))
     (shell-command command)))

(defun thumbs-resize-image (&optional increment size)
  "Resize image in current buffer.
if INCREMENT is set, make the image bigger, else smaller.
Or, alternatively, a SIZE may be specified."
  (interactive)
  ;; cleaning of old temp file
  (setq buffer-read-only nil)
  (thumbs-remove-image)
  (delete-region (point-min)(point-max))
  (let* ((x (if size size
	      (let ((f (lambda (x)
			 (round 
			  (funcall 
			   (if increment '+ '-) 
			   x
			   (/ (* thumbs-image-resizing-step 
				 x) 
			      100))))))
		(cons (funcall f (car thumbs-current-image-size))
		      (funcall f (cdr thumbs-current-image-size))))))
	 (type (or (thumbs-image-type thumbs-current-image-filename) 'jpeg))
	 (ext  (if (string-match (image-file-name-regexp) thumbs-current-image-filename)
		   (match-string 1 thumbs-current-image-filename)
		 "jpg"))
	 (tmp (format "%s%s.%s" thumbs-temp-file (gensym) ext)))
    (thumbs-call-convert (or thumbs-current-tmp-filename 
			     thumbs-current-image-filename) 
			 tmp 
			 "sample" 
			 (concat (number-to-string (car x)) "x" 
				 (number-to-string (cdr x))) type)
    (thumbs-insert-image tmp type 0)
    (setq thumbs-current-tmp-filename tmp))
  (setq buffer-read-only t))


(defun thumbs-save-changes ()
  (interactive)
  (if (not thumbs-current-tmp-filename)
      (message (concat "Image " thumbs-current-image-filename " not changed."))
    (let ((new-file nil))
      (setq new-file (read-file-name "Save image to: "))
      (if (file-exists-p (expand-file-name new-file))
	  (if (y-or-n-p (concat new-file " exist, overwrite?"))
	      (copy-file thumbs-current-tmp-filename new-file t))
	(copy-file thumbs-current-tmp-filename new-file t)
	))))

(defun thumbs-resize-interactive (w h)
  "Resize Image interactively."
  (interactive "nWidth: \nnHeight: ")
  (thumbs-resize-image nil (cons w h)))

(defun thumbs-resize-image-to-window ()
  "Fit image in window."
  (interactive )
  (thumbs-resize-image nil (cons (window-pixel-width) (window-pixel-height))))
  
(defun thumbs-resize-image-size-down ()
  "Resize image."
  (interactive)
  (thumbs-resize-image nil))

(defun thumbs-resize-image-size-up ()
  "Resize image."
  (interactive)
  (thumbs-resize-image t))

(defsubst thumbs-compare-entries (l r inx func)
  "Compare the time of two files, L and R, the attribute indexed by INX."
  (let ((lt (nth inx (cdr l)))
	(rt (nth inx (cdr r))))
 (if (equal lt rt)
	(string-lessp (directory-file-name (car l))
		      (directory-file-name (car r)))
      (funcall func rt lt))))

(defun thumbs-sort-entries (entries)
  "Sort ENTRIES, a list of files and attributes, by atime."
  (reverse (sort entries
		 (function
		  (lambda (l r)
		    (let ((result
			   (thumbs-compare-entries
			    l r 4 'eshell-time-less-p)))
		      result))))))

(defun thumbs-subst-char-in-string (orig rep string)
  "Replace occurrences of character ORIG with character REP in STRING.
Return the resulting (new) string. -- (defun borowed to Dave Love)"
  (let ((string (copy-sequence string))
	(l (length string))
	(i 0))
    (while (< i l)
      (if (= (aref string i) orig)
	  (aset string i rep))
      (setq i (1+ i)))
    string))

(defun thumbs-thumbname (img)
  "Return a thumbnail name for the image IMG."
  (thumbs-subst-char-in-string
   ?\  ?\_
   (concat thumbs-thumbsdir "/"
	   (apply 
	    'concat
	    (split-string
	     (expand-file-name img) "/")))))

(defun thumbs-make-thumb (img)
  "Create the thumbnail for IMG."
  (thumbs-create-thumbsdir)
  (let* ((fn (expand-file-name img))
	 (tn (thumbs-thumbname img)))
    (if (not (or (file-exists-p tn)
		 (file-exists-p (concat tn ".0"))))
	(thumbs-call-convert fn tn "sample" (concat thumbs-geometry " -border \"3x3\" -bordercolor \"firebrick\"")))
    tn))
  
(defun thumbs-image-type (img)
  "Return image type from filename IMG."
  (cond ((string-match ".*\.jpg$" img) 'jpeg)
	((string-match ".*\.jpeg$" img) 'jpeg)
	((string-match ".*\.JPG$" img) 'jpeg)
	((string-match ".*\.JPEG$" img) 'jpeg)
	((string-match ".*\.gif$" img) 'gif)
	((string-match ".*\.GIF$" img) 'gif)
	((string-match ".*\.xpm$" img) 'xpm)
	((string-match ".*\.XPM$" img) 'xpm)
	))

(defun thumbs-find-thumb (img)
  "Display the thumbnail for IMG."
  (interactive)
  (find-file (thumbs-make-thumb img)))

(defun thumbs-insert-image (img type relief)
  "Insert IMG at point.
Argument TYPE describe type."
  (let ((i (make-image-instance (vector type :file img))))
    ;;(insert-file img)
    (if i
	(let ((glyph (make-glyph i)))
	  (if glyph
	      (progn
		(setq buffer-read-only nil)
		(set-extent-begin-glyph (make-extent (point) (point-max)) glyph)
		(setq thumbs-current-image-size 
		      (cons (image-instance-width i)
			    (image-instance-height i)))
		))))))

(defun thumbs-insert-thumb (img)
  "Insert the thumbnail for IMG at point."
  (let ((tn (thumbs-make-thumb img)))
    (if (file-exists-p tn)
	(thumbs-insert-image tn 'jpeg thumbs-relief)
      (if (file-exists-p (concat tn ".0"))
	  (thumbs-insert-image (concat tn ".0") 'jpeg thumbs-relief)))))

(defun thumbs-show-thumbs-list (L &optional buffer-name)
  "Make a preview buffer for all images in L."
  (pop-to-buffer (or buffer-name "*THUMB-View*"))
  (setq buffer-read-only nil)
  (thumbs-remove-image)
  (delete-region (point-min)(point-max))
  (thumbs-mode)
  (setq thumbs-fileL nil)
  (while L
    (setq thumbs-fileL (cons (cons (point)
				   (car L))
			     thumbs-fileL))
    (thumbs-insert-thumb (car L))
    (setq buffer-read-only nil)
    (goto-char (point-max))
    (insert " ")
    (setq L (cdr L)))
  (goto-char (point-min))
  (setq buffer-read-only t)
  (make-variable-buffer-local
   'thumbs-fileL))


(defun thumbs-show-all-from-dir (dir &optional reg)
  "Make a preview buffer for all images in DIR.
Optional argument REG to select file matching a regexp"
  (interactive "DDir: ")
  (thumbs-show-thumbs-list
   (directory-files dir t
		    (or reg (image-file-name-regexp)))
   (concat "*Thumbs: " dir))
  (setq thumb-current-dir dir)
  (make-variable-buffer-local 'thumb-current-dir))

(defun thumbs-find-image-at-point (&optional img)
  "Display image for thumbnail at point in
the preview buffer."
  (interactive)
  (let* ((L thumbs-fileL)
	 (n (point))
	 (i (or img (cdr (assoc n L)))))
    (switch-to-buffer
     (concat "*Image: " (file-name-nondirectory i) " - " 
	     (number-to-string n) "*"))
    (thumbs-view-image-mode)
    (setq buffer-read-only nil)
    (make-variable-buffer-local 'thumbs-current-image-filename)
    (make-variable-buffer-local 'thumbs-current-tmp-filename)
    (make-variable-buffer-local 'thumbs-current-image-size)
    (make-variable-buffer-local 'thumbs-fileL)
    (make-variable-buffer-local 'thumbs-image-num)
    (thumbs-remove-image)
    (delete-region (point-min)(point-max))
    (thumbs-insert-image i (thumbs-image-type i) 0)
    (setq thumbs-current-image-filename i
	  thumbs-fileL L
	  thumbs-current-tmp-filename nil
	  thumbs-image-num n
	  buffer-read-only t)))

(defun thumbs-find-image-at-point-other-window ()
  "Display image for thumbnail at point
in the preview buffer. Open another window."
  (interactive)
  (split-window)
  (next-window)
  (thumbs-find-image-at-point
   (cdr (assoc (point) thumbs-fileL))))

(defun thumbs-call-Esetroot (img)
  (shell-command 
   (concat thumbs-setroot-program " -fit \"" img "\"")))

(defun thumbs-set-image-at-point-to-root-window ()
  "Use Esetroot to use the image at
point as a desktop wallpaper."
  (interactive)
  (thumbs-call-Esetroot (cdr (assoc (point) thumbs-fileL))))

(defun thumbs-delete-image-at-point ()
  "Delete the image at point (and it's thumbnail)."
  (interactive)
  (let* ((f (cdr (assoc (point) thumbs-fileL)))
	 (tn (thumbs-thumbname f)))
    (if (yes-or-no-p (concat "Really delete " f " "))
	(progn
	  (delete-file f)
	  (if (file-exists-p tn)
	      (delete-file tn))
	  (if (file-exists-p (concat tn ".0"))
	      (delete-file (concat tn ".0"))))))
  (thumbs-show-all-from-dir thumb-current-dir))

(defun thumbs-make-html ()
  (interactive)
  (let ((L thumbs-fileL)
	(count 0))
    (pop-to-buffer "*html*")
    (thumbs-remove-image)
    (delete-region (point-min)(point-max))
    (insert "<html>\n<body>\n<table>\n")
    (while L
      (let ((tn (thumbs-thumbname (cdar L))))
	(setq count (+ 1 count)) 
	(if (equal 1 count)(insert "<tr>\n"))
	(insert (concat "<td><a href=\"file:///" 
			(cdar L) 
			"\"><img src=\"file:///" 
			(if (file-exists-p (concat tn ".0"))
			    (delete-file (concat tn ".0"))
			  (thumbs-thumbname (cdar L)))
			"\"></a></td>\n"))
	(setq L (cdr L))
	(if (equal thumbs-html-width count)
	    (progn 
	      (insert "</tr>\n")
	      (setq count 0))))
      (insert "</tr>\n</table>\n</body>\n</html>\n"))))

(defun thumbs-kill-buffer ()
  "Kill the current buffer"
  (interactive)
  (ignore-errors 
    (mapcar 'delete-file 
	   (directory-files 
	    thumbs-temp-dir 
	    t 
	    thumbs-temp-prefix-regexp)))
  (let ((buffer (current-buffer)))
    (thumbs-remove-image)
    (ignore-errors (delete-window (selected-window)))
    ;; I must find another way to do this.
    (kill-buffer buffer)))

(defun thumbs-show-image-num (num)
  (interactive "nNumber: ")
  (setq buffer-read-only nil)
  (thumbs-remove-image)
  (delete-region (point-min)(point-max))
  (let ((i (cdr (assoc num thumbs-fileL))))
    (thumbs-insert-image i (thumbs-image-type i) 0)
    (sleep-for 2)
    (rename-buffer (concat "*Image: " 
			   (file-name-nondirectory i) 
			   " - " 
			   (number-to-string num) "*"))
    (setq thumbs-image-num num)
    (setq thumbs-current-image-filename i
	  thumbs-current-tmp-filename nil
	  buffer-read-only t)))

(defun thumbs-next-image ()
  (interactive)
  (thumbs-show-image-num (+ thumbs-image-num 1)))

(defun thumbs-previous-image ()
  (interactive)
  (thumbs-show-image-num (- thumbs-image-num 1)))

;; Image modification routines

(defvar thumbs-convert-command-list)
(setq thumbs-convert-command-list 
      (list 
       (list "adjoin")             
       (list "affine matrix")      
       (list "antialias")          
       (list "append")             
       (list "average")            
       (list "blur geometry")      
       (list "border geometry")    
       (list "bordercolor color")  
       (list "box color")          
       (list "cache threshold")    
       (list "channel type")       
       (list "charcoal radius")    
       (list "coalesce")           
       (list "colorize value")     
       (list "colors value")       
       (list "colorspace type")    
       (list "comment string")     
       (list "compress type")      
       (list "contrast")           
       (list "crop geometry")      
       (list "cycle amount")       
       (list "delay value")        
       (list "deconstruct")        
       (list "density geometry")   
       (list "depth value")        
       (list "despeckle")          
       (list "display server")     
       (list "dispose method")     
       (list "dither")             
       (list "draw string")        
       (list "edge radius")        
       (list "emboss radius")      
       (list "enhance")            
       (list "equalize")           
       (list "fill color")         
       (list "filter type")        
       (list "flatten")            
       (list "flip")               
       (list "flop")               
       (list "font name")          
       (list "frame geometry")     
       (list "fuzz distance")      
       (list "gamma value")        
       (list "geometry geometry")  
       (list "gaussian geometry")  
       (list "gravity type")       
       (list "implode amount")     
       (list "intent type")        
       (list "interlace type")     
       (list "label name")         
       (list "list type")          
       (list "loop iterations")    
       (list "map filename")       
       (list "matte")              
       (list "median radius")      
       (list "modulate value")     
       (list "monochrome")         
       (list "morph value")        
       (list "mosaic")             
       (list "negate")             
       (list "noise")              
       (list "normalize")          
       (list "opaque color")       
       (list "page geometry")      
       (list "paint radius")       
       (list "ping")               
       (list "pointsize value")    
       (list "preview type")       
       (list "profile filename")   
       (list "quality value")      
       (list "raise value")        
       (list "region geometry")    
       (list "roll geometry")      
       (list "rotate degrees")     
       (list "sample geometry")    
       (list "scale geometry")     
       (list "scene value")        
       (list "segment values")     
       (list "seed value")         
       (list "shade degrees")      
       (list "sharpen geometry")   
       (list "shave geometry")     
       (list "shear geometry")     
       (list "size geometry")      
       (list "solarize threshold") 
       (list "spread amount")      
       (list "stroke color")       
       (list "strokewidth value")  
       (list "swirl degrees")      
       (list "texture filename")   
       (list "threshold value")    
       (list "tile filename")      
       (list "transparent color")  
       (list "treedepth value")    
       (list "type type")          
       (list "units type")         
       (list "verbose")            
       (list "view")               
       (list "wave geometry")))


(defun thumbs-modify-image-interactive (action)
  "Call convert to modify the image."
  (interactive 
   (list 
    (let ((args "")
	  (arg  "")
	  (count 0))
      (while (not 
	      (equal 
	       "" 
	       (progn
		 (setq count (1+ count))
		 (setq arg 
		       (completing-read 
			(concat "Arg[" (number-to-string count) "] (Enter for finish): ")
			thumbs-convert-command-list)))))
	(if (equal count 1)
	    (setq args (concat args arg))
	  (setq args (concat args " -" arg))))
      args)))
  ;; cleaning of old temp file
  (setq buffer-read-only nil)
  (thumbs-remove-image)
  (delete-region (point-min)(point-max))
  (let* ((type (or (thumbs-image-type thumbs-current-image-filename) 'jpeg))
	 (ext  (if (string-match (image-file-name-regexp) thumbs-current-image-filename)
		   (match-string 1 thumbs-current-image-filename)
		 "jpg"))
	 (tmp (format "%s%s.%s" thumbs-temp-file (gensym) ext)))
    (thumbs-call-convert (or thumbs-current-tmp-filename 
			     thumbs-current-image-filename)
			 tmp
			 action nil type)
    (thumbs-insert-image tmp type 0)
    (setq thumbs-current-tmp-filename tmp))
  (setq buffer-read-only t))


(defun thumbs-modify-image (action &optional arg)
  "Call convert to modify the image."
  (interactive "sAction: \nsValue: ")
  ;; cleaning of old temp file
  (setq buffer-read-only nil)
  (thumbs-remove-image)
  (delete-region (point-min)(point-max))
  (let* ((type (or (thumbs-image-type thumbs-current-image-filename) 'jpeg))
	 (ext  (if (string-match (image-file-name-regexp) thumbs-current-image-filename)
		   (match-string 1 thumbs-current-image-filename)
		 "jpg"))
	 (tmp (format "%s%s.%s" thumbs-temp-file (gensym) ext)))
    (thumbs-call-convert (or thumbs-current-tmp-filename 
			     thumbs-current-image-filename)
			 tmp
			 action
			 (or arg "") type)
    (thumbs-insert-image tmp type 0)
    (setq thumbs-current-tmp-filename tmp))
  (setq buffer-read-only t))

(defun thumbs-emboss-image (emboss)
  "Emboss the image."
  (interactive "nEmboss value: ")
  (if (or (< emboss 3)(> emboss 31)(evenp emboss))
      (error "Arg must be a odd number between 3 and 31"))
  (thumbs-modify-image "emboss" (number-to-string emboss)))

(defun thumbs-monochrome-image ()
  "Turn the image to monochrome."
  (interactive)
  (thumbs-modify-image "monochrome"))

(defun thumbs-negate-image ()
  "Negate the image."
  (interactive)
  (thumbs-modify-image "negate"))

(defun thumbs-forward-char ()
  (interactive)
  (forward-char)
  (message (cdr (assoc (point) thumbs-fileL))))

(defun thumbs-backward-char ()
  (interactive)
  (forward-char -1)
  (message (cdr (assoc (point) thumbs-fileL))))

(defun thumbs-save-current-image ()
  (interactive)
  (let ((f (or thumbs-current-tmp-filename 
	       thumbs-current-image-filename))
	(sa (read-from-minibuffer "save file as: " 
				  thumbs-current-image-filename)))
    (copy-file f sa)))

;; thumbs-mode

(define-derived-mode thumbs-mode text-mode "thumbs")
(setq thumbs-mode-map (make-sparse-keymap))

(define-key thumbs-mode-map [return]
  'thumbs-find-image-at-point)
(define-key thumbs-mode-map [(meta return)]
  'thumbs-find-image-at-point-other-window)
(define-key thumbs-mode-map [(control return)]
  'thumbs-set-image-at-point-to-root-window)
(define-key thumbs-mode-map [delete]
  'thumbs-delete-image-at-point)
(define-key thumbs-mode-map [right] 'thumbs-forward-char)
(define-key thumbs-mode-map [left] 'thumbs-backward-char)
(define-key thumbs-mode-map [(control right)]
  #'(lambda () (interactive)(goto-char (+ (point) 10))
      (message (cdr (assoc (point) thumbs-fileL)))))
(define-key thumbs-mode-map [(control left)]
  #'(lambda () (interactive)(goto-char (- (point) 10))
      (message (cdr (assoc (point) thumbs-fileL)))))

(define-key thumbs-mode-map "d"
  #'(lambda ()
      (interactive)
      (dired-other-window thumb-current-dir)))

(define-key thumbs-mode-map "g"
  #'(lambda ()
      (interactive)
      (thumbs-show-all-from-dir thumb-current-dir)))
(define-key thumbs-mode-map "s"
  #'(lambda ()
      (interactive)
      (message (cdr (assoc (point) thumbs-fileL)))))
(define-key thumbs-mode-map "q" 'thumbs-kill-buffer)

;; thumbs-view-image-mode
(define-derived-mode thumbs-view-image-mode
  text-mode "image-view-mode")
(setq thumbs-view-image-mode-map
      (make-sparse-keymap))

(define-key thumbs-view-image-mode-map [prior]
  'thumbs-previous-image)
(define-key thumbs-view-image-mode-map [next]
  'thumbs-next-image)
(define-key thumbs-view-image-mode-map "-"
  'thumbs-resize-image-size-down)
(define-key thumbs-view-image-mode-map "+"
  'thumbs-resize-image-size-up)
(define-key thumbs-view-image-mode-map "="
  'thumbs-resize-image-to-window)

(define-key thumbs-view-image-mode-map "M"
  'thumbs-modify-image-interactive)

(define-key thumbs-view-image-mode-map "e"
  'thumbs-emboss-image)
(define-key thumbs-view-image-mode-map "m"
  'thumbs-monochrome-image)

(define-key thumbs-view-image-mode-map "r"
  'thumbs-resize-interactive)
(define-key thumbs-view-image-mode-map "q"
  'thumbs-kill-buffer)
(define-key thumbs-view-image-mode-map "w"
  #'(lambda () (interactive)(thumbs-call-Esetroot (or thumbs-current-tmp-filename
						      thumbs-current-image-filename))))
(define-key thumbs-view-image-mode-map "s" 
  'thumbs-save-changes)

;; Modifications to dired-mode.

(defvar thumbs-show-preview nil)
(defvar thumbs-preview-buffer-name "*Preview*")
(defvar thumbs-preview-buffer-size 20)
(defconst thumbs-remote-file-regexp "^/[^/:]*:"
  "Regexp to match for remote filename.")

(defun thumbs-current-dir-is-remote ()
  (cond ((and (dired-current-directory)
              (string-match 
	       thumbs-remote-file-regexp 
	       (dired-current-directory))) 
	 t)))

(defun toggle-thumbs-show-preview ()
  (interactive)
  (if thumbs-show-preview
      (let ((w (get-buffer-window thumbs-preview-buffer-name)))
	(if w (progn (kill-buffer thumbs-preview-buffer-name)
		     (delete-window w)))))
  (setq thumbs-show-preview (not thumbs-show-preview))
  (thumbs-dired-show-preview))

(defun thumbs-dired-show-preview ()
  (if (and thumbs-show-preview (not (thumbs-current-dir-is-remote)))
      (let ((thumb-buffer-name
	     thumbs-preview-buffer-name)
	    (f (dired-get-filename))
	    (old-buf (current-buffer)))
	(if (string-match (image-file-name-regexp) f)
	    (progn
	      (if (get-buffer-window thumb-buffer-name)
		  (pop-to-buffer thumb-buffer-name)
		(progn
		  (split-window
		   (get-buffer-window (current-buffer))
		   (- (window-width)
		      thumbs-preview-buffer-size) t)
		  (select-window (next-window))
		  (switch-to-buffer thumb-buffer-name)))
	      (progn
		(thumbs-remove-image)
		(delete-region (point-min)(point-max))
		(thumbs-insert-thumb f)
		(pop-to-buffer old-buf t)))
	  (thumbs-kill-preview-buffer-and-window)))))

(defun thumbs-kill-preview-buffer-and-window ()
  (interactive)
  (if (buffer-live-p (get-buffer thumbs-preview-buffer-name))
      (let ((ob (current-buffer)))
	(pop-to-buffer thumbs-preview-buffer-name)
	(kill-buffer thumbs-preview-buffer-name)
	(delete-window)
	(pop-to-buffer ob))))

(defadvice dired-next-line (after show-thumbnail (arg))
  (thumbs-dired-show-preview))

(defadvice dired-previous-line (after show-thumbnail (arg))
  (thumbs-dired-show-preview))

(defadvice dired-advertised-find-file (before winkill)
  (thumbs-kill-preview-buffer-and-window))

(ad-activate 'dired-next-line)
(ad-activate 'dired-previous-line)
(ad-activate 'dired-advertised-find-file)

(define-key dired-mode-map "I" 'toggle-thumbs-show-preview)

(define-key dired-mode-map "T"
  #'(lambda ()
      (interactive)
      (thumbs-find-thumb (dired-get-filename))
      (setq buffer-read-only t)
      ))

(define-key dired-mode-map "\C-t"
  #'(lambda()
      (interactive)
      (thumbs-show-all-from-dir (dired-current-directory))))
(define-key dired-mode-map "\M-t"
  #'(lambda()
      (interactive)
      (thumbs-show-thumbs-list
       (dired-get-marked-files)
       (concat "Thumbs : MARKED from " (dired-current-directory)))))
(define-key dired-mode-map "q"
  #'(lambda ()
      (interactive)
      (let ((w (get-buffer-window thumbs-preview-buffer-name)))
	(if w
	    (progn
	      (kill-buffer thumbs-preview-buffer-name)
	      (delete-window w)))
	(dired-quit))))
(define-key dired-mode-map "w"
  #'(lambda ()
      (interactive)
      (thumbs-call-Esetroot (dired-get-filename))))



(provide 'thumbs-xmas)

;;; thumbs-xmas.el ends here
