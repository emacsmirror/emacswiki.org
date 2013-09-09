;;; doxy-preview.el --- Preview doxygen special commands (e.g. dot, msc)

;; Author: Samuel DA MOTA <da.mota.sam@gmail.com>
;; Keywords: programming, documentation, doxygen

;; License: coffee/beer license
;;
;; If you like this code and would like to thank me, you can buy me a
;; beer or a coffee (or simply send me an email to say "thanks").
;; You can do whatever you want with this file.

;;; Commentary:

;; doxy-preview is a minor mode that displays images in place of some
;; doxygen comments like dot, msc, or latex formula.
;;
;; e.g: on the following code
;;
;; /*!
;;  * \dot 
;;  *   digraph G {
;;  *     "A" -> "B" [ label = "C" ];
;;  *   }
;;  * \enddot
;;  */
;;
;; the part between \dot and \enddot will be replaced by the
;; corresponding image
;;
;; This minor mode is best used with doc-mode
;; (http://nschum.de/src/emacs/doc-mode/). Indeed doc-mode's "folding
;; documentation" feature comes in a handy when displaying images as
;; they take lots of place in the buffer.
;;
;; You should have received another file named make-image.sh along
;; with this one. Put it anywhere pointed to by the variable ${PATH}
;; and make it executable.
;;
;; Also, note that albeit not handled by doxygen, it would be very
;; straightforward to add support for other graph languages like
;; ditaa, plantuml, graph-easy, yuml, scruffy, glumly, SDML,
;; Trace2UML, blockdiag, seqdiag, actdiag, nwdiag, ...
;;
;; This mode was higly inspired by iimage mode (almost a copy-paste).
;;
;; Requirements:
;;   dot (for \dot .. \enddot commands)
;;   mscgen (for \msc .. \endmsc commands)
;;   latex & dvipng (for \f$ .. \f$, \f[ .. \f] commands)
;;   imagemagick (to revert colors - optional)

;;; Code:

(defgroup doxy-preview nil
  "Support for previewing some doxygen commands."
  :version "24.1"
  :group 'doxy-preview)

(defconst doxy-preview-version "0.1")
(defvar doxy-preview-mode nil)
(defvar doxy-preview-mode-map nil)

;; Set up key map.
(unless doxy-preview-mode-map
  (setq doxy-preview-mode-map (make-sparse-keymap))
  ;; TODO find a better key map as it is common to have \C-l bound to
  ;; "center cursor"
  (define-key doxy-preview-mode-map "\C-l" 'doxy-preview-redraw))


(defvar doxy-preview-mode-temp-dir nil)

(defun doxy-preview-delete-temp-dir ()
  "deletes the temporary directory used for the images"
  (if doxy-preview-mode-temp-dir
      (progn
	(delete-directory doxy-preview-mode-temp-dir t)
	(setq doxy-preview-mode-temp-dir nil))))

;;;###autoload
(defun doxy-preview-mode-on ()
  "Unconditionally turn on doxy-preview mode."
  (interactive)
  (doxy-preview-delete-temp-dir)
  (setq doxy-preview-mode-temp-dir 
	(make-temp-file "doxy-preview-mode-make-image" t))
  (doxy-preview-mode-buffer t)
  (setq doxy-preview-mode t))

(defun doxy-preview-mode-off ()
  "Unconditionally turn off doxy-preview mode."
  (interactive)
  (doxy-preview-delete-temp-dir)
  (doxy-preview-mode-buffer nil)
  (setq doxy-preview-mode nil))

(defun doxy-preview-redraw ()
  "redraw the images"
  (interactive)
  (doxy-preview-mode-off)
  (doxy-preview-mode-on))

(defun doxy-preview-mode-temp-file (str)
  "creates a temporary file inside doxy-preview temp dir"
  (let ((file-template (expand-file-name str doxy-preview-mode-temp-dir)))
    (make-temp-file file-template)))

(defun doxy-preview-mode-make-image (str filename)
  (let* ((content-file (doxy-preview-mode-temp-file "random-stuff")))
    (with-temp-file content-file
      (insert str))

    (if (not (eq 0 (shell-command
		    ;; the creation of the image is left to another
		    ;; program to keep this mode minimalist and easy
		    ;; to hack on
		    (concat "make-image.sh " content-file " " filename))))
	(error "processing error"))

    (delete-file content-file)))

    
(defcustom doxy-preview-mode-image-regex-alist
  `(
    ;; \dot \enddot
    ("\\([@\\]dot\\(.\\|\n\\)*?\\(?:[@\\]enddot\\)\\)" . 1)

    ;; \msc \endmsc
    ("\\([@\\]msc\\(.\\|\n\\)*?\\(?:[@\\]endmsc\\)\\)" . 1)

    ;; \f$ \f$
    ("\\([@\\]f\\$\\(.\\|\n\\)*?[@\\]f\\$\\)" . 1)

    ;; \f[ \f]
    ("\\(\\(?:[@\\]f\\[\\)\\(.\\|\n\\)*?\\(?:\\f]\\)\\)" . 1)

    ;; \f{environment}{ .. \f}
    ("\\(\\(?:[@\\]f{[^}]+}{?\\)\\(.\\|\n\\)*?\\(?:\\f}\\)\\)" . 1)

    ;; \image
    ("\\([@\\]image\\(.\\)*\\)" . 1)

   )
  
  "A list of regexp that matches doxygen special to be replace by their images"  
  :type '(alist :key-type regexp :value-type integer)
  :group 'doxy-preview

)


(defun doxy-preview-mode-buffer (arg)
  "Display/undisplay images.
If arg is non nil, display images; hide otherwise"
  (interactive)
  (let ((modp (buffer-modified-p (current-buffer)))
	file buffer-read-only)
    (save-excursion
      (dolist (pair doxy-preview-mode-image-regex-alist)
	(goto-char (point-min))
	(while (re-search-forward (car pair) nil t)
	  (if arg
	      (progn
		(let ((output-image-file (doxy-preview-mode-temp-file "tmp_image")))	

		  ;; create the image file
		  (doxy-preview-mode-make-image (match-string (cdr pair)) output-image-file)

		  ;; display it
		  ;; FIXME: we don't mark our images, so we can't reliably
		  ;; remove them either (we may leave some of ours, and we
		  ;; may remove other packages's display properties).
		  (add-text-properties (match-beginning 0) (match-end 0)
				       (list 'display (create-image output-image-file)))
		  ))
	    (remove-text-properties (match-beginning 0) (match-end 0)
				    '(display))))))
    (set-buffer-modified-p modp)))


;;;###autoload
(define-minor-mode doxy-preview nil
  "Preview doxygen dot/msc/latex formula documentations."
  :group 'doxy-preview :lighter " doxy-prev" :keymap doxy-preview-mode-map
  (if doxy-preview-mode
      (doxy-preview-mode-on)
    (doxy-preview-mode-off)))

(provide 'doxy-preview)
