;;; mutt.el --- Use Emacs 20 as an external editor for the Mutt mailer
;; Copyright 1998 Eric Kidd

;; Author: Eric Kidd <eric.kidd@pobox.com>
;; Version: $Revision: 1.4 $

;; This is free software distributed under the GPL, yadda, yadda, yadda.
;; It has no warranty. See the GNU General Public License for more
;; information. Send me your feature requests and patches, and I'll try
;; to integrate everything.

;;; Commentary:

;; This is a major mode for use with Mutt, the spiffy *nix mailreader
;; du jour. See <http://www.cs.hmc.edu/~me/mutt/index.html>. To use this
;; mode, add the following line to the .emacs file in your home directory:
;;
;;   (load "/your/local/path/to/this/file/mutt")
;;
;; Note that you can omit the ".el" from the file name when calling load.
;;
;; If you want to make it available to all your users, type \C-h v
;; load-path RET, pick an appropriate directory for mutt.el, and modify
;; your sitewide default.el to (require 'mutt).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Thanks
;;;
;;; Dave Pearson: Code, feature ideas, Mutt experience. Many thanks!
;;; Louis Theran: Encouragement to make Mutt mode work like Emacs MUAs.
;;; Ronald: Enlightening gripes about what Emacs should do, but doesn't.
;;; Robert Napier: Bug reports about font-lock mode, fancy wrapping.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Revision History
;;;
;;; $Log: mutt.el,v $
;;; Revision 1.4  1998/04/11 00:05:46  emk
;;; Fixed font-lock bug. Also made mutt-mode a little more careful about
;;; saving various bits of Emacs state when moving around the buffer.
;;;
;;; Revision 1.3  1998/03/25 00:37:36  emk
;;; Added support for menus and font-lock mode, plus a few bug fixes.
;;;
;;; Revision 1.2  1998/03/24 13:19:46  emk
;;; Major overhaul--more commands, a minor mode for header editing, and other
;;; desirable features. Attaching files seems to be broken, though.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Required Packages

(require 'derived)
(require 'cl) ; Big but featureful. Do we need this?
(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customization Support
;;;
;;; Set up our customizable features. You can edit these (and lots of other
;;; fun stuff) by typing M-x customize RET. The Mutt preferences can be
;;; found under the [Applications] [Mail] category.

(defgroup mutt nil
  "Composing e-mail messages with Mutt.
Emacs can run as an external editor for Mutt, the spiffy Unix mail reader
du jour. You can get Mutt from <http://www.cs.hmc.edu/~me/mutt/index.html>."
  :group 'mail)

(defcustom mutt-uses-fill-mode t
  "*Specifies whether Mutt should automatically wrap lines.
Set this to t to enable line wrapping, and nil to disable line
wrapping. Note that if a paragraph gets messed up (the line wrapper
is very primitive), you can type \\[fill-paragraph] to rewrap the paragraph."
  :type 'boolean
  :group 'mutt)

(defcustom mutt-signature-pattern "\\(--\\|Cheers,\\| \\)"
  "*Pattern for identifying signatures.
Mutt uses this to locate signatures. It should contain no leaading or
trailing whitespace."
  :type 'string
  :group 'mutt)

(defcustom mutt-file-pattern "mutt-[a-z]+-[0-9]+-[0-9]+\\'"
  "*Regular expression which matches Mutt's temporary files."
  :type 'string
  :group 'mutt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable Faces
;;; The dark background versions are probably uglier than the light
;;; (which I use). If you find a more attractive, subdued color scheme,
;;; please mail it to me.

(defgroup mutt-faces nil
  "Typefaces used for composing messages with Mutt."
  :group 'mutt
  :group 'faces)

(defface mutt-header-keyword-face
  '((((class color)
      (background light))
     (:foreground "Navy" :bold t))
    (((class color)
      (background dark))
     (:foreground "LightBlue" :bold t))
    (t
     (:bold t)))
  "Face used for displaying keywords (e.g. \"From:\") in headers."
  :group 'mutt-faces)

(defface mutt-header-value-face
  '((((class color)
      (background light))
     (:foreground "MidnightBlue"))
    (((class color)
      (background dark))
     (:foreground "LightSteelBlue")))
  "Face used for displaying the values of headers."
  :group 'mutt-faces)

(defface mutt-quoted-text-face
  '((((class color)
      (background light))
     (:foreground "Sienna" :italic t))
    (((class color)
      (background dark))
     (:foreground "Wheat" :italic t))
    (t
     (:bold t :italic t)))
  "Face used for displaying text which has been quoted (e.g. \">foo\")."
  :group 'mutt-faces)

(defface mutt-multiply-quoted-text-face
  '((((class color)
      (background light))
     (:foreground "Firebrick" :italic t))
    (((class color)
      (background dark))
     (:foreground "Tan" :italic t))
    (t
     (:italic t)))
  "Face used for text which has been quoted more than once (e.g. \">>foo\")."
  :group 'mutt-faces)

(defvar mutt-font-lock-keywords
  '(("^\\([A-Z][-A-Za-z0-9.]+:\\)\\(.*\\)$"
     (1 'mutt-header-keyword-face)
     (2 'mutt-header-value-face))
    ("^[ \t\f]*\\(>[ \t\f]*[^ \t\f>].*\\)$"
     (1 'mutt-quoted-text-face))
    ("^[ \t\f]*\\(>[ \t\f]*\\)\\(>.*\\)$"
     (1 'mutt-quoted-text-face)
     (2 'mutt-multiply-quoted-text-face)))
  "Highlighting rules for message mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interactive Commands

(defun mutt-save-current-buffer-and-exit ()
  "Save the current buffer and exit Emacs."
  (interactive)
  (basic-save-buffer)

  ;; Added by Rob Reid 10/13/1998 to prevent accumulating *Composing* buffers
  ;; when using (emacs|gnu)client.  Helped by Eric Marsden's Eliza example in
  ;; http://www.ssc.com/lg/issue29/marsden.html
  (server-edit)

  (let ((buf (get-buffer "*Composing*")))
    (if buf (kill-buffer buf)))
  )

(defun mutt-delete-quoted-signatures ()
  "Delete quoted signatures from buffer."
  (interactive)
  (goto-char (point-min))
  (flush-lines (concat "^\\([ \t\f]*>[ \t\f>]*\\)"
		       mutt-signature-pattern
		       "[ \t\f]*\\(\n\\1.*\\)*")))

(defun mutt-delete-old-citations ()
  "Delete citations more than one level deep from buffer."
  (interactive)
  (goto-char (point-min))
  (flush-lines "^[ \t\f]*>[ \t\f]*>[ \t\f>]*"))

(defun mutt-goto-body ()
  "Go to the beginning of the message body."
  (interactive)
  (goto-char (point-min))
  ;; If the message has headers, slide downward.
  (and headers-mode
       (save-match-data (re-search-forward "^$" nil t))
       (next-line 1)))

(defun mutt-goto-signature ()
  "Go to the beginning of the message signature."
  (interactive)
  (goto-char (point-max))
  (and (save-match-data
	 (re-search-backward (concat "^" mutt-signature-pattern
				     "[ \t\f]*$")
			     nil t))
       (previous-line 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mutt Mode Meat

(define-derived-mode mutt-mode text-mode "Mutt"
  "Major mode for composing E-mail with the Mutt mailer.
To customize it, type \\[customize] and select [Applications] [Mail] [Mutt].
When you finish editing this message, type \\[mutt-save-current-buffer-and-exit] to save and exit Emacs.

\\{mutt-mode-map}"

  (rename-buffer "*Composing*" t)
  (auto-fill-mode (if mutt-uses-fill-mode 1 0))

  ;; Make Emacs smarter about wrapping citations and paragraphs.
  ;; We probably can't handle Supercited messages, though.
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start
	"\\([ \t\n\f]+[^ \t\n\f>]\\|[ \t\f>]*$\\)"
	paragraph-separate
	"[ \t\f>]*$")

  ;; If Mutt passed us headers, activate the necessary commands.
  (when (looking-at "^[-A-Za-z0-9]+:")
    (headers-mode 1))

  ;; Our temporary file lives in /tmp. Yuck! Compensate appropriately.
  (make-local-variable 'backup-inhibited)
  (setq backup-inhibited t)
  (cd "~")

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mutt-font-lock-keywords t))

  (mutt-goto-body)
  (message (substitute-command-keys "Type \\[describe-mode] for help composing; \\[mutt-save-current-buffer-and-exit] when done.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mutt Headers Mode

(defvar headers-mode nil)

(defun headers-mode (&optional arg)
  "Commands for editing the headers of an e-mail or news message.

\\{headers-mode-map}"

  (interactive "P")
  (make-local-variable 'headers-mode)
  (setq headers-mode
	(if (null arg)
	    (not headers-mode)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defvar headers-mode-map (make-sparse-keymap)
  "Keymap used for editing RFC822 headers.")

(defun headers-position-on-value ()
  (beginning-of-line)
  (skip-chars-forward "-A-Za-z0-9:")
  ;; XXX - Should make sure we stay on line.
  (forward-char))

(defun headers-goto-field (field)
  (let ((case-fold-search t))
    (goto-char (point-min))
    (save-match-data
      (when (re-search-forward (concat "^\\($\\|" field ": \\)"))
	(if (looking-at "^$")
	    (progn
	      (insert-string field ": \n")
	      (forward-char -1))
	  (headers-position-on-value))))))

(defmacro define-header-goto (name header)
  `(defun ,name ()
     ,(concat "Position the cursor on the " header ": header.")
     (interactive)
     (headers-goto-field ,header)))

(define-header-goto headers-goto-to "To")
(define-header-goto headers-goto-cc "Cc")
(define-header-goto headers-goto-fcc "Fcc")
(define-header-goto headers-goto-summary "Summary")
(define-header-goto headers-goto-keywords "Keywords")
(define-header-goto headers-goto-subject "Subject")
(define-header-goto headers-goto-bcc "Bcc")
(define-header-goto headers-goto-reply-to "Reply-To")
(define-header-goto headers-goto-from "From")
(define-header-goto headers-goto-organization "Organization")

(defun headers-attach-file (file description)
  "Attach a file to the current message (works with Mutt)."
  (interactive "fAttach file: \nsDescription: ")
  (when (> (length file) 0)
    (save-excursion
      (save-match-data
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (search-forward-regexp "^$")
	  (insert-string (concat "Attach: " (file-truename file) " "
				 description "\n"))
	  (message (concat "Attached '" file "'.")))))))

(or (assq 'headers-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(headers-mode " Headers") minor-mode-alist)))

(or (assq 'headers-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'headers-mode headers-mode-map)
		minor-mode-map-alist)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key Bindings

(define-key mutt-mode-map "\C-c\C-c" 'mutt-save-current-buffer-and-exit)
(define-key mutt-mode-map "\C-c\C-d\C-s" 'mutt-delete-quoted-signatures)
(define-key mutt-mode-map "\C-c\C-d\C-c" 'mutt-delete-old-citations)
(define-key mutt-mode-map "\C-c\C-b" 'mutt-goto-body)
(define-key mutt-mode-map "\C-c\C-i" 'mutt-goto-signature)

(define-key headers-mode-map "\C-c\C-f\C-t" 'headers-goto-to)
(define-key headers-mode-map "\C-c\C-f\C-c" 'headers-goto-cc)
(define-key headers-mode-map "\C-c\C-f\C-w" 'headers-goto-fcc)
(define-key headers-mode-map "\C-c\C-f\C-u" 'headers-goto-summary)
(define-key headers-mode-map "\C-c\C-f\C-k" 'headers-goto-keywords)
(define-key headers-mode-map "\C-c\C-f\C-s" 'headers-goto-subject)
(define-key headers-mode-map "\C-c\C-f\C-b" 'headers-goto-bcc)
(define-key headers-mode-map "\C-c\C-f\C-r" 'headers-goto-reply-to)
(define-key headers-mode-map "\C-c\C-f\C-f" 'headers-goto-from)
(define-key headers-mode-map "\C-c\C-f\C-o" 'headers-goto-organization)
(define-key headers-mode-map "\C-c\C-a" 'headers-attach-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Menus

(easy-menu-define
 mutt-mode-menu mutt-mode-map "Mutt Message Composition Commands." 
 '("Mutt"
   ["Delete Quoted Signatures" mutt-delete-quoted-signatures t]
   ["Delete Doubly-Quoted Text" mutt-delete-old-citations t]
   "----"
   ["Go To Body of Message" mutt-goto-body t]
   ["Go To Signature of Message" mutt-goto-signature t]
   "----"
   ["Save Message and Return to Mutt" mutt-save-current-buffer-and-exit t]))

(easy-menu-define
 headers-mode-menu headers-mode-map "Header Editing Commands."
 '("Headers"
   ["Attach File..." headers-attach-file t]
   "----"
   ["Edit From Header" headers-goto-from t]
   ["Edit Subject Header" headers-goto-subject t]
   ["Edit To Header" headers-goto-to t]
   ["Edit Cc Header" headers-goto-cc t]
   ["Edit Bcc Header" headers-goto-bcc t]
   ["Edit Fcc Header" headers-goto-fcc t]
   ["Edit Reply-To Header" headers-goto-reply-to t]
   ["Edit Summary Header" headers-goto-summary t]
   ["Edit Keywords Header" headers-goto-keywords t]
   ["Edit Organization Header" headers-goto-organization t]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finish Installing Mutt Mode

(unless (assq mutt-file-pattern auto-mode-alist)
  (setq auto-mode-alist
	(cons (cons mutt-file-pattern 'mutt-mode)
	      auto-mode-alist)))

(provide 'mutt)
