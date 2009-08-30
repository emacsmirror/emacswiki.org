;;; rmime.el --- read MIME messages

;; Author: Ray Moody <moody@cray.com>
;; Version: $Id: rmime.el,v 1.2 1996/05/30 02:24:58 moody Exp $
;; Keywords: MIME, mail

;;; Commentary:

;; RMIME provides MIME support for several Emacs message reading
;; packages.  RMIME has been designed with RMAIL in mind, but it has
;; also been tested with mh-e and VM.  It should work with most other
;; major modes as well.
;;
;; This is version 1.2 of RMIME.  It was released on May 29th, 1996.
;; For up to date information on the latest version of RMIME, please
;; see http://www.cinti.net/~rmoody/rmime.
;;
;; RMIME requires Emacs version 19.28 or greater.  Emacs version 19.29
;; or greater is required for full functionality.
;;
;; To use this file with RMAIL, add the following to your .emacs file:
;;
;;	(add-hook 'rmail-show-message-hook 'rmime-format)
;;	(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;;	(autoload 'rmime-format "rmime" "" nil)
;;
;; To use this file with mh-e, add the following to your .emacs file:
;;
;;	(add-hook 'mh-show-mode-hook 'rmime-format)
;;	(autoload 'rmime-format "rmime" "" nil)
;;
;; To use this file with VM, add the following to your .emacs file:
;;
;;	(setq vm-preview-lines t)
;;	(add-hook 'vm-select-message-hook 'rmime-format)
;;	(autoload 'rmime-format "rmime" "" nil)
;;
;; To use this file manually, add the following to your .emacs file:
;;
;;	(autoload 'rmime-mode   "rmime" "" t)
;;
;; -----------------------------------------------------------------
;;
;;		       Defining new MIME types
;;
;; Most new MIME types need only to be defined to metamail.  RMIME
;; only needs to know about MIME types that should be displayed as
;; text in an Emacs buffer.  New MIME types can be defined for
;; metamail by placing entries in $HOME/.mailcap or in a system wide
;; mailcap file.  This is an important feature of RMIME --- most MIME
;; aware packages get their configuration out of mailcap files and so
;; should Emacs.
;;
;; To define new text MIME types to Emacs, define a function in the
;; form:
;;
;; (defun rmime-$(BASETYPE)/$(SUBTYPE) (content type encoding
;;					disposition description) ...)
;;
;; CONTENT is an opaque structure that should only be used with
;; rmime-content-excursion or rmime-insert-content or
;; rmime-insert-unencoded-content or rmime-rmime-write-content.
;;
;; TYPE is a string taken from the Content-Type field, or nil if the
;; Content-Type field was empty or not supplied.  Comments and
;; newlines followed by whitespace have been removed from the string.
;; The string may be passed to rmime-parameters for further parsing.
;;
;; ENCODING is a string taken from the Content-Encoding field.  It has
;; been converted to lowercase and is never nil.  The empty string is
;; used if the field is empty or not supplied.
;;
;; DISPOSITION is a string taken from the Content-Disposition field,
;; or nil if the Content-Disposition field was empty or not supplied.
;; Comments and newlines followed by whitespace have been removed from
;; the string.  The string may be passed to rmime-parameters for
;; further parsing.
;;
;; DESCRIPTION is a string taken from the Content-Description field,
;; or nil if the Content-Description field was empty or not supplied.
;;
;; If the rmime-$(BASETYPE)/$(SUBTYPE) symbol has the rmime-unwise
;; property set, it will not be called while formatting a
;; Multipart/Alternative body part except as a last resort.  This
;; property should be set for MIME types that don't produce text or
;; produce text that doesn't display well in an Emacs buffer.  This
;; property should also be set when SUBTYPE is "*".  Also, the
;; Multipart/Alternative formatting code will backup if a error symbol
;; containing rmime-unknown-conventions is signaled.
;;
;; The formatting function should insert the MIME body part into the
;; Emacs buffer as best as possible.
;;
;; The formatting function should return a list of four element lists:
;;		((point content type encoding)
;;		 (point content type encoding)
;;			...
;;		 (point content type encoding))
;; This list of four element lists is used to call metamail.  Metamail
;; deals with MIME body parts containing sounds, images, etc by
;; calling an external viewer.  If the MIME body part displays
;; perfectly in an Emacs buffer, metamail doesn't have to be called
;; and the list of four element lists can be the empty list.  Usually,
;; metamail needs to be called once for each MIME body part and the
;; list of four element lists contains exactly one four element list.
;; Multipart body parts may need to call metamail several times.  In
;; this case, the list of four element lists can contain many four
;; element lists.
;;
;; POINT is a marker showing where the rmime overlay arrow should
;; appear while metamail is showing this MIME body part (or nil if no
;; overlay arrow is desired)
;;
;; CONTENT is the opaque structure (the formatting function's first
;; parameter).
;;
;; TYPE is a character string describing the Content Type to metamail.
;; The formatting function's second parameter works fine here.
;;
;; ENCODING is a character string describing the Content Encoding to
;; metamail.  The formatting function's third parameter works fine
;; here.
;;
;; No function starting with "rmime-" and containing a slash should be
;; defined unless it is safe to call for an unauthenticated message.
;;
;; The MIME formatting should be independent of Emacs state (ie, fill
;; column) and of other MIME messages.  Once formatting is performed
;; on a message, it sticks even if the Emacs state changes or new,
;; associated MIME messages arrive.

;;; Code:

(defvar rmime-clean-files nil
  "*If non-nil, RMIME undoes MIME formatting before a buffer save")

(defvar rmime-immediate-play nil
  "*If non-nil, RMIME starts metamail immediatly when a message is formatted")

(defvar rmime-arrow #("-->" 0 3 (face rmime-arrow-face))
  "*Arrow indicator used during MIME play")

(defvar rmime-leader (make-string (length rmime-arrow) ? )
  "*Leader inserted before nontext MIME entities.")

;; Path to metamail.  Metamail is called when there is no Emacs lisp
;; function to handle a MIME type.
;;
;; Metamail can be found in ftp://ftp.bellcore.com/pub/nsb.
(defvar rmime-metamail-program "metamail"
  "Program to decode MIME messages with content-types unknown to Emacs")

;; Path to mimencode.  Mimencode will decode MIME messages with base64
;; or quoted-printable encodings.  Mimencode comes with metamail.
(defvar rmime-mimencode-program "mimencode"
  "Program to decode base64 or quoted-printable MIME messages")

;; Path to the uufilter program, or nil if uufilter is not available.
;; The uufilter program is an enhanced version of uudecode that will
;; write to stdout if it is given -s.  If you don't have uufilter, set
;; this variable to nil and don't worry about it.  This style of
;; encoding is deprecated.
(defvar rmime-uufilter-program nil
  "Program to decode uuencoded MIME messages.")

;; Path to ftp.  Ftp is called to get Message/External-Body data when
;; access-type=ftp or access-type=anon-ftp
(defvar rmime-ftp-program "ftp"
  "Program to transfer external-body messages with access-type=ftp")

;; These variables determine the appearance of multipart/digest messages.
(defvar rmime-digest-indentation "")
(defvar rmime-digest-recursive-indentation "   ")
(defvar rmime-digest-toc-intro "\t\t\t  Table Of Contents\n")
(defvar rmime-digest-item-intro #("\f\t\t\t  #################\n" 0 24 (face bold)))

;; List of transfer-encodings that don't need to be processed by
;; mimencode.  RMIME is a user agent so it can treat all of these
;; encodings exactly the same way.  The distinctions between these
;; encodings are only important to the transfer agent.
(defvar rmime-transparent-encodings '("" "7bit" "8bit" "binary"))

;; This magic string is inserted between the raw and formatted parts
;; of a message.  It is important when it appears in a auto save file
;; because it is the only way of knowing what part of a message needs
;; to be deleted and redone.
(defvar rmime-magic-string "\037 -=- MIME -=- \037\014\n")
(defvar rmime-magic-string-times2 (concat rmime-magic-string rmime-magic-string))
(defvar rmime-magic-regexp (regexp-quote rmime-magic-string))

;; This is the default regexp to separate message headers from the
;; message body.  We are being slightly fancy by permitting whitespace
;; characters.  This means that the header parsing code needs to know
;; that lines beginning with whitespace but containing no nonwhite
;; characters isn't a continuation line.  This pattern must end with a
;; newline instead of `$' because we expect point (and match-end) to
;; be the first character of the message body.
;;
;; Any major mode that needs to set a local value should also set
;; rmime-mimeinfo-method to rmime-standard-mimeinfo (or at least to a
;; routine that calls rmime-standard-mimeinfo as a subroutine).
(defvar rmime-separator-regexp "^[ \t]*\n")

;; Function to call to extract MIME information for the current
;; message in the current buffer.
;;
;; Buffers that don't hold an RFC 822 message (or that may mess around
;; with message headers) must assign a local value to this variable.
(defvar rmime-mimeinfo-method 'rmime-obsolete-mimeinfo)

;; rmime-tmp-buffer-name names the temporary buffer used to hold raw
;; MIME.  rmime-tmp-buffer-string is the raw MIME presently in the
;; temporary buffer.  We check to see if we have what we want in the
;; temporary buffer by running "eq" on rmime-tmp-string --- this
;; avoids character-by-character comparision.
;;
;; It is convenient to make rmime-tmp-string a buffer local variable
;; with a value of nil in every buffer except the tmp buffer.  This
;; way we can easily detect if someone killed the tmp buffer.
(defvar rmime-tmp-buffer-name " *rmime-tmp*")
(defvar rmime-tmp-string nil)
(make-variable-buffer-local 'rmime-tmp-string)

;; Highlight face for formatting message/rfc822 headers.
(defvar rmime-highlight-face
  (or (identity rmail-highlight-face)
      (if (face-differs-from-default-p 'bold) 'bold 'highlight)))

;; Default type for bodies that have no Content-Type header.
;;
;; Inside of rmime-multipart/digest, this variable has a temporary
;; binding of 'rmime-message/rfc822.
(defvar rmime-default-type 'rmime-text/plain)

;; This variable is only used when a metamail process is alive.  It is
;; a list of the body parts that still need to be passed off to
;; metamail.  Before rmime-play starts metamail, the play list is a
;; text property set on the last character of the message.
(defvar rmime-playlist nil)
(make-variable-buffer-local 'rmime-playlist)

;; The process displaying a body while a "play" command is doing its
;; thing.  Setting this variable changes the appearance of the mode
;; line to include a "Playing" indicator.  We are careful to mess
;; around with this indicator before the "RMIME" indicator so that the
;; "Playing" indicator appears second in the mode line.
(defvar rmime-process nil)
(make-variable-buffer-local 'rmime-process)
(or (assq 'rmime-process minor-mode-alist)
    (setq minor-mode-alist (cons '(rmime-process " Playing") minor-mode-alist)))

;; A boolean variable indicating if formatting is active or not in the
;; current message of the current buffer.  Setting this variable
;; changes the appearance of the mode line to include a "RMIME"
;; indicator.  We are careful to mess around with this indicator after
;; the "Playing" indicator so that the "Playing" indicator appears
;; first in the mode line.
(defvar rmime-mode nil)
(make-variable-buffer-local 'rmime-mode)
(or (assq 'rmime-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(rmime-mode " RMIME") minor-mode-alist)))

;; These variable control the reassembly of partial messages.
;; rmime-partial-partlist is an ordered association list suitable for
;; rmime-partial-1.  rmime-partial-parts is the total number of parts
;; expected (an integer) or nil if we haven't encountered a message
;; with a "TOTAL" parameter on it yet.
(defvar rmime-partial-partlist nil)
(defvar rmime-partial-parts nil)
(make-variable-buffer-local 'rmime-partial-partlist)
(make-variable-buffer-local 'rmime-partial-parts)

;; This hook is to write raw, unformatted MIME even if formatted MIME
;; is being displayed.  This is a global hook in case formatted MIME
;; is moved from one buffer to another.  rmail-output moves formatted
;; MIME from one buffer to another.
(add-hook 'write-region-annotate-functions 'rmime-annotate)

;; We use this handy macro whenever we are going to modify a buffer.
;; This macro evaluates its body after clearing read-only status and
;; disabling file locking.  Read-only status, modification status, and
;; file locking are restored to their original values after the body
;; has finished or in the event of an error.
;;
;; We would like to temporarily disable undo too (saves space and
;; time), but this isn't possible --- the data structure is such that
;; if we don't keep our own undo records around, all previous undo
;; records become worthless.
;;
;; The form produced by this macro looks like this:
;;
;; (let ((symbol28473a (buffer-modified-p))
;;       (inhibit-read-only t)
;;       (buffer-file-truename))
;;   (unwind-protect
;;       (progn body)
;;     (set-buffer-modified-p symbol28473a)))
(defmacro rmime-modify-buffer (&rest body)
  (let ((symbol (make-symbol "rmime-buffer-modified-flag")))
    (list 'let (cons (cons symbol '((buffer-modified-p))) '((inhibit-read-only t) (buffer-file-truename)))
	  (list 'unwind-protect (cons 'progn body)
		              (list 'set-buffer-modified-p symbol)))))
(put 'rmime-modify-buffer 'lisp-indent-function 0)

;; This macro evaluates its body with the restriction set to raw MIME
;; message contents.
;;
;; The form produced by this macro looks like this:
;;
;; (save-excrusion
;;  (apply 'rmime-tmp-buffer (car arguments))
;;  (body)
(defmacro rmime-content-excursion (arguments &rest body)
  (or (and (listp arguments) (eq (length arguments) 1))
      (error "Bad arguments to rmime-content-excursion"))
  (cons 'save-excursion (cons (list 'apply '(quote rmime-tmp-buffer) (car arguments)) body)))
(put 'rmime-content-excursion 'lisp-indent-function 1)

;; Ensure that rmime-markup-face is defined.
(or (member 'rmime-markup-face (face-list))
    (progn (make-face 'rmime-markup-face)
	   (set-face-foreground 'rmime-markup-face "blue")))

;; Ensure that rmime-arrow-face is defined.
(or (member 'rmime-arrow-face (face-list))
    (progn (make-face 'rmime-arrow-face)
	   (set-face-foreground 'rmime-arrow-face "red")))

(defun rmime-mode (&optional arg)
  "Toggle RMIME mode.
With arg, turn RMIME mode on if arg is positive, off otherwise.

RMIME mode is used to read MIME multimedia messages.  Multimedia
messages may contain images, audio clips, video clips and other
nontextual data as well as encoded and specially formatted text.  When
rmime-mode is active, messages will be searched for text which can be
displayed in an Emacs buffer.  Message bodies that cannot be displayed
in an Emacs buffer (such as sounds and images) will be represented by
a special indicator line.  Pressing C-c C-c on the indicator line will
start an external program to display the message.

Also see \\[play]"
  (interactive "P")
  (if (if (identity arg) (> (prefix-numeric-value arg) 0) (not rmime-mode))
      (rmime-format)
    (rmime-cancel)))

(defun rmime-format ()

  ;; See if we have formatted here before.  See if the formatting is
  ;; still valid.
  ;;
  ;; If this is the empty message, we couldn't possibly have formatted
  ;; here before because formatting never results in a empty message.
  ;; Moreover, if we have an empty message, we can't call
  ;; get-text-property without the possibility of an args-out-of-range
  ;; error.
  ;;
  ;; If we have formatted here before, the last character of the
  ;; message has the rmime-cache property set.  If we are certain that
  ;; the formatting is valid any position in any buffer, that property
  ;; is set to t.  Otherwise, that property is set to a marker where
  ;; the formatting is known to be valid.
  (if (and (not (eq (point-min) (point-max)))
	   (let ((cache (get-text-property (1- (point-max)) 'rmime-cache)))
	     (or (eq cache t)
		 (and (identity cache)
		      (eq (marker-buffer   cache) (current-buffer))
		      (eq (marker-position cache) (1- (point-max)))))))

      ;; We have formatted here before.  All we have to do is set the
      ;; indicator in the mode line.  (Chances are that the indicator
      ;; is set already, but it might not be if we just moved from an
      ;; unformatted message to a formatted message in the same
      ;; buffer, with, for example, rmail-next-undeleted-message.)
      (setq rmime-mode t)

    ;; The mimeinfo method may switch buffers or move the point.
    (save-excursion

      ;; Get MIME information for this buffer.  Different major modes
      ;; may have different ways of managing MIME information.
      (let ((mimeinfo (funcall rmime-mimeinfo-method)))

	;; It is tempting to set change-major-mode-hook here, but this
	;; breaks.  RMAIL calls change-major-mode-hook during buffer
	;; revert, and we don't want to trash all MIME formating.

	;; Any call to revert-buffer turns RMIME off.
	(if (fboundp 'make-local-hook)
	    (progn (make-local-hook 'before-revert-hook)
		   (add-hook 'before-revert-hook (function (lambda nil (setq rmime-mode nil))) nil t))
	  (make-local-variable 'before-revert-hook)
	  (add-hook 'before-revert-hook (function (lambda nil (setq rmime-mode nil)))))

	;; We may call narrow-to-region inside the next if statement.
	(save-restriction

	  ;; Prepare the buffer to hold the formatted data.
	  ;;
	  ;; There are two ways of doing this depending on whether or
	  ;; not the formatted data is to be in the same buffer as the
	  ;; raw data.
	  (if (eq (car mimeinfo) (current-buffer))

	      ;; The formatted message is to be displayed in the same
	      ;; buffer as the raw representation.  Narrow the
	      ;; restriction to exclude any headers.  Everything
	      ;; before point is formatted, everything after is raw.
	      (narrow-to-region (point) (point-max))

	    ;; The formatted message is to be displayed in a new
	    ;; buffer.  The buffer is already selected, but not empty.
	    (erase-buffer))

	  ;; Canonicalize header information.
	  (let ((type                      (rmime-uncomment (nth 1 mimeinfo)))
		(encoding    (downcase (or (rmime-uncomment (nth 2 mimeinfo)) "")))
		(disposition               (rmime-uncomment (nth 3 mimeinfo)))
		(description                                (nth 4 mimeinfo)))

	    ;; Decide how formatting should be done.
	    (let ((handler (rmime-handler type)))

	      ;; Now go do the formatting.
	      (rmime-modify-buffer

		;; We try to take a shortcut if we can.  There are two
		;; shortcuts available to us.  Both require that the
		;; message is plain text and that the data is in the
		;; current buffer.
		;;
		;; The first shortcut requires that the encoding be
		;; transparent.  Nearly all incoming messages qualify
		;; for this shortcut.
		;;
		;; The second shortcut requires the message body to
		;; already be decoded.  This is indicated by the
		;; presence of rmime-magic-string.  This lets us
		;; decode quoted-printable encoding once for the
		;; entire life of the message.  Of course,
		;; rmime-clean-files must be nil for this to work
		;; well.  No, we can't get here if this message has
		;; already been encountered in this session because
		;; get-text-properties would have found a rmime-cache
		;; property.  This shortcut caches quoted-printable
		;; encoding across sessions.
		;;
		;; Before deciding to use the second shortcut, we need
		;; to verify that the formatted text is complete.  We
		;; do this by keeping two copies of the magic string
		;; in the buffer while formatting is in progress.
		(if (and (eq (indirect-function handler) (indirect-function 'rmime-text/plain))
			 (eq (car mimeinfo) (current-buffer))
			 (not (or (assoc "filename" (rmime-parameters disposition))
				  (assoc "name" (rmime-parameters type))))
			 (or (member encoding rmime-transparent-encodings)
			     (and (search-forward rmime-magic-string nil t)
				  (not (looking-at rmime-magic-regexp)))))

		    ;; A shortcut applies.  See wich one.
		    (if (bobp)

			;; The first shortcut applies.
			;;
			;; If the message is the empty message, we
			;; have nowhere to attach our text properties.
			;; Unlike the hard way down below, we can't
			;; just fudge up a character because we are
			;; dealing with the raw message.  In this
			;; case, we just don't set any text
			;; properties.  The next time we are called
			;; upon to format the same message, we will
			;; execute extra lines of code, but still get
			;; the same answer.
			(or (eobp)
			    (add-text-properties (1- (point-max)) (point-max) (list 'rear-nonsticky t 'rmime-cache t)))

		      ;; The second shortcut applies.
		      ;;
		      ;; The raw content is from point to point-max.
		      ;; Moving backward over the magic string takes
		      ;; us to the formatted content.
		      (let ((string (buffer-substring (point) (point-max))))
			(backward-char (length rmime-magic-string))
			(rmime-set-formatting t string nil)))

		  ;; The shortcuts do not apply.

		  ;; Formatting may take a while when a shortcut
		  ;; doesn't apply.  Let the poor user know what is
		  ;; up.
		  (message "Formatting MIME message...")

		  ;; First order of business is cleaning up.
		  ;;
		  ;; If we find any occurances of the magic string, it
		  ;; must be because we are looking at an auto-save
		  ;; file.  Everything up to the magic string is
		  ;; formatted MIME that has lost its text properties.
		  ;; It needs to be deleted before it can be redone.
		  ;;
		  ;; If there is more than one occurance of the magic
		  ;; string, only the last one counts.
		  ;;
		  ;; The search-forward up above may have moved point
		  ;; before looking-at decided that we couldn't take a
		  ;; shortcut.  We don't care about this because point
		  ;; will be right after an occurrence of the magic
		  ;; string.
		  ;;
		  ;; The delete must atomically delete all occurances
		  ;; of the magic string at once or else the message
		  ;; may appear correctly formatted for a brief moment
		  ;; in time.  Look at the shortcut code.
		  (while (search-forward rmime-magic-string nil t))
		  (delete-region (point-min) (point))

		  ;; We use unwind-protect to try to restore the
		  ;; buffer in the event of a formatting error.
		  (unwind-protect

		      ;; We need to extract the raw content.  If there
		      ;; is a rmime-end text property set on the last
		      ;; character of the restriction, this message was
		      ;; copied into this buffer from somewhere else
		      ;; and we need to use this text property to
		      ;; determine the raw content.  Otherwise, the raw
		      ;; content is from point to point-max.
		      (let ((content (or (if (not (eobp))
					     (let ((string (get-text-property (1- (point-max)) 'rmime-end)))
					       (if (identity string)
						   (list (identity string)
							 (identity 1)
							 (1+ (length string))))))
					 (save-excursion (set-buffer (car mimeinfo))
							 (list (buffer-substring (point) (point-max))
							       (identity 1)
							       (- (point-max) (point) -1))))))

			;; We atomically insert two copies of the
			;; magic string before before we try any
			;; formatting.  The last copy separates the
			;; formatted from the raw message.  The second
			;; copy is a sentinel to the second shortcut
			;; that this is an incompletly formatted
			;; buffer.
			;;
			;; This is the first change to the message.
			;; If we crap out anytime after this insertion
			;; and before formatting is complete, we
			;; delete everything up to and including the
			;; last copy of the magic string and get the
			;; original message back.
			;;
			;; BUG: If the message is the empty message,
			;; some of the markers that point to point-min
			;; may be indicating start-of-text and others
			;; may be indicating end-of-message.  There is
			;; no way to distinguish which is which.  We
			;; assume that start-of-text markers are more
			;; common than empty messages and use insert
			;; instead of insert-before-markers.
			(save-excursion (insert rmime-magic-string-times2))

			;; Now go do the formatting.
			(let ((playlist (funcall handler content type encoding disposition description)))

			  ;; We need to have text to have text
			  ;; properties.  If we have a message that
			  ;; formats into nothing (for example, a
			  ;; multipart message with a big prologue and
			  ;; no body parts), we fudge up a newline.
			  (if (bobp)
			      (insert "\n"))

			  ;; Set the various text properties that need
			  ;; to be maintained on formatted MIME and
			  ;; remove the raw text from the buffer.
			  (rmime-set-formatting (copy-marker (1- (point))) (car content) playlist)

			  ;; This exists as an optimization.
			  ;;
			  ;; This prevents unwind-protect handler that
			  ;; immediatly follows from wasting time
			  ;; searching for an occurance of the magic
			  ;; string that doesn't exist.
			  ;;
			  ;; It is important to use point-min instead
			  ;; of some other value so that point winds
			  ;; up being set to the beginning of the
			  ;; buffer.  This is only important when the
			  ;; source buffer is not the current buffer.
			  ;; A save-excursion is in effect for the
			  ;; source buffer.  This is only important
			  ;; when the current buffer isn't selected in
			  ;; some window.  The window's point would
			  ;; override.  Really, this isn't important
			  ;; in any presently known major mode.
			  (narrow-to-region (point-min) (point-min))

			  ;; If rmime-immediate-play is set, append up
			  ;; the playlist.  If the result is not nil,
			  ;; and if no metamail process is running,
			  ;; now would be a good time to start one.
			  (if (identity rmime-immediate-play)
			      (if (setq rmime-playlist (append playlist rmime-playlist))
				  (if (not rmime-process)
				      (rmime-start-play))))))

		    ;; This is in an unwind-protect so it gets
		    ;; executed even if an error occurs.  It is
		    ;; possible for an error to occur after
		    ;; rmime-set-formatting deleted the magic string.
		    ;; In this case, formatting succeeded and the
		    ;; delete-region won't do anything.  If formatting
		    ;; failed, atomically delete everything up to and
		    ;; including the last copy of the magic string.
		    ;; This wipes out the formatted text leaving the
		    ;; raw text.
		    ;;
		    ;; We do not depend on this unwind-protect for
		    ;; data integrity!
		    ;;
		    ;; The delete must atomically delete all
		    ;; occurances of the magic string at once or else
		    ;; the message may appear correctly formatted for
		    ;; a brief moment in time.  Look at the shortcut
		    ;; code.
		    (goto-char (point-min))
		    (while (search-forward rmime-magic-string nil t))
		    (delete-region (point-min) (point)))

		  ;; We have finished formatting the message without
		  ;; utilizing a shortcut.
		  (message "Formatting MIME message... done")))

	      ;; Now that MIME formatting has succeeded, it is safe
	      ;; to activate the minor mode indicator.  We need to do
	      ;; this while we are still in the save-excursion or
	      ;; else we may flag the source buffer instead of the
	      ;; destination buffer.
	      (setq rmime-mode t))))))))

;; This function must be atomic.  If we do not succeed, we must fail
;; in a way that permits full recovery of raw MIME.
;;
;; Our atomic operation is delete-region.  If this delete happens, it
;; atomically wipes any copies of the magic string, wipes the raw MIME
;; that comes afterward, and leaves text properties on the last
;; character of the restriction.  If the delete fails, there is at
;; least one copy of the magic string hanging around, the raw MIME
;; comes after the last occurrence of the magic string, and there are
;; no text properties set on the last character of the restriction.
;;
;; The delete-region will not partially happen.
;;
;; The region being deleted can't be empty because it contains at
;; least one copy of the magic string.
;;
;; All of rmime-annotate, rmime-format, and rmime-cancel must be able
;; to back out of any situation that could arise before the delete and
;; accept any situation that could arise after the delete.
;;
;; It is tempting to set the read-only property, but this breaks
;; rmail-expunge.
(defun rmime-set-formatting (cache string playlist)
  (put-text-property (point-min) (point) 'rmime-start string)
  (add-text-properties (1- (point)) (point) (list 'rear-nonsticky t
						  'rmime-playlist playlist
						  'rmime-end      string
						  'rmime-cache    cache))
  (delete-region (point) (point-max)))

;; Cancel formatting on the current message.
(defun rmime-cancel ()

  ;; rmime-mimeinfo-method may move the point or set the buffer
  (save-excursion

    ;; Go to the message we are to unformat.
    (funcall rmime-mimeinfo-method)

    ;; If we are at the end of the buffer, there is no formatting to
    ;; cancel and we are done.  Remember, formatting never results in
    ;; the empty message.  We have to check for the empty message or
    ;; else we may get an args-out-of-range error when we call
    ;; get-text-property.
    (if (not (eobp))

	;; Get the raw MIME which was saved as a text property.  (This
	;; property will be nil if formatting is not yet complete.)
	(let ((string (get-text-property (1- (point-max)) 'rmime-end)))

	  ;; Prepare to modify the buffer.
	  (rmime-modify-buffer

	    ;; See if formatting was complete.
	    (if (not string)

		;; Formatting is incomplete.  We cancel the incomplete
		;; formatting by atomically deleting everything up to
		;; and including the last occurrence of the magic
		;; string.  If we don't delete every occurance at
		;; once, the message may appear correctly formatted
		;; for a brief moment in time.  Look at the shortcut
		;; code.
		(save-restriction
		  (narrow-to-region (point) (point-max))
		  (while (search-forward rmime-magic-string nil t))
		  (delete-region (point-min) (point)))

	      ;; Formatting has completed.  We have to undo formatting
	      ;; in a way that preserves data integrity even if an
	      ;; error occurs.  First, we drop the rmime-cache
	      ;; property so that rmime-format will do the right thing
	      ;; even if cancelling doesn't make it.  Then we insert
	      ;; the raw MIME.  If we encounter an error after the
	      ;; insert and before the delete, we will insert another
	      ;; copy of the raw MIME the next time we are called.  We
	      ;; don't care.  The delete-region atomically deletes any
	      ;; extraneous copies of the raw MIME and the formatted
	      ;; MIME at once.
	      (remove-text-properties (1- (point-max)) (point-max) '(rmime-cache))
	      (insert string)
	      (delete-region (point) (point-max))))))

    ;; We change the appearance of the mode line no matter what we had
    ;; to do to the buffer.
    (setq rmime-mode nil)))

;; A function for write-region-annotate-functions.
;;
;; This function is called for any buffer write, including auto-save
;; files.  It is the only way we can get raw MIME in an auto-save
;; file.
;;
;; There are two ways we may proceed.  If rmime-clean-files is set, we
;; copy the buffer and delete formatted MIME before inserting raw
;; MIME.  If rmime-clean-files is not set, we quickly arrange for raw
;; MIME to be inserted during the write along with copies of the magic
;; string to identify raw MIME as such.
(defun rmime-annotate (start end)
  (if (integer-or-marker-p start)	; in case of (write-region "string" ...)
       (if (identity rmime-clean-files)
	   (rmime-annotate-cleanly start end)
	 (rmime-annotate-quickly start end))))

;; This function cleans formatted MIME and inserts raw MIME during a
;; buffer save.
;;
;; Annotation is done for every buffer save, so we optimize the case
;; where no formatted MIME exists.
;;
;; We are concerned about not being able to save at all if we don't
;; have enough memory available to copy the buffer.  We fall back on
;; rmime-annotate-quickly if we encounter any error while marking up
;; the buffer.
(defun rmime-annotate-cleanly (start end)

  ;; See if any work needs done in this buffer.
  (if (text-property-not-all start end 'rmime-end nil)

      ;; Remember what buffer we are working on.
      (let ((buffer (current-buffer)))

	;; Set up to catch any error and fall back.
	(condition-case nil
	    (progn

	      ;; Access the tmp buffer.  Mark it as no longer caching
	      ;; any raw MIME.  We make a copy of the region we are
	      ;; saving.
	      (set-buffer (get-buffer-create rmime-tmp-buffer-name))
	      (kill-all-local-variables) ; In case of strange default-major-mode mode.
	      (erase-buffer)
	      (insert-buffer-substring buffer start end)

	      ;; Apply any pending annotations from other functions
	      ;; that may exist in write-region-annotate-functions.
	      (let ((fuzz (- start (point-min))))
		(mapcar (function lambda (x) (goto-char (- (car x) fuzz)) (insert (cdr x)))
			(reverse write-region-annotations-so-far)))

	      ;; Starting at the beginning, search for formatted MIME.
	      (goto-char (point-min))
	      (let (point)
		(while (setq point (text-property-not-all (point) (point-max) 'rmime-end nil))
		  (let ((string (get-text-property point 'rmime-end))
			(end    (1+ point)))
		    (goto-char end)
		    (insert string)
		    (delete-region (previous-single-property-change end 'rmime-start nil (point-min)) end)
		    (narrow-to-region (point) (point-max)))))

	      ;; We may have narrowed our restriction inside the loop.
	      ;;
	      ;; Widen returns nil, so we do to.  This is important.
	      (widen))

	  ;; In the event of an error, try to free up memory by
	  ;; killing our temporary buffer.  We might not succeed
	  ;; because we might not have been able to create the
	  ;; temporary buffer.  We reselect the source buffer and try
	  ;; a annotator that is more likely to succeed.
	  (error (condition-case nil (kill-buffer rmime-tmp-buffer-name) (error))
		 (set-buffer buffer)
		 (rmime-annotate-quickly start end))))))

;; This annotator inserts raw MIME without trying to delete formatted
;; MIME.  This is quick and easy because we don't have to make a copy
;; of the buffer.
;;
;; We build the annotation list from back to front so that it comes
;; out sorted.  This is important.
;;
;; We insert a copy of the magic string before raw MIME so that raw
;; MIME can be identified.
;;
;; Each occurrence of the rmime-end property represents one message.
;; If two characters next to each other have identical rmime-end
;; properties, this means that there are two messages (formatted to
;; one character each) next to each other in the buffer.
(defun rmime-annotate-quickly (start end)
  (let (result)
    (while (< start end)
      (let ((string (get-text-property (1- end) 'rmime-end)))
	(if (identity string)
	    (progn (setq result (cons (cons end rmime-magic-string) (cons (cons end string) result)))
		   (setq end (1- end)))
	  (setq end (previous-single-property-change end 'rmime-end nil start)))))
    (identity result)))

 
;;; Text type ---


;; Careful to insert contents before setting buffer-file-name to avoid
;; unnecessary file locking.
(defun rmime-text/plain (content type encoding disposition description)
  (let* ((parameters (rmime-parameters type))
	 (mode (intern (concat (or (cdr (assoc "x-emacs-mode" parameters)) "") "-mode")))
	 (file (cdr (or (assoc "filename" (rmime-parameters disposition))
			(assoc "name"     (rmime-parameters type))))))
    (if (or (fboundp mode)
	    (identity file))
	(let ((name (generate-new-buffer-name (concat (or file (buffer-name)) "<mime>"))))
	  (unwind-protect
	      (let ((buffer (get-buffer-create name)))
		(save-excursion
		  (set-buffer buffer)
		  (rmime-insert-content nil encoding content)
		  (setq buffer-file-name file)
		  (if (fboundp mode)
		      (funcall mode)
		    (set-auto-mode)))
		(insert-buffer-substring buffer)))
	    (condition-case nil
		(save-excursion
		  (set-buffer name)
		  (set-buffer-modified-p nil)
		  (kill-buffer nil))
	      (error)))
      (rmime-insert-content nil encoding content))
    (or (bolp)
	(insert "\n"))
    (identity nil)))

(defun rmime-text/enriched (content type encoding disposition description)
  (let ((point (point)))
    (rmime-insert-content nil encoding content)
    (enriched-decode point (point))))

;; We need to truly undefine text/enriched if we don't have it.  Just
;; making text/enriched call text/plain won't prevent
;; multipart/alternative from selecting the wrong part.
(or (fboundp 'enriched-decode)
    (fmakunbound 'rmime-text/enriched))

(defalias 'rmime-text/* 'rmime-text/plain)
(put 'rmime-text/* 'rmime-unwise t)	; So multipart/alternative avoids this choice

 
;;; Multipart type ---


;; multipart/mixed is formatted one body part at a time.  Each body
;; part may be independently played.
(defun rmime-multipart/mixed (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-multipart-alist (rmime-multipart-split content type)))
    (apply 'append (mapcar 'rmime-multipart-dispatch rmime-multipart-alist))))

;; multipart/parallel is formatted one body part at a time just like
;; multipart/mixed.  The message is only played as a whole.  Metamail
;; provides for the parallel display of the body parts.
(defun rmime-multipart/parallel (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-multipart-alist (rmime-multipart-split content type)))
    (mapcar 'rmime-multipart-dispatch rmime-multipart-alist))
  (cons (list nil content type encoding) nil))

(defun rmime-multipart/alternative (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-multipart-alist (rmime-multipart-split content type)))

    ;; Three possible ways of formatting the multipart/alternative
    ;; message:
    ;;
    ;; 1) Search the list for the last "good" body part and display
    ;;    it.  A good body part is one that doesn't have the
    ;;    rmime-unwise property set on its handler.
    ;;
    ;; 2) Give up if there are no bodyparts whatsoever.
    ;;
    ;; 3) Display the first body part best we can, whether it is good
    ;;    or not.
    (or (rmime-alternative (cdr rmime-multipart-alist))
	(not rmime-multipart-alist)
	(rmime-multipart-dispatch (car rmime-multipart-alist))))

  ;; Provide the whole message for playing.  When the message is
  ;; displayed, metamail may pick a different body part.
  (cons (list nil content type encoding) nil))

(defun rmime-alternative (bodies)
  (if (identity bodies)
      (or (rmime-alternative (cdr bodies))
	  (if (not (get (rmime-handler (rmime-uncomment (nth 2 (car bodies)))) 'rmime-unwise))
	      (let ((backtrack (point)))
		(condition-case nil
		    (progn (rmime-multipart-dispatch (car bodies))
			   (identity t))
		  (rmime-unknown-conventions (delete-region backtrack (point)))))))))

;; Permit me the indulgence of a featureful and fast digest type.  I
;; use this daily when I read news.  Check out my newsdigest program.
;; Be sure to check out at least version 2.8 --- earlier versions were
;; not MIME aware.
;;
;; The idea here is to generate a table of contents.  Pressing C-c C-c
;; on a hot area in the table of contents takes us directly to the
;; message.  rmime-digest-toc generates the table of contents by
;; calling rmime-digest-toc-1 on each body part and appending the
;; results.  rmime-digest-toc-1 usually inserts one line into the
;; buffer and returns a list of length one.  If rmime-digest-toc-1
;; encounters a digest within the digest, it calls rmime-digest-toc
;; and uses mutual recursion.  When this happens, rmime-digest-toc-1
;; can insert any number of lines and return a list of any length.
;; This approach flattens the digest.  We intentionally do not try to
;; flatten multipart messages that are not digests.
;;
;; The car of every element in the list returned by rmime-digest-toc
;; is a marker that needs to be set when we finally discover where the
;; message body will be formatted.  The cdr of every element is a
;; funcall call to format the message body.
(defun rmime-multipart/digest (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((alist (rmime-multipart-split content type)))
    (insert rmime-digest-toc-intro)
    (let ((contents (let ((case-fold-search t) ; In case of (rmime-fetch-header "Subject").
			  (rmime-default-type 'rmime-message/rfc822))
		      (rmime-digest-toc alist))))
      (apply 'append (mapcar 'rmime-digest-dispatch contents)))))

(defun rmime-digest-toc (alist)
  (apply 'append (mapcar 'rmime-digest-toc-1 alist)))

(defun rmime-digest-toc-1 (body)
  (let ((here	(point))
	(marker (make-marker))		; Marker set in rmime-digest-dispatch
	(keymap (copy-keymap (or (current-local-map) '(keymap))))
	(content				    (nth 1 body))
	(type			   (rmime-uncomment (nth 2 body)))
	(encoding    (downcase (or (rmime-uncomment (nth 3 body)) "")))
	(disposition		   (rmime-uncomment (nth 4 body)))
	(description				    (nth 5 body)))

    (let ((handler (rmime-handler type)))

      ;; Insert one line into the table of contents.  Use the
      ;; Content-Description header field if one exists.  If we don't
      ;; have a Content-Description header field, but the body is a
      ;; rfc822 message, use the subject line.  case-fold-search has
      ;; already been set (in rmime-multipart/digest) for
      ;; (rmime-fetch-header "subject").
      (insert (identity rmime-digest-indentation)
	      (or (identity description)
		  (if (eq handler 'rmime-message/rfc822)
		      (rmime-content-excursion (content)
			(goto-char (point-min))
			(if (re-search-forward "^[ \t]*\n" nil t)
			    (narrow-to-region (point-min) (match-beginning 0)))
			(rmime-fetch-header "Subject")))
		  (identity "No description provided"))
	      (identity "\n"))

      ;; See if there is a digest within the digest.
      (if (eq handler 'rmime-multipart/digest)

	  ;; If a digest is encountered within a digest, use mutual
	  ;; recursion to generate one master table of contents for
	  ;; the entire message.
	  (let ((rmime-digest-indentation (concat rmime-digest-indentation rmime-digest-recursive-indentation)))
	    (rmime-digest-toc (rmime-multipart-split content type)))

	;; Make the line in the table of contents hot and return a
	;; list suitable to be applied to rmime-digest-dispatch.
	;;
	;; Notice that "handler" was computed while the default
	;; handler was message/rfc822 and not text/plain.  It is
	;; important that the dispatch function use the provided value
	;; and not recompute its own.
	(rmime-interact keymap 'rmime-digest-item-action marker)
	(add-text-properties here (point) (list 'local-map keymap 'face 'rmime-markup-face))
	(cons (list marker handler content type encoding disposition description) nil)))))

;; This function formats an individual body part from the return value
;; generated while formatting the table of contents entry.  It is
;; tempting to use a generic dispatch subroutine for this purpose, but
;; this is wrong.  First, it would be waste since the generic
;; subroutine would have to canonicalize the mimeinfo --- an expensive
;; operation that was already done once when the table of contents
;; entry was generated.  Second, rmime-handler might return a
;; different value now than it did when rmime-default-type was set to
;; message/rfc822.
(defun rmime-digest-dispatch (body)
  (insert rmime-digest-item-intro)
  (set-marker (car body) (point))
  (apply 'funcall (cdr body)))

(defun rmime-digest-item-action (marker)
  (or (and (eq (marker-buffer   marker) (current-buffer))
	   (>= (marker-position marker) (point-min))
	   (<= (marker-position marker) (point-max)))
      (error "Message not accessible in this buffer"))
  (push-mark)
  (goto-char marker)
  (or (pos-visible-in-window-p)
      (recenter 2)))

(defun rmime-multipart-dispatch (body)
  (let ((content                                    (nth 1 body))
        (type                      (rmime-uncomment (nth 2 body)))
        (encoding    (downcase (or (rmime-uncomment (nth 3 body)) "")))
        (disposition               (rmime-uncomment (nth 4 body)))
        (description                                (nth 5 body)))
    (funcall (rmime-handler type) content type encoding disposition description)))

;; This function splits a multipart MIME message.  A list of the
;; subparts is returned.  The car of each element is the Content-Id.
;; A body part that needs to know about other body parts can use
;; assoc.
;;
;; It is tempting to make this a recursive function, but large
;; messages can cause a recursive implementation to exceed either
;; max-lisp-eval-depth or max-specpdl-size.
(defun rmime-multipart-split (content type)
  (let ((boundary (cdr (assoc "boundary" (rmime-parameters type)))))
    (if (or (not (stringp boundary)) (string= boundary ""))
	(error "A MIME multipart message has no boundary string"))
    (let ((separator (concat "^--" (regexp-quote boundary)))
	  (case-fold-search)
	  (result))
      (rmime-content-excursion (content)
	(goto-char (point-min))
	(re-search-forward separator nil 'move)
	(while (and (not (looking-at "--")) (zerop (forward-line)))
	  (let ((start (point)))
	    (re-search-forward separator nil 'move)
	    (setq result (cons (save-excursion
				 (save-restriction
				   (end-of-line 0)
				   (narrow-to-region start (point))
				   (goto-char (point-min))
				   (re-search-forward "^[ \t]*\n" nil 'move)
				   (let ((case-fold-search t)
					 (content (list rmime-tmp-string (point) (point-max))))
				     (narrow-to-region (point-min) (point))
				     (cons (rmime-fetch-header "Content-Id")
					   (cons content (rmime-fetch-mime-headers))))))
			       (identity result)))))
	(nreverse result)))))

(defalias 'rmime-multipart/* 'rmime-multipart/mixed)
(put 'rmime-multipart/* 'rmime-unwise t) ; So multipart/alternative avoids this choice

 
;;; Message/RFC822 type ---


(defun rmime-message/rfc822 (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((case-fold-search    t)		; For highlighed & ignored headers, below.
	(here                (point))
	(buffer              (current-buffer)))

    (let (headers leader body)

      (rmime-content-excursion (content)
	(setq headers (rmime-standard-headers "^[ \t]*\n"))
	(setq body    (list rmime-tmp-string (point) (point-max)))

	;; Narrow the temporary buffer to just the message headers.
	;;
	;; Shortly, we will use (insert-buffer-substring) to get at
	;; the headers.
	(if (re-search-backward "^[ \t]*\n" nil t)
	    (narrow-to-region (point-min) (point))))

      ;; This save-restriction is not inside a save-excursion.
      ;; We intend to move the point.
      (save-restriction

	;; Narrow to just the headers (which aren't inserted yet).
	(narrow-to-region (point) (point))

	;; Get the headers out of the appropriately narrowed temporary
	;; buffer.
	(insert-buffer-substring rmime-tmp-buffer-name)

	;; Apply either rmail-displayed-headers or rmail-ignored-headers.
	(goto-char (point-min))
	(if (and (boundp 'rmail-displayed-headers) (identity rmail-displayed-headers))

	    ;; Apply rmail-displayed-headers.
	    (let ((regexp (concat "\\(" rmail-displayed-headers "\\)\\(\\(\n[ \t]\\|.\\)*\n\\)"))
		  (start  (point)))
	      (while (re-search-forward regexp nil t)
		(delete-region start (match-beginning 0))
		(setq start (point)))
	      (delete-region start (point-max)))

	  ;; Apply rmail-ignored-headers.
	  (let ((regexp (concat "\\(" rmail-ignored-headers "\\)\\(\\(\n[ \t]\\)\\|.\\)*\n")))
	    (while (re-search-forward regexp nil t)
	      (delete-region (match-beginning 0) (match-end 0)))))

	;; Apply rmail-highlighted-headers.
	(goto-char (point-min))
	(while (re-search-forward rmail-highlighted-headers nil t)
	  (if (looking-at "[ \t]*\\([^ \t\n]\\(\\(\n?[ \t]+\\)?[^ \t\n]\\)*\\)")
	      (put-text-property (match-beginning 1) (match-end 1) 'face rmime-highlight-face)))

	;; If there are any headers, insert a newline to separate them
	;; from the message body.
	(goto-char (point-max))
	(or (bobp)
	    (insert "\n"))

	;; Now insert the message body at the end of the headers.
	(rmime-multipart-dispatch (cons nil (cons body headers)))))))

 
;;; Message/Partial type ---


(defun rmime-message/partial (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((parameters (rmime-parameters type)))
    (let ((number (cdr (assoc "number" parameters)))
	  (total  (cdr (assoc "total"  parameters)))
	  (id     (cdr (assoc "id"     parameters)))
	  (here   (point))
	  (keymap (copy-keymap (or (current-local-map) '(keymap)))))
      (if (not id)
	  (error "A MIME partial message has no id"))
      (if (not number)
	  (error "A MIME partial message has no part number"))
      (save-excursion
	(set-buffer (get-buffer-create (concat "*rmime-" id "*")))
	(setq buffer-read-only nil)
	(setq rmime-partial-partlist (rmime-partial-1 (string-to-int number) rmime-partial-partlist content))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(if (identity total)
	    (setq rmime-partial-parts (string-to-int total)))
	(goto-char (point-min))
	(rmime-interact keymap 'rmime-partial-action (current-buffer)))
      (cond (description (insert rmime-leader description "\n"))
	    (total (insert rmime-leader (format "Part %s of message %s (%s total parts)\n" number id total)))
	    ((insert rmime-leader (format "Part %s of message %s\n" number id))))
      (add-text-properties here (point) (list 'local-map keymap 'face 'rmime-markup-face))
      (identity nil))))

;; This function inserts one part of a partial message into the current buffer.
;;
;; number is the part number.
;; alist is an ordered association list of (part number . last-char-marker)
;; content is a data structure suitable to be given to rmime-insert-unencoded-content.
;;
;; We return a new association list.
(defun rmime-partial-1 (number alist content)
  (cond ((null alist)
	 (goto-char (point-min))
	 (rmime-insert-unencoded-content content)
	 (cons (cons number (point-marker)) nil))
	((< (car (car alist)) number)
	 (goto-char (cdr (car alist)))
	 (rmime-insert-unencoded-content content)
	 (cons (cons number (point-marker)) alist))
	((> (car (car alist)) number)
	 (cons (car alist) (rmime-partial-1 number (cdr alist) content)))
	(alist)))

(defun rmime-partial-action (buffer)
  (let ((variables (buffer-local-variables buffer)))
    (let ((total (cdr (assoc 'rmime-partial-parts    variables)))
	  (list  (cdr (assoc 'rmime-partial-partlist variables))))
      (if (not (eq total (length list)))
	  (if (identity total)
	      (message "You have seen only %d of %d parts" (length list) total)
	    (message "You have seen only %d parts" (length list)))
	(pop-to-buffer buffer)
	(let ((rmime-immediate-play t))
	  (rmime-format))))))

 
;;; Message/External-Body type ---


;; We need an error symbol to signal if we don't recognize the
;; access-type.  This error symbol contains
;; rmime-unknown-conventions in its `error-conditions' property.
;; rmime-alternative catches errors with the rmime-unknown-conventions
;; property.
(put 'rmime-unknown-accesstype 'error-conditions '(rmime-unknown-accesstype rmime-unknown-conventions error))
(put 'rmime-unknown-accesstype 'error-message "An External-body MIME message has an unknown access type")

(defun rmime-message/external-body (content type encoding description disposition)
  (rmime-barf-if-opaque encoding)
  (let* ((parameters (rmime-parameters type))
	 (access     (cdr (assoc "access-type" parameters)))
	 (case-fold-search t))		; Propagated to rmime-fetch-header
    (let (mimeinfo body id)
      (rmime-content-excursion (content)
	(setq mimeinfo (rmime-standard-headers "^[ \t]*\n"))
	(setq body     (list rmime-tmp-string (point) (point-max)))
	(setq id       (progn (narrow-to-region (point) (point-min)) (rmime-fetch-header "Content-Id")))
	(if (null description) (setq description (nth 3 mimeinfo))))
      (if (or (null id) (string= id ""))
	  (error "A MIME external-body message has no Content-Id"))
      (cond ((equal access "anon-ftp")
	     (rmime-external content type description id mimeinfo 'rmime-external-data 'rmime-ftp parameters t))
	    ((equal access "ftp")
	     (rmime-external content type description id mimeinfo 'rmime-external-data 'rmime-ftp parameters nil))
	    ((equal access "mail-server")
	     (rmime-external-batch content type description id mimeinfo 'rmime-mailserver parameters body))
	    ((and (member access '("local-file" "afs")))
	     (let ((filename (cdr (assoc "name" parameters))))
	       (if (identity filename)
		   (rmime-external-interactive id mimeinfo 'rmime-external-data 'insert-file-contents filename)
		 (error "a MIME local-file message has no name"))))
	    ((signal 'rmime-unknown-accesstype (list access)))))))

;; Some access-types are always fetched immediatly (local-file).
;;	We call rmime-external-interactive to handle these.
;; Some access-types are never fetched immediatly (mail-server).
;;	We call rmime-external-batch to handle these.
;; The rest are fetched immediatly if and only if rmime-immediate-play is set.
;;	We call rmime-external to handle these.

(defun rmime-external (content type description &rest arguments)
  (if (identity rmime-immediate-play)
      (apply 'rmime-external-interactive arguments)
    (apply 'rmime-external-batch content type description arguments)))

(defun rmime-external-interactive (id mimeinfo command &rest arguments)
  (let ((content (save-excursion (apply command id mimeinfo arguments)
				 (or (if (not (eq (point-min) (point-max)))
					 (let ((string (get-text-property (1- (point-max)) 'rmime-end)))
					   (if (identity string)
					       (list (identity string)
						     (identity 1)
						     (1+ (length string))))))
				     (list (buffer-substring (point-min) (point-max))
					   (identity 1)
					   (- (point-max) (point-min) -1))))))
    (rmime-multipart-dispatch (cons nil (cons content mimeinfo)))))

(defun rmime-external-batch (content type description id mimeinfo command &rest arguments)
  (prog1
      (cons (list (point-marker) content type nil) nil)
    (let ((here       (point))
	  (keymap     (copy-keymap (or (current-local-map) '(keymap)))))
      (insert rmime-leader (or description "A reference to data stored in an external location") "\n")
      (rmime-interact keymap 'rmime-external-action id mimeinfo command arguments)
      (add-text-properties here (point) (list 'local-map keymap 'face 'rmime-markup-face)))))

(defun rmime-external-action (id mimeinfo command arguments)
  (if (apply command id mimeinfo arguments)
      (progn (switch-to-buffer (current-buffer))
	     (rmime-format))))

;; rmime-external-data fetches (and caches) the data without
;; formatting it.  Never returns nil --- rmime-external-action depends
;; on this to know if it can format the data.

(defun rmime-external-data (id mimeinfo command &rest arguments)
  (let* ((name (concat "*rmime-" id "*"))
	 (buffer (get-buffer name)))
    (if (identity buffer)
	(set-buffer buffer)
      (set-buffer (get-buffer-create rmime-tmp-buffer-name))
      (kill-all-local-variables)	; In case of strange default-major-mode.
      (erase-buffer)
      (save-excursion (apply command arguments))
      (make-local-variable 'rmime-mimeinfo-method)
      (setq rmime-mimeinfo-method (list 'lambda 'nil '(goto-char (point-min)) (list 'quote (cons (current-buffer) mimeinfo))))
      (rename-buffer name))))

(defun rmime-ftp (parameters anon)
  (let ((name      (cdr (assoc "name"      parameters)))
	(site      (cdr (assoc "site"      parameters)))
	(directory (cdr (assoc "directory" parameters)))
	(mode      (cdr (assoc "mode"      parameters)))
	(user      (if (identity anon) "anonymous"))
	(pass      (if (identity anon) (concat (user-login-name) "@" (system-name))))
	(modes     (default-file-modes))
	(control   (make-temp-name "/tmp/ftp"))
	(data      (make-temp-name "/tmp/mime")))

    (if (or (null name) (string= name ""))
	(error "A MIME anon-ftp message has no name parameter"))
    (if (or (null site) (string= site ""))
	(setq site (read-from-minibuffer "Site for FTP access: ")))
    (if (or (null user) (string= user ""))
	(setq user (read-from-minibuffer (concat "Username on " site ": "))))
    (if (or (null pass) (string= pass ""))
	(setq pass (read-from-minibuffer (concat "Password for " user "@" site ": "))))

    (unwind-protect
	(let ((windows (current-window-configuration)))
	  (save-excursion
	    (pop-to-buffer (get-buffer-create "*ftp*"))
	    (if (not (bobp))
		(progn (goto-char (point-max))
		       (insert "\f\n")))
	    (set-window-start nil (point))
	    (sit-for 0)
	    (set-default-file-modes 448) ; -rwx------
	    (write-region (concat "open " site                            "\n"
				  "user " user " " pass                   "\n"
				  (if mode      (concat "type " mode      "\n"))
				  (if directory (concat "cd "   directory "\n"))
				  "get "  name " " data                   "\n"
				  "quit"                                  "\n")
			  nil control nil 'quiet)
	    (let ((here (point)))
	      (call-process rmime-ftp-program control t t "-nv")
	      (if (not (save-excursion (re-search-backward "^226 " here t)))
		  (error "FTP file transfer failed")))
	    (bury-buffer))		; Too bad pop-to-buffer doesn't have a NORECORD option.
	  (insert-file-contents data)
	  (set-window-configuration windows))
      (condition-case nil (delete-file data)    (file-error))
      (condition-case nil (delete-file control) (file-error))
      (set-default-file-modes modes))))

;; Format up a message to send to a mailserver.  Always return nil.

(defun rmime-mailserver (id mimeinfo parameters body)
  (let ((server  (cdr (assoc "server"  parameters)))
	(subject (cdr (assoc "subject" parameters))))
    (if (or (null server) (string= server ""))
	(error "A MIME mail-server message has no address"))
    (if (mail nil server subject nil nil nil nil)
	(save-excursion
	  (goto-char (point-max))
	  (rmime-insert-unencoded-content body)))))

 
;;; Application/Octet-Stream type ---


(defun rmime-application/octet-stream (content type encoding disposition description)
  (let ((here   (point))
	(keymap (copy-keymap (or (current-local-map) '(keymap))))
	(name   (cdr (or (assoc "filename" (rmime-parameters disposition))
			 (assoc "name"     (rmime-parameters type))))))
    (insert rmime-leader (or description "Press C-c C-c here to receive file transmission") "\n")
    (rmime-interact keymap 'rmime-octet-action name content encoding)
    (add-text-properties here (point) (list 'local-map keymap 'face 'rmime-markup-face))
    (identity nil)))

(defun rmime-octet-action (name content encoding)
  (let ((filename (read-file-name "Save transmitted file as: " nil nil nil name)))
    (pop-to-buffer (or (get-file-buffer filename) (create-file-buffer filename)))
    (erase-buffer)
    (rmime-insert-content t encoding content)
    (goto-char (point-min))
    (set-visited-file-name filename)
    (normal-mode)
    (save-buffer)))

 
;;; Default type ---


(defun rmime-*/* (content type encoding disposition description)
  (let ((here   (point))
	(keymap (copy-keymap (or (current-local-map) '(keymap)))))
    (prog1
	(cons (list (point-marker) content type encoding) nil)
      (insert rmime-leader (or description (format "Press C-c C-c here for \"%s\" data" type)) "\n")
      (rmime-interact keymap 'rmime-action content type encoding)
      (add-text-properties here (point) (list 'local-map keymap 'face 'rmime-markup-face)))))
(put 'rmime-*/* 'rmime-unwise t)	; So multipart/alternative avoids this choice

(defun rmime-action (content type encoding)
  (let ((filename (make-temp-name "/tmp/metamail")))
    (rmime-write-content filename content)
    (call-process rmime-metamail-program nil 0 nil "-b" "-d" "-x" "-z" "-m" "emacs" "-c" type "-E" encoding filename)))

 
;;; Play command ---


(defun rmime-play (arg)
  "Sequentially play nontextual MIME message bodies.

With positive argument, immediatly start the playback of the current
message.  With a negative argument, cancel any running playback.  With
no argument (or a zero argument), queue the nontextual parts of the
message for playback after any presently active playback completes.

The overlay arrow is used to align the nontextual message bodies with
the textual ones which will appear in the Emacs buffer."
  (interactive "P")
  (save-excursion
    (funcall rmime-mimeinfo-method)	; Moves point and sets buffer

    ;; If arg is provided and less than zero, stop playing.  If arg is
    ;; not provided or equal to zero, append onto any already existing
    ;; playlist.  Otherwise, since arg is provided and positive,
    ;; replace the existing playlist.
    (if (and (integerp arg) (< arg 0))
	(setq rmime-playlist nil)
      (let ((playlist (and (not (eq (point-min) (point-max))) (get-text-property (1- (point-max)) 'rmime-playlist))))
	(if (memq arg '(0 nil))
	    (setq rmime-playlist (append rmime-playlist playlist))
	  (setq rmime-playlist playlist))))

    ;; If a process is already busy playing...
    ;;
    ;;		If arg is provided and not zero, kick the process.  If
    ;;		there is anything to play, a new process will be
    ;;		started by the process sentinel.
    ;;
    ;; If a process is not already busy playing...
    ;;
    ;;		If we need a process, start one.
    ;;
    (if (identity rmime-process)
	(or (memq arg '(0 nil))
	    (interrupt-process rmime-process t))
      (if (identity rmime-playlist)
	  (rmime-start-play)))))
(defalias 'play 'rmime-play)

(defun rmime-start-play ()
  (make-local-variable 'overlay-arrow-string)
  (setq overlay-arrow-string rmime-arrow)
  (rmime-start-metamail))

(defun rmime-start-metamail ()
  (let ((filename (make-temp-name "/tmp/metamail"))
	(body     (car rmime-playlist))
	(process-connection-type))
    (rmime-write-content filename (nth 1 body))
    (setq rmime-process (start-process "metamail" (current-buffer) rmime-metamail-program "-b" "-d" "-x" "-z" "-c" (nth 2 body) "-E" (or (nth 3 body) "binary") "-m" "emacs" filename))
    (setq overlay-arrow-position (car body)))
  (set-process-filter rmime-process t)	; So no random output gets inserted into the buffer
  (set-process-sentinel rmime-process 'rmime-sentinel)
  (setq rmime-playlist (cdr rmime-playlist))) ; So we get the right error if canceled after finished.

(defun rmime-sentinel (process change)
  (if (buffer-name (process-buffer process)) ; Killed buffer?
      (save-excursion
	(set-buffer (process-buffer process))
	(if (identity rmime-playlist)	; See if there is a next part to play
	    (rmime-start-metamail)	; If so, start metamail to play it
	  (setq overlay-arrow-position nil)
	  (setq rmime-process nil)))))

 
;;; Useful subroutines ---


(defun rmime-barf-if-opaque (encoding)
  (or (member encoding rmime-transparent-encodings)
      (error "A MIME bodypart has an explicitly prohibited Content-Transfer-Encoding")))

;; We need an error symbol to signal if we don't recognize the
;; transfer encoding.  This error symbol contains
;; rmime-unknown-conventions in its `error-conditions' property.
;; rmime-alternative catches errors with the rmime-unknown-conventions
;; property.
(put 'rmime-unknown-encoding 'error-conditions '(rmime-unknown-encoding rmime-unknown-conventions error))
(put 'rmime-unknown-encoding 'error-message "A MIME message has an unknown Content-Transfer-Encoding")

(defun rmime-insert-content (binaryp encoding content)
  (cond ((member encoding rmime-transparent-encodings)
	 (rmime-insert-unencoded-content                                                 content))

	((and rmime-mimencode-program (string= encoding "base64"))
	 (rmime-decode-content rmime-mimencode-program (if binaryp '("-u") '("-u" "-p")) content))

	((and rmime-mimencode-program (string= encoding "quoted-printable"))
	 (rmime-decode-content rmime-mimencode-program '("-u" "-q")                      content))

	((and rmime-uufilter-program  (string= encoding "x-uue"))
	 (rmime-decode-content rmime-uufilter-program  '("-s")                           content))

	((signal 'rmime-unknown-encoding (list encoding)))))

(defun rmime-insert-unencoded-content (content)
  (insert-buffer-substring (save-excursion (apply 'rmime-tmp-buffer content))))

(defun rmime-decode-content (program args content)
  (let ((filename (make-temp-name "/tmp/mime")))
    (unwind-protect
	(progn (rmime-write-content filename content)
	       (or (zerop (apply 'call-process program filename t nil args))
		   (error "couldn't decode contents")))
      (condition-case nil (delete-file filename) (file-error)))))

(defun rmime-write-content (filename content)
  (let ((modes (default-file-modes)))
    (unwind-protect
	(rmime-content-excursion (content)
	  (set-default-file-modes 448)	; -rwx------
	  (write-region (point-min) (point-max) filename nil 'quiet))
      (set-default-file-modes modes))))

(defun rmime-tmp-buffer (string min max)
  (prog1
      (set-buffer (get-buffer-create rmime-tmp-buffer-name))
    (if (not (eq string rmime-tmp-string))
	(progn (setq rmime-tmp-string nil)
	       (erase-buffer)
	       (insert string)
	       (setq rmime-tmp-string string)))
    (widen)
    (narrow-to-region min max)))

(defun rmime-interact (keymap function &rest arguments)
  (let ((action (make-symbol "rmime-interaction")))
    (fset action (list 'lambda nil '(interactive) (list 'apply (list 'quote function) (list 'quote arguments))))
    (define-key keymap "\C-c\C-c" action)))

 
;;; Routines for collecting information about a MIME entity ---


;; This function returns the function to call to display this
;; mime-type.
;;
;; We look at the Content-Type field and expect to find a string of
;; the form "type/subtype".  There may be whitespace around the slash
;; and case is insignificant.  If we find a Content-Type field that we
;; can't understand, we use the fallback handler (rmime-*/*).  If we
;; can't find a Content-Type field, we assume the default type.  The
;; default type is usually text/plain, but the multipart/digest sets a
;; local default type of message/rfc822.
(defun rmime-handler (type)
  (if (and type (string-match "\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)[ \t]*/[ \t]*\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)" type))
      (or (rmime-handler-1 (downcase (substring type (match-beginning 1) (match-end 1)))
			   (downcase (substring type (match-beginning 2) (match-end 2))))
	  (identity 'rmime-*/*))
    (identity rmime-default-type)))

(defun rmime-handler-1 (basetype subtype)
  (or (rmime-handler-2 basetype subtype)
      (rmime-handler-2 basetype "*")))

(defun rmime-handler-2 (basetype subtype)
  (let ((handler (intern (concat "rmime-" basetype "/" subtype))))
    (if (fboundp handler) handler)))

;; This recursive function parses a MIME parameter list.
;;
;; parameter list is *(";" parameter)
;; parameter      is attribute "=" value
;; attribute      is token
;; token          is 1*[!#$%&'*+---.0-9A-Z^_a-z{|}~]
;; value          is a token or a quoted-string
;;
;; Whitespace is permitted to appear around any token.
;;
;; Notice that this regular expression is not anchored to the
;; beginning of the string.  This means that we will ignore any
;; garbage we happen to find during parsing and continue the best we
;; can.  We depend on this when we pass in a Content-Type field
;; without first removing the basetype/subtype clause.
;;
;; The string ";[ \t]*" matches the leading semicolon and any
;; whitespace that may come after it.
;;
;; The string "[!#$%&'*+---.0-9A-Z^_a-z{|}~]+" matches the attribute.
;;
;; The string "[ \t]*=[ \t]*" matches the equals sign and any
;; whitespace that may come around it.
;;
;; The string "[!#$%&'*+---.0-9A-Z^_a-z{|}~]+" matches the (unquoted)
;; value.
;;
;;				  -- or --
;;
;; The string "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"" matches the (quoted)
;;                  **********************
;;               ****************************
;; value.
;;
;; The string ".*" matches anything.
;;
;; Put them together to parse a parameter list.
(defun rmime-parameters (field)
  (if (and field (string-match "\\(;[ \t]*\\)\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)\\([ \t]*=[ \t]*\\)\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\|\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"\\)\\(.*\\)" field))
      (cons (cons (downcase (substring field (match-beginning 2) (match-end 2)))
		  (if (match-beginning 5)
		      (save-match-data (rmime-unquote (substring field (match-beginning 5) (match-end 5))))
		    (substring field (match-beginning 4) (match-end 4))))
	    (rmime-parameters (substring field (match-beginning 7) (match-end 7))))))

;; This function extracts MIME information from a buffer which hasn't
;; set rmime-mimeinfo-method.  We look for a function name
;; MAJORMODE-mimeinfo.  If we find it, we call it.  If we don't, we
;; call rmime-standard-mimeinfo.
;;
;; We use this function to add MIME support to major modes that were
;; written before this package was.
;;
;; This function is obsolete before it was even written.  In the
;; future, it will be removed.  rmime-mimeinfo-method will point to
;; rmime-standard-mimeinfo and buffers that need special coding must
;; bind a local value to this variable.
(defun rmime-obsolete-mimeinfo ()
  (let ((function (intern (concat (symbol-name major-mode) "-mimeinfo"))))
    (if (fboundp function)
	(funcall function)
      (rmime-standard-mimeinfo))))

;; This function extracts MIME information from a buffer which
;; contains a header and a body separated by a specified regular
;; expression.  Most buffers fall into this category.
;;
;; The regular expression is stored in rmime-separator-regexp.  This
;; may be a buffer-local variable.  The default value matches a blank
;; line.
;;
;; If the regular expression doesn't match anything, the entire buffer
;; is the header and the body is empty.  This is important when we
;; parse phantom bodies in rmime-message/external-body.
;;
;; Someday this function will be the value of rmime-mimeinfo-method
;; and rmime-obsolete-mimeinfo will no longer exist.
;;
;; MIME infomation is represented as:
;;  	(type description encoding disposition)
;; All fields are character strings or nil if the field is not
;; provided.
(defun rmime-standard-mimeinfo ()
  (cons (current-buffer) (rmime-standard-headers rmime-separator-regexp)))

(defun rmime-standard-headers (regexp)
  (goto-char (point-min))
  (let (case-fold-search)
    (let ((found (re-search-forward regexp nil 'move)))
      (save-excursion
	(save-restriction
	  (if found (narrow-to-region (point-min) (match-beginning 0)))
	  (rmime-fetch-mime-headers))))))

(defun rmime-fetch-mime-headers ()
  (let ((case-fold-search t))
    (mapcar 'rmime-fetch-header '("Content-Type" "Content-Transfer-Encoding"
				  "Content-Disposition" "Content-Description"))))

 
;;; Routines for RFC 822 header manipulations ---


;; This function extracts one header from a message.
;;
;; Any function which calls this function *must* set case-fold-search
;; to t and must also narrow the buffer to just the headers of a
;; message.  The blank line that separates the headers from the body
;; may optionally be included.
;;
;; The string "^xxx:" matches the header we are looking for.
;;
;; The string "\\(\n*[ \t]\\)*" matches any string of whitespace not
;;             ***************
;; ending with a newline.  We don't have to worry about encountering
;; two newlines in a row because we have narrowed our buffer.  In
;; fact, if we do encounter two newlines in a row, we assume that the
;; function that called us knew what it was doing and we treat the
;; blank line as part of a header.
;;
;; The string "[^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*" matches any
;;                        ***************
;;                     ******************************
;; sequence of characters that doesn't start or end with whitespace.
;; At least one character is required.  Any newlines must be followed
;; by a tab or a space.
(defun rmime-fetch-header (header)
  (goto-char (point-min))
  (if (re-search-forward (concat "\\(^" (regexp-quote header) ":\\)\\(\\(\n*[ \t]\\)*\\)\\([^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*\\)") nil t)
      (let ((answer (buffer-substring (match-beginning 4) (match-end 4))))
	(set-text-properties 0 (length answer) nil answer)
	(rmime-unfold answer))))

;; This recursive function unfolds RFC 822 header lines.
;;
;; RFC 822 doesn't say anything about whitespace at the end of a line,
;; but RFC 1521 implies that they should be blown away.
;;
;; RFC 822 says to replace the CRLF LWSP sequence with just the LWSP
;; char, but if we do this, the example in RFC 1521 won't work.  We
;; choose to replace CRLF LWSP with a space.
;;
;; The string "\\(.*[^ \t]\\)?" matches a string not ending with white
;;             ***************
;; space (including the empty string).
;;
;; The string "[ \t]*\n[ \t]*" matches a line break and any white
;; space around it.
;;
;; The string "\\(.\\|\n\\)*" matches anything (including linebreaks).
;;             *************
;;
;; Put them together and get some code to unfold RFC 822 header lines.
(defun rmime-unfold (string)
  (if (string-match "^\\(\\(.*[^ \t]\\)?\\)\\([ \t]*\n[ \t]*\\)\\(\\(.\\|\n\\)*\\)$" string)
      (concat (substring string (match-beginning 1) (match-end 1))
	      (identity " ")
	      (rmime-unfold (substring string (match-beginning 4) (match-end 4))))
    (identity string)))

;; This recursive function removes RFC 822 comments.
;;
;; Comments are text in parenthesis.
;;
;; Comments can nest.  Comments can't appear inside quotes.
;;
;; Except at the beginning or end of a line, comments are replaced
;; with a space.  This is always safe because comments can't appear in
;; an atom.  At the beginning or end of a line, they are just removed
;; (along with any whitespace around them).
;;
;; The string "\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*"
;;                                                **********************
;;                      *****************************************************
;;             ******************************************************************
;; matches any string with an even number of quote marks not ending
;; with white space.  Quote marks that are backquoted don't count.  We
;; won't match a string ending with a backslash.  We will match a
;; string ending with backslash space.
;;
;; The string "[ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*" matches a comment
;;                    **********************
;; and its surrounding white space.
;;
;; The string "\\([^ \t].*\\)?" matches any string not beginning with
;;             ***************
;; white space.
;;
;; Put them together and get some code to remove comments from headers.
(defun rmime-uncomment (string)
  (if (and string (string-match "^\\(\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*\\)\\([ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*\\)\\(\\([^ \t].*\\)?\\)$" string))
      (if (eq (match-beginning 1) (match-end 1))
	  (if (eq (match-beginning 7) (match-end 7))
	      (identity nil)
	    (rmime-uncomment (substring string (match-beginning 7) (match-end 7))))
	(if (eq (match-beginning 7) (match-end 7))
	    (rmime-uncomment (substring string (match-beginning 1) (match-end 1)))
	  (rmime-uncomment (concat (substring string (match-beginning 1) (match-end 1))
				   (identity " ")
				   (substring string (match-beginning 7) (match-end 7))))))
    (identity string)))

;; This recursive function removes backslashes to reveal the character
;; they protect.
;;
;; Note that this function has nothing to do with "this kind of
;; quoting".
;;
;; The string "[^\\]*" matches any string not containing a backslash.
;;
;; The string "\\\\" matches a backslash.
;;
;; The string "." matches a character following a backslash.
;;
;; The string ".*" matches anything.
;;
;; Put them together and get some code to remove backslashes.
(defun rmime-unquote (string)
  (if (and string (string-match "^\\([^\\]*\\)\\(\\\\\\)\\(.\\)\\(.*\\)$" string))
      (concat (substring string (match-beginning 1) (match-end 1))
	      (substring string (match-beginning 3) (match-end 3))
	      (rmime-unquote (substring string (match-beginning 4) (match-end 4))))
    (identity string)))

 
;;; Temporary glue for major modes that should know about MIME


;; Get MIME information from a RMAIL summary buffer.
(defun rmail-summary-mode-mimeinfo ()
  (set-buffer rmail-buffer)
  (rmail-mode-mimeinfo))

;; Get MIME information from a RMAIL buffer.
;;
;; We can't use rmime-standard-mimeinfo because an important header
;; line may be part of rmail-ignored-headers.
(defun rmail-mode-mimeinfo ()

  ;; Shut up the byte compiler.
  (eval-when-compile (require 'rmail))

  ;; Extract MIME information.
  (goto-char (point-min))
  (re-search-forward "\\(\\`\\|\n\\)[ \t]*\n" nil 'move)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (aref rmail-message-vector rmail-current-message))
	(narrow-to-region (point) (point-max))
	(rmime-standard-mimeinfo)))))

;; Get MIME information from a *mail* buffer or *post-news* buffer.
;;
;; It is wrong to terminate the regular expression with "$" instead of
;; "\n".  Using "$" will cause the newline at the end of the separator
;; to be interpreted as the first character of the body.
(defun mail-mode-mimeinfo ()
  (prog1
      (cons (current-buffer) (rmime-standard-headers (concat "^" (regexp-quote mail-header-separator) "\n")))
    (let ((window (selected-window)))
      (pop-to-buffer (concat (buffer-name) "-mime"))
      (save-excursion
	(select-window window)))))
(defalias 'news-reply-mode-mimeinfo 'mail-mode-mimeinfo)

;; Get MIME information from a +inbox buffer.
(defun mh-folder-mode-mimeinfo ()

  ;; Shut up the byte compiler.
  (eval-when-compile (require 'mh-e))

  ;; Select the output buffer.
  (set-buffer (get-buffer-create mh-show-buffer))

  ;; See if all the header lines are available.
  (if (or (identity mh-showing-with-headers)
	  (not (or mhl-formfile mh-clean-message-header)))

      ;; Wonderful!  All the headers are present.
      (rmime-standard-mimeinfo)

    ;; Obscene!  We have to read the file again to get at the headers.
    (goto-char (point-min))
    (re-search-forward "^[ \t]*\n" nil 'move)
    (let ((filename buffer-file-name)
	  (name (generate-new-buffer-name "*mh-headers*")))
      (unwind-protect
	  (cons (current-buffer) (save-excursion (set-buffer (get-buffer-create name))
						 (insert-file-contents filename)
						 (rmime-standard-headers "^[ \t]*\n")))
	(kill-buffer name)))))

;; Get MIME information from an INBOX buffer.
(defun vm-mode-mimeinfo ()

  ;; We can't compile the rest of this function because some of the
  ;; subroutines we need are really macros.
  (dont-compile

    ;; Make sure the variables and macros used by vm-mode-mimeinfo are
    ;; defined.
    (require 'vm)

    ;; Check that vm is configured right.
    ;;
    ;; vm-preview-current-message sets the restriction before calling
    ;; vm-select-message-hook.  We are called inside a
    ;; save-restriction so we can't change our restriction.  We can't
    ;; edit outside of our restriction or else parts of the following
    ;; message might show after save-restriction restores the old
    ;; restriction.
    (or (eq vm-preview-lines t)
	(error "Sorry --- you must (setq vm-preview-lines t) to use VM with RMIME"))

    ;; Extract MIME information.
    (let (case-fold-search)
      (prog1
	  (save-restriction
	    (narrow-to-region (vm-headers-of (car vm-message-pointer))
			      (vm-text-of    (car vm-message-pointer)))
	    (rmime-standard-mimeinfo))
	(goto-char (vm-text-of (car vm-message-pointer)))))))

 
;;; RMIME is available!


(run-hooks 'rmime-load-hook)		; For user customizations.
(provide 'rmime)

;;; rmime.el ends here
