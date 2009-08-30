;;; mh-crypt.el --- Manage encrypted or compressed MH messages.
;;;
;;; Copyright (C) 1994 Frederick Knabe
;;;
;;; Author:     Fritz Knabe <knabe@acm.org>
;;; Created:    31 Jan 1994
;;; $Revision: 1.21 $
;;;
;;;
;;; LCD Archive Entry:
;;; mh-crypt|Fritz Knabe|knabe@acm.org|
;;; Store and retrieve MH messages in encrypted or compressed form.|
;;; $Date: 1998/05/28 12:45:19 $|$Revision: 1.21 $|~/misc/mh-crypt.el|
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; If you did not receive a copy of the GNU General Public License with
;;; this program, write to the Free Software Foundation, Inc., 675 Mass
;;; Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; DESCRIPTION
;;;
;;; mh-crypt allows you to store and manipulate your MH messages in
;;; encrypted or compressed ("encoded") form. Because mh-crypt leaves
;;; some or all of the header fields in plaintext, MH and the mh-e
;;; package are still able to "understand" encrypted and encoded
;;; messages. Using mh-crypt you can then view messages in plaintext
;;; form.
;;;
;;; This package is *not* a public key/digital signature system; it is a
;;; way to store MH messages in a more secure or compact form than
;;; plaintext. For public key encryption in Emacs, see Jin Choi's very
;;; useful mailcrypt.el.
;;;
;;; WARNING: The default settings for this package leave potentially
;;; valuable information (all message header fields) as plaintext. Make
;;; sure to change the user variables if this is not appropriate for
;;; you.
;;;
;;; ANOTHER WARNING: Before you start encrypting your messages, remember
;;; that if you forget your encryption key, your messages will be lost!
;;;
;;; LAST WARNING: You may want to try mh-crypt on some dummy messages
;;; and folders first, just in case it is not compatible with your
;;; versions of crypt, mh-e, etc.
;;;
;;;
;;; INSTALLATION
;;;
;;; This package requires crypt++.el, which is available via anonymous
;;; ftp to archive.cis.ohio-state.edu [128.146.8.52] in
;;; /pub/gnu/emacs/elisp-archive/misc/crypt++.el.Z. You must have an
;;; encryption program (such as DES or PGP) or a compression program
;;; (such as compress or gzip) installed on your system that crypt++ can
;;; use. This package also require advice.el, which comes with FSF Emacs
;;; 19. advice.el is also available from the same ftp site as above in
;;; .../packages/advice.el.Z.
;;;
;;; Place mh-crypt.el somewhere on your load path, optionally
;;; byte-compile it, and add the following line to your .emacs:
;;;
;;;     (eval-after-load "mh-e" '(require 'mh-crypt))
;;;
;;; You may also want to set some of the user customization variables
;;; (described below) in your .emacs.
;;;
;;; USE
;;;
;;; When mh-crypt is loaded, it will add new key bindings to
;;; mh-folder-mode:
;;;
;;; Binding            Function
;;;
;;;   E                 mhc-encrypt-msg
;;;   M-E               mhc-encrypt-folder
;;;   C                 mhc-encode-msg
;;;   M-C               mhc-encode-folder
;;;   D                 mhc-decrypt-or-decode-msg
;;;   M-D               mhc-decrypt-or-decode-folder
;;;
;;; mhc-encrypt-msg: Encrypts the current message on disk. With a prefix
;;;     argument, prompts for a message sequence to encrypt.
;;;
;;; mhc-encrypt-folder: Encrypts all the messages in the current folder
;;;     and performs a rescan. With a prefix argument, prompts for
;;;     the range of messages to rescan.
;;;
;;; mhc-encode-msg: Same as mhc-encrypt-msg, but for encoding.
;;;
;;; mhc-encode-folder: Same as mhc-encrypt-folder, but for encoding.
;;;
;;; mhc-decrypt-or-decode-msg: Decrypts or decodes the current message
;;;     into the show buffer, automatically deciding which to do. The
;;;     stored version of the message remains unchanged. With a prefix
;;;     argument, decrypts or decodes the current message on disk. With
;;;     a prefix argument of 0, prompts for a message sequence to
;;;     decrypt/decode on disk.
;;;
;;; mhc-decrypt-or-decode-folder: Decrypts or decodes all the messages
;;;     in the current folder, choosing the proper action for each
;;;     message, and performs a rescan. With a prefix argument, prompts
;;;     for the range of messages to rescan.
;;;
;;; In addition, the functions mhc-decrypt-msg and mhc-decode msg are
;;; available but not bound to keys. These functions simply try to
;;; decrypt or decode directly. They are provided in case the X-MH-Crypt
;;; header necessary for automatic decisions somehow becomes lost or
;;; corrupted.
;;;
;;; mhc-deactivate-key can be used to explicitly deactivate your
;;; encryption/decryption key and remove it from the cache. See
;;; mhc-key-timeout below.
;;;
;;;
;;; CUSTOMIZATION
;;;
;;; The following variables can be used to affect the behavior of mh-crypt:
;;;
;;; mhc-move-headers: If non-nil, mh-crypt will move all header fields
;;;     not specified in mhc-preserved-fields into the body of the
;;;     message during encryption, causing these header fields to be
;;;     encrypted. On decryption, mh-crypt will restore these header
;;;     fields. This variable is buffer-local; see its documentation
;;;     below for an example of its use in a buffer-local way.
;;;
;;; mhc-preserved-fields: A list of the fields to leave in plaintext if
;;;     mhc-move-headers is non-nil.
;;;
;;; mhc-key-timeout: The length of time to cache your key after a use;
;;;     nil means don't cache. Having your key cached means you don't
;;;     have to type it in each time you need to use it. If you are
;;;     using Emacs 19, after a period of time (default ten minutes), the
;;;     key will automatically be deactivated. Be aware that after you
;;;     type in your password, it will still be visible with the
;;;     view-lossage command (C-h l) until 100 keystrokes have been
;;;     made.
;;;
;;; mhc-auto-crypt: Non-nil means mh-crypt will automatically decrypt
;;;     encrypted messages (or decode encoded messages) in the show
;;;     buffer whenever they are displayed. If your key is currently
;;;     cached, this operation is transparent (since mh-crypt doesn't
;;;     have to ask you for a key).
;;;
;;; NOTES
;;;
;;; * For automatic decryption/decoding to work, the field "X-MH-Crypt:"
;;;   must exist in the message header. This field is normally added by
;;;   mh-crypt. However, the mh-e header cleaning process could make it
;;;   invisible if you have set the mh-e cleaning variables. Therefore,
;;;   if you set mh-visible-headers in your .emacs, you should add
;;;   "X-MH-Crypt:" to the headers matched by the regexp. If you instead
;;;   set mh-invisible-headers, you should make sure that "X-MH-Crypt:"
;;;   is not matched by the regexp.
;;;
;;; * If you want, you can multiply encrypt or encode messages. This is
;;;   unlikely to improve security and may even weaken it. Multiple
;;;   encodings can perhaps be useful (for example, convert to DOS
;;;   format and then compress), or you may want to compress and then
;;;   encrypt a message (though this can provide a means of attack on
;;;   the encryption system; see the documentation for crypt++). If you
;;;   choose to do this, on decryption or decoding mh-crypt will undo
;;;   one level of encryption or encoding at a time.
;;;
;;; * If you set require-final-newline in your .emacs, this can
;;;   interfere with the saving of encrypted or encoded messages. You can
;;;   unset require-final-newline in your mh-folder-mode-hook. Thanks to
;;;   Henry Guillaume <henryg@tusc.com.au> for pointing this out.
;;;


(require 'mh-e)
(eval-and-compile
  (if (fboundp 'mh-version)
      (require 'mh-pick)))  ; For mh-e 4.x
(require 'crypt++)
(require 'advice)

;;; User customization

(defvar mhc-move-headers nil
  "*Non-nil means mh-crypt will encrypt message headers.
Those fields specified in mhc-preserved-fields will not be encrypted. On
decryption, mh-crypt will check for and restore encrypted headers. This
variable is buffer-local and can be set in your mh-folder-mode-hook. For
example:

    (add-hook 'mh-folder-mode-hook
              (function (lambda ()
                          (if (equal mh-current-folder \"+private\")
                              (setq mhc-move-headers t)))))

The default value is nil.")

(make-variable-buffer-local 'mhc-move-headers)

(defvar mhc-preserved-fields '("Date" "From" "To")
  "*A list of header fields that mh-crypt should leave as plaintext.
\"From\" and \"Date\" should generally be included at a minimum, as
without them MH scan listings will be very uninformative.")

(defvar mhc-key-timeout "10 min" 
  "*Time to deactivate key in after a use, or nil for immediately.
If you are not running Emacs 19, mh-crypt will not cache keys and will
remind you of this until this variable is set to nil.")

(defvar mhc-auto-crypt t
  "*If non-nil, MH messages will be decrypted or decoded when displayed.")

;;; Code

(defvar mhc-key nil "The cached key value.")

(defvar mhc-decrypted nil)
(make-variable-buffer-local 'mhc-decrypted)
(or (assq 'mhc-decrypted minor-mode-alist)
    (setq minor-mode-alist (cons '(mhc-decrypted mhc-decrypted)
     minor-mode-alist)))

(defun mhc-encrypt-folder (range)
  "Encrypt all messages in the current folder. 
If optional prefix argument is provided, prompt for the range of
messages to display after encrypting. Otherwise, show the entire
folder."
  (interactive (list (if current-prefix-arg
                         (mh-read-msg-range
                          "Range to scan after encrypting [all]? "))))
  (mhc-enc-folder t range))

(defun mhc-encode-folder (range)
  "Encode all messages in the current folder. 
If optional prefix argument is provided, prompt for the range of
messages to display after encoding. Otherwise, show the entire
folder."
  (interactive (list (if current-prefix-arg
                         (mh-read-msg-range
                          "Range to scan after encoding [all]? "))))
  (mhc-enc-folder nil range))

(defun mhc-enc-folder (encrypt range)
  ;; Encrypt or encode all messages in the current folder. Flag ENCRYPT
  ;; is non-nil for encryption, nil for encoding. RANGE specifies the
  ;; messages to display after encrypting.
  (if (or range mh-do-not-confirm
          (y-or-n-p (format "%s folder %s? "
       (if encrypt "Encrypt" "Encode")
       mh-current-folder)))
      (progn
        (if (null range) (setq range "all"))
        ;; Remove any leftover (i.e. no associated file name) show
        ;; buffer with plaintext in it
        (let ((show-buffer (get-buffer mh-show-buffer)))
          (if (and show-buffer (null (buffer-file-name show-buffer)))
              (progn
                (delete-windows-on show-buffer)
                (kill-buffer show-buffer))))
        ;; Build an internal sequence of all the messages
        (mh-add-msgs-to-seq
  (mh-seq-from-command mh-current-folder 'dummy
         (list "pick" "all" mh-current-folder))
  'all t)
        (mhc-enc-msg 'all encrypt)
        (mh-delete-seq-locally 'all)
        (mh-regenerate-headers range))))

(defun mhc-encrypt-msg (msg-or-seq)
  "Encrypt the specified messages on disk. Default is the displayed message.
If optional prefix argument is provided then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
                         (mh-read-seq "Encrypt" t)
                       (mh-get-msg-num t))))
  (mhc-enc-msg msg-or-seq t))

(defun mhc-encode-msg (msg-or-seq)
  "Encode the specified messages on disk. Default is the displayed message.
If optional prefix argument is provided then prompt for the message sequence."
  (interactive (list (if current-prefix-arg
                         (mh-read-seq "Encode" t)
                       (mh-get-msg-num t))))
  (mhc-enc-msg msg-or-seq nil))

(defun mhc-enc-msg (msg-or-seq encrypt)
  ;; Encrypt or encode messages on disk. Default is the displayed
  ;; message. MSG-OR-SEQ specifies the messages. Flag ENCRYPT is non-nil
  ;; for encryption, nil for encoding.
  (let ((key-or-type
  (if encrypt
      (mhc-get-key "Encrypt using key: " "Encrypt using cached key? " t)
    (crypt-read-encoding-type)
    crypt-buffer-encoding-type)))
    (if (numberp msg-or-seq)
        (mhc-enc-a-msg msg-or-seq encrypt key-or-type)
      (mhc-map-to-seq-msgs 'mhc-enc-a-msg msg-or-seq encrypt key-or-type)
      (message "Done."))))

(defun mhc-enc-a-msg (msg encrypt key-or-type)
  (let* ((local-mhc-move-headers mhc-move-headers)
         (msg-filename (mh-msg-filename msg))
         (show-buffer (get-buffer mh-show-buffer))
         (msg-shown (and show-buffer
                         (equal msg-filename (buffer-file-name show-buffer)))))
    (save-excursion
      (set-buffer (create-file-buffer msg-filename))
      (insert-file-contents msg-filename t)
      (mhc-alter-header encrypt local-mhc-move-headers key-or-type)
      (mhc-goto-header-end 1)
      (if encrypt
   (crypt-encrypt-region (point) (point-max) key-or-type)
 (let ((crypt-buffer-encoding-type key-or-type))
   (crypt-encode-region (point) (point-max))))
      (save-buffer)
      (kill-buffer nil))
    ;; If the message is shown, then if mhc-auto-crypt is on, change
    ;; mhc-decrypted appropriately, otherwise invalidate the show buffer
    ;; and redisplay.
    (if msg-shown
 (if mhc-auto-crypt
     (save-excursion
       (set-buffer show-buffer)
       (setq mhc-decrypted (if encrypt " Encrypted" " Encoded")))
   (save-excursion
     (save-window-excursion
       (mh-invalidate-show-buffer)
       (mh-display-msg msg (buffer-name))))))))

(defun mhc-decrypt-or-decode-folder (range)
  "Decrypt or decode (as appropriate) all messages in the current folder.
If optional prefix argument is provided, prompt for the range of
messages to display after decrypting/decoding.  Otherwise, show the entire
folder."
  (interactive (list (if current-prefix-arg
                         (mh-read-msg-range
                          "Range to scan after decrypting/decoding [all]? "))))
  (if (or range mh-do-not-confirm
          (y-or-n-p (format "Decrypt/Decode folder %s? " mh-current-folder)))
      (progn
        (if (null range) (setq range "all"))
        ;; Build an internal sequence of all the messages
 (mh-add-msgs-to-seq
  (mh-seq-from-command mh-current-folder 'dummy
         (list "pick" "all" mh-current-folder))
  'all t)
        (mhc-decrypt-or-decode-msg 'all)
        (mh-delete-seq-locally 'all)
        (mh-regenerate-headers range))))

(defun mhc-decrypt-or-decode-msg (msg-or-seq &optional decrypt-on-disk)
  "Decrypt or decode the specified message into the show buffer. 
Default is the displayed message. If optional prefix argument is provided
then decrypt/decode on disk. If prefix argument is 0 then prompt for a
message sequence and decrypt/decode the messages to disk."
  (interactive (list (if (and (numberp current-prefix-arg)
                              (eq current-prefix-arg 0))
                         (mh-read-seq "Decrypt/Decode" t)
                       (mh-get-msg-num t))
                     current-prefix-arg))
  (mhc-dec-msg msg-or-seq 'guess decrypt-on-disk))

(defun mhc-decrypt-msg (msg-or-seq &optional decrypt-on-disk)
  "Decrypt the specified message into the show buffer. 
Default is the displayed message. If optional prefix argument is
provided then decrypt on disk. If prefix argument is 0 then prompt for a
message sequence and decrypt the messages to disk."
  (interactive (list (if (and (numberp current-prefix-arg)
                              (eq current-prefix-arg 0))
                         (mh-read-seq "Decrypt" t)
                       (mh-get-msg-num t))
                     current-prefix-arg))
  (mhc-dec-msg msg-or-seq 'decrypt decrypt-on-disk
        (mhc-get-key "Decrypt using key: "
       "Decrypt using cached key? ")))

(defun mhc-decode-msg (msg-or-seq &optional decode-on-disk)
  "Decode the specified message into the show buffer. 
Default is the displayed message. If optional prefix argument is
provided then decode on disk. If prefix argument is 0 then prompt for a
message sequence and decode the messages to disk."
  (interactive (list (if (and (numberp current-prefix-arg)
                              (eq current-prefix-arg 0))
                         (mh-read-seq "Decode" t)
                       (mh-get-msg-num t))
                     current-prefix-arg))
  (mhc-dec-msg msg-or-seq 'decode decode-on-disk
        (progn
   (crypt-read-encoding-type)
   crypt-buffer-encoding-type)))

(defun mhc-dec-msg (msg-or-seq action &optional decrypt-on-disk key-or-type)
  ;; Decrypt or decode the specified message into the show buffer.
  (if (numberp msg-or-seq)
      (mhc-dec-a-msg msg-or-seq action key-or-type decrypt-on-disk)
    (mhc-map-to-seq-msgs 'mhc-dec-a-msg msg-or-seq action key-or-type t)
    (message "Done.")))

(defvar mhc-guess-flag nil)

(defun mhc-dec-a-msg (msg action key-or-type decrypt-on-disk)
  (while (not (if decrypt-on-disk
                  (mhc-dec-on-disk msg (eq action 'decrypt) key-or-type
       (eq action 'guess))
                (mhc-dec-buffer msg (eq action 'decrypt) key-or-type
    (eq action 'guess))))
    (cond
     ((or (eq action 'decrypt) (and (eq action 'guess)
        (eq mhc-guess-flag 'decrypt)
        (setq action 'decrypt)))
      (beep)
      (setq key-or-type
     (mhc-get-key
      (format "Decryption of message %s failed. Decrypt using key: "
       msg))))
     ((or (eq action 'decode)
   (and (eq action 'guess) (eq mhc-guess-flag 'decode)))
      (error "Decoding of message %s failed." msg))
     ((eq mhc-guess-flag 'badmethod)
      (error "Message %s requires an unavailable decoding method." msg))
     ((eq mhc-guess-flag 'nofield)
      (error "Message %s has no visible X-MH-Crypt field." msg))
     ((eq mhc-guess-flag 'quit)
      (signal 'quit nil)))))

(defun mhc-dec-on-disk (msg decrypt key-or-type guess)
  (let* ((msg-filename (mh-msg-filename msg))
         (show-buffer (get-buffer mh-show-buffer))
         (msg-shown (and show-buffer
                         (equal msg-filename
                                (buffer-file-name show-buffer))))
         mhc-auto-crypt decrypt-okay guesses)
    (save-excursion
      (set-buffer (create-file-buffer msg-filename))
      (insert-file-contents msg-filename t)
      (mhc-goto-header-end 1)
      (if (and guess (setq guesses (mhc-guess)))
   (setq decrypt (car guesses)
  key-or-type (cdr guesses)))
      (if (not (and guess (null guesses)))
   (progn
     (if decrypt
  (crypt-encrypt-region (point) (point-max) key-or-type t)
       (let ((crypt-buffer-encoding-type key-or-type))
  (crypt-encode-region (point) (point-max) t)))
     (setq decrypt-okay (mhc-restore-header))))
      (if decrypt-okay
   (save-buffer)
 (set-buffer-modified-p nil))
      (kill-buffer nil))
    (if (and decrypt-okay msg-shown)
 (save-excursion
   (save-window-excursion
     (mh-invalidate-show-buffer)
     (mh-display-msg msg (buffer-name)))))
    decrypt-okay))

(defun mhc-dec-buffer (msg decrypt key-or-type guess)
  (let (mhc-auto-crypt decrypt-okay guesses)
    (mh-invalidate-show-buffer)
    (mh-show msg)
    (save-excursion
      (set-buffer mh-show-buffer)
      (goto-char (point-min))
      (mhc-goto-header-end 1)
      (if (and guess (setq guesses (mhc-guess)))
   (setq decrypt (car guesses)
  key-or-type (cdr guesses)))
      (if (not (and guess (null guesses)))
   (progn
     (if decrypt
  (crypt-encrypt-region (point) (point-max) key-or-type t)
       (let ((crypt-buffer-encoding-type key-or-type))
  (crypt-encode-region (point) (point-max) t)))
     (setq decrypt-okay (mhc-restore-header))))
      (if decrypt-okay
   (progn
     (goto-char (point-min))
     (setq mhc-decrypted (if decrypt " Encrypted" " Encoded"))
     (set-buffer-modified-p nil))))
    (if (not decrypt-okay)  ; Outside save-excursion, so test again
 (progn
   (mh-invalidate-show-buffer)
   (mh-show msg)))
    decrypt-okay))

(defun mhc-guess ()
  ;; Determine whether this buffer should be decoded or decrypted, and
  ;; get the encoding type or decryption key, respectively.
  (setq mhc-guess-flag nil)
  (condition-case nil   ; In case user quits during read
      (let (decrypt key-or-type)
 (save-excursion
   (goto-char (point-min))
   (mhc-goto-header-end 0)
   (if (re-search-backward "^X-MH-Crypt:" nil t)
       (progn
  (setq decrypt
        (not (looking-at "X-MH-Crypt:.*Encoded \\(.*\\)")))
  (if decrypt
      (setq mhc-guess-flag 'decrypt
     key-or-type (mhc-get-key "Decrypt using key: " t))
    (setq mhc-guess-flag 'decode
   key-or-type
   (intern (buffer-substring (match-beginning 1)
        (match-end 1)))))
  (if (or decrypt (crypt-get-encoding-program key-or-type))
      (cons decrypt key-or-type)
    (setq mhc-guess-flag 'badmethod)
    nil))
     (setq mhc-guess-flag 'nofield)
     nil)))
    (quit (setq mhc-guess-flag 'quit)
   nil)))

(defun mhc-do-auto-crypt ()
  "Automatically decrypt or decode the current buffer containing a message."
  (if mhc-auto-crypt
      (let (decrypt key-or-type guess)
 (if (setq guess (mhc-guess))
     (condition-case nil  ; In case user quits during read
  (progn
    (setq decrypt (car guess)
   key-or-type (cdr guess))
    (while
        (save-excursion
   (goto-char (point-min))
   (mhc-goto-header-end 1)
   (undo-boundary)
   (if decrypt
       (crypt-encrypt-region (point) (point-max)
        key-or-type t)
     (let ((crypt-buffer-encoding-type key-or-type))
       (crypt-encode-region (point) (point-max) t)))
   (if (mhc-restore-header) ; t if successful action
       (progn
         (goto-char (point-min))
         (setq mhc-decrypted
        (if decrypt " Encrypted" " Encoded"))
         (set-buffer-modified-p nil)
         nil) ; Don't enter while body
     (primitive-undo 1 buffer-undo-list)
     (beep)
     (if decrypt
         t  ; Enter while body
       (message "Decoding failed.")
       nil)))
      ;; The body of the while
      (setq key-or-type
     (mhc-get-key
      "Decryption failed. Decrypt using key: "))))
       (quit (beep)))
   (if (eq mhc-guess-flag 'quit) (beep))))))

(defadvice mh-display-msg (after mhc-display-msg act)
  "If mhc-auto-crypt is non-nil, decrypt or decode messages when shown."
  (save-excursion
    (if mh-show-buffer (set-buffer mh-show-buffer))
    (mhc-do-auto-crypt)))

(defvar mhc-timer nil "Timer object for key deactivation.")

(defun mhc-get-key (prompt &optional use-cache confirm)
  ;; Use PROMPT to read and cache an MH-Crypt key, resetting the
  ;; timer. If USE-CACHE is t, the cached value of the key is used, if
  ;; it exists. If USE-CACHE is a string, use it to enquire whether to
  ;; use the current cached value.  Possibly CONFIRM key.

  ;; We copy the cached key so that if the timer goes off while we're in
  ;; this function, we can't lose our copy. However, we must take care
  ;; to remove this copy on a quit. There are still some race conditions
  ;; (for example, we can possibly get a cached key of all nulls), but
  ;; these are unlikely.
  (let ((key (and use-cache (copy-sequence mhc-key))))
    (if (not (and key (or (eq use-cache t)
     (and (stringp use-cache)
          (condition-case nil
       (prog1
           (y-or-n-p use-cache)
         (message ""))
     (quit (fillarray key 0) ; Erase copy
           (signal 'quit nil)))))))
 (progn
   (if (arrayp key) (fillarray key 0)) ; Erase copy
   (setq key (crypt-read-string-no-echo
       prompt (and confirm crypt-confirm-password)))))
    (if mhc-key-timeout
 (if (fboundp 'run-at-time)
     (progn
       (if mhc-timer (cancel-timer mhc-timer))
       (setq mhc-timer (run-at-time mhc-key-timeout 
        nil 'mhc-deactivate-key))
       (if (arrayp mhc-key) (fillarray mhc-key 0)) ; Erase key
       (setq mhc-key key))
   (message
    "This version of Emacs does not support timers. Key not cached."))
      (setq mhc-timer nil))
    key))

(defun mhc-deactivate-key ()
  "Deactivates the MH-Crypt encryption/decryption key."
  (interactive)
  (fillarray mhc-key 0)   ; Overwrite the string in memory
  (setq mhc-key nil)
  (and mhc-timer (fboundp 'cancel-timer) (cancel-timer mhc-timer))
  (message "MH-Crypt password deactivated"))

(defun mhc-map-to-seq-msgs (func seq &rest args)
  ;; Invoke the FUNCTION on each message in the SEQUENCE, passing the
  ;; remaining ARGS as arguments.
  (save-excursion
    (let ((msgs (mh-seq-to-msgs seq)))
      (while msgs
        (apply func (car msgs) args)
        (setq msgs (cdr msgs))))))

(defun mhc-alter-header (encrypt encrypt-fields type)
  ;; If flag ENCRYPT is non-nil, we are encrypting, otherwise encoding.
  ;; If ENCRYPT-FIELDS is t, moves header fields to be preserved into a
  ;; new header. The other fields are moved to the end of the message.
  ;; In any case, add an X-MH-Crypt field to the header and to the end
  ;; of the message (where it can be used to detect failed decryption).
  ;; Assumed to start with point at point-min.  Returns with point at
  ;; point-min. TYPE carries the type of encoding, if ENCRYPT is nil.
  (let ((case-fold-search t)
        (header-start (copy-marker (point-min)))
        (header-end (progn (mhc-goto-header-end 0)
                           (copy-marker (point))))
        (fields mhc-preserved-fields)
        (bofd (make-marker))
        (eofd (make-marker))
        (moved-at-least-one nil)
 (xmh-crypt-field (concat "X-MH-Crypt: "
     (if encrypt "Encrypted"
       (concat "Encoded " (symbol-name type)))
     "\n")))
    (while (and encrypt-fields (consp fields))
      (goto-char header-end)
      (if (re-search-backward (format "^%s:" (car fields)) header-start t)
          (progn
            (setq moved-at-least-one t)
            (set-marker bofd (point))
            (end-of-line)
            (set-marker eofd (+ 1 (point))) ; Add 1 to get newline
            (goto-char header-start)
            (insert-before-markers (buffer-substring bofd eofd))
            (delete-region bofd eofd)))
      (setq fields (cdr fields)))
    (goto-char header-end)
    (insert-string xmh-crypt-field)
    (goto-char (point-max))
    (insert-string (concat "\n\n" xmh-crypt-field))
    (if moved-at-least-one
        (progn
          (insert-string (buffer-substring header-start header-end))
          (delete-region header-start header-end)))
    (goto-char (point-min))))

(defun mhc-restore-header ()
  ;; Deletes the X-MH-Crypt field in the header. Then joins the
  ;; previously encrypted headers from the end of the message with the
  ;; existing header, and once again removes the X-MH-Crypt
  ;; field. Leaves point at the end of the header. Returns t if this
  ;; message was properly decrypted (all the X-MH-Crypt fields are
  ;; there).
  (let ((stored-fields (make-marker))
        pos)
    (goto-char (point-min))
    (mhc-goto-header-end 0)
    (condition-case nil
        (progn
          ;; We try to remove an X-MH-Crypt field in the header. But if it's
          ;; not there (perhaps due to header cleaning by mh-e), we continue.
          (if (re-search-backward "^X-MH-Crypt:" nil t)
              (progn
                (setq pos (point))
                (beginning-of-line 2)
                (delete-region pos (point)))) ; Remove the X-MH-Crypt field
          ;; Find the stored fields
          (goto-char (point-max))
          (re-search-backward "\n\n")
          (delete-char 2)               ; Delete the newlines
          (set-marker stored-fields (point))
          (goto-char (point-min))
          (mhc-goto-header-end 0)
          ;; Move the stored fields
          (insert-before-markers (buffer-substring stored-fields (point-max)))
          (delete-region stored-fields (point-max))
          (goto-char (point-min))
          (mhc-goto-header-end 0)
          (re-search-backward "^X-MH-Crypt:")
          (setq pos (point))
          (beginning-of-line 2)
          (delete-region pos (point))   ; Remove the X-MH-Crypt field
          t)
      (search-failed nil))))

(defun mhc-goto-header-end (arg)
  ;; Find the end of the message header in the current buffer and position
  ;; the cursor at the ARG'th newline after the header.
  (if (re-search-forward "^$\\|^-+$" nil t)
      (forward-line arg)))

(define-key mh-folder-mode-map "E" 'mhc-encrypt-msg)
(define-key mh-folder-mode-map "C" 'mhc-encode-msg)
(define-key mh-folder-mode-map "D" 'mhc-decrypt-or-decode-msg)
(define-key mh-folder-mode-map "\eE" 'mhc-encrypt-folder)
(define-key mh-folder-mode-map "\eC" 'mhc-encode-folder)
(define-key mh-folder-mode-map "\eD" 'mhc-decrypt-or-decode-folder)

(provide 'mh-crypt)

;;; mh-crypt.el ends here
