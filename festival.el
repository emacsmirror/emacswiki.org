;;; festival.el --- emacs interface into festival.
;; Copyright 1999,2000 by Dave Pearson <davep@davep.org>
;; $Revision: 1.6 $

;; festival.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;; 
;; festival.el provides a simple interface into the festival speech
;; synthesis program <URL:http://www.cstr.ed.ac.uk/projects/festival.html>
;; from emacs lisp.
;;
;; BTW, it was only once I'd more or less finished writing the first version
;; of this that I noticed that a festival.el comes with festival itself
;; (yeah, I know, I should spend more time examining the contents of
;; software packages). As it is, I decided to press on with this anyway
;; because I'd done a couple of different things and, more importantly, I
;; was having far too much fun.

;;; Code:

;; Attempt to handle older/other emacs.
(eval-and-compile
  ;; If customize isn't available just use defvar instead.
  (unless (fboundp 'defgroup)
    (defmacro defgroup  (&rest rest) nil)
    (defmacro defcustom (symbol init docstring &rest rest)
      `(defvar ,symbol ,init ,docstring)))
  ;; Seems pre-20 emacs doesn't have with-current-buffer.
  (unless (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      `(save-current-buffer (set-buffer ,buffer) ,@body))))

;; Customize options.

(defgroup festival nil
  "festival.el - Interface to the festival speech synthesis program."
  :group 'external
  :prefix "festival-")

(defcustom festival-program "/usr/bin/festival"
  "*Location of the festival program."
  :type  '(file :must-match t)
  :group 'festival)

(defcustom festival-buffer "*festival*"
  "*Name of buffer to attach to the festival process.

Set this to NIL if you don't want a buffer created."
  :type  '(choice (string :tag "Buffer name")
                  (const  :tag "Don't attach a buffer" nil))
  :group 'festival)

(defcustom festival-default-audio-mode 'async
  "*Default audio_mode for a new festival process."
  :type  '(choice (const async)
                  (const sync)
                  (const close)
                  (const shutup)
                  (const query))
  :group 'festival)

(defcustom festival-default-voice 'festival-voice-english-male
  "*Default voice."
  :type  '(choice (const :tag "English, male" festival-voice-english-male)
                  (const :tag "US, male"      festival-voice-US-male))
  :group 'festival)

(defcustom festival-voices-alist '(("english-fair" . festival-voice-english-fair)
                                   ("english-male" . festival-voice-english-male)
                                   ("us-male"      . festival-voice-US-male))
  "*alist of voice name to set-function mappings."
  :type  '(repeat (cons string function))
  :group 'festival)

(defcustom festival-auto-start t
  "*Should festival start when any of the functions are called?"
  :type  'boolean
  :group 'festival)

;; Non-customize variables.

(defvar festival-process nil
  "Process handle for the festival program.")

;; Main code.

;;;###autoload
(defun festival-start ()
  "Start a festival process. If a process is already running, this is a no-op."
  (interactive)
  (let ((proc-name "festival"))
    (unless (get-process proc-name)
      (setq festival-process (start-process proc-name festival-buffer festival-program))
      ;; (process-kill-without-query festival-process)
      (process-query-on-exit-flag festival-process)
      (festival-audio-mode festival-default-audio-mode)
      (funcall festival-default-voice))))

(defun festival-stop ()
  "Stop a festival process. If there is no process running this is a no-op."
  (interactive)
  (when (processp festival-process)
    (kill-process festival-process))
  (setq festival-process nil))

(defun festivalp ()
  "Return `t' if a festival process is running, NIL if not.

Note that if `festival-auto-start' is set to t this function will always
return t and, if a festival proecss isn't running, it will start one for
you."
  (let ((festivalp (processp festival-process)))
    (when (and (not festivalp) festival-auto-start)
      (festival-start)
      (setq festivalp t))
    festivalp))

(defun festival-send (format &rest args)
  "Send text to the festival process, FORMAT is a `format' format string."
  (when (festivalp)
    (process-send-string festival-process (apply #'format format args))))

;;;###autoload
(defun festival-audio-mode (mode)
  "Set the festival audio mode to MODE.

See the festival documentation for a list of valid modes."
  (festival-send "(audio_mode '%s)\n" mode))

;;;###autoload
(defun festival-say (text)
  "Say TEXT via the festival process."
  (interactive "sText: ")
  (festival-send "(SayText \"%s\")\n" text))

;;;###autoload
(defun festival-read-file (file)
  "Get festival to read the contents of FILE."
  (interactive "fFile: ")
  (festival-send "(tts_file \"%s\")\n" (expand-file-name file)))

(defun festival-read-region-in-buffer (buffer start end)
  "Read region START to END from BUFFER."
  (when (festivalp)
    (let ((temp-file (make-temp-name "/tmp/emacs-festival-")))
      (with-current-buffer buffer
        (write-region start end temp-file nil 0)
        (festival-send "(progn (tts_file \"%s\") (delete-file \"%s\"))\n"
                       temp-file temp-file)))))

;;;###autoload
(defun festival-read-buffer (buffer)
  "Read the contents of BUFFER."
  (interactive "bBuffer: ")
  (with-current-buffer (get-buffer buffer)
    (festival-read-region-in-buffer (current-buffer) (point-min) (point-max))))

;;;###autoload
(defun festival-read-region (start end)
  "Read a region of text from the `current-buffer'."
  (interactive "r")
  (festival-read-region-in-buffer (current-buffer) start end))

;;;###autoload
(defun festival-intro ()
  "Fire off the festival intro."
  (interactive)
  (festival-send "(intro)\n"))

;; Functions for selecting various voices.

;;;###autoload
(defun festival-voice-english-fair ()
  "Choose an male English voice."
  (interactive)
  (festival-send "(voice.select 'us1_mbrola)\n"))

(defun festival-voice-english-male ()
  "Choose an male English voice."
  (interactive)
  (festival-send "(voice.select 'us1_mbrola)\n"))

;;;###autoload
(defun festival-voice-US-male ()
  "Choose a male US voice."
  (interactive)
  (festival-send "(voice.select 'ked_diphone)\n"))

;;;###autoload
(defun festival-voice (voice-name)
  "Interactively set the voice."
  (interactive (list (completing-read "Voice: " festival-voices-alist nil t)))
  (funcall (cdr (assoc voice-name festival-voices-alist))))

;; Functions for hooking into other parts of emacs and making them talk.

;;;###autoload
(defun festival-hook-doctor ()
  "Hook `doctor' so that the doctor talks via festival."
  (interactive)
  (defadvice doctor-txtype (before festival-doctor-txtype activate)
    (festival-say ans)))

(defun festival-unhook-doctor ()
  "Undo the hook set by `festival-hook-doctor'."
  (interactive)
  (ad-remove-advice 'doctor-txtype 'before 'festival-doctor-txtype)
  (ad-update 'doctor-txtype))

;;;###autoload
(defun festival-hook-message ()
  "Hook `message' so that all passed text is spoken."
  (interactive)
  (defadvice message (before festival-message (&rest ad-subr-args) activate)
    (festival-say (apply #'format ad-subr-args))))

(defun festival-unhook-message ()
  "Undo the hook set by `festival-hook-message'."
  (interactive)
  (ad-remove-advice 'message 'before 'festival-message)
  (ad-update 'message))

;;;###autoload
(defun festival-hook-error ()
  "Hook `error' so that all passed text is spoken."
  (interactive)
  (defadvice error (before festival-error (&rest ad-subr-args) activate)
    (festival-say (apply #'format ad-subr-args))))

(defun festival-unhook-error ()
  "Undo the hook set by `festival-hook-error'."
  (interactive)
  (ad-remove-advice 'error 'before 'festival-error)
  (ad-update 'error))

;;;###autoload
(defun festival-describe-function (f)
  "Talking version of  `describe-function'."
  (interactive "aDescribe function: ")
  (with-temp-buffer
    (insert (documentation f))
    (festival-read-buffer (current-buffer))))

;; And some other silly things.

;;;###autoload
(defun festival-spook ()
  "Feed those hidden microphones."
  (interactive)
  (with-temp-buffer
    (spook)
    (festival-read-buffer (current-buffer))))

;;;###autoload
(defun festival-yow ()
  "Yow! It speaks!"
  (interactive)
  (festival-say (yow)))

(provide 'festival)

;;; festival.el ends here
