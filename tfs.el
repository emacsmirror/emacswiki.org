;;; tfs.el --- MS Team Foundation Server commands for Emacs.

;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Version    : 2012.5.7
;; X-URL      : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=tfs.el
;; URL        : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=tfs.el
;; License    : MS-PL
;; Last saved : <2012-May-07 09:09:50>
;;
;; Copyright 2009-2012 Dino Chiesa

;;; Commentary:
;;
;; Basic steps to setup:
;;   1. Place `tfs.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'tfs)
;;        (setq tfs/tf-exe  "c:\\vs2010\\common7\\ide\\tf.exe")
;;        (setq tfs/login "/login:domain\\userid,password")
;;              -or-
;;        (setq tfs/login (getenv "TFSLOGIN"))
;;   3. also in your .emacs file:
;;        set local or global key bindings for tfs commands.  like so:
;;
;;        (global-set-key  "\C-xvo" 'tfs/checkout)
;;        (global-set-key  "\C-xvi" 'tfs/checkin)
;;        (global-set-key  "\C-xvp" 'tfs/properties)
;;        (global-set-key  "\C-xvr" 'tfs/rename)
;;        (global-set-key  "\C-xvg" 'tfs/get)
;;        (global-set-key  "\C-xvh" 'tfs/history)
;;        (global-set-key  "\C-xvu" 'tfs/undo)
;;        (global-set-key  "\C-xvd" 'tfs/diff)
;;        (global-set-key  "\C-xv-" 'tfs/delete)
;;        (global-set-key  "\C-xv+" 'tfs/add)
;;        (global-set-key  "\C-xvs" 'tfs/status)
;;        (global-set-key  "\C-xva" 'tfs/annotate)
;;        (global-set-key  "\C-xvw" 'tfs/workitem)
;;
;;

;;; License:
;;
;; Microsoft Public License (Ms-PL)
;;
;; This license governs use of the accompanying software, the tfs.el
;; library ("the software"). If you use the software, you accept this
;; license. If you do not accept the license, do not use the software.
;;
;; 1. Definitions
;;
;; The terms "reproduce," "reproduction," "derivative works," and
;; "distribution" have the same meaning here as under U.S. copyright
;; law.
;;
;; A "contribution" is the original software, or any additions or
;; changes to the software.
;;
;; A "contributor" is any person that distributes its contribution under
;; this license.
;;
;; "Licensed patents" are a contributor's patent claims that read
;; directly on its contribution.
;;
;; 2. Grant of Rights
;;
;; (A) Copyright Grant- Subject to the terms of this license, including
;; the license conditions and limitations in section 3, each contributor
;; grants you a non-exclusive, worldwide, royalty-free copyright license
;; to reproduce its contribution, prepare derivative works of its
;; contribution, and distribute its contribution or any derivative works
;; that you create.
;;
;; (B) Patent Grant- Subject to the terms of this license, including the
;; license conditions and limitations in section 3, each contributor
;; grants you a non-exclusive, worldwide, royalty-free license under its
;; licensed patents to make, have made, use, sell, offer for sale,
;; import, and/or otherwise dispose of its contribution in the software
;; or derivative works of the contribution in the software.
;;
;; 3. Conditions and Limitations
;;
;; (A) No Trademark License- This license does not grant you rights to
;; use any contributors' name, logo, or trademarks.
;;
;; (B) If you bring a patent claim against any contributor over patents
;; that you claim are infringed by the software, your patent license
;; from such contributor to the software ends automatically.
;;
;; (C) If you distribute any portion of the software, you must retain
;; all copyright, patent, trademark, and attribution notices that are
;; present in the software.
;;
;; (D) If you distribute any portion of the software in source code
;; form, you may do so only under this license by including a complete
;; copy of this license with your distribution. If you distribute any
;; portion of the software in compiled or object code form, you may only
;; do so under a license that complies with this license.
;;
;; (E) The software is licensed "as-is." You bear the risk of using
;; it. The contributors give no express warranties, guarantees or
;; conditions. You may have additional consumer rights under your local
;; laws which this license cannot change. To the extent permitted under
;; your local laws, the contributors exclude the implied warranties of
;; merchantability, fitness for a particular purpose and
;; non-infringement.


(defcustom tfs/tf-exe  "c:\\Program Files\\Microsoft Visual Studio 9.0\\common7\\ide\\tf.exe"
  "location of the tf.exe command.  Defaults to \"c:\\Program Files\\Microsoft Visual Studio 9.0\\common7\\ide\\tf.exe\""
  :group 'tfs)

(defcustom tfs/tfpt-exe  "c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe"
  "location of the tfpt.exe command.  Defaults to \"c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe\""
  :group 'tfs)


(defcustom tfs/login "/login:domain\\user,password"
  "/login option for all TFS activity."
  :group 'tfs)

(defcustom tfs/buffer-name "*TFS Messages*"
  "name of buffer for TFS Messages"
  :group 'tfs)


;; make user/password optional
(defun tfs/maybe-add-login ()
  "If login details aren't supplied, don't try using them"
 (if (equal tfs/login "/login:domain\\user,password")
     () ; return the empty list
   (list tfs/login)))


(defvar tfs-add-hook nil
  "Hook that is invoked after `tfs/add'.

Set this with
  (add-hook 'tfs-add-hook 'my-fn-to-call-after-tfs-add) .

Used by tfs-mode.el")


(defun tfs/revert-buffer-safely ()
  "Revert buffer respecting flymake's aggressiveness."
  (let ((is-flymake-enabled
         (and (fboundp 'flymake-mode) ;; function
              (boundp 'flymake-mode) ;; variable
              flymake-mode)))
    ;; disable
    (if is-flymake-enabled
        (flymake-mode-off))

    ;; get the checked-out version - read from the disk file
    (revert-buffer t t)

    (if is-flymake-enabled
        (flymake-mode-on))))


(defun tfs/determine-file (filename prompt)
  "determine the name of the file to use in a TF command."
  (cond
   ((stringp filename)
    filename)
   ((eq major-mode 'dired-mode)
    (dired-get-filename))
   (buffer-file-name
    buffer-file-name)
   (t
    (expand-file-name (read-file-name "File to checkout: ")))))



;; -------------------------------------------------------
;; tfs/checkout
;; performs a TFS checkout on a file.
(defun tfs/checkout (&optional filename)
  "Performs a tf checkout (edit).

The file to checkout is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to checkout.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to checkout.

Checkout will occur only if the file is non-writable before the
call; checkout will fail if the specified file is currently
writable."
  (interactive)
  (let ((file-to-checkout (tfs/determine-file filename "File to checkout: ")))

    (if file-to-checkout
        (if (not (file-writable-p file-to-checkout))
            (let* ((exitcode nil)
                   (shortname (file-name-nondirectory file-to-checkout))
                   (command (list tfs/tf-exe "checkout" shortname)))
              (tfs/append-to-message-buffer (concat "checkout " shortname ": "
                                                    (prin1-to-string command) "\n"))
              (setq exitcode (apply 'call-process
                                    (car command)
                                    nil
                                    tfs/buffer-name
                                    nil
                                    (append (cdr command) (tfs/maybe-add-login))))
              (if (equal exitcode 0)
                  (if (string= file-to-checkout buffer-file-name)
                      (tfs/revert-buffer-safely))
                (error "Checkout of %s was unsuccessful (%S)" file-to-checkout exitcode))))
      (error "tfs/checkout: No file"))))



;; -------------------------------------------------------
;; tfs/checkin
;; performs a TFS checkin on the file being visited by the current buffer.
(defun tfs/checkin ()
  "perform a tf checkin on the file being visited by the current
buffer.  Checkin happens only if the file is writable now.  This
function allows you to specify a checkin comment.  It checks in
only the current file being visited - pending changes for any
other files will not be checked in."
  (interactive)
  (if buffer-file-name
      (if (file-writable-p buffer-file-name)
          (let* ((exitcode nil)
                 (shortname (file-name-nondirectory buffer-file-name))
                 (comment (read-string (format "Comment for %s: " shortname) nil nil nil))
                 (command (list tfs/tf-exe "checkin" (format "/comment:%s" comment)
                                buffer-file-name)))
            (tfs/append-to-message-buffer (concat "checkin " shortname ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  tfs/buffer-name
                                  nil
                                  (append (cdr command) (tfs/maybe-add-login))))
            (if (equal exitcode 0)
                ;; revert to the (now) readonly version
                (revert-buffer t t)
              (error "Checkin of %s was unsuccessful (%S)" buffer-file-name exitcode)))

        (error "Cannot checkin %s : the file is not writable" buffer-file-name))
    (error "tfs/checkin: No file")))



;; -------------------------------------------------------
;; tfs/rename
;; performs a TFS rename .
(defun tfs/rename (&optional filename new-name)
  "perform a tf rename on a file.

The file to rename is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

The file is renamed to NEW-NAME, a string, which should be the
name of a non-existent file in the current directory, specified
without a qualifying path.

If the rename is successful, and if the buffer is visiting the
file that is being renamed, then this function also renames the
buffer to the new name.
"
  (interactive)
  (let ((file-to-rename (tfs/determine-file filename "File to rename: ")))
    (if file-to-rename
        (let* ((exitcode nil)
               (shortname (file-name-nondirectory file-to-rename))
               (newname (or new-name (read-string (format "New name for %s: " shortname) nil nil nil)))
               (command (list tfs/tf-exe "rename" shortname newname)))
          (tfs/append-to-message-buffer (concat "rename " shortname " " newname ": "
                                                (prin1-to-string command) "\n"))
          (setq exitcode (apply 'call-process
                                (car command)
                                nil
                                tfs/buffer-name
                                nil
                                (append (cdr command) (tfs/maybe-add-login))))
          (if (equal exitcode 0)
              (if (string= file-to-rename buffer-file-name)
                  (set-visited-file-name newname))
            (error "Rename of %s was unsuccessful (%S)" file-to-rename exitcode)))

      (error "tfs/rename: No file"))))



;; -------------------------------------------------------
;; tfs/add
;; performs a TFS add on a file
(defun tfs/add (&optional filename)
  "perform a tf add on a file.

The file to add is deteremined this way:

 - if FILENAME is specified, then this function selects that file
   to add.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file to add.

"
  (interactive)
  (let ((file-to-add (tfs/determine-file filename "File to add: ")))
  (if file-to-add
      (let* ((shortname (file-name-nondirectory file-to-add))
             (command (list tfs/tf-exe "add" shortname "/noprompt"))
             (exitcode nil))
        (tfs/append-to-message-buffer (concat "add " shortname ": "
                                                  (prin1-to-string command) "\n"))
        (setq exitcode (apply 'call-process
                              (car command)
                              nil
                              tfs/buffer-name
                              nil
                              (append (cdr command) (tfs/maybe-add-login))))
        (if (equal exitcode 0)
            (progn
              ;; TODO: make this conditional on a verbose setting
              ;; After using this package for a while, the Add is sort of
              ;; opaque. Hard to know when it's done.  It's nice to get
              ;; a confirmation message. The warm and fuzzy factor.
              (message (format "Successful add of %s" file-to-add))
              (run-hooks 'tfs-add-hooks))
          (error "Add of %s was unsuccessful (%S)" file-to-add exitcode)))

    (error "tfs/add: No file"))))




;; -------------------------------------------------------
;; tfs/delete
;; performs a TFS delete on a file.
(defun tfs/delete (&optional filename no-confirm)
  "perform a tf delete on a file.

The file to delete is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

The function prompts for confirmation before proceeding,
unless NO-CONFIRM is non-nil.

If the delete is successful, and if the buffer is visiting the file that
is being deleted, then this function also kills the buffer.

"
  (interactive)
  (let ((file-to-delete (tfs/determine-file filename "File to delete: ")))
    (if file-to-delete
        (let ((command)
              (exitcode nil)
              (shortname (file-name-nondirectory file-to-delete)))

          (if (or (not no-confirm)
                  (y-or-n-p (concat "Really delete " shortname  "? ")))
              (progn
                (setq command (list tfs/tf-exe
                                    "delete"
                                    shortname))
                (tfs/append-to-message-buffer (concat "delete " shortname ": "
                                                      (prin1-to-string command) "\n"))
                (setq exitcode (apply 'call-process
                                      (car command)
                                      nil
                                      tfs/buffer-name
                                      nil
                                      (append (cdr command) (tfs/maybe-add-login))))
                (if (equal exitcode 0)
                    (if (string= file-to-delete buffer-file-name)
                        (kill-buffer))
                  (error "Delete of %s was unsuccessful (%S)" file-to-delete exitcode)))))
      (error "tfs/delete: No file"))))



;; -------------------------------------------------------
;; tfs/get
;; performs a TFS get: retrieves a readonly copy of a file from TFS.
;;
(defun tfs/get (&optional filename)
  "perform a tf get on a file. Happens only when the
file is not writable.

The file to get is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

"
  (interactive)
  (let ((file-to-get (tfs/determine-file filename "File to get: ")))
    (if file-to-get
        (let ((command (list tfs/tf-exe "get" file-to-get))
              (exitcode nil)
              (shortname (file-name-nondirectory file-to-get)))
          (if (not (file-writable-p file-to-get))
              (progn
                (tfs/append-to-message-buffer (concat "get " shortname ": "
                                                      (prin1-to-string command) "\n"))
                (setq exitcode (apply 'call-process
                                      (car command)
                                      nil
                                      tfs/buffer-name
                                      nil
                                      (append (cdr command) (tfs/maybe-add-login))))
                (if (equal exitcode 0)
                    (if (string= file-to-get buffer-file-name)
                        (revert-buffer t t)) ;; get the latest version
                  (error "Get of %s was unsuccessful (%S)" file-to-get exitcode)))
            (error "Will not get %s : the file is writable." shortname)))
      (error "tfs/get: No file"))))


;; -------------------------------------------------------
;; tfs/undo
;; performs a TFS undo: discards pending changes for a file.
(defun tfs/undo (&optional filename no-confirm)
  "perform a tf undo on a file.

The file to undo is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

The undo happens only when the file is writable. Confirms before
discarding edits.
"
  (interactive)
  (let ((file-to-undo (tfs/determine-file filename "File to undo: ")))
    (if file-to-undo
        (let
            ((command (list tfs/tf-exe "undo" file-to-undo "/noprompt"))
             (exitcode nil)
             (shortname (file-name-nondirectory file-to-undo)))
          (if (file-writable-p file-to-undo)
              (if (or (not no-confirm)
                      (y-or-n-p (concat "Discard current changes for " shortname  "? ")))
                  (progn
                    (tfs/append-to-message-buffer (concat "undo " shortname ": "
                                                          (prin1-to-string command) "\n"))
                    (setq exitcode (apply 'call-process
                                          (car command)
                                          nil
                                          tfs/buffer-name
                                          nil
                                          (append (cdr command) (tfs/maybe-add-login))))
                    (if (equal exitcode 0)
                        (if (string= file-to-undo buffer-file-name)
                            (tfs/revert-buffer-safely)) ;; get the reverted version
                      (error "undo on %s was unsuccessful (%S)"
                             file-to-undo exitcode))))
            (error "cannot undo %s : the file is not writable" shortname)))
      (error "tfs/undo: No file"))))



;; -------------------------------------------------------
;; tfs/history
;; performs a TFS history: retrieve and display the TFS history of a file
(defun tfs/history (&optional filename)
  "perform a tf history on a file."
  (interactive)
  (let ((file-of-interest (tfs/determine-file filename "File: ")))
    (if file-of-interest
        (let* ((command (list tfs/tf-exe "history" "/format:detailed"
                              file-of-interest))
               (exitcode nil)
               (history-bufname (concat "*TFS-history* " file-of-interest))
               (shortname (file-name-nondirectory file-of-interest))
               (buffer (get-buffer-create history-bufname)))

          (with-current-buffer buffer
            (erase-buffer)
            (tfs/append-to-message-buffer (concat "history " shortname ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  history-bufname
                                  nil
                                  (append (cdr command) (tfs/maybe-add-login)))))
          (if (equal exitcode 0)
              (display-buffer history-bufname t)
            (error "tf history of %s was unsuccessful (%S)" shortname exitcode)))
      (error "tfs/history: No file"))))


;; -------------------------------------------------------
;; tfs/action
;; gets information on the file being visited by the current buffer.
;; Currently there are two verbs supported: diff and properties.
;; displays that information in a new temp buffer.
(defun tfs/action (verb retcode &optional filename)
  "Performs a tf \"action\": gets a tf query for a file.

The file is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

"
  (interactive)
  (let ((file-of-interest (tfs/determine-file filename "File: ")))
    (if file-of-interest
        (let* ((command (list tfs/tf-exe verb file-of-interest "/noprompt"))
               (exitcode nil)
               (info-bufname (concat "*TFS-" verb "* " file-of-interest))
               (buffer (get-buffer-create info-bufname))
               (shortname (file-name-nondirectory file-of-interest)))

          (with-current-buffer buffer
            (erase-buffer)
            (tfs/append-to-message-buffer (concat verb  shortname ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  info-bufname
                                  nil
                                  (append (cdr command) (tfs/maybe-add-login)))))

          (if (or (equal exitcode 0) (not (numberp retcode)) (equal exitcode retcode))
              (display-buffer info-bufname t)
            (error (concat "Get TFS " verb " for %s was unsuccessful (%S)")
                   file-of-interest exitcode)))
      (error "tfs/%s: No file" verb))))


;; -------------------------------------------------------
;; tfs/properties
;; gets information on the file being visited by the current buffer.
;; displays that information in a new temp buffer.
(defun tfs/properties (&optional filename)
  "Performs a tf properties: gets TFS properties for a file. "
  (interactive)
  (tfs/action "properties" filename nil))

;; -------------------------------------------------------
;; tfs/diff
;; diff on the file being visited by the current buffer.
(defun tfs/diff (&optional filename)
  "Performs a tf diff on a file. "
  (interactive)
  (tfs/action "diff" filename 100))


;; -------------------------------------------------------
;; tfs/localversions
;; gets version of a file.
;; returns it as a string, or nil if errors.
(defun tfs/localversions (&optional filename)
  "Performs a tf localversions on a file.

The file is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

"

  (interactive)
  (let ((file-of-interest (tfs/determine-file filename "File: ")))
    (if file-of-interest
        (let* ((command (list tfs/tf-exe "localversions" file-of-interest))
               (exitcode nil)
               (info-bufname (concat "*TFS-localversions* " file-of-interest))
               (buffer (get-buffer-create info-bufname))
               (shortname (file-name-nondirectory file-of-interest)))
          (save-excursion (set-buffer buffer) (erase-buffer))
          (tfs/append-to-message-buffer (concat "localversions "  shortname ": "
                                                (prin1-to-string command) "\n"))
          (setq exitcode (apply 'call-process
                                (car command)
                                nil
                                info-bufname
                                nil
                                (append (cdr command) (tfs/maybe-add-login))))
          (cond
           ((equal exitcode 0)
            ;; parse the version out of the info buffer
            (let (start-pos end-pos version)
              (save-excursion
                (set-buffer buffer)
                (goto-line 3)
                (search-forward ";")
                (setq start-pos (point))
                (move-end-of-line nil)
                (setq end-pos (point))
                (setq version (buffer-substring-no-properties start-pos end-pos))
                (kill-buffer buffer)
                version)))

           ((equal exitcode 1) ;; no items match buffer files (not under source control)
            (kill-buffer buffer)
            nil)

           (t
            (error (concat "TF localversions for %s was unsuccessful (%S)")
                   file-of-interest exitcode))))

      (error "tfs/localversions: No file"))))



;; -------------------------------------------------------
;; tfs/annotate
(defun tfs/annotate (&optional filename)
  "Gets line-by-line annotation for a file.

Displays that information in the annotation
viewer. This requires the TFPT.exe tool.  See `tfs/tfpt-exe'.

The file is deteremined this way:

 - if FILENAME is specified, then this function selects that file.

 - When this function is called from a buffer that is in
   `dired-mode', it selects the file on the current line.

 - when there is a file backing the current buffer, it selects
   the file being visited by the current buffer.

 - else, prompt the user for the file.

"
  (interactive)
  (if (file-exists-p tfs/tfpt-exe)
  (let ((file-of-interest (tfs/determine-file filename "File: ")))
      (if file-of-interest
          (let* ((exitcode nil)
                 (shortname (file-name-nondirectory file-of-interest))
                 (command (list tfs/tfpt-exe "annotate" "/noprompt"
                                shortname))
                 (annotation-bufname (concat "*TFS annotation* " shortname))
                 (buffer (get-buffer-create annotation-bufname)))

            (with-current-buffer buffer
              (erase-buffer)
              (message "computing...")
              ;;(message (apply 'concat command))
              (tfs/append-to-message-buffer (concat "annotate " shortname ": "
                                                    (prin1-to-string command) "\n"))
              (setq exitcode (apply 'call-process
                                    (car command)
                                    nil
                                    annotation-bufname
                                    nil
                                    (append (cdr command) (tfs/maybe-add-login)))))

            (if (equal exitcode 0)
                (progn
                  (display-buffer annotation-bufname t)
                  (beginning-of-buffer-other-window 0))

              (error "Get TFS properties for %s was unsuccessful (%S)"
                     file-of-interest exitcode)))
        (error "tfs/annotate: No file"))
    (error "%s does not exist. (have you set tfs/tfpt-exe?)"  tfs/tfpt-exe))))


;; -------------------------------------------------------
;; tfs/thinginfo
(defun tfs/thinginfo (exe thing)
  "Gets info on a workitem or changeset. This requires the TFPT.exe tool.  See `tfs/tfpt-exe'."
  (if (file-exists-p exe)
      (let* ((exitcode nil)
             (guess (thing-at-point 'word))
             (item-number (read-string (concat thing ": ")  guess nil nil))
             (command (list exe thing item-number))
             (bufname (concat "*TFS " thing "* " item-number))
             (buffer (get-buffer-create bufname)))

        (with-current-buffer buffer
          (erase-buffer)
          ;;(message (apply 'concat command))
          (tfs/append-to-message-buffer (concat thing " " item-number ": "
                                                (prin1-to-string command) "\n"))
          (setq exitcode (apply 'call-process
                                (car command)
                                nil
                                bufname
                                nil
                                (append (cdr command) (tfs/maybe-add-login)))))

        (if (equal exitcode 0)
            (progn
              (display-buffer bufname t)
              (beginning-of-buffer-other-window 0))

          (error (concat "Get TFS " thing "%s was unsuccessful (%S)"
                         item-number exitcode))))

    (error "%s does not exist. (have you set tfs/tfpt-exe or tfs/tf-exe?)"  exe)))


;; -------------------------------------------------------
;; tfs/workitem
(defun tfs/workitem ()
  "Gets info on a workitem. This requires the TFPT.exe tool.  See `tfs/tfpt-exe'."
  (interactive)
  (tfs/thinginfo  tfs/tfpt-exe "workitem"))

;; -------------------------------------------------------
;; tfs/changeset
(defun tfs/changeset ()
  "Gets info on a changeset. This requires the TFPT.exe tool.  See `tfs/tfpt-exe'."
  (interactive)
  (tfs/thinginfo tfs/tf-exe "changeset"))



;; -------------------------------------------------------
;; tfs/status
;; tf status.
(defun tfs/status ()
  "Performs a tf status. Displays the result in a buffer."
  (interactive)
  (let* ((command (list tfs/tf-exe "status"))
         (exitcode nil)
         (status-bufname  "*TFS-status*")
         (buffer (get-buffer-create status-bufname)))

    (with-current-buffer buffer
      (erase-buffer)
      (insert (concat (mapconcat 'identity command " ") "\n\n"))
      (tfs/append-to-message-buffer (concat "status" ": "
                                            (prin1-to-string command) "\n"))
      (setq exitcode (apply 'call-process
                            (car command)
                            nil
                            status-bufname
                            nil
                            (append (cdr command) (tfs/maybe-add-login)))))

    (if (equal exitcode 0)
        (display-buffer status-bufname t)
      (error "Get TFS status was unsuccessful (%S)" exitcode))))


(defun tfs/prep-message-buffer ()
  "scrolls the TFS Messages buffer to the end. Intended to be
used by the tfs.el module internally, before appending content to
the messages buffer."
  (let ((buf (current-buffer))
        (tfsbuffer (get-buffer-create tfs/buffer-name)))
    (set-buffer tfsbuffer)
    (goto-char (point-max))
    (set-buffer buf)))


(defun tfs/append-to-message-buffer (text)
  "Append text to the TFS Messages buffer.  Intended for internal use only."
  (let ((buf (current-buffer))
        (tfsbuffer (get-buffer-create tfs/buffer-name)))
    (set-buffer tfsbuffer)
    (goto-char (point-max))
    (insert text)
    (set-buffer buf)))


(provide 'tfs)

;;; tfs.el ends here
