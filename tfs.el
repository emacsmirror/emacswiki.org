;;; tfs.el --- MS Team Foundation Server commands for Emacs.

;; Author     : Dino Chiesa <dpchiesa@hotmail.com>
;; Version    : 0.2.6
;; X-URL      : http://cheeso.members.winisp.net/srcview.aspx?dir=emacs&file=tfs.el
;; Last saved : <2011-May-02 15:32:49>
;;
;; Copyright 2009-2010 Dino Chiesa

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

;;; Commentary:
;;
;; Basic steps to setup:
;;   1. Place `tfs.el' in your `load-path'.
;;   2. In your .emacs file:
;;        (require 'tfs)
;;        (setq tfs/tf-exe  "c:\\vs2008\\common7\\ide\\tf.exe")
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


(defvar tfs/tf-exe  "c:\\Program Files\\Microsoft Visual Studio 9.0\\common7\\ide\\tf.exe"
  "location of the tf.exe command.  Defaults to \"c:\\Program Files\\Microsoft Visual Studio 9.0\\common7\\ide\\tf.exe\"")

(defvar tfs/tfpt-exe  "c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe"
  "location of the tfpt.exe command.  Defaults to \"c:\\Program Files\\Microsoft Team Foundation Server 2008 Power Tools\\TFPT.exe\"")

(defvar tfs/login "/login:domain\\user,password"
  "/login option for all TFS activity.")

(defvar tfs/buffer-name "*TFS Messages*"
  "name of buffer for TFS Messages")



;; -------------------------------------------------------
;; tfs/checkout
;; performs a TFS checkout on the file being visited by the current buffer.
(defun tfs/checkout ()
  "Performs a tf checkout (edit) on the file being visited by the current buffer.  Checkout happens only if the file is non-writable now. In other words checkout will fail if the local file is currently writable."
  (interactive)
  (if buffer-file-name
      (if (not (file-writable-p buffer-file-name))
          (let* ((exitcode nil)
                 (shortname (file-name-nondirectory buffer-file-name))
                 (command (list tfs/tf-exe "checkout" shortname)))
            (tfs/append-to-message-buffer (concat "checkout " shortname ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  tfs/buffer-name
                                  nil
                                  (append (cdr command) (list tfs/login))))
            (if (equal exitcode 0)
                (let ((is-flymake-enabled
                       (and (fboundp 'flymake-mode)
                            flymake-mode)))
                  ;; disable
                  (if is-flymake-enabled
                      (flymake-mode-off))

                ;; get the checked-out version - read from the disk file
                (revert-buffer t t)

                  (if is-flymake-enabled
                      (flymake-mode-on)))

              (error "Checkout of %s was unsuccessful (%S)" buffer-file-name exitcode))))
    (error "tfs/checkout: No file")))



;; -------------------------------------------------------
;; tfs/checkin
;; performs a TFS checkin on the file being visited by the current buffer.
(defun tfs/checkin ()
  "perform a tf checkin on the file being visited by the current buffer.  Checkin happens only if the file is writable now.  This function allows you to specify a checkin comment.  It checks in only the current file being visited - pending changes for any other files will not be checked in."
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
                                  (append (cdr command) (list tfs/login))))
            (if (equal exitcode 0)
                ;; revert to the (now) readonly version
                (revert-buffer t t)
              (error "Checkin of %s was unsuccessful (%S)" buffer-file-name exitcode)))

        (error "Cannot checkin %s : the file is not writable" buffer-file-name))
    (error "tfs/checkin: No file")))



;; -------------------------------------------------------
;; tfs/rename
;; performs a TFS rename on the file being visited by the current buffer.
(defun tfs/rename ()
  "perform a tf rename on the file being visited by the current buffer.  If successful, it also renames the buffer to the new name.
"
  (interactive)
  (if buffer-file-name
      (let* (
             (exitcode nil)
             (shortname (file-name-nondirectory buffer-file-name))
             (newname (read-string (format "New name for %s: " shortname) nil nil nil))
             (command (list tfs/tf-exe "rename" shortname newname)))
        (tfs/append-to-message-buffer (concat "rename " shortname " " newname ": "
                                                  (prin1-to-string command) "\n"))
        (setq exitcode (apply 'call-process
                              (car command)
                              nil
                              tfs/buffer-name
                              nil
                              (append (cdr command) (list tfs/login))))
        (if (equal exitcode 0)
            (set-visited-file-name newname)
          (error "Rename of %s was unsuccessful (%S)" buffer-file-name exitcode)))

    (error "tfs/rename: No file")))



;; -------------------------------------------------------
;; tfs/add
;; performs a TFS add on a file
(defun tfs/add ()
  "perform a tf add on the file being visited by the current buffer."
  (interactive)
  (if buffer-file-name
      (let* ((shortname (file-name-nondirectory buffer-file-name))
             (command (list tfs/tf-exe "add" shortname))
             (exitcode nil))

        (tfs/append-to-message-buffer (concat "add " shortname ": "
                                                  (prin1-to-string command) "\n"))
        (setq exitcode (apply 'call-process
                              (car command)
                              nil
                              tfs/buffer-name
                              nil
                              (append (cdr command) (list tfs/login))))
        (if (equal exitcode 0)
            ;; TODO: make this conditional on a verbose setting
            ;; After using this package for a while, the Add is sort of
            ;; opaque. Hard to know when it's done.  It's nice to get
            ;; a confirmation message. The warm and fuzzy factor.
            (message (format "Successful add of %s" buffer-file-name))
          (error "Add of %s was unsuccessful (%S)" buffer-file-name exitcode)))

    (error "tfs/add: No file")))




;; -------------------------------------------------------
;; tfs/delete
;; performs a TFS delete on a file.
(defun tfs/delete ()
  "perform a tf delete on the file being visited by the current buffer. Kills the buffer if the delete is successful."
  (interactive)
  (if buffer-file-name
      (let ((command)
            (exitcode nil)
            (shortname (file-name-nondirectory buffer-file-name)))

        (if (y-or-n-p (concat "Really delete " shortname  "? "))
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
                                  (append (cdr command) (list tfs/login))))
              (if (equal exitcode 0)
                  (kill-buffer)
                (error "Delete of %s was unsuccessful (%S)" buffer-file-name exitcode)))))
    (error "tfs/delete: No file")))




;; -------------------------------------------------------
;; tfs/get
;; performs a TFS get: retrieve a readonly copy of the specified file.
;;
(defun tfs/get ()
  "perform a tf get on the specified file. Happens only when the file is not writable. "
  (interactive)
  (if buffer-file-name
      (let ((command (list tfs/tf-exe "get" buffer-file-name))
            (exitcode nil)
            (shortname (file-name-nondirectory buffer-file-name)))
        (if (not (file-writable-p buffer-file-name))
            (progn
              ;;(tfs/prep-message-buffer)
              (tfs/append-to-message-buffer (concat "get " shortname ": "
                                                  (prin1-to-string command) "\n"))
              (setq exitcode (apply 'call-process
                                    (car command)
                                    nil
                                    tfs/buffer-name
                                    nil
                                    (append (cdr command) (list tfs/login))))
              (if (equal exitcode 0)
                  ;; get the latest version
                  (revert-buffer t t)
                (error "Get of %s was unsuccessful (%S)" buffer-file-name exitcode)))

          (error "Will not get %s : the file is writable." shortname)))
    (error "tfs/get: No file")))


;; -------------------------------------------------------
;; tfs/undo
;; performs a TFS undo: discards pending changes for the specified file. Happens only when writable.
(defun tfs/undo ()
  "perform a tf undo on the specified file. Happens only when the file is writable. Confirms before discarding edits."
  (interactive)
  (if buffer-file-name
      (let ((command (list tfs/tf-exe "undo" buffer-file-name))
            (exitcode nil)
            (shortname (file-name-nondirectory buffer-file-name)))
        (if (file-writable-p buffer-file-name)
            (if (y-or-n-p (concat "Discard current changes for " shortname  "? "))
                (progn
                  (tfs/append-to-message-buffer (concat "undo " shortname ": "
                                                  (prin1-to-string command) "\n"))
                  (setq exitcode (apply 'call-process
                                        (car command)
                                        nil
                                        tfs/buffer-name
                                        nil
                                        (append (cdr command) (list tfs/login))))
                  (if (equal exitcode 0)
                      ;; get the checked-out (reverted) version
                      (revert-buffer t t)
                    (error "undo on %s was unsuccessful (%S)"
                           buffer-file-name exitcode))))
          (error "cannot undo %s : the file is not writable" shortname)))
    (error "tfs/undo: No file")))



;; -------------------------------------------------------
;; tfs/history
;; performs a TFS history: retrieve and display the TFS history of specified file
(defun tfs/history ()
  "perform a tf history on the specified file."
  (interactive)
  (if buffer-file-name
      (let* ((command (list tfs/tf-exe "history" "/format:detailed"
                            buffer-file-name))
             (exitcode nil)
             (history-bufname (concat "*TFS-history* " buffer-file-name))
             (shortname (file-name-nondirectory buffer-file-name))
             (buffer (get-buffer-create history-bufname)))
        (save-excursion (set-buffer buffer) (erase-buffer))
        (tfs/append-to-message-buffer (concat "history " shortname ": "
                                                  (prin1-to-string command) "\n"))
        (setq exitcode (apply 'call-process
                              (car command)
                              nil
                              history-bufname
                              nil
                                  (append (cdr command) (list tfs/login))))
        (if (equal exitcode 0)
            (display-buffer history-bufname t)
          (error "tf history of %s was unsuccessful (%S)" shortname exitcode)))
    (error "tfs/history: No file")))


;; -------------------------------------------------------
;; tfs/properties
;; gets information on the file being visited by the current buffer.
;; displays that information in a new temp buffer.
(defun tfs/properties ()
  "Performs a tf properties: gets TFS properties of the current file. "
  (interactive)
  (tfs/action "properties" nil))




;; -------------------------------------------------------
;; tfs/action
;; gets information on the file being visited by the current buffer.
;; diff, properties, etc
;; displays that information in a new temp buffer.
(defun tfs/action (verb retcode)
  "Performs a tf \"action\": gets a tf query for the current file. "
  (interactive)
  (if buffer-file-name
      (let* ((command (list tfs/tf-exe verb buffer-file-name))
             (exitcode nil)
             (info-bufname (concat "*TFS-" verb "* " buffer-file-name))
             (buffer (get-buffer-create info-bufname))
             (shortname (file-name-nondirectory buffer-file-name)))
        (save-excursion (set-buffer buffer) (erase-buffer))
        (tfs/append-to-message-buffer (concat verb  shortname ": "
                                                  (prin1-to-string command) "\n"))
        (setq exitcode (apply 'call-process
                              (car command)
                              nil
                              info-bufname
                              nil
                              (append (cdr command) (list tfs/login))))

        (if (or (equal exitcode 0) (not (numberp retcode)) (equal exitcode retcode))
            (display-buffer info-bufname t)
          (error (concat "Get TFS " verb " for %s was unsuccessful (%S)")
                 buffer-file-name exitcode)))
    (error "tfs/%s: No file" verb)))



;; -------------------------------------------------------
;; tfs/annotate
(defun tfs/annotate ()
  "Gets line-by-line annotation for the file being visited by the current buffer. Displays that information in the annotation viewer. This requires the TFPT.exe tool.  See 'tfs/tfpt-exe'."
  (interactive)
  (if (file-exists-p tfs/tfpt-exe)
      (if buffer-file-name
          (let* ((exitcode nil)
                 (shortname (file-name-nondirectory buffer-file-name))
                 (command (list tfs/tfpt-exe "annotate" "/noprompt"
                                shortname))
                 (annotation-bufname (concat "*TFS annotation* " shortname))
                 (buffer (get-buffer-create annotation-bufname)))
            (save-excursion (set-buffer buffer) (erase-buffer))
            (message "computing...")
            ;;(message (apply 'concat command))
            (tfs/append-to-message-buffer (concat "annotate " shortname ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  annotation-bufname
                                  nil
                                  (append (cdr command) (list tfs/login))))

            (if (equal exitcode 0)
                (progn
                  (display-buffer annotation-bufname t)
                  (beginning-of-buffer-other-window 0))

              (error "Get TFS properties for %s was unsuccessful (%S)"
                     buffer-file-name exitcode)))
        (error "tfs/annotate: No file"))
    (error "%s does not exist. (have you set tfs/tfpt-exe?)"  tfs/tfpt-exe)))


;; -------------------------------------------------------
;; tfs/thinginfo
(defun tfs/thinginfo (exe thing)
  "Gets info on a workitem or changeset. This requires the TFPT.exe tool.  See 'tfs/tfpt-exe'."
  (if (file-exists-p exe)
          (let* ((exitcode nil)
                 (guess (thing-at-point 'word))
                 (item-number (read-string (concat thing ": ")  guess nil nil))
                 (command (list exe thing item-number))
                 (bufname (concat "*TFS " thing "* " item-number))
                 (buffer (get-buffer-create bufname)))
            (save-excursion (set-buffer buffer) (erase-buffer))
            ;;(message (apply 'concat command))
            (tfs/append-to-message-buffer (concat thing " " item-number ": "
                                                  (prin1-to-string command) "\n"))
            (setq exitcode (apply 'call-process
                                  (car command)
                                  nil
                                  bufname
                                  nil
                                  (append (cdr command) (list tfs/login))))

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
  "Gets info on a workitem. This requires the TFPT.exe tool.  See 'tfs/tfpt-exe'."
  (interactive)
  (tfs/thinginfo  tfs/tfpt-exe "workitem"))

;; -------------------------------------------------------
;; tfs/changeset
(defun tfs/changeset ()
  "Gets info on a changeset. This requires the TFPT.exe tool.  See 'tfs/tfpt-exe'."
  (interactive)
  (tfs/thinginfo tfs/tf-exe "changeset"))


;; -------------------------------------------------------
;; tfs/diff
;; diff on the file being visited by the current buffer.
(defun tfs/diff()
  "Performs a tf diff on the current file. "
  (interactive)
  (tfs/action "diff" 100))



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
    (save-excursion (set-buffer buffer) (erase-buffer))
    (tfs/append-to-message-buffer (concat "status" ": "
                                                  (prin1-to-string command) "\n"))
    (setq exitcode (apply 'call-process
                          (car command)
                          nil
                          status-bufname
                          nil
                                  (append (cdr command) (list tfs/login))))

    (if (equal exitcode 0)
        (display-buffer status-bufname t)
      (error "Get TFS status was unsuccessful (%S)" exitcode))))


(defun tfs/prep-message-buffer ()
  "scrolls the TFS Messages buffer to the end. Intended to be used by the tfs.el module internally, before appending content to the messages buffer."

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

