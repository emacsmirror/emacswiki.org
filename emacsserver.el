;;; emacsserver.el is free software
;; -*-mode:Emacs-Lisp;tab-width:4;indent-tabs-mode:nil-*-
;; Time-stamp: <2005-03-04 19:20:44 dhu2kor>
;;-----------------------------------------------------------------------------
;; File   : emacsserver.el
;; Auth   : Dhruva Krishnamurthy (dhruva.krishnamurthy@gmail.com)
;; Status : Development (flaky)
;; Usage  :
;; o As server:
;;   (require 'emacsserver)
;;   (emacsserver-start "magic")
;; o As client:
;;   emacs --batch --load emacsserver.el
;;         --eval "(emacsclient-command '(find-file \"~/_emacs\") \"magic\")"
;;
;; TODO   :
;; o Does not work on XEmacs ('make-network-process not available)
;; o Code cleanup, optimize, document and misc stuff
;;-----------------------------------------------------------------------------
;; This is not available on XEmacs and Emacs prior to 21.4
(if (not (featurep 'make-network-process))
    (error "Incompatible version of [X]Emacs"))

(defvar emacsclient-hash
  (make-hash-table :test 'eq)
  "emacsserver: Internal client connection info")

(defvar emacsserver-hash
  (make-hash-table)
  "emacsserver: Internal server details")

;;-----------------------------------------------------------------------------
;;                        GNU Emacs server code
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; emacsserver-start
;;-----------------------------------------------------------------------------
(defun emacsserver-start (&optional magic port)
  "emacsserver: Starts a server on specified port and binds to localhost"
  (interactive)
  (catch 'ret
    (let ((server-port (if (integerp port)
               port
             55555))
      (key (if magic
           magic
         "houdini")))

      ;; Prevent running another server on same port
      ;; in the current emacs session
      (if (gethash server-port emacsserver-hash)
      (throw 'ret nil))

      ;; Store a hash of port->(magic,server proc) for client auth
      (puthash server-port (cons key (make-network-process
                      :name "emacsserver"
                      :buffer nil
                      :type nil
                      :server t
                      :service server-port
                      :local (vector 127 0 0 1 server-port)
                      :noquery t
                      :filter 'emacsserver-filter
                      :sentinel 'emacsserver-sentinel
                      :keepalive t))
           emacsserver-hash))
    (throw 'ret t)))

;;-----------------------------------------------------------------------------
;; emacsserver-filter
;;   Do the actual auth'ing
;;   Message format: (magic (expr to be evaluated))
;;-----------------------------------------------------------------------------
(defun emacsserver-filter (proc mesg)
  "emacsserver: Server side message processing with auth"
  (catch 'ret
    (let ((cwd default-directory)
      (auth (gethash proc emacsclient-hash))
      (serv (gethash (aref (process-contact proc ':local) 4)
             emacsserver-hash)))
      (if (not (listp serv))
      (throw 'ret nil))

      ;; Try auth'ing till the connection is auth'ed
      (if (not auth)
      (if (string= (car serv) (caar (read-from-string mesg)))
          (progn
        (puthash proc t emacsclient-hash)
        (setq auth t))))

      (if (not auth)
      (throw 'ret nil))

      ;; Eval the code to be executed
      (eval (car (cdar (read-from-string mesg))))
      ;; Change back from client's default-dir to server's default-dir
      (cd cwd))
    (throw 'ret t)))

;;-----------------------------------------------------------------------------
;; emacsserver-sentinel
;;-----------------------------------------------------------------------------
(defun emacsserver-sentinel (proc mesg)
  "emacsserver: Populate emacs client connections in a hash pending auth'ing"
  (emacsclient-refresh)
  (if (eq (process-status proc) 'open)
      (puthash proc nil emacsclient-hash)))

;;-----------------------------------------------------------------------------
;; emacsserver-kill
;;-----------------------------------------------------------------------------
(defun emacsserver-kill ()
  "emacsserver: Kill all emacs client & server instances"
  (interactive)
  (emacsclient-kill)                    ; Clear the clients first
  (maphash '(lambda (key val)
          (delete-process (cdr val))) emacsserver-hash)
  (clrhash emacsserver-hash)
  (if (interactive-p)
      (message "Emacs client & server processes cleared")))

;;-----------------------------------------------------------------------------
;; emacsserver-enum
;;-----------------------------------------------------------------------------
(defun emacsserver-enum ()
  "emacsserver: Enumerate server instances"
  (interactive)
  (maphash '(lambda (key val)
          (princ (format "Server process:%s,Auth:%s" (cdr val) (car val))))
       emacsserver-hash))

;;-----------------------------------------------------------------------------
;; emacsclient-refresh
;;-----------------------------------------------------------------------------
(defun emacsclient-refresh ()
  "emacsserver: Refreshes client instance hash by clearing dead connections"
  (interactive)
  (maphash '(lambda (key val)
          (if (/= (process-exit-status key) 0)
          (remhash key emacsclient-hash)))
       emacsclient-hash)
  (if (interactive-p)
      (message "Emacs client processes refreshed")))

;;-----------------------------------------------------------------------------
;; emacsclient-enum
;;-----------------------------------------------------------------------------
(defun emacsclient-enum ()
  "emacsserver: Enumerate client instances"
  (interactive)
  (emacsclient-refresh)
  (maphash '(lambda (key val)
          (princ (format "Client process:%s, Auth:%s" key val)))
       emacsclient-hash))

;;-----------------------------------------------------------------------------
;; emacsclient-kill
;;-----------------------------------------------------------------------------
(defun emacsclient-kill ()
  "emacsserver: Kill all emacs client instances"
  (interactive)
  (maphash '(lambda (key val)
          (delete-process key)) emacsclient-hash)
  (clrhash emacsclient-hash)
  (if (interactive-p)
      (message "Emacs client processes cleared")))

;;-----------------------------------------------------------------------------
;; emacsclient-command
;;-----------------------------------------------------------------------------
(defun emacsclient-command (expr &optional magic port)
  "emacsserver: Dispatches a expression to emacs server. The message follows
the format (magic (cd client-dir) (progn (expr)))"
  (catch 'ret
    (let ((emacsclient (make-network-process
            :name "emacsclient"
            :buffer nil
            :type nil
            :host "127.0.0.1"
            :service 55555
            :noquery t
            :keepalive t))
      (key (if magic
           magic
         "houdini")))
      (if emacsclient
      (progn
        (process-send-string
         emacsclient (concat
              "(" key
              "(progn (cd " (prin1-to-string default-directory) ")"
              (prin1-to-string expr) "))"))
        (throw 'ret t))
    (throw 'ret nil)))
    (throw 'ret nil))
  t)

;;-----------------------------------------------------------------------------
(provide 'emacsserver)
;;-----------------------------------------------------------------------------
;;; emacsserver.el ends here
