;;; syslog-mode.el --- Mode for viewing system logfiles
;;
;; ~harley/share/emacs/pkg/syslog-mode.el ---
;;
;; $Id: syslog-mode.el,v 1.7 2003/03/17 18:50:12 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/syslog-mode.el
;; License:   GPL v2
;; Keywords:  syslog, emacs

;;; Commentary:
;; * Handy functions for looking at system logs.
;; * Fontifys the date and su messages.

;;; History:
;; 20-Mar-2013    Christian Giménez
;;    Added more keywords for font-lock.
;;  2003-03-16 : Updated URL and contact info.

;;; Code:

;; Setup
(defvar syslog-mode-hook nil
  "*Hook to setup `syslog-mode'.")

(defvar syslog-mode-load-hook nil
  "*Hook to run when `syslog-mode' is loaded.")

;;;###autoload
(defvar syslog-setup-on-load nil
  "*If not nil setup syslog mode on load by running syslog-add-hooks.")

;; I also use "Alt" as C-c is too much to type for cursor motions.
(defvar syslog-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map [C-down] 'syslog-boot-start)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version))
	t)
    map)
  "The local keymap for `syslog-mode'.")


;;;###autoload
(defun syslog-mode ()
  "Major mode for working with system logs.

\\{syslog-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "syslog")
  (setq major-mode 'syslog-mode)
  (use-local-map syslog-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(syslog-font-lock-keywords))
  ;;
  (run-hooks 'syslog-mode-hook)
  )


(defvar syslog-boot-start-regexp "unix: SunOS"
  "Regexp to match the first line of boot sequence.")

(defun syslog-boot-start ()
  "Jump forward in the log to when the system booted."
  (interactive)
  (search-forward-regexp syslog-boot-start-regexp (point-max) t)
  (beginning-of-line))

(defvar syslog-ip-face 'syslog-ip-face)

(defcustom syslog-ip-face
  '(
    (t :underline t :slant italic :weight bold)
    )
  "Face for IPs"
  )

(defcustom syslog-hour-face
  '(
    (t :weight bold  :inherit font-lock-type-face)
    )
  "Face for IPs"
  )

(defcustom syslog-error-face
  '(
    (t  :weight bold :foreground "red")
    )
  "Face for IPs"
  )

(defcustom syslog-warn-face
  '(
    (t  :weight bold :foreground "goldenrod")
    )
  "Face for IPs"
  )

(defcustom syslog-info-face
  '(
    (t  :weight bold :foreground "deep sky blue")
    )
  "Face for IPs"
  )

(defcustom syslog-debug-face
  '(
    (t  :weight bold :foreground "medium spring green")
    )
  "Face for IPs"
  )

(defcustom syslog-su-face
  '(
    (t  :weight bold :foreground "firebrick")
    )
  "Face for IPs"
  )

  


;; Keywords
;; Todo: Seperate the keywords into a list for each format, rather
;; than one for all.
(defvar syslog-font-lock-keywords
  '(
    ;; Hours: 17:36:00 
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}:[[:digit:]]\\{1,2\\}\\(:[[:digit:]]\\{1,2\\}\\)?\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
    ;; Date
    ("\\(?:^\\|[[:space:]]\\)\\([[:digit:]]\\{1,2\\}/[[:digit:]]\\{1,2\\}/[[:digit:]]\\{2,4\\}\\)\\(?:$\\|[[:space:]]\\)" . (1 syslog-hour-face append))
    ;; Dates: May  9 15:52:34
    ("^\\(\\(?:[[:alpha:]]\\{3\\}\\)?[[:space:]]*[[:alpha:]]\\{3\\}\\s-+[0-9]+\\s-+[0-9:]+\\)" (1 font-lock-type-face t))
    ;; Su events
    ("\\(su:.*$\\)" . (1 syslog-su-face t))
    ("\\(sudo:.*$\\)" . (1 syslog-su-face t))    
    ("\\[[^]]*\\]" . 'font-lock-comment-face)
    ;; IPs
    ("[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}\\.[[:digit:]]\\{1,3\\}" (0 syslog-ip-face append))
    ("[Ee][Rr][Rr]\\(?:[Oo][Rr]\\)?" . (0 syslog-error-face append))
    ("[Ii][Nn][Ff][Oo]" . (0 syslog-info-face append))
    ("STARTUP" . (0 syslog-info-face append))
    ("CMD" . (0 syslog-info-face append))
    ("[Ww][Aa][Rr][Nn]\\(?:[Ii][Nn][Gg]\\)?" . (0 syslog-warn-face append))
    ("[Dd][Ee][Bb][Uu][Gg]" . (0 syslog-debug-face append))
    ("(EE)" . (0 syslog-error-face append))
    ("(WW)" . (0 syslog-warn-face append))
    ("(II)" . (0 syslog-info-face append))
    ("(NI)" . (0 syslog-warn-face append))
    ("(!!)" . (0 syslog-debug-face append))
    ("(--)" . (0 syslog-debug-face append))
    ("(\\*\\*)" . (0 syslog-debug-face append))
    ("(==)" . (0 syslog-debug-face append))
    ("(\\+\\+)" . (0 syslog-debug-face append))

    )
  "Expressions to hilight in `syslog-mode'.")


;;; Setup functions
(defun syslog-find-file-func ()
  "Invoke `syslog-mode' if the buffer appears to be a system logfile.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
	   (looking-at syslog-sequence-start-regexp))
      (syslog-mode)))

(defun syslog-add-hooks ()
  "Add a default set of syslog-hooks.
These hooks will activate `syslog-mode' when visiting a file
which has a syslog-like name (.fasta or .gb) or whose contents
looks like syslog.  It will also turn enable fontification for `syslog-mode'."
  ;; (add-hook 'find-file-hooks 'syslog-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\(messages\\(\\.[0-9]\\)?\\|SYSLOG\\)\\'" . syslog-mode))
  )

;; Setup hooks on request when this mode is loaded.
(if syslog-setup-on-load
    (syslog-add-hooks))

;; done loading
(run-hooks 'syslog-mode-load-hook)
(provide 'syslog-mode)

;;; syslog-mode.el ends here
