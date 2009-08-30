;;; procmaillog.el --- Read the procmail log file in Gnus

;; <http://www.dd.chalmers.se/~bojohan/emacs/lisp/>

;; This is free software.

;; We need blank lines in the log file:

;; [ ~/.procmailrc]:
;; LOGFILE=$HOME/.procmaillog
;; LOG="
;; "
 
(defvar procmaillog-file "~/.procmaillog")
 
(defun nndoc-procmaillog-type-p ()
  (equal (expand-file-name procmaillog-file) nndoc-address))
 
(defun nndoc-procmaillog-article-transform (article)
  (delete-region (point-min) (point-max))
  (nndoc-procmaillog-generate-head article)
  (insert "\n" (concat (gnus-fetch-field "folder"))))
 
(defun nndoc-procmaillog-generate-head (article)
  (let ((entry (cdr (assq article nndoc-dissection-alist)))
        subject from date folder number)
    (save-excursion
      (set-buffer nndoc-current-buffer)
      (save-restriction
        (narrow-to-region (car entry) (nth 3 entry))
        (goto-char (point-min))
        (when
            (re-search-forward
             "^from \\(\\S-*\\) \\s-*\\(.*\\)")
          (setq from (match-string 1))
          (setq date (match-string 2)))
        (when (re-search-forward
               "^\\s-*Subject: \\(.*\\)" nil t)
          (setq subject (match-string 1)))
        (when (re-search-forward
               "^\\s-*folder: \\(\\S-*\\)\\s-+\\(.*\\)")
          (setq folder (match-string 1))
          (setq number (match-string 2)))))
    (insert "From: " (or from "(nobody)")
            "\nSubject: " (or subject "(no subject)")
            (if date (concat "\nDate: " date) "")
            "\nX-Procmail-Folder: " folder
            "\nX-Procmail-Number: " number
            "\n")))
 
(nndoc-add-type
 `(procmaillog
  ;; (first-article . ,(concat "^" (make-string 70 ?-) "\n\n+"))
  (article-begin . "^From")
  (article-transform-function . nndoc-procmaillog-article-transform)
  (generate-head-function . nndoc-procmaillog-generate-head)
  ;; (prepare-body-function . nndoc-procmaillog-prepare-body)
  ;; (body-begin . "")
  ;; (body-end . "")
  ;; (body-end-function . nndoc-digest-body-end)
  ;;(head-end . "\nFrom")
  ;; (file-end . "^End of .*digest.*[0-9].*\n\\*\\*\\|^End of.*Digest *$")
  ;; (subtype digest guess)
  )
 'first)
 
;;;;;;;;;;;;;;;;
 
(defvar procmaillog-summary-line-format
  "%U%R%z%I%(%[%4L: %-23,23f%]%) %-12u&procmail-folder; %s\n"
  ;;"%ua%U%R%4N %6&user-date; %1{%[%} %(%-20,20f%)%* %1{%]%} %-10u&procmail-folder; %0{%S%}\n"
)
 
(defun gnus-user-format-function-procmail-folder (header)
  (let* ((folder (cdr (assq 'X-Procmail-Folder
                            (mail-header-extra header)))))
    (cond ((null folder)
           (setq folder ""))
          ((string= folder "/dev/null")
            (set-text-properties 0 (length folder) '(face gnus-face-1) folder))
          ((string-match "spam" folder)
           (set-text-properties 0 (length folder) '(face gnus-face-4) folder)))
    folder))
 
(add-hook 'gnus-started-hook
          (lambda ()
            (add-to-list 'gnus-extra-headers 'X-Procmail-Folder)))
 
(add-to-list 'gnus-parameters
             '("^nndoc.*\\.procmaillog$"
               (gnus-summary-line-format procmaillog-summary-line-format)
               (gnus-summary-highlight nil)))
 
(provide 'procmaillog)
 
;;; procmaillog.el ends here
