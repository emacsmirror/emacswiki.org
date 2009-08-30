;;; hidesearch.el --- Incremental search while hiding non-matching lines.
;; 
;;; Author: Craig Muth
;; 
;;; History
;; 
;; 2007-06-05 - Initial release
;; 
;;; Commentary
;; 
;; Here's a sample of what you might add to your .emacs file to get this
;; to work:
;;
;; (add-to-list 'load-path "~/<dir containing this file and hide-lines.el")
;; (require 'hide-lines)
;; (require 'hidesearch)
;; (global-set-key (kbd "C-c C-s") 'hidesearch)
;; (global-set-key (kbd "C-c C-a") 'show-all-invisible)
;;
;;; Other notes (assumes you used the above key mappings)
;;
;;  - To try it out type C-c C-s then a few chars
;;   - (a substring that exists within your buffer)
;;   - You'll see the hiding happen incrementally
;;   - Press enter when done
;;  - To un-hide everything, type C-c C-a
;;  - During a search, press C-a to cancel and un-hide all
;;   - This should probably be C-g, but I didn't know how to trap it
;;  - During a search, tab resets search string and continues
;;   - (behaves like an "or" operator)
;;  - During a search try pressing C-1 through C-9
;;   - to jump to the nth visible line and un-hide all
;;   - Some terminals don't correctly pass these chars through to emacs
;; 

(defun hidesearch (&optional initial-pattern press-enter) (interactive)
"Incrementally show only lines in file based on what user types.  If 'initial-pattern is provided, start out searching with it.  If press-enter is provided, execute C-m if C-0 (and C-1 .etc) is pressed."

  (when initial-pattern
    (hide-non-matching-lines initial-pattern)
;    (recenter -2)
    )
  (let ((pattern (or initial-pattern "")) ch)
    ;(message "3 Show lines matching: ")
    (setq ch (read-char))
    ; This happens when called from command-execute
    (unless (= -1 ch)
      ; Until they hit a control char
      (while (or (string-match "[?&={}@\"|()<>a-zA-Z!*\\_',.\t~# /$!:;+-]" (char-to-string ch)) (and (>= ch ?0) (<= ch ?9)))
        (cond
          ; If tab, start over with new pattern
          ((= ch ?\C-i) (progn (message "tab!") (setq pattern "")))
          ; Otherwise
          (t
            (setq pattern (concat pattern (regexp-quote (char-to-string ch))))
            (hide-non-matching-lines pattern)))
        (recenter -2)
        ;(message "4 Show lines matching: %s" pattern)
        (setq ch (read-char)))

      ; Check last char entered
      (cond 
        ; Do nothing if C-m
        ((= ch ?\C-m))
        ((= ch ?\C-a) (show-all-invisible))

        ; Go to nth line if C-1 - C-9
        ((and (>= ch ?\C-1) (<= ch ?\C-9))
          (hidesearch-goto-line-show-all (- ch 67108912) press-enter))

        ; If meta, jump (as above) and make it hide region (UBO)
        ((and (>= ch ?\M-1) (<= ch ?\M-9))
;         (message "-> meta")
;         (pp ch)
         (hidesearch-goto-line-show-all (- ch 134217776) press-enter)
         ; TODO call callback fuction
         (if (fboundp 'hidesearch-meta-callback)
             (hidesearch-meta-callback)
             )

         )

        ; Otherwise, run whatever they typed last as a command (it was probably C-e, etc.)
        (t (command-execute (char-to-string ch))))

      ; Remember search string, so subsequent isearch's use it
      (isearch-update-ring pattern)
))
)

(defun hidesearch-goto-line-show-all (line &optional press-enter)
  (beginning-of-buffer)
  (next-line line)
  (show-all-invisible)
  (next-line -1)
  (recenter 0)
  ; Press enter (probably to open a file) if press-enter
  (when press-enter (command-execute "\C-m"))
)

(define-key isearch-mode-map (kbd "C-h")
  (lambda ()
    "When run during isearch, show only lines matching search."
    (interactive)
    (isearch-done)
    (isearch-clean-overlays)
    (hide-non-matching-lines (if isearch-regexp isearch-string (regexp-quote isearch-string)) )
    (recenter -2)
    (hidesearch (if isearch-regexp isearch-string (regexp-quote isearch-string)) )))

(provide 'hidesearch)
