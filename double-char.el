;;; double-char.el --- Get different insertion when you hit a char twice fast enought

;;; Commentary:
;; a silly usage but clear:
;; (defdoublechar albatros "12" "abc")
;; (local-set-key "ù" 'double-char---albatros)
;; then press "ùù" in less than 0.3 second and get "abc"
;; or "1212" if more than 0.3 second
;; change the delay with the var double-char-delay
;; A less silly example:
;; (defdoublechar great "<" ">")
;; (global-set-key (kbd "<") 'double-char---great)
;; now you have just one key for "<" and ">"

;;; Code:

(defvar double-char--last-time nil
  "Keep track of the time we were last called.")

(defcustom double-char-delay 0.3
  "Maximum delay between command invocations."
  :type 'float
  :group 'double-char)

(defvar double-char--list (make-hash-table :test 'equal)
  "Association between the name of a function and the strings to insert.
string to insert if the double press fail and the string to
  insert it succed.")

(defun double-char--success (command)
  "The success string associated with COMMAND."
  (second (gethash command double-char--list)))

(defun double-char--fail (command)
  "The fail string associated with COMMAND."
  (first (gethash command double-char--list)))

(defmacro defdoublechar (name pressed-char string-insert)
  "Produce a function which call 'double-char' with NAME.
parameter and fill the 'double-char--list' hash.  Name the name you
want for the function, PRESSED-CHAR the insert on fail,
STRING-INSERT the insert on success."
  (let ((fun-name (intern (concat "double-char---" (symbol-name name)))))
    (puthash fun-name (list pressed-char string-insert) double-char--list)
    `(defun ,fun-name ()
       (interactive)
       (double-char ',fun-name))))

(defun double-char (command)
  "Insert the fail or successstring of the COMMAND."
  (let ((time (current-time))
        (prev-cmd last-command))
    (if (and (eq prev-cmd command)
             (< (- (time-to-seconds time)
                   (time-to-seconds double-char--last-time))
                double-char-delay))
        (progn
	  (setq double-char--last-time '(0 0 0))
          (delete-backward-char (string-width (double-char--fail command)))
          (insert (double-char--success command)))
      (progn
	(insert (double-char--fail command))
	(setq double-char--last-time (current-time))))))

(provide 'double-char)
(provide 'double-char)

;;; double-char.el ends here
