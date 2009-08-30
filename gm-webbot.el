;;; gm-webbot.el --- An implementation of Webbots known from MS Frontpage.
;;
;; Written by Gregor Mitsch <www.gmdsoft.de/mitsch>
;; This file is free software.
;;
;; What are Webbots?
;; see `bot-doc.pdf'
;;
;;
;; Usage:
;; 1) Load this file with `load'
;; 2) Call `gm-bot-activate-for-modes' to specify the modes that should be
;;    botified.
;; 
;; Example for `init.el's:
;; ;; This enables the bot feature in html-mode and php-mode.
;; (load "path/to/webbot/gm-webbot")
;; (gm-bot-activate-for-modes 'html-mode 'php-mode)
;;
;;
;; Available bots and "How to add your own bot?": see gm-bots.el
;;


;;; Commentary:
;; 
;; <!--webbot bot="el-functions" line-number="t" prefix=";; " startspan -->
;; gm-bot-register-bot - line
;; (setq buffer-regexp-list (buffer-name)) (list-matches-in-buffers "(defun [^ ]+")
;;
;; now in testphase!!!
;; 
;; how to test if a symbol is a member of a list --> memq

;;; History:
;; 
;; version 1.0:
;;  basic implementation including two bots:
;;  "Timestamp" and "Substitution" for user-full-name.
;;

;;; Code:

;;------------------------------------------------------------------------------

;; defvars / regexps:

(defvar gm-bot-start-str "<!--webbot "
  "A bot starts with this string.")

(defvar gm-bot-end-str "-->"
  "A bot ends with this string.")

(defvar gm-bot-startspan-str "startspan"
  "Name of the key that indicates that the bot is the left side of a spanned bot.")

(defvar gm-bot-endspan-str "endspan"
  "Name of the key that indicates that the bot is the right side of a spanned bot.")

(defvar gm-bot-space-str
  "[ 	
]"
  "Regex that matches spaces, tabs and newlines.")

(defvar gm-bot-value-char-str
  "\\([
]\\|.\\)"
  "Regex that matches a valid value string.")

(defvar gm-bot-available-bots (cons nil nil)
  "Stores all known bots in a assoc list.
((\"botname1\" botfunction2) (\"botname2\" botfunction2) ...)")

(defvar gm-bot-botified-modes nil)
  "List of modes that use the bot feature."


;; gm-bot-warnings:
;; ((line-number "text") (line-number2 "text2"))

;; gm-bot-last-error
;; a string

;;------------------------------------------------------------------------------

(defun gm-bot-activate-for-modes (&rest MODES)
  "Declare the modes MODES botified. This means the bot feature is enabled in
theses modes. Example:
(gm-bot-activate-for-modes 'html-mode 'php-mode)."
  (setq gm-bot-botified-modes MODES)
  )

(defun gm-bot-register-hooks ()
"Activate the bot feature for files in the mode listed in
`gm-bot-botified-modes'."

  ;; unspan bots and clear modification flag
  (add-hook 'find-file-hooks
            '(lambda ()
               (if (not
                    ;; check if name of the current major mode 
                    ;; is in the list of modes that are botified
                    (memq (symbol-name major-mode)
                          (mapcar 'symbol-name gm-bot-botified-modes)))
                   nil
                 (gm-bot-on-load-file)
                 (not-modified)
                 nil)
               ))

  ;; span bots and save buffer
  (add-hook 'write-file-hooks
            '(lambda ()
               (if (not
                    (memq (symbol-name major-mode)
                          (mapcar 'symbol-name gm-bot-botified-modes)))
                   nil
                 (gm-bot-on-save-file)
                 nil
                 )
               ))

  ;; unspan bots and  clear modification flag
  (add-hook 'after-save-hook
            '(lambda ()
               (if (not
                    (memq (symbol-name major-mode)
                          (mapcar 'symbol-name gm-bot-botified-modes)))
                   nil
                 (gm-bot-on-load-file)
                 (not-modified)
                 nil)
               ))
)


;;------------------------------------------------------------------------------

(defun gm-bot-register-bot (NAME FUNCTION)
  "NAME: name of the bot (bot=\"NAME\")
FUNCTION: function that handles all the bot's action.  This function takes two
parameters: a parse-data list DATA.  It returns nil if this list is not valid
otherwise it returns the string based on DATA.
Second parameter is ACTION.  If ACTION==\"valid\" the function return t
if data is valid otherwise nil.  You can use the `gm-bot-last-error' to supply
an error message.
If ACTION==\"insert\" the function inserts its string into the current buffer.
If DATA was not valid it returns nil otherwise t."

  ;; if bot exists change it
  (if (assoc NAME gm-bot-available-bots)
      (setcdr (assoc NAME gm-bot-available-bots) (cons FUNCTION nil))
    (if (first gm-bot-available-bots)
        ;; if list first element is nil replace it
        (setcdr (last gm-bot-available-bots) (list (list NAME FUNCTION)))
      ;; else append it
      (setcar (last gm-bot-available-bots) (list NAME FUNCTION))
      )
    )
  )

(defun gm-bot-call-bot-function (NAME DATA ACTION)
  "Call the function of the bot NAME with DATA and ACTION as parameters.
Return nil if bot NAME is not registerd otherwise the return value
of the called function."

  (if (assoc NAME gm-bot-available-bots)
      (progn
        ;; reset last error
        (setq gm-bot-last-error nil)
        
        ;; call bot function
        (funcall (second (assoc NAME gm-bot-available-bots)) data ACTION)
        )
    nil)
  )

;;------------------------------------------------------------------------------

;;
;; load bots:
;;
;;                                                      without / !!!
(load-file (concat (file-name-directory load-file-name) "gm-bots.el"))

;;
;; add hooks:
;;
(gm-bot-register-hooks)


;;------------------------------------------------------------------------------

(defun gm-bot-unspan-next-bot ()
  "Unspans the next bot in the current buffer.
Returns nil if there if the last `gm-bot-start-str' tag was passed;
        t if bot was successfully unspanned;
        a string that contains an error message otherwise.  In that case point
will be placed right before the next `gm-bot-start-str' tag."

  (catch 'exception
    (progn
      ;;(1)
      (if (gm-bot-goto-next-bot)
          (let ((marker-startspan-bot-start (point-marker));;(3)
                (marker-startspan-bot-end nil)
                (marker-endspan-bot-start nil)
                (marker-endspan-bot-end nil)
                (data nil) (data2 nil))

            ;;(4)
            (setq data (gm-bot-parse-bot))

            ;;(5)
            (if (gm-bot-data-get-error data)
                (throw 'exception (gm-bot-data-get-error data)))

            (if (not (gm-bot-data-get-key data "bot"))
                (throw 'exception "Key 'bot' not found."))

            (if (not (assoc (second (gm-bot-data-get-key data "bot"))
                            gm-bot-available-bots))
                (throw 'exception
                       (concat "Bot '"
                               (second (gm-bot-data-get-key data "bot"))
                               "' not registered.")))

            (if (not (gm-bot-data-get-key data gm-bot-startspan-str))
                (throw 'exception "startspan not found."))

            (if (second (gm-bot-data-get-key data gm-bot-startspan-str))
                (throw 'exception "startspan value not nil."))

            ;;(6)
            (if (gm-bot-call-bot-function
                      (second (gm-bot-data-get-key data "bot")) data "valid")
                (progn
                  ;;(7)
                  (setq marker-startspan-bot-end (point-marker))
                  ;;(8)
                  (if (gm-bot-goto-next-bot)
                      (progn
                        ;;(9)
                        (setq marker-endspan-bot-start (point-marker))
                        ;;(10)
                        (setq data2 (gm-bot-parse-bot))

                        ;;(11)
                        (if (gm-bot-data-get-error data2)
                            (throw 'exception
                                   (progn
                                     ;; go back (that further bots can be found)
                                     (goto-char marker-endspan-bot-start)
                                     (gm-bot-data-get-error data2))))

                        ;;(12)
                        (setq marker-endspan-bot-end (point-marker))

                        ;;(13)
                        (if (and
                             (equal
                              (second (gm-bot-data-get-key data "bot"))
                              (second (gm-bot-data-get-key data2 "bot")))
                             (gm-bot-data-get-key data2 gm-bot-endspan-str)
                             (not (second
                                   (gm-bot-data-get-key data2 gm-bot-endspan-str))))
                            (progn
                              ;;(14)
                              (gm-bot-data-remove-key data gm-bot-startspan-str)

                              ;;(15)
                              (delete-region marker-startspan-bot-start
                                             marker-endspan-bot-end)

                              ;;(16)
                              (insert (gm-bot-data-key-values-to-string data))
                              (insert gm-bot-end-str)
                            
                            
                              )
                          (throw 'exception
                                 "startspan and endspan bots do not match OR endspan does not exist.")
                          )
                        )
                    (throw 'exception "There is no endspan bot.")
                    )
                  )
              (throw 'exception (concat "Bot function error: "
                                        gm-bot-last-error))
              )
            t ;; return 'success'
            )
        nil ;; return 'no further bot found'
        )
      
      ));; end catch
  ;; return 'one of the throw statements'
  )

(defun gm-bot-span-next-bot ()
  "Spans the next bot in the current buffer.
That is use the bot functions to insert text.
Returns nil if there if the last `gm-bot-start-str' tag was passed;
        t if bot was successfully spanned;
        a string that contains an error message otherwise.  In that case point
will be placed right before the next `gm-bot-start-str' tag."

  (catch 'exception
    (progn
      ;;(1)
      (if (gm-bot-goto-next-bot)
          (let ((marker-bot-start (point-marker));;(3)
                (marker-bot-end nil)
                (data nil) (data2 nil))

            ;;(4)
            (setq data (gm-bot-parse-bot))

            ;;(5)(6) error detection
            (if (gm-bot-data-get-error data)
                (throw 'exception (gm-bot-data-get-error data)))

            ;; bot should not have a 'startspan' key (ignore)
            (if (gm-bot-data-get-key data gm-bot-startspan-str)
                (throw 'exception t)) ;; Bot is already spanned. return success

            ;; bot should not have an 'endspan' key (ignore)
            (if (gm-bot-data-get-key data gm-bot-endspan-str)
                (throw 'exception t)) ;; Bot is already spanned. return success
            
            (if (not (gm-bot-data-get-key data "bot"))
                (throw 'exception "Key 'bot' not found."))

            (if (not (assoc (second (gm-bot-data-get-key data "bot"))
                            gm-bot-available-bots))
                (throw 'exception
                       (concat "Bot '"
                               (second (gm-bot-data-get-key data "bot"))
                               "' not registered.")))

            ;; check if data is valid
            (if (not (gm-bot-call-bot-function
                      (second (gm-bot-data-get-key data "bot")) data "valid"))
                (throw 'exception
                       (concat "Bot '" (second (gm-bot-data-get-key data "bot"))
                               "' error: "
                               gm-bot-last-error)))

            ;;(7)
            (setq marker-bot-end (point-marker))

;            (message "ok")(read-char) ; debug

            ;;(8)
            (gm-bot-data-set-key data gm-bot-startspan-str nil)

            ;;(9)
            (delete-region marker-bot-start marker-bot-end)

;            (message "ok")(read-char) ; debug

            ;;(10)
            (insert (gm-bot-data-key-values-to-string data))
            (insert gm-bot-end-str)

;            (message "ok")(read-char) ; debug

            ;;(11) insert the bot's string (should be valid, we checked it above)
            (gm-bot-call-bot-function
             (second (gm-bot-data-get-key data "bot")) data "insert")

            ;;(12)
            (setq data2 (gm-bot-data-init))
            (gm-bot-data-set-key data2 "bot" (second (gm-bot-data-get-key data "bot")))
            (gm-bot-data-set-key data2 gm-bot-endspan-str nil)

            ;;(13)
            (insert gm-bot-start-str)
            (insert (gm-bot-data-key-values-to-string data2))
            (insert gm-bot-end-str)

            t ;; return 'success'
            )
        nil ;; return 'no further bot found'
        )
      ))
  )



(defun gm-bot-on-load-file ()
  "Unspanns all spanned bots that are registered and valid (see bot.doc)."

  (interactive)
  
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        ;; goto the beginning of the file
        (beginning-of-buffer)

        (setq gm-bot-warnings "")

        (let ((res nil) (cnt 0) (msg ""))
          (while (setq res (gm-bot-unspan-next-bot))
            (setq cnt (+ cnt 1))
            (if (stringp res)
                (setq gm-bot-warnings
                      ;; line X: msg
                      (concat gm-bot-warnings "line " (int-to-string (line-number))
                              ": " res "\n")))
            )

          (setq msg (concat "gm-bot: " (int-to-string cnt) " bots passed."))
          (if (not (equal gm-bot-warnings ""))
              (setq msg (concat msg " Warnings see next line...\n" gm-bot-warnings))
            )
;;    (insert (concat "\n\n[[[" msg  "]]]")) ; debug
          (message msg)
          )
        )
      )
    )
)


(defun gm-bot-on-save-file ()
  "Spans all registred bots if their data is valid (see bot.doc)."

  (interactive)

  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        ;; goto the beginning of the file
        (beginning-of-buffer)

        (setq gm-bot-warnings "")

        (let ((res nil) (cnt 0) (msg ""))
          (while (setq res (gm-bot-span-next-bot))
            (setq cnt (+ cnt 1))
            (if (stringp res)
                (setq gm-bot-warnings
                      ;; line X: msg
                      (concat gm-bot-warnings "line " (int-to-string (line-number))
                              ": " res "\n")))
            )

          (setq msg (concat "gm-bot: " (int-to-string cnt) " bots passed."))
          (if (not (equal gm-bot-warnings ""))
              (setq msg (concat msg " Warnings see next line...\n" gm-bot-warnings))
            )
;     (insert (concat "\n\n[[[" msg  "]]]")) ; debug
          (message msg)
          )
        )
      )
    )
  )

;;------------------------------------------------------------------------------

(defun gm-bot-goto-next-bot ()
  "Places point after the next occurence of `gm-bot-start-str'.
Returns nil if no further bot was found."
  ;;                                        no limit, no error
  (re-search-forward (concat gm-bot-start-str) nil t)
)


(defun gm-bot-parse-key-value (DATA)
  "Parse a key-value pair (key=\"string\" append the result to DATA.
Returns the current data."

  ;; search for spaces and valid key characters
    (let ((start-pos (point)))
      (save-excursion
      (if (or (not (re-search-forward
                    (concat gm-bot-space-str "*\\([-a-zA-Z0-9_]+\\)") nil t))
              (not (equal start-pos (match-beginning 0))))
          (gm-bot-data-set-error DATA "Valid key expected.")
        )
      )
      (goto-char (match-end 0))
      )

  ;;     key-str = name of the parsed key
  (let ((key-str (buffer-substring (match-beginning 1) (match-end 1)))
        (start-pos (point)))

    ;; search for value
    (if (and (re-search-forward (concat gm-bot-space-str
                                        "*="
                                        gm-bot-space-str
                                        "*\"\\(" gm-bot-value-char-str
                                        "+?\\)\"") nil t)
             (equal start-pos (match-beginning 0)))
        ;; if success add key and value to DATA
        (progn
          (gm-bot-data-set-key DATA key-str
                               (buffer-substring
                                (match-beginning 1) (match-end 1)))
          (goto-char (match-end 0))
          )
      ;; else add nil value
      (gm-bot-data-set-key DATA key-str nil)
      (goto-char start-pos)
      )

    DATA ;; return value
    )
  )

(defun gm-bot-parse-bot ()
  "Precondition: POINT is after a `gm-bot-start-str'.
Parses all keys and values until a `gm-bot-end-str' appears.
Returns the bot's data."

  
  (let ((DATA (gm-bot-data-init))(continue t) (start-pos nil))
    (while continue
      (setq start-pos (point))
      ;; if next token is the end-bot token
      (if (and (re-search-forward
                (concat gm-bot-space-str "*" gm-bot-end-str) nil t)
               (equal start-pos (match-beginning 0)))
          ;; end loop in next iteration
          (setq continue nil)

        ;; else return to search begin pos
        (goto-char start-pos)

        ;; parse a key-value pair
        (setq DATA (gm-bot-parse-key-value DATA))
        (if (gm-bot-data-get-error DATA)
            (setq continue nil))

;        (message "ok")(read-char) ; debug
        )
      )
    DATA ;; return value
    )
  )

;;------------------------------------------------------------------------------

;;(defun gm-bot-data-check-error (DATA)
;;  "Displays a warning if DATA has got an error."
;;  (if (gm-bot-data-get-error DATA)
;;      (if (y-or-n-p (concat "Parsing error: "
;;                            (gm-bot-data-get-error DATA) " continue (y)?"))
;;          ;; reset parsing error
;;          (gm-bot-data-set-error DATA nil)
;;        ;; else quit with an error
;;        (error 'syntax-error "while parsing a gm-bot.")
;;        )
;;    )
;;  )

(defun gm-bot-data-init ()
  "Return a new parse data list."
  (list nil (cons nil nil))
  )

(defun gm-bot-data-get-error (DATA)
  "Return nil if there is no error in DATA otherwise return an error string."
  (first DATA)
  )

(defun gm-bot-data-set-error (DATA MESSAGE)
  "Set DATA to have an error message MESSAGE.
If MESSAGE is nil the error flag is removed."
  (setcar DATA MESSAGE)
  )

(defun gm-bot-data-set-key (DATA KEY VALUE)
  "Set the KEY of DATA to VALUE. If KEY does not exist append it."

  ;; if key exists
  (if (gm-bot-data-get-key DATA KEY)
      (setcdr (gm-bot-data-get-key DATA KEY) (cons VALUE nil))
    ;; else
    (setcdr (last (second DATA)) (list (list KEY VALUE)))
      )
  )

(defun gm-bot-data-get-key (DATA KEY)
  "Return a list that contain the name of KEY and the associated value from DATA.
Return nil if key does not exist."
  (assoc KEY (second DATA))
  )

(defun gm-bot-data-remove-key (DATA KEY)
  "Remove KEY and its associated value from DATA."
  (remassoc KEY (second DATA))
  )

(defun gm-bot-data-key-values-to-string (DATA)
  "Return a string with all keys and values from DATA."

  (let ((tmplist DATA) (result ""))
    (setq tmplist (second tmplist))
    (while tmplist
      (if (first tmplist)
          (progn
            (setq result (concat result (first (first tmplist))))
            (if (second (first tmplist))
                (setq result (concat result "=\""
                                     (second (first tmplist)) "\" "))
              (setq result (concat result " ")))
            ))
      (setq tmplist (cdr tmplist))
      )
    result
    )
  )

;;------------------------------------------------------------------------------

(provide 'gm-webbot)

;;------------------------------------------------------------------------------

;;; gm-webbot.el ends here
