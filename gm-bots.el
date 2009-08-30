;;; gm-bots.el --- Here you can define new bots used by gm-webbot.el.
;;
;; Written by Gregor Mitsch <www.gmdsoft.de/mitsch>
;; This file is free software.
;;


;;; Commentary:
;;
;; Standard bots (Frontpage compatible):
;; 'Timestamp' and 'Substitution'.
;;
;; 'Navigation2' is used to make simple "go one level up" hyperlinks.
;;
;; I you wish to add your own bot use `gm-bot-bot-timestamp' as template.
;;


;;; History:
;; 
;; 2003-09-28: gm-bot-bot-navigation added (a navigation bot)
;;

;;; Code:

;; define standard bots
;; use gm-bot-data-get-key to get a key's value (see sample bots)


;;------------------------------------------------------------------------------


(defun gm-bot-bot-timestamp (DATA ACTION)
  "Insert a timestamp.  DATA must contain the following keys:
\"S-Format\" which value specifies the time format string.
ACTION see `gm-bot-register-bot'."

  ;; There are two bot actions: "valid" and "insert"
  ;; "valid" checks the validity of the given bot data
  ;;         (existence of keys, validity of values)
  ;; "insert" inserts the actual text if the bot is spanned.
  (cond
   ((equal ACTION "valid")
    (if (gm-bot-data-get-key DATA "S-Format")
        ;; return t --> OK
        t
      ;; set an error message
      (setq gm-bot-last-error "Key 'S-Format' not found.")
      ;; and return nil --> Error occured.
      nil
    ))
   ((equal ACTION "insert")
    ;; check for validiy
    (if (gm-bot-data-get-key DATA "S-Format")
        (progn
          ;; insert the text
          (insert (format-time-string
                   ;; extract the value of key `S-Format'
                   (second (gm-bot-data-get-key DATA "S-Format"))))
          ;; t == OK
          t)
      ;; nil == data not valid
      nil)
    )
   (t ;; default branch...
    ;; ...must never been reached (programmer's error)
    nil 
    ))
)


;;------------------------------------------------------------------------------


(defun gm-bot-bot-substitution (DATA ACTION)
  "Key 'S-Variable' must exists in DATA.
If value of 'S-Variable' equal \"user-full-name\" insert the value of
(user-full-name).
ACTION see `gm-bot-register-bot'."

  (cond
   ((equal ACTION "valid")
    (if (gm-bot-data-get-key DATA "S-Variable")
        (if (equal (second (gm-bot-data-get-key DATA "S-Variable"))
                   "user-full-name")
            t
          (setq gm-bot-last-error "'S-Variable' must be \"user-full-name\".") nil)
      (setq gm-bot-last-error "Key 'S-Variable' not found.") nil)
    )
   ((equal ACTION "insert")
    (if (gm-bot-bot-substitution DATA "valid")
        (progn
          (if (equal (second (gm-bot-data-get-key DATA "S-Variable"))
                     "user-full-name")
              (insert (user-full-name))
            t))
      nil)
    )
   (t
    nil ;; programmer error
    ))
)


;;------------------------------------------------------------------------------


(defun gm-bot-bot-navigation (DATA ACTION)
  "A webbot. bot=\"Navigation2\".
Insert a hyperlink to the next available index.htm or index.html.

Optional bot keys:
align: used for the align option of the HTML tag <p>
       if not given, no <p> tag will be inserted (only the <a> tag)
caption: specifies the hyperlink text (the link target if not specified)
   available variables: $dir (name of the directory where target is located
                        $target (name of the target file)
targetfile: search first for this file and not for index.[htm][html].

For example the current file is 'index.htm' and one of the target files
is 'index.htm' too. Then the bot searches for ../index.htm."


  (defun gm-bot-find-file-going-upwards (FILENAMES DEPTH)
    "Return the name of the found filename (including ../../...) (first),
the name of the directory (second),
the filename without ../../... (third),
the directory level (0=current, 1=parent, 2=...) where the file is located (fourth).
Or nil if nothing found.
DEPTH specifies the maximum directory level."

    (let ((cnt 0)
          (finish nil)
          (result nil)
          )
      (while (and
              (< cnt DEPTH)
              (not finish))

        ;; stand-alone-function...

        (let ((lst FILENAMES)
              (cur-file (car FILENAMES))
              (found nil))

          (while (and
                  ;; important: order!
                  (not found)
                  (setq cur-file (car lst))
                  )
            (setq lst (cdr lst))
            (setq found (file-exists-p cur-file))
            )
     
          (if (not found)
              ;; go upwards (see below)
              nil
            ;; else set result
            (setq result
                  (list cur-file
                        (file-name-nondirectory
                         (substring
                          (file-name-directory
                           (expand-file-name cur-file)) 0 -1))
                        (file-name-nondirectory cur-file)
                        cnt
                        ))
            ;; set finish
            (setq finish t))

          ) ;; ...end (stand-alone-function)

        ;; go up one level
        (setq FILENAMES (mapcar (lambda (FILE) (concat "../" FILE)) FILENAMES))

        ;; increase counter
        (setq cnt (+ cnt 1))
        )

      result
      )
    )



  (cond
   ((equal ACTION "valid")
    t
    )
   ((equal ACTION "insert")
    (if (gm-bot-bot-navigation DATA "valid")
        (progn
          ;; standard values
          (let ((href-align nil)
                (target-files (list "index.htm" "index.html"))
                (href-caption "$target")
                (href-target nil)
                (search-res nil)
                (cnt 0)
                (tmpstr nil)
                )

            ;; optional align
            (if (gm-bot-data-get-key DATA "align")
                (setq href-align (second (gm-bot-data-get-key DATA "align")))
              )

            ;; optional target file
            (if (gm-bot-data-get-key DATA "targetfile")
                (progn
                  (setq target-files (list (second
                                (gm-bot-data-get-key DATA "targetfile")))))
              )

            ;; optional caption
            (if (gm-bot-data-get-key DATA "caption")
                (setq href-caption (second
                                    (gm-bot-data-get-key DATA "caption")))
              )

            
            ;; if search file name is equal to current file
            ;; go one directory upwards
            (setq target-files (mapcar (lambda (FILE)
                                         (if (equal FILE
                                                    (file-name-nondirectory
                                                     (buffer-file-name)))
                                               (concat "../" FILE)
                                           FILE
                                           ))
                                       target-files))

            ;; create href-target
            ;;   ; search for target files (depth == 5)
            (setq search-res (gm-bot-find-file-going-upwards target-files 5))

            ;;   ; if not found
            (if (not search-res)
                (setq href-target ""))

            ;;   ; set href-target
            (setq href-target (concat href-target (first search-res)))


            ;; create caption string
            ;;   ; if not found
            (if (not search-res)
                (setq href-caption "File not found.")
              ;; ; replace...
              (setq href-caption
                    (replace-in-string
                     (replace-in-string
                      ;; $target with target's filename
                      href-caption "$target" (third search-res))
                     ;; $dir with directory name
                     "$dir" (second search-res))))
            

            ;; insert the hyperlink
            ;; e.g.: <p align="right"><a href="index.htm">up <font size=-2>(fun/index.htm)</font></a></p>
            (if href-align
                ;; insert with <p> tag
                (insert (concat "<p align=\"" href-align "\"><a href=\""
                                href-target "\">" href-caption "</a></p>"))
              ;; insert without <p> tag
              (insert (concat "<a href=\""
                              href-target "\">" href-caption "</a>"))
              )
                              
            ;; return t
            t
            ))
      nil)
    )
   (t
    nil ;; programmer error
    ))
)


;;------------------------------------------------------------------------------
;;
;; register bots:
;;
(gm-bot-register-bot "Timestamp" 'gm-bot-bot-timestamp)
(gm-bot-register-bot "Substitution" 'gm-bot-bot-substitution)
(gm-bot-register-bot "Navigation2" 'gm-bot-bot-navigation)

;;------------------------------------------------------------------------------

(provide 'gm-bots)

;;; gm-bots.el ends here
