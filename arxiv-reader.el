;;; arxiv-reader.el --- an interface for reading and sorting arXiv abstracts.
;;; Inspired by Hubert Chen's java "reader."

;; Copyright (C) 2008,2009 Peter H. Mao

;; Author: Peter H. Mao <peter.mao@gmail.com> <peterm@srl.caltech.edu>
;; Version %Id: 8%

;; arxiv-reader.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version. (http://www.gnu.org/licenses/gpl-3.0.txt)

;; arxiv-reader.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;;; change log
;;
;; 2009-10-16: re-introduce doc-view, as it is now part of emacs-23.
;; arxiv-get-pdf ('P') now downloads the pdf and displays it.
;;
;; 2009-08-27: minor change: use switch-to-buffer instead of
;; set-buffer after look-at-this-file
;;
;; 2009-08-21: BUG FIX -- previously, if the first abstract was an
;; update, the look-buffer contents would still hold that file, even
;; though it was moved to the arxiv-b-list.  The solution was to use a
;; new look-mode function called look-at-this-file, which refreshes
;; the contents of the buffer based on the present state of the file
;; list.
;;
;; 2009-03-24: added increment/decrement subdir list, removed cyclic
;; permutation subroutines

;;; Commentary:  this documentation is getting insane.
;;
;; This package extends the functionality of look-mode.  It defines
;; the minor mode "arxiv" which provides a keymap to move abstracts
;; into and between subdirectories.  
;; 
;; The program typically is run from dired mode with "r" bound to
;; the function arxiv-read-abstracts and 'a' bound to
;; arxiv-look-at-abstracts.  See setup instructions below.
;; 
;; arxiv-keyword-list is a list of regular expressions that are
;; highlighted in the abstracts.  It can be modified via the
;; Customize interface.  "H" (think: highlight) toggles the variable
;; arxiv-keyword-matches-only, which when 't', causes arxiv-reader
;; to navigate only to abstracts with highlighted keywords.
;; 
;; Upon startup, the program makes a recursive check into your
;; subdirectories for duplicate or updated abstracts.  If there are
;; any, they are reported in a startup buffer, and on uses "C-x b"
;; get back to the abstracts.  To view the updated abstracts,
;; type "S" (arxiv-swap-lists).  "S" toggles you back and forth
;; between the main abstract list and the updated abstract list.
;; 
;; Presently, I do not know the effect of trying to move files in
;; the updated abstract list, as they may be in subdirectories
;; several levels down.
;; 
;; arxiv-move-to-subdir (bound to 'R') moves the currently shown
;; file into default destination directory (indicated by the
;; highlighted index.  To move files into other directories, use a
;; numeric prefix argument (ex: "C-2 R" or "C-u 2 R" moves the file
;; into the #2 directory).  That directory will then become the
;; default destination directory.  Abstracts can be moved back into
;; the starting directory by using 0 as the prefix argument.  If the
;; pdf has been downloaded, it moves with the abstract.
;; 
;; arxiv-get-pdf (bound to 'P') gets the pdf of the current
;; abstract, puts it in <filename><Last><FI>.pdf.  The pdf will be
;; saved in the same directory as the current file and will be
;; opened using 'doc-view' (planned for emacs 23).
;; 
;; Side effect: arXiv mode sets "look-show-subdirs" and to t.  This
;; makes look list the subdirectories of the starting directory on
;; the header line.
;; 
;; File names that begin with '#', ',' or '.', or end with 'pdf' are
;; excluded from the file list.  Directories that begin with '.' or ','
;; are excluded from the directory list.

;;; Setup:
;;
;; put this file and look-mode.el into a directory in your load-path.
;; look-mode also requires eimp.el, but that should be fixed at some point.
;; Or cons them onto your load-path.
;; ex: (setq load-path (cons "~/my_lisp_files/" load-path))
;;     (load "look-mode")
;;     (load "arxiv-reader")
;; I like to bind "r" to arxiv-read-abstracts and "a" to 
;; arxiv-look-at-abstracts in dired:
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;             (define-key dired-mode-map "r" 'arxiv-read-abstracts)
;;             (define-key dired-mode-map "a" 'arxiv-look-at-abstracts)))

;;; Usage:
;;
;; (arxiv-read-abstracts &optional filename)
;;
;; When browsing abstracts, you can run 
;; (arxiv-move-to-subdir subdir-number) 
;; where subdir-number is the integer in front of the
;; directory name.  With no subdir-number specified, the program
;; uses 1.
;;
;; To customize highlighting in the abstract, run "C-c k" to access the 
;; Customize interface for keywords.
;;
;; H: turn on/off skipping to highlighted abstracts
;; P: get abstract's pdf-formatted paper
;; R: rename file into subdirectory
;; S: swap file lists (to see updated abstracts)
;; C-c k: edit keywords

;;; future:
;;
;; tweak the highlighting so that Title and Author fields are visually enhanced.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; variables and definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup arxiv nil
  "A mode for reading arXiv abstracts"
  :prefix "arxiv-"
  :group 'applications)

(defcustom arxiv-keyword-list nil
  "A list of regexp keywords to highlight in arXiv abstracts."
  :group 'arxiv
  :type '(repeat regexp))
(defvar arxiv-keyword-matches-only nil
  "When set to 't', only abstracts that match a keyword appear")
(defvar arxiv-b-list nil
  "the secondary list in arxiv-reader, by default this is the
  list of files that have matches in the new abstract list")
(defvar arxiv-b-list-location 0
  "Start position when switching lists")
(defvar arxiv-showing-b-list nil
  "state variable to determine which list is being viewed")
(defvar arxiv-updates "*arXiv-updates*"
  "buffer to hold abstract auto-update information")
(defvar arxiv-default-subdir 1
  "default subdir number. becomes last subdir explicitly called")
(defvar arxiv-minor-mode-map
  (let ((map (make-sparse-keymap)))
;    (define-key map (kbd "d") 'symbol);mark for deletion)
    (define-key map (kbd "R") 'arxiv-move-to-subdir)
    (define-key map (kbd "P") 'arxiv-get-pdf)
    (define-key map (kbd "S") 'arxiv-swap-lists)
    (define-key map (kbd "H") 'arxiv-toggle-keyword-matches-only)
    (define-key map (kbd ".") 'arxiv-look-at-next-file)
    (define-key map (kbd ",") 'arxiv-look-at-previous-file)
    (define-key map (kbd ">") 'arxiv-increment-default-subdir)
    (define-key map (kbd "<") 'arxiv-decrement-default-subdir)
    (define-key map (kbd "C-c k") 
      (lambda ()
        (interactive)
        (customize-variable 'arxiv-keyword-list)))
    (define-key map (kbd "C-c a")
      (lambda ()
        (interactive)
        (customize-group 'arxiv)))
; these are for look-mode compatibility
    (define-key map (kbd "C-.") 'arxiv-look-at-next-file)
    (define-key map (kbd "C-,") 'arxiv-look-at-previous-file)
    (define-key map (kbd "M-n") 'arxiv-look-at-next-file)
    (define-key map (kbd "M-p") 'arxiv-look-at-previous-file)
    map)
  "Keymap for arXiv mode.")

(defmacro string<= (string1 string2)
  `(or (string< ,string1 ,string2) (string= ,string1 ,string2)))

(define-minor-mode arxiv-mode
  "a minor mode to read arXiv abstracts.  Defines keybindings to
  move files into subdirectories."
  :init-value nil ; maybe make this t?
  :lighter (:eval (if arxiv-keyword-matches-only " arXiv:H" " arXiv"))
  :keymap arxiv-minor-mode-map
  (dolist (arxiv-re arxiv-keyword-list)
    (highlight-regexp arxiv-re))
  (goto-address)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; interactive functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arxiv-read-abstracts ()
  "call arxiv-split-abstracts then start look mode with arxiv mode"
  (interactive)
  (arxiv-split-abstracts)
  (arxiv-look-at-abstracts)
  )

(defun arxiv-look-at-abstracts ()
  "start look mode for reading abstracts.  This is used when the abstracts
have already been split from the mail file."
  (interactive)
  (setq arxiv-b-list-location 0)
  (setq arxiv-b-list nil)
  (setq look-show-subdirs t)
  (setq arxiv-default-subdir 1)
  (setq look-hilight-subdir-index 1)
  ; skip dot-files and pdfs
  (add-to-list 'look-skip-file-list "pdf$")
  (add-to-list 'look-skip-file-list "^[#\\.,]")
  (add-to-list 'look-skip-directory-list "^[\\.,]")
  (look-at-files "")
  (arxiv-mode t)
  (pop look-skip-file-list)
  (pop look-skip-file-list)
  (pop look-skip-directory-list)
  (if (arxiv-detect-existing-abstracts)
      (progn
        (set-buffer look-buffer)
        (look-update-header-line)
        (look-at-this-file) (arxiv-mode t) ; takes care of bug when 1st abstract is an update
        (switch-to-buffer arxiv-updates)
        )
    (switch-to-buffer look-buffer)
    (kill-buffer arxiv-updates))
  )

(defun arxiv-look-at-next-file ()
  "calls look-at-next-file.  If arxiv-keyword-matches-only is
true, then skip over non-matching files."
  (interactive)
  (look-at-next-file)
  (if arxiv-keyword-matches-only
      (catch 'found-it
        (while (and (dolist (arxiv-re arxiv-keyword-list t)
                      (if (> (how-many arxiv-re 1 (point-max)) 0)
                          (throw 'found-it t)))
                    look-current-file)
          (look-at-next-file))
        (look-no-more)))
  (arxiv-mode t)
  )

(defun arxiv-look-at-previous-file ()
  "calls look-at-previous-file.  If arxiv-keyword-matches-only is
true, then skip over non-matching files."
  (interactive)
  (look-at-previous-file)
  (if arxiv-keyword-matches-only
      (catch 'found-it
        (while (and (dolist (arxiv-re arxiv-keyword-list t)
                      (if (> (how-many arxiv-re 1 (point-max)) 0)
                          (throw 'found-it t)))
                    look-current-file)
          (look-at-previous-file))
        (look-no-more)))
  (arxiv-mode t)
  )

(defun arxiv-increment-default-subdir ()
  "increment the default subdirectory"
  (interactive)
  (let ((n-subdirs (1- (length look-subdir-list))))
    (if (equal arxiv-default-subdir n-subdirs)
        (setq arxiv-default-subdir 1)
      (setq arxiv-default-subdir (1+ arxiv-default-subdir)))
    (setq look-hilight-subdir-index arxiv-default-subdir)
    (look-update-header-line)
    ) )

(defun arxiv-decrement-default-subdir ()
  "decrement the default subdirectory"
  (interactive)
  (let ((n-subdirs (1- (length look-subdir-list))))
    (if (equal arxiv-default-subdir 1)
        (setq arxiv-default-subdir n-subdirs)
      (setq arxiv-default-subdir (1- arxiv-default-subdir)))
    (setq look-hilight-subdir-index arxiv-default-subdir)
    (look-update-header-line)
    ) )

(defun arxiv-toggle-keyword-matches-only ()
  "toggles the state of arxiv-keyword-matches-only"
  (interactive)
  (setq arxiv-keyword-matches-only (not arxiv-keyword-matches-only))
  (princ (list "arxiv-keyword-matches-only" arxiv-keyword-matches-only))
  )

(defun arxiv-move-to-subdir (subdir-number)
  "Move current file into the subdir-number^th directory in
   look-subdir-list.  With no explicit prefix, it puts the file
   in the first listed subdirectory."
  (interactive "P"); nil if none specified
  (if subdir-number
      (if (> subdir-number 0)           ;0 moves file back to the top dir.
                                        ;which should never be default
          (progn
            (setq arxiv-default-subdir subdir-number)
            (setq look-hilight-subdir-index arxiv-default-subdir)
            ))
    (setq subdir-number arxiv-default-subdir))
  (let* ((arxiv-target-dir (nth subdir-number look-subdir-list))
         (arxiv-new-filename 
          (concat
           look-pwd
           arxiv-target-dir
           (file-name-nondirectory look-current-file)))
         (arxiv-current-pdf 
          (car (file-expand-wildcards (concat look-current-file "*pdf"))))
         (arxiv-new-pdf
          (concat
           look-pwd
           arxiv-target-dir
           (and arxiv-current-pdf ; protects agains nil argument
                (file-name-nondirectory arxiv-current-pdf))))
         arxiv-match-string
         )
    (if (not (file-exists-p arxiv-new-filename))
        (progn 
          ; increment category counts if saving from 0 to >0
          ; decrement category counts if saving from >0 to 0
          ;;; not yet implemented
          ; (if (and  (> subdir-number 0) (equal (file-name-directory look-current-file) look-pwd))
          ;    increment category counts)
          ; (if (and (= subdir-number 0) (not (equal (file-name-directory look-current-file) look-pwd)))
          ;     decrement category counts )
          ;;;
          ; incr/decr cats function needs: 
          ; current buffer (maybe better to put file into temp buffer to extract cats and then dump it)
          ;;;

          ; move the file to its new location
          (rename-file look-current-file arxiv-new-filename)
          ; move the pdf to the same dir if it exists
          (if arxiv-current-pdf
              (progn
                (rename-file arxiv-current-pdf arxiv-new-pdf)
                (princ (concat "Moved " look-current-file " and " 
                               (file-name-nondirectory arxiv-current-pdf)
                               " to " arxiv-target-dir))
                )
            (princ (concat "Moved " look-current-file " to " arxiv-target-dir))
          )
          (setq look-current-file arxiv-new-filename)
          (look-update-header-line)
          )
      ;see if  "replaced with ... \d+\w+)" appears in the existing file
      ;I'd like to do this more generally, but this works (for now)
      (beginning-of-buffer); the search is point-location sensitive
      (if (search-forward-regexp 
           "^\\(replaced [[:alnum:][:space:],:]+([[:alnum:],]+)\\)$" nil t)
          (progn 
            (setq arxiv-match-string (match-string 1))
            (beginning-of-buffer); for tidiness
            (switch-to-buffer "*arxiv-temp*")
            (insert-file-contents arxiv-new-filename)
            (if (search-forward arxiv-match-string nil t)
                (princ (concat look-current-file " has already been moved to "
                               arxiv-new-filename))
              (file-cat arxiv-new-filename "-----\n" look-current-file)
              (princ (concat "Appended " look-current-file " to "
                             arxiv-new-filename))
              )
            (kill-buffer "*arxiv-temp*")
            )
        (princ "File exists in subdirectory, but could not find a \"replaced with...\" string")
        ) ;fi
      ) ;fi
    ) ;tel
  ) ;nufed

(defun arxiv-get-pdf ()
  "download and display the pdf of the current file"
  (interactive)
  (beginning-of-buffer)
  ;  extract the first author name
  (search-forward-regexp 
;   "^Authors?: \\([[:alpha:]-'\"\\. ]+?\\)\\( ?(\\|,\\| et\\| and\\|$\\)")
   "^Authors?: \\(.*?\\)\\( ?(\\|,\\| et\\| and\\|$\\)")
  (beginning-of-buffer)
  (let* ( (first-author-name (match-string 1))
          first-initial
          last-name
          formatted-name
          (abstract-number (replace-regexp-in-string 
                            "\\(^arXiv:\\|\\.x$\\)" "" 
                            (file-name-nondirectory look-current-file)))
          (arxiv-url (concat "http://arxiv.org/pdf/" 
                             (replace-regexp-in-string
                              "\\([a-z]\\)\\([0-9]\\)" "\\1/\\2" ;reinsert the slash
                              abstract-number)))
          output-file output-file-deslashed
        )
    ;parse the first author name
    (string-match "^\\([A-Z]\\)" first-author-name)
    (setq first-initial (match-string 1 first-author-name))
    (string-match 
     ;   name prefixes                   |  last name  |   titles
     "\\(\\(\\(van\\|de\\)[[:space:]]+\\)?[[:alpha:]-~'\"\\]+\\([[:space:]]+[JS]r\\)?\\)\\.?$"
     first-author-name)
    (setq last-name (match-string 1 first-author-name))
    (setq formatted-name 
          (replace-regexp-in-string " " "_" (concat last-name first-initial)))
    (setq output-file (substring-no-properties
                       (concat (file-name-directory look-current-file)
                               "arXiv:" abstract-number "_" formatted-name ".pdf")))
    ; get the pdf and change the abstract file name
    (setq output-file-deslashed (replace-regexp-in-string "\\\\" "" output-file))
    (if (file-regular-p output-file-deslashed)
        (princ (concat output-file " already exists"))
      ; get the file
      (shell-command (concat "curl -o " output-file " " arxiv-url))
      )
    ; (princ (concat last-name first-initial))
    (if (string-match "Emacs 23" (emacs-version))    
        (find-file-read-only output-file-deslashed))
    ; in emacs 22, doc-view fails if the filename arg has a ~ AND the file is not in the cache!
    )
  )

(defun arxiv-swap-lists ()
  "swap between viewing the look-mode list and the arxiv-b-list"
  (interactive)
  (setq arxiv-showing-b-list (not arxiv-showing-b-list))
  (let* ((new-b-list (nconc (reverse look-reverse-file-list)
                            (list look-current-file);this can produce a (nil) that needs to be zapped
                            look-forward-file-list))
         (new-b-list-location (length look-reverse-file-list))
         (reverse-b-list (reverse new-b-list))
         )
    (if (not (car new-b-list))
        (pop new-b-list) ; zap leading nils
      (unless (car reverse-b-list) 
          (pop reverse-b-list) ;zap trailing nils
          (setq new-b-list (reverse reverse-b-list))))

    (setq look-forward-file-list (nthcdr arxiv-b-list-location arxiv-b-list))
    (if (> arxiv-b-list-location 0)
        (setq look-current-file (nth (1- arxiv-b-list-location) arxiv-b-list))
      (setq look-current-file nil))
    (if (> arxiv-b-list-location 1)
        (progn
          (setcdr (nthcdr (- arxiv-b-list-location 2) arxiv-b-list) nil)
          (setq look-reverse-file-list (reverse arxiv-b-list))
          )
      (setq look-reverse-file-list nil))

    (setq arxiv-b-list new-b-list)
    (setq arxiv-b-list-location new-b-list-location)

    (arxiv-look-at-next-file)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subroutines (non interactive defun's) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun arxiv-split-abstracts (&optional filename)
  "split an arXiv email into separate abstracts
taken shamelessly from Hubert Chen's 'breakup' perl script
that does exactly the same thing."
  (setq filename (dired-get-file-for-visit))
  (switch-to-buffer "*arxiv-extract-abstracts*")
  (insert-file-contents filename)
  (while (re-search-forward "\\(^arXiv:[-./[:alnum:]]+\\)" nil t)
    (setq abs-filename
          (replace-regexp-in-string "/" "" (match-string 0)))
    (move-beginning-of-line 1)
    (setq abs-start (point))
    (search-forward "\\ (" )
    (move-beginning-of-line 2)
    (setq abs-end (point))
    (write-region abs-start abs-end abs-filename nil nil nil)
    )
  (kill-buffer "*arxiv-extract-abstracts*")
  )  

(defun arxiv-detect-existing-abstracts ()
  "Detect abstracts to append and duplicates. Hard coded to
operate at end of arxiv-look-at-files.  Returns t if anything
matches.  Removes those files from look-forward-file-list"
  (let ((arxiv-new-abstract-list (nconc (list look-current-file)
                                        look-forward-file-list))
        (arxiv-saved-abstract-list nil)
        (found-matches-p nil)
        (look-forward-file-list-recon nil))
    (get-buffer-create arxiv-updates)
    (switch-to-buffer arxiv-updates)
    (setq header-line-format 
          "Updated and duplicated abstracts. Type \"C-x b\" to continue with arXiv-reader.")
    ;create list arxiv-saved-abstract-list sorted by abstract name
    (setq arxiv-saved-abstract-list 
          (sort (dolist (arxiv-subdir (cdr look-subdir-list) arxiv-saved-abstract-list)
                  (setq arxiv-saved-abstract-list
                        (nconc arxiv-saved-abstract-list
                               (directory-files-recursive arxiv-subdir "ar*[0-9]"))))
                (lambda (x y)
                  (string< (file-name-nondirectory x)
                           (file-name-nondirectory y)))))
    ; check arxiv-new-abstract-list against arxiv-saved-abstract-list
    ; append matches to existing files or flag prior existence
    (dolist (arxiv-new-abstract arxiv-new-abstract-list)
      (catch 'next-abstract
        (while (and arxiv-saved-abstract-list
                    (string<= (file-name-nondirectory (car arxiv-saved-abstract-list))
                              (file-name-nondirectory arxiv-new-abstract)))
          (setq arxiv-saved-abstract (pop arxiv-saved-abstract-list))
          (if (string= (file-name-nondirectory arxiv-saved-abstract) 
                       (file-name-nondirectory arxiv-new-abstract))
              (progn
                ; report match results in arxiv-updates buffer
                (insert (arxiv-append-file arxiv-new-abstract arxiv-saved-abstract) "\n")
                (setq arxiv-b-list (nconc arxiv-b-list
                                          (list arxiv-saved-abstract)))
                (setq found-matches-p t)
                (throw 'next-abstract nil))
            )
          )
        (setq look-forward-file-list-recon 
              (nconc look-forward-file-list-recon (list arxiv-new-abstract)))
        )
      )
    (setq look-current-file (pop look-forward-file-list-recon))
    (setq look-forward-file-list look-forward-file-list-recon)
    found-matches-p
    )
  )

(defun arxiv-append-file (file1 file2)
  "Append file1 to file2 if it has not already been appended.
Returns a string indicating the outcome of the function"
  ; outcomes: 1. file1 does not have a "replaced with..." string --> nil
  ;           2. file2 has file1's "replaced with..." string --> nil
  ;           3. file2 doesnt have file1's "replaced with..." string --> append,t
  (let ((filename1 (file-name-nondirectory file1))
        return-value
        replaced-with-string
        )
    (switch-to-buffer "*arxiv-temp*")
    (insert-file-contents file1)
    (if (search-forward-regexp 
         "^\\(replaced [[:alnum:][:space:],:]+([[:alnum:],]+)\\)$" nil t)
        (progn
          (setq replaced-with-string (match-string 1))
          (kill-buffer "*arxiv-temp*") (switch-to-buffer "*arxiv-temp*")
          (insert-file-contents file2)
          (if (search-forward replaced-with-string nil t)
              ;outcome 2
              (setq return-value
                    (concat filename1 ": no action, already incorporated into " file2))
            ;outcome 3
            (file-cat file2 "-----\n" file1)
            (setq return-value (concat filename1 ": appended to " file2))
            )
          )
      ; outcome 1
      (setq return-value
            (concat filename1 ": not an update--already saved as " file2))
      )
    (kill-buffer "*arxiv-temp*")
    return-value
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generally useful subroutines ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun directory-files-recursive (dfr-dir dfr-wildcard)
  "recursively list regular files that match dfr-wildcard under dfr-dir"
 ;this is potentially a stand-alone function
  (let ((recursive-file-list nil)
        dfr-file)
    (dolist (dfr-file (directory-files dfr-dir t) recursive-file-list)
      (if (and (file-directory-p dfr-file)
               (not (string-match "^\.\.?$" (file-name-nondirectory dfr-file)))
               )
          (setq recursive-file-list
                (nconc recursive-file-list
                       (directory-files-recursive dfr-file dfr-wildcard)))
        (if (and (file-regular-p dfr-file)
                 (string-match (wildcard-to-regexp dfr-wildcard) 
                               (file-name-nondirectory dfr-file)))
            (setq recursive-file-list 
                  (nconc recursive-file-list (list dfr-file))))
        ) ) ) )

(defun file-cat (file1 &rest file-or-string)
  "concatenate files or strings to a file
replaces calls to 'cat <file-or-string> >> <file1>'"
  (switch-to-buffer "*temptemp*")
  (dolist (item file-or-string)
    (if (file-regular-p item)
        (progn
          (insert-file-contents item)
          (goto-char (point-max))
          )
      (insert item)
      )
    )
  (append-to-file 1 (point-max) file1)
  (kill-buffer "*temptemp*")
)

(provide 'arxiv-mode)

;;; arxiv-reader.el ends here
