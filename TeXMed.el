;;; TeXMed.el - Query and retrieve BibTeX from NCBI pubmed via TeXMed in Emacs
;;
;;; AUTHOR:  Emanuel Heitlinger <emanuelheitlinger@gmail.com>
;; LICENCE: GPL2
;;
;;; Commentary:
;;
;; Based on: TeXMed - http://www.bioinformatics.org/texmed/
;; an interface to NCBI PubMed http://www.ncbi.nlm.nih.gov, 
;; that allows you to query PubMed and to store references in BibTeX format. 
;; by Arne Muller
;;
;; This mode adds some convenience functions to the w3m-buffers opened
;; searching on TexMed
;;
;; How to install:
;; 1). Download this file and put in your emacs's load-path
;; 2). Put the following in your .emacs:
;;     (require 'TeXMed)
;;     (global-set-key "\C-ct" 'TeXMed-search)
;;
;; DEPENDENCIES: 
;; 1). emacs-w3m http://emacs-w3m.namazu.org/ 
;;
;; How to use:
;; 1). Type C-ct to start a query (that's press Control and c together then
;;     t alone)
;; 2). Presented with the results of the query you have these options:
;;     a) Type C-cea to export all results to a bibtex-file
;;     b) Type C-cel to go through results and choose one by one
;;         + If you do this till the last entry your selection will be 
;;           exported automatically
;;         + If you are satisfied before the end abort with C-g an goto 3.)
;; 3). Type C-cee to export a selection
;; 4). To generally include abstracts or article links you can put the
;;     following into your .emacs:
;;     (setq  TeXMed-include-abstract t)
;;     (setq  TeXMed-include-article-id t)
;; 5). To set the folder in which TeXMed results are stored you can put
;;     (setq TeXMed-bibtex-folder "~/bibtex/")
;      in your .emacs (the default is your homedir)
;;
;; Only tested with Gnu-Emacs 23.1.1 on Linux.

;; This is my very first minor mode for Emacs:
;; It probabely has many bugs, uses maybe bad elisp
;; and fails following some of the conventions

;; load dependencies
(require 'w3m-search)

;; Add the serch TeXMed search engine
(add-to-list 'w3m-search-engine-alist '("TeXMed" "http://www.bioinformatics.org/texmed/cgi-bin/query.cgi?query=%s"))

;; Local keybindings
(defgroup TeXMed nil "TeXMed: retrieve bibtex from pubmed" :group 'Tex)
(defvar TeXMed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cea" 'TeXMed-export-all)
    (define-key map "\C-cel" 'TeXMed-ask-loop)
    (define-key map "\C-cee" 'TeXMed-export)
    map))

;; The variables 
(defvar TeXMed-search-history nil "Stores the queries used in TeXMed")
(defvar TeXMed-include-article-id nil "Non nil means store article id field is ticked before export")
(defvar TeXMed-include-abstract nil "Non nil means include abstract field is ticked before export")
(defvar TeXMed-bibtex-folder "~/" "The folder in which TeXMed exports are stored")
(defvar TeXMed-current-date-time-format "%a %b %d %Y %H:%M:%S %Z"
  "Format of date to insert with `insert-current-date-time' func
for possible replacements in TeXMed export files. See also help
of `format-time-string'")

;; global functions Texmed search 
(defun TeXMed-search ()
  "Search for a querry you are prompted for on TeXMed,
an online-service, which allows retieval of bibtex from
pubmed"
  (interactive)
  (let ((query          
         (read-from-minibuffer "TeXMed search: " nil nil nil 'TeXMed-search-history (thing-at-point 'word))))
    (w3m-search-do-search 'w3m-goto-url "TeXMed" query)
    (if (string-match "^$" query) ; user entered empty string
        (TeXMed-search) ; search again 
      (progn ; else go agead
        (TeXMed-mode t) 
        (add-to-list 'TeXMed-search-history query)))))

(defun TeXMed-tick-field (proceeding)
  "Tick the field proceeding the argument"
  (beginning-of-buffer)
  (while (w3m-form-goto-next-field)
    (when (looking-at proceeding)
      (w3m-view-this-url))))

(defun TeXMed-export ()
  "Export the entries marked in TeXMed's w3m buffer."
  (interactive)
  (beginning-of-buffer)
  (while (w3m-form-goto-next-field)
    (if (looking-at "*]PMID")
        (progn                 ; at least one field is ticked go on!
          (when TeXMed-include-article-id (TeXMed-tick-field  " ] link article ids"))
          (when TeXMed-include-abstract   (TeXMed-tick-field  " ] incl. abstract"))
          (TeXMed-tick-field "\\[export]")
          (run-at-time "0.1 sec" 0.1 'TeXMed-to-file))  ; after 0.1s check every 0.1s whether the export already succeded  
      (message "mark at least one entry to export"))))

(defun TeXMed-to-file ()
  (when (and (string-match "@Article" (buffer-string))  ; wait for w3m 
             (string-equal "w3m-mode" major-mode))      ; to finish exporting
    (write-file (concat TeXMed-bibtex-folder "TeXMed_search_" 
                        (car TeXMed-search-history) ".bib"))
    (delete-region (point-min) (search-forward "trying to export"))
    (insert "%")
    (insert (format-time-string TeXMed-current-date-time-format (current-time)))
    (insert ": TeXMed exported")
    (bibtex-mode)
    (write-file (buffer-file-name))
    ))

(defun TeXMed-ask-loop ()
  "Go through entries found on TexMed 
and ask wether to export to a BibTeX file, 
export the chosen"
  (interactive)
  (beginning-of-buffer)
  (while (w3m-form-goto-next-field)
    (when (looking-at " ]PMID")
      (if(y-or-n-p 
          (concat "export entry " 
                  (buffer-substring-no-properties (search-backward ".") (+ 1 (search-backward "\n")))
                  " ?"))
          (progn     ; additional next field because regex search above moves point
            (w3m-form-goto-next-field)
            (w3m-view-this-url))
        (w3m-form-goto-next-field))))
  (TeXMed-export))

(defun TeXMed-export-all ()
  "Export all the entries found on TexMed to a BibTeX file"
  (interactive)
  (TeXMed-tick-field  " ]PMID")
  (TeXMed-export))

(define-minor-mode TeXMed-mode
  "Toggle TeXMed mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."     
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " TeXMed")

(provide 'TeXMed)

;;; TeXMed.el ends here.
