;;; ell.el --- Browse the Emacs Lisp List
;; Author: Jean-Philippe Theberge (jphil@emacslisp.org)
;;         Stephen Eglen (stephen@cogsci.ed.ac.uk)
;;         Nascif A. Abousalh Neto (nascif@acm.org)
;; Created: 2000-05-22 - last update: Mon 14 Aug 2006
;; Version: 1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; Ell.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; Ell.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;;  The Emacs Lisp Lisp is available at
;;  http://www.damtp.cam.ac.uk/user/sje30/emacs/ell.html
;;
;;  If Stephen changes the layout of his web page, this package may
;;  stop to work correctly.  You may then need to upgrade this
;;  package.

;; After loading this file, just do "M-x ell-packages" to view the
;; list in its own buffer.
;; Use a prefix argument (i.e. "C-u M-x ell-packages") to sort by
;; author name.

;; (New in 1.1)

;; Retrieves information from ell.xml instead of ell.html. Old method relied on
;; regular expressions for parsing and was skipping some entries.

;; (New in 1.0)

;; Packages added to the ELL since the last time you called
;; "ell-packages" will be marked with a tag ("<New>").  This is
;; achieved by storing relevant information about the last time you
;; accessed ELL in a file.  If you delete this file, the next time you
;; access the ELL, all files will be marked as new once more.  If
;; ell-occur-if-new-found is t, an *Occur* buffer will automatically
;; show you the new entries.
;;
;; The elib package is required for cache management (using the
;; avltree facility).  If you do not have elib, get it from any GNU
;; mirror, such as
;; ftp://wuarchive.wustl.edu/systems/gnu/emacs/elib-1.0.tar.gz
;; 
;; Variables.
;; 
;; Set ell-locate to t (default nil) if you want emacs to
;; indicate which lisp files are already available on your system.
;;
;; Set ell-goto-addr to t (default nil) if you want to turn the
;; URLs into hyperlinks using the goto-addr package.
;;
;; Set ell-last-read-filename to the name of the file where you want
;; to store information from the last time the ELL site was accessed.

;; To Do:
;;
;; + Do the http fetching in the background so emacs is not
;;   freezed on slow connections
;;
;; + Take consideration for the accented character in the sort by
;;   author.
;;
;; + replace sort* with something else so the need for cl.el is
;;   no more required. (is this really necessary?)
;;
;; + Highlight packages already somewhere on your local lisp path.

;;; Code:

(require 'cl)                           ;needed for `sort*' routine.
(require 'avltree)                      ;from elib, needed for cache management
(require 'xml)                           ;needed for `sort*' routine.

(defvar ell-host "www.damtp.cam.ac.uk")
(defvar ell-path "user/sje30/emacs/ell.xml")

(defvar ell-proxy-host nil
  "*If nil dont use proxy, else name of proxy server.")

(defvar ell-proxy-port nil
  "*Port number of proxy server.  Default is 80.")

(defvar ell-locate nil
  "*Non-nil if we want to test whether package is available on local system.
This will considerably slow down viewing of this buffer.")

(defvar ell-occur-if-new-found t
  "*Non-nil if we want to activate an *Occur* buffer listing new packages.")

(defvar ell-goto-addr nil
  "*Non-nil if we want to use turn URLs into hyperlinks.
If nil, you may want to use another package, such as ffap, instead.
\(This feature may not be available in XEmacs.)")

(defvar ell-last-updated nil
  "Date that the list was last updated.")

(defvar ell-use-font-lock t
  "*If non-nil, we font-lock the ELL buffer.")

(defvar ell-last-read-filename "~/.ell-last-read"
  "File where information about the last known state of the ELL is stored.")

(if ell-goto-addr
    (require 'goto-addr))

(defun ell-read-from-file (filename)
  "Read a generic Lisp object from FILENAME."
  (if (file-readable-p filename)
      (with-temp-buffer
        (insert-file-contents-literally filename)
        (read (current-buffer)))))

(defun ell-new-cache ()
  "Create a new cache entry."
(avltree-create (lambda (package1 package2)
                    (and (string< (car package1) (car package2))
                         (string< (cdr package1) (cdr package2))))))

(defun ell-read-cache-from-file (filename)
  "Return a package cache from FILENAME, or a new one if none was found."
  (let ((previous-cache (ell-read-from-file filename)))
    (if (or (null previous-cache) (not (avltree-p previous-cache)))
        (ell-new-cache)
      previous-cache)))

(defun ell-write-to-file (object filename)
  "Writes a generic Lisp OBJECT to FILENAME."
  (if (file-writable-p filename)
      (with-temp-file filename
        (print object (current-buffer)))))

(defun ell-write-cache-to-file (new-cache)
  "Write NEW-CACHE cache to disk, created from the HTML page we just read."
  (ell-write-to-file new-cache ell-last-read-filename)
)

;; defvars to keep the byte compiler quiet.
(defvar ell-ref1)
(defvar ell-msg)
(defvar ell-dstr)
(defvar ell-font-lock-keywords)

(eval-and-compile
  (condition-case nil
      (require 'working)
    (error
     (progn
       (defmacro working-status-forms (message donestr &rest forms)
         "Contain a block of code during which a working status is shown."
         (list 'let (list (list 'ell-msg message) (list 'ell-dstr donestr)
                          '(ell-ref1 0))
               (cons 'progn forms)))
  
       (defun working-status (&optional percent &rest args)
         "Called within the macro `working-status-forms', show the status."
         (message "%s%s" (apply 'format ell-msg args)
                  (if (eq percent t) (concat "... " ell-dstr)
                    (format "... %3d%%"
                            (or percent
                                (floor (* 100.0 (/ (float (point))
                                                   (point-max)))))))))
  
       (defun working-dynamic-status (&optional number &rest args)
         "Called within the macro `working-status-forms', show the status."
         (message "%s%s" (apply 'format ell-msg args)
                  (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% ell-ref1 4))))
         (setq ell-ref1 (1+ ell-ref1)))
  
       (put 'working-status-forms 'lisp-indent-function 2)))))


(defun ell-packages-list (&optional byauthor)
  "Insert the contents of URL at point.
Optional argument BYAUTHOR determines whether we should sort by author."
  (if (get-buffer "*ell-temp-buffer*")
      (kill-buffer "*ell-temp-buffer*"))
  (with-temp-buffer
    (let* ((host ell-host)
           (path ell-path)
           (coding-system-for-read 'binary)
           (coding-system-for-write 'binary)
           (http (open-network-stream
                  "ell-retrieval-process"
                  "*ell-temp-buffer*"
                  (if ell-proxy-host ell-proxy-host host)
                  (if ell-proxy-port ell-proxy-port 80)))
           (pbuf (process-buffer http)))
      (process-send-string
       http (concat
             "GET "
             (if ell-proxy-host
                 (concat "http://" ell-host "/")
               "/")
             path " HTTP/1.0\r\n\r\n"))

      (working-status-forms "Retrieving ell.xml" "done"
        (while (eq (process-status http) 'open)
          (working-dynamic-status nil)
          ;;(working-dynamic-status (buffer-size pbuf))
          (sleep-for 1)
          ))
      (insert-buffer-substring pbuf)
      (kill-buffer pbuf)

      (ell-build-packages-list))))

(defun ell-build-packages-list ()
  "parses the contents of the current buffer, which is expected to contain the downloaded contents of the ELL site ell.xml file"
  (ell-fix-for-xml-parser-bug)
  (goto-char (point-min))
  (let* ((xml (xml-parse-region (point-min) (point-max) (current-buffer)))
         (ell-root (car xml))
         (ell-entries (cadddr ell-root)))
    
      (setq ell-last-updated (nth 2 (caddr ell-root)))

      (mapcar '(lambda (entry) (let ((attrs (cadr entry))) 
                                 (list 
                                  (cdr (assoc 'site attrs))
                                  (cdr (assoc 'filename attrs))
                                  (cdr (assoc 'description attrs))
                                  (cdr (assoc 'contact attrs))
                                  )))  (cddr ell-entries))))

(defun ell-sort-by-author (packagesL)
  "Auxiliary routine to sort PACKAGESL by author."
  (sort*  (mapcar (lambda (x)
                    (let ((authorl (split-string (car (last x)))))
                      (list (car x)(cadr x)(caddr x)(cadddr x)(car (last authorl)))))
                  packagesL)
          'string-lessp
          :key #'(lambda (x) (car (last x)))))

(define-derived-mode ell-mode view-mode "Ell"
  "Major mode to display the Emacs lisp list.
Special commands:
\\{ellmode-map}"

  (if ell-use-font-lock
      (progn
	(setq ell-font-lock-keywords
	      (list
	       '(" <New> "  0  font-lock-warning-face)
	       '("^\\(.*\\.el\\) " 1 font-lock-keyword-face)
	       '("^\\(ht\\|f\\)tp.*$" . font-lock-comment-face)
	       )
	      )
	(make-local-variable 'font-lock-defaults)
	(setq font-lock-defaults '(ell-font-lock-keywords nil t)))
    ))

(defun ell-prepare-buffer ()
  "Prepare to make the new *ell-packages* buffer."
  (if (get-buffer "*ell-packages*")
      (kill-buffer "*ell-packages*"))
  (switch-to-buffer "*ell-packages*")
  (insert "==========================================")
  (center-line)(insert "\n")
  (insert "The Emacs Lisp List")(center-line)(insert "\n")
  (insert "by Stephen Eglen: stephen@gnu.org")(center-line)(insert "\n")
  (insert "==========================================")
  (center-line)(insert "\n\n")
)

(defun ell-update-buffer (ell-last-updated new-count)
  "Update the counters at the top of the *ell-packages* buffer.
ELL-LAST-UPDATED is the date when ELL was last updated.
NEW-COUNT is the number of new entries."
  (if ell-last-updated
      (progn
        (goto-line 4)                   ;naughty...
        (insert (concat "Last updated: " ell-last-updated "\n"))
        (forward-line -1)   (center-line)))
  (if (> new-count 0)
      (progn
        (goto-line 4)                   ;ditto...
        (insert (format "Number of new entries: %d\n" new-count))
        (forward-line -1)   (center-line)))
)

;;;###autoload
(defun ell-packages (byauthor)
  "Display the Emacs Lisp list in a Emacs buffer.
If BYAUTHOR is true, we sort the list by author name."
  (interactive "P")
  (let ((packagesL (ell-packages-list))
        (cache (ell-read-cache-from-file ell-last-read-filename))
        (new-cache (ell-new-cache))
        (new-count 0))
    (ell-prepare-buffer)
    (if ell-locate
        (insert "Note: Files with an asterisk (*) \
are already installed on your system.\n\n"))
    (mapcar (lambda (x)
              ;; name - description - (by author)
              ;; URL
              (let* ((url (car x))
                     (name (cadr x))
                     (description (car (cdr (cdr x))))
                     (author (car (cdr (cdr (cdr x)))))
                     (package (cons name author)))
                (avltree-enter new-cache package)
                (insert (format "%s %s- %s (by %s)\n%s\n\n"
                                (if (and ell-locate (locate-library name))
                                    (concat "*" name)
                                  name)
                                (if (avltree-member cache package)
                                     ""
                                  (progn (setq new-count (1+ new-count)) "<New> "))
                                description author url))))
            (if byauthor
                (ell-sort-by-author packagesL)
;;              (reverse packagesL)))
              packagesL))
    (ell-write-cache-to-file new-cache)
    (ell-update-buffer ell-last-updated new-count)
    (ell-mode)
    (if ell-goto-addr
        ;; ELL is a big file, so ensure the maximum size for fontifying
        ;; addresses is okay.
        (progn
          (set (make-local-variable 'goto-address-fontify-maximum-size)
               (+ 10 (buffer-size)))
          (goto-address)))
    (goto-char (point-min))
    (if ell-use-font-lock
	(font-lock-fontify-buffer))
    (if (and ell-occur-if-new-found (> new-count 0))
        (occur "<New>"))))


(defun ell-fix-for-xml-parser-bug ()
  "current version of xml.el can't deal with empty strings. Not a problem for us, so let's just remove them"
  (goto-char (point-min))
  (while (search-forward "note=\"\"" nil t)
    (replace-match "" nil t))
)


(provide 'ell)
  
;;; ell.el ends here
