;;; extview.el --- open files with external viewer

;; Copyright (C) 2006 Tamas Patrovics

;; $Date: 2006-04-04 12:58:37 $

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Load the library and try opening a file which has an associated
;; application in `extview-application-associations' or in mailcap.
;; 
;; `extview-application-associations' can be used to override mailcap
;; handlers:
;;
;;   (require 'extview)
;;   (push '("\\.pdf$" . "acroread %s") extview-application-associations)
;;   (push '("\\.py$" . nil) extview-application-associations)
;;   (push '("\\.html$" . ask) extview-application-associations)
;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'mailcap)

(mailcap-parse-mailcaps)

(defvar extview-application-associations nil
  "List of (REGEXP . HANDLER) descriptors which are tested for the
file name to be opened. The first matching handler is used to open
the file. If no handler matches then an attempt is made to use
mailcap to open the file.

If no handler is found normal `find-file' is executed.

If HANDLER is nil then the file is opened normally regardless if
it has a mailcap entry.

If HANDLER is the symbol `ask' then the user has to decide whether the
file is to be opened in emacs or with an external viewer.

This variable is normally used to override mailcap handlers.")


(defadvice find-file (around extview-find-file 
                             (filename &optional wildcards) activate)
  "Around advice for find-file which checks if the file should be
opened with an external viewer instead of Emacs."
  (interactive "FFind file: \np")
  ;; check `extview-application-associations' first, since it has
  ;; priority
  (let ((handler (some (lambda (descriptor)
                         (if (string-match (car descriptor) filename)
                             descriptor))
                       extview-application-associations)))
    (if (and handler
             (not (and (eq 'ask (cdr handler))
                       (y-or-n-p "Open with external viewer? "))))
        (if (eq (cdr handler) 'ask)     ; open with emacs
            (setq handler nil)
          (setq handler (cdr handler)))

      ;; check if there is an appropriate mailcap entry
      (let* ((ext (file-name-extension filename))
             (mime (if ext 
                       (mailcap-extension-to-mime ext))))
        (if mime
            (setq handler (mailcap-mime-info mime)))

        ;; only string handlers are considered, since we're interested
        ;; in external viewers, not emacs functions
        (unless (stringp handler)
          (setq handler nil))))

    ;; call the handler if found
    (if handler
        (let* ((logbuffer "*extview log*")
               (components (split-string handler))
               ;; hopefully the first component is always the
               ;; application name
               (app (car components))
               ;; substitute file name for %s args
               (args (mapcar (lambda (arg)
                               (if (or (equal "%s" arg)
                                       (equal "'%s'" arg))
                                   (expand-file-name filename)
                                 arg))
                             (cdr components))))
          
          (get-buffer-create logbuffer)
          (with-current-buffer logbuffer
            (goto-char (point-max))
            (insert (format "Opening file %s with handler: %s"
                            filename handler)
                    "\n"))

          (apply 'start-process "extview-process" logbuffer app args)          

          (message (concat "File is opened with an external viewer. "
                           "See buffer %s for status messages.")
                   logbuffer))
                         
      ad-do-it)))

(provide 'extview)
;;; extview.el ends here
