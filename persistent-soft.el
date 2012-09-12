;;; persistent-soft.el --- Persistent storage, returning nil on failure
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/persistent-soft
;; URL: http://raw.github.com/rolandwalker/persistent-soft/master/persistent-soft.el
;; Version: 0.8.1
;; Last-Updated: 27 Aug 2012
;; EmacsWiki: PersistentSoft
;; Keywords: data, extensions
;; Package-Requires: ((pcache "0.2.3"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'persistent-soft)
;;     (persistent-soft-store 'hundred 100 "mydatastore")
;;     (persistent-soft-fetch 'hundred "mydatastore")    ; 100
;;     (persistent-soft-fetch 'thousand "mydatastore")   ; nil
;;
;;     quit and restart Emacs
;;
;;     (persistent-soft-fetch 'hundred "mydatastore")    ; 100
;;
;; Explanation
;;
;; This is a (trivial) wrapper around pcache.el, providing "soft"
;; fetch and store routines which never throw an error, but instead
;; return nil on failure.
;;
;; There is no end-user interface for this library.  It is only
;; useful from other Lisp code.
;;
;; The following functions are provided
;;
;;     `persistent-soft-store'
;;     `persistent-soft-fetch'
;;     `persistent-soft-exists-p'
;;     `persistent-soft-flush'
;;
;; To use persistent-soft, place the persistent-soft.el library
;; somewhere Emacs can find it, and add the following to your
;; ~/.emacs file:
;;
;;     (require 'persistent-soft)
;;
;; See Also
;;
;;     M-x customize-group RET persistent-soft RET
;;
;; Notes
;;
;;     Using pcache with a more recent version of CEDET gives
;;
;;         Unsafe call to `eieio-persistent-read'.
;;         eieio-persistent-read: Wrong type argument: class-p, nil
;;
;;     This library provides something of a workaround.
;;
;; Compatibility and Requirements
;;
;;     Tested on GNU Emacs versions 23.3 and 24.1
;;
;;     Uses if present: pcache.el (all operations are noops when
;;     not present)
;;
;; Bugs
;;
;;     Persistent-soft is a wrapper around pcache which is a wrapper
;;     around eieio.  Therefore, persistent-soft should probably be
;;     rewritten to use eieio directly or recast as a patch to pcache.
;;
;; TODO
;;
;;     Notice and delete old data files.
;;
;;; License
;;
;; Simplified BSD License:
;;
;; Redistribution and use in source and binary forms, with or
;; without modification, are permitted provided that the following
;; conditions are met:
;;
;;    1. Redistributions of source code must retain the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; This software is provided by Roland Walker "AS IS" and any express
;; or implied warranties, including, but not limited to, the implied
;; warranties of merchantability and fitness for a particular
;; purpose are disclaimed.  In no event shall Roland Walker or
;; contributors be liable for any direct, indirect, incidental,
;; special, exemplary, or consequential damages (including, but not
;; limited to, procurement of substitute goods or services; loss of
;; use, data, or profits; or business interruption) however caused
;; and on any theory of liability, whether in contract, strict
;; liability, or tort (including negligence or otherwise) arising in
;; any way out of the use of this software, even if advised of the
;; possibility of such damage.
;;
;; The views and conclusions contained in the software and
;; documentation are those of the authors and should not be
;; interpreted as representing official policies, either expressed
;; or implied, of Roland Walker.
;;
;;; Code:
;;

;;; requires

;; for callf, flet
(eval-when-compile
  (require 'cl))

(require 'pcache nil t)

(declare-function pcache-get         "pcache.el")
(declare-function pcache-has         "pcache.el")
(declare-function pcache-put         "pcache.el")
(declare-function pcache-repository  "pcache.el")
(declare-function pcache-save        "pcache.el")

;;; customizable variables

;;;###autoload
(defgroup persistent-soft nil
  "Persistent storage, returning nil on failure."
  :version "0.8.1"
  :link '(emacs-commentary-link "persistent-soft")
  :prefix "persistent-soft-"
  :group 'extensions)

(defcustom persistent-soft-default-expiration-days 90
  "Number of days to keep on-disk cache data unless otherwise specified."
  :type 'number
  :group 'persistent-soft)

;;; utility functions

;;;###autoload
(defun persistent-soft-exists-p (symbol location)
  "Return t if SYMBOL exists in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error."
  (when (and (featurep 'pcache)
             (stringp location))
    (let ((repo (ignore-errors
                  (flet ((message (&rest args) t))
                    (pcache-repository location)))))
      (when (and repo (ignore-errors
                        (flet ((message (&rest args) t))
                          (pcache-has repo symbol))))
        t))))

;;;###autoload
(defun persistent-soft-fetch (symbol location)
  "Return the value for SYMBOL in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error."
  (when (and (featurep 'pcache)
             (stringp location))
    (let ((repo (ignore-errors
                  (flet ((message (&rest args) t))
                    (pcache-repository location)))))
      (and repo (ignore-errors
                  (flet ((message (&rest args) t))
                    (pcache-get repo symbol)))))))

;;;###autoload
(defun persistent-soft-flush (location)
  "Flush data for the LOCATION data store to disk."
  (when (and (featurep 'pcache)
             (stringp location))
    (let ((repo (ignore-errors
                  (flet ((message (&rest args) t))
                    (pcache-repository location)))))
      (when (and repo (ignore-errors
                        (flet ((message (&rest args) t))
                          (pcache-save repo 'force))))
        t))))

;;;###autoload
(defun persistent-soft-store (symbol value location &optional expiration)
  "Under SYMBOL, store VALUE in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Optional EXPIRATION sets an expiry times in seconds.

Returns a true value if storage was successful.  Returns nil
on failure, without throwing an error."
  (when (and (featurep 'pcache)
             (stringp location))
    (callf or expiration (round (* 60 60 24 persistent-soft-default-expiration-days)))
    (let ((repo (ignore-errors
                (flet ((message (&rest args) t))
                    (pcache-repository location)))))
      (and repo (ignore-errors
                (flet ((message (&rest args) t))
                    (pcache-put repo symbol value expiration)))))))

(provide 'persistent-soft)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; End:
;;
;; LocalWords:  pcache eieio callf
;;

;;; persistent-soft.el ends here
