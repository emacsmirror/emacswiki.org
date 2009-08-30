;;; anything-kyr-config.el --- Configuration of anything-kyr.el
;; $Id: anything-kyr-config.el,v 1.6 2009/02/21 20:04:25 rubikitch Exp rubikitch $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, convenience
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-kyr-config.el

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
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Configuration of anything-kyr.el .

;;; History:

;; $Log: anything-kyr-config.el,v $
;; Revision 1.6  2009/02/21 20:04:25  rubikitch
;; config for linkd.
;;
;; Revision 1.5  2009/02/20 00:41:58  rubikitch
;; config for trace.el
;;
;; Revision 1.4  2009/02/19 10:26:23  rubikitch
;; highlight.el and transient-mark-mode config
;;
;; Revision 1.3  2009/02/19 10:06:34  rubikitch
;; config for highlight.el
;;
;; Revision 1.2  2009/02/18 10:19:54  rubikitch
;; Commands are now symbols.
;;
;; Revision 1.1  2009/02/18 10:09:40  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-kyr-config-version "$Id: anything-kyr-config.el,v 1.6 2009/02/21 20:04:25 rubikitch Exp rubikitch $")
(require 'anything-kyr)
(setq anything-kyr-commands-by-condition
      '(;; linkd.el
        ;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")
        (;; First element is a condition sexp.
         (and (featurep 'linkd)
              (equal (buffer-substring (point-at-bol) (+ (point-at-bol)
                                                         (length comment-start)))
                     comment-start))
         ;; The rest elements are COMMAND or (COMMAND . DESCRIPTION)
         (linkd-insert-star . "(@* ... )")
         (linkd-insert-tag  . "(@> ...)")
         linkd-insert-link)
        ;; multiverse.el
        ;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/multiverse.el")
        ((and (featurep 'multiverse)
              (assoc (current-buffer) multiverse-stored-versions))
         multiverse-restore multiverse-diff-current multiverse-diff-other
         multiverse-forget)

        ;; rct-fork (rcodetools.el)
        ((and (boundp 'rct-fork-minor-mode)
              rct-fork-minor-mode)
         rct-fork-kill)

        ;; highlight.el
        ;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/highlight.el")
        ((and (featurep 'highlight)
              (or (and hlt-use-overlays-flag
                       (loop for overlay in (overlays-in (point-min) (point-max))
                             thereis (overlay-get overlay 'hlt-highlight)))
                  (and (not (eq hlt-use-overlays-flag 'only))
                       (next-single-property-change (point-min) 'hlt-highlight))))
         hlt-unhighlight-region)
        ((and transient-mark-mode
              (featurep 'highlight))
         hlt-highlight-region hlt-highlight-regexp-region)

        ;; trace.el
        ((and (featurep 'trace)
              (loop for (funcname) in ad-advised-functions
                    thereis (trace-is-traced (intern funcname))))
         untrace-all)
        ))

(setq anything-kyr-commands-by-file-name
      '(;; yasnippet 
        ("snippets/text-mode"
         yas/reload-all)
        ;; yaoddmuse
        ("/emacs/yaoddmuse/"
         yaoddmuse-browse-current-page)
        ))

(setq anything-kyr-commands-by-major-mode
      '((emacs-lisp-mode
         byte-compile-file)
        ))

(provide 'anything-kyr-config)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-kyr-config.el")
;;; anything-kyr-config.el ends here
