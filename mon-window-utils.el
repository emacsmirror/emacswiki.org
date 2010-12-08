;;; mon-window-utils.el --- window related procedures for mon-*utils features
;; -*- mode: EMACS-LISP; -*-


;;; ================================================================
;; Copyright © 2010 MON KEY. All rights reserved.
;;; ================================================================

;; FILENAME: mon-window-utils.el
;; AUTHOR: MON KEY
;; MAINTAINER: MON KEY
;; CREATED: 2010-11-22T16:17:39-05:00Z
;; VERSION: 1.0.0
;; COMPATIBILITY: Emacs23.*
;; KEYWORDS: lisp, extensions, emacs,

;;; ================================================================

;;; COMMENTARY: 

;; =================================================================
;; DESCRIPTION:
;; mon-window-utils provides window related procedures for mon-*utils features
;;
;; FUNCTIONS:►►►
;; `mon-map-windows->plist', `mon-twin-horizontal', `mon-twin-vertical',
;; `mon-toggle-menu-bar', `mon-scratch', `mon-switch-to-messages',
;; `mon-kill-completions', `mon-flip-windows', `frame-live-visible-graphic-p',
;; `mon-map-windows->plist'
;;
;; FUNCTIONS:◄◄◄
;;
;; MACROS:
;;
;; METHODS:
;;
;; CLASSES:
;;
;; CONSTANTS:
;;
;; FACES:
;;
;; VARIABLES:
;;
;; GROUPS:
;;
;; ALIASED/ADVISED/SUBST'D:
;;
;;  <UNQUALIFIED-ALIAS>                <PREFIX>-<NON-CORE-SYMBOL>
;; `frame-live-visible-graphic-p'    -> `mon-frame-live-visible-graphic-p'
;;
;;  <PREFIX>-<QUALIFIED>                <PREFIX>-<NON-CORE-SYMBOL>
;; `mon-get-window-plist'            -> `mon-map-windows->plist'
;; `mon-window-flip'                 -> `mon-flip-windows'
;; `mon-window-get-if-buffer'        -> `mon-get-buffer-window-if'
;; `mon-window-map-active-to-plist'  -> `mon-map-windows->plist'
;; `mon-window-split-horiz'          -> `mon-twin-horizontal'
;; `mon-window-split-vert'           -> `mon-twin-vertical'
;; `mon-buffer-kill-completions'     -> `mon-kill-completions'
;; `mon-buffer-get-messages'         -> `mon-switch-to-messages'
;; `mon-buffer-get-scratch'          -> `mon-scratch'
;; `mon-buffer-get-shell'            -> `mon-shell'
;;
;; DEPRECATED:
;;
;; RENAMED:
;;
;; MOVED:
;; `mon-frame-live-visible-graphic-p'          <- mon-dir-utils.el
;; `mon-scratch'                               <- mon-utils.el
;; `mon-switch-to-messages'                    <- mon-utils.el
;; `mon-kill-completions'                      <- mon-utils.el
;; `mon-toggle-menu-bar'                       <- mon-utils.el
;; `mon-flip-windows'                          <- mon-utils.el
;; `mon-twin-horizontal'                       <- mon-utils.el
;; `mon-twin-vertical'                         <- mon-utils.el
;;
;; TODO:
;;
;; NOTES:
;;
;; SNIPPETS:
;;
;; REQUIRES:
;;
;; THIRD-PARTY-CODE:
;;
;; URL: http://www.emacswiki.org/emacs/mon-window-utils.el
;; FIRST-PUBLISHED:
;;
;; EMACSWIKI: { URL of an EmacsWiki describing mon-window-utils. }
;;
;; FILE-CREATED:
;; <Timestamp: #{2010-11-22T16:17:39-05:00Z}#{10471} - by MON KEY>
;;
;; =================================================================

;;; LICENSE:

;; =================================================================
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; =================================================================
;; Permission is granted to copy, distribute and/or modify this
;; document under the terms of the GNU Free Documentation License,
;; Version 1.3 or any later version published by the Free Software
;; Foundation; with no Invariant Sections, no Front-Cover Texts,
;; and no Back-Cover Texts. A copy of the license is included in
;; the section entitled ``GNU Free Documentation License''.
;; 
;; A copy of the license is also available from the Free Software
;; Foundation Web site at:
;; (URL `http://www.gnu.org/licenses/fdl-1.3.txt').
;;; ==============================
;; Copyright © 2010 MON KEY 
;;; ==============================

;;; CODE:

(eval-when-compile (require 'cl))

(unless (and (intern-soft "*IS-MON-OBARRAY*")
             (bound-and-true-p *IS-MON-OBARRAY*))
(setq *IS-MON-OBARRAY* (make-vector 17 nil)))

;;; ==============================
;;; :PREFIX "mflvgp-"
;;; :CHANGESET 2091
;;; :CREATED <Timestamp: #{2010-08-26T14:46:20-04:00Z}#{10344} - by MON KEY>
(defun mon-frame-live-visible-graphic-p (test-frame)
  "Whether TEST-FRAME is live, visible, not iconified, and not invisible.
Return cons (TEST-FRAME . { t | nil } when TEST-FRAME satisfies the predicate
`frame-live-p' where a live frame has the type either `x` or `w32` and the
predicate `frame-visible-p' returns non-nil with a visible frame not of type
`icon`.\n
:EXAMPLE\n\n\(mon-frame-live-visible-graphic-p \(selected-frame\)\)\n
\(mapcar  #'mon-frame-live-visible-graphic-p \(frame-list\)\)\n
\(filtered-frame-list #'mon-frame-live-visible-graphic-p\)\n
:ALIASED-BY `frame-live-visible-graphic-p'\n
:SEE-ALSO `mon-find-buffer-visiting-other-live-frame',
`dired-find-file-other-window', `filtered-frame-list', `make-frame-names-alist',
`visible-frame-list', `mon-help-frame-functions'.\n►►►"
  (let ((mflvgp-flp (frame-live-p test-frame))
        (mflvgp-fvp (frame-visible-p test-frame)))
    (if (and mflvgp-flp (memq mflvgp-flp '(x w32)) 
             (not (null mflvgp-fvp))
             (not (eq mflvgp-fvp 'icon)))
        (cons test-frame  t)
      (list test-frame))))
;;
;; :TEST-ME (mon-frame-live-visible-graphic-p (selected-frame))
;; :TEST-ME (mapcar  #'mon-frame-live-visible-graphic-p (frame-list)) 
;; :TEST-ME (filtered-frame-list #'mon-frame-live-visible-graphic-p)

;;; ==============================
;;; :NOTE Fashioned after `with-help-window's `list-of-window-tuples' :FILE help.el
;;; :CHANGESET 2101
;;; :CREATED <Timestamp: #{2010-08-31T19:04:24-04:00Z}#{10352} - by MON KEY>
(defun mon-map-windows->plist ()
  "Return a list of plist's mapping window properties of windows on all frames.\n
Does not return for mini-buffers.\n
plist elements of returned list have the format:\n
 \(:window                 <WINDOW>
  :window-frame           <FRAME>
  :window-buffer          <BUFFER>
  :window-buffer-name     <BUFFER-NAME>
  :window-buffer-visiting <FILE-NAME> 
  :window-point           <INTEGER>
  :window-start           <INTEGER>
  :window-end             <INTEGER>
  :window-point-col-row \(:posn-col <INTEGER> 
                         :posn-row <INTEGER>\)
  :window-edges \(:left   <INTEGER> 
                 :top    <INTEGER>
                 :right  <INTEGER>
                 :bottom <INTEGER>\)
  :window-point-char-width-height \(:char-width  <INTEGER>
                                   :char-height <INTEGER>\)\)\n
:EXAMPLE\n\n(mon-map-windows->plist\)\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window-buffer-name\)\) \(mon-map-windows->plist\)\)\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window\)\) \(mon-map-windows->plist\)\)\n
\(mapcar #'\(lambda \(pl\) \(plist-get pl :window-point\)\) \(mon-map-windows->plist\)\)\n
\(mon-mapcar #'mon-plist-keys \(mon-map-windows->plist\)\)\n
:ALIASED-BY `mon-get-window-plist'
:ALIASED-BY `mon-window-map-active-to-plist'\n
:SEE-ALSO `mon-get-buffer-window-if', `mon-plist-keys',
`mon-map-obarray-symbol-plist-props', `mon-list-all-properties-in-buffer',
`mon-help-window-functions', `mon-help-plist-functions',
`mon-help-plist-properties'.\n►►►"
  (let (mmwp-wdo-l)
    (walk-windows
     #'(lambda (window)
         (let* ((mmwp-wdo         window)
                (mmwp-wdo-pnt     (window-point mmwp-wdo))
                (mmwp-wdo-dbind   
                 (destructuring-bind (dbnd-w psn-area psn-area-cns tsmp obj 
                                            pos pos-col/row img img-x/y w/h)
                     (posn-at-point mmwp-wdo-pnt mmwp-wdo)
                   (list :window dbnd-w
                         :window-frame (window-frame   dbnd-w)
                         :window-buffer (window-buffer dbnd-w)
                         :window-buffer-name (buffer-name (window-buffer dbnd-w))
                         :window-buffer-visiting (buffer-file-name (window-buffer dbnd-w))
                         :window-point pos
                         :window-start (window-start dbnd-w)    
                         :window-end   (window-end   dbnd-w)
                         :window-point-col-row 
                         `(:posn-col ,(car pos-col/row) :posn-row ,(cdr pos-col/row))
                         :window-edges 
                         (destructuring-bind (lft top rgt btm) (window-edges dbnd-w)
                           (list :left lft :top top :right rgt :bottom btm))
                         ;; psn-area psn-area-cns ;; tsmp ;; obj ;; img img-x/y
                         :window-point-char-width-height 
                         `(:char-width ,(car w/h) :char-height ,(cdr w/h))))))
           (push mmwp-wdo-dbind mmwp-wdo-l)))
     'no-mini t)
    mmwp-wdo-l))

;;; ==============================
;; 
;; (frame-parameters (selected-frame))
;; :frame (selected-frame)
;; :display (frame-parameter (selected-frame) 'display)

;;; ==============================
;;; :COURTESY Francois Fleuret <fleuret@idiap.ch> :HIS fleuret.emacs.el
;;; :SEE (URL `http://www.idiap.ch/~fleuret/files/fleuret.emacs.el')
;;; :WAS `ff/twin-horizontal-current-buffer' -> `mon-twin-horizontal'
(defun mon-twin-horizontal () 
  "Split current-buffer horizontally.\n
:ALIASED-BY `mon-window-split-horiz'\n
:SEE-ALSO `mon-twin-vertical', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (balance-windows))
;;
;;; :WAS `ff/twin-vertical-current-buffer' -> `mon-twin-vertical'
(defun mon-twin-vertical () 
  "Split current-buffer vertically.\n
:ALIASED-BY `mon-window-split-vert'\n
:SEE-ALSO `mon-twin-horizontal', `mon-flip-windows'.\n►►►"
  (interactive)
  (delete-other-windows)
  (split-window-vertically)
  (balance-windows))

;;; ==============================
;;; :PREFIX "mtmb-"
(defun mon-toggle-menu-bar ()
  "Toggle the top menu bar.\nGets the max editor screen for your money!\n
:SEE-ALSO `mon-toggle-dired-dwim-target', `mon-toggle-truncate-line'
`mon-toggle-eval-length', `mon-naf-mode-toggle-restore-llm',
`mon-toggle-show-point-mode'.\n►►►"
  (interactive)
  (let ((mtmb-height (frame-height)))
    (menu-bar-mode nil)
    (set-frame-height (selected-frame)
                      (if menu-bar-mode
                          (1- mtmb-height)
                        (1+ mtmb-height)))
    (force-mode-line-update t)))

;;; ==============================
;;; :CHANGESET 1751 <Timestamp: #{2010-05-21T16:27:55-04:00Z}#{10205} - by MON KEY>
(defun mon-scratch (&optional w-this-scratch)
  "Switch to *scratch* buffer in other window.\n
When *scratch* buffer does not exist, get \(or create\) it now!\n
When W-THIS-SCRATCH is non-nil or called-interactively with prefix arg if
current-buffer is \"*scratch*\" erase buffer contents else find an empty scratch buffer.\n
:EXAMPLE\n\n\(mon-scratch\)\n
:ALIASED-BY `mon-buffer-get-scratch'
:SEE-ALSO `mon-switch-to-mesages', `mon-kill-completions'.\n►►►"
  (interactive "P")
  (let ((confirm-nonexistent-file-or-buffer nil))
    ;; (same-window-buffer-names '("*scratch*")))
    (if (or current-prefix-arg w-this-scratch)
        (if (equal (buffer-name (current-buffer)) "*scratch*")
            (with-current-buffer (current-buffer) (erase-buffer))
          (switch-to-buffer-other-window "*scratch*" t))
      (switch-to-buffer-other-window "*scratch*"  t))
    )) ;; (lisp-interaction-mode)
;;
;;; :TEST-ME (mon-scratch)
;;; :TEST-ME (mon-scratch t)

;;; ==============================
(defun mon-switch-to-messages ()
  "Select buffer *Messages* in the current window.\n
:ALIASED-BY `mon-buffer-get-messages'
:SEE-ALSO `mon-scratch', `mon-kill-completions'\n►►►"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;; ==============================
;;; :CREATED <Timestamp: Thursday March 05, 2009 @ 04:49.29 PM - by MON KEY>
(defun mon-kill-completions ()
  "Kill *Completions* buffer without leaving point.\n
:ALIASED-BY `mon-buffer-kill-completions'\n
:SEE-ALSO `mon-scratch', `mon-switch-to-messages'.\n►►►"
  (interactive)
  (save-excursion
    (when (get-buffer-window "*Completions*")
      (progn
	(switch-to-completions)
	(delete-completion-window)))))

;;; ==============================
(defun mon-flip-windows ()
  "Swap current-buffer display with buffer in other window.\n
:ALIASED-BY `mon-window-flip'\n
:SEE-ALSO `mon-twin-vertical', `mon-twin-horizontal'.\n►►►"
  (interactive)
  (let ((cur-buffer (current-buffer))
        (top-buffer)
        (bottom-buffer))
    (pop-to-buffer (window-buffer (frame-first-window)))
    (setq top-buffer (current-buffer))
    (other-window 1)
    (setq bottom-buffer (current-buffer))
    (switch-to-buffer top-buffer)
    (other-window -1)
    (switch-to-buffer bottom-buffer)
    (pop-to-buffer cur-buffer)))

;;; ==============================
(provide 'mon-window-utils)
;;; ==============================

 
;; Local Variables:
;; mode: EMACS-LISP
;; coding: utf-8
;; generated-autoload-file: "./mon-loaddefs.el"
;; End:

;;; ====================================================================
;;; mon-window-utils.el ends here
;;; EOF
