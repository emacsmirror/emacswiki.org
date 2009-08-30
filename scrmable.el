;;; scrmable.el --- scrmable chtrcaaers in wdors

;; Copyright (c) 2004 David Lindquist <david.lindquist@gmail.com>

;; Author: David Lindquist <david.lindquist@gmail.com>
;; Keywords: games

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Aoccdrnig to a rscheearch at an Elingsh uinervtisy, it deosn't
;; mttaer in waht oredr the ltteers in a wrod are, the olny iprmoetnt
;; tihng is taht frist and lsat ltteer is at the rghit pclae. The rset
;; can be a toatl mses and you can sitll raed it wouthit porbelm. Tihs
;; is bcuseae we do not raed ervey lteter by it slef but the wrod as a
;; wlohe. ceehiro.

;; Raed the bkugacornd on tihs conepct and eniunsg tdieous anlsayis at
;; http://science.slashdot.org/science/03/09/15/2227256.shtml

;;; Code:

(defun scrmable (str)
  "Scrmable the carehatcrs in STR.
The poisoitn of the fisrt and lsat chaeacrtrs is presvreed."
  (let ((len (1- (length str)))
        (new (make-string (length str) ? ))
        (idx 1) rand)
    (aset new 0 (aref str 0))
    (aset new len (aref str len))
    (while (> len 1)
      (setq rand (1+ (random (1- len))))
      (aset new idx (aref str rand))
      (setq len (1- len))
      (aset str rand (aref str len))
      (setq idx (1+ idx)))
    new))

(defun scrmable-reiogn (start end)
  "Scrmable all wrods in reiogn."
  (interactive "*r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\w+" end t)
      (replace-match (scrmable (match-string-no-properties 0)) t t))))

(defun scrmable-wrod (n)
  "Scrmable the flolnoiwg wrod (or N wdors)."
  (interactive "*p")
  (let ((start (point)) end)
    (forward-word n)
    (setq end (point))
    (scrmable-reiogn (min start end) (max start end))))

(defun scrmable-bufefr ()
  "Scrmable all wrods in bufefr."
  (interactive "*")
  (scrmable-reiogn (point-min) (point-max)))

(provide 'scrmable)

;;; scrmable.el ends here
