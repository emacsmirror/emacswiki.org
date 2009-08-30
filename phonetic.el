;;; phonetic.el: Phonetize (Alpha, Bravo, Charlie...) the region

;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Author & Maintainer: Joseph Eydelnant <jey222-AT-mail-DOT-com@NO_SPAM.COM>
;; Location: www.emacswiki.org
;; Keywords: alphabet phonetic spell 
;; Version: 0.11

;; phonetic.el is free software.

;; This file is not a part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Abstract:
;; This alphabet dates from about 1955 and is approved by
;; the NATO, the International Civil Aviation Organization,
;; the FAA and the International Telecommunication Union.
;; See also: www.columbia.edu/~fuat/cuarc/phonetic.html

;;; Activation:
;; Copy this file into a directory on the load-path, byte-compile it 
;; for speed (optional) and place the following into your ~/.emacs:
;; (require 'phonetize)

;;; Usage:
;; Select a region, then M-x phonetize-region

;;; Todo:
;; Expand into alphabets other than English

;;; Acknowledgements:
;; Reused code from morse.el by Rick Farnbach <rick_farnbach@MENTORG.COM>


(defvar phonetic-translatn-tbl '(
    ("a" . "alpha"    ) ("A" . "Alpha"    )
    ("b" . "bravo"    ) ("B" . "Bravo"    )
    ("c" . "charlie"  ) ("C" . "Charlie"  )
    ("d" . "delta"    ) ("D" . "Delta"    )
    ("e" . "echo"     ) ("E" . "Echo"     )
    ("f" . "foxtrot"  ) ("F" . "Foxtrot"  )
    ("g" . "golf"     ) ("G" . "Golf"     )
    ("h" . "hotel"    ) ("H" . "Hotel"    )
    ("i" . "india"    ) ("I" . "India"    )
    ("j" . "juliet"   ) ("J" . "Juliet"   )
    ("k" . "kilo"     ) ("K" . "Kilo"     )
    ("l" . "lima"     ) ("L" . "Lima"     )
    ("m" . "mike"     ) ("M" . "Mike"     )
    ("n" . "november" ) ("N" . "November" )
    ("o" . "oscar"    ) ("O" . "Oscar"    )
    ("p" . "papa"     ) ("P" . "Papa"     )
    ("q" . "quebec"   ) ("Q" . "Quebec"   )
    ("r" . "romeo"    ) ("R" . "Romeo"    )
    ("s" . "sierra"   ) ("S" . "Sierra"   )
    ("t" . "tango"    ) ("T" . "Tango"    )
    ("u" . "uniform"  ) ("U" . "Uniform"  )
    ("v" . "victor"   ) ("V" . "Victor"   )
    ("w" . "whiskey"  ) ("W" . "Whiskey"  )
    ("x" . "xray"     ) ("X" . "Xray"     )
    ("y" . "yankee"   ) ("Y" . "Yankee"   )
    ("z" . "zulu"     ) ("Z" . "Zulu"     )
    ))


(defun phonetize-region (beg end)
  "Translate the region according to the phonetic alphabet."
  (interactive "r")
  (split-window-vertically -4)
  (other-window 1)
  (get-buffer-create "*phonetic*")
  (copy-to-buffer "*phonetic*" (region-beginning) (region-end))
  (switch-to-buffer "*phonetic*")
  (let ((sep "") str to-phonetize)
    (while (not (eobp))
      (setq str (buffer-substring (point) (1+ (point))))
        (cond ((looking-at "\\s-+")
               (goto-char (match-end 0))
               (setq sep ""))
              ((setq to-phonetize (assoc str phonetic-translatn-tbl))
               (delete-char 1)
               (insert sep (cdr to-phonetize))
               (setq sep " "))
              (t
               (forward-char 1)
               (setq sep "")))))
  (message "Press C-x 0 to close this window."))

(provide 'phonetic)
;;; phonetic.el ends here
