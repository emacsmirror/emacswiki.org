;;; gothic.el --- Quail package for inputting characters from the gothic alphabet (as used by Wulfila)  -*-coding: utf-8;-*-

;; Copyright (C) 2008  Niels Giesen <niels.giesen@gmail.com>

;; Author: Niels Giesen <niels.giesen@gmail.com>
;; Keywords: mule, input method, Gothic
;; Version: 0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;; Numerics are input unintelligently, so if you want to, say, input
;; 412, type in 400 1 2, (or "wab" of course).
(require 'quail)

(quail-define-package
 "gothic" "Gothic" "𐌾" t
 "Gothic (UTF-8) input method."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("a" ["𐌰"])
 ("1"  ["𐌰"])
 ("b" ["𐌱"])
 ("2"  ["𐌱"])
 ("g" ["𐌲"])
 ("3"  ["𐌲"])
 ("d" ["𐌳"])
 ("4"  ["𐌳"])
 ("e" ["𐌴"])
 ("5"  ["𐌴"])
 ("q" ["𐌵"])
 ("6"  ["𐌵"])
 ("z" ["𐌶"])
 ("7"  ["𐌶"])
 ("h" ["𐌷"])
 ("8"  ["𐌷"])
 ("th" ["𐌸"])
 ("9"  ["𐌸"])
 ("i" ["𐌹"])
 ("10"  ["𐌹"])
 ("\"i" ["𐌹̈"])
 ("10"  ["𐌹̈"])
 ("k" ["𐌺"])
 ("20"  ["𐌺"])
 ("l" ["𐌻"])
 ("30"  ["𐌻"])
 ("m" ["𐌼"])
 ("40"  ["𐌼"])
 ("n" ["𐌽"])
 ("50"  ["𐌽"])
 ("j" ["𐌾"])
 ("60"  ["𐌾"])
 ("u" ["𐌿"])
 ("70"  ["𐌿"])
 ("p" ["𐍀"])
 ("80"  ["𐍀"])
 ("q'" ["𐍁"])	
 ("90"  ["𐍁"])
 ("r" ["𐍂"])
 ("100"  ["𐍂"])
 ("s" ["𐍃"])
 ("200"  ["𐍃"])
 ("t" ["𐍄"])
 ("300"  ["𐍄"])
 ("w" ["𐍅"])
 ("400"  ["𐍅"])
 ("f" ["𐍆"])
 ("500"  ["𐍆"])
 ("x" ["𐍇"])
 ("600"  ["𐍇"])
 ("hw" ["𐍈"])
 ("700"  ["𐍈"])
 ("o" ["𐍉"])
 ("800"  ["𐍉"])
 ("S" ["𐍊"])
 ("900" ["𐍊"]))

(provide 'gothic)
;;; gothic.el ends here
