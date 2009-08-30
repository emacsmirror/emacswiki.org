;;; latex-math-preview-extra-data.el --- Extra data for latex-math-preview.el.

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: LaTeX TeX
;; Version: 0.1.0
;; Created: Thu Aug  6 17:36:25 2009

;; Copyright 2009 Takayuki YAMAGUCHI
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later 
;; version. 
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT 
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;; You may add
;; latex-math-preview-textcomp-symbol-data,
;; latex-math-preview-pifont-zapf-dingbats-symbol-data or
;; latex-math-preview-pifont-symbol-fonts-symbol-data
;; to latex-math-preview-text-symbol-datasets
;; 
;; (add-to-list 'latex-math-preview-text-symbol-datasets
;; 	     latex-math-preview-textcomp-symbol-data)
;; (add-to-list 'latex-math-preview-text-symbol-datasets
;; 	     latex-math-preview-pifont-zapf-dingbats-symbol-data)
;; (add-to-list 'latex-math-preview-text-symbol-datasets
;; 	     latex-math-preview-pifont-symbol-fonts-symbol-data)

(defvar latex-math-preview-textcomp-symbol-data
  '("Textcomp" 
    ("textcomp" ("\\usepackage[T1]{fontenc}" "\\usepackage{textcomp}")
     ("\\textquotestraightbase" "\\textquotestraightdblbase" "\\texttwelveudash"
      "\\textthreequartersemdash" "\\textleftarrow" "\\textrightarrow"
      "\\textblank" "\\textdollar" "\\textquotesingle" "\\textasteriskcentered"
      "\\textdblhyphen" "\\textfractionsolidus"
      "\\textzerooldstyle" "\\textoneoldstyle" "\\texttwooldstyle"
      "\\textthreeoldstyle" "\\textfouroldstyle" "\\textfiveoldstyle"
      "\\textsixoldstyle" "\\textsevenoldstyle" "\\texteightoldstyle"
      "\\textnineoldstyle"
      "\\textlangle" "\\textminus" "\\textrangle" "\\textmho" "\\textbigcircle"
      "\\textohm" "\\textlbrackdbl" "\\textrbrackdbl" "\\textuparrow"
      "\\textdownarrow" "\\textasciigrave" "\\textborn" "\\textdivorced"
      "\\textdied" "\\textleaf" "\\textmarried" "\\textmusicalnote" "\\texttildelow"
      "\\textdblhyphenchar" "\\textasciibreve" "\\textasciicaron" "\\textacutedbl"
      "\\textgravedbl" "\\textdagger" "\\textdaggerdbl" "\\textbardbl"
      "\\textperthousand" "\\textbullet" "\\textcelsius" "\\textdollaroldstyle"
      "\\textcentoldstyle" "\\textflorin" "\\textcolonmonetary" "\\textwon" 
      "\\textnaira" "\\textguarani" "\\textpeso" "\\textlira" "\\textrecipe" 
      "\\textinterrobang" "\\textinterrobangdown"
      "\\textdong" "\\texttrademark" "\\textpertenthousand" "\\textpilcrow"
      "\\textbaht" "\\textnumero" "\\textdiscount" "\\textestimated"
      "\\textopenbullet" "\\textservicemark" "\\textlquill" "\\textrquill"
      "\\textcent" "\\textsterling" "\\textcurrency" "\\textyen" 
      "\\textbrokenbar" "\\textsection" "\\textasciidieresis" "\\textcopyright" 
      "\\textordfeminine" "\\textcopyleft" "\\textlnot" "\\textcircledP" 
      "\\textregistered" "\\textasciimacron" "\\textdegree" "\\textpm" 
      "\\texttwosuperior" "\\textthreesuperior" "\\textasciiacute" "\\textmu"
      "\\textparagraph" "\\textperiodcentered" "\\textreferencemark"
      "\\textonesuperior" "\\textordmasculine" "\\textsurd" "\\textonequarter" 
      "\\textonehalf" "\\textthreequarters" "\\texteuro" "\\texttimes" "\\textdiv"
      )))
  "Symbol data for textcomp.sty.")

(defvar latex-math-preview-pifont-zapf-dingbats-symbol-data
  '("PifontZapfDingbats"
    ("Zapf Dingbats" ("\\usepackage{pifont}")
     ("\\ding{\"21}" "\\ding{\"22}" "\\ding{\"23}" "\\ding{\"24}" "\\ding{\"25}"
      "\\ding{\"26}" "\\ding{\"27}" "\\ding{\"28}" "\\ding{\"29}" "\\ding{\"2A}"
      "\\ding{\"2B}" "\\ding{\"2C}" "\\ding{\"2D}" "\\ding{\"2E}" "\\ding{\"2F}" 
      "\\ding{\"30}" "\\ding{\"31}" "\\ding{\"32}" "\\ding{\"33}" "\\ding{\"34}"
      "\\ding{\"35}" "\\ding{\"36}" "\\ding{\"37}" "\\ding{\"38}" "\\ding{\"39}"
      "\\ding{\"3A}" "\\ding{\"3B}" "\\ding{\"3C}" "\\ding{\"3D}" "\\ding{\"3E}"
      "\\ding{\"3F}" 
      "\\ding{\"40}" "\\ding{\"41}" "\\ding{\"42}" "\\ding{\"43}" "\\ding{\"44}"
      "\\ding{\"45}" "\\ding{\"46}" "\\ding{\"47}" "\\ding{\"48}" "\\ding{\"49}"
      "\\ding{\"4A}" "\\ding{\"4B}" "\\ding{\"4C}" "\\ding{\"4D}" "\\ding{\"4E}"
      "\\ding{\"4F}" 
      "\\ding{\"50}" "\\ding{\"51}" "\\ding{\"52}" "\\ding{\"53}" "\\ding{\"54}"
      "\\ding{\"55}" "\\ding{\"56}" "\\ding{\"57}" "\\ding{\"58}" "\\ding{\"59}"
      "\\ding{\"5A}" "\\ding{\"5B}" "\\ding{\"5C}" "\\ding{\"5D}" "\\ding{\"5E}"
      "\\ding{\"5F}" 
      "\\ding{\"60}" "\\ding{\"61}" "\\ding{\"62}" "\\ding{\"63}" "\\ding{\"64}"
      "\\ding{\"65}" "\\ding{\"66}" "\\ding{\"67}" "\\ding{\"68}" "\\ding{\"69}"
      "\\ding{\"6A}" "\\ding{\"6B}" "\\ding{\"6C}" "\\ding{\"6D}" "\\ding{\"6E}"
      "\\ding{\"6F}" 
      "\\ding{\"70}" "\\ding{\"71}" "\\ding{\"72}" "\\ding{\"73}" "\\ding{\"74}"
      "\\ding{\"75}" "\\ding{\"76}" "\\ding{\"77}" "\\ding{\"78}" "\\ding{\"79}"
      "\\ding{\"7A}" "\\ding{\"7B}" "\\ding{\"7C}" "\\ding{\"7D}" "\\ding{\"7E}" 
      "\\ding{\"A1}" "\\ding{\"A2}" "\\ding{\"A3}" "\\ding{\"A4}" "\\ding{\"A5}"
      "\\ding{\"A6}" "\\ding{\"A7}" "\\ding{\"A8}" "\\ding{\"A9}" "\\ding{\"AA}"
      "\\ding{\"AB}" "\\ding{\"AC}" "\\ding{\"AD}" "\\ding{\"AE}" "\\ding{\"AF}" 
      "\\ding{\"B0}" "\\ding{\"B1}" "\\ding{\"B2}" "\\ding{\"B3}" "\\ding{\"B4}"
      "\\ding{\"B5}" "\\ding{\"B6}" "\\ding{\"B7}" "\\ding{\"B8}" "\\ding{\"B9}"
      "\\ding{\"BA}" "\\ding{\"BB}" "\\ding{\"BC}" "\\ding{\"BD}" "\\ding{\"BE}"
      "\\ding{\"BF}" 
      "\\ding{\"C0}" "\\ding{\"C1}" "\\ding{\"C2}" "\\ding{\"C3}" "\\ding{\"C4}"
      "\\ding{\"C5}" "\\ding{\"C6}" "\\ding{\"C7}" "\\ding{\"C8}" "\\ding{\"C9}"
      "\\ding{\"CA}" "\\ding{\"CB}" "\\ding{\"CC}" "\\ding{\"CD}" "\\ding{\"CE}"
      "\\ding{\"CF}" 
      "\\ding{\"D0}" "\\ding{\"D1}" "\\ding{\"D2}" "\\ding{\"D3}" "\\ding{\"D4}"
      "\\ding{\"D5}" "\\ding{\"D6}" "\\ding{\"D7}" "\\ding{\"D8}" "\\ding{\"D9}"
      "\\ding{\"DA}" "\\ding{\"DB}" "\\ding{\"DC}" "\\ding{\"DD}" "\\ding{\"DE}"
      "\\ding{\"DF}" 
      "\\ding{\"E0}" "\\ding{\"E1}" "\\ding{\"E2}" "\\ding{\"E3}" "\\ding{\"E4}"
      "\\ding{\"E5}" "\\ding{\"E6}" "\\ding{\"E7}" "\\ding{\"E8}" "\\ding{\"E9}"
      "\\ding{\"EA}" "\\ding{\"EB}" "\\ding{\"EC}" "\\ding{\"ED}" "\\ding{\"EE}"
      "\\ding{\"EF}" 
      "\\ding{\"F1}" "\\ding{\"F2}" "\\ding{\"F3}" "\\ding{\"F4}" "\\ding{\"F5}"
      "\\ding{\"F6}" "\\ding{\"F7}" "\\ding{\"F8}" "\\ding{\"F9}" "\\ding{\"FA}"
      "\\ding{\"FB}" "\\ding{\"FC}" "\\ding{\"FD}" "\\ding{\"FE}")))
  "Symbol data for Zapf Dingbats in pifont.sty")

(defvar latex-math-preview-pifont-symbol-fonts-symbol-data
  '("PifontSymbolFonts"
    ("Symbol font" ("\\usepackage{pifont}")
     ("\\Pisymbol{psy}{\"21}" "\\Pisymbol{psy}{\"22}"
      "\\Pisymbol{psy}{\"23}" "\\Pisymbol{psy}{\"24}" "\\Pisymbol{psy}{\"25}"
      "\\Pisymbol{psy}{\"26}" "\\Pisymbol{psy}{\"27}" "\\Pisymbol{psy}{\"28}"
      "\\Pisymbol{psy}{\"29}" "\\Pisymbol{psy}{\"2A}" "\\Pisymbol{psy}{\"2B}"
      "\\Pisymbol{psy}{\"2C}" "\\Pisymbol{psy}{\"2D}" "\\Pisymbol{psy}{\"2E}"
      "\\Pisymbol{psy}{\"2F}" 
      "\\Pisymbol{psy}{\"30}" "\\Pisymbol{psy}{\"31}" "\\Pisymbol{psy}{\"32}"
      "\\Pisymbol{psy}{\"33}" "\\Pisymbol{psy}{\"34}" "\\Pisymbol{psy}{\"35}"
      "\\Pisymbol{psy}{\"36}" "\\Pisymbol{psy}{\"37}" "\\Pisymbol{psy}{\"38}"
      "\\Pisymbol{psy}{\"39}" "\\Pisymbol{psy}{\"3A}" "\\Pisymbol{psy}{\"3B}"
      "\\Pisymbol{psy}{\"3C}" "\\Pisymbol{psy}{\"3D}" "\\Pisymbol{psy}{\"3E}"
      "\\Pisymbol{psy}{\"3F}" 
      "\\Pisymbol{psy}{\"40}" "\\Pisymbol{psy}{\"41}" "\\Pisymbol{psy}{\"42}"
      "\\Pisymbol{psy}{\"43}" "\\Pisymbol{psy}{\"44}" "\\Pisymbol{psy}{\"45}"
      "\\Pisymbol{psy}{\"46}" "\\Pisymbol{psy}{\"47}" "\\Pisymbol{psy}{\"48}"
      "\\Pisymbol{psy}{\"49}" "\\Pisymbol{psy}{\"4A}" "\\Pisymbol{psy}{\"4B}"
      "\\Pisymbol{psy}{\"4C}" "\\Pisymbol{psy}{\"4D}" "\\Pisymbol{psy}{\"4E}"
      "\\Pisymbol{psy}{\"4F}" 
      "\\Pisymbol{psy}{\"50}" "\\Pisymbol{psy}{\"51}" "\\Pisymbol{psy}{\"52}"
      "\\Pisymbol{psy}{\"53}" "\\Pisymbol{psy}{\"54}" "\\Pisymbol{psy}{\"55}"
      "\\Pisymbol{psy}{\"56}" "\\Pisymbol{psy}{\"57}" "\\Pisymbol{psy}{\"58}"
      "\\Pisymbol{psy}{\"59}" "\\Pisymbol{psy}{\"5A}" "\\Pisymbol{psy}{\"5B}"
      "\\Pisymbol{psy}{\"5C}" "\\Pisymbol{psy}{\"5D}" "\\Pisymbol{psy}{\"5E}"
      "\\Pisymbol{psy}{\"5F}" 
      "\\Pisymbol{psy}{\"60}" "\\Pisymbol{psy}{\"61}" "\\Pisymbol{psy}{\"62}"
      "\\Pisymbol{psy}{\"63}" "\\Pisymbol{psy}{\"64}" "\\Pisymbol{psy}{\"65}"
      "\\Pisymbol{psy}{\"66}" "\\Pisymbol{psy}{\"67}" "\\Pisymbol{psy}{\"68}"
      "\\Pisymbol{psy}{\"69}" "\\Pisymbol{psy}{\"6A}" "\\Pisymbol{psy}{\"6B}"
      "\\Pisymbol{psy}{\"6C}" "\\Pisymbol{psy}{\"6D}" "\\Pisymbol{psy}{\"6E}"
      "\\Pisymbol{psy}{\"6F}" 
      "\\Pisymbol{psy}{\"70}" "\\Pisymbol{psy}{\"71}" "\\Pisymbol{psy}{\"72}"
      "\\Pisymbol{psy}{\"73}" "\\Pisymbol{psy}{\"74}" "\\Pisymbol{psy}{\"75}"
      "\\Pisymbol{psy}{\"76}" "\\Pisymbol{psy}{\"77}" "\\Pisymbol{psy}{\"78}"
      "\\Pisymbol{psy}{\"79}" "\\Pisymbol{psy}{\"7A}" "\\Pisymbol{psy}{\"7B}"
      "\\Pisymbol{psy}{\"7C}" "\\Pisymbol{psy}{\"7D}" "\\Pisymbol{psy}{\"7E}"
      "\\Pisymbol{psy}{\"A1}" "\\Pisymbol{psy}{\"A2}"
      "\\Pisymbol{psy}{\"A3}" "\\Pisymbol{psy}{\"A4}" "\\Pisymbol{psy}{\"A5}"
      "\\Pisymbol{psy}{\"A6}" "\\Pisymbol{psy}{\"A7}" "\\Pisymbol{psy}{\"A8}"
      "\\Pisymbol{psy}{\"A9}" "\\Pisymbol{psy}{\"AA}" "\\Pisymbol{psy}{\"AB}"
      "\\Pisymbol{psy}{\"AC}" "\\Pisymbol{psy}{\"AD}" "\\Pisymbol{psy}{\"AE}"
      "\\Pisymbol{psy}{\"AF}" 
      "\\Pisymbol{psy}{\"B0}" "\\Pisymbol{psy}{\"B1}" "\\Pisymbol{psy}{\"B2}"
      "\\Pisymbol{psy}{\"B3}" "\\Pisymbol{psy}{\"B4}" "\\Pisymbol{psy}{\"B5}"
      "\\Pisymbol{psy}{\"B6}" "\\Pisymbol{psy}{\"B7}" "\\Pisymbol{psy}{\"B8}"
      "\\Pisymbol{psy}{\"B9}" "\\Pisymbol{psy}{\"BA}" "\\Pisymbol{psy}{\"BB}"
      "\\Pisymbol{psy}{\"BC}" "\\Pisymbol{psy}{\"BD}" "\\Pisymbol{psy}{\"BE}"
      "\\Pisymbol{psy}{\"BF}" 
      "\\Pisymbol{psy}{\"C0}" "\\Pisymbol{psy}{\"C1}" "\\Pisymbol{psy}{\"C2}"
      "\\Pisymbol{psy}{\"C3}" "\\Pisymbol{psy}{\"C4}" "\\Pisymbol{psy}{\"C5}"
      "\\Pisymbol{psy}{\"C6}" "\\Pisymbol{psy}{\"C7}" "\\Pisymbol{psy}{\"C8}"
      "\\Pisymbol{psy}{\"C9}" "\\Pisymbol{psy}{\"CA}" "\\Pisymbol{psy}{\"CB}"
      "\\Pisymbol{psy}{\"CC}" "\\Pisymbol{psy}{\"CD}" "\\Pisymbol{psy}{\"CE}"
      "\\Pisymbol{psy}{\"CF}" 
      "\\Pisymbol{psy}{\"D0}" "\\Pisymbol{psy}{\"D1}" "\\Pisymbol{psy}{\"D2}"
      "\\Pisymbol{psy}{\"D3}" "\\Pisymbol{psy}{\"D4}" "\\Pisymbol{psy}{\"D5}"
      "\\Pisymbol{psy}{\"D6}" "\\Pisymbol{psy}{\"D7}" "\\Pisymbol{psy}{\"D8}"
      "\\Pisymbol{psy}{\"D9}" "\\Pisymbol{psy}{\"DA}" "\\Pisymbol{psy}{\"DB}"
      "\\Pisymbol{psy}{\"DC}" "\\Pisymbol{psy}{\"DD}" "\\Pisymbol{psy}{\"DE}"
      "\\Pisymbol{psy}{\"DF}" 
      "\\Pisymbol{psy}{\"E0}" "\\Pisymbol{psy}{\"E1}" "\\Pisymbol{psy}{\"E2}"
      "\\Pisymbol{psy}{\"E3}" "\\Pisymbol{psy}{\"E4}" "\\Pisymbol{psy}{\"E5}"
      "\\Pisymbol{psy}{\"E6}" "\\Pisymbol{psy}{\"E7}" "\\Pisymbol{psy}{\"E8}"
      "\\Pisymbol{psy}{\"E9}" "\\Pisymbol{psy}{\"EA}" "\\Pisymbol{psy}{\"EB}"
      "\\Pisymbol{psy}{\"EC}" "\\Pisymbol{psy}{\"ED}" "\\Pisymbol{psy}{\"EE}"
      "\\Pisymbol{psy}{\"EF}" 
      "\\Pisymbol{psy}{\"F1}" "\\Pisymbol{psy}{\"F2}"
      "\\Pisymbol{psy}{\"F3}" "\\Pisymbol{psy}{\"F4}" "\\Pisymbol{psy}{\"F5}"
      "\\Pisymbol{psy}{\"F6}" "\\Pisymbol{psy}{\"F7}" "\\Pisymbol{psy}{\"F8}"
      "\\Pisymbol{psy}{\"F9}" "\\Pisymbol{psy}{\"FA}" "\\Pisymbol{psy}{\"FB}"
      "\\Pisymbol{psy}{\"FC}" "\\Pisymbol{psy}{\"FD}" "\\Pisymbol{psy}{\"FE}"
      )))
  "Symbol data for syymbol fonts in pifont.sty.")

(provide 'latex-math-preview-extra-data)

;;; latex-math-preview-extra-data.el ends here
