;;; ddg-mode.el --- 
;; 
;; Filename: ddg-mode.el
;; Description: 
;; Author: Christian Giménez
;; Maintainer: 
;; Created: mié mar 20 01:49:35 2013 (-0300)
;; Version: 
;; Last-Updated: mié mar 20 02:38:37 2013 (-0300)
;;           By: Christian
;;     Update #: 37
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   Cannot open load file: ddg-mode.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 20-Mar-2013    Christian  
;;    Last-Updated: mié mar 20 02:36:15 2013 (-0300) #34 (Christian)
;;    Major mode first revision.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

					; --------------------
					; Faces

(defface ddg-mode-face-field
  '(
    (t
     :width wide
     :foreground "black"
     :background "black"
     )
    )  
  ""
  )

(defface ddg-mode-face-answer
  '(
    (t
     :width wide
     :foreground "spring green"
     )
    )  
  ""
  )

(defface ddg-mode-face-query
  '(
    (t
     :slant italic
     :box (:line-width 1 :color "spring green" :style pressed-button)
     )
    )  
  ""
  )  

(defface ddg-mode-face-section
  '(
    (t
     :weight bold
     :foreground "aquamarine"
     )
    )  
  ""
  )  

(defface ddg-mode-face-result
  '(
    (t
     :foreground "dim gray"
     )
    )  
  ""
  )  
					; --------------------
					; Regexps

(defvar ddg-mode-regexp-answer "^\\(Answer: \\)\\(.*\\)$"
  "Regexp for the Answer field.")

(defvar ddg-mode-regexp-query "^\\(Query: \\)\\(.*\\)$"
  "Regexp for the Query field.")

(defvar ddg-mode-regexp-result "^\\(Result: \\)\\(.*\\)$"
  "Regexp for each Result field.")

(defvar ddg-mode-regexp-field "^[^\\*]\\{2\\}[^:]*:"
  "Regexp for matching every field label.")

(defvar ddg-mode-regexp-section "^** .*:$"
  "Regexp for matching every section.")


					; --------------------
					; Font-lock

(defvar ddg-mode-font-lock 
  (list
    ;; font-lock-keywords
    (list
     (list ddg-mode-regexp-answer 2 ''ddg-mode-face-answer)
     (list ddg-mode-regexp-result 2 ''ddg-mode-face-result)
     (list ddg-mode-regexp-query 2 ''ddg-mode-face-query)
     (list ddg-mode-regexp-section 0 ''ddg-mode-face-section t)
     (list ddg-mode-regexp-field 0 ''ddg-mode-face-field t)
     )    
    )
  ;;
  "Font lock for `ddg-mode'"
  )

(define-derived-mode ddg-mode nil "DuckDuckGo"
  "Major mode for DuckDuckGo(ddg-mode)."
  (make-local-variable 'text-mode-variant)
  (setq text-mode-variant t)
  ;; These two lines are a feature added recently.
  (set (make-local-variable 'require-final-newline)
       mode-require-final-newline)
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  ;; font lock para ej-mode
  (set (make-local-variable 'font-lock-defaults)
       ddg-mode-font-lock)
  ;; (set (make-local-variable 'font-lock-keywords)
  ;;     ddg-mode-font-lock)
  (set (make-local-variable 'buffer-read-only) t)
  )

(provide 'ddg-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ddg-mode.el ends here
