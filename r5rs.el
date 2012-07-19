;;;
;;; r5rs.el --- Browse documentation from the R5RS Revised5 Report
;;;             on Schema Programing language

;; Author: Numa Tortolero <numa.tortolero@gmail.com>
;; Keywords: Scheme

;; This file is not part of GNU Emacs, but distributed under the same
;; conditions as GNU Emacs, and is useless without GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Richard Kelsey, William Clinger, and Jonathan Rees have edited
;; the Scheme R5RS: the Revised5 Report on the Algorithmic Language
;; Schema. This package makes it convenient to peruse this documentation
;; from within Emacs.
;;
;; Installation:
;; Put this file in some directory of your system, preferibly in one
;; included in the load-path list; say "~/.emacs.d". Else, you have to 
;; add the directory to your load-path list:
;;
;; (add-to-list 'load-path "<path_where_you_put_this_file>"
;;
;; Next, add to your emacs init file (~/.emacs or wherever) the next lines
;;
;; (require 'r5rs.el)
;; (global-set-key [(control ?.) ?h] 'scheme-r5rs-lookup)
;;
;; Now, when you are writing scheme code, you can type 'control "." "h"',
;; type the scheme word which info you want to get and finally enter... 
;; your web browser will pop up with the info about the symbol that you 
;; have requested.
;;
;; You can download the R5RS html files and save it in your disk. Then,
;; you have to add the next line to your emacs init file:
;;
;; (setq scheme-r5rs-root "<path_where_you_saved_r5rs_web_files>")
;;
;; Then you can type "Ctl-. h" and next the scheme reserved word that you
;; need lookup and voila!
;;
;; Example:
;;
;; I've copied the r5rs.el file in "~/.local/share/emacs/site-lisp".
;; I've installed the R5RS files in "~/.local/share/r5rs/", and I've added
;; to my ~/.emacs the next lines:
;;
;; (add-to-list 'loadpath 
;;	     (expand-file-name "~/.local/share/emacs/site-lisp/"))
;; (defvar scheme-r5rs-root
;;   (concat "file:/" (expand-file-name "~/.local/share/r5rs/")))
;; (require 'r5rs)
;; (global-set-key [(control ?.) ?h] 'scheme-r5rs-lookup)
;;
;; This file is a rewriting of hyperspec.el, fixing the code to work for 
;; scheme, using the R5RS specification. I wait that be usefull for you!
;;
;;
;; Numa Tortolero
;; Universidad Simón Bolívar
;; Caracas, Venezuela
;;

;;; Code:

(require 'cl)
(require 'browse-url)			; you need the Emacs 20 version or major
(require 'thingatpt)

(defvar scheme-r5rs-root
  "http://www.schemers.org/Documents/Standards/R5RS/HTML/"
; file:/usr/local/doc/r5rs/"
  "The root of the Scheme R5rs URL.
If you copy the R5RS to your local system, set this variable to
something like \"file:/usr/local/doc/R5RS/\".")

(defvar scheme-r5rs-history nil
  "History of symbols looked up in the Scheme R5rs.")

;; if only we had had packages or hash tables..., but let's fake it.
(defvar scheme-r5rs-symbols (make-vector 67 0))

(defun scheme-r5rs-lookup (symbol-name)
  "View the documentation on SYMBOL-NAME from the Scheme R5rs.
If SYMBOL-NAME has more than one definition, all of them are displayed with
your favorite browser in sequence.  The browser should have a \"back\"
function to view the separate definitions.
The Scheme R5RS is the Revised5 Report on the Algorithmic Language Schema, provided
by Richard Kelsey, William Clinger, and Jonathan Rees (Editors).
The report gives a defining description of the programming language Scheme. 
Scheme is a statically scoped and properly tail-recursive dialect of the Lisp 
programming language invented by Guy Lewis Steele Jr. and Gerald Jay Sussman. 
It was designed to have an exceptionally clear and simple semantics and few 
different ways to form expressions.
By default, the R5RS WWW site is visited to retrieve the information.
If you copy the R5rs to another location, customize the
variable `scheme-r5rs-root' to point to that location."
  (interactive (list (let ((symbol-at-point (thing-at-point 'symbol)))
		       (if (and symbol-at-point
				(intern-soft (downcase symbol-at-point)
					     scheme-r5rs-symbols))
			   symbol-at-point
		       (completing-read
			"Look up symbol in Scheme R5RS: "
			scheme-r5rs-symbols #'boundp
			t symbol-at-point
			'scheme-r5rs-history)))))
  (maplist (lambda (entry)
	     (browse-url (concat scheme-r5rs-root "" (car entry))
			 browse-url-new-window-flag)
	   (if (cdr entry)
	       (sleep-for 1.5)))
	   (let ((symbol (intern-soft (downcase symbol-name)
				    scheme-r5rs-symbols)))
	     (if (and symbol (boundp symbol))
		 (symbol-value symbol)
	       (error "The symbol `%s' is not defined in Scheme"
		    symbol-name)))))

(mapc (lambda (entry)
	  (let ((symbol (intern (car entry) scheme-r5rs-symbols)))
	    (if (boundp symbol)
	      (push (cadr entry) (symbol-value symbol))
	      (set symbol (cdr entry)))))
	'(("..." "r5rs-Z-H-7.html#%_idx_186")
	  ("*" "r5rs-Z-H-9.html#%_idx_280")
	  ("'" "r5rs-Z-H-7.html#%_idx_88")
	  ("lookup-data" )
	  ("r5rs-Z-H-15.html" )
	  ("scm-lookup-data.scm" )
	  ("+" "r5rs-Z-H-9.html#%_idx_278")
	  ("," "r5rs-Z-H-7.html#%_idx_156")
	  (",@" "r5rs-Z-H-7.html#%_idx_158")
	  ("-" "r5rs-Z-H-9.html#%_idx_282")
	  ("/" "r5rs-Z-H-9.html#%_idx_284")
	  (";" "r5rs-Z-H-5.html#%_idx_24")
	  ("&lt;" "r5rs-Z-H-9.html#%_idx_256")
	  ("&lt;=" "r5rs-Z-H-9.html#%_idx_260")
	  ("=" "r5rs-Z-H-9.html#%_idx_254")
	  ("=&gt;" "r5rs-Z-H-7.html#%_idx_110")
	  ("&gt;" "r5rs-Z-H-9.html#%_idx_258")
	  ("&gt;=" "r5rs-Z-H-9.html#%_idx_262")
	  ("`" "r5rs-Z-H-7.html#%_idx_160")
	  ("abs" "r5rs-Z-H-9.html#%_idx_286")
	  ("acos" "r5rs-Z-H-9.html#%_idx_326")
	  ("and" "r5rs-Z-H-7.html#%_idx_118")
	  ("angle" "r5rs-Z-H-9.html#%_idx_344")
	  ("append" "r5rs-Z-H-9.html#%_idx_420")
	  ("apply" "r5rs-Z-H-9.html#%_idx_556")
	  ("asin" "r5rs-Z-H-9.html#%_idx_324")
	  ("assoc" "r5rs-Z-H-9.html#%_idx_438")
	  ("assq" "r5rs-Z-H-9.html#%_idx_434")
	  ("assv" "r5rs-Z-H-9.html#%_idx_436")
	  ("atan" "r5rs-Z-H-9.html#%_idx_328")
	  ("#b" "r5rs-Z-H-9.html#%_idx_228")
	  ("begin" "r5rs-Z-H-7.html#%_idx_136")
	  ("boolean?" "r5rs-Z-H-6.html#%_idx_46")
	  ("caar" "r5rs-Z-H-9.html#%_idx_402")
	  ("cadr" "r5rs-Z-H-9.html#%_idx_404")
	  ("call-with-current-continuation" "r5rs-Z-H-9.html#%_idx_566")
	  ("call-with-input-file" "r5rs-Z-H-9.html#%_idx_588")
	  ("call-with-output-file" "r5rs-Z-H-9.html#%_idx_590")
	  ("call-with-values" "r5rs-Z-H-9.html#%_idx_574")
	  ("car" "r5rs-Z-H-9.html#%_idx_392")
	  ("case" "r5rs-Z-H-7.html#%_idx_114")
	  ("cdddar" "r5rs-Z-H-9.html#%_idx_406")
	  ("cddddr" "r5rs-Z-H-9.html#%_idx_408")
	  ("cdr" "r5rs-Z-H-9.html#%_idx_396")
	  ("ceiling" "r5rs-Z-H-9.html#%_idx_304")
	  ("char-&gt;integer" "r5rs-Z-H-9.html#%_idx_480")
	  ("char-alphabetic?" "r5rs-Z-H-9.html#%_idx_470")
	  ("char-ci&lt;=?" "r5rs-Z-H-9.html#%_idx_466")
	  ("char-ci&lt;?" "r5rs-Z-H-9.html#%_idx_462")
	  ("char-ci=?" "r5rs-Z-H-9.html#%_idx_460")
	  ("char-ci&gt;=?" "r5rs-Z-H-9.html#%_idx_468")
	  ("char-ci&gt;?" "r5rs-Z-H-9.html#%_idx_464")
	  ("char-downcase" "r5rs-Z-H-9.html#%_idx_486")
	  ("char-lower-case?" "r5rs-Z-H-9.html#%_idx_478")
	  ("char-numeric?" "r5rs-Z-H-9.html#%_idx_472")
	  ("char-ready?" "r5rs-Z-H-9.html#%_idx_620")
	  ("char-upcase" "r5rs-Z-H-9.html#%_idx_484")
	  ("char-upper-case?" "r5rs-Z-H-9.html#%_idx_476")
	  ("char-whitespace?" "r5rs-Z-H-9.html#%_idx_474")
	  ("char&lt;=?" "r5rs-Z-H-9.html#%_idx_456")
	  ("char&lt;?" "r5rs-Z-H-9.html#%_idx_452")
	  ("char=?" "r5rs-Z-H-9.html#%_idx_450")
	  ("char&gt;=?" "r5rs-Z-H-9.html#%_idx_458")
	  ("char&gt;?" "r5rs-Z-H-9.html#%_idx_454")
	  ("char?" "r5rs-Z-H-6.html#%_idx_54")
	  ("close-input-port" "r5rs-Z-H-9.html#%_idx_608")
	  ("close-output-port" "r5rs-Z-H-9.html#%_idx_610")
	  ("complex?" "r5rs-Z-H-9.html#%_idx_242")
	  ("cond" "r5rs-Z-H-7.html#%_idx_106")
	  ("cons" "r5rs-Z-H-9.html#%_idx_390")
	  ("cos" "r5rs-Z-H-9.html#%_idx_320")
	  ("current-input-port" "r5rs-Z-H-9.html#%_idx_596")
	  ("current-output-port" "r5rs-Z-H-9.html#%_idx_598")
	  ("#d" "r5rs-Z-H-9.html#%_idx_232")
	  ("define" "r5rs-Z-H-8.html#%_idx_190")
	  ("define-syntax" "r5rs-Z-H-8.html#%_idx_198")
	  ("delay" "r5rs-Z-H-7.html#%_idx_142")
	  ("denominator" "r5rs-Z-H-9.html#%_idx_300")
	  ("display" "r5rs-Z-H-9.html#%_idx_624")
	  ("do" "r5rs-Z-H-7.html#%_idx_138")
	  ("dynamic-wind" "r5rs-Z-H-9.html#%_idx_576")
	  ("#e" "r5rs-Z-H-9.html#%_idx_236")
	  ("else" "r5rs-Z-H-7.html#%_idx_108")
	  ("eof-object?" "r5rs-Z-H-9.html#%_idx_618")
	  ("eq?" "r5rs-Z-H-9.html#%_idx_216")
	  ("equal?" "r5rs-Z-H-9.html#%_idx_218")
	  ("eqv?" "r5rs-Z-H-9.html#%_idx_210")
	  ("eval" "r5rs-Z-H-9.html#%_idx_578")
	  ("even?" "r5rs-Z-H-9.html#%_idx_272")
	  ("exact-&gt;inexact" "r5rs-Z-H-9.html#%_idx_346")
	  ("exact?" "r5rs-Z-H-9.html#%_idx_250")
	  ("exp" "r5rs-Z-H-9.html#%_idx_314")
	  ("expt" "r5rs-Z-H-9.html#%_idx_332")
	  ("#f" "r5rs-Z-H-9.html#%_idx_356")
	  ("floor" "r5rs-Z-H-9.html#%_idx_302")
	  ("for-each" "r5rs-Z-H-9.html#%_idx_560")
	  ("force" "r5rs-Z-H-9.html#%_idx_562")
	  ("gcd" "r5rs-Z-H-9.html#%_idx_294")
	  ("#i" "r5rs-Z-H-9.html#%_idx_238")
	  ("if" "r5rs-Z-H-7.html#%_idx_98")
	  ("imag-part" "r5rs-Z-H-9.html#%_idx_340")
	  ("inexact-&gt;exact" "r5rs-Z-H-9.html#%_idx_348")
	  ("inexact?" "r5rs-Z-H-9.html#%_idx_252")
	  ("input-port?" "r5rs-Z-H-9.html#%_idx_592")
	  ("integer-&gt;char" "r5rs-Z-H-9.html#%_idx_482")
	  ("integer?" "r5rs-Z-H-9.html#%_idx_248")
	  ("interaction-environment" "r5rs-Z-H-9.html#%_idx_584")
	  ("lambda" "r5rs-Z-H-7.html#%_idx_96")
	  ("lcm" "r5rs-Z-H-9.html#%_idx_296")
	  ("length" "r5rs-Z-H-9.html#%_idx_418")
	  ("let" "r5rs-Z-H-7.html#%_idx_124")
	  ("let*" "r5rs-Z-H-7.html#%_idx_124")
	  ("let-syntax" "r5rs-Z-H-7.html#%_idx_180")
	  ("letrec" "r5rs-Z-H-7.html#%_idx_132")
	  ("letrec-syntax" "r5rs-Z-H-7.html#%_idx_182")
	  ("list" "r5rs-Z-H-9.html#%_idx_416")
	  ("list-&gt;string" "r5rs-Z-H-9.html#%_idx_528")
	  ("list-&gt;vector" "r5rs-Z-H-9.html#%_idx_550")
	  ("list-ref" "r5rs-Z-H-9.html#%_idx_426")
	  ("list-tail" "r5rs-Z-H-9.html#%_idx_424")
	  ("list?" "r5rs-Z-H-9.html#%_idx_414")
	  ("load" "r5rs-Z-H-9.html#%_idx_630")
	  ("log" "r5rs-Z-H-9.html#%_idx_316")
	  ("magnitude" "r5rs-Z-H-9.html#%_idx_342")
	  ("make-polar" "r5rs-Z-H-9.html#%_idx_336")
	  ("make-rectangular" "r5rs-Z-H-9.html#%_idx_334")
	  ("make-string" "r5rs-Z-H-9.html#%_idx_492")
	  ("make-vector" "r5rs-Z-H-9.html#%_idx_538")
	  ("map" "r5rs-Z-H-9.html#%_idx_558")
	  ("max" "r5rs-Z-H-9.html#%_idx_274")
	  ("member" "r5rs-Z-H-9.html#%_idx_432")
	  ("memq" "r5rs-Z-H-9.html#%_idx_428")
	  ("memv" "r5rs-Z-H-9.html#%_idx_430")
	  ("min" "r5rs-Z-H-9.html#%_idx_276")
	  ("modulo" "r5rs-Z-H-9.html#%_idx_292")
	  ("negative?" "r5rs-Z-H-9.html#%_idx_268")
	  ("newline" "r5rs-Z-H-9.html#%_idx_626")
	  ("not" "r5rs-Z-H-9.html#%_idx_368")
	  ("null-environment" "r5rs-Z-H-9.html#%_idx_582")
	  ("null?" "r5rs-Z-H-9.html#%_idx_410")
	  ("number-&gt;string" "r5rs-Z-H-9.html#%_idx_350")
	  ("number?" "r5rs-Z-H-6.html#%_idx_52")
	  ("numerator" "r5rs-Z-H-9.html#%_idx_298")
	  ("#o" "r5rs-Z-H-9.html#%_idx_230")
	  ("odd?" "r5rs-Z-H-9.html#%_idx_270")
	  ("open-input-file" "r5rs-Z-H-9.html#%_idx_604")
	  ("open-output-file" "r5rs-Z-H-9.html#%_idx_606")
	  ("or" "r5rs-Z-H-7.html#%_idx_120")
	  ("output-port?" "r5rs-Z-H-9.html#%_idx_594")
	  ("pair?" "r5rs-Z-H-6.html#%_idx_48")
	  ("peek-char" "r5rs-Z-H-9.html#%_idx_616")
	  ("port?" "r5rs-Z-H-6.html#%_idx_60")
	  ("positive?" "r5rs-Z-H-9.html#%_idx_266")
	  ("procedure?" "r5rs-Z-H-6.html#%_idx_62")
	  ("quasiquote" "r5rs-Z-H-7.html#%_idx_150")
	  ("quote" "r5rs-Z-H-7.html#%_idx_86")
	  ("quotient" "r5rs-Z-H-9.html#%_idx_288")
	  ("rational?" "r5rs-Z-H-9.html#%_idx_246")
	  ("rationalize" "r5rs-Z-H-9.html#%_idx_310")
	  ("read" "r5rs-Z-H-9.html#%_idx_612")
	  ("read-char" "r5rs-Z-H-9.html#%_idx_614")
	  ("real-part" "r5rs-Z-H-9.html#%_idx_338")
	  ("real?" "r5rs-Z-H-9.html#%_idx_244")
	  ("remainder" "r5rs-Z-H-9.html#%_idx_290")
	  ("reverse" "r5rs-Z-H-9.html#%_idx_422")
	  ("round" "r5rs-Z-H-9.html#%_idx_308")
	  ("scheme-report-environment" "r5rs-Z-H-9.html#%_idx_580")
	  ("set!" "r5rs-Z-H-7.html#%_idx_102")
	  ("set-car!" "r5rs-Z-H-9.html#%_idx_398")
	  ("set-cdr!" "r5rs-Z-H-9.html#%_idx_400")
	  ("setcar" "r5rs-Z-H-10.html#%_idx_644")
	  ("sin" "r5rs-Z-H-9.html#%_idx_318")
	  ("sqrt" "r5rs-Z-H-9.html#%_idx_330")
	  ("string" "r5rs-Z-H-9.html#%_idx_494")
	  ("string-&gt;list" "r5rs-Z-H-9.html#%_idx_526")
	  ("string-&gt;number" "r5rs-Z-H-9.html#%_idx_352")
	  ("string-&gt;symbol" "r5rs-Z-H-9.html#%_idx_446")
	  ("string-append" "r5rs-Z-H-9.html#%_idx_524")
	  ("string-ci&lt;=?" "r5rs-Z-H-9.html#%_idx_518")
	  ("string-ci&lt;?" "r5rs-Z-H-9.html#%_idx_514")
	  ("string-ci=?" "r5rs-Z-H-9.html#%_idx_504")
	  ("string-ci&gt;=?" "r5rs-Z-H-9.html#%_idx_520")
	  ("string-ci&gt;?" "r5rs-Z-H-9.html#%_idx_516")
	  ("string-copy" "r5rs-Z-H-9.html#%_idx_530")
	  ("string-fill!" "r5rs-Z-H-9.html#%_idx_532")
	  ("string-length" "r5rs-Z-H-9.html#%_idx_496")
	  ("string-ref" "r5rs-Z-H-9.html#%_idx_498")
	  ("string-set!" "r5rs-Z-H-9.html#%_idx_500")
	  ("string&lt;=?" "r5rs-Z-H-9.html#%_idx_510")
	  ("string&lt;?" "r5rs-Z-H-9.html#%_idx_506")
	  ("string=?" "r5rs-Z-H-9.html#%_idx_502")
	  ("string&gt;=?" "r5rs-Z-H-9.html#%_idx_512")
	  ("string&gt;?" "r5rs-Z-H-9.html#%_idx_508")
	  ("string?" "r5rs-Z-H-6.html#%_idx_56")
	  ("substring" "r5rs-Z-H-9.html#%_idx_522")
	  ("symbol-&gt;string" "r5rs-Z-H-9.html#%_idx_444")
	  ("symbol?" "r5rs-Z-H-6.html#%_idx_50")
	  ("syntax-rules" "r5rs-Z-H-7.html#%_idx_184")
	  ("#t" "r5rs-Z-H-9.html#%_idx_354")
	  ("tan" "r5rs-Z-H-9.html#%_idx_322")
	  ("transcript-off" "r5rs-Z-H-9.html#%_idx_634")
	  ("transcript-on" "r5rs-Z-H-9.html#%_idx_632")
	  ("truncate" "r5rs-Z-H-9.html#%_idx_306")
	  ("values" "r5rs-Z-H-9.html#%_idx_572")
	  ("vector" "r5rs-Z-H-9.html#%_idx_540")
	  ("vector-&gt;list" "r5rs-Z-H-9.html#%_idx_548")
	  ("vector-fill!" "r5rs-Z-H-9.html#%_idx_552")
	  ("vector-length" "r5rs-Z-H-9.html#%_idx_542")
	  ("vector-ref" "r5rs-Z-H-9.html#%_idx_544")
	  ("vector-set!" "r5rs-Z-H-9.html#%_idx_546")
	  ("vector?" "r5rs-Z-H-6.html#%_idx_58")
	  ("with-input-from-file" "r5rs-Z-H-9.html#%_idx_600")
	  ("with-output-to-file" "r5rs-Z-H-9.html#%_idx_602")
	  ("write" "r5rs-Z-H-9.html#%_idx_622")
	  ("write-char" "r5rs-Z-H-9.html#%_idx_628")
	  ("#x" "r5rs-Z-H-9.html#%_idx_234")
	  ("zero?" "r5rs-Z-H-9.html#%_idx_264"))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'r5rs)

;;; r5rs.el ends here
