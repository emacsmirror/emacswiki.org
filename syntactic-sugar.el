;;; syntactic-sugar.el --- Effect-free forms such as if/then/else
;;
;; Copyright (c) 2012 Roland Walker
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/syntactic-sugar
;; URL: http://raw.github.com/rolandwalker/syntactic-sugar/master/syntactic-sugar.el
;; Version: 0.9.4
;; Last-Updated:  1 Nov 2013
;; EmacsWiki: SyntacticSugar
;; Package-Requires:
;; Keywords: extensions
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'syntactic-sugar)
;;
;;     (if t
;;         (then (message "true"))
;;       (else (message "false")))
;;
;;     (unwind-protect
;;         (protected
;;           (error "Error"))
;;       (unwind
;;         (message "cleanup")))
;;
;; Explanation
;;
;; This library offers absolutely no functionality!  The following
;; macros are provided as synonyms for `progn':
;;
;;     `then'
;;     `else'
;;     `protected'
;;     `unwind'
;;
;; These macros can be used to clarify `if' or `unwind-protect' forms.
;;
;; Note that as synonyms for `progn', these forms have no useful
;; effects, and no additional syntax check is done, so nothing
;; prevents you from writing obfuscatory expressions such as
;;
;;     (if t (else 1) (then 2))      ; same as (if t (progn 1) (progn 2))
;;
;; or idiotic expressions such as
;;
;;     (if t
;;         (protected 1)
;;       (unwind 2))
;;
;; So, think of these macros as glorified comments.  And realize that
;; if you are tempted to use them, if in fact you have even read the
;; documentation to this point, you are hopelessly impure at heart.
;;
;; To use syntactic-sugar, place the syntactic-sugar.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'syntactic-sugar)
;;
;; See Also
;;
;;     M-x customize-group RET syntactic-sugar RET
;;
;; Notes
;;
;; When this library is loaded, the provided forms are registered as
;; keywords in font-lock.  This may be disabled via customize.
;;
;; The included macros are intentionally not autoloaded, because they
;; are outside the package namespace.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.4-devel     : yes, at the time of writing
;;     GNU Emacs version 24.3           : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.2           : yes
;;     GNU Emacs version 21.x and lower : unknown
;;
;; Bugs
;;
;; TODO
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

;;; customizable variables

;;;###autoload
(defgroup syntactic-sugar nil
  "Effect-free forms for clarification"
  :version "0.9.4"
  :link '(emacs-commentary-link :tag "Commentary" "syntactic-sugar")
  :link '(url-link :tag "GitHub" "http://github.com/rolandwalker/syntactic-sugar")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/SyntacticSugar")
  :prefix "syntactic-sugar-"
  :group 'extensions)

(defcustom syntactic-sugar-add-font-lock-keywords t
  "Add syntactic-sugar macros to font-lock keywords when editing Emacs Lisp."
  :type 'boolean
  :group 'syntactic-sugar)

(when syntactic-sugar-add-font-lock-keywords
  (eval-after-load "lisp-mode"
    '(progn
       (let ((new-keywords '(
                             "then"
                             "else"
                             "protected"
                             "unwind"
                             )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                     1 font-lock-keyword-face)) 'append)))))

;;; macros

(defmacro then (&rest body)
  "A synonym for `progn' which can be used to clarify `if' forms."
  (declare (indent 0) (debug t))
  `(progn
     ,@body))

(defmacro else (&rest body)
  "A synonym for `progn' which can be used to clarify `if' forms."
  (declare (indent 0) (debug t))
  `(progn
     ,@body))

(defmacro protected (&rest body)
  "A synonym for `progn' which can be used to clarify `unwind-protect' forms."
  (declare (indent 0) (debug t))
  `(progn
     ,@body))

(defmacro unwind (&rest body)
  "A synonym for `progn' which can be used to clarify `unwind-protect' forms."
  (declare (indent 0) (debug t))
  `(progn
     ,@body))

(provide 'syntactic-sugar)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: SyntacticSugar ARGS alist devel
;;

;;; syntactic-sugar.el ends here
