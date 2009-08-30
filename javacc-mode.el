;;;
;;* ----------------------------------------------------------------------------
;;* "THE BEER-WARE LICENSE" (Revision 42):
;;* <bwaite@connect.carleton.ca> wrote this file.
;;* As long as you retain this notice you can do whatever you want with this 
;;* stuff. If we meet some day, and you think this stuff is worth it, you can 
;;* buy me a beer in return.
;;*                                                               -Bryan Waite
;;* (lovingly stolen from the original Beer-ware license by Poul-Henning Kamp)
;;* ----------------------------------------------------------------------------
;;
;;;;Note that this has only been tested with GNU Emacs 22.1 but I assume it
;;;;will work with any version that has java-mode
;;;;Last updated Mon 25 Feb 2008 11:00:22 PM EST
(defconst javacc-mode-version "0.1")
(defconst javacc-author-name  "Bryan Waite")
(defconst javacc-author-email "bwaite@connect.carleton.ca")
(defconst javacc-web-page     "www.thesiteiwillonedayhave.com")

;;this is pretty much all I'm adding
(defconst javacc-keyword-re
  (regexp-opt '("DEBUG_PARSER" "DEBUG_LOOKAHEAD" "DEBUG_TOKEN_MANAGER"
                 "COMMON_TOKEN_ACTION" "IGNORE_CASE" "CHOICE_AMBIGUITY_CHECK"
                 "OTHER_AMBIGUITY_CHECK" "STATIC LOOKAHEAD" "ERROR_REPORTING"
                 "USER_TOKEN_MANAGER"  "USER_CHAR_STREAM" "JAVA_UNICODE_ESCAPE"
                 "UNICODE_INPUT" "LOOKAHEAD" "STATIC" "BUILD_PARSER"
                 "BUILD_TOKEN_MANAGER" "SANITY_CHECK" "FORCE_LA_CHECK"
                 "PARSER_BEGIN" "PARSER_END"
                 "TOKEN" "SKIP" "MORE" "SPECIAL_TOKEN"
                 "DEBUG" "IGNORE_IN_BNF" "MULTI" "NODE_DEFAULT_VOID"
                 "NODE_EXTENDS" "VISITOR" "options") 'words))

;;;add new words to the java keywords (TOKEN shows up the wrong colour)
(defvar javacc-font-lock-keywords 
  (append java-font-lock-keywords
          (list (list javacc-keyword-re 1 font-lock-keyword-face)))
  "Default highlighting expressions for JAVACC mode")

;;;do the deriving
(define-derived-mode javacc-mode java-mode "JavaCC"
   "A major mode for editing javacc and jjtree files."
   (set (make-local-variable 'font-lock-defaults)
 	'(javacc-font-lock-keywords)))

(provide 'javacc-mode)

;;; Also, some smart-compile entries to add to smart-compile-alist
;;;   ("\\.jj\\'" . "javacc %f")
;;;   ("\\.jjt\\'" . "jjtree %f")
 
