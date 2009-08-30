;;;----------------------------------------------------------------------
;; cm-string.el
;; Primary Author: Mike Mattie
;; Copyright (C) 2009 Mike Mattie
;; License: LGPL-v3
;;;----------------------------------------------------------------------

(defun bracket-strings ( bracket list )
  (apply 'concat bracket (prefix-strings bracket list) bracket))

(defun prefix-strings ( prefix list )
  "prefix-strings PREFIX LIST

   transform LIST concatenating the strings with PREFIX."
  (mapcar
    (lambda ( string )
      (concat prefix string))
    list))

(defun string-join (prefix list)
  ;; This is analogous to the perl5 join function.
  ;; given a <prefix> and a <list> of strings join the
  ;; strings with <prefix> as a seperator between the
  ;; list values.
  ;;
  ;; The result is a single string value.
  (apply 'concat
    (car list)
    (if (cdr list) (prefix-strings prefix (cdr list))) ))

(provide 'cm-string)
