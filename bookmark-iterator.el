;;; bookmark-iterator.el --- iterate through tags - similar to DevStudio
;; Author: Patrick Anderson 
;; Version: 1
;; This is free software
;; License: GPL

;install:
;this file in your load path

;add
; (require 'etags-iterator)
;to your .emacs file

;execute
; M-x eval-buffer
;so you don't have to restart

;todo:
;interface/parse with Micropoly's .bsc format

(defun etags ()
  "shell out etags.exe"
  (interactive)
  (shell-command "etags *")
  (require 'etags)
  (tags-reset-tags-tables))

(defun find-tag-current-word()
  "find the tag for the word currently under the cursor.
if none is found, call etags"
  (interactive)
  (find-tag (current-word)))

	
(define-key global-map [(control f12)] 'find-tag-current-word)
(define-key global-map [(f12)] '(lambda () "find next tag" (interactive) (execute-kbd-macro "\C-u\256")))
(define-key global-map [(shift f12)] '(lambda () "find prev tag" (interactive) (execute-kbd-macro "\C-u-\256")))
(define-key global-map [(shift control f12)] 'select-tags-table)
(define-key global-map [(meta control f12)] 'tags-reset-tags-tables)
(define-key global-map [(f4)] 'tags-loop-continue)

(provide 'etags-iterator)
(provide 'bookmark-iterator)
;;; bookmark-iterator.el ends here
