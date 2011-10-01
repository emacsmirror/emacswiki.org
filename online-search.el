;;; online-search.el --- Online dictionary or intelligent api search engine for developers
;; -*- mode: EMACS-LISP; -*-
;;; ================================================================
;; Copyright © 2010-2011 Denny Zhang
;;; ================================================================

;;; online-search.el --- Emacs interface to the Online Search
;;;
;;; Author: Denny Zhang <markfilebat@126.com>
;;; Keywords: search
;;; Version: 0.1
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, ShangHai, China
;;
;; Please send suggestions and bug reports to
;; <markfilebat@126.com>. The latest version of this package
;; should be available from
;;
;; <URL:http://www.emacswiki.org/emacs/OnlineSearch/>

;;; Overview ==========================================================
;;
;; This module enable us to do intelligent search.
;; - The default behaviour actives as online dictionary. Currently, it's Chinese to English dictionary.
;; - For different programming lanuages, it enables us to query api from official websites
;; Currently, we support query for C++, PYTHON, PHP, etc.
;;
;; There are the ways to display the result: w3m mode and plain text mode.
;; Normally, we use w3m, displaying result in a *w3m* buffer.
;; However, w3m is not widely available in Windows OS.
;; Thus, users to downgrade to plain text, by configure the variable of is-w3m-enable

;;; INSTALLATION =====================================================
;;
;; (load-file "directory_to_be_decided/online-search.el") ;; Notice: Change to correct value
;; (global-set-key [(control c) (s)] 'online-search)
;; ;; If current environment is windows, w3m may be probably not available.
;; ;; In this case downgrade from w3m mode to plaintext mode.
;; (setq is-plaintext-enable 't))
;;
;; § -------------------------- separator --------------------------

;;; FLY TRY =====================================================
;; Move cursor to one word, and press C-c s, to check it out

;;; Code:

;; define data structure of constructure
(defstruct online-search-struct
  (search-engine-url-format "http://www.iciba.com/")
  (start-anchor-str "") ;; filter content
  (end-anchor-str "更多网络释义")
  )

;; configuration of search engine
(setq online-search-configuration (make-online-search-struct))

;; configure below variable as not nil, if w3m tool is available
(setq is-plaintext-enable nil)

;; replace rule list
(setq replace-rule-list
      '(("<[^>]+>" "")
        ("&nbsp;" " ")
        (" " "")
        ("\r" "")
        (" +$" "")
        ("^$\r" "\r")
        ))

;;;###autoload
(defun online-search (word)
  ;; online search for dictionary, api query etc.
  (interactive (list (read-string "Online lookup for: " (current-word))))
  (if (not (stringp mode-name)) return)

  ;; default behavior
  ;; online dictionary, using 金山词霸
  (setq online-search-configuration
        (make-online-search-struct
         :search-engine-url-format "http://www.iciba.com/"
         :start-anchor-str ""
         :end-anchor-str "更多网络释义")
        )

  ;; search php api, in php mode
  (if (string-equal mode-name "PHP")
      (setq online-search-configuration
            (make-online-search-struct
             :search-engine-url-format "http://cn.php.net/manual/en/function."
             :start-anchor-str ""
             :end-anchor-str "")))

  ;; search c/c++ api, in c/c++ mode
  (if (or (string-equal mode-name "C/l") (string-equal mode-name "C++/l"))
      (setq online-search-configuration
            (make-online-search-struct
             :search-engine-url-format "http://www.cplusplus.com/search.do?q="
             :start-anchor-str ""
             :end-anchor-str "")))

  ;; perform the actual operation
  (online-search-word word)
  )

(defun online-search-word (word)
  "Lookup a word or phrase in the Online Server."
  (interactive (list (read-string "Online lookup for: " (current-word))))
  (if is-plaintext-enable
      (display-by-plaintext word)
    (display-by-w3m word))
  )

(defun display-by-w3m(word)
  "Display result in way of *w3m*"
  (save-excursion
    (setq search-url (concat (online-search-struct-search-engine-url-format online-search-configuration) word))
    (w3m (concat search-url))
    ;; TODO only open a seperate buffer
    (get-buffer "*w3m*")
    (goto-char (point-min))
    ;; TODO: problemetic, move cursor to precise location
    (if (re-search-forward (online-search-struct-start-anchor-str online-search-configuration) nil t)
        (goto-char (match-end 0)))
    )
  )

(defun display-by-plaintext(search-url)
  "Display result as plain text, since w3m is not widely available in Windows platform
 This is done by invoke http request and extract plain text from html code"
  (let ((buf (get-buffer-create "*dict*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dict-fetch-plaintext word)
    (dict-wash-plaintext)
    (set-buffer-modified-p nil)))

(defun dict-fetch-plaintext (word)
  "Send http request to retrieve related html content"
  (require 'url)
  (setq search-url (concat (online-search-struct-search-engine-url-format online-search-configuration ) word))
  (url-insert-file-contents search-url)
  (if (string= (online-search-struct-start-anchor-str online-search-configuration) "")
      (setf (online-search-struct-start-anchor-str online-search-configuration) word)
    ))

(defun dict-wash-plaintext ()
  "Wash content by remove or replace useless characters"
  (goto-char (point-min))
  (let ((case-fold-search t))
    ;; limit content
    (when (re-search-forward "</form>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    ;; wash content by replacing
    (while (> (length replace-rule-list) 0)
      (progn
        (setq replace-rule (pop replace-rule-list))
        (goto-char (point-min))
        (while (re-search-forward (nth 0 replace-rule) nil t)
          (replace-match (nth 1 replace-rule) t))
        )
      )
    ;; limit content
    (goto-char (point-min))
    (when (re-search-forward (online-search-struct-start-anchor-str online-search-configuration) nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward (online-search-struct-end-anchor-str online-search-configuration) nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min)))
  )

(provide 'online-search)

;; online-search.el end
