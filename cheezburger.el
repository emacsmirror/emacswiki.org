;;; cheezburger.el --- ICanHasCheezburger.com viewer for emacs!

;; Copyright (C) 2008 Zachary McGrew 

;; Authors: Zachary McGrew <zmcgrew @-A-T-@ lunar-linux.org>
;;          Tabitha Tipper <bluefoo at googlemail dot com>
;; Keywords: cheezburger, lolcat
;; Created: 19 Nov 2008

;; Version: 1.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x cheezburger
;; Now with 100% more cheezy goodness!

;; This is my first experiment with elisp. Comments and criticisms (And patches) welcome!

;;; Future Ideas:

;; Offer to open videos in a browser
;; Add support for the rest of the I Can LOL network
;; Save the first image, and inform the user when they reach where they started last session

;;; Compatibility:

;; Only tested on GNU Emacs 23.0.60.1
;; Should work on 21+ if you have json and url

;;; Changelog:

;; 1.0 Initial release
;; 1.1 11/20/2008 ZM: Added "P" key binding for manually selecting a page
;;     11/20/2008 ZM: Added "b" key to browse to the blog post
;;     11/20/2008 PT: Changes to the internal structure, added title display
;;                    and image caching.
;; 1.2 18/12/2008 PT: Fixed cheez-regex-replace to use with-temp-buffer, added
;;                    another entity to cheez-parse-title

;;; Code:

(require 'json)
(require 'url)
(require 'cl)

(defun cheez-setup-keymap ()
  (setq cheez-map (make-sparse-keymap))
  (suppress-keymap cheez-map)
  (define-key cheez-map "n" 'cheezburger-next-image)
  (define-key cheez-map "p" 'cheezburger-prev-image)
  (define-key cheez-map "P" 'cheezburger-set-page)
  (define-key cheez-map "i" 'cheezburger-kill-image-url)
  (define-key cheez-map "u" 'cheezburger-kill-url)
  (define-key cheez-map "b" 'cheezburger-browse-url)
  (define-key cheez-map "q" 'cheezburger-quit)
  (use-local-map cheez-map))

(define-derived-mode cheezburger-mode nil "cheez"
  "Major mode for I Can Has Cheezburger.
          \\{cheez-map}"
  (make-local-variable 'cheez-page)
  (make-local-variable 'cheez-current-image-num)
  (make-local-variable 'cheez-num-images)
  (make-local-variable 'cheez-data)
  (make-local-variable 'cheez-image-data)
  (make-local-variable 'cheez-ignore-pattern)
  (make-local-variable 'cheez-title-parser)
  (make-local-variable 'cheez-image-cache)

  (setq cheez-page 1)
  (setq cheez-current-image-num 0)
  (setq cheez-num-images -1)
  (setq cheez-data nil)
  (setq cheez-image-data nil)
  (setq cheez-ignore-pattern ".*/wp-content/.*")
  (setq cheez-title-parser #'cheez-parse-title)
  (setq cheez-image-cache (make-hash-table :test #'equal))
  
  (cheez-setup-keymap))


(defun cheezburger ()
  (interactive)

  ;; Create the buffer to display the image
  (get-buffer-create "*cheezburger*")
  (switch-to-buffer "*cheezburger*")
  (cheezburger-mode)

  ;; get page data and display first lol
  (cheezburger-get-data)  
  (cheez-disp-lol 0))


(defun cheezburger-get-data () 
  "Get data from the icanlol.com server, parse it, and prepare to show images.  Warning: Side effects"
  (let ((cheez-json
         (save-excursion
           (set-buffer (url-retrieve-synchronously (format "http://www.icanlol.com/cheezburger.php?page=%d" cheez-page)))
           (progn (goto-char (point-min))
                  (delete-region (point-min) (search-forward "\n\n"))
                  (buffer-substring (point-min) (point-max))))))

    ;; Really this should calculate the skip value using cheez-num-generator
    (setq cheez-current-image-num 0)
    (setq cheez-data (json-read-from-string cheez-json))
    (setq cheez-num-images (1- (length cheez-data)))))


(defun cheezburger-next-image ()
  "Fetch and display the next image"
  (interactive)

  (let* ((new-num (cheez-num-generator 'next 
                                       cheez-data 
                                       cheez-current-image-num)))
    (cheez-disp-lol new-num)
    (setq cheez-current-image-num new-num)))


(defun cheezburger-prev-image ()
  "Fetch and display the previous image"
  (interactive)
  (let* ((new-num (cheez-num-generator 'prev 
                                       cheez-data 
                                       cheez-current-image-num)))
    (cheez-disp-lol new-num)
    (setq cheez-current-image-num new-num)))


(defun cheez-get-details (data number)
  "Given the page data and an image number this returns a list of (url title).  It never skips patterns, use cheez-generate-num for that"

  (let ((url (cdr (assoc 'url (aref data number))))
        (title (cdr (assoc 'title (aref data number)))))
    
    ;; Return the details
    (list url title)))


(defun cheez-regex-replace (source find replace)
  "Oh gods this hurts, why doesn't Emacs include the clpcre library?

In short give a source string it will check it for the 'find' string and replace instances of 'find' with 'replace'.  Find can be Elisp Regexps.  It returns the modified source string.

Its internals aren't very nice at all, there has to be an easier way I don't know about."
  (with-temp-buffer
    (insert source)
    (goto-char 0)
    (while (re-search-forward find nil t)
      (replace-match replace nil nil))
    (buffer-string)))

(defun cheez-parse-title (title)
  "Parses the title badly, really should be using htmlr or pushing it through w3m or something, but its better than nothing"

  (mapcar (lambda (pair)
            (let ((find (nth 0 pair))
                  (replace (nth 1 pair)))
              (setq title (cheez-regex-replace title find replace))))

          '(("&amp;" "&")
            ("&nbsp;" " ")
            ("&#8217;" "'")
            ("&#8230;" "...")
            ("&#8220;" "\"")
            ("&#8221;" "\"")))
  title)


(defun lolzy-p (data number)
  "Given page data and a number return boolean t/nil if that data is lolzy or video or other undisplayable stuff"
  (let ((url (nth 0 (cheez-get-details data number))))
    (if (string-match-p cheez-ignore-pattern url)
        nil
      t)))


(defun cheez-num-generator (direction data number)
  "Give it a prev/next direction, the data structure usually cheez-data and the current number your on, returns the next number in sequence, taking into account skipping images and start/end of pages

This function has too many side effects"

  (cond ((equalp direction 'next)

         ;; If we're going forward check that we're inside the page limit
         (if (>=  number cheez-num-images)
             
             ;; If we're more over it then bump the page count and get
             ;; the new details
             (progn 
               (incf cheez-page)
               (cheezburger-get-data)
               
               ;; Deal with the side effects of cheezburger-get-data
               (setq number cheez-current-image-num)
               (setq data cheez-data)
               
               ;; Recurse to find next lolzy number
               (while (not (lolzy-p data number))
                 (setq number (cheez-num-generator direction data number))))

           
           ;; If we're inside the page limit then just bump the number
           (progn 
             (incf number)
             
             ;; And check its lolzyness
             (while (not (lolzy-p data number))
               (setq number (cheez-num-generator direction data number))))))
        

        ;; If we're going backwards...
        ((equalp direction 'prev)
         (if (<= number 0)

             (progn
               ;;Decrement cheez-page and fetch previous batch of data
               (decf cheez-page)

               ;; Make sure not to loop the first page... this could
               ;; be neater
               (if (< cheez-page 1)
                   (progn
                     (setq cheez-page 1)
                     (message "Already at the first page!")

                     (cheezburger-get-data)

                     ;; Note we don't go to the last image on the first page
                     (setq number 0)
                     (setq data cheez-data))

                 ;; If not actually go backwards, remember to go to
                 ;; the last image of the page.
                 (progn 
                   (cheezburger-get-data)

                   (setq number (1- (length cheez-data)))
                   (setq data cheez-data)))

               ;; Recuse to find next lolzy number
               (while (not (lolzy-p data number))
                 (setq number (cheez-num-generator direction data number))))

           ;; Otherwise just decrement the number
           (progn
             (decf number)

             (while (not (lolzy-p data number))
               (setq number (cheez-num-generator direction data number)))))))

  ;; Return 
  number)


(defun cheez-disp-lol (number)
  "Given a number it blanks the current buffer, gets the lol from the cheez-data and displays it"
  (let* ((data (cheez-get-details cheez-data number))
         (url (nth 0 data))
         (title (nth 1 data)))

    ;; Clear up the buffer, print the image then the title
    (erase-buffer)
    (insert (format "\n%s\n\n" (funcall cheez-title-parser title)))
    (cheezburger-disp-image url)))


(defun cheez-fetch-image (url)
  (let ((data (save-excursion
                (set-buffer (url-retrieve-synchronously url))
                (progn (goto-char (point-min))
                       (delete-region (point-min) (search-forward "\n\n"))
                       (buffer-substring (point-min) (point-max))))))
    data))


(defun cheez-cache-check (url)
  "Check the cache for the url and return either the data or nil"
  (let ((cache-hit (gethash url cheez-image-cache)))
    (if cache-hit cache-hit nil)))


(defun cheez-cache-add (url data)
  "Add the data to the url reference in the image cache"
  (puthash url cheez-image-data cheez-image-cache))


(defun cheezburger-disp-image (url)
  "Fetch and display the current image in the buffer"

  (let* ((cheez-cache-possible (cheez-cache-check url))
         (cheez-image-data (if cheez-cache-possible
                               cheez-cache-possible
                             (cheez-fetch-image url)))
         (cheez-image (create-image cheez-image-data nil t)))

    ;; Add the data to the hash if needed
    (if (not cheez-cache-possible)
        (cheez-cache-add url cheez-image-data))

    ;; Display
    (insert-image cheez-image)


;;;     ;; Dump the data for debugging, displays after the image
;;;     (insert (format "\n\nPage %s\nGlobal Image Number %s\nNumber of Images %s\nURL: %s\nCached? %s\n"
;;;                     cheez-page 
;;;                     cheez-current-image-num 
;;;                     cheez-num-images 
;;;                     url
;;;                     (if cheez-cache-possible
;;;                         t
;;;                       nil)))

    ))

(defun cheezburger-set-page (n)
  (interactive "NPage number: ")
  (setq cheez-page n)
  (cheezburger-get-data)
  ;Cheat and let it advance to the first image =)
  (setq cheez-current-image-num -1)
  (cheezburger-next-image))


(defun cheezburger-kill-image-url ()
  "Copy the URL of the image"
  (interactive)
  (kill-new (cdr (assoc 'url (aref cheez-data cheez-current-image-num))))
  (message "URL to image inserted into kill ring"))


(defun cheezburger-kill-url ()
  "Copy the URL of the actual blog post"
  (interactive)
  (kill-new (cdr (assoc 'permalink (aref cheez-data cheez-current-image-num))))
  (message "URL to post inserted into kill ring"))


(defun cheezburger-browse-url ()
  (interactive)
  (browse-url (cdr (assoc 'permalink (aref cheez-data cheez-current-image-num)))))


(defun cheezburger-quit ()
  "Close the cheezburger buffer, and continue with real work."
  (interactive)
  (kill-buffer "*cheezburger*"))


;; Only provide cheezburger for hungry lolcats at the end in case the
;; file fails to load.

;;I can has cheezburger?
(provide 'cheezburger)
;;Nom nom nom! Is delishus!

;;; cheezburger.el ends here
