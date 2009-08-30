

;;; planner-delicious.el --- bringing del.icio.us bookmarks to your Planner


    


    

;; Copyright (C) 2005 John Sullivan


    


    

;; Author: John Sullivan <john@wjsullivan.net>


    

;; Created 31 March 2005


    

;; Version: 0.1 2005-08-18


    

;; Keywords: comm, hypermedia


    


    

;; This program is free software; you can redistribute it and/or modify it


    

;; under the terms of the GNU General Public License as published by the Free


    

;; Software Foundation; either version 2 of the License, or (at your option)


    

;; any later version.


    


    

;; This program is distributed in the hope that it will be useful, but WITHOUT


    

;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or


    

;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for


    

;; more details.


    


    

;; You should have received a copy of the GNU General Public License along with


    

;; this program; if not, a copy is available at http://www.wjsullivan.net, or


    

;; write to the Free Software Foundation, Inc., 51 Franklin Street, 5th Fl.,


    

;; Boston, MA 02110 USA


    


    

;;; Commentary


    


    

;; You can load this file by putting (require 'planner-delicious) in your


    

;; `.emacs' file. This program requires Planner, which is available at Sacha


    

;; Chua's Web site <http://sacha.free.net.ph>.  Please report any bugs or


    

;; suggestions to me at <john@wjsullivan.net>. The newest version of this file


    

;; can always be found at <http://www.wjsullivan.net>.


    

;;


    

;; To use this on a Planner page, you need to have:


    

;;


    

;;* Delicious


    

;;


    

;; or whatever else you choose for `planner-delicious-section' on the pages


    

;; where you want your posts to appear. These functions can either append links


    

;; to the end of the section, or overwrite the section completely. You could


    

;; add one or more to your `planner-day-page-template' if you want posts to


    

;; appear every day, or to your `planner-plan-page-template' if you want them


    

;; to be written on your plan/project pages.


    

;;


    

;; When constructing your date regexp, keep in mind that dates are stored in a


    

;; format like `2005-04-23T20:22:55Z'. Things like "2005", "2005.07.25" or


    

;; "2005.07", etc., will work fine.


    

;;


    

;; The commands to use, either interactively or called from your template, are


    

;; `planner-delicious-append-posts-match-any' and


    

;; `planner-delicious-append-posts-match-all' if you want to add posts to the


    

;; ones you already have on your page, or


    

;; `planner-delicious-rewrite-posts-match-any' and


    

;; `planner-delicious-rewrite-posts-match-all' if you want to replace the posts


    

;; that are there already. Read their docstrings for details.


    


    

;;; Code:


    


    

(require 'delicious)


    

(require 'planner)


    


    

(defcustom planner-delicious-section "Delicious"


    

  "*Header for the del.icio.us section in a plan page."


    

  :version "21.4.1"


    

  :type 'string


    

  :group 'delicious)


    


    

(defun planner-delicious-append (posts)


    

  "Append POSTS to `planner-delicious-section'."


    

  (if (null (planner-narrow-to-section planner-delicious-section))


    

      (error "No delicious section on this page")


    

    (goto-char (point-max))


    

    (mapc


    

     (lambda (post)


    

       (let* ((href (cdr (assoc "href" post)))


    

              (desc (cdr (assoc "description" post)))


    

              (link (planner-make-link href desc)))


    

         (insert link "\n")))


    

     posts)


    

    (insert "\n")))


    

(defun planner-delicious-rewrite (posts)


    

  "Erase `planner-delicious-section' and insert POSTS."


    

  (if (null (planner-narrow-to-section planner-delicious-section))


    

      (error "No delicious section on this page")


    

    (let ((beg (point))


    

          (end (point-max)))


    

      (delete-region beg end))


    

    (mapc


    

     (lambda (post)


    

       (let* ((href (cdr (assoc "href" post)))


    

              (desc (cdr (assoc "description" post)))


    

              (link (planner-make-link href desc)))


    

         (insert link "\n")))


    

     posts)


    

    (insert "\n")))


    


    

(defun planner-delicious-modify-section (posts method)


    

  "METHOD can be rewrite or append."


    

  (cond ((eq method 'append)


    

         (planner-delicious-append posts))


    

        ((eq method 'rewrite)


    

         (planner-delicious-rewrite posts))


    

        (t


    

         (error "Unknown modification method %s" method))))


    


    

(defun planner-delicious-read-date ()


    

  "Input a date."


    

  (let ((date (read-string "Date (a regexp pattern): ")))


    

    date))


    


    

(defun planner-delicious-rewrite-posts-match-all (tags &optional search-date)


    

  "Replace `planner-delicious-section' contents with posts matching all TAGS.


    

If a prefix is given, do not filter by date."


    

  (interactive (list (delicious-complete-tags nil nil nil nil t)


    

                     (unless current-prefix-arg


    

                       (planner-delicious-read-date))))


    

  (planner-delicious-posts-match-all tags 'rewrite search-date))


    


    

(defun planner-delicious-append-posts-match-all (tags &optional search-date)


    

  "Append all your posts matching all of the tags and the date entered.


    

If a prefix is given, do not filter by date."


    

  (interactive (list (delicious-complete-tags nil nil nil nil t)


    

                     (unless current-prefix-arg


    

                       (planner-delicious-read-date))))


    

  (planner-delicious-posts-match-all tags 'append search-date))


    


    

(defun planner-delicious-rewrite-posts-match-date (search-date)


    

  "Replace `planner-delicious-section' with posts matching SEARCH-DATE."


    

  (interactive (list (planner-delicious-read-date)))


    

  (let ((matches (delicious-posts-matching-date search-date)))


    

    (save-excursion


    

      (save-restriction


    

	(planner-delicious-modify-section matches 'rewrite)))))


    


    

(defun planner-delicious-append-posts-match-date (search-date)


    

  "Append all your posts with date matching the regexp SEARCH-DATE."


    

  (interactive (list (planner-delicious-read-date)))


    

  (let ((matches (delicious-posts-matching-date search-date)))


    

    (save-excursion


    

      (save-restriction


    

	(planner-delicious-modify-section matches 'append)))))


    


    

(defun planner-delicious-posts-match-all (tags modification


    

                                               &optional search-date)


    

  (save-excursion


    

    (save-restriction


    

      (let ((matches (delicious-posts-matching-tags tags)))


    

        (when search-date


    

          (setq matches (delicious-posts-narrow-by-date matches search-date)))


    

        (planner-delicious-modify-section matches modification)))))


    


    

(defun planner-delicious-append-posts-match-any (tags &optional search-date)


    

  "Append all your posts matching any of TAGS with date matching SEARCH-DATE.


    

If a prefix is given, do not filter by date."


    

    (interactive (list (delicious-complete-tags nil nil nil nil t)


    

                     (unless current-prefix-arg


    

                       (planner-delicious-read-date))))


    

    (planner-delicious-posts-match-any tags 'append search-date))


    


    

(defun planner-delicious-rewrite-posts-match-any (tags &optional search-date)


    

  "Replace `planner-delicious-section' contents with posts matching any of TAGS.


    

If a prefix is given, do not filter by date."


    

  (interactive (list (delicious-complete-tags nil nil nil nil t)


    

                     (unless current-prefix-arg


    

                       (planner-delicious-read-date))))


    

  (planner-delicious-posts-match-any tags 'rewrite search-date))


    


    

(defun planner-delicious-posts-match-any (tags modification


    

                                               &optional search-date)


    

  (save-excursion


    

    (save-restriction


    

      (let ((matches (delicious-posts-matching-tags-any tags)))


    

        (when search-date


    

          (setq matches (delicious-posts-narrow-by-date matches search-date)))


    

        (planner-delicious-modify-section matches modification)))))


    


    

(provide 'planner-delicious)


    


