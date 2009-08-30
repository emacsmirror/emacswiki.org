;;; org-blog.el --- create and publish a blog with org-mode

;; Copyright (C) 2006  David O'Toole

;; Author: David O'Toole <dto@gnu.org>
;; Keywords: hypermedia, tools
;; $Id: org-blog.el,v 1.18 2007/06/13 16:21:24 dto Exp dto $	

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This program adds simple blog publishing support to org-mode. It is
;; built on top of org-publish.el.

;; You should read the documentation for org-publish.el before
;; continuing.

;; The latest version of this program, and of org-publish.el, can be
;; found at: http://dto.freeshell.org/notebook/OrgMode.html

;;;; 1. Basic configuration
;;
;;    First add (require 'org-blog) to your emacs initialization file. 
;;
;;    Then set the variable org-blog-directory (you can also leave it
;;    as the default, "~/blog/"). This directory should be different
;;    from the directory where your normal *.org files are stored,
;;    otherwise they will get "posted".
;;
;;    You should also set the variable
;;    org-blog-unfinished-directory. The default is
;;    "~/blog/unfinished". This is the directory where unfinished
;;    posts are stored. You can leave posts in the unfinished
;;    directory while you are working on them, and they won't be
;;    published.

;;;; 2. Create a post 
;;
;;    Use M-x org-blog-new-post. You'll be prompted for a
;;    filename. Enter a short name for this post (without the ".org")
;;    and press RET. You'll see a new buffer with a blank TITLE field.

;;    You can work on more than one post at once. They'll all be
;;    stored in your `org-blog-unfinished-directory'. To view a list
;;    of posts in progress, use M-x
;;    org-blog-find-unfinished-posts. You'll see the directory listing
;;    of `org-blog-unfinished-directory', and you can use RET to
;;    select a post to edit.

;;;; 3. Finish a post
;;
;;    When your post is ready, visit the file and hit M-x
;;    org-blog-finish-post. This does not mean the post is published
;;    on your website, only that the post is "finished" and given a
;;    timestamped filename. Your blog post and updated index will be
;;    published when you execute M-x org-publish-all.
;;
;;    But first, let's take a detour to make sure blog publishing is
;;    set up properly.
 
;;;; 4. Configure blog publishing
;;
;;    Org-blog contains an index function to publish a front page for
;;    your blog. This index can be configured to display the most
;;    recent posts, and your "blogroll" or list of links to other
;;    blogs. The newest post will always be at the top.
;;    
;;    You should add a project called "blog" to your
;;    org-publish-project-alist. Here is an example project
;;    configuration you can adapt to your needs: 

;; '("blog" :base-directory "~/blog/"
;; 	    :base-extension "org"
;; 	    :publishing-directory "/protocol:user@host:~/html/blog/"
;; 	    :publishing-function org-publish-org-to-html
;; 	    :auto-index t
;;          :blog-base-url "http://dto.freeshell.org/blog/"
;;	    :blog-title "dto.freeshell.org blog"
;;	    :blog-description "David O'Toole's web log."
;;	    :blog-export-rss t
;; 	    :index-function org-publish-blog-index
;; 	    :index-filename "index.org"
;; 	    :index-title "Title of my Blog"
;; 	    :index-posts 2
;;          :preamble my-blogroll-html
;;          :postamble my-footer-html)

;;    Most of these keywords are documented along with
;;    org-publish-project-alist. Before moving on, we'll explain
;;    usages specific to blogging support.

;;    The keyword :index-posts controls how many posts will be shown
;;    on the blog's front page. Set its value to an integer. Remaining
;;    posts will be shown as a list of links at the bottom of the
;;    page.

;;    The :index-title should be used to set the title of your blog.
;;    You can use the standard :preamble and :postamble keywords to
;;    set the header and footer of your blog posts and front page.
;;    This is a great place to include your HTML blogroll and
;;    copyright notices.

;;;;  5. Now publish!
;;
;;    After you've updated your org-publish-project-alist and created
;;    a post or two, hit M-x org-publish-all. Your posts should be
;;    uploaded, and an index frontpage generated.


;;; Code:

(require 'org-publish)

(defgroup org-blog nil
  "Options for keeping and publishing a blog with org-mode."
  :tag "Org Blog"
  :group 'org-publish)

(defcustom org-blog-directory "~/blog/" 
  "Directory where finished blog posts are stored."
  :group 'org-blog)

(defcustom org-blog-unfinished-directory "~/blog/unfinished"
  "Directory where unfinished posts are stored."
  :group 'org-blog)

(defcustom org-blog-time-format "%Y-%m-%d %I:%M%p -- "
  "Format string used when timestamping posts."
  :group 'org-blog)

(defun org-blog-new-post-file ()
  (concat (file-name-as-directory org-blog-directory) (format-time-string "blog-%Y-%m-%d-%H%M.org")))

(defun org-blog-new-post (filename)
  "Create a new post in FILENAME.
Post is stored in `org-blog-unfinished-directory'." 
  (interactive "sFilename for new post: ")
  (find-file (concat (file-name-as-directory org-blog-unfinished-directory)
		     filename ".org"))
  (insert "#+TITLE: \n")
  (insert "#+DESCRIPTION: "))

(defun org-blog-find-unfinished-posts ()
  "Open `org-blog-unfinished-directory'."
  (interactive)
  (let ((dir (file-name-as-directory org-blog-unfinished-directory)))
    (when (not (file-exists-p dir))
      (make-directory dir t))
    (find-file dir)))

(defun org-blog-finish-post ()
  "Complete and timestamp the unfinished post in the current buffer. 
Follow up with org-publish-all to upload to the site."
  (interactive)
  (write-file (org-blog-new-post-file)))

;; pluggable index generation function for org-publish.

(defun org-publish-blog-index (plist &optional index-filename)
  "Publish an index of all finished blog posts.
This function is suitable for use in the :index-function keyword
of org-publish-project-alist."
  (let* ((posts (nreverse (sort (org-publish-get-base-files plist "*~") 'string<)))
	 (base-directory (file-name-as-directory (or org-blog-directory (plist-get plist :base-directory))))
	 (blog-base-url (file-name-as-directory (plist-get plist :blog-base-url)))
	 (blog-title (plist-get plist :blog-title))
	 (publishing-directory (file-name-as-directory 
				(plist-get plist :publishing-directory)))
	 (blog-description (plist-get plist :blog-description))
	 (blog-rss-feed nil)
	 (rss (plist-get plist :blog-export-rss))
	 (post-content nil)
	 (index-file (concat base-directory (or index-filename "index.org")))
	 (index-buffer (find-buffer-visiting index-file))
	 (num-posts (or (plist-get plist :index-posts) 5))
	 (index-title (plist-get plist :index-title))
	 (count 0)
	 (p nil))

    (message "RSS = %S" rss)
    ;;
    ;; if buffer is already open, kill it
    (if index-buffer
	(kill-buffer index-buffer))
    ;;
    ;; start the RSS feed
    (when rss
      (push (org-blog-rss-preamble blog-title blog-base-url blog-description)
	    blog-rss-feed))
    ;;
    (with-temp-buffer
      ;;
      ;; process each post
      (while (setq p (pop posts))
	(let ((basename (file-name-nondirectory p))
	      (post-title nil)
	      (post-time (format-time-string 
			  "%a, %d %b %Y %H:%M:00 %z"
			  (nth 5 (file-attributes p))))	      
	      (post-description nil))
	  ;;
	  ;; grab post details
	  (with-temp-buffer
	    (insert-file-contents p)
	    ;;
	    ;; make sure we are in org-mode (otherwise export won't work properly)
	    (let ((org-inhibit-startup t)) (org-mode))
	    (goto-char (point-min))
	    (re-search-forward "#\\+TITLE: \\(.*\\)$" nil t)
	    (setf post-title (match-string 1))
	    (re-search-forward "#\\+DESCRIPTION: \\(.*\\)$" nil t)
	    (setf post-description (match-string 1))
	    (setf post-content (buffer-substring-no-properties
				(match-end 1) (point-max))))
	  ;;
	  ;; avoid inserting existing index; this would be a loop!
	  (when (not (string= basename index-filename))
	    ;;
	    ;; add rss item
	    (when rss
	      (push (org-blog-rss-item post-title 
				       (concat blog-base-url
					       (file-name-sans-extension
						(file-name-nondirectory p))
					       ".html")
				       post-content
				       post-time)		    
		    blog-rss-feed))
	    (if (< count num-posts)
		;;
		;; insert full text of post
		(progn (insert-file-contents p)
		       ;; permalink
		       (goto-char (point-max))
		       (insert (with-temp-buffer 
				 (insert (concat "\n\n [[file:" basename "][Permalink]]\n\n"))
				 (buffer-substring-no-properties (point-min) (point-max)))))
	      
	      ;;
	      ;; or, just insert link with title
	      (progn
		(goto-char (point-max))
		(when (= count num-posts)
		  (insert "\n** Older posts\n"))
		(insert (concat " - [[file:" 
				basename "][" 
				post-title "]]\n")))))
	  (setq count (+ 1 count))))
      ;;
      ;; finish rss feed and write
      (when rss
	(push (org-blog-rss-postamble) blog-rss-feed)
	(with-temp-buffer
	  (apply 'insert (nreverse blog-rss-feed))
	  (message "%S - %S" 
		   (concat publishing-directory "blog.rss")
		   blog-rss-feed)
	  
	  (write-file (concat publishing-directory "blog.xml"))))
      ;;
      ;; turn pasted titles into headings
      (goto-char (point-min))
      (while (search-forward "#+TITLE: " nil t)
	(replace-match "** " nil t))
      ;;
      ;; insert index title, if any
      (when index-title
	(goto-char (point-min))
	(insert (concat "#+TITLE: " index-title "\n\n")))
      (write-file index-file)
      (kill-buffer (current-buffer)))))
   

;;;; minimal RSS 2.0 support


(defun org-blog-rss-preamble (title link description)
  (format 
"<rss version=\"2.0\">
   <channel>
      <title>%s</title>
      <link>%s</link>
      <description>%s</description>
      <generator>OrgBlog</generator>"
 title link description))


(defun org-blog-rss-postamble ()
  "</channel></rss>")


(defun org-blog-rss-item (title permalink description pubdate)
  (let ((description-html (with-temp-buffer
			    (insert description)
			    (org-export-region-as-html (point-min) (point-max)
						       :body-only 'string))))
    (format 
     " <item>
 <title>%s</title>
 <description>%s</description>
 <pubDate>%s</pubDate>
 <guid isPermaLink=\"true\">%s</guid>
 </item>\n" title description-html pubdate permalink)))




(provide 'org-blog)
;;; org-blog.el ends here
