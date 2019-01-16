;;; org-readme.el --- Integrates Readme.org and Commentary/Change-logs.
;; 
;; Filename: org-readme.el
;; Description: Integrate Readme.org and Commentary/Change Logs.
;; Author: Matthew L. Fidler
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Created: Fri Aug  3 22:33:41 2012 (-0500)
;; Version: 20190116.1923
;; Package-Requires: ((http-post-simple "1.0") (yaoddmuse "0.1.1")(header2 "21.0") (lib-requires "21.0") (cl-lib "0.5"))
;; Last-Updated: Wed Jan 16 19:22:48 2019
;;           By: Joe Bloggs
;;     Update #: 818
;; URL: https://github.com/vapniks/org-readme
;; Keywords: Header2, Readme.org, Emacswiki, Git
;; Compatibility: Tested with Emacs 24.1 on Windows.
;;
;;
;; Features that might be required by this library:
;;
;;   cl-lib yaoddmuse http-post-simple org-html auto-document
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;; Installation
;; 
;; To make sure you have the most up-to-date version of this library it is best to install 
;; using the emacs package system, with the appropriate repository added (e.g https://melpa.org/)
;; 
;; To install without using a package manager:
;; 
;;  - Put the library in a directory in the emacs load path, like ~/.emacs.d/
;;  - Add (require 'LIB-NAME) in your ~/.emacs file
;; 
;;; Using org-readme
;; Org readme is used to:
;; 
;; - Create/Update a "History" section in the Readme.org based on the changelog
;;   section of the Emacs Log.
;; - Create/Update a "Library Information" Section Based on the Emacs lisp header.
;; - Create/Update a "Possible Dependencies" Section Based on the Emacs
;;   lisp header.
;; - Create/Update a "Functions" Section based on the functions defined
;;   in the single lisp library.
;; - Create/Update a "Variables" Section based on the variables defined
;;   in the single lisp library.
;; - Create/Update "Commands & keybindings" & "Customizable Options" sections as
;;   output by `auto-document'
;; 
;; All other sections of the Readme.org are then put into the
;; "Commentary" section of the readme.org.
;; 
;; In addition this library defines `org-readme-sync',  a convenience function that:
;; 
;; - Asks for a commentary about the library change.
;;   - To exit/save press `C-c C-c'
;; - Asks the user whether to add 
;; - Updates the headers in the elisp library according to the current date, time
;;   and the value of `org-readme-author-name'
;; - Asks if this is a minor revision
;;   - If it is a minor revision, bumps the revision up so the new
;;     library will be posted to marmalade-repo.org
;;   - The package will attempt to add the readme to the info
;;     documentation system within emacs.
;; - Syncs the Readme.org with the lisp file as described above.
;; - Updates emacswiki with the library description and the library
;;   itself (requires yaoddmuse).
;; - Updates Marmalade-repo if the library version is different than the
;;   version in the server (requires http-post-simple).
;; - Updates the git repository with the differences that you posted.
;; - If you are using github, this library creates a melpa recipe.
;; - If you are using github, this library creates a el-get recipe. 
;; 
;; When `org-readme-sync' is called in a `Readme.org' file that is not a
;; single lisp file, the function exports the readme in EmacsWiki format
;; and posts it to the EmacsWiki.
;;;; EmacsWiki Page Names
;; EmacsWiki Page names are generated from the file.  `org-readme.el'
;; would generate a page of OrgReadme.
;; 
;;;; Why each required library is needed
;; There are a few required libraries.  This is a list of the require
;; libraries and why they are needed.
;; 
;; |------------------+---------------------------------------------------------------------|
;; | Library          | Why it is needed                                                    |
;; |------------------+---------------------------------------------------------------------|
;; | yaoddmuse        | Publish to emacswiki                                                |
;; | http-post-simple | Publish to marmalade-repo.org                                       |
;; | header2          | To create header and changelog                                      |
;; | lib-requires     | To generate the library dependencies                                |
;; | auto-document    | To generate list of commands & options within elisp file (optional) |
;; |------------------+---------------------------------------------------------------------|
;;;; Notes
;; If you use `auto-insert' you may need to change your elisp 
;; entry of `auto-insert-alist' so that the end of the header section 
;; matches `org-readme-end-section-regexp'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `org-readme-add-autoloads'
;;    Query user to add ;;;###autoload magic comments to each function/macro/option.
;;    Keybinding: M-x org-readme-add-autoloads
;;  `org-readme-insert-autodoc'
;;    Use `auto-document' to document functions and options in current elisp file.
;;    Keybinding: M-x org-readme-insert-autodoc
;;  `org-readme-insert-variables'
;;    Extracts variable documentation and places it in the readme file.
;;    Keybinding: M-x org-readme-insert-variables
;;  `org-readme-build-el-get'
;;    Builds an el-get recipe. This assumes github, though others could be added.
;;    Keybinding: M-x org-readme-build-el-get
;;  `org-readme-build-melpa'
;;    Builds a melpa recipe. This assumes github, though other could be added.
;;    Keybinding: M-x org-readme-build-melpa
;;  `org-readme-marmalade-post'
;;    Posts the current buffer to Marmalade.
;;    Keybinding: M-x org-readme-marmalade-post
;;  `org-readme-edit-commit'
;;    Changelog for editing.
;;    Keybinding: C-x C-s
;;  `org-readme-edit-cancel'
;;    Cancel the edit log.
;;    Keybinding: C-c C-k
;;  `org-readme-edit'
;;    Edit change comment for commit.
;;    Keybinding: M-x org-readme-edit
;;  `org-readme-convert-to-markdown'
;;    Convert Readme.org to markdown Readme.md.
;;    Keybinding: M-x org-readme-convert-to-markdown
;;  `org-readme-convert-to-emacswiki'
;;    Convert Readme.org to oddmuse markup and upload to emacswiki.
;;    Keybinding: M-x org-readme-convert-to-emacswiki
;;  `org-readme-git'
;;    Add current file and other relevant files to git.
;;    Keybinding: M-x org-readme-git
;;  `org-readme-gen-info'
;;    With the proper tools, generate an info and dir from the current readme.org.
;;    Keybinding: M-x org-readme-gen-info
;;  `org-readme-sync'
;;    Syncs Readme.org with current buffer.
;;    Keybinding: M-x org-readme-sync
;;  `org-readme-to-commentary'
;;    Replace Commentary section in elisp file with text from Readme.org.
;;    Keybinding: M-x org-readme-to-commentary
;;  `org-readme-top-header-to-readme'
;;    Copy top header from the elisp file into the readme file as Library Information.
;;    Keybinding: M-x org-readme-top-header-to-readme
;;  `org-readme-changelog-to-readme'
;;    This puts the Emacs Lisp change-log into the Readme.org file.
;;    Keybinding: M-x org-readme-changelog-to-readme
;;  `org-readme-update-required-features-section'
;;    Update the required features section of the elisp file.
;;    Keybinding: M-x org-readme-update-required-features-section
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `org-readme-default-template'
;;    Default template for blank Readme.org Files. LIB-NAME is replaced with the library.
;;    default = "\n* Installation\n\nTo make sure you have the most up-to-date version of this library it is best to install \nusing the emacs package system, with the appropriate repository added (e.g https://melpa.org/)\n\nTo install without using a package manager:\n\n - Put the library in a directory in the emacs load path, like ~/.emacs.d/\n - Add (require 'LIB-NAME) in your ~/.emacs file\n\n"
;;  `org-readme-end-section-regexp'
;;    Regexp to match the end of a header/comments/changelog section in the elisp file comments.
;;    default = "^;;;;+[ 	]*$"
;;  `org-readme-features-regexp'
;;    Regexp to match the header line for the required libraries section.
;;    default = "^[ 	]*Features that might be required by this library:?[ 	]*$"
;;  `org-readme-changelog-lines-regexp'
;;    Regexp matching changelog lines in the elisp file (you probably shouldn't change this).
;;    default = "^[ 	]*\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ 	]*.*\n.*(\\([^)]*\\))[ 	]*\n\\(\\(?:\n\\|.\\)*?\\)\n[ 	]*\\([0-9][0-9]?\\)"
;;  `org-readme-final-changelog-line-regexp'
;;    Regexp matching the final changelog line in the elisp file (you probably shouldn't change this).
;;    default = "\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ 	]*\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*\\)"
;;  `org-readme-use-melpa-versions'
;;    Use Melpa-type versions YYYYMMDD.HHMM instead of 0.0.0 versions.
;;    default = (quote prompt)
;;  `org-readme-create-tar-package'
;;    Create a tar package for use in ELPA.
;;    default = (quote prompt)
;;  `org-readme-marmalade-server'
;;    Marmalade server website.
;;    default = "http://marmalade-repo.org"
;;  `org-readme-marmalade-token'
;;    Marmalade token to upload content to the marmalade server.
;;    default = nil
;;  `org-readme-marmalade-user-name'
;;    Marmalade user name to upload content to the marmalade server.
;;    default = nil
;;  `org-readme-author-name'
;;    Name to use as author when updating "Last-Updated" info in elisp header.
;;    default = user-full-name
;;  `org-readme-sync-emacswiki'
;;    Post library to the emacswiki.
;;    default = (quote prompt)
;;  `org-readme-sync-marmalade'
;;    Post library to marmalade-repo.org.
;;    default = (quote prompt)
;;  `org-readme-sync-git'
;;    Post library to git.
;;    default = (quote prompt)
;;  `org-readme-build-melpa-recipe'
;;    Build a melpa recipe based on github information.
;;    default = (quote prompt)
;;  `org-readme-build-el-get-recipe'
;;    Build an el-get recipe based on github information.
;;    default = (quote prompt)
;;  `org-readme-use-pandoc-markdown'
;;    Use pandoc's grid tables instead of transferring the tables to html.
;;    default = (quote prompt)
;;  `org-readme-drop-markdown-after-build-texi'
;;    Remove Readme.md after texinfo is generated.
;;    default = t
;;  `org-readme-build-info'
;;    Build .info file from Reade.org using texi.
;;    default = nil
;;  `org-readme-drop-texi-after-build-info'
;;    Remove the texi information after building info files.
;;    default = t
;;  `org-readme-add-readme-to-lisp-file'
;;    Update elisp file header with commentary section of Readme.org.
;;    default = (quote prompt)
;;  `org-readme-use-autodoc'
;;    Use `auto-document' to document elisp file.
;;    default = (quote prompt)
;;  `org-readme-add-autodoc-to-readme'
;;    Copy `auto-document' output to Readme.org.
;;    default = (quote prompt)
;;  `org-readme-add-functions-to-readme'
;;    Add a Functions section to Readme.org.
;;    default = (quote prompt)
;;  `org-readme-add-variables-to-readme'
;;    Add a Variables section to Readme.org.
;;    default = (quote prompt)
;;  `org-readme-update-changelog'
;;    Add/update Changelog file.
;;    default = (quote prompt)
;;  `org-readme-add-changelog-to-readme'
;;    Add Changelog information to Readme.org.
;;    default = (quote prompt)
;;  `org-readme-add-top-header-to-readme'
;;    Add Top Header information to Readme.org.
;;    default = (quote prompt)
;;  `org-readme-remove-sections'
;;    List of sections to remove when changing the Readme.org to Commentary.
;;    default = (quote ("History" "Possible Dependencies" "Library Information" "Installation" "Functions & macros" ...))
;;  `org-readme-remove-sections-from-markdown'
;;    List of sections to remove when changing the Readme.org to 
;;    default = (quote ("Functions & macros" "Variables"))

;;; Installation:
;;
;; Put org-readme.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'org-readme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 16-Jan-2019    Joe Bloggs  
;;    Last-Updated: Wed Jan 16 19:19:18 2019 #817 (Joe Bloggs)
;;    org-readme-sync: allow syncing without updating the Changelog 
;; 16-Jan-2019    Joe Bloggs  
;;    Last-Updated: Wed Jan 16 18:06:06 2019 #812 (Joe Bloggs)
;;    A few minor changes
;; 30-Nov-2015    Joe Bloggs  
;;    Last-Updated: Mon Nov 30 19:45:19 2015 #807 (Joe Bloggs)
;;    Add melpa recipe to package-build-recipes-dir
;; 19-Nov-2015    Joe Bloggs  
;;    Last-Updated: Thu Nov 19 02:43:29 2015 #802 (Joe Bloggs)
;;    Automatically update required features section in elisp comments
;; 14-Nov-2015    Joe Bloggs  
;;    Last-Updated: Sat Nov 14 23:05:32 2015 #800 (Joe Bloggs)
;;    Add auto-document integration
;; 14-Nov-2015    Joe Bloggs  
;;    Last-Updated: Sat Nov 14 00:29:15 2015 #798 (Joe Bloggs)
;;    Update "Last-Updated:", "By:" & "Update #:" fields
;; 12-Nov-2015   Joe Bloggs   
;;    Last-Updated: Thu Nov 12 23:37:26 2012 (-0500) #795 (Joe Bloggs)
;;    Refactor and tidy up code
;; 8-May-2013    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Add bugfix from vapniks for org-readme-to-commentary
;; 3-May-2013    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Uploading using org-readme.
;; 22-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix for org-readme generating texinfo documentation from org-files.
;; 22-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Separated out the texinfo conversion so that this may be applied to a
;;    generalized readme.
;; 13-Mar-2013    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added bug fix so that starred initial variables do not mess with
;;    org-cut-region.  That way, strange duplication of lines and regions do
;;    not occur.
;; 10-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Changed melpa versions to be nil.  However if a melpa version is
;;    detected, continue using it.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Post to marmalade
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Remove tar support because it is broken without gnu tar.  Gnu tar in
;;    windows is broken in opening elpa tarballs.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Use 7zip to create tar.  May create a readable tar for package.el
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Trying to test the org-readme tar balls
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Trying to post the tar package again.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix -- Tar package contents to include trailing /, otherwise emacs
;;    complains :(
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix for MELPA versions.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Updated org-readme to use MELPA versions.  Therefore when you upload
;;    to marmalade-repo and MELPA doesn't pick up your revision, you can
;;    download the latest version yourself and try it out.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added info to melpa recipie.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Attempted to add Readme in info format in the elpa package.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix for deleting directory.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Remove the directory that was created to make the package tarball 
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Attempted to fix the package information file.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added tar package that includes the info file
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    No longer deletes ilg files.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix for info generation.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added mecahism to build info files and dir files for elpa package.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Get description from info file.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    The description should now be picked up.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Attempting to update description.
;; 07-Dec-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Test directory entry
;; 07-Dec-2012    Matthew L. Fidler
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added directory entry to texinfo file.
;; 18-Sep-2012
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix to allow changes that read
;;    12-Sep-2012      
;;        Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;        Handle errors with the package gracefully.
;;    
;;    to include the author name who updated the file.
;;    
;; 12-Sep-2012      
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Handle errors with the package gracefully.
;; 12-Sep-2012      
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix to eliminate duplicate headers in Readme.org and emacswiki
;; 12-Sep-2012      
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Bug fix when org todo faces are not set.
;; 12-Sep-2012      
;;    Last-Updated: Wed Aug 22 13:11:26 2012 (-0500) #794 (Matthew L. Fidler)
;;    Added bug fix when `org-todo-keyword-faces' is undefined.
;; 22-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:05:14 2012 (-0500) #792 (Matthew L. Fidler)
;;    Attempting to upload again
;; 22-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 22 13:03:44 2012 (-0500) #790 (Matthew L. Fidler)
;;    Now will remove variable name and functions from markdown and
;;    outputted texinfo.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:57:54 2012 (-0500) #786 (Matthew L. Fidler)
;;    Bug fix.  When variables/functions are documented with an initial
;;    asterisk, change that asterisk to a bulleted item.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 21 12:34:00 2012 (-0500) #783 (Matthew L. Fidler)
;;    Another documentation update where I document how to change the
;;    comment and that org-readme may change the minor revision of the library.
;; 21-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:58:29 2012 (-0500) #781 (Matthew L. Fidler)
;;    Updated the documentation for org-readme.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:56:08 2012 (-0500) #779 (Matthew L. Fidler)
;;    Bug fix for variables that don't really transport well to the documentation.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:43:07 2012 (-0500) #774 (Matthew L. Fidler)
;;    Bump minor version for marmalade-repo.org
;; 20-Aug-2012    Matthew L. Fidler
;;    Last-Updated: Mon Aug 20 22:36:02 2012 (-0500) #772 (Matthew L. Fidler)
;;    Attempt to fix the History list 
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:34:35 2012 (-0500) #770 (Matthew L. Fidler)
;;    Added ability to customize which sections are added to the Readme.org
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:26:41 2012 (-0500) #757 (Matthew L. Fidler)
;;    Bug fix for creating function readme
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:22:04 2012 (-0500) #754 (Matthew L. Fidler)
;;    Will now remove the Functions and Variables sections before putting
;;    them in the commentary section.
;; 20-Aug-2012    Matthew L. Fidler
;;    Last-Updated: Mon Aug 20 22:17:03 2012 (-0500) #750 (Matthew L. Fidler)
;;    Attempt to remove Readme.md when not needed.
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 22:06:30 2012 (-0500) #744 (Matthew L. Fidler)
;;    Added ability to add function documentation and variable documentation
;;    to the Readme.org file
;; 20-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 20 09:33:22 2012 (-0500) #684 (Matthew L. Fidler)
;;    Added pandoc markdown table support (optional)
;; 13-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 13 21:52:37 2012 (-0500) #679 (Matthew L. Fidler)
;;    Another attempt to make texinfo documents.
;; 13-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 13 17:23:40 2012 (-0500) #676 (Matthew L. Fidler)
;;    Added texinfo output.  Allows native emacs documentation.
;; 13-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 13 16:48:37 2012 (-0500) #670 (Matthew L. Fidler)
;;    Tried to post behind firewall.  Reattempting.
;; 13-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 13 16:44:23 2012 (-0500) #668 (Matthew L. Fidler)
;;    Changed the `org-readme-remove-section' to use `org-cut-subtree'.
;;    Hopefully all errors will resolve themselves now.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 23:51:30 2012 (-0500) #665 (Matthew L. Fidler)
;;    Reverted. Still buggy.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 17:17:44 2012 (-0500) #662 (Matthew L. Fidler)
;;    Another attempt at bug fix to remove section.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 17:13:24 2012 (-0500) #659 (Matthew L. Fidler)
;;    Another attempt at a remove-section fix.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 17:07:22 2012 (-0500) #654 (Matthew L. Fidler)
;;    Bug fix for org-readme version tagging.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 17:03:26 2012 (-0500) #650 (Matthew L. Fidler)
;;    Test the bug where some of the section text is deleted 
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 16:46:03 2012 (-0500) #647 (Matthew L. Fidler)
;;    Added more documentation
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 16:45:19 2012 (-0500) #645 (Matthew L. Fidler)
;;    One last bug fix to the markdown export engine.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 16:42:41 2012 (-0500) #642 (Matthew L. Fidler)
;;    Markdown bug fix
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 16:31:18 2012 (-0500) #637 (Matthew L. Fidler)
;;    Bug fix for el-get recipe.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 16:28:45 2012 (-0500) #633 (Matthew L. Fidler)
;;    Added the ability to create a markdown Readme (Readme.md) as well as
;;    adding a el-get recipe.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 11:22:44 2012 (-0500) #529 (Matthew L. Fidler)
;;    Bug fix for emacswiki post and melpa bug fix
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 11:20:16 2012 (-0500) #525 (Matthew L. Fidler)
;;    Bug fix for adding melpa recipes. 
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 11:19:36 2012 (-0500) #524 (Matthew L. Fidler)
;;    Bug fix for creating melpa recipe.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 11:16:46 2012 (-0500) #521 (Matthew L. Fidler)
;;    Added ability to add melpa recipe
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 07:12:35 2012 (-0500) #492 (Matthew L. Fidler)
;;    Bug fix for pushing tags to a git repository
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:21:02 2012 (-0500) #480 (Matthew L. Fidler)
;;    Another fix for git tags.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:18:42 2012 (-0500) #477 (Matthew L. Fidler)
;;    Found a bug, let see if tagging works now.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:17:40 2012 (-0500) #475 (Matthew L. Fidler)
;;    Added Git tagging of new versions.  Lets see if it works.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:08:21 2012 (-0500) #467 (Matthew L. Fidler)
;;    Git push worked.  Bumping minor version.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:07:26 2012 (-0500) #465 (Matthew L. Fidler)
;;    Attempted to push repository again.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:03:15 2012 (-0500) #461 (Matthew L. Fidler)
;;    Attempt to push with git.  Something changed.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 01:02:04 2012 (-0500) #457 (Matthew L. Fidler)
;;    Added better Package-Requires tag.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:58:56 2012 (-0500) #454 (Matthew L. Fidler)
;;    Made request for minor revision earlier, and fixed bug.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:57:29 2012 (-0500) #451 (Matthew L. Fidler)
;;    Fixed code typo
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:56:12 2012 (-0500) #449 (Matthew L. Fidler)
;;    Bug fix for deleting a section of a Readme.org file.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:55:37 2012 (-0500) #447 (Matthew L. Fidler)
;;    Testing bug.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:55:30 2012 (-0500) #446 (Matthew L. Fidler)
;;    Minor bug fix.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:53:32 2012 (-0500) #444 (Matthew L. Fidler)
;;    Bug fix for comment sync, now Readme.org =file= is translated to lisp
;;    `file'.  Additionally, asks for version bump.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:44:17 2012 (-0500) #434 (Matthew L. Fidler)
;;    Bug fix for syncing readme.  Now the returns should not be as prevalent.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:39:23 2012 (-0500) #430 (Matthew L. Fidler)
;;    Attempting to post to marmlade again...
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:35:40 2012 (-0500) #426 (Matthew L. Fidler)
;;    Attempting to fix org-readme-marmalade-post.
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:26:36 2012 (-0500) #420 (Matthew L. Fidler)
;;    Bug fix to upload to emacswiki and upload to marmalade-repo
;; 11-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug 11 00:21:27 2012 (-0500) #413 (Matthew L. Fidler)
;;    Added marmalade-repo support.  Now org-readme should upload to
;;    marmalade-repo when the version is different from the latest version.
;; 08-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Wed Aug  8 18:44:37 2012 (-0500) #343 (Matthew L. Fidler)
;;    Fixed preformatting tags in emacswiki post.  Previously they may have
;;    been replaced with <PRE></pre> instead of <pre></pre>.  This makes the
;;    emacswiki page display correctly.
;; 07-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug  7 19:22:53 2012 (-0500) #333 (Matthew L. Fidler)
;;    To use, put (require 'ess-smart-underscore) in your ~/.emacs file
;; 7-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Tue Aug  7 19:14:34 2012 (-0500) #331 (Matthew L. Fidler)
;;    Added a Comment to EmcsWiki pages that states that the content of the
;;    page will likely be overwitten since it is automatically generated by `org-readme'
;; 7-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug  6 23:42:02 2012 (-0500) #328 (Matthew L. Fidler)
;;    Added more documentation.
;; 06-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug  6 23:24:22 2012 (-0500) #326 (Matthew L. Fidler)
;;    Added support for uploading Readme.org files to emacswiki without
;;    having to have a single associated lisp file.
;; 06-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug  6 20:44:55 2012 (-0500) #282 (Matthew L. Fidler)
;;    Bug fix for syncing from the single lisp file.
;; 06-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Mon Aug  6 20:12:50 2012 (-0500) #274 (Matthew L. Fidler)
;;    Added the ability to call `org-readme-sync' from Readme.org
;; 05-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sun Aug  5 12:30:26 2012 (-0500) #250 (Matthew L. Fidler)
;;    Added git pushing to org-readme
;; 05-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sun Aug  5 12:21:53 2012 (-0500) #237 (Matthew L. Fidler)
;;    Added git support as well as a comment mode.  The only thing that
;;    should need to be called is `org-readme-sync'
;; 04-Aug-2012    Matthew L. Fidler  
;;    Last-Updated: Sat Aug  4 21:40:14 2012 (-0500) #122 (Matthew L. Fidler)
;;    Added syncing with emacswiki. 
;; 04-Aug-2012    Matthew L. Fidler
;;    Last-Updated: Sat Aug  4 00:02:49 2012 (-0500) #20 (Matthew L. Fidler)
;;    Initial Release
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

(require 'cl-lib)
(require 'yaoddmuse nil t)
(require 'http-post-simple nil t)
(require 'org-html nil t)
(require 'auto-document nil t)

(defgroup org-readme nil
  "Org-readme is a way to create Readme.org files based on an elisp file."
  :group 'org)

;; Define a new customization type that we will be using a lot.
(define-widget 'yesnoprompt 'lazy
  "A binary tree made of cons-cells and strings."
  :offset 4
  :tag "Choice"
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)
		 (const :tag "Ask" prompt)))

(defcustom org-readme-default-template "
* Installation

To make sure you have the most up-to-date version of this library it is best to install 
using the emacs package system, with the appropriate repository added (e.g https://melpa.org/)

To install without using a package manager:

 - Put the library in a directory in the emacs load path, like ~/.emacs.d/
 - Add (require 'LIB-NAME) in your ~/.emacs file

"
  "Default template for blank Readme.org Files. LIB-NAME is replaced with the library."
  :type 'string
  :group 'org-readme)

(defcustom org-readme-end-section-regexp "^;;;;+[ \t]*$"
  "Regexp to match the end of a header/comments/changelog section in the elisp file comments."
  :type 'regexp
  :group 'org-readme)

(defcustom org-readme-features-regexp "^[ \t]*Features that might be required by this library:?[ \t]*$"
  "Regexp to match the header line for the required libraries section."
  :type 'regexp
  :group 'org-readme)

(defcustom org-readme-changelog-lines-regexp "^[ \t]*\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]*.*\n.*(\\([^)]*\\))[ \t]*\n\\(\\(?:\n\\|.\\)*?\\)\n[ \t]*\\([0-9][0-9]?\\)"
  "Regexp matching changelog lines in the elisp file (you probably shouldn't change this).

It should contain 4 parenthesised subexpressions matching:
 1) The date of the changelog entry.
 2) The author name.
 3) The changelog comment.
 4) The beginning of the next changelog line (used to delimit the lines).

Note: it should match the output from `make-revision'."
  :type 'regexp
  :group 'org-readme)

(defcustom org-readme-final-changelog-line-regexp "\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]*\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*\\)"
  "Regexp matching the final changelog line in the elisp file (you probably shouldn't change this).

  It should contain 3 parenthesised subexpressions matching:
  1) The date of the changelog entry.
  2) The author name.
  3) The changelog comment.

Note: it should match the output from `make-revision'."
  :type 'regexp
  :group 'org-readme)

(defcustom org-readme-use-melpa-versions 'prompt
  "Use Melpa-type versions YYYYMMDD.HHMM instead of 0.0.0 versions."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-create-tar-package 'prompt
  "Create a tar package for use in ELPA."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-marmalade-server "http://marmalade-repo.org"
  "Marmalade server website.
This should start with http: and should notend with a trailing forward slash,
just like the default value of http://marmalade-repo.org"
  :type 'string
  :group 'org-readme)

(defcustom org-readme-marmalade-token nil
  "Marmalade token to upload content to the marmalade server."
  :type 'string
  :group 'org-readme)

(defcustom org-readme-marmalade-user-name nil
  "Marmalade user name to upload content to the marmalade server."
  :type 'string
  :group 'org-readme)

(defcustom org-readme-author-name user-full-name
  "Name to use as author when updating \"Last-Updated\" info in elisp header.
Used by `org-readme-update-last-update'."
  :type 'string
  :group 'org-readme)

(defcustom org-readme-sync-emacswiki 'prompt
  "Post library to the emacswiki.
Requires `yaoddmuse'."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-sync-marmalade 'prompt
  "Post library to marmalade-repo.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-sync-git 'prompt
  "Post library to git."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-melpa-recipe 'prompt
  "Build a melpa recipe based on github information."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-el-get-recipe 'prompt
  "Build an el-get recipe based on github information."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-use-pandoc-markdown 'prompt
  "Use pandoc's grid tables instead of transferring the tables to html."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-drop-markdown-after-build-texi t
  "Remove Readme.md after texinfo is generated."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-info nil
  "Build .info file from Reade.org using texi.
Requires pandoc and makeinfo to be found.
This will also create the directory entry using install-info, if it is found."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-drop-texi-after-build-info t
  "Remove the texi information after building info files.
Either the .texi file or the info file and dir will be added to
the git repo depending on this option."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-readme-to-lisp-file 'prompt
  "Update elisp file header with commentary section of Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-use-autodoc 'prompt
  "Use `auto-document' to document elisp file."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-autodoc-to-readme 'prompt
  "Copy `auto-document' output to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-functions-to-readme 'prompt
  "Add a Functions section to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-variables-to-readme 'prompt
  "Add a Variables section to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-update-changelog 'prompt
  "Add/update Changelog file."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-changelog-to-readme 'prompt
  "Add Changelog information to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-top-header-to-readme 'prompt
  "Add Top Header information to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-remove-sections
  '("History" "Possible Dependencies" "Library Information" "Installation"
    "Functions & macros" "Variables" "Customizable Options" "Commands & keybindings")
  "List of sections to remove when changing the Readme.org to Commentary."
  :group 'org-readme
  :type '(repeat (string :tag "Section")))

(defcustom org-readme-remove-sections-from-markdown
  '("Functions & macros" "Variables")
  "List of sections to remove when changing the Readme.org to 
Markdown which is an intermediary for texinfo (using pandoc)."
  :group 'org-readme
  :type '(repeat (string :tag "Section")))

(defvar org-readme-edit-mode-map nil
  "Keymap for editing change-logs.")

(unless org-readme-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'org-readme-edit-commit)
    (define-key map (kbd "C-x C-s") 'org-readme-edit-commit)
    (define-key map (kbd "C-c C-k") 'org-readme-edit-cancel)
    (setq org-readme-edit-mode-map map)))

(defvar org-readme-edit-last-window-configuration nil)

(defvar org-readme-edit-last-buffer nil)

(defvar-local org-readme-added-autoloads nil
  "Whether autoload's have been added yet.")

(cl-defmacro org-readme-check-opt (opt &optional prompt override)
  "Query user if option OPT is 'prompt, or OVERRIDE is non-nil otherwise return OPT.
If PROMPT is supplied use that for the prompt, otherwise use
the first sentence of the docstring for OPT."
  `(if (or ,override (eq ,opt 'prompt))
       (y-or-n-p (or ,prompt
		     (replace-regexp-in-string
		      "\n.*" ""
		      (documentation-property
		       ',opt 'variable-documentation))))
     ,opt))

;; This occurs frequently
(defsubst goto-start nil
  "Move cursor to start of buffer."
  (goto-char (point-min)))

;; The following function is a slightly modified version of `xah-replace-regexp-pairs-region'
;; available here: https://github.com/xahlee/xah-replace-pairs
(defun org-readme-regexp-pairs (pairs &optional fixedcase literal string subexp)
  "Replace regex string find/replace PAIRS in buffer.
BEGIN END are the region boundaries.
PAIRS is: [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.
The optional arguments FIXEDCASE, LITERAL, STRING & SUBEXP are the same as in `replace-match'."
  (save-excursion
    (mapc (lambda (x)
	    (goto-start)
	    (while (search-forward-regexp (elt x 0) (point-max) t)
	      (replace-match (elt x 1) fixedcase literal string subexp)))
	  pairs)))

(defun org-readme-update-last-update nil
  "Update the \"Last-Updated:\", \"By:\" & \"Update #:\" fields in the elisp file header."
  (save-excursion
    (goto-start)
    (if (re-search-forward "^[; \t]*[Ll]ast[ -][Uu]pdated:[ \t]*\\(.*\\)$" nil t)
	(let ((endpos (save-excursion (forward-line 3) (point))))
	  (replace-match (current-time-string) t nil nil 1)
	  (if (re-search-forward "^[; \t]*[Bb]y:[ \t]*\\(.*\\)$" endpos t)
	      (replace-match (or (and (not (equal org-readme-author-name ""))
				      org-readme-author-name)
				 (read-string "Author Name: " user-full-name)) t nil nil 1))
	  (if (re-search-forward "^[; \t]*[Uu]pdate[ -]\\(?:#\\|[Nn]umber\\):[ \t]*\\(.*\\)$" endpos t)
	      (replace-match (number-to-string (1+ (string-to-number (match-string 1))))
			     t nil nil 1))))))

(defun org-readme-guess-package-name nil
  "Return the name of the elisp package."
  (let ((base (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name)))))
    (when (string= (downcase base) "readme")
      (let ((df (directory-files (file-name-directory (buffer-file-name))
				 t ".*[.]el$")))
        (unless (= 1 (length df))
          (setq df (directory-files (file-name-directory (buffer-file-name))
				    t ".*-mode[.]el$")))
        (unless (= 1 (length df))
          (setq df (directory-files (file-name-directory (buffer-file-name))
				    t ".*-pkg[.]el$")))
        (when (= 1 (length df))
          (setq base (file-name-sans-extension (file-name-nondirectory (nth 0 df)))))))
    base))

(defun org-readme-add-autoloads nil
  "Query user to add ;;;###autoload magic comments to each function/macro/option.
If ALL is non-nil (or called interactively with prefix arg) then add
the ;;;###autoload magic comment to all functions/macros/options."
  (interactive)
  (query-replace-regexp
   "^\\(;;?[^;\n]*\\|[ \t]*\\)\n(\\(def\\|cl-def\\)"
   "\\1\n;;;###autoload\n(\\2")
  (setq org-readme-added-autoloads t))

(defun org-readme-insert-autodoc (&optional copy)
  "Use `auto-document' to document functions and options in current elisp file.
If COPY is non-nil copy the output to Readme.org."
  (interactive)
  (auto-document)
  (if copy
      (let* ((readme (org-readme-find-readme))
	     (txt (with-output-to-string (adoc-output (current-buffer))))
	     (formattedtxt
	      (with-temp-buffer
		(insert txt)
		(org-readme-regexp-pairs
		 [["^;;;;+" ""]
		  ["^;;;+" "*"]
		  [";+" ""]
		  ["`\\(.*?\\)'" " - *\\1* :"]
		  ["^\\*[ \t]+Commands:?" "* Commands & keybindings"]
		  ["^\\*[ \t]+Customizable Options:?" "* Customizable Options"]
		  ["\n[ \t]*default = \\(.*\\)" "\\\\\\\\\n    default value: =\\1="]
		  ["\n[ \t]*Keybinding:[ \t]*\\(.*\\)$" "\\\\\\\\\n    Keybinding: =\\1="]
		  ["=\"\\(.*\\)\"=" "=\\1="]])
		(buffer-string))))
	(with-temp-buffer
	  (insert-file-contents readme)
	  (org-readme-remove-section "Commands & keybindings")
	  (org-readme-remove-section "Customizable Options" formattedtxt)
	  (write-file readme)))))

(defun org-readme-get-matches (regex &optional n)
  "Return sorted list of matches to REGEX in current file.
If N is provided return all matches of the Nth subexpression of REGEX."
  (save-excursion
    (goto-start)
    (sort (cl-loop while (re-search-forward regex nil t)
		   collect (match-string-no-properties (or n 0)))
	  'string<)))

(defun org-readme-insert-functions ()
  "Extracts function & macro documentation and places it in the Readme.org file."
  (let ((lst (org-readme-get-matches
	      "(\\(?:cl-\\)?def\\(?:un\\|macro\\)[*]?[ \t\n]+\\([^ \t\n]+\\)" 1))
	(readme (org-readme-find-readme))
	tmp ret1 ret2 ret3 ret)
    (cl-flet ((fd (x)
		  (with-temp-buffer
		    (insert x)
		    (goto-start)
		    (when (re-search-forward "'[.]" nil t)
		      (skip-chars-forward " \t\n")
		      (delete-region (point) (point-min)))
		    (goto-start)
		    (when (re-search-forward "[(]" nil t)
		      (goto-char (match-beginning 0))
		      (insert "=")
		      (forward-list)
		      (insert "="))
		    (org-readme-regexp-pairs
		     [["`\\(.*?\\)'" "=\\1="] ;change `' quotes to =
		      ["^[ \t]*[*]+[ \t]+" " - "] ;reformat list items
		      ["^[ \t]*[*]+" ""]])	  ;remove empty list items
		    (goto-char (point-max))
		    (insert "\n")	;final extra newline
		    (buffer-string)))
	      (addfns (txt) (if (string-match "\\*\\*\\*" txt)
				(concat "\n" txt))))
      (setq ret1 "** Interactive Functions\n"
	    ret2 "** Internal Functions\n"
	    ret3 "** Macros\n")
      (mapc
       (lambda(x)
	 (condition-case err
	     (when (intern x)
	       (setq tmp (describe-function (intern x)))
	       (cond
		((string-match "Not documented" tmp))
		((string-match "Lisp macro" tmp)
		 (setq ret3 (concat ret3 "\n*** " x "\n" (fd tmp))))
		((string-match "interactive" tmp)
		 (setq ret1 (concat ret1 "\n*** " x "\n" (fd tmp))))
		(t (setq ret2 (concat ret2 "\n*** " x "\n" (fd tmp))))))
	   (error nil)))
       lst)
      (setq ret (concat "* Functions & macros" (addfns ret1) (addfns ret2) (addfns ret3))))
    (with-temp-buffer
      (insert-file-contents readme)
      (org-readme-remove-section "Functions & macros"
				 (if (string-match "\\*\\*" ret)
				     ret
				   (message "No functions or macros found")
				   nil))
      (write-file readme))))

(defun org-readme-insert-variables ()
  "Extracts variable documentation and places it in the readme file."
  (interactive)
  (condition-case err (eval-buffer) (error nil))
  (let ((lst (org-readme-get-matches
	      "(def\\(?:var\\|var-local\\|custom\\)[*]?[ \t\n]+\\([^ \t\n]+\\)" 1))
	(readme (org-readme-find-readme))
	tmp ret1 ret2 ret)
    (cl-flet ((fd (x) ;function to format text returned by `describe-variable'
		  (with-temp-buffer
		    (insert x)
		    (goto-start)
		    ;; remove whitespace before and after variable description
		    (when (re-search-forward "Documentation:" nil t)
		      (skip-chars-forward " \t\n")
		      (delete-region (point) (point-min)))
		    (when (re-search-forward "You can customize this variable" nil t)
		      (goto-char (match-beginning 0))
		      (skip-chars-backward " \t\n")
		      (delete-region (point) (point-max)))
		    ;; reformat to org format
		    (org-readme-regexp-pairs
		     [["`\\(.*?\\)'" "=\\1="] ;change `' quotes to =
		      ["^[ \t]*[*]+[ \t]+" " - "] ;reformat list items
		      ["^[ \t]*[*]+" ""]]) ;remove empty list items
		    (goto-char (point-max))
		    (insert "\n")	;final extra newline
		    (buffer-string)))
	      (addvars (txt) (if (string-match "\\*\\*\\*" txt)
				 (concat "\n" txt))))
      (setq ret1 "** Customizable Variables\n"
	    ret2 "** Internal Variables\n")
      (mapc
       (lambda(x)
	 (condition-case err
	     (when (intern x)
	       (setq tmp (describe-variable (intern x)))
	       (cond
		((string-match "Not documented" tmp))
		((string-match "customize" tmp)
		 (setq ret1 (concat ret1 "\n*** " x "\n" (fd tmp))))
		(t (setq ret2 (concat ret2 "\n*** " x "\n" (fd tmp))))))
	   (error nil)))
       lst)
      (setq ret (concat "* Variables" (addvars ret1) (addvars ret2))))
    (with-temp-buffer
      (insert-file-contents readme)
      (org-readme-remove-section "Variables"
				 (if (string-match "\\*\\*" ret)
				     ret
				   (message "No variables found")
				   nil))
      (write-file readme))))

(defun org-readme-get-github-repo nil
  "Return the name of the github repo for the project, or nil if none found.
Assumes the current buffer contains a toplevel project file."
  (let* ((dir (file-name-directory (buffer-file-name)))
	 (git-cfg (expand-file-name "config" (expand-file-name ".git" dir))))
    (if (file-exists-p git-cfg)
	(with-temp-buffer
	  (insert-file-contents git-cfg)
	  (goto-start)
	  (if (or (re-search-forward "git@github.com:\\(.*?\\)[.]git" nil t)
		  (re-search-forward "https://github.com/\\(.*\\)" nil t))
	      (match-string 1))))))

(defun org-readme-build-el-get ()
  "Builds an el-get recipe. This assumes github, though others could be added.
Returns file name if created."
  (interactive)
  (let* ((el-get (expand-file-name
		  "el-get" (file-name-directory (buffer-file-name))))
	 (lib-name (org-readme-guess-package-name))
	 (github (org-readme-get-github-repo)))
    (unless (file-exists-p el-get)
      (make-directory el-get))
    (setq el-get (expand-file-name lib-name el-get))
    (unless (not github)
      (with-temp-file el-get
	(insert (format "(:name %s\n :description \"%s\"%s\n :website \"%s\"\n :type git\n :url \"%s\")"
			lib-name	; :name
			(save-excursion	; :description
			  (goto-start)
			  (if (re-search-forward
			       (format "^[ \t]*;;;[ \t]*%s[.]el[ \t]*--+[ \t]*\\(.*?\\)[ \t]*$"
				       lib-name) nil t)
			      (match-string 1)
			    lib-name))
			(save-excursion	; :depends
			  (goto-start)
			  (if (re-search-forward "^[ \t]*;+[ \t]*[Pp]ackage-[Rr]equires:[ \t]*\\(.*?\\)[ \t]*$" nil t)
			      (condition-case err
				  (format "\n :depends %s" (mapcar 'car (read (match-string 1))))
				(error (message "Error parsing package-requires: %s" err) ""))
			    ""))
			(save-excursion	; :website
			  (goto-start)
			  (if (re-search-forward "^[ \t]*;+[ \t]*URL:[ \t]*\\(.*\\)[ \t]*$" nil t)
			      (match-string 1)
			    (format "https://github.com/%s" github)))
			;; :url
			(format "https://github.com/%s.git" github))))
      el-get)))

(defun org-readme-build-melpa ()
  "Builds a melpa recipe. This assumes github, though other could be added.
Returns file name if created."
  ;; this assumes we are in the main elisp file
  (interactive)
  (let* ((melpa (expand-file-name
		 "melpa" (file-name-directory (buffer-file-name))))
	 (lib-name (org-readme-guess-package-name))
	 (github (org-readme-get-github-repo)))
    (unless (file-exists-p melpa)
      (make-directory melpa))
    (setq melpa (expand-file-name lib-name melpa))
    (when github
      (with-temp-file melpa
	(insert (format "(%s\n :repo \"%s\"\n :fetcher github\n)"
			lib-name github)))
      melpa)))

(defun org-readme-buffer-version ()
  "Gets the version of the current buffer."
  (let ((case-fold-search t))
    (save-excursion
      (goto-start)
      (when (re-search-forward "^ *;+ *Version: *\\(.*?\\) *$" nil t)
        (match-string-no-properties 1)))))

(defun org-readme-marmalade-post ()
  "Posts the current buffer to Marmalade."
  (interactive)
  (let* ((package (file-name-sans-extension
		   (file-name-nondirectory (buffer-file-name))))
         (m-ver (org-readme-marmalade-version package))
         (b-ver (org-readme-buffer-version))
         token resp)
    (message "Marmalade %s Version: %s, Buffer Version: %s"
             package m-ver b-ver)
    (when (or (not m-ver) (not (string= m-ver b-ver)))
      (message "Should post %s, the marmalade package is outdated or does not exist."
               package)
      (setq token (org-readme-token)
	    resp (http-post-simple-multipart
		  (format "%s/v1/packages" org-readme-marmalade-server)
		  `((name . ,org-readme-marmalade-user-name)
		    (token . ,token))
		  `(("package" ,(if (file-exists-p (concat package ".tar"))
				    (concat (file-name-sans-extension (buffer-file-name)) ".tar")
				  (buffer-file-name))
		     "text/x-script.elisp"
		     ,(if (file-exists-p (concat package ".tar"))
			  (with-temp-buffer
			    (insert-file-contents (concat package ".tar"))
			    (buffer-string))
			(buffer-string))))))
      (message "%s" resp))))

(defun org-readme-marmalade-version (package)
  "Gets the marmalade version of the PACKAGE."
  (let ((ver-json (url-retrieve-synchronously
                   (format "%s/v1/packages/%s/latest"
                           org-readme-marmalade-server package)))
        ver)
    (when ver-json
      (save-excursion
        (set-buffer ver-json)
        (goto-start)
        (when (re-search-forward "\"version\"[ \t]*:[ \t]*\"\\(.*?\\)\"" nil t)
          (setq ver (match-string 1)))
        (kill-buffer (current-buffer))))
    (symbol-value 'ver)))

(defun org-readme-token ()
  "Gets marmalade-token, if not already saved."
  (or org-readme-marmalade-token
      (let ((user-name (or org-readme-marmalade-user-name
                           (read-string "Marmalade username: ")))
            (password (read-passwd "Marmalade password: "))
            token)
        (setq token
              (with-temp-buffer
                (insert
                 (format "%s"
                         (nth 0 (http-post-simple
				 (format "%s/v1/users/login" org-readme-marmalade-server)
				 `((name . ,user-name)
				   (password . ,password))))))
                (goto-start)
                (when (re-search-forward "\"token\"[ \t]*:[ \t]*\"\\(.*?\\)\"" nil t)
		  (match-string 1))))
        (when token
          (setq org-readme-marmalade-user-name user-name
		org-readme-marmalade-token token)
	  (customize-save-variable 'org-readme-marmalade-user-name org-readme-marmalade-user-name)
	  (customize-save-variable 'org-readme-marmalade-token org-readme-marmalade-token))
	(symbol-value 'token))))

(defun org-readme-edit-commit ()
  "Changelog for editing."
  (interactive)
  (let ((comment (buffer-substring (point-min) (point-max)))
        mr)
    (kill-buffer (get-buffer "*Change Comment*"))
    (with-temp-buffer
      (insert comment)
      (goto-start)
      (end-of-line)
      (while (re-search-forward "^" nil t)
        (insert ";;    "))
      (setq mr (buffer-substring (point-min) (point-max))))
    (set-buffer org-readme-edit-last-buffer)
    (let ((user-full-name org-readme-author-name))
      (make-revision))
    (insert mr)
    (save-buffer)
    (with-temp-file (org-readme-get-change)
      (insert comment))
    (org-readme-sync t)))

(defun org-readme-edit-cancel ()
  "Cancel the edit log."
  (interactive)
  (kill-buffer (get-buffer "*Change Comment*"))
  (when org-readme-edit-last-window-configuration
    (set-window-configuration org-readme-edit-last-window-configuration)))

(defun org-readme-edit ()
  "Edit change comment for commit."
  (interactive)
  (unless org-readme-edit-last-window-configuration
    (setq org-readme-edit-last-window-configuration (current-window-configuration)))
  (switch-to-buffer-other-window (get-buffer-create "*Change Comment*"))
  (org-readme-edit-mode))

(define-derived-mode org-readme-edit-mode text-mode "Org-readme Log edit.")

;;;###autoload
(defun org-readme-convert-to-markdown ()
  "Convert Readme.org to markdown Readme.md.
If called with a prefix arg always prompt for options." 
  (interactive)
  (let ((readme (org-readme-find-readme))
        (markdown "Readme.md")
        p1 md tmp tmp2)
    (with-temp-buffer
      (insert-file-contents readme)
      (mapc
       (lambda(section) (org-readme-remove-section section))
       org-readme-remove-sections-from-markdown)
      ;; Convert org keywords
      (org-readme-regexp-pairs [["#[+]TITLE:" "+TITLE:"]
				["#[+]AUTHOR:" ""]
				["^[ \t]*#.*" ""]
				["[+]TITLE:" "# "]])
      ;; Convert Headings
      (goto-start)
      (while (re-search-forward "^\\([*]+\\) *!?\\(.*\\)" nil t)
	(setq tmp (make-string  (+ 1 (length (match-string 1))) ?#))
	(replace-match (format "%s %s" tmp (match-string 2)) t t))
      (org-readme-regexp-pairs [;; Convert links [[link][text]] to [text](link)
				["\\[\\[\\(\\(?:https?\\|ftp\\).*?\\)\\]\\[!?\\(.*?\\)\\]\\]" "[\\2](\\1)"]
				;; Replace file links.
				["\\[\\[file:\\(.*?[.]el\\)\\]\\[\\1\\]\\]" "\\1"]
				;; Underline _ul_ to <ul>ul</ul>
				["_\\(.*+?\\)_" "<ul>\\1</ul>"]
				;; Emphasis /emp/ to _emph_
				["/\\(.*+?\\)/" "_\\1_"]] t)
      ;; Bold *bold* to __bold__
      (goto-start)
      (while (re-search-forward "[*]\\(.*+?\\)[*]" nil t)
	(unless (save-match-data (string-match "^[*]+$" (match-string 1)))
	  (replace-match "__\\1__" t)))
      ;; convert list items and quoted symbols
      (org-readme-regexp-pairs [["^[ \t]*[+-] +\\(.*?\\) *::" "- __\\1__ -- "]
				["=\\(.*+?\\) *=" "`\\1`"]])
      ;; convert code blocks
      (goto-start)
      (while (re-search-forward "^ *#[+]BEGIN_SRC.*" nil t)
	(setq tmp (point))
	(when (re-search-forward "^ *#[+]END_SRC" nil t)
	  (beginning-of-line)
	  (setq tmp2 (point))
	  (goto-char tmp)
	  (while (and (> tmp2 (point))
		      (re-search-forward "^" tmp2 t))
	    (replace-match "::::"))))
      ;; convert evaluation results
      (goto-start)
      (while (re-search-forward "^: " nil t)
	(replace-match "\n::::" t t) ;
	(while (progn (end-of-line)
		      (re-search-forward "\\=\n: " nil t))
	  (replace-match "\n:::: "))
	(end-of-line))
      ;; Convert pre-formatted
      (org-readme-regexp-pairs [["^::::" "    "]])
      ;; Convert tables to html
      (goto-start)
      (while (re-search-forward "^[ \t]*|.*|[ \t]*$" nil t)
	(beginning-of-line)
	(setq p1 (point))
	(end-of-line)
	(while (re-search-forward "\\=\n[ \t]*|" nil t)
	  (end-of-line))
	(end-of-line)
	(save-restriction
	  (narrow-to-region p1 (point))
	  (if (org-readme-check-opt org-readme-use-pandoc-markdown nil current-prefix-arg)
	      (org-readme-regexp-pairs [["^\\([ \t]*\\)|\\(-.*?-\\)|\\([ \t]*\\)$" "\\1+\\2+\\3"]])
	    (if (featurep 'org-html)
		(org-replace-region-by-html (point-min) (point-max))
	      (org-export-replace-region-by 'html))
	    (org-readme-regexp-pairs [["class" "align"]]))))
      ;; Lists are the same.
      (setq markdown (buffer-string)))
    (with-temp-file (expand-file-name "Readme.md" (file-name-directory (buffer-file-name)))
      (insert markdown))))

;;;###autoload
(defun org-readme-convert-to-emacswiki ()
  "Convert Readme.org to oddmuse markup and upload to emacswiki."
  (interactive)
  (let ((readme (org-readme-find-readme))
        (what (file-name-nondirectory (buffer-file-name)))
        (wiki (org-readme-get-emacswiki-name))
        tmp tmp2)
    (with-temp-buffer
      (insert-file-contents readme)
      ;; Take out CamelCase Links
      (let ((case-fold-search nil))
	(org-readme-regexp-pairs [["\\([A-Z][a-z]+[A-Z][A-Za-z]*\\)" "!\\1"]] t))
      ;; Convert Tables.
      (org-readme-regexp-pairs [["^[ \t]*|[-+]+|[ \t]*\n" ""]])
      (goto-start)
      (while (re-search-forward "^[ \t]*|" nil t)
	(replace-match "||")
	(while (re-search-forward "|" (point-at-eol) t)
	  (replace-match "||")))
      ;; Convert Links
      (org-readme-regexp-pairs [["\\[\\[\\(\\(?:https?\\|ftp\\).*?\\)\\]\\[!?\\(.*?\\)\\]\\]" "[\\1 \\2]"]
				["^[ \t]*[A-Z]+:[ \t]*\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*" ""]
				["\\[\\[file:\\(.*?[.]el\\)\\]\\[\\1\\]\\]" "[[\\1]]"]
				["\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]" "\\2"]
				["=\\(.*?\\)=" "<tt>\\1</tt>"]] t)
      (goto-start)
      (while (re-search-forward "^\\([*]+\\) *!?\\(.*\\)" nil t)
	(setq tmp (make-string (min 4 (+ 1 (length (match-string 1)))) ?=))
	(replace-match (format "%s %s %s" tmp (match-string 2) tmp) t t)
	(beginning-of-line)
	(let ((case-fold-search nil))
	  (while (re-search-forward "!\\([A-Z][a-z]+[A-Z][A-Za-z]*\\)" (point-at-eol) t)
	    (replace-match "\\1" t))))
      (goto-start)
      (while (re-search-forward "^: " nil t)
	(replace-match "<pre>\n::::" t t) ;
	(while (progn (end-of-line)
		      (re-search-forward "\\=\n: " nil t))
	  (replace-match "\n:::: "))
	(end-of-line)
	(insert "\n</pre>"))
      (goto-start)
      (while (re-search-forward "^ *#[+]BEGIN_SRC emacs-lisp *.*" nil t)
	(replace-match "{{{")
	(setq tmp (point))
	(when (re-search-forward "^ *#[+]END_SRC" nil t)
	  (replace-match "}}}")
	  (beginning-of-line)
	  (setq tmp2 (point))
	  (goto-char tmp)
	  (while (and (> tmp2 (point))
		      (re-search-forward "^" tmp2 t))
	    (replace-match "::::"))))
      (goto-start)
      (while (re-search-forward "^ *#[+]BEGIN_SRC.*" nil t)
	(replace-match "<pre>" t t)
	(setq tmp (point))
	(when (re-search-forward "^ *#[+]END_SRC" nil t)
	  (replace-match "</pre>" t t)
	  (beginning-of-line)
	  (setq tmp2 (point))
	  (goto-char tmp)
	  (while (and (> tmp2 (point))
		      (re-search-forward "^" tmp2 t))
	    (replace-match "::::"))))
      (org-readme-regexp-pairs [["^[ \t]*[+-] +\\(.*?\\)::" "* <b>\\1</b> -- "]
				["^[ \t]*[+-] +" "* "]
				["^[ \t]*#.*" ""]
				["^[ \t]*[0-9]+[.)] +" "# "]
				["^[ \t]+" ""]])
      (goto-start)
      (while (re-search-forward "^::::" nil t)
	(replace-match "")
	(let ((case-fold-search nil))
	  (while (re-search-forward "!\\([A-Z][a-z]+[A-Z][A-Za-z]*\\)" (point-at-eol) t)
	    (replace-match "\\1" t))))
      (goto-char (point-max))
      (insert "\n\nThis was generated with OrgReadme.  On updating the library, this page is likely to be replaced with updated content.")
      (setq readme (buffer-substring (point-min) (point-max))))
    (with-temp-file wiki (insert readme))
    (save-excursion
      (set-buffer (find-file-noselect wiki))
      (emacswiki-post nil "")
      (kill-buffer (current-buffer)))
    (delete-file wiki)))

;;;###autoload
(defun org-readme-git (addmelpa addelget)
  "Add current file and other relevant files to git.
If ADDMELPA and/or ADDELGET are non-nil then add a melpa/el-get recipe,
and either of these arguments are filepaths then use those files as the
recipes.
If called with a prefix arg always prompt for options."
  (interactive (list (org-readme-check-opt org-readme-build-melpa-recipe nil current-prefix-arg)
		     (org-readme-check-opt org-readme-build-el-get-recipe nil current-prefix-arg)))
  (let* ((base (org-readme-guess-package-name))
	 (texifile (concat base ".texi"))
	 (infofile (concat base ".info"))
	 (changelog (org-readme-get-change))
         melpa el-get)
    ;; these functions will be used later
    (cl-flet ((gitadd (file) (message "Git adding %s" file)
		      (shell-command (concat "git add " file)))
	      (gitrm (file) (delete-file file)
		     (shell-command (concat "git rm --ignore-unmatch " file))
		     (shell-command (concat "rm -f " file))))
      ;; add melpa recipe if necessary
      (when addmelpa
	(setq melpa (if (and (stringp addmelpa)
			     (file-readable-p addmelpa))
			addmelpa
		      (org-readme-build-melpa)))
	(when melpa
	  (gitadd (concat (file-name-as-directory "melpa") (file-name-nondirectory melpa)))))
      ;; add el-get recipe
      (when addelget
	(setq el-get (if (and (stringp addelget)
			      (file-readable-p addelget))
			 addelget
		       (org-readme-build-el-get)))
	(when el-get
	  (gitadd (concat (file-name-as-directory "el-get") (file-name-nondirectory el-get)))))
      ;; add Readme.org
      (gitadd (file-name-nondirectory (org-readme-find-readme)))
      ;; add either .info or .texi file, and delete Readme.md if necessary
      (when (file-exists-p texifile)
	(when (and (org-readme-check-opt org-readme-drop-markdown-after-build-texi nil current-prefix-arg)
		   (file-exists-p "Readme.md"))
	  (gitrm "Readme.md"))
	;; add either the info & dir files or the texifile
	(if (and (org-readme-check-opt org-readme-drop-texi-after-build-info nil current-prefix-arg)
		 (file-exists-p infofile))
	    (progn (gitadd infofile)
		   (if (file-exists-p (expand-file-name
				       "dir" (file-name-directory (buffer-file-name))))
		       (gitadd "dir"))
		   (gitrm texifile))
	  (gitadd texifile)))
      ;; add Readme.md if it wasn't deleted
      (when (file-exists-p "Readme.md")
	(gitadd "Readme.md"))
      ;; add this file
      (gitadd (file-name-nondirectory (buffer-file-name)))
      ;; commit the changes
      (when (file-exists-p changelog)
	(message "Git Committing")
	(shell-command (concat "git commit -F " (file-name-nondirectory changelog)))
	(delete-file changelog)
	;; push the commits
	(message "Git push")
	(shell-command "git push")
	;; tag the commit if this is a new version
	(let ((tags (shell-command-to-string "git tag"))
	      (ver (org-readme-buffer-version)))
	  (when ver
	    (unless (string-match (concat "v" (regexp-quote ver)) tags)
	      (message "Tagging the new version")
	      (message "git tag -a v%s -m \"version %s\"" ver ver)
	      (shell-command (format "git tag -a v%s -m \"version %s\"" ver ver))
	      (shell-command "git push --tags"))))))))

(defun org-readme-in-readme-org-p ()
  "Determine if the currently open buffer is the Readme.org"
  (string= "readme.org" (downcase (file-name-nondirectory (buffer-file-name)))))

(defun org-readme-single-lisp-p ()
  "Determine if the Readme.org is in a directory with a single Lisp file.
If so, return the name of that Lisp file, otherwise return nil."
  (let* ((dn (file-name-directory (buffer-file-name)))
         (df (directory-files dn t "[.][Ee][Ll]$")))
    (if (= 1 (length df))
        (progn (setq df (nth 0 df))
	       (symbol-value 'df))
      nil)))

;;;###autoload
(defun org-readme-gen-info ()
  "With the proper tools, generate an info and dir from the current readme.org.
If called with a prefix arg always prompt for options."
  (interactive)
  (if (not (executable-find "pandoc"))
      (error "Can't find pandoc executable"))
  (if (not (executable-find "makeinfo"))
      (error "Can't find makeinfo executable"))
  ;; first create the Readme.md file
  (org-readme-convert-to-markdown)
  (cl-flet ((shell-cmd-concat (&rest strs) (shell-command (apply 'concat strs)))
	    (getval (var str default) ;var = variable to assign to, str = string to search for
		    (goto-start)      ;default = default value
		    (if (search-forward str nil t)
			(set var (buffer-substring (point) (point-at-eol)))
		      (set var default))))
    (let ((base (org-readme-guess-package-name))
	  (texifile (concat (file-name-sans-extension
			     (file-name-nondirectory (buffer-file-name)))
			    ".texi"))
	  pkg ver desc cnt)
      ;; convert Readme.md to a .texi file
      (shell-cmd-concat "pandoc Readme.md -s -o " texifile)
      ;; get information for direntry
      (setq cnt (with-temp-buffer
		  (insert-file-contents texifile)
		  (getval 'desc "@strong{Description} -- " base)
		  (getval 'pkg "@strong{Package-Requires} -- " "()")
		  (getval 'ver "@strong{Version} -- " "0.0")
		  (buffer-string)))
      ;; Now add direntry to the .texi file
      (with-temp-file texifile
	(insert cnt)
	(goto-start)
	(when (re-search-forward "@documentencoding")
	  (goto-char (point-at-eol))
	  (insert "\n@dircategory Emacs lisp libraries\n@direntry\n* "
		  base ": (" base ").     " desc "\n@end direntry\n")))
      ;; create the .info file
      (shell-cmd-concat "makeinfo " base ".texi")
      ;; only create the dir file if the install-info executable is present
      (when (executable-find "install-info")
	(shell-cmd-concat "install-info --dir-file=dir " base ".info"))))
  ;; remove the markdown file as it is no longer needed
  (when (org-readme-check-opt org-readme-drop-markdown-after-build-texi nil current-prefix-arg)
    (delete-file "Readme.md")))

(defun org-readme-create-tar-archive nil
  "Create tar archive for package files in the current directory."
  (cl-flet ((getval (regexp) (save-excursion (goto-start)
					     (re-search-forward regexp)
					     (match-string 1))))
    (if (not (or (executable-find "tar")
		 (executable-find "7z")
		 (executable-find "7za")))
	(error "Can't find tar or 7z program")
      (let* ((base (org-readme-guess-package-name))
	     (desc (getval "---[ \t]*\\(.*\\)[ \t]*"))
	     (ver (getval "^[ \t]*;+[ \t]*Version:[ \t]*\\(.*\\)[ \t]*"))
	     (pkg (getval "^;;[ \t]*Package-Requires:[ \t]*\\(.*\\)[ \t]*"))
	     (tardir (concat base "-" ver))
	     (tarbase (concat (file-name-as-directory tardir) base))
	     (infofile (concat base ".info")))
	(when (file-exists-p (concat base ".tar"))
	  (delete-file (concat base ".tar")))
	(make-directory tardir t)
	(copy-file (concat base ".el") (concat tarbase ".el") t)
	(when (file-exists-p infofile)
	  (copy-file (concat base ".info") (concat tarbase ".info") t))
	(when (file-exists-p "dir")
	  (copy-file "dir" (concat (file-name-as-directory tardir) "dir") t))
	(with-temp-file (concat tarbase "-pkg.el")
	  (insert "(define-package \"" base "\" \"" ver  "\" \"" desc "\" '" pkg ")"))
	(if (executable-find "tar")
	    (shell-command (concat "tar -cvf " base ".tar " (file-name-as-directory tardir)))
	  (shell-command (concat "7z" (if (executable-find "7za") "a" "")
				 " -ttar -so " base ".tar " (file-name-as-directory tardir) "*.*")))
	(mapc (lambda (x) (when (file-exists-p x) (delete-file x)))
	      (list (concat tarbase ".el")
		    (concat tarbase "-pkg.el")
		    (concat tarbase ".info")
		    (concat (file-name-as-directory tardir) "dir")))
	(delete-directory tardir)))))

;;;###autoload
(defun org-readme-sync (&optional comment-added)
  "Syncs Readme.org with current buffer.
When COMMENT-ADDED is non-nil, the comment has been added and the syncing should begin.
If called with a prefix arg always prompt for options."
  (interactive)
  ;; Store the name of the package in `base'
  (let ((base (org-readme-guess-package-name))
	(single-lisp-file (org-readme-single-lisp-p))
	addmelpa addelget melpa elget)
    ;; Check that we are either in the Readme.org file or an elisp file
    (unless (or (org-readme-in-readme-org-p)
		(eq major-mode 'emacs-lisp-mode))
      (error "Cannot run `org-readme-sync' from this buffer"))
    ;; Check if we need to switch file or update the changelog first
    ;; (`comment-added' should be nil unless this function was called internally)
    (cl-block nil
      (if (and (not comment-added)
	       (org-readme-in-readme-org-p))
	  (progn (message "In Readme.org")
		 ;; If there's only one lisp file, switch to it, and start again.
		 (if single-lisp-file
		     (progn (setq org-readme-edit-last-window-configuration
				  (current-window-configuration))
			    (find-file single-lisp-file)
			    (setq org-readme-edit-last-buffer (current-buffer))
			    (org-readme-sync))
		   ;; otherwise there are several elisp files so just post Readme.org to emacswiki if necessary
		   (unless (not (org-readme-check-opt
				 org-readme-sync-emacswiki
				 "Post Readme.org to emacswiki without changes"
				 current-prefix-arg))
		     (message "Posting Description to emacswiki")
		     (org-readme-convert-to-emacswiki))))
	(if (and (not comment-added)
		 (org-readme-check-opt org-readme-update-changelog nil current-prefix-arg))
	    ;; Update the Changelog file if necessary (and set `comment-added' to t)
	    (progn
	      (setq org-readme-edit-last-buffer (current-buffer))
	      (org-readme-update-last-update)
	      ;; `org-readme-sync' will be called again with `comment-added' set to t
	      (org-readme-edit)
	      (cl-return))
	  ;; Otherwise, make sure we are in the elisp file
	  (if (not (eq major-mode 'emacs-lisp-mode))
	      (if single-lisp-file
		  (find-file single-lisp-file)
		(error "Can't find elisp file"))))
	;; Add autoload's
	(when (and (not org-readme-added-autoloads)
		   (y-or-n-p "Add autoloads? "))
	  (org-readme-add-autoloads))
	;; Update required features section
	(org-readme-update-required-features-section)
	;; Update last update & version number
	(unless comment-added (org-readme-update-last-update))
	(when (y-or-n-p "Update version number? ")
	  (save-excursion
	    (goto-start)
	    (let ((case-fold-search t))
	      (when (re-search-forward "^[ \t]*;+[ \t]*Version:" nil t)
		(if (or (org-readme-check-opt org-readme-use-melpa-versions nil current-prefix-arg)
			(save-match-data (looking-at "[ \t]*[0-9]\\{8\\}[.][0-9]\\{2,4\\}[ \t]*$")))
		    (progn
		      (delete-region (point) (point-at-eol))
		      (insert (concat " " (format-time-string "%Y%m%d." (current-time))
				      (format "%d" (or (string-to-number (format-time-string "%H%M" (current-time))) 0)))))
		  (end-of-line)
		  (when (looking-back "\\([ .]\\)\\([0-9]+\\)[ \t]*")
		    (replace-match (format "\\1%s"
					   (+ 1 (string-to-number (match-string 2)))))))))))
	;; Replace commentary section in elisp file with text extracted from readme file
	;; (if this file doesn't yet exist it will be created and `org-readme-default-template' inserted).
	;; The user will be prompted to save the existing Commentary section to the kill ring.
	(when (org-readme-check-opt org-readme-add-readme-to-lisp-file nil current-prefix-arg)
	  (message "Adding Readme to Header Commentary")
	  (if (called-interactively-p 'any)
	      (call-interactively 'org-readme-to-commentary)
	    (org-readme-to-commentary)))
	;; Document commands and options in elisp file
	(when (and (require 'auto-document nil t)
		   (org-readme-check-opt
		    org-readme-use-autodoc nil current-prefix-arg))
	  (message "Updating using autodoc.")
	  (org-readme-insert-autodoc
	   (org-readme-check-opt org-readme-add-autodoc-to-readme nil current-prefix-arg)))
	;; Add functions section to readme file
	(when (org-readme-check-opt
	       org-readme-add-functions-to-readme nil current-prefix-arg)
	  (message "Updating Functions.")
	  (org-readme-insert-functions))
	;; Add variables section to readme file
	(when (org-readme-check-opt
	       org-readme-add-variables-to-readme nil current-prefix-arg)
	  (message "Updating Variables.")
	  (org-readme-insert-variables))
	;; Add Changelog to readme file
	(when (org-readme-check-opt
	       org-readme-add-changelog-to-readme nil current-prefix-arg)
	  (message "Updating Changelog in current file.")
	  (org-readme-changelog-to-readme))
	;; Copy top header from elisp file into readme file
	(when (org-readme-check-opt
	       org-readme-add-top-header-to-readme nil current-prefix-arg)
	  (org-readme-top-header-to-readme))
	;; save the elisp buffer before moving on
	(save-buffer)
	;; Create info documentation
	(when (org-readme-check-opt
	       org-readme-build-info nil current-prefix-arg)
	  (org-readme-gen-info))
	;; Create .tar archive
	(when (and (or (executable-find "tar")
		       (executable-find "7z")
		       (executable-find "7za"))
		   (org-readme-check-opt
		    org-readme-create-tar-package nil current-prefix-arg))
	  (org-readme-create-tar-archive))
	;; post to marmalade
	(when (and (featurep 'http-post-simple)
		   (org-readme-check-opt
		    org-readme-sync-marmalade nil current-prefix-arg))
	  (message "Attempting to post to marmalade-repo.org")
	  (org-readme-marmalade-post))
	;; post to elisp file to emacswiki
	(when (and (featurep 'yaoddmuse)
		   (org-readme-check-opt
		    org-readme-sync-emacswiki "Post elisp file to emacswiki?" current-prefix-arg))
	  (message "Posting elisp file to emacswiki")
	  (emacswiki-post nil ""))
	;; add melpa recipe if necessary
	(setq addmelpa (org-readme-check-opt
			org-readme-build-melpa-recipe nil current-prefix-arg))
	(when addmelpa
	  (setq melpa (org-readme-build-melpa))
	  (when (and (require 'package-build nil t)
		     (file-directory-p package-build-recipes-dir))
	    (let ((melpa2 (expand-file-name (org-readme-guess-package-name)
					    package-build-recipes-dir)))
	      (if (file-writable-p melpa2)
		  (copy-file melpa melpa2 t)
		(error "Can't write to %s" package-build-recipes-dir)))))
	;; add el-get recipe if necessary
	(setq addelget (org-readme-check-opt
			org-readme-build-el-get-recipe nil current-prefix-arg))
	(when addelget (setq elget (org-readme-build-el-get)))
	;; add files to git repo, along with MELPA and el-get recipes
	(when (org-readme-check-opt
	       org-readme-sync-git nil current-prefix-arg)
	  ;; TODO: allow creation of melpa and el-get recipes without syncing to git?
	  (org-readme-git melpa elget))
	;; post readme file to emacswiki
	(when (and (featurep 'yaoddmuse)
		   (org-readme-check-opt
		    org-readme-sync-emacswiki
		    "Post Readme.org to emacswiki?" current-prefix-arg))
	  (message "Posting Description to emacswiki")
	  (org-readme-convert-to-emacswiki))
	;; revert the window config back to how it was before
	(when org-readme-edit-last-window-configuration
	  (set-window-configuration org-readme-edit-last-window-configuration)
	  (setq org-readme-edit-last-window-configuration nil))))))

;;;###autoload
(defun org-readme-to-commentary (&optional savetokr)
  "Replace Commentary section in elisp file with text from Readme.org.
If SAVETOKR is non-nil then save the existing Commentary section to the `kill-ring'."
  (interactive (list (y-or-n-p "Save existing Commentary section to kill ring? ")))
  (let ((readme (org-readme-find-readme)) p1)
    (with-temp-buffer
      (insert-file-contents readme)
      (org-mode)
      ;; remove some sections
      (mapc (lambda (section) (org-readme-remove-section section))
	    org-readme-remove-sections)
      ;; remove stuff
      (org-readme-regexp-pairs [["=\\<\\(.*?\\)\\>=" "`\\1'"] ;replace =SYMBOL= with `SYMBOL'
				["#.*" ""] ;remove all org #+KEYWORDS
				["^[ \t]*[A-Z]+:[ \t]*\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*" ""]
				["^:" ""] ;remove : at beginning of lines
				["^[ \t]*\\*+ Commentary" ""] ;remove Commentary line (this already exists in elisp file)
				["^\\*" ";;;"] ;replace *'s with ;'s to keep outline structure
				["^\\(;*\\)\\*" "\\1;"]   ;second *
				["^\\(;*\\)\\*" "\\1;"]   ;third *
				["^\\(;*\\)\\*" "\\1;"]   ;fourth *
				["^[ \t]*\\*+" ""]]) ;remove *'s from beginning of lines
      ;; remove all TODO items
      (goto-start)
      (when org-todo-keyword-faces
	(while (org-readme-remove-section
		(regexp-opt (mapcar (lambda(x) (nth 0 x)) org-todo-keyword-faces))
		nil t)))
      ;; replace initial & final whitespace with single newline chars
      (goto-start)
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (point))
      (insert "\n")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n")
      ;; comment all lines with ;;
      (org-readme-regexp-pairs [["^\\([^;]\\)" ";; \\1"]])
      (setq readme (buffer-string)))
    ;; delete current "Commentary" region in elisp file, and replace
    ;; with text extracted from Readme.org
    (goto-start)
    (when (re-search-forward "^;;;[ \t]*Commentary:?[ \t]*$" nil t)
      (forward-line 1)
      (let ((pt (point)))
	(when (re-search-forward org-readme-end-section-regexp nil t)
	  (goto-char (match-beginning 0))
	  (forward-line 0)
	  (funcall (if savetokr 'kill-region 'delete-region) pt (point))
	  (insert readme))))))

(defun org-readme-get-emacswiki-name ()
  "Gets emacswiki-style name based on buffer."
  (if (org-readme-in-readme-org-p)
      (let ((wiki (file-name-nondirectory (substring (file-name-directory (buffer-file-name)) 0 -1))))
        (with-temp-buffer
          (insert wiki)
          (goto-start)
          (when (looking-at ".") (replace-match (upcase (match-string 0)) t t))
          (while (re-search-forward "[-._]\\(.\\)" nil t)
            (replace-match  (upcase (match-string 1))) t t)
          (setq wiki (buffer-substring (point-min) (point-max))))
        (symbol-value 'wiki))
    (let ((dir (file-name-directory (buffer-file-name)))
          (name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      (with-temp-buffer
        (insert (downcase name))
        (goto-start)
        (when (looking-at ".") (replace-match (upcase (match-string 0)) t t))
        (while (re-search-forward "-\\(.\\)" nil t)
          (replace-match  (upcase (match-string 1))) t t)
        (setq name (concat dir (buffer-substring (point-min) (point-max)))))
      (symbol-value 'name))))

(defun org-readme-get-change ()
  "Get file for changelog commits."
  (expand-file-name "Changelog" (file-name-directory (buffer-file-name))))

(defun org-readme-find-readme ()
  "Find the Readme.org, or create it if it doesn't yet exist."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (df (directory-files dir t "^[Rr][Ee][Aa][Dd][Mm][Ee][.][Oo][Rr][Gg]$")))
    (if (= 1 (length df))
        (setq df (nth 0 df))
      (setq df (expand-file-name "Readme.org" dir))
      (let ((lib-name (file-name-sans-extension
                       (file-name-nondirectory (buffer-file-name)))))
        (with-temp-file df
          (insert org-readme-default-template)
          (goto-start)
          (while (re-search-forward "LIB-NAME" nil t)
            (replace-match lib-name t t)))))
    (symbol-value 'df)))

(defun org-readme-remove-section (section &optional txt any-level at-beginning)
  "Remove `org-mode' SECTION. Optionally insert TXT.
When ANY-LEVEL is non-nil, the SECTION may be at any level.
When AT-BEGINNING is non-nil, if the section is not found, insert TXT at the beginning."
  (let ((case-fold-search t)
        (mtch ""))
    (save-excursion
      (goto-start)
      (if (re-search-forward (format "^\\([*]%s\\)[ \t]+%s" (if any-level "+" "") section)
			     nil t)
          (progn
            (org-cut-subtree)
            (save-excursion (when txt (insert txt)))
            t)
        (when txt
          (goto-char (if at-beginning (point-min) (point-max)))
          ;; Skip comments
          (if at-beginning
              (while (re-search-forward "\\=[ \t]*#.*\n" nil t))
            (while (re-search-backward "\n[ \t]*#.*\\=" nil t)))
          (beginning-of-line)
          (save-excursion (insert txt)))
        nil))))

;;;###autoload
(defun org-readme-top-header-to-readme ()
  "Copy top header from the elisp file into the readme file as Library Information.
The top header is defined as all text between the start of the file and the first 
match to `org-readme-end-section-regexp'."
  (interactive)
  (let ((top-header "")
        (readme (org-readme-find-readme)))
    ;; copy the top header from the elisp file
    (save-excursion
      (goto-start)
      (when (re-search-forward org-readme-end-section-regexp nil t)
        (beginning-of-line)
        (setq top-header (buffer-substring (point-min) (point)))))
    ;; copy top header info and reformat it for orgmode
    (with-temp-buffer
      (insert top-header)
      (goto-start)
      ;; uncomment and format first line
      ;; (remove initial ;'s and backslash quote the library name)
      (when (looking-at ";;; *\\(.*?\\) *--+ *\\(.*\\)")
        (replace-match " /\\1/ --- \\2"))
      ;; remove elisp comment chars (;'s)
      (org-readme-regexp-pairs [["^ *;; ?" ""]])
      ;; replace filename with orglink to filename
      (goto-start)
      (when (re-search-forward "[Ff]ile[Nn]ame: *\\(.*\\) *$" nil t)
	(replace-match "Filename: [[file:\\1][\\1]]"))
      ;; format other info lines into an org list
      (org-readme-regexp-pairs [["^\\(.*?\\):\\(.*?[A-Za-z0-9.].*\\)$" " - \\1 ::\\2"]])
      ;; add the header line
      (goto-start)
      (insert "* Library Information\n")
      ;; make new header for dependencies info
      (org-readme-regexp-pairs (list (list org-readme-features-regexp
					   "* Possible Dependencies")) t t)
      ;; save new org-formatted text into `top-header'
      (setq top-header (buffer-substring (point-min) (point-max))))
    ;; Read the readme file and replace the "Library Information"
    ;; and "Possible Dependencies" sections with the new org-formatted text
    (with-temp-buffer
      (insert-file-contents readme)
      (org-readme-remove-section "Possible Dependencies")
      (org-readme-remove-section "Library Information" top-header nil t)
      ;; save the new readme file
      (write-file readme))))

;;;###autoload
(defun org-readme-changelog-to-readme ()
  "This puts the Emacs Lisp change-log into the Readme.org file."
  (interactive)
  (when (buffer-file-name)
    (let ((readme (org-readme-find-readme))
          pt1 pt2 txt)
      (save-excursion
        (goto-start)
        (when (re-search-forward "^[ \t]*;;; \\(?:Change Log\\|History\\):[ \t]*$" nil t)
          (setq pt1 (point))
          (when (re-search-forward org-readme-end-section-regexp nil t)
            (setq pt2 (match-beginning 0)
		  txt (buffer-substring-no-properties pt1 pt2))
	    (with-temp-buffer
	      (insert txt)
	      ;; Remove initial ;'s
	      (org-readme-regexp-pairs [["^[ \t]*;+ ?" ""]])
	      (goto-start)
	      (cl-symbol-macrolet	;a couple of symbol-macros to save space
		  ((comment (save-match-data
			      (replace-regexp-in-string
			       "~~~~" "\n    + "
			       (replace-regexp-in-string
				"  +" " "
				(replace-regexp-in-string
				 "\n" " "
				 (replace-regexp-in-string
				  "\n[ \t]*[*-+] +" "~~~~" (match-string 3)))))))
		   (author (save-match-data (replace-regexp-in-string "[ \t]*$" "" (match-string 2)))))
		;; copy and reformat each changelog line
		(while (re-search-forward org-readme-changelog-lines-regexp nil t)
		  (replace-match
		   (format " - %s :: %s (%s)\n %s"
			   (match-string 1) comment author (match-string 4))
		   t t)
		  (beginning-of-line))
		;; copy and reformat final changelog line
		(when (re-search-forward org-readme-final-changelog-line-regexp nil t)
		  (replace-match
		   (format " - %s :: %s (%s)\n" (match-string 1) comment author)
		   t t)))
	      ;; replace `' quotes with = quotes, and initial whitespace with single whitespace
	      (org-readme-regexp-pairs [["`\\(.*?\\)'" "=\\1="]
					["^[ \t][ \t]+[-]" " -"]])
	      ;; add org header
	      (goto-start)
	      (insert "* History\n")
	      (setq txt (buffer-substring-no-properties (point-min) (point-max))))
	    ;; add to readme file and save it
	    (with-temp-buffer
	      (insert-file-contents readme)
	      (org-readme-remove-section "History" txt)
	      (write-file readme))))))))

(defun org-readme-get-required-features nil
  "Return list of names of libraries required in current file."
  (save-excursion
    (goto-start)
    (cl-loop with form
	     while (setq form (condition-case v
				  (read (current-buffer)) (error nil)))
	     if (eq (car form) 'require)
	     collect (symbol-name (cl-cadadr form)))))

;;;###autoload
(defun org-readme-update-required-features-section nil
  "Update the required features section of the elisp file."
  (interactive)
  (save-excursion
    (goto-start)
    (if (re-search-forward (replace-regexp-in-string
			    "\\$" "\n;;[ \t]*\n;;[ \t]*\\\\(.*\\\\)$"
			    (replace-regexp-in-string
			     "\\^" ";;**" org-readme-features-regexp)) nil t)
	(replace-match (mapconcat 'identity (org-readme-get-required-features) " ")
		       nil nil nil 1)
      (unless (not (re-search-forward ";;;;+\\|;;+[ \t]Commentary" nil t))
	(forward-line -1)
	(insert (concat ";;\n;; Features that might be required by this library:\n;;\n;;   "
			(mapconcat 'identity (org-readme-get-required-features) " ")
			"\n;;\n"))))))

(provide 'org-readme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-readme.el ends here
