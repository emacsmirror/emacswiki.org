;;; org-readme.el --- Integrates Readme.org and Commentary/Change-logs.
;; 
;; Filename: org-readme.el
;; Description: Integrate Readme.org and Commentary/Change Logs.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri Aug  3 22:33:41 2012 (-0500)
;; Version: 20151112.2115
;; Package-Requires: ((http-post-simple "1.0") (yaoddmuse "0.1.1")(header2 "21.0") (lib-requires "21.0"))
;; Last-Updated: Thu Nov 12 23:37:26 2012 (-0500)
;;           By: Joe Bloggs
;;     Update #: 794
;; URL: https://github.com/mlf176f2/org-readme
;; Keywords: Header2, Readme.org, Emacswiki, Git
;; Compatibility: Tested with Emacs 24.1 on Windows.
;;
;; Features that might be required by this library:
;;
;;   yaoddmuse, http-post-simple, org-html, header2
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; * Using org-readme
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
;; 
;; All other sections of the Readme.org are then put into the
;; "Commentary" section of the readme.org.
;; 
;; In addition this library defines `org-readme-sync',  a convenience function that:
;; 
;; - Asks for a commentary about the library change.
;;   - To exit/save press `C-c C-c'
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
;; - If you are using github, this library creates a melpa recipie.
;; - If you are using github, this library creates a el-get recipie. 
;; 
;; When `org-readme-sync' is called in a `Readme.org' file that is not a
;; single lisp file, the function exports the readme in EmacsWiki format
;; and posts it to the EmacsWiki.
;; ** EmacsWiki Page Names
;; EmacsWiki Page names are generated from the file.  `org-readme.el'
;; would generate a page of OrgReadme.
;; 
;; ** Why each required library is needed
;; There are a few required libraries.  This is a list of the require
;; libraries and why they are needed.
;; 
;; |------------------+--------------------------------------|
;; | Library          | Why it is needed                     |
;; |------------------+--------------------------------------|
;; | yaoddmuse        | Publish to emacswiki                 |
;; | http-post-simple | Publish to marmalade-repo.org        |
;; | header2          | To create header and changelog       |
;; | lib-requires     | To generate the library dependencies |
;; |------------------+--------------------------------------|
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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

(require 'yaoddmuse nil t)
(require 'http-post-simple nil t)
(require 'org-html nil t)

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

(defcustom org-readme-use-melpa-versions nil
  "Use Melpa-type versions YYYYMMDD.HHMM instead of 0.0.0 versions."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-create-tar-package nil
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

(defcustom org-readme-author-name nil
  "Name to use as author when updating \"Last Updated\" info in elisp header."
  :type 'string
  :group 'org-readme)

(defcustom org-readme-sync-emacswiki t
  "Post library to the emacswiki.
Requires `yaoddmuse'."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-sync-marmalade t
  "Post library to marmalade-repo.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-sync-git t
  "Post library to git."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-melpa-recipe t
  "Build a melpa recipe based on github information."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-el-get-recipe t
  "Build an el-get recipe based on github information."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-markdown t
  "Build Readme.md from Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-use-pandoc-markdown t
  "Use pandoc's grid tables instead of transferring the tables to html."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-texi t
  "Build library-name.texi from Readme.org, using Readme.md and pandoc.
Requires `org-readme-build-markdown' to be non-nil as pandoc to be found."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-drop-markdown-after-build-texi t
  "Remove Readme.md after texinfo is generated."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-build-info t
  "Build library-name.info from Reade.org using texi.  
Requires `org-readme-build-texi' to be non-nil, pandoc and makeinfo to be found. 
This will also create the directory entry using install-info, if it is found."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-drop-texi-after-build-info t
  "Remove the texi information after building info files."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-readme-to-lisp-file t
  "Update elisp file header with commentary section of Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-functions-to-readme t
  "Add a Functions section to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-variables-to-readme t
  "Add a Variables section to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-update-changelog t
  "Add/update Changelog file."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-changelog-to-readme t
  "Add Changelog information to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(defcustom org-readme-add-top-header-to-readme t
  "Add Top Header information to Readme.org."
  :type 'yesnoprompt
  :group 'org-readme)

(cl-defmacro org-readme-check-opt (opt &optional prompt)
  "Query user if option OPT is 'prompt, otherwise return OPT.
If PROMPT is supplied use that for the prompt, otherwise use
the first sentence of the docstring for OPT."
  `(if (eq ,opt 'prompt)
       (y-or-n-p (or ,prompt
		     (replace-regexp-in-string
		      "\n.*" ""
		      (documentation-property
		       ',opt 'variable-documentation))))
     ,opt))

;; The following function is a slightly modified version of `xah-replace-regexp-pairs-region'
;; available here: https://github.com/xahlee/xah-replace-pairs
(defun org-readme-regexp-pairs (pairs &optional fixedcase literal string subexp)
  "Replace regex string find/replace PAIRS in buffer.
BEGIN END are the region boundaries.
PAIRS is: [[regexStr1 replaceStr1] [regexStr2 replaceStr2] …]
It can be list or vector, for the elements or the entire argument.
The optional arguments FIXEDCASE, LITERAL, STRING & SUBEXP are the same as in `replace-match'."
  (save-restriction
    (mapc
     (lambda (x)
       (goto-char (point-min))
       (while (search-forward-regexp (elt x 0) (point-max) t)
	 (replace-match (elt x 1) fixedcase literal)))
     pairs)))

;; (dexfxun org-readme-update-last-update nil
;;   "Change the \"Last Updated:\" date in the elisp file."
;;   (save-excursion
;;     (goto-char (point-min))
;;     ;; Last-Updated: Wed Aug 22 13:11:26 2012 (-0500)
;;     ;;           By: Matthew L. Fidler
;;     ;;     Update #: 794
;;     (if (re-search-forward "^ *Last Updated: *\\(.*\\)" nil t)
;; 	(replace-match (current-time-string) t nil nil 1))
;;     (if (re-search-forward "^ *By: *\\(.*\\)" nil t)
;; 	(replace-match (or org-readme-author-name
;; 			   (read-string "Author name: ")) t nil nil 1))
;;     (if (re-search-forward "^ *Update #: *\\(.*\\)" nil t)
;; 	(replace-match (number-to-string (1+ (string-to-number (match-string 1))))
;; 		       t nil nil 1))
;;     ;;org-readme-author-name
;;     )
;;   )

(defun org-readme-insert-functions ()
  "Extracts function & macro documentation and places it in the Readme.org file."
  (save-excursion
    (goto-char (point-min))
    (let ((lst1 '()) tmp ret1 ret2 ret lst
          (readme (org-readme-find-readme)))
      (while (re-search-forward "(\\(?:cl-\\)?def\\(?:un\\|macro\\)[*]?[ \t\n]+\\([^ \t\n]+\\)" nil t)
        (add-to-list 'lst1 (match-string-no-properties 1)))
      (setq lst (sort lst1 'string<))
      (cl-flet ((fd (x)
		    (with-temp-buffer
		      (insert x)
		      (goto-char (point-min))
		      (when (re-search-forward "'[.]" nil t)
			(skip-chars-forward " \t\n")
			(delete-region (point) (point-min)))
		      (goto-char (point-min))
		      (when (re-search-forward "[(]" nil t)
			(goto-char (match-beginning 0))
			(insert "=")
			(forward-list)
			(insert "="))
		      (org-readme-regexp-pairs [["`\\(.*?\\)'" "=\\1="] ["^[ \t]*[*]+[ \t]+" " - "]])
		      (goto-char (point-max))
		      (insert "\n")
		      (org-readme-regexp-pairs [["^[ \t]*[*]+" ""]])
		      (buffer-string))))
        (setq ret1 "** Interactive Functions\n")
        (setq ret2 "** Internal Functions\n")
	(setq ret3 "** Macros\n")
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
                  (t
                   (setq ret2 (concat ret2 "\n*** " x "\n" (fd tmp))))))
             (error nil)))
         lst)
        (setq ret (concat "* Functions & macros\n" ret1 "\n" ret2 "\n" ret3)))
      (with-temp-buffer
        (insert-file-contents readme)
        (org-readme-remove-section "Functions & macros" ret)
        (write-file readme)))))

(defun org-readme-insert-variables ()
  "Extracts variable documentation and places it in the readme file."
  (interactive)
  (condition-case err
      (eval-buffer)
    (error nil))
  (save-excursion
    (goto-char (point-min))
    (let ((lst1 '()) tmp ret1 ret2 ret lst
          (readme (org-readme-find-readme)))
      (while (re-search-forward "(def\\(?:var\\|var-local\\|custom\\)[*]?[ \t\n]+\\([^ \t\n]+\\)" nil t)
        (add-to-list 'lst1 (match-string-no-properties 1)))
      (setq lst (sort lst1 'string<))
      (cl-flet ((fd (x)
		    (with-temp-buffer
		      (insert x)
		      (goto-char (point-min))
		      (when (re-search-forward "Documentation:" nil t)
			(skip-chars-forward " \t\n")
			(delete-region (point) (point-min)))
		      (when (re-search-forward "You can customize this variable" nil t)
			(goto-char (match-beginning 0))
			(skip-chars-backward " \t\n")
			(delete-region (point) (point-max)))
		      (org-readme-regexp-pairs [["`\\(.*?\\)'" "=\\1="] ["^[ \t]*[*]+[ \t]+" " - "]])
		      (goto-char (point-max))
		      (insert "\n")
		      (org-readme-regexp-pairs [["^[ \t]*[*]+" ""]])
		      (buffer-string))))
	(setq ret1 "** Customizable Variables\n")
	(setq ret2 "** Internal Variables\n")
	(mapc
	 (lambda(x)
	   (condition-case err
	       (when (intern x)
		 (setq tmp (describe-variable (intern x)))
		 (cond
		  ((string-match "Not documented" tmp))
		  ((string-match "customize" tmp)
		   (setq ret1 (concat ret1 "\n*** " x "\n" (fd tmp))))
		  (t
		   (setq ret2 (concat ret2 "\n*** " x "\n" (fd tmp))))))
	     (error nil)))
	 lst)
	(setq ret (concat "* Variables\n" ret1 "\n" ret2)))
      (with-temp-buffer
	(insert-file-contents readme)
	(org-readme-remove-section "Variables" ret)
	(write-file readme)))))

(defun org-readme-build-el-get ()
  "Builds an el-get recipe. This assumes github, though others could be added.
Returns file name if created."
  (let ((el-get (expand-file-name
                 "el-get" (file-name-directory (buffer-file-name))))
        (lib-name (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name))))
        (git-cfg
         (expand-file-name
          "config"
          (expand-file-name
           ".git" (file-name-directory (buffer-file-name)))))
        github tmp
        rcp)
    (unless (file-exists-p el-get)
      (make-directory el-get))
    (setq el-get (expand-file-name lib-name el-get))
    (when (file-exists-p git-cfg)
      (with-temp-buffer
        (insert-file-contents git-cfg)
        (goto-char (point-min))
        (when (re-search-forward "git@github.com:\\(.*?\\)[.]git" nil t)
          (setq github (match-string 1))))
      (setq rcp
            (format "(:name %s\n :description \"%s\"%s\n :website \"%s\"\n :type git\n :url \"%s\")"
                    lib-name
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward "^[ \t]*;;;[ \t]*%s[.]el[ \t]*--+[ \t]*\\(.*?\\)[ \t]*$" nil t)
                          (match-string 1)
                        lib-name))
                    ;; :depends
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward "^[ \t]*;+[ \t]*[Pp]ackage-[Rr]equires:[ \t]*\\(.*?\\)[ \t]*$" nil t)
                          (condition-case err
                              (progn
                                (setq tmp (match-string 1))
                                (with-temp-buffer
                                  (insert (format "(setq tmp '%s)" tmp))
                                  (condition-case err
                                      (eval-buffer)
                                    (error nil)))
                                (format "\n :depends (%s)"
                                        (mapconcat
                                         (lambda(x)
                                           (symbol-name (car x)))
                                         tmp " ")))
                            (error (message "Error parsing package-requires: %s" err)
                                   ""))
                        ""))
                    ;; Website
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward "^[ \t]*;+URL:[ \t]*\\(.*\\)[ \t]*$" nil t)
                          (match-string 1)
                        (format "https://github.com/%s" github)))
                    ;; Github
                    (format "https://github.com/%s.git" github)
                    lib-name))
      (when rcp (with-temp-file el-get (insert rcp))))
    (if (file-exists-p el-get) (symbol-value 'el-get) nil)))

(defun org-readme-build-melpa ()
  "Builds a melpa recipe. This assumes github, though other could be added.
Returns file name if created."
  (let ((melpa (expand-file-name
                "melpa" (file-name-directory (buffer-file-name))))
        (lib-name (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name))))
        (git-cfg
         (expand-file-name
          "config"
          (expand-file-name
           ".git" (file-name-directory (buffer-file-name)))))
        rcp)
    (unless (file-exists-p melpa)
      (make-directory melpa))
    (setq melpa (expand-file-name lib-name melpa))
    (when (file-exists-p git-cfg)
      (with-temp-buffer
        (insert-file-contents git-cfg)
        (goto-char (point-min))
        (when (re-search-forward "git@github.com:\\(.*?\\)[.]git")
          (setq rcp
                (format "(%s\n :repo \"%s\"\n :fetcher github\n :files (\"%s.el\" \"dir\" \"%s.info\"))"
                        lib-name (match-string 1) lib-name lib-name))))
      (when rcp (with-temp-file melpa (insert rcp))))
    (if (file-exists-p melpa) (symbol-value 'melpa) nil)))

(defun org-readme-buffer-version ()
  "Gets the version of the current buffer."
  (let (ver
        (case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^ *;+ *Version: *\\(.*?\\) *$" nil t)
        (setq ver (match-string-no-properties 1))))
    (symbol-value 'ver)))

(defun org-readme-marmalade-post ()
  "Posts the current buffer to Marmalade."
  (interactive)
  (let* ((package (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name))))
         (m-ver (org-readme-marmalade-version package))
         (b-ver (org-readme-buffer-version))
         token
         resp)
    (message "Marmalade %s Version: %s, Buffer Version: %s"
             package m-ver b-ver)
    (when (or (not m-ver) (not (string= m-ver b-ver)))
      (message "Should post %s, the marmalade package is outdated or does not exist."
               package)
      (setq token (org-readme-token))
      
      (setq resp (http-post-simple-multipart
                  (format "%s/v1/packages"
                          org-readme-marmalade-server)
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
                           org-readme-marmalade-server
                           package)))
        ver)
    (when ver-json
      (save-excursion
        (set-buffer ver-json)
        (goto-char (point-min))
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
                         (nth 0
                              (http-post-simple
                               (format "%s/v1/users/login"
                                       org-readme-marmalade-server)
                               `((name . ,user-name)
                                 (password . ,password))))))
                (goto-char (point-min))
                
                (if (not (re-search-forward "\"token\"[ \t]*:[ \t]*\"\\(.*?\\)\"" nil t))
                    nil
                  (match-string 1))))
        (when token
          (setq org-readme-marmalade-user-name user-name)
          (setq org-readme-marmalade-token token)
          (customize-save-variable 'org-readme-marmalade-user-name org-readme-marmalade-user-name)
          (customize-save-variable 'org-readme-marmalade-token org-readme-marmalade-token))
        (symbol-value 'token))))

(defcustom org-readme-remove-sections
  '("History" "Possible Dependencies" "Library Information"
    "Functions & macros" "Variables")
  "List of sections to remove when changing the Readme.org to Commentary."
  :group 'org-readme
  :type '(repeat (string :tag "Section")))

(defcustom org-readme-remove-sections-from-markdown
  '("Functions & macros" "Variables")
  "List of sections to remove when changing the Readme.org to Markdown which is an intermediary for texinfo (using pandoc)."
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

(defun org-readme-edit-commit ()
  "Changelog for editing."
  (interactive)
  (let ((comment (buffer-substring (point-min) (point-max)))
        mr)                             
    (kill-buffer (get-buffer "*Change Comment*"))
    (with-temp-buffer
      (insert comment)
      (goto-char (point-min))
      (end-of-line)
      (while (re-search-forward "^" nil t)
        (insert ";;    "))
      (setq mr (buffer-substring (point-min) (point-max))))
    (set-buffer org-readme-edit-last-buffer)
    (make-revision)
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

(defvar org-readme-edit-last-window-configuration nil)

(defvar org-readme-edit-last-buffer nil)

(defun org-readme-edit ()
  "Edit change comment for commit."
  (interactive)
  (unless org-readme-edit-last-window-configuration
    (setq org-readme-edit-last-window-configuration (current-window-configuration)))
  (switch-to-buffer-other-window (get-buffer-create "*Change Comment*"))
  (org-readme-edit-mode))

(define-derived-mode org-readme-edit-mode text-mode "Org-readme Log edit.")

(defun org-readme-convert-to-markdown ()
  "Converts Readme.org to markdown Readme.md."
  (interactive)
  (let ((readme (org-readme-find-readme))
        (what "Readme.md")
        p1 md tmp tmp2)
    (with-temp-buffer
      (insert-file-contents readme)
      (mapc
       (lambda(section)
         (org-readme-remove-section section))
       org-readme-remove-sections-from-markdown)
      ;; Convert org keywords
      (org-readme-regexp-pairs [["#[+]TITLE:" "+TITLE:"]
				["#[+]AUTHOR:" ""]
				["^[ \t]*#.*" ""]
				["[+]TITLE:" "# "]])
      ;; Convert Headings
      (goto-char (point-min))
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
      (goto-char (point-min))
      (while (re-search-forward "[*]\\(.*+?\\)[*]" nil t)
	(unless (save-match-data (string-match "^[*]+$" (match-string 1)))
	  (replace-match "__\\1__" t)))

      (org-readme-regexp-pairs [["^[ \t]*[+-] +\\(.*?\\) *::" "- __\\1__ -- "]
				;; Code blocks
				["=\\(.*+?\\) *=" "`\\1`"]])
      
      (goto-char (point-min))
      (while (re-search-forward "^ *#[+]BEGIN_SRC.*" nil t)
	(setq tmp (point))
	(when (re-search-forward "^ *#[+]END_SRC" nil t)
	  (beginning-of-line)
	  (setq tmp2 (point))
	  (goto-char tmp)
	  (while (and (> tmp2 (point))
		      (re-search-forward "^" tmp2 t))
	    (replace-match "::::"))))
      
      (goto-char (point-min))
      (while (re-search-forward "^: " nil t)
	(replace-match "\n::::" t t) ;
	(while (progn
		 (end-of-line)
		 (re-search-forward "\\=\n: " nil t))
	  (replace-match "\n:::: "))
	(end-of-line))
      ;; Convert pre-formatted
      (org-readme-regexp-pairs [["^::::" "    "]])
      ;; Convert tables to html
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|.*|[ \t]*$" nil t)
	(beginning-of-line)
	(setq p1 (point))
	(end-of-line)
	(while (re-search-forward "\\=\n[ \t]*|" nil t)
	  (end-of-line))
	(end-of-line)
	(save-restriction
	  (narrow-to-region p1 (point))
	  (if (org-readme-check-opt org-readme-use-pandoc-markdown)
	      (org-readme-regexp-pairs [["^\\([ \t]*\\)|\\(-.*?-\\)|\\([ \t]*\\)$" "\\1+\\2+\\3"]])
	    (if (featurep 'org-html)
		(org-replace-region-by-html (point-min) (point-max))
	      (org-export-replace-region-by 'html))
	    (org-readme-regexp-pairs [["class" "align"]]))))
      ;; Lists are the same.
      (setq readme (buffer-string)))
    (with-temp-file (expand-file-name
		     "Readme.md"
		     (file-name-directory (buffer-file-name)))
      (insert readme))))

(defun org-readme-convert-to-emacswiki ()
  "Converts Readme.org to oddmuse markup and uploads to emacswiki."
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
      (goto-char (point-min))
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
      (goto-char (point-min))
      (while (re-search-forward "^\\([*]+\\) *!?\\(.*\\)" nil t)
	(setq tmp (make-string (min 4 (+ 1 (length (match-string 1)))) ?=))
	(replace-match (format "%s %s %s" tmp (match-string 2) tmp) t t)
	(beginning-of-line)
	(let ((case-fold-search nil))
	  (while (re-search-forward "!\\([A-Z][a-z]+[A-Z][A-Za-z]*\\)" (point-at-eol) t)
	    (replace-match "\\1" t))))
      (goto-char (point-min))
      (while (re-search-forward "^: " nil t)
	(replace-match "<pre>\n::::" t t) ;
	(while (progn (end-of-line)
		      (re-search-forward "\\=\n: " nil t))
	  (replace-match "\n:::: "))
	(end-of-line)
	(insert "\n</pre>"))
      (goto-char (point-min))
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
      (goto-char (point-min))
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
      (goto-char (point-min))           
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

(defun org-readme-git ()
  "Add The files to git."
  (interactive)
  (let* ((df (file-name-directory (buffer-file-name)))
         (default-directory df)
         melpa el-get)
    (when (org-readme-check-opt org-readme-build-melpa-recipe)
      (setq melpa (org-readme-build-melpa))
      (when melpa
        (message "Adding Melpa recipe")
        (shell-command (format "git add melpa/%s" (file-name-nondirectory melpa)))))
    
    (when (org-readme-check-opt org-readme-build-el-get-recipe)
      (setq el-get (org-readme-build-el-get))
      (when el-get
        (message "Adding El-Get recipe")
        (shell-command (format "git add el-get/%s" (file-name-nondirectory el-get)))))
    
    (message "Git Adding Readme")
    (shell-command
     (format "git add %s"
             (file-name-nondirectory (org-readme-find-readme))))
    
    (when (file-exists-p (concat (file-name-sans-extension
				  (file-name-nondirectory (buffer-file-name)))
				 ".texi"))
      (when (and (org-readme-check-opt org-readme-drop-markdown-after-build-texi)
                 (file-exists-p "Readme.md"))
        (delete-file "Readme.md")
        (shell-command
         (concat "git rm Readme.md")))
      (if (and (org-readme-check-opt org-readme-drop-texi-after-build-info)
               (file-exists-p (concat
                               (file-name-sans-extension
                                (file-name-nondirectory (buffer-file-name)))
                               ".info")))
          (progn
            (delete-file (concat
                          (file-name-sans-extension
                           (file-name-nondirectory (buffer-file-name)))
                          ".texi"))
            (shell-command
             (concat "git add "
                     (concat
                      (file-name-sans-extension
                       (file-name-nondirectory (buffer-file-name)))
                      ".info")))
            (if (file-exists-p (expand-file-name "dir"
                                                 (file-name-directory (buffer-file-name))))
                (shell-command
                 (concat "git add dir")))
            (shell-command
             (concat "git rm "
                     (file-name-sans-extension
                      (file-name-nondirectory (buffer-file-name)))
                     ".texi")))
	(shell-command
	 (concat "git add "
		 (concat
		  (file-name-sans-extension
		   (file-name-nondirectory (buffer-file-name)))
		  ".texi")))))
    
    (when (file-exists-p "Readme.md") (shell-command "git add Readme.md"))
    
    (message "Git Adding %s" (file-name-nondirectory (buffer-file-name)))
    (shell-command
     (format "git add %s"
             (file-name-nondirectory (buffer-file-name))))
    
    (when (file-exists-p (org-readme-get-change))
      (message "Git Committing")
      (shell-command
       (format "git commit -F %s"
               (file-name-nondirectory
                (org-readme-get-change))))
      (delete-file (org-readme-get-change))
      (message "Git push")
      (shell-command "git push")
      (let ((tags (shell-command-to-string "git tag"))
            (ver  (org-readme-buffer-version)))
        (when ver
          (unless (string-match (concat "v" (regexp-quote ver)) tags)
            (message "Tagging the new version")
            (message "git tag -a v%s -m \"version %s\"" ver ver)
            (shell-command (format "git tag -a v%s -m \"version %s\"" ver ver))
            (shell-command "git push --tags")))))))

(defun org-readme-in-readme-org-p ()
  "Determine if the currently open buffer is the Readme.org"
  (string= "readme.org"
           (downcase (file-name-nondirectory (buffer-file-name)))))

(defun org-readme-single-lisp-p ()
  "Determine if the Readme.org is in a directory with a single Lisp file.
If so, return the name of that Lisp file, otherwise return nil."
  (let* ((dn (file-name-directory (buffer-file-name)))
         (df (directory-files dn t "[.][Ee][Ll]$")))
    (if (= 1 (length df))
        (progn
          (setq df (nth 0 df))
          (symbol-value 'df))
      nil)))

;;;###autoload
(defun org-readme-gen-info ()
  "With the proper tools, generates an info and dir from the current readme.org"
  (interactive)
  (when (org-readme-check-opt org-readme-build-markdown) 
    (org-readme-convert-to-markdown)
    (when (org-readme-check-opt org-readme-build-texi)
      (when (executable-find "pandoc")
        (let ((default-directory (file-name-directory (buffer-file-name)))
              (base (file-name-sans-extension
                     (file-name-nondirectory (buffer-file-name))))
              (file (concat (file-name-sans-extension
                             (file-name-nondirectory (buffer-file-name)))
                            ".texi"))
              pkg ver desc cnt)
          (when (string= (downcase base) "readme")
            (let ((df (directory-files (file-name-directory (buffer-file-name)) t ".*[.]el$")))
              (unless (= 1 (length df))
                (setq df (directory-files (file-name-directory (buffer-file-name)) t ".*-mode[.]el$")))
              (unless (= 1 (length df))
                (setq df (directory-files (file-name-directory (buffer-file-name)) t ".*-pkg[.]el$")))
              (when (= 1 (length df))
                (setq base (file-name-sans-extension (file-name-nondirectory (nth 0 df))))
                (setq file (concat base ".texi")))))
          (shell-command (concat "pandoc Readme.md -s -o " file))
          ;; Now add direntry.
          (setq cnt (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (if (not (search-forward "@strong{Description} -- " nil t))
                          (setq desc base)
                        (setq desc (buffer-substring (point) (point-at-eol))))
                      (goto-char (point-min))
                      (if (not (search-forward "@strong{Package-Requires} -- " nil t))
                          (setq pkg "()")
                        (setq pkg (buffer-substring (point) (point-at-eol))))
                      (goto-char (point-min))
                      (if (not (search-forward "@strong{Version} -- " nil t))
                          (setq ver "0.0")
                        (setq ver (buffer-substring (point) (point-at-eol))))
                      (buffer-string)))
          (with-temp-file file
            (insert cnt)
            (goto-char (point-min))
            (when (re-search-forward "@documentencoding")
              (goto-char (point-at-eol))
              (insert "\n@dircategory Emacs lisp libraries\n@direntry\n* "
		      base ": (" base ").     " desc "\n@end direntry\n")))
          (when (and (org-readme-check-opt org-readme-build-info)
                     (executable-find "makeinfo"))
            (shell-command (concat "makeinfo " base ".texi"))
            (when (executable-find "install-info")
              (shell-command (concat "install-info --dir-file=dir " base ".info")))))))))

;;;###autoload
(defun org-readme-sync (&optional comment-added)
  "Syncs Readme.org with current buffer.
When COMMENT-ADDED is non-nil, the comment has been added and the syncing should begin."
  (interactive)
  ;; Store the name of the package in `base'
  (let ((base (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name)))))
    (when (string= (downcase base) "readme")
      (let ((df (directory-files (file-name-directory (buffer-file-name)) t ".*[.]el$")))
        (unless (= 1 (length df))
          (setq df (directory-files (file-name-directory (buffer-file-name)) t ".*-mode[.]el$")))
        (unless (= 1 (length df))
          (setq df (directory-files (file-name-directory (buffer-file-name)) t ".*-pkg[.]el$")))
        (when (= 1 (length df))
          (setq base (file-name-sans-extension (file-name-nondirectory (nth 0 df)))))))
    ;; Check if we need to switch file or update the changelog first
    (if (and (not comment-added)
	     (org-readme-in-readme-org-p))
        (let ((single-lisp-file (org-readme-single-lisp-p)))
          (message "In Readme.org")
	  ;; If there's only one lisp file, switch to it, and start again.
          (if single-lisp-file
              (progn
                (setq org-readme-edit-last-window-configuration (current-window-configuration))
                (find-file single-lisp-file)
                (setq org-readme-edit-last-buffer (current-buffer))
                (org-readme-sync))
            ;; Post to emacswiki if necessary
	    (unless (not (org-readme-check-opt
			  org-readme-sync-emacswiki
			  "Post Readme.org to emacswiki"))
	      (message "Posting Description to emacswiki")
	      (org-readme-convert-to-emacswiki))))
      (if (and (not comment-added)
	       (org-readme-check-opt org-readme-update-changelog))
	  ;; Update the Changelog file if necessary
          (progn
            (setq org-readme-edit-last-buffer (current-buffer))
            (org-readme-edit))
	;; Update version number
        (when (yes-or-no-p "Update version number? ")
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search t))
              (when (re-search-forward "^[ \t]*;+[ \t]*Version:" nil t)
                (if (or (org-readme-check-opt org-readme-use-melpa-versions)
                        (save-match-data (looking-at "[ \t]*[0-9]\\{8\\}[.][0-9]\\{4\\}[ \t]*$")))
                    (progn
                      (delete-region (point) (point-at-eol))
                      (insert (concat " " (format-time-string "%Y%m%d." (current-time))
                                      (format "%d" (or (string-to-number (format-time-string "%H%M" (current-time))) 0)))))
                  (end-of-line)
                  (when (looking-back "\\([ .]\\)\\([0-9]+\\)[ \t]*")
                    (replace-match (format "\\1%s"
                                           (+ 1 (string-to-number (match-string 2)))))))))))
	;; Replace commentary section in elisp file with text extracted from readme file
	(when (org-readme-check-opt org-readme-add-readme-to-lisp-file)
	  (message "Adding Readme to Header Commentary")
	  (org-readme-to-commentary))
	;; Add functions section to readme file
        (when (org-readme-check-opt org-readme-add-functions-to-readme)
          (message "Updating Functions.")
          (org-readme-insert-functions))
	;; Add variables section to readme file
        (when (org-readme-check-opt org-readme-add-variables-to-readme)
          (message "Updating Variables.")
          (org-readme-insert-variables))
	;; Add Changelog to readme file
        (when (org-readme-check-opt org-readme-add-changelog-to-readme)
          (message "Updating Changelog in current file.")
          (org-readme-changelog-to-readme))
	;; Copy top header from elisp file into readme file
        (when (org-readme-check-opt org-readme-add-top-header-to-readme)
          (org-readme-top-header-to-readme))
        (save-buffer)
	;; Create info documentation (if required; checks are done in `org-readme-gen-info')
        (org-readme-gen-info)
	;; Create .tar archive
        (when (file-exists-p (concat base ".tar"))
          (delete-file (concat base ".tar")))
        (when (and (org-readme-check-opt org-readme-create-tar-package)
                   (or (executable-find "tar")
                       (executable-find "7z")
                       (executable-find "7za")))
          (make-directory (concat base "-" ver))
          (copy-file (concat base ".el") (concat base "-" ver "/" base ".el"))
          (copy-file (concat base ".info") (concat base "-" ver "/" base ".info"))
          (copy-file "dir" (concat base "-" ver "/dir"))
          (with-temp-file (concat base "-" ver "/" base "-pkg.el")
            (insert "(define-package \"" base "\" \"" ver  "\" \"" desc "\" '" pkg ")"))
          (if (executable-find "tar")
              (shell-command (concat "tar -cvf " base ".tar " base "-" ver "/"))
            (shell-command (concat "7z" (if (executable-find "7za") "a" "")
                                   " -ttar -so " base ".tar " base "-" ver "/*.*")))
	  (mapcar 'delete-file
		  (list (concat base "-" ver "/" base ".el")
			(concat base "-" ver "/" base "-pkg.el")
			(concat base "-" ver "/" base ".info")
			(concat base "-" ver "/dir")))
          (delete-directory (concat base "-" ver)))
	;; post to marmalade
        (when (and (featurep 'http-post-simple)
                   (org-readme-check-opt org-readme-sync-marmalade))
          (message "Attempting to post to marmalade-repo.org")
          (org-readme-marmalade-post))
	;; post to elisp file to emacswiki
        (when (and (featurep 'yaoddmuse)
                   (org-readme-check-opt
		    org-readme-sync-emacswiki
		    "Post elisp file to emacswiki?"))
          (message "Posting elisp file to emacswiki")
          (emacswiki-post nil ""))
	;; add files to git repo
        (when (org-readme-check-opt org-readme-sync-git) (org-readme-git))
	;; post readme file to emacswiki
        (when (and (featurep 'yaoddmuse)
                   (org-readme-check-opt
		    org-readme-sync-emacswiki
		    "Post Readme.org to emacswiki?"))
          (message "Posting Description to emacswiki")
          (org-readme-convert-to-emacswiki))
	;; revert the window config back to how it was before
        (when org-readme-edit-last-window-configuration
          (set-window-configuration org-readme-edit-last-window-configuration)
          (setq org-readme-edit-last-window-configuration nil))))))

;;;###autoload
(defun org-readme-to-commentary ()
  "Replace Commentary section in elisp file with text from Readme.org."
  (interactive)
  (let ((readme (org-readme-find-readme)) p1)
    (with-temp-buffer
      (insert-file-contents readme)
      (org-mode)
      ;; remove some sections
      (mapc
       (lambda (section)
         (org-readme-remove-section section))
       org-readme-remove-sections)
      ;; remove stuff
      (org-readme-regexp-pairs [["=\\<\\(.*?\\)\\>=" "`\\1'"] ;replace =SYMBOL= with `SYMBOL'
				["#.*" ""] ;remove all org #+KEYWORDS
				["^[ \t]*[A-Z]+:[ \t]*\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*" ""]
				["^:" ""] ;remove : at beginning of lines
				])
      ;; remove all TODO items
      (goto-char (point-min))
      (when org-todo-keyword-faces
        (while (org-readme-remove-section
                (regexp-opt
                 (mapcar
                  (lambda(x)
                    (nth 0 x))
                  org-todo-keyword-faces)) nil t)))
      ;; replace initial & final whitespace with single newline chars
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      (delete-region (point-min) (point))
      (insert "\n")
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n")
      ;; comment all lines with ;;
      (org-readme-regexp-pairs [["^\\(.?\\)" ";; \\1"]])
      (setq readme (buffer-string)))
    ;; delete current "Commentary" region in elisp file, and replace
    ;; with text extracted from Readme.org
    (goto-char (point-min))
    (when (re-search-forward "^;;;[ \t]*Commentary:[ \t]*$" nil t)
      (skip-chars-forward "\n")
      (let ((pt (point)))
        (when (re-search-forward "^;;;;+[ \t]*$" nil t)
          (goto-char (match-beginning 0))
          (skip-chars-backward "\n")
          (delete-region pt (point))
          (insert readme))))))

(defun org-readme-get-emacswiki-name ()
  "Gets emacswiki-style name based on buffer."
  (if (org-readme-in-readme-org-p)
      (let ((wiki (file-name-nondirectory (substring (file-name-directory (buffer-file-name)) 0 -1))))
        (with-temp-buffer
          (insert wiki)
          (goto-char (point-min))
          (when (looking-at ".") (replace-match (upcase (match-string 0)) t t))
          (while (re-search-forward "[-._]\\(.\\)" nil t)
            (replace-match  (upcase (match-string 1))) t t)
          (setq wiki (buffer-substring (point-min) (point-max))))
        (symbol-value 'wiki))
    (let ((dir (file-name-directory (buffer-file-name)))
          (name (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      (with-temp-buffer
        (insert (downcase name))
        (goto-char (point-min))
        (when (looking-at ".") (replace-match (upcase (match-string 0)) t t))
        (while (re-search-forward "-\\(.\\)" nil t)
          (replace-match  (upcase (match-string 1))) t t)
        (setq name (concat dir (buffer-substring (point-min) (point-max)))))
      (symbol-value 'name))))

(defun org-readme-get-change ()
  "Get file for changelog commits."
  (expand-file-name "Changelog" (file-name-directory (buffer-file-name))))

(defcustom org-readme-default-template "
* Installation

To use without using a package manager:

 - Put the library in a directory in the emacs load path, like ~/.emacs.d
 - Add (require 'LIB-NAME) in your ~/.emacs file
 - If you have [[http://www.marmalade-repo.org/][marmalade-repo.org]], this LIB-NAME is part of the emacs packges you can install.  Just type M-x package-install LIB-NAME marmalade 

This is in emacswiki, so this package can also be installed using el-get.

After installing el-get, Type M-x el-get-install LIB-NAME.
"
  "Default template for blank Readme.org Files. LIB-NAME is replaced with the library."
  :type 'string
  :group 'org-readme)

(defun org-readme-find-readme ()
  "Find the Readme.org."
  (let* ((dir (file-name-directory (buffer-file-name)))
         (df (directory-files dir t "^[Rr][Ee][Aa][Dd][Mm][Ee][.][Oo][Rr][Gg]$")))
    (if (= 1 (length df))
        (setq df (nth 0 df))
      (setq df (expand-file-name "Readme.org" dir))
      (let ((lib-name (file-name-sans-extension
                       (file-name-nondirectory (buffer-file-name)))))
        (with-temp-file df
          (insert org-readme-default-template)
          (goto-char (point-min))
          (while (re-search-forward "LIB-NAME" nil t)
            (replace-match lib-name t t)))))
    (symbol-value 'df)))

(defun org-readme-remove-section (section &optional txt any-level at-beginning)
  "Remove `org-mode' SECTION. Optionally insert TXT.
When ANY-LEVEL is non-nil, any level may be specified.
When AT-BEGINNING is non-nil, if the section is not found, insert it at the beginning."
  (let ((case-fold-search t)
        (mtch ""))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (format "^\\([*]%s\\) +%s"
                   (if any-level "+" "")
                   section)
	   nil t)
          (progn
            (org-cut-subtree)
            (save-excursion (when txt (insert txt)))
            t)
        (when txt
          (goto-char (if at-beginning
                         (point-min)
                       (point-max)))
          ;; Skip comments
          (if at-beginning
              (while (re-search-forward "\\=[ \t]*#.*\n" nil t))
            (while (re-search-backward "\n[ \t]*#.*\\=" nil t)))
          (beginning-of-line)
          (save-excursion (insert txt)))
        nil))))

;;;###autoload
(defun org-readme-top-header-to-readme ()
  "Copy top header from the elisp file into the readme file as Library Information."
  (interactive)
  (let ((top-header "")
        (readme (org-readme-find-readme)))
    ;; copy the top header from the elisp file
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^;;;;+[ \t]*$" nil t)
        (beginning-of-line)
        (setq top-header (buffer-substring (point-min) (point)))))
    ;; copy top header info and reformat it for orgmode
    (with-temp-buffer
      (insert top-header)
      (goto-char (point-min))
      ;; uncomment and format first line
      ;; (remove initial ;'s and backslash quote the library name)
      (when (looking-at ";;; *\\(.*?\\) *--+ *\\(.*\\)")
        (replace-match " /\\1/ --- \\2"))
      ;; remove elisp comment chars (;'s)
      (org-readme-regexp-pairs [["^ *;; ?" ""]])
      ;; replace filename with orglink to filename
      (goto-char (point-min))
      (when (re-search-forward "[Ff]ile[Nn]ame: *\\(.*\\) *$" nil t)
	(replace-match "Filename: [[file:\\1][\\1]]"))
      ;; format other info lines into an org list
      (org-readme-regexp-pairs [["^\\(.*?\\):\\(.*?[A-Za-z0-9.].*\\)$" " - \\1 ::\\2"]])
      ;; add the header line
      (goto-char (point-min))
      (insert "* Library Information\n")
      ;; make new header for dependencies info
      (org-readme-regexp-pairs [["^[ \t]*Features that might be required by this library:[ \t]*$"
				 "* Possible Dependencies"]] t t)
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
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*;;; Change Log:[ \t]*$" nil t)
          (setq pt1 (point))
          (when (re-search-forward "^[ \t]*;;;;+[ \t]*$" nil t)
            (setq pt2 (match-beginning 0))
            (setq txt (buffer-substring-no-properties pt1 pt2))
            (with-temp-buffer
              (insert txt)
              ;; Take out comments
	      (org-readme-regexp-pairs [["^[ \t]*;+ ?" ""]])
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]*\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]*.*\n.*(\\([^)]*\\))[ \t]*\n\\(\\(?:\n\\|.\\)*?\\)\n[ \t]*\\([0-9][0-9]?\\)" nil t)
                (replace-match
                 (format " - %s :: %s (%s)\n %s"
                         (match-string 1)
                         (save-match-data
                           (replace-regexp-in-string
                            "~~~~" "\n    + "
                            (replace-regexp-in-string
                             "  +" " "
                             (replace-regexp-in-string
                              "\n" " "
                              (replace-regexp-in-string
                               "\n[ \t]*[*-+] +" "~~~~" (match-string 3))))))
                         (save-match-data
                           (replace-regexp-in-string
                            "[ \t]*$" ""
                            (match-string 2))) (match-string 4)) t t)
                (beginning-of-line))
              (when (re-search-forward "\\([0-9][0-9]?-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9]\\)[ \t]+\\(.*\\)\n.*\n\\(\\(?:\n\\|.\\)*\\)" nil t)
                (replace-match
                 (format " - %s :: %s (%s)"
                         (match-string 1)
                         (save-match-data
                           (replace-regexp-in-string
                            "~~~~" "\n    + "
                            (replace-regexp-in-string
                             "  +" " "
                             (replace-regexp-in-string
                              "\n" " "
                              (replace-regexp-in-string
                               "\n[ \t]*[*-+] +" "~~~~" (match-string 3))))))
                         (save-match-data
                           (replace-regexp-in-string
                            "[ \t]*$" ""
                            (match-string 2)))) t t))
	      (org-readme-regexp-pairs [["`\\(.*?\\)'" "=\\1="]
					["^[ \t][ \t]+[-]" " -"]])
              (goto-char (point-min))
              (insert "* History\n")
              (setq txt (buffer-substring-no-properties (point-min) (point-max))))
	    (with-temp-buffer
	      (insert-file-contents readme)
	      (org-readme-remove-section "History" txt)
	      (write-file readme))))))))

(provide 'org-readme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-readme.el ends here
