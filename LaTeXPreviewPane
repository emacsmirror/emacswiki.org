= LaTeX Preview Pane =

latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. 
It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
 
The latest version of latex-preview-pane can always be found at: https://github.com/jsinglet/latex-preview-pane

The following document details the installation and usage of latex-preview-pane. Below you can see a screenshot of LaTeX Preview Pane in action.

[[image:LaTeXPreviewPaneSS]]


Below, you can see LaTeX Preview Pane highlighting an error in a buffer.

[[image:LaTeXPreviewPaneError]]

== Installing LaTeX Preview Pane == 

To install latex-preview-pane you will need the following:

* A installed copy of Emacs 24+ with a working packages system
* The command pdflatex on your PATH
* Access to the Marmalade package repository

If you don't already have Marmalade installed, add the following snippet to your .emacs file and either restart emacs or simply <code>eval-region</code> the snippet. 

{{{
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
}}}

To install latex-preview-pane, simply execute <code>M-x package-install latex-preview-pane</code> and restart Emacs.

== Using LaTeX Preview Pane ==

To use LaTeX Preview Pane, simply open any TeX file and LaTeX Preview Pane will open and attempt to generate your TeX preview. Note that there is also a menu in this mode which contains the following functions:
* Refresh Preview (bound to M-p)
* Open in External Program (Bound to M-P)
* Disable LaTeX Preview Pane (turns the mode off, you can also use <code> M-x latex-preview-pane</code> to toggle it off.
* Customize LaTeX Preview Pane (opens a customization buffer where you can set the command to use for generating previews)

== Questions, Feedback ==

Please direct all feedback to me over on this project's GitHub page, here: https://github.com/jsinglet/latex-preview-pane 
