'''w3m''' is a standalone textual browser much like lynx.  It runs in a terminal. [[emacs-w3m]] is an Emacs interface to w3m, so that one can browse web sites within emacs.

<b>Note:</b> there's some confusion about w3 and w3m.  [[w3]] is also a web browser, completely written in elisp; it runs within Emacs, and is bundled with Xemacs. 

w3m was developed by Akinori ITO.  You can
get it here:

* http://w3m.sourceforge.net/index.en.html (English)
* http://w3m.sourceforge.net/index.ja.html (Japanese)

The rest of this page is dedicated to
setting up w3m for Emacs.  

== Using Emacs as the external editor for w3m ==

Go to the w3m options and and find the options Editor and Mailer.  Use
the following:

* Editor: gnuclient
* Mailer: gnudoit '(compose-mail "%s")' &

This requires GnuClient.  If you only have EmacsClient, you cannot use
the Mailer.  For the Editor, use this:

* Editor: emacsclient

== Editing files in Emacs ==

Use SimpleWikiMode to edit w3m textarea files.  This even works in the
console -- start Emacs in one console, and w3m in another.  When you
edit a textarea, switch to Emacs, when done, switch back to w3m.

Once you kill the buffer, you are living dangerously -- all your stuff
is deleted on the disk, it only exists in w3m.  And so if you hit the
back button in w3m, or quit w3m, there is not even a backup file for
you.

Here is how to force it:

    (add-hook 'kill-buffer-hook 'make-w3m-backup-copy)
    
    (defun make-w3m-backup-copy ()
      (when (string-match "^w3m" (buffer-name))
        (write-file (concat (buffer-file-name) ".last"))))

== Running w3m ==

I like my w3m to look just like my ColorTheme, and I want to save
myself from typing the -B all the time.  I also want to be able to
switch to UTF-8 at will:

This is my ~/bin/web-browser:

    #! /bin/bash
    GEOM="-geometry 155x55"
    if [ -z "$DISPLAY" ]; then
        PROG=w3m
        if [ "u" == "$1" ]; then
            URL=$2
        else
            URL=$1
        fi
    elif [ "u" == "$1" ]; then
        PROG="xterm-utf8 -name web-browser $GEOM -bg #243 -fg NavajoWhite -e w3m-m17n"
        URL=$2
    else
        PROG="xterm -name web-browser $GEOM -bg #342 -fg NavajoWhite -e w3m"
        URL=$1
    fi
    
    if [ -z "$URL" ]; then
        URL=-B
    fi
    
    $PROG $URL

And this is my ~/bin/xterm-utf8:

    #!/bin/sh
    exec uxterm \
        -bg '#003' \
        -fn '*-unifont-*--16-*-iso10646-1' \
        $*

Running <tt>web-browser u [URL]</tt> will then visit URL using UTF8.

== Editing Emacs Wiki ==

To edit this wiki using w3m make sure you enable cookies in w3m! Hit ##o## for options and check for the following:

{{{
Cookie Settings

Enable cookie processing                           (*)YES  ( )NO
Print a message when receiving a cookie            (*)YES  ( )NO
Accept cookies                                     (*)YES  ( )NO
}}}

Next, [http://www.emacswiki.org/emacs/uihnscuskc/SiteMap follow this link] to set the cookie.

Use `C-k' to examine your cookie jar and verify that you've got the appropriate cookie set.

