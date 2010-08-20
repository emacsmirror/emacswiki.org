[[ja:TwitteringMode-ja]]
== Description ==

twittering-mode.el is a Twitter client

[[image:TwitteringModeScreenshot]]

Home page: http://twmode.sourceforge.net/

Github page: http://github.com/hayamiz/twittering-mode/tree/master

##git clone git://github.com/hayamiz/twittering-mode.git##

== Installation Guide ==

=== For Windows users ===

* Uncompress the archive file, and add a path of a directory containing `twittering-mode.el` to your `load-path`.
: For example, if you uncompressed it under `%%C:\foo\%%`, there may be a directory `%%C:\foo\twittering-mode-X.X.X\%%` which contains `twittering-mode.el`. So add the following code to your `.emacs`
{{{
        (add-to-list 'load-path "C:/foo/twittering-mode-X.X.X")
}}}
* Proceed to **Set up your Emacs**

=== For Linux and other Unix users ===

* Uncompress the archive file, and add a path of a directory containing `twittering-mode.el` to your `load-path`.
: For example, if you uncompressed it under `/path/to/foo`, there may be a directory `/path/to/foo/twittering-mode-X.X.X/` which contains `twittering-mode.el`. So add the following code to your `.emacs`
{{{
        (add-to-list 'load-path "/path/to/foo/twittering-mode-X.X.X")
}}}
* Install cURL, gnutls, or openssl for using SSL connection.
* Proceed to **Set up your Emacs**

=== Set up your Emacs ===

* Add following code to your `.emacs`:
{{{
      (require 'twittering-mode)
}}}

=== Run twittering-mode ===

# Execute `M-x twit` to run twittering-mode.
# Open OAuth authorization page with your browser, click "Allow", and enter the PIN code.
# Your friends' timeline will appear. Enjoy!

== Key-bindings on View mode ==
`M-x twit', and you enter to this mode. You can see various timelines.

=== Move the cursor on the current timeline ===

; `j': Go to the next tweet. If the cursor placed on the oldest tweet, older tweets are retrieved. You can specify how many tweets will be retrieved by the variable `twittering-number-of-tweets-on-retrieval'.(`twittering-goto-next-status')
; `k': Go to the previous tweet. (`twittering-goto-previous-status')
; `n': Go to the next tweet whose author is the same as the current tweet. (`twittering-goto-next-status-of-user')
; `p': Go to the previous tweet whose author is the same as the current tweet. (`twittering-goto-previous-status-of-user')
; `l': Go to the next character. (`forward-char')
; `h': Go to the previous character. (`backward-char')
; `0': Go to the beginning of the current line. (`beginning-of-line')
; `^': Go to the beginning of the text on the current line. (`beginning-of-line-text')
; `$': Go to the end of the current line. (`end-of-line')
; `C-i': Go to the next username, URI, or timeline symbol. (`twittering-goto-next-thing')
; `M-C-i', `<backtab>': Go to the previous username, URI, or timeline symbol. (`twittering-goto-previous-thing')
; `<backspace>', `M-v': Scroll down. (`twittering-scroll-down')
; `<space>', `C-v': Scroll up. (`twittering-scroll-up')
; `H': Go to the beginning of the current buffer. (`twittering-goto-first-status')
; `G': Go to the end of the current buffer. (`end-of-buffer')

=== Apply some operation to the current timeline ===

; `g': Retrieve new tweets of the current timeline. (`twittering-current-timeline')
; `r': Display replied tweets related to the current tweet. (`twittering-toggle-show-replied-statuses')
; `C-c D': Delete the current tweet. You can delete only yours. (`twittering-delete-status')
; `C-c C-e': Remove all tweets of the current timeline from Emacs cache. (`twittering-erase-old-statuses')
; `q': Kill the current timeline buffer. (`twittering-kill-buffer')

=== Open other timeline ===

If you open a new timeline, a new buffer is generated.

; `v': Open a timeline pointed by the cursor. (`twittering-other-user-timeline')
; `V': Open a various timeline. You can specify the timeline by [[#timeline-spec]]. (`twittering-visit-timeline')
: For convenience, there are following short-cuts.
** `C-c C-f' Open your friends-timeline. (`twittering-friends-timeline')
** `C-c C-u' Open your user-timeline. (`twittering-user-timeline')
** `C-c C-r' Open your reply-timeline, which consists of replies to your tweets. (`twittering-replies-timeline')
** `C-c C-d' Open direct messages timeline, which consists of direct messages to you. (`twittering-direct-messages-timeline')
; `L': Open a timeline of a certain list. (`twittering-other-user-list-interactive')
; `C-c C-q': Search. (`twittering-search')
; `f': Switch to the next timeline buffer. (`twittering-switch-to-next-timeline')
; `b': Switch to the previous timeline buffer. (`twittering-switch-to-previous-timeline')

=== Post a tweet ===

; `u', `C-c C-s': Post a tweet. (`twittering-update-status-interactive')
; `C-m', `Enter': Post a reply to the tweet pointed by the cursor. If the cursor points a URI, invoke `browse-url' for the URI. (`twittering-enter')
; `C-c C-m', `C-c Enter': Post a non-official (organic) retweet for the tweet pointed by the cursor. (`twittering-retweet')
; `C-u C-c C-m', `C-u C-c Enter': Post a official (native) retweet for the tweet pointed by the cursor. (`twittering-retweet')
; `d': Send a direct message. (`twittering-direct-message')

=== Invoke an external browser ===

; `<mouse-1>': Open user page or URI by `browse-url'. (`twittering-click')
; `C-c C-v': Open the pointed user's page by `browse-url'. (`twittering-view-user-page')

=== Change display mode ===

; `a': Toggle automatic retrieval of the current timeline. (`twittering-toggle-activate-buffer')
; `i': Toggle displaying icons of the current timeline. (`twittering-icon-mode')
; `s': Toggle scroll mode for the current timeline. (`twittering-scroll-mode')
; `t', `C-c C-p': Toggle proxy. (`twittering-toggle-proxy')

=== Others ===

; `C-c C-t': Set the current hashtag. (`twittering-set-current-hashtag')
; `C-c C-l': Post the message "λかわいいよλ"("Lambda is cute, lambda."). (`twittering-update-lambda')

== Key-bindings on Edit mode ==

; `M-p': Replace a tweet being edited with the previous tweet on history. (`twittering-edit-previous-history')
; `M-n': Replace a tweet being edited with the next tweet on history. (`twittering-edit-next-history')
; `<f4>': Shorten the URL pointed by the cursor. (`twittering-edit-replace-at-point')
; `C-c C-k': Cancel editing a tweet. (`twittering-edit-cancel-status')
; `C-c C-c': Post the current tweet. (`twittering-edit-post-status')

== Timeline Spec ==
[:timeline-spec]
under construction...

== Customizing your key bindings ==

You can use `twittering-mode-hook' to customize your configuration. This example shows how to define a few key bindings:

 (add-hook 'twittering-mode-hook
           (lambda ()
             (mapc (lambda (pair)
                     (let ((key (car pair))
                           (func (cdr pair)))
                       (define-key twittering-mode-map
                         (read-kbd-macro key) func)))
                   '(("F" . twittering-friends-timeline)
                     ("R" . twittering-replies-timeline)
                     ("U" . twittering-user-timeline)
                     ("W" . twittering-update-status-interactive)))))

== Other useful options ==

 (setq twittering-icon-mode t)                ; Show icons
 (setq twittering-timer-interval 300)         ; Update your timeline each 300 seconds (5 minutes)

twittering-mode also provides hooks that are run when new tweets arrive. This example calls notify-send (included in the libnotify package) to display a message when new tweets arrive:

 (add-hook 'twittering-new-tweets-hook (lambda ()
   (let ((n twittering-new-tweets-count))
     (start-process "twittering-notify" nil "notify-send"
                    "-i" "/usr/share/pixmaps/gnome-emacs.png"
                    "New tweets"
                    (format "You have %d new tweet%s"
                            n (if (> n 1) "s" ""))))))

== Discussion ==
[new:halloleo:2010-01-20 01:00 UTC]

[[halloleo]]: i'm trying this mode and it works pretty good, but i'm not sure how to completely /finish/ using twitter. does 'q' (`twittering-suspend') do this? i don't think so: it only switches to another buffer, therefore the twitter timer still retrieves tweets...

[new]
RichardRiley : What packages are needed for icon support? Also, wouldnt


      (define-key km "q" 'delete-window)


be a good addition?

[new]
For icon support, try adding ##(twittering-icon-mode)## to your ##.emacs## -- AlbertoGarcia

[new]
You need wget for icon support on Linux. it can take a while to download icons so be patient - RichardRiley.

[new]
That's a bug IMHO. The problem is that icons are downloaded while the timeline is being displayed on screen, so the first time a new user appears in your timeline, you won't see their icon because it's still being downloaded. You have to wait till the next refresh (or press "g" to force it) -- AlbertoGarcia.

[new]
I'm just wondering if I have missed something obvious. The default is to see your friends tweets. But how do I tweet to my tweet account from there? Currently I have to  go to my timeline (C-c C-u)) and then hitting return : is there a quicker way? -- RichardRiley.

[new]
You can use the somewhat non-obvious keybinding of C-c C-s (twittering-update-status-interactive).

[new]
Icon support does not work in windows.  I have gnu wget installed and in path, and the icons are downloading to twittering-tmp-dir.  However, they do not display in icon mode. --Tim Hoolihan

[new]
Icon Works on Windows needs GNU Win32 image packages (http://gnuwin32.sourceforge.net/packages.html) installed, which includes jpeg, libungif, libpng,  xpm, and zlib, etc. And the packages dir must be set into the %PATH% environment various. 

BTW: I wish this code would support HTTPS and third-party API in the future. It would be very useful. --Moses

[new]
I wish twittering-mode would support socket proxy. I only use tor to connect twitter. --rix

[new]
I wrote some patches so that icon-mode works well without external programs, wget and convert.
You can get it from my repository on github http://github.com/cvmat/twittering-mode/tree/without-external-program . I confirmed it with NTEmacs on Windows XP and Emacs on Debian. -- Tadashi MATSUO

[new]
Sorry for my rudeness. At last, I found tor+privoxy can make a http proxy. privoxy's default proxy port is 8118. --rix

[new]
hayamiz has merged my patches into the main repository. The main repository http://github.com/hayamiz/twittering-mode/tree/master now supports icon-mode without external programs. --Tadashi MATSUO

[new]
Any chance we can have a count of characters while tweeting in the minibuffer? (twit.el has this, I should look at how it's done) 

[new]
If you use 0.9.0, the count is displayed in default. If you use the latest version on github, you may need the following statement in your .emacs;
  (setq twittering-use-show-minibuffer-length t)
But the option is t in default... --Tadashi MATSUO

[new]
I can't see my followee's retweets. I'm following @ebertchicago(Roger Ebert, a film critik), but can't see his massive retweets. Can anybody tell me how can I fix it? -Jeongtae Roh

[new]
Do you use 0.9.0? In default, it retrieves friends timeline, which does not include native retweets unless the special option is embedded in the request (described at http://dev.twitter.com/doc/get/statuses/friends_timeline ). I recommend you to use the latest version on github. It supports home timeline, which includes native retweets. --Tadashi MATSUO

[new]
 Suddenly I get this error message from my twitteringmode client. Strange thing is it was working fine before. Any clues?

error in process sentinel: Search failed: “^M? ^M?

Thanks in advance.

[new]
The version 0.9.0 seems to have some problems on dealing of newlines. But your problem, which is also reported by other people, may be triggered by recent modification on twitter.com because this problem seems to appear recently. The latest version on github will solve the problem. --Tadashi MATSUO

[new]
How do I avoid having to authorize the app once and again when using OAuth? -- AlbertoGarcia.

[new]
With the HEAD version on github, you can store the authorized token with encryption by GnuPG.
If you have an encrypted token, you no longer need to retrieve PIN code via web browser.
Instead, you need to input a pass phrase for decryption. You can enable this function by the following code.
{{{
    (setq twittering-use-master-password t)
}}}
It requires GnuPG and either EasyPG or 'alpaca.el'. Please refer to [http://github.com/hayamiz/twittering-mode/blob/master/NEWS NEWS file on the HEAD] -- Tadashi MATSUO

[new]
I have just installed 1.0 after removing 0.8.  When I use M-x twit to enter tw-mode, I just get "Authorization via OAuth failed. Type M-x twit to retry." in the minibuffer, and an empty :home buffer.  This is unhelpful.  What can I do to actually authorize? I can't have emacs launch a browser: my emacs session lives on a distant machine and I access it strictly in text, as a dumb tty. -- Krinn
:[new]
It is because Twitter has changed the CA certificate required for server authentication of api.twitter.com and search.twitter.com. The CA certificate embedded in 1.0.0 fails. The HEAD and the most recent revision on 1.0.x branch on github have new CA certificate, which can authenticate the servers. -- Tadashi MATSUO

[new]
Fiddled around with the hook and wrote one I think is more useful than the suggested. I also used ToDoChiKu so it should work with growl, snarl, and libnotify.

{{{
(add-hook 'twittering-new-tweets-hook (lambda ()
  (let ((n twittering-new-tweets-count))
    (if (> n 10)
        (todochiku-message
          (twittering-timeline-spec-to-string twittering-new-tweets-spec)
          (format "You have %d new tweet%s"
            n (if (> n 1) "s" ""))
          (todochiku-icon 'social))
      (dolist (el twittering-new-tweets-statuses)
	(todochiku-message 
          (twittering-timeline-spec-to-string twittering-new-tweets-spec)
	  (concat (cdr (assoc 'user-screen-name el))
	     " said: " 
	     (cdr (assoc 'text el)))                                   
          (todochiku-icon 'social)))))))
}}}

When more than 10 tweets are retrieved, it makes a message with the spec as the title and the number of tweets in the message.
If it gets less than 10, a message is made of each. Hope someone else finds it useful. -- DaveKerschner
----
See also: [[Twitter]] [[Identica-mode]]
