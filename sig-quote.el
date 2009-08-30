;;; sig-quote.el --  Add quotes to signature according to recipients (Gnus/RMAIL/MH-E/vm)
;;
;; This file is *NOT* part of GNU emacs
;;
;; Copyright (C) 1995, 1996, 1997, 2002 Trey Jackson
;; Author:       Trey Jackson <trey@cs.berkeley.edu>
;; Maintainer:   Trey Jackson <bigfaceworm@hotmail.com>
;; Created:      Oct 1 1995

;; LCD Archive Entry:
;; sig-quote.el|Trey Jackson|bigfaceworm@hotmail.com|
;; Add quotes to signature according to recipients (RMAIL/MH-E/Gnus)|
;; 09-Jan-2002|Version 1.42||

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;; Installation:

;; Put this file on your emacs-lisp load path, byte-compile it for efficiency.
;;
;; For a minimal installation, put the following in your ~/.emacs startup file:
;;
;; (require 'sig-quote)
;; (sig-quote-mode 'rmail)   ;; 'vm or 'mh-e -- depending on which system used
;;
;; Read "Usage documentation" to learn how to use/set up files for this mode.

;;{{{ Usage documentation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General description of sig-quote-mode:
;;
;;
;; This mode is designed to allow a user to have quotes appended to their mail
;; and news posts according to whom/where the mail is being sent.  For
;; example, one might wish to have "insightful" quotes sent to Prof. Apple,
;; and "crude" quotes to your brother Sam.  With this mode, upon sending a
;; piece of mail, the email addresses are scanned (this includes the To:, CC:,
;; and BCC: headers), and an appropriate quote is inserted into the letter.
;;
;;
;; Quotes are stored in the file specified by 'sq-quote-file.  Within this
;; file, quotes are grouped together into types.  Each quote in a type is
;; separated by a string, specified by the regular expression in
;; 'sq-quote-delimeter.  Let's illustrate this by use of an example:
;;
;; The following is an excerpt from a typical quote file:
;; (ignore the `;; ' preceeding each line)
;;
;;
;; BEGIN QUOTE-TYPE: witty
;; All that is glitters is not gold.
;; -----------------------------------------quote
;; A bird in the hand is worth two in the bush.
;; -----------------------------------------quote
;; This statement is false.
;; END QUOTE-TYPE
;;
;; BEGIN QUOTE-TYPE: not so witty
;; "If you melt dry ice, can you swim in it without getting wet?"
;; -- S.R., Newark, NJ
;; -----------------------------------------quote
;; "Do fish ever get thirsty?"
;; -- B.G., Albuquerque, NY
;; -----------------------------------------quote
;; "Does water sink or float?"
;; -- L.T., Tinley Park, Ill.
;; END QUOTE-TYPE
;;
;;
;; If the above were my .signature.quotes file, I would have six quotes, three
;; of the type "witty" and three of the type "not so witty".  Every quote must
;; be inside 'sq-quote-type-begin and 'sq-quote-type-end.  The quotes can be
;; as long as you like, and will be inserted exactly as they are found in the
;; file.  Any number of quotes can be grouped together in the same type group.
;; Everything between the groups of quotes is ignored, thus you can have as
;; many newlines separating them as you wish.  As you can see, quote types are
;; just a string, but cannot contain newlines.
;;
;;
;; Now, to specify that I want to send "not so witty" quotes to my brother,
;; whose email is: "sam@aol.com", and "witty" quotes to the professor, whose
;; email is: "prof_apple@podunk.edu", I would define
;; sq-address-n-quote-type-list as follows:
;;
;; (setq sq-address-n-quote-type-alist
;;       '(("prof_apple@podunk\.edu" . ("witty"))
;;         ("sam@aol\.com"           . ("not so witty"))
;;         ("bob"                    . ("crude" "silly"))
;;         ("jill"                   . t)
;;         ("@cs\.berkeley\.edu"     . nil))))
;;
;; This also specifies that both "crude" and "silly" quotes go to anyone with
;; "bob" in their email address, all quotes go to people with "jill" in their
;; email address, and no jokes go to people who have "@cs.berkeley.edu" as a
;; part of their address.
;;
;; Note: The "\." is needed instead of simply "." because the strings that
;; match email addresses are really regexps, and `.' has special meaning in a
;; regexp.
;;
;; Note: The matching is done from the beginning of the list to the end.
;; Thus, if I were sending a piece of email to "bob@cs.berkeley.edu", he would
;; still get either a "crude" or "silly" joke, because a match was found
;; before getting to the entry ("@cs\.berkeley\.edu" . nil).
;;
;; Note: If you want to distinguish posts to newsgroups vs. email, use the
;; following regexp:
;;            "^\\([^\\.]*\\|[^@]*@[^@]*\\)$"
;; It matches strings w/out `.' or with a single `@'. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User specified options:
;;
;; Variable: sq-quote-delimeter
;;    This regexp specifies what string separates one quote from another.
;;    The default "^-+quote" means that a line beginning with at least one `-'
;;    (possibly many) follwed by the string "quote".  This will match both:
;;    ----quote
;;    -----------------quote
;;    You can change this variable to match what you want.
;;
;; Variable: sq-quote-type-begin
;;    If you don't like having groups of quotes begin with "BEGIN QUOTE-TYPE: "
;;    you can change this variable to have the regexp you wish to specify the
;;    beginning of a group of quotes.
;;
;; Variable: sq-quote-type-end
;;    If you don't like having groups of quotes end with "END QUOTE-TYPE"
;;    you can change this variable to have the regexp you wish to specify the
;;    end of a group of quotes.
;;
;; Variable: sq-quote-type-default
;;    This variable contains a list of quote types from which to choose if an
;;    email address does not match any in sq-address-n-quote-type-alist.
;;    The default is nil.
;;
;; Variable: sq-quote-credit-regex
;;    Regexp found before the credit for a quote type.
;;    This regexp must be located within the quote delimeters for a specific
;;    quote type. Everything between the regexp and the end of the line will
;;    be taken as the credit.
;;
;;    For example, if you modified the witty type to have the following form:
;;
;;      BEGIN QUOTE-TYPE: witty
;;      CREDIT: -- This is a way deep saying.
;;      All that is glitters is not gold.
;;      -----------------------------------------quote
;;      A bird in the hand is worth two in the bush.
;;      -----------------------------------------quote
;;      This statement is false.
;;      END QUOTE-TYPE
;;
;;    All witty quotes would be associated with "-- this is a way deep saying."
;;    The following would be inserted in a letter to Prof. Apple:
;;
;;        A bird in the hand is worth two in the bush.
;;        -- This is a way deep saying.
;;
;;    Any "witty" quote would be followed by "-- This is a way deep saying."
;;
;; Variable: sq-confirm-quote
;;    If this is true, sig-quote-mode will prompt you to see if you like the
;;    quote that has been chosen.  An answer of `y' will result in the
;;    inserting the quote into the letter, an answer of `n' will give you
;;    another quote to confirm.
;;
;; Variable: sq-quote-file
;;    This variable contains the filename of the file which contains the
;;    quotes to be used.
;;
;; Variable: sq-mail-system
;;    This variable specifies which mail system you wish to use.
;;    sig-quote mode will default to 'rmail, but 'mh-e is also supported.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface:
;;
;; In mail-mode you can use the following key-bindings:
;;
;; C-c n   sig-quote-mode
;;    This toggles quoting on and off.
;;
;; C-c c   sq-toggle-confirm-quote
;;    This toggles explicit confirmation of each quote
;;
;; C-c t   sq-quote-types
;;    This lists the types of quotes available for the people
;;    the mail is addressed to
;;
;; C-c r   sq-remove-quote
;;    Remove the last quote inserted.
;;
;; C-c C-n sq-add-quote
;;    This forces the addition of a quote to the current letter.
;;    Repeated use of this function will result in replacing the
;;    old quotes with the newly chosen quote.
;;
;; C-c a   sq-add-quote-no-addresses
;;    Like sq-add-quote, but ignores email addresses and asks user
;;    for a quote type to use.
;;
;; C-c s   sq-send-without-quote
;;      Send message using `mail-send-and-exit' but without adding a quote.
;;
;; C-c S   sq-add-quote-search
;;    Like sq-add-quote, but chooses from quotes whose text
;;    contains a user inputted string.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quirks/Features:
;;
;; o Clobbers C-c C-n in message-mode-map, tough, fix it if you wish.
;;
;; o Mail aliases are expanded before an appropriate quote is chosen.
;;
;; o If mail-signature is nil, quotes are appended to the letter.
;;
;; o If you append a file after your signature, the quote will be
;;   added after the signature, but before the file. (spiffy, eh?)
;;
;; o The mode-line will have "Sig-Quote" as a minor mode if it is on.
;;
;; o Signature quote file may be folded - as long as each group of quotes is
;;   entirely within a fold.
;;
;; o The quote credits can now be specified from within the quote file.
;;
;; o If the headers do not contain email addresses, or
;;   the addresses have no common quote types, the user is queried for
;;   a quote type from which to choose.
;;
;; o A quote may be added ignoring email addresses.  (C-c a)
;;
;; o The user may choose quotes by searching for a specified string. (C-c S)
;;
;; o You may remove a quote which has been added with a simple key sequence.
;;   (C-c r)
;;
;; o Quotes are read from the file the first time a quote is added, and
;;   when the quote file has been modified.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Thanks to the following for testing my code and providing feedback:
;;   Sam Jackson, Matt Podolsky, Jeff Forbes, Ralph Finch, and Jim Zatorski.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; For more information, see the documentation for the individual function or
;; variable.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Has worked with:
;; Emacs 19.28+, XEmacs 19.13, RMAIL, MH-E, VM 5.95b, Gnus 5.3
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To do:
;;
;; o add documentation about sq-credit-after-quote, credit-begin/end,
;;   sq-header-list
;; o add option which warns you of new person who isn't in the alist (BBDB)
;; o check documentation, now that multiple mail buffers supported
;; o update docs to reflect mh-e, vm, and gnus support
;; o figure out if functions in hooks can't be debugged.
;;   sq-quote-setup y-or-n-p
;; o allow user to specify keymap by using ((KEY FCN) ...) form
;; o write better documentation in sig-quote-mode function b/c that's
;;   used in the C-hm display
;;

;;}}}

;;{{{ Code

(and (locate-library "rnewspost")
     (load-library "rnewspost"))   ;; no provide provided

;;{{{ Variables

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sig-quote external variables

(defvar sig-quote-mode nil
  "Buffer local: t if `sig-quote' mode is on, nil if off.
Use the function 'sig-quote-mode' to toggle sig-quote-mode.")
(make-variable-buffer-local 'sig-quote-mode)

(defvar sq-mail-system nil
  "Determines which mail system to configure `sig-quote' for.
sig quote mode supports RMAIL and MH-E, default is RMAIL.

Set `sq-mail-system' to either 'rmail or 'mh-e to indicate which system.
If not set, it will default to 'rmail.")

(defvar sq-confirm-quote t
  "*t if confirmation is required to insert random quote.")

(defvar sq-gnus-too nil
  "If `sq-gnus-too' is non-nil, sig quote mode is enabled for news posts.
Default to nil so as not to break on systems w/out Gnus 5.3.")

(defvar sq-quote-file "~/.signature.quotes"
  "*File containing quotes to be added to signature.")

(defvar sq-header-list '("Newsgroups" "To" "CC" "BCC")
  "*List of headers sig-quote uses to determine appropriate quote.
Must be properly capitalized.
If nil, no headers are used, if t, all headers are used.")

(defvar sq-warn-if-no-quote t
  "*Non nil means to warn the user if addresses result in no quote.")

(defvar sq-quote-type-resolution-function 'sq-intersect-type-lists
  "Name of the function used to resolve differences between quote types.
The function must take any number of type lists, and return a single type list.
The default, `sq-intersect-type-lists', finds the set intersection.
The other built-in function is `sq-unify-type-lists', which uses the set union.")

(defvar sq-quote-delimeter "^-+quote"
  "Regexp of string to be found separating quotes in the file `sq-quote-file'.
The default is \"^-+quote\" - which amounts to a line with a bunch of `-'
followed by `quote' e.g.
-----------------------quote")

(defvar sq-quote-type-begin "^BEGIN QUOTE-TYPE: "
  "Regexp denoting the beginning of a quote type group.
Note: the type for the following group of quotes is the
string directly following this regexp.")

(defvar sq-quote-type-end "^END QUOTE-TYPE"
  "Regexp denoting the end of a quote type group.")

(defvar sq-credit-after-quote t
  "Non-nil value specifies to put the credit after the quote.")

(defvar sq-quote-credit-regex "^CREDIT: "
  "Regexp found before the credit for a quote type.
This regexp must be located within the quote delimeters for a specific
quote type. Everything between the regexp and the end of the line will
be taken as the credit.

For example, if you had a number of quotes from the TV show Jeopardy,
you could add the following line to the Jeopardy type:

CREDIT: -- From the best game show on Earth, Jeopardy.

All Jeopardy quotes added to your outgoing mail/news would be
appended with \"-- From the best game show on Earth, Jeopardy.\"")

(defvar sq-quote-credit-begin "^CREDIT BEGIN"
  "Regexp denoting beginning of credit for a quote type.
This is used with `sq-quote-credit-end' for multi-line credits.
See `sq-quote-credit-regex' for an explanation of credits.")

(defvar sq-quote-credit-end "^CREDIT END"
  "Regexp denoting end of credit for a quote type.
This is used with `sq-quote-credit-begin' for multi-line credits.
See `sq-quote-credit-regex' for an explanation of credits.")

(defvar sq-quote-credit-alist nil
  "An association list with elements of the form (QUOTE-TYPE . CREDIT).
The function `sq-quote-credit' uses this alist to append an appropriate credit.

NOTE: This structure is now automatically formed when loading the quote file.
see documentation for the variable `sq-quote-credit-regex' for details.")

(defvar sq-quote-type-default nil
  "*Default type of quote to be inserted after signature.

This corresponds to the QUOTE-TYPE-LIST that will be used if a given email
address does not match any of the ADDRESS-REGEXPs in
sq-address-n-quote-type-alist.")

(defvar sq-newline-before-quote t
  "*Non-nil value specifies to insert a newline between signature and quote.")

(defvar sq-address-n-quote-type-alist nil
  "An alist that associates quote types and addressies.
It has entries of the form (ADDRESS-REGEXP . QUOTE-TYPE-LIST)
where ADDRESS-REGEXP is the regexp searched for in the addresses
of the mail.  The QUOTE-TYPE-LIST is a list of quote types to be
used when sending mail to the ADDRESS-REGEXP.

If QUOTE-TYPE-LIST is t, all types are used.
If QUOTE-TYPE-LIST is nil, no quotes are used.
If QUOTE-TYPE-LIST is of the form (not TYPE1 TYPE2 ... TYPEn),
then all quote types except TYPE1, TYPE2, ..., TYPEn will be used.

For example, the following binding for `sq-address-n-quote-type-alist' would
have the effect of sending:
    o \"witty\" and \"silly\" quotes to bob
    o \"serious\" quotes to anyone at @whitehouse.gov
    o all types of quotes except \"silly\" and \"zany\" to people at
      @cs.berkeley.edu
    o no quotes to jill
    o and all types of quotes to sam@podunk.edu

 (setq sq-address-n-quote-type-alist
       '((\"bob\"                 . (\"witty\" \"silly\"))
         (\"@whiteouse\\.gov\"     . (\"serious\"))
         (\"@cs\\.berkeley\\.edu\"  . (not \"silly\" \"zany\"))
         (\"jill\"                . nil)
         (\"sam@podunk\\.edu\"     . t)))")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sig-quote internal variables

                                        ; sq-quote-mode
(defvar sq-quote-mode nil
  "Internal variable storing state of sig quote.")

                                        ; sq-last-quote
(defvar sq-last-quote ""
  "Text of last quote inserted. (buffer local)")
(make-variable-buffer-local 'sq-last-quote)

                                        ; sq-list-of-quotes
(defvar sq-list-of-quotes nil
  "Do not modify this, use the function `sq-list-of-quotes'")

                                        ; sq-list-of-quote-types
(defvar sq-list-of-quote-types nil
  "Do not modify this, use the function `sq-list-of-quote-types'")

                                        ; sq-quote-file-timestamp
(defvar sq-quote-file-timestamp nil
  "Copy of last modified timestamp for the file specified by 'sq-quote-file.")

;                                        ; sq-valid-buffer-names
;(defvar sq-valid-buffer-names (list "*mail*" "draft" "*post-news*" "*VM-mail*")
;  "List of buffer names sig-quote mode will take effect in.")


                                        ; sq-key-bindings
(defvar sq-key-bindings
  (`((, t)
     ("\C-c\C-n" sq-add-quote)
     ("\C-ca"    sq-add-quote-no-addresses)
     ("\C-cs"    sq-send-without-quote)
     ("\C-cS"    sq-add-quote-search)
     ("\C-cr"    sq-remove-quote)
     ("\C-ct"    sq-quote-types)
     ("\C-cc"    sq-toggle-confirm-quote)
     ("\C-cn"    sig-quote-mode)))
  "Default key bindings for sig-quote-mode.
Default key bindings for sig-quote-mode are (in mail-mode only):

C-c C-n sq-add-quote
          (add quote to *mail* - replacing old one if it exists)

C-c a   sq-add-quote-no-addresses
          (add quote to *mail* ignoring email addresses)

C-c s   sq-send-without-quote
          Send message using `mail-send-and-exit' but without adding a quote.

C-c S   sq-add-quote-search
          (add quote to *mail* whose text contains specified string)

C-c r   sq-remove-quote
          (remove last quote inserted from *mail* buffer)

C-c t   sq-quote-types
          (list types of quotes for addresses of this mail)

C-c c   sq-toggle-confirm-quote
          (toggle quote confirmation)

C-c n   sig-quote-mode
          (toggle quoting mode)")

(defvar sq-news-key-bindings (copy-sequence sq-key-bindings)
  "See `sq-key-bindings' for documentation.")


(defvar sq-mail-mode-map nil
  "mode-map to add the keybindings")
(defvar sq-mail-mode-hook nil
  "hook to add the `sq-quote-setup' function")
(defvar sq-mail-send-hook nil
  "hook called before sending a letter/post")
(defvar sq-quote-added-manually nil
  "buffer local variable indicating whether or not a quote has been added")
(make-variable-buffer-local 'sq-quote-added-manually)
(defvar sq-signature nil
  "stores user's signature")

;;}}}

;;{{{ Functions

;;{{{ User interface functions and setup

;;;###autoload
(defun sig-quote-mode (&optional val no-keys)
  "Enable or disable adding email address specific quotes to signature.
If VAL is positive, enable.  If VAL negative, disable.  If omitted, toggle.
If NO-KEYS is non-nil, do not add default keybindings.

Initially, `sig-quote-mode' should be called with the type of mail system used.
e.g.
  (sig-quote-mode 'rmail)   ;; this is the default if no system is provided
  (sig-quote-mode 'mh-e)
  (sig-quote-mode 'vm)

See documentation for the variable `sq-key-bindings' for default key bindings.
When enabled, keybindings are in place, when disabled, all but C-cn binding
removed."
  (interactive)
  (setq sq-quote-mode (or (sq-mail-system val)
                          (and (eq val nil) (not sq-quote-mode))
                          (and val (> val 0))))
  (if (sq-valid-buffer-p)
      (setq sig-quote-mode sq-quote-mode))


  ;; now just install keys once, never uninstall
  (and (not no-keys)
       (car sq-key-bindings)
       (sq-switch-key-bindings))

  (if sq-quote-mode
      (progn
        (add-hook sq-mail-mode-hook 'sq-quote-setup)
        (add-hook sq-mail-send-hook 'sq-add-quote)

        ;; Gnus relevant
        (and sq-gnus-too
             (add-hook 'message-signature-setup-hook 'sq-quote-setup)
             (add-hook 'message-send-hook 'sq-add-quote))

        (and (sq-valid-buffer-p)
             (sq-quote-setup))

        (message "Random signature quotes enabled."))

    (progn
      (remove-hook sq-mail-mode-hook 'sq-quote-setup)
      (remove-hook sq-mail-send-hook 'sq-add-quote)
      (remove-hook 'message-send-hook 'sq-add-quote)
      (remove-hook 'message-signature-setup-hook 'sq-quote-setup))

    (message "Random signature quotes disabled."))

  (force-mode-line-update)
  sq-quote-mode)

(defun sq-quote-setup ()
  "Initialize variables for a new mail/news editing session."
  (setq sig-quote-mode sq-quote-mode)

  (setq sq-quote-added-manually nil)
  (setq sq-signature nil)
  (setq sq-last-quote "")
  (save-window-excursion
    ;; in gnus     next chunk of code taken from message-insert-signature, in message.el
    (cond ((and (eq major-mode 'message-mode)
                sq-gnus-too
                (setq sq-signature
                      (let* ((signature 
                              (cond ((functionp message-signature)
                                     (funcall message-signature))
                                    ((listp message-signature)
                                     (eval message-signature))
                                    (t message-signature)))
                             (signature
                              (cond ((stringp signature)
                                     signature)
                                    ((and (eq t signature)
                                          message-signature-file
                                          (file-exists-p message-signature-file))
                                     signature))))

                        (when signature
                          (concat "-- \n"
                                  (if (eq signature t)
                                      (let (sig
                                            (buf (find-file message-signature-file)))
                                        (setq sig (buffer-string))
                                        (kill-buffer buf)
                                        sig)
                                    signature)))))))
                      
          ;; in mail
          ((and (not sq-signature))
            (setq sq-signature
                  (cond ((eq mail-signature t)
                         (file-exists-p "~/.signature")
                         (let (sig
                               (buf (find-file "~/.signature")))
                           (setq sig (concat "-- \n" (buffer-string)))
                           (kill-buffer buf)
                           sig))
                        (mail-signature)))))))


(defun sq-assert-mode-on ()
  "Report an error if `sig-quote-mode' is nil."
  (if (not sig-quote-mode)
      (if (y-or-n-p "Sig-Quote mode is NOT on! - Continue anyway? ")
          (progn (message "Sig-Quote mode is still disabled.")
                 (sit-for 1.5))
        (error "Sig-Quote mode is NOT on!"))))

(defun sq-send-without-quote (&optional prefix)
  "Send message and exit buffer without adding a quote."
  (interactive"P")
  (sq-assert-mode-on)
  (if sq-quote-added-manually
      (sq-remove-quote))
  (setq sq-quote-added-manually t)
  (sq-send-and-exit prefix))

(defun sq-add-quote (&optional ignore-addresses)
  "Add a quote after the signature according to the email addresses.
Non-nil optional argument causes email addresses to be ignored,
and the user is prompted for a type of quote."
  (interactive)
  (sq-assert-mode-on)
  (if (or (interactive-p)
          ignore-addresses
          (not sq-quote-added-manually))
      (let ((quote (sq-select-appropriate-quote ignore-addresses)))
        (if quote
            (progn (setq sq-quote-added-manually t)
                   (sq-insert-signature-quote quote))))))

(defun sq-add-quote-no-addresses ()
  "Add a quote after the signature ignoring email addresses.
User is prompted for a type of quote."
  (interactive)
  (sq-add-quote 'ignore))

(defun sq-add-quote-search ()
  "Add a quote containing user-specified text.
The quote is chosen from quotes whose text contains a user-specified string."
  (interactive)
  (sq-add-quote 'search))

(defun sq-remove-quote (&optional new-quote)
  "Remove the quote added after the signature.
Optional argument NEW-QUOTE specifies what replaces the old quote."
  (interactive)
  (sq-assert-mode-on)
  (and (interactive-p)
       (or (sq-valid-buffer-p)
           (error (format "Not in %s buffer."
                          (if (eq major-mode
                                  'message-mode)
                              "*news*"
                            "mail"))))
       (or sq-last-quote (error "There is no quote to remove!")))
  (save-excursion
    (goto-char (point-max))
    (if (search-backward sq-last-quote (point-min) t)
        (progn
          (replace-match (or new-quote ""))
          (setq sq-last-quote (or new-quote ""))
          (or new-quote t)))))

(defun sq-toggle-confirm-quote ()
  "Toggle the value of sq-confirm-quote."
  (interactive)
  (setq sq-confirm-quote (not sq-confirm-quote))
  (message (concat "sq-confirm-quote is " (if sq-confirm-quote "t" "nil"))))

(defun sq-quote-types ()
  "Display the types of quotes appropriate for this letter."
  (interactive)
  (sq-assert-mode-on)
  (let ((type-string (sq-make-type-string
                      (sq-generate-type-list (sq-header-addresses)))))
    (if type-string
        (sq-display-quote (sq-make-quote type-string)
                          "These are the types of quotes to chose from...")
      (if (sq-header-addresses)
          (message "The list of quote types is empty!")
        (message "There are no email addresses to determine the quote type!")
        ))))


;; add string to the mode-line
(and (not (assq 'sig-quote-mode minor-mode-alist))
     (setq minor-mode-alist (cons (list 'sig-quote-mode " Sig-Quote")
                                  minor-mode-alist)))

;;}}}

;;{{{ Functions operating on a quote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions operating on a quote

(defun sq-insert-signature-quote (quote)
  "Given a quote, insert it after the signature, replacing any previous quote.
If `mail-signature' is nil, add the quote to the end of the mail."
  (let ((quote-text (sq-quote-text quote)))
    (if sq-credit-after-quote
        (setq quote-text (concat quote-text (sq-quote-credit quote)))
      (setq quote-text (concat (sq-quote-credit quote) quote-text)))
    (if sq-newline-before-quote
        (setq quote-text (concat "\n" quote-text))) ; add a newline before the quote
    (if (not (equal "\n" (substring quote-text -1))) ; newline after quote
        (setq quote-text (concat quote-text "\n")))
    (save-excursion
      (cond

       ;; if last quote exists, replace it
       ((and (not (equal "" sq-last-quote))
             (sq-remove-quote quote-text)))

       ;; if no last quote, but mail-signature, add quote after signature
       (mail-signature
        (goto-char (point-max))
        (if (search-backward sq-signature nil t)    ; if can find signature
            (forward-char (length sq-signature)))
        (and (not (bolp)) (insert "\n"))            ; add newline if necessary
        (setq sq-last-quote quote-text)
        (insert quote-text))

       ;; else, no signature, so add at end
       (t
        (goto-char (point-max))
        (setq sq-last-quote quote-text)
        (insert quote-text))))))

(defun sq-display-quote (quote &optional query)
  "Displays a string in a temporary window until a key is pressed.
Optional second argument is either a function to run, or a message ()
<string object> to display while waiting for the user to press a key.
Returns either the value returned by the function, or the key pressed."
  (if quote
      (save-excursion
        (let* (ret-val
               (orig-window (selected-window))
                                        ;       (orig-buffer (current-buffer))
               (orig-window-height (window-height))
               (prev-min-height window-min-height)
               (quote-window (split-window-vertically))
               (quote-buffer (generate-new-buffer " Signature Quote")))

          (select-window quote-window)
          (switch-to-buffer quote-buffer)

          ;; fix me, should include the credit also...
          (insert (sq-quote-text quote))
          (goto-char (point-min))

          (setq window-min-height 1)
          (shrink-window-if-larger-than-buffer)
          (select-window orig-window)

          (unwind-protect
              (if (and query (not (stringp query)))
                  (setq ret-val (apply query '()))
                (setq ret-val (progn
                                (message (if query
                                             query
                                           "Press any key to continue..."))
                                (read-char-exclusive))))
            (delete-window quote-window)

            (kill-buffer quote-buffer)
            (setq window-min-height prev-min-height)

            ;; nasty hack - if mail window is not on bottom of emacs frame
            ;; the shrink-window-if-larger-than-buffer makes the *mail*
            ;; buffer smaller, so enlarge it to look normal
            (enlarge-window (- orig-window-height (window-height)))
            (message nil)
            ret-val)))
    (message "No quote to display!")))


;; quote abstraction
(defun sq-make-quote (text &optional type)
  (cons text type))

(defun sq-quote-text (quote)
  (car quote))
(defun sq-quote-type (quote)
  (cdr quote))
(defun sq-quote-credit (quote)
  "Given a quote, return the quote with its credit (type dependent) appended.
The quote credits are held in sq-quote-credit-alist."
  (cdr (assoc (sq-quote-type quote) sq-quote-credit-alist)))

;;}}}

;;{{{ functions operating on no arguments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions operating on no arguments

(defun sq-list-of-quotes ()
  "Return a list of the quotes found in the file `sq-quote-file'.
Open the file when first called, then again only when the quote file
has been modified."
  (save-excursion
    ;; if not modified since last loading
    (if (and sq-list-of-quotes
             (not (sq-quote-file-modified-p)))
        (progn
          ;; need to re initialize the quote types, oops...
          (sq-list-of-quote-types)
          sq-list-of-quotes)
      (setq sq-list-of-quotes
            (let ((ret-val nil)
                  p
                  (quote-buffer
                   (set-buffer
                    (find-file-noselect
                     (expand-file-name sq-quote-file)))))

              ;; save last-modified timestamp for future use...
              (setq sq-quote-file-timestamp
                    (nth 5 (file-attributes sq-quote-file)))

              (goto-char (point-min))

              ;; added this to allow for folded signature quote file
              (and (boundp 'folding-mode)
                   folding-mode
                   (folding-open-buffer))

              ;; while there are still groups of quotes to read in
              (while (re-search-forward sq-quote-type-begin nil 1 1)
                (let* ((eol (save-excursion (end-of-line) (point)))
                       (current-type (buffer-substring (point) eol))
                       (eot (save-excursion  ; end of type
                              (re-search-forward sq-quote-type-end)
                              (beginning-of-line)
                              (point)))
                       (current-credit ; credit for current quote type
                        (let* ((start (point))
                               (pos (re-search-forward sq-quote-credit-regex eot t))
                               (eol (save-excursion (end-of-line) (point)))
                               ret)
                          (cond (pos
                                 (setq ret (buffer-substring (point) eol))
                                 (goto-char start) ;;(beginning-of-line)
                                 (forward-line 1)
                                 ret)
                                ((goto-char start)
                                 nil)))))

                  ;; add current-credit to sq-quote-credit-alist
                  (and current-credit
                       (let ((pair (assoc current-type sq-quote-credit-alist)))
                         (if pair
                             (setcdr pair current-credit)
                           (setq sq-quote-credit-alist (cons (cons current-type current-credit)
                                                             sq-quote-credit-alist)))))


                  (forward-line 1)
                  (setq p (point))  ;; beginning of type

                  (while (re-search-forward sq-quote-delimeter eot 1 1)
                    (beginning-of-line) ;; before sq-quote-delimeter
                    (setq ret-val (cons (sq-make-quote (buffer-substring
                                                        p (point))
                                                       current-type)
                                        ret-val))
                    (forward-line 1) ;; point after sq-quote-delimeter
                    (setq p (point)))
                  (setq ret-val (cons (sq-make-quote (buffer-substring
                                                      p (point))
                                                     current-type)
                                      ret-val))))
              (kill-buffer quote-buffer)
              (message nil)
              ret-val)))))

(defun sq-list-of-quotes ()
  "Return a list of the quotes found in the file `sq-quote-file'.
Open the file when first called, then again only when the quote file
has been modified."
  (save-excursion
    ;; if not modified since last loading
    (if (and sq-list-of-quotes
             (not (sq-quote-file-modified-p)))
          sq-list-of-quotes

      ;; file has changed, so...
      (setq sq-list-of-quotes
            (let ((ret-val nil)
                  p
                  (quote-buffer
                   (set-buffer
                    (find-file-noselect
                     (expand-file-name sq-quote-file)))))

              ;; save last-modified timestamp for future use...
              (setq sq-quote-file-timestamp
                    (nth 5 (file-attributes sq-quote-file)))

              (goto-char (point-min))

              ;; added this to allow for folded signature quote file
              (and (boundp 'folding-mode)
                   folding-mode
                   (folding-open-buffer))

              ;; while there are still groups of quotes to read in
              (while (re-search-forward sq-quote-type-begin nil 1 1)
                (let* ((current-type (buffer-substring (point) (save-excursion
                                                                 (end-of-line)
                                                                 (point))))
                       (bot (save-excursion  ; beginning of type
                              (beginning-of-line)
                              (forward-line 1)
                              (point)))
                       (eot (save-excursion  ; end of type
                              (re-search-forward sq-quote-type-end)
                              (beginning-of-line)
                              (point)))

                       current-credit)

                  ;; get quote credit
                  (narrow-to-region bot eot)
                  (setq current-credit (sq-get-credit))
                  (widen)

                  ;; add current-credit to sq-quote-credit-alist
                  (and current-credit
                       (setq sq-quote-credit-alist (cons (cons current-type
                                                               current-credit)
                                                         sq-quote-credit-alist)))

                  ;; but now that the point is after the credit
                  ;; narrow to just contain quotes
                  (narrow-to-region (point) eot)


                  (setq p (point))
                  
                  (while (re-search-forward sq-quote-delimeter (point-max) 1 1)
                    (beginning-of-line) ;; before sq-quote-delimeter
                    (setq ret-val (cons (sq-make-quote (buffer-substring
                                                        p (point))
                                                       current-type)
                                        ret-val))
                    (forward-line 1) ;; point after sq-quote-delimeter
                    (setq p (point)))
                  (setq ret-val (cons (sq-make-quote (buffer-substring
                                                      p (point))
                                                     current-type)
                                      ret-val))

                  (widen)))

              ;; no more types to parse
              (kill-buffer quote-buffer)
              (message nil)

              ret-val))

      ;; need to initialize the quote-types too...
      ;; bad programming....
      (sq-list-of-quote-types)

      ;; still want to return the right value
      sq-list-of-quotes)))

(defun sq-get-credit ()
  "Return the first quote credit after the point.
The quote credit is either the string following `sq-quote-credit-regex',
or on lines between `sq-quote-credit-begin' and `sq-quote-credit-end'.
Returns first such occurrence in visible buffer,
thus if narrowed to a certain quote type, will return its credit.

Upon return,
the point will be at the beginning of the first line following the credit,
or if no credit, at (point-min)."
  (goto-char (point-min))
  
  (cond

   ;; multiple line credit
   ((re-search-forward sq-quote-credit-end (point-max) t)
    (let* ((eoc (progn (beginning-of-line)
                       (backward-char 1)  ; don't want extra "\n"
                       (point)))
           (boc (progn (re-search-backward sq-quote-credit-begin (point-min) t)
                       (forward-line 1)
                       (beginning-of-line)
                       (point))))
      (goto-char eoc)
      (beginning-of-line)
      (forward-line 2)
      (buffer-substring boc eoc)))

   ;; single line credit
   ((re-search-forward sq-quote-credit-regex (point-max) t)
    (buffer-substring (point) (progn (end-of-line) (point))))

   ;; no credit
   (t
    (goto-char (point-min))
    nil)))

(defun sq-list-of-quote-types ()
  "Return a list of the quotes found in the file `sq-quote-file'."
  (or (and (not (sq-quote-file-modified-p))
           sq-list-of-quote-types)
      (setq sq-list-of-quote-types
            (sq-uniquify (mapcar 'sq-quote-type (sq-list-of-quotes))))))

(defun sq-get-header-addresses (header)
  "Return a list of the addresses in the HEADER field."
  (save-excursion
    (cond ((mail-position-on-field header t)
           (let (p
                 res)
             (apply (cdr (assoc header '(("Newsgroups" . news-reply-newsgroups)
                                         ("To" . mail-to)
                                         ("CC" . mail-cc)
                                         ("BCC" . mail-bcc))))
                    nil)
             (setq p (point))
             (search-backward-regexp (concat "^" (regexp-case-insensitive header) ":"))
             (narrow-to-region (+ (length header) 2 (point)) p)
             (goto-char (point-min))
             (setq res (mail-parse-comma-list))
             (widen)
             res)))))

(defun sq-header-addresses ()
  "Return a list of the addresses in the fields specified by sq-header-list."
  (apply 'nconc (mapcar 'sq-get-header-addresses (if (eq t sq-header-list)
                                                     '("Newsgroups" "To" "CC" "BCC")
                                                   sq-header-list))))
    
(defun sq-insert-gnus-signature ()
  "Function to add the gnus-post-prepare-hook to insert signature properly."
  (if (or (and message-signature
               (funcall message-signature gnus-newsgroup-name))
          message-signature-file)
      (message-insert-signature)
    (save-excursion
      (goto-char (point-max))
      (insert "\n\n")
      (goto-char (point-max))
      (cond ((null mail-signature) nil)
            ((eq mail-signature t)
             (insert "-- \n")
             (insert-file-contents "~/.signature"))
            ((file-exists-p mail-signature)
             (insert "-- \n")
             (insert-file-contents mail-signature))
            ((stringp mail-signature)
             (insert mail-signature))))))


(defun sq-select-appropriate-quote (&optional option)
  "Return a quote randomly chosen from the appropriate type.
Quote types are associated with email addresses in
sq-address-n-quote-type-alist.

Optional argument OPTION is used to specify the quote type
selection process.
  nil       - use email addresses
  ignore    - use user inputted quote type (ignore email addresses)
  search    - select quotes containing user inputted string"

  ;; choose quote-list and randomly choose quote until user is satisfied
  (let (quote
        quote-list
        (quote-ok nil))

    ;; choose appropriate quote list
    (setq quote-list
          (cond ((eq option 'search)
                 (sq-search-quotes))
                ((eq option 'ignore)
                 (sq-quotes-based-on-address t))
                (t (sq-quotes-based-on-address))))

    (random t)
    (while (not quote-ok)
      (if quote-list
          (setq quote (nth (random (length quote-list)) quote-list))
        (setq quote nil))
      (if sq-confirm-quote
          (setq quote-ok (sq-display-quote quote 'sq-confirm-quote-fcn))
        (setq quote-ok t)))
    quote))


(defun sq-search-quotes ()
  "Return a list of quotes containing a string specified by the user."
  (let ((search-string (read-string "Search quote text: ")))
    (map-filter (function (lambda (quote) (string-match
                                           (regexp-quote search-string)
                                           (sq-quote-text quote))))
                (sq-list-of-quotes))))


(defun sq-quotes-based-on-address (&optional ignore-addresses)
  "Return a list of quotes depending on the email addresses.

If optional argument IGNORE-ADDRESSES is non-nil,
the user is queried for a quote type."

  (let ((type-list nil)
        (addresses (sq-header-addresses)))

    ;; if no addresses
    (if (and (not addresses) (not ignore-addresses))
        (if (y-or-n-p
             "No email addresses to determine quote type!  -- default to all? ")
            (setq type-list t))
      (setq type-list (sq-generate-type-list addresses)))

    ;; if no common quote types amongst email addresses,
    ;; or if ignore-addresses -> chose a quote type
    (if (or ignore-addresses
            (and (not type-list)
                 sq-warn-if-no-quote
                 (y-or-n-p
"The email addresses have no common types, would you like to chose a type of quote? ")))
        (setq type-list (list (completing-read "Choose a quote type: "
                                               (mapcar 'list (sq-list-of-quote-types))
                                               nil
                                               t)))
      (message nil))

    (sq-filter-quotes (sq-list-of-quotes) type-list)))


(defun sq-confirm-quote-fcn ()
  (y-or-n-p "Do you like this quote? "))

(defun sq-valid-buffer-p ()
  "Return t if current buffer is in mode for composing mail/news."
  (memq major-mode '(mail-mode mh-letter-mode message-mode)))

(defun sq-quote-file-modified-p ()
  "Return t if quote file has been modified since last use."
  (not (equal sq-quote-file-timestamp
              (nth 5 (file-attributes sq-quote-file)))))

;;}}}

;;{{{ functions operating on a quote or type list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions operating on a quote or type list

(defun sq-types-of-quotes (quote-list)
  "Given a list of quotes, return a list of their types without duplicates."
  (let ((type-list nil)
        (all-quote-types (mapcar 'sq-quote-type quote-list)))
    (while all-quote-types
      (if (and (car all-quote-types)
               (not (member (car all-quote-types) type-list)))
          (setq type-list (cons (car all-quote-types) type-list)))
      (setq all-quote-types (cdr all-quote-types)))
    type-list))


(defun sq-filter-quotes (qlist type-list &optional throw-away)
  "Given a list of quotes, QLIST, return quotes of the types in TYPE-LIST.
If TYPE-LIST is t, then return QLIST.

If optional THROW-AWAY is true, the types listed in
TYPE-LIST are to be excluded from the resulting list."
  (if (eq type-list t)
      qlist
    (let ((keep-list nil))
      (while qlist
        (if (xor (member (sq-quote-type (car qlist)) type-list)
                 throw-away)
            (setq keep-list (cons (car qlist) keep-list)))
        (setq qlist (cdr qlist)))
      keep-list)))


(defun sq-make-type-string (type-list)
  "Concatenate the given list of quote types TYPE-LIST separated by newlines.
If TYPE-LIST is t, for the string from all possible quote types."
  (if (eq type-list t)
      (concat-string-list (sq-types-of-quotes (sq-list-of-quotes)) "\n")
    (concat-string-list type-list "\n")))


;;(defun sq-intersect-type-lists (&rest lists)
;;  "Given an arbitrary number of lists of strings, return a list containing
;;strings present in all the lists.  If one of the lists isn't a list, but 't
;;then use the rule `t intersect X = X'.  If the list begins with the atom
;;'not then treat this list as all types but those listed."

;;  (defun merge (A B)
;;    (defun merge-help (l)
;;      (if l
;;          (if (member (car l) B)
;;              (cons (car l) (merge-help (cdr l)))
;;            (merge-help (cdr l)))
;;        nil))

;;    ;; if A or B are t then return the other.
;;    (cond ((not (listp A)) B)
;;          ((not (listp B)) A)
;;          (t (merge-help A))))

;;  (cond ((not lists) nil)
;;        ((not (cdr lists)) (sq-un-not-ify (car lists)))
;;        (t (merge (sq-un-not-ify (car lists)) (apply 'sq-intersect-type-lists
;;                                                     (cdr lists))))))

(require 'cl)
(defun sq-intersect-type-lists (&rest lists)
  "Find the set intersection of any number of lists of strings.
If one of the arguments is t, then use the rule `t intersect X = X'.
Lists beginning with the atom `not' are treated as a list of all *but*
those in the list."
  (if (member nil lists)
      nil
    (let ((new-lists (mapcar (lambda (l)
                                (if (eq t l)
                                    (sq-list-of-quote-types)
                                  (sq-un-not-ify l)))
                              lists))
           res)
      (setq res (car new-lists))
      (setq new-lists (cdr new-lists))
      (while (and new-lists res)
        (setq res (intersection res (car new-lists) :test 'equal))
        (setq new-lists (cdr new-lists)))
      res)))

(defun sq-unify-type-lists (&rest lists)
  (if (member t lists)
      (sq-list-of-quote-types)
    (let ((new-lists (mapcar 'sq-un-not-ify lists))
          res)
      (setq res (car new-lists))
      (setq new-lists (cdr new-lists))
      (while (car new-lists)
        (setq res (union res (car new-lists) :test 'equal))
        (setq new-lists (cdr new-lists)))
      res)))

(defun sq-un-not-ify (type-list)
  "Invert type lists beginning with the atom `not'.
Given a type list of the form (not TYPE1 TYPE2 ... TYPEn),
return a list containing all types but those listed.
All other type lists are returned as is."
  (if (and (listp type-list)
           (eq (car type-list) 'not))
      (let ((unwanted-types (cdr type-list)))
        (map-filter (function
                     (lambda (type)
                       (not (member type unwanted-types))))
                    (sq-list-of-quote-types)))
    type-list))


(defun sq-generate-type-list (address-list)
  "Given a list of email addresses, return a list of appropriate quote types.
The ADDRESS-LIST is parsed according to the associations
in `sq-address-n-quote-type-alist'."
  (apply sq-quote-type-resolution-function ;;was 'sq-intersect-type-lists
         (mapcar (function
                  (lambda (address)
                    (let ((ret-val nil)
                          (go t)
                          (regexp-pairs sq-address-n-quote-type-alist)
                          first-pair)
                      (while (and go regexp-pairs)
                        (setq first-pair (car regexp-pairs))
                        (if (string-match (car first-pair) address)
                            (progn (setq ret-val (cdr first-pair))
                                   (setq go nil)))
                        (setq regexp-pairs (cdr regexp-pairs)))
                      (if (not go)
                          ret-val
                        (or ret-val sq-quote-type-default)))))
                 address-list)))

;;}}}

;;{{{ other functions

(defun sq-uniquify (lst)
  "Remove duplicate strings from the list."
  (let ((res nil))
    (while lst
      (if (not (member (car lst) res))
          (setq res (cons (car lst) res)))
      (setq lst (cdr lst)))
    res))

(defun concat-string-list (lst &optional between)
  "Concatenates each element of the list to form a single string.
Spaces are placed between each element.  Optional second argument
is a string to insert between each element of the list, (default: \" \")"
  (setq between (or between " "))
  (and lst
       (concat (car lst) (if (cdr lst) between) (concat-string-list (cdr lst)
                                                                    between))))

;(defun concat-string-list (lst &optinal between)
;  (mapconcat 'identity lst (or between " ")))
 

(defun regexp-case-insensitive (str)
  "Given a string, return the regexp matching the string w/out case"
  (let ((up (upcase str))
        (down (downcase str))
        (len (length str))
        (ret ""))
    (while (< 0 len)
      (setq len (1- len))
      (let ((u (char-to-string (elt up len)))
            (d (char-to-string (elt down len))))
        (if (equal u d)
            (setq ret (concat u ret))
          (setq ret (concat "[" u d "]" ret)))))
    ret))

(defun map-filter (pred lst)
  "Filter out elements of a list that fail the predicate."
  (let ((ret nil))
    (while lst
      (if (apply pred (list (car lst)))
          (setq ret (cons (car lst) ret)))
      (setq lst (cdr lst)))
    ret))

(defun xor (a b)
  (or (and a (not b)) (and (not a) b)))

(defun sq-mail-system (sys)
  "Adjust internal variables for various mail systems."
  (if (or (not sq-mail-system)  ; first time
          (memq sys '(rmail mh-e vm)))
      (progn
        (require 'mail-utils)
        (cond ((eq sys 'mh-e)
               (require 'mh-comp)
               (setq sq-mail-system 'mh-e
                     sq-mail-mode-map 'mh-letter-mode-map
                     sq-mail-mode-hook 'mh-letter-mode-hook
                     sq-mail-send-hook 'mh-before-send-letter-hook)
               (fset 'sq-send-and-exit 'mh-send-letter))
              ((eq sys 'vm)
               (require 'vm)
               (setq sq-mail-system 'vm
                     sq-mail-mode-map 'vm-mail-mode-map
                     sq-mail-mode-hook 'vm-mail-mode-hook
                     sq-mail-send-hook 'mail-send-hook)
               (fset 'sq-send-and-exit 'vm-mail-send-and-exit))
              (t ;else rmail
               (require 'sendmail)
               (setq sq-mail-system 'rmail
                     sq-mail-mode-map 'mail-mode-map
                     sq-mail-mode-hook 'mail-mode-hook
                     sq-mail-send-hook 'mail-send-hook)
               (fset 'sq-send-and-exit 'mail-send-and-exit)))
        (and sq-gnus-too (require 'message))
        t)
    nil))

(defun sq-switch-key-bindings ()
  "Used to swap sig quote keybindings in and out of the mail/news keymaps.

This function uses `sq-key-bindings' (and `sq-news-key-bindings') to determine
which keys should be bound to which sig quote functions.

`sq-key-bindings' is a list of lists of the form (KEY FCN), except that the
first element of the list is either t or nil (used as a flag).
When the first element is t, the keys have not been installed.
When the first element is nil, the keys have been installed, and the functions
previously bound to those keymaps are stored in the FCN position.

`sq-news-key-bindings' is analogous to `sq-key-bindings'"
  (setq sq-key-bindings (cons (not (car sq-key-bindings))
                              (mapcar (function
                                       (lambda (stuff)
                                         (apply 'sq-install-key
                                                (eval sq-mail-mode-map)
                                                stuff)))
                                      (cdr sq-key-bindings))))
  (and sq-gnus-too
       (setq sq-news-key-bindings (cons (not (car sq-news-key-bindings))
                                        (mapcar (function
                                                 (lambda (stuff)
                                                   (apply 'sq-install-key
                                                          message-mode-map
                                                          stuff)))
                                                (cdr sq-news-key-bindings))))))

(defun sq-install-key (keymap key op)
  (let ((oldop (lookup-key keymap key)))
    (define-key keymap key op)
    (list key oldop)))

;;}}}

;;}}}

;;}}}

(provide 'sig-quote)



