;;; wiki-nav.el --- simple file navigation using [[WikiStrings]]
;;
;; Copyright (c) 2011 D Roland Walker
;;
;; Author: D Roland Walker <walker@pobox.com>
;; URL: https://github.com/rolandwalker/button-lock/raw/master/wiki-nav.el
;; Version: 0.12
;; Last-Updated: 16 Nov 2011
;; EmacsWiki: WikiNavMode
;; Keywords: mouse, button, hyperlink, navigation
;; Package-Requires: ((button-lock "0.8"))
;;
;; Simplified BSD License
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;
;;
;; Table of Contents
;;
;; [[Intro]]
;; [[Example usage]]
;; [[See Also]]
;; [[Prior Art]]
;; [[Notes]]
;; [[Bugs]]
;; [[Compatibility]]
;; [[Todo]]
;; [[License]]
;; [[Code]]
;;
;;;;;;;;;;;;;;;;;;;
;;
;; [[<Intro]]
;;
;; Wiki-nav.el is a minor mode which recognizes [[wiki-style]]
;; double-bracketed navigation links in any type of file, providing
;; the ability to jump between sections, between files, or open
;; external links.
;;
;; Wiki-nav.el requires button-lock.el, which in turn requires
;; font-lock.el.  Font-lock.el is provided with Emacs.
;; Button-lock.el is available here
;;
;;    https://github.com/rolandwalker/button-lock
;;
;; [[<Example usage]]
;;
;;     Add the following to your ~/.emacs
;;
;;        (require 'wiki-nav)
;;        (global-wiki-nav-mode 1)
;;
;;     and sprinkle
;;
;;        [[links]]
;;
;;     throughout your files.  That's it.  There's more functionality,
;;     but simple [[links]] may be all you need.
;;
;;     Clicking a [[link]] will invoke a text search for the next
;;     matching link.  Double-clicking a link will search for matching
;;     links in all open buffers.
;;
;;     Text matching between links is always case-insensitive.
;;
;;     To navigate upward to a previous matching link, add a '<'
;;     symbol before the search text
;;
;;        [[<links]]
;;
;;     You can insert the '>' symbol, too, but that simply indicates
;;     the default forward-search navigation.
;;
;;     Both foward and backward navigation will wrap around the ends
;;     of the file without prompting.
;;
;;     Leading and trailing space inside a link is ignored.
;;
;;     After navigating to a new location, wiki-nav gives a helpful
;;     transient flash to help you find the cursor.  If you find this
;;     annoying, it can be changed by customizing wiki-nav-flash-delay.
;;
;;     From the keyboard:
;;
;;        control-c w   skip foward in the buffer to the next link
;;
;;        control-c W   skip backward in the buffer to the previous link
;;
;;        return        if positioned on a link, activate it
;;
;;        tab           if positioned on a link, skip forward in the
;;                      buffer to the next link of any kind (need not
;;                      match the current link)
;;
;;        S-tab         if positioned on a link, skip backward in the
;;                      buffer to the previous link of any kind (need not
;;                      not match the current link)
;;
;; Advanced usage:
;;
;;     Bracketed links may contain external URLs
;;
;;        [[http://google.com]]
;;
;;     Or they may use various internally-recognized URI schemes:
;;
;;     visit: navigates to another file
;;
;;        [[visit:/etc/hosts]]
;;
;;        [[visit:/path/to/another/file:NameOfLink]]
;;
;;     func: navigates to the definition of a function
;;
;;        [[func:main]]
;;
;;     line: navigates to a line number
;;
;;        [[line:12]]
;;
;;     visit: may be combined with other schemes:
;;
;;        [[visit:/path/to/another/file:func:main]]
;;
;;        [[visit:/etc/hosts:line:5]]
;;
;;     Path names and simialar strings are subjected to URI-style
;;     unescaping before lookup.  To link a filename which contains a
;;     colon, substitute "%3A" for the colon character.
;;
;;     See the documentation for the function wiki-nav for more
;;     information.
;;
;; [[<See Also]]
;;
;;     M-x customize-group RET wiki-nav RET
;;
;; [[<Prior Art]]
;;
;;     linkd.el
;;     David O'Toole <dto@gnu.org>
;;
;;     org-mode
;;     Carsten Dominik <carsten at orgmode dot org>
;;
;; [[<Notes]]
;;
;;     wiki-nav uses industry-standard left-clicks rather than
;;     Emacs-traditional middle clicks.
;;
;;     It is difficult to edit the text inside a link using the
;;     mouse.  To make a link inactive, position the point after the
;;     link and backspace into it.  Once the trailing delimiters have
;;     been modified, the link reverts to ordinary text.
;;
;; [[<Bugs]]
;;
;;     Double-square-brackets represent a valid construct in some
;;     programming languages, and may be mistakenly linked.  Workaround:
;;     don't click.  Second workaround: change delimiters to triple-
;;     square-brackets via customize.
;;
;;     Newlines are not escaped in regexp fields in customize.
;;
;;     Case-sensitivity on matching the delimiters is unknown because
;;     it depends on how font-lock-defaults was called for the current
;;     mode.  However, this is not an issue unless the default delimiters
;;     are changed to use alphabetical characters.
;;
;;     Auto-complete interacts and causes keyboard interaction
;;     problems.  Auto-complete should be suppressed if the point is
;;     on a link?
;;
;; [[<Compatibility]]
;;
;;     Tested only on GNU Emacs version 23.x
;;
;; [[<Todo]]
;;
;;    remember position and undo last motion
;;
;;    visit:-1 counts from end of file
;;
;;    keyboard analog for double-click
;;
;;    right-click context menu
;;
;;    link any string <<<text>>> together within a file
;;    like org-mode radio links
;;
;;    use pulse if present for more subtle flash overlay
;;
;;    patch font-lock to support keyword searching in comment only,
;;    like 'keep, only different
;;
;;    raised button style option
;;
;;    break down monolithic dispatch function wiki-nav-action-1
;;
;;    schemes to add
;;       search:
;;       regexp:
;;       elisp:
;;
;; [[<License]]
;;
;;    Simplified BSD License
;;
;;    Copyright (c) 2011, D Roland Walker
;;    All rights reserved.
;;
;;    Redistribution and use in source and binary forms, with or
;;    without modification, are permitted provided that the following
;;    conditions are met:
;;
;;       1. Redistributions of source code must retain the above
;;          copyright notice, this list of conditions and the following
;;          disclaimer.
;;
;;       2. Redistributions in binary form must reproduce the above
;;          copyright notice, this list of conditions and the following
;;          disclaimer in the documentation and/or other materials
;;          provided with the distribution.
;;
;;    This software is provided by D Roland Walker "AS IS" and any express
;;    or implied warranties, including, but not limited to, the implied
;;    warranties of merchantability and fitness for a particular
;;    purpose are disclaimed.  In no event shall D Roland Walker or
;;    contributors be liable for any direct, indirect, incidental,
;;    special, exemplary, or consequential damages (including, but not
;;    limited to, procurement of substitute goods or services; loss of
;;    use, data, or profits; or business interruption) however caused
;;    and on any theory of liability, whether in contract, strict
;;    liability, or tort (including negligence or otherwise) arising in
;;    any way out of the use of this software, even if advised of the
;;    possibility of such damage.
;;
;;    The views and conclusions contained in the software and
;;    documentation are those of the authors and should not be
;;    interpreted as representing official policies, either expressed
;;    or implied, of D Roland Walker.
;;
;; [[<Code]]
;;; Code:
;;

(eval-when-compile
  (require 'font-lock)
  (require 'button-lock))

(autoload 'button-lock-mode "button-lock" nil nil)

;;;###autoload
(defgroup wiki-nav nil
  "Wiki-style navigation links."
  :group 'button-lock)

(defcustom wiki-nav-less-feedback nil
  "Give less echo area feedback."
  :group 'wiki-nav
  :type 'boolean)

(defcustom wiki-nav-flash-delay .5
  "How many seconds to flash `wiki-nav-flash-face' after jumping to a new location via wiki-nav.

Setting this to zero will turn off the indicator."
  :group 'wiki-nav
  :type 'number)

(defcustom wiki-nav-modestring " wn"
  "This string appears in the modeline when wiki-nav mode is active."
  :group 'wiki-nav
  :type 'string)

(defcustom wiki-nav-comment-only-modes '(cperl-mode perl-mode sh-mode ruby-mode python-mode lisp-mode emacs-lisp-mode)
"A list of major modes for which to attempt to find navigation links in comments only.

Because this functionality is not built into font-lock, the
comment-only search must be less exact and/or less efficient.
This method is particularly inexact, but avoids being slow.

It will not work for c-mode and many other modes that have
multi-line comments or multi-character comment delimiters."
  :group 'wiki-nav
  :type '(repeat symbol))

(defcustom wiki-nav-exclude-modes  '(org-mode)
  "List of major modes in which global wiki-nav will not be activated.

This is in addition to any modes listed in `button-lock-exclude-modes'.

Modes may be excluded for reasons of security (since buttons can
execute arbitrary functions), efficiency, or to avoid conflicts
with modes that provide similar functionality."
  :type '(repeat symbol)
  :group 'wiki-nav)

(defcustom wiki-nav-multi-action-function 'wiki-nav-default-multi-action
  "Function to run on double-click of a wiki-nav link."
  :type 'symbol
  :group 'wiki-nav)

;;;###autoload
(defgroup wiki-nav-keys nil
  "Keyboard bindings used by wiki-nav"
  :group 'wiki-nav)

(defcustom wiki-nav-find-any-link-keys '("C-c w")
  "List of key sequences to search forward for the next defined wiki-nav link.

The search will automatically wrap past the end of the buffer.
The key binding is in effect anywhere in the buffer when wiki-nav
mode is active.

The format for key sequences is as defined by `kbd'."
  :group 'wiki-nav-keys
  :type '(repeat string))

(defcustom wiki-nav-find-any-previous-link-keys '("C-c W")
  "List of key sequences to search backward for the previous defined wiki-nav link.

The search will automatically wrap past the beginning of the
buffer.  The key binding is in effect anywhere in the buffer when
wiki-nav mode is active.

The format for key sequences is as defined by `kbd'."
  :group 'wiki-nav-keys
  :type '(repeat string))

(defcustom wiki-nav-activate-keys '("RET")
  "List of key sequences to activate a wiki-nav link under the point.

The key binding is active only when the point is on a wiki-nav link.

The format for key sequences is as defined by `kbd'."
  :group 'wiki-nav-keys
  :type '(repeat string))

(defcustom wiki-nav-skip-to-next-keys '("TAB")
  "List of key sequences to skip forward from a wiki-nav link to the next defined link.

The key binding is active only when the point is on a wiki-nav link.

The format for key sequences is as defined by `kbd'."
  :group 'wiki-nav-keys
  :type '(repeat string))

(defcustom wiki-nav-skip-to-previous-keys '("S-TAB" "S-<tab>" "<backtab>" "S-<iso-lefttab>")
  "List of key sequences to skip backward from a wiki-nav link to the previous defined link.

The key binding is active only when the point is on a wiki-nav link.

The format for key sequences is as defined by `kbd'."
  :group 'wiki-nav-keys
  :type '(repeat string))

;;;###autoload
(defgroup wiki-nav-faces nil
  "Strings and regular expressions used by wiki-nav to define links."
  :group 'wiki-nav)

(defface wiki-nav-link-face
   '((t (:inherit link)))
  "Face to show wiki-nav links"
  :group 'wiki-nav-faces)

(defface wiki-nav-mouse-face
   '((t (:inherit button-lock-mouse-face)))
  "Face to highlight wiki-nav link mouseovers"
  :group 'wiki-nav-faces)

(defface wiki-nav-flash-face
   '((t (:inherit highlight)))
  "After jumping to a location via wiki-nav, the line of the new point location is briefly highlighted with this face."
  :group 'wik-nav-faces)

;;;###autoload
(defgroup wiki-nav-parsing nil
  "Strings and regular expressions used by wiki-nav to define links."
  :group 'wiki-nav)

(defcustom wiki-nav-link-start   "[["
"A string (not a regular expression) which open a wiki-style navigation link.

Since the construct [[text]] can show up for other reasons, you might change this to \"[[[\"."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-link-stop   "]]"
"A string (not a regular expression) which closes a wiki-style navigation link.

Since the construct [[text]] can show up for other reasons, you might change this to \"]]]\"."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-link-text  "[^][\n]+"
"A regular expression defining the text inside wiki-style navigation links.

The value should exclude newlines and start/stop delimiters."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-external-link-pattern "^[a-zA-Z]+:[^[:space:]]+"
"A regular expression for recognizing URLs inside wiki-style navigation links.

The default is very permissive. To be stricter, try \"^[a-zA-Z]+://[^[:space:]]+\",
or \"^http://[^[:space:]]+\".

Setting the value to the empty string will disable the feature
entirely, suppressing the recognition of external URLs."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-visit-link-pattern "^visit:\\([^:\n]+?\\)\\(?:\\|:\\([^\n]*\\)\\)$"
"A regular expression for recognizing wiki-nav links outside the current file.

The format defined by the default expression is delimited by colons

   visit:/posix/path/to/another/file

      or

   visit:/posix/path/to/another/file:WikiString

Other interally recognized link schemes may be substititued for
the WikiString

   visit:/posix/path/to/another/file:line:10

Set this value to the empty string to disable the feature entirely."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-function-link-pattern "^func\\(?:tion\\)?:\\([^\n]+\\)$"
"A regular expression for recognizing wiki-nav links that point to function definitions.

The format defined by the default expression is delimited by colons

   func:function_name

Imenu is used to find the function definition.

Set this value to the empty string to disable the feature entirely."
  :group 'wiki-nav-parsing
  :type 'string)

(defcustom wiki-nav-line-number-link-pattern "^line:\\([0-9]+\\)$"
"A regular expression for recognizing wiki-nav links that point to line numbers.

The format defined by the default expression is delimited by colons

   line:111

Set this value to the empty string to disable the feature entirely."
  :group 'wiki-nav-parsing
  :type 'string)

(defvar wiki-nav-mode-keymap
  (let ((map (make-sparse-keymap)))
    (dolist (key wiki-nav-find-any-link-keys)
      (define-key map (eval `(kbd ,key))  'wiki-nav-find-any-link))
    (dolist (key wiki-nav-find-any-previous-link-keys)
      (define-key map (eval `(kbd ,key)) 'wiki-nav-find-any-previous-link))
    map))

(defvar wiki-nav-flash-overlay
  ;; Create and immediately delete, to get "overlay in no buffer".
  (let ((ol (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face      'wiki-nav-flash-face)
    (overlay-put ol 'priority  1000000)
    ol))

(define-minor-mode wiki-nav-mode
  "Turn on navigation by bracketed [[WikiStrings]] within a document.

When wiki-nav links are activated, clicking on a bracketed link
causes emacs to search the document for another link with text
matching the inner string.  If a match is found, the cursor is
moved to the location of the match.

If the string looks like it might be a URL (starts with
alphabetical characters followed by a colon), an external browser
will be spawned on the URL.  This bevavior can be controlled by the
custommizable variable `wiki-nav-external-link-pattern'.

If `multi-occur' is installed (standard with recent Emacs),
double-clicking a wiki-nav link will search for matching links in
all open file buffers.

If the link follows the form

   visit:/path/name:WikiString

Emacs will visit the named file, and search for the navigation
string there.  This behavior can be controlled by the customizable
variable `wiki-nav-visit-link-pattern'.

If the link follows the form

   func:FunctionName

the link will lead to the defintion of the given function, as
defined by imenu. This behavior can be controlled by the
customizable variable `wiki-nav-function-link-pattern'.

If the link follows the form

   line:<digits>

the link will lead to the given line number.  This behavior can
be controlled by the customizable variable
`wiki-nav-line-number-link-pattern'.

The leading and trailing delimiters which define the navigation
links may be customized, as may the regular expressions that
match URLs and non-URL inner text.

With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."
  nil wiki-nav-modestring wiki-nav-mode-keymap
  (when (or noninteractive (eq (aref (buffer-name) 0) ?\s))  ; don't set up wiki-nav on hidden or noninteractive
                                                             ; buffers, b/c there will be no font-lock
    (setq wiki-nav-mode nil))
  (if wiki-nav-mode
      (progn
        (wiki-nav-link-set)
        (when (called-interactively-p)
          (message "wiki-nav mode enabled")))
  (wiki-nav-link-set -1)
  (when (called-interactively-p)
    (message "wiki-nav mode disabled"))))

(define-minor-mode global-wiki-nav-mode
  "Toggle global `wiki-nav-mode'.

The global mode will cause button-lock to be activated in every buffer,
unless specifically excluded by `wiki-nav-exclude-modes',
`button-lock-exclude-modes' or `button-lock-exclude-pattern'.

With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode."

  nil nil nil
  :global t
  :group 'wiki-nav

  (if global-wiki-nav-mode
      (progn
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (maybe-local-wiki-nav)))
         (add-hook 'after-change-major-mode-hook 'maybe-local-wiki-nav))
    (remove-hook 'after-change-major-mode-hook  'maybe-local-wiki-nav)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (maybe-local-wiki-nav -1)))))

(defun maybe-local-wiki-nav (&optional arg)
  "Called by global-wiki-nav-mode to activate wiki-nav mode in a buffer if appropriate.

If called with a negative ARG, deactivate wiki-nav mode in the buffer."
  (setq arg (or arg 1))
  (unless (or wiki-nav-mode
              (or noninteractive (eq (aref (buffer-name) 0) ?\s))
              (memq major-mode wiki-nav-exclude-modes)
              (memq major-mode button-lock-exclude-modes)
              (string-match-p button-lock-exclude-pattern (buffer-name (current-buffer))))
    (wiki-nav-mode 1)))

(defun wiki-nav-link-set (&optional arg)
  "Use button-lock to set up wiki-nav links in a buffer.

If called with negative ARG, remove the link."
  (setq arg (or arg 1))
  (unless button-lock-mode
    (button-lock-mode 1))
  (let ((button (button-lock-set-button (concat (if (member major-mode wiki-nav-comment-only-modes) "\\s<[^\n]*?" "")
                                                (regexp-quote wiki-nav-link-start)
                                                "\\(" wiki-nav-link-text "\\)"
                                                (regexp-quote wiki-nav-link-stop))
                                        'wiki-nav-mouse-action
                                        :double-mouse-1 wiki-nav-multi-action-function
                                        :face 'wiki-nav-link-face :mouse-face 'wiki-nav-mouse-face :face-policy 'prepend
                                        :additional-property 'wiki-nav
                                        :grouping 1
                                        :remove (if (< arg 0) t nil))))
    (dolist (key wiki-nav-activate-keys)
      (button-lock-extend-binding button 'wiki-nav-keyboard-action         nil key))
    (dolist (key wiki-nav-skip-to-next-keys)
      (button-lock-extend-binding button 'wiki-nav-find-any-link           nil key))
    (dolist (key wiki-nav-skip-to-previous-keys)
      (button-lock-extend-binding button 'wiki-nav-find-any-previous-link  nil key))))

(defun wiki-nav-point-before ()
  "Return the position before the current point, or (point-min) if the point is at the minimum."
  (if (> (point-min) (- (point) 1))
      (point-min)
    (- (point) 1)))

;; was doing this by scanning text properties, but that
;; fails when font-lock has not fontified the whole buffer
(defun wiki-nav-find-any-link (&optional arg)
  "Skip forward to the next defined wiki-nav link.

Automatically wraps past the end of the buffer.

With a negative prefix argument ARG, skip backward to the
previous defined wiki-nav link."
  (interactive "p")
  (let ((newpos nil)
        (skip-function 'next-single-property-change)
        (search-function 're-search-forward)
        (look-function 'point)
        (wrap-point (point-min))
        (bounds nil))
    (when (and arg
               (< arg 0))
      (setq skip-function 'previous-single-property-change)
      (setq search-function 're-search-backward)
      (setq wrap-point (point-max))
      (setq look-function 'wiki-nav-point-before))
    ;; get out of the current link if we are in one
    (when (and (get-text-property (funcall look-function) 'wiki-nav)
               (setq newpos (funcall skip-function (point) 'wiki-nav)))
      (goto-char newpos))
    ;; find the next link
    (deactivate-mark)
    (if (funcall search-function (concat (if (member major-mode wiki-nav-comment-only-modes) "\\s<[^\n]*?" "")
                                         "\\("
                                         (regexp-quote wiki-nav-link-start)
                                         "\\("
                                         wiki-nav-link-text
                                         "\\)"
                                         (regexp-quote wiki-nav-link-stop)
                                         "\\)")
                 nil t)

        (progn
          (goto-char (match-beginning 2))
          (wiki-nav-flash-show))
      ;; else
      (goto-char wrap-point)
      (deactivate-mark)
      (when (funcall search-function (concat (if (member major-mode wiki-nav-comment-only-modes) "\\s<[^\n]*?" "")
                                             "\\("
                                             (regexp-quote wiki-nav-link-start)
                                             "\\("
                                             wiki-nav-link-text
                                             "\\)"
                                             (regexp-quote wiki-nav-link-stop)
                                             "\\)")
                     nil t)
        (goto-char (match-beginning 2))
        (wiki-nav-flash-show)))))

(defun wiki-nav-find-any-previous-link ()
  "Skip backward to the previous defined wiki-nav link.

Automatically wraps past the beginning of the buffer.

With a negative prefix argument ARG, skip backward to the
previous defined wiki-nav link."
  (interactive)
  (wiki-nav-find-any-link -1))

(defun wiki-nav-mouse-action (event)
  "Dispatch the default navigation action for the wiki-nav link at the mouse cursor."
  (interactive "e")
  (wiki-nav-action-1 (posn-point (event-end event))))

(defun wiki-nav-keyboard-action ()
  "Dispatch the default navigation action for the wiki-nav link under the point."
  (interactive)
  (wiki-nav-action-1 (point)))

;; Monolithic function to dispatch any link action.
(defun wiki-nav-action-1 (pos)
  "Dispatch the default navigation action for the wiki-nav link at POS."
  (let ((bounds (button-lock-find-extent pos 'wiki-nav))
        (string nil)
        (found nil)
        (visit nil)
        (search-function 're-search-forward)
        (wrap-point (point-min))
        (wrap-message "Search wrapped past end of file")
        (buffer-start (current-buffer))
        (point-start (point))
        (new-point nil)
        (case-fold-search t)
        (search-upper-case nil))
    (when bounds
      (setq string (apply #'buffer-substring-no-properties bounds))
      (when (string-match-p "^[[:space:]]*<" string)
        (setq search-function 're-search-backward)
        (setq wrap-point (point-max))
        (setq wrap-message "Search wrapped past beginning of file"))
      (setq string (replace-regexp-in-string "\\(^[[:space:]<>]*\\|[[:space:]]*$\\)" "" string))
      (if (and wiki-nav-external-link-pattern
               (and (> (length wiki-nav-visit-link-pattern) 0)
                    (not (string-match-p wiki-nav-visit-link-pattern string)))
               (and (> (length wiki-nav-function-link-pattern) 0)
                    (not (string-match-p wiki-nav-function-link-pattern string)))
               (and (> (length wiki-nav-line-number-link-pattern) 0)
                    (not (string-match-p wiki-nav-line-number-link-pattern string)))
               (and (> (length wiki-nav-external-link-pattern) 0)
                    (string-match-p wiki-nav-external-link-pattern string)))
          (progn
            (message "browsing to external URL...")
            (browse-url string))
        (save-match-data
          (when (and (> (length wiki-nav-visit-link-pattern) 0)
                     (string-match wiki-nav-visit-link-pattern string))
            (let ((tmp ""))
              (setq tmp (match-string 2 string))
              (switch-to-buffer (find-file (expand-file-name (url-unhex-string (match-string 1 string)))))
              (setq string tmp))
            (when (= (length string) 0)
              (wiki-nav-flash-show))
            (setq visit t))
          (when (> (length string) 0)
            (cond
             ((and (> (length wiki-nav-function-link-pattern) 0)
                   (string-match wiki-nav-function-link-pattern string))
                 ;; imenu return value is not helpful.  It also sometimes changes the mark. Wrap it in an excursion
                 (when (and (setq new-point (save-excursion (imenu (url-unhex-string (match-string 1 string))) (point)))
                            (not (= new-point (point))))
                   (goto-char new-point)
                   (wiki-nav-flash-show)
                   (setq found :func))
                 ;; return to the original buffer on failure
                 (unless found
                   (when (and visit
                              (> (length string) 0))
                     (switch-to-buffer buffer-start)
                     (setq visit nil))
                   (goto-char point-start)))
             ((and (> (length wiki-nav-line-number-link-pattern) 0)
                   (string-match wiki-nav-line-number-link-pattern string))
                 ;; For line-number scheme, go as far as possible, but don't set found unless successful.
                 ;; Don't worry about returning to original buffer on failure.
                 (let ((ln (string-to-number (match-string 1 string))))
                   (widen)
                   (goto-char (point-min))
                   (forward-line (1- ln))
                   (wiki-nav-flash-show)
                   (if (= (line-number-at-pos) ln)
                       (setq found :line))))
             (t
              (setq string (regexp-quote (url-unhex-string string)))
              (deactivate-mark)
              (if (funcall search-function (concat  (if (member major-mode wiki-nav-comment-only-modes) "\\s<[^\n]*?" "")
                                                   "\\("
                                                   (regexp-quote wiki-nav-link-start)
                                                   "\\("
                                                   "[[:space:]<>]*" string "[[:space:]]*"
                                                   "\\)"
                                                   (regexp-quote wiki-nav-link-stop)
                                                   "\\)")
                           nil t)
                  (progn
                    (setq found :jump)
                    (goto-char (match-beginning 2))
                    (wiki-nav-flash-show))
                ;; else
                (goto-char wrap-point)
                (deactivate-mark)
                (if (funcall search-function (concat (if (member major-mode wiki-nav-comment-only-modes) "\\s<[^\n]*?" "")
                                                     "\\("
                                                     (regexp-quote wiki-nav-link-start)
                                                     "\\("
                                                     "[[:space:]<>]*" string "[[:space:]]*"
                                                     "\\)"
                                                     (regexp-quote wiki-nav-link-stop)
                                                     "\\)")
                             nil t)
                    (progn
                      (setq found :wrap)
                      (goto-char (match-beginning 2))
                      (wiki-nav-flash-show))))
              ;; return to the original buffer on failure
              (unless found
                (when (and visit
                           (> (length string) 0))
                  (switch-to-buffer buffer-start)
                  (setq visit nil))
                (goto-char point-start))))
            (cond
             (visit
                 (unless wiki-nav-less-feedback
                   (message "followed link to new file")))
             ((or (not found)
                  (and (>= (point) (- (car bounds) (length wiki-nav-link-start)))
                       (<= (point) (+ (cadr bounds) (length wiki-nav-link-stop)))))
                 ;; give failure message even when wiki-nav-less-feedback is set
                 (message "no matching link found"))
             ((eq found :wrap)
                 (unless wiki-nav-less-feedback
                   (message wrap-message))))))))
    found))

(defun wiki-nav-default-multi-action (event)
  "Dispatch the default double-click navigation action for the wiki-nav link at POS."
  (interactive "e")
  (let ((bounds (button-lock-find-extent (posn-point (event-end event)) 'wiki-nav))
        (string nil)
        (case-fold-search t)
        (search-upper-case nil))
    (when bounds
      (setq string (replace-regexp-in-string "\\(^[[:space:]<>]*\\|[[:space:]]*$\\)" ""
                                             (apply #'buffer-substring-no-properties bounds)))
      (when (fboundp 'multi-occur-in-matching-buffers)
        (multi-occur-in-matching-buffers "^[^ *]"
                                         (concat
                                          (regexp-quote wiki-nav-link-start)
                                          "[[:space:]<>]*" string "[[:space:]]*"
                                          (regexp-quote wiki-nav-link-stop))
                                          t)))))


(defun wiki-nav-flash-remove-overlay ()
  "Remove the navigation flash overlay."
  (remove-hook 'pre-command-hook 'wiki-nav-flash-remove-overlay t)
  (delete-overlay wiki-nav-flash-overlay))

(defun wiki-nav-flash-show ()
  "Show the navigation flash overlay."
  (when (> wiki-nav-flash-delay 0)
    (move-overlay wiki-nav-flash-overlay (line-beginning-position) (1+ (line-end-position)))
    (add-hook 'pre-command-hook 'wiki-nav-flash-remove-overlay nil t)
    (run-with-idle-timer wiki-nav-flash-delay nil 'wiki-nav-flash-remove-overlay)))

(provide 'wiki-nav)
;;; wiki-nav.el ends here
