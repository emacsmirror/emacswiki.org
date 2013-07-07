;;; auto-completing-read.el
;;
;; Author: Phil S.

;; Commentary:
;;
;; This library provides two `completing-read' variants, each of which
;; immediately completes as far as possible after each new character insertion
;; by the user. If multiple options remain they are displayed.  When only one
;; option remains, it is accepted automatically.
;;
;; `auto-completing-read' is similar to `completing-read'
;; `auto-ido-completing-read' is similar to `ido-completing-read'

;; Example usage:
;;
;; (auto-completing-read ;; or: (auto-ido-completing-read 
;;  "Number: " 
;;  '("six" "twelve" "eighteen" "twenty four"))
;;
;; Typing "s" or "e" will immediately return "six" or "eighteen" respectively
;; because that first letter is sufficient to uniquely identify the value.
;; Typing "t" will immediately complete the input as far as the maximum common
;; prefix "twe" and offer the two options "twelve" and "twenty four". The user
;; can then type either "l" or "n" (being the next distinguishing letter) to
;; immediately return the appropriate value.

;;; Code:
;; `completing-read' variant:

(defvar auto-completing-read-map (make-sparse-keymap)
  "Keymap for `auto-completing-read', remapping `self-insert-command'
to `auto-completing-read-self-insert'.

`minibuffer-local-must-match-map' is used by `completing-read' when its
REQUIRE-MATCH argument is t.

In `auto-completing-read' we bind this keymap in its place.")

(define-key auto-completing-read-map
  [remap self-insert-command] 'auto-completing-read-self-insert)

(set-keymap-parent auto-completing-read-map minibuffer-local-must-match-map)

;;;###autoload
(defun auto-completing-read (prompt collection)
  "Like `completing-read' with auto-completion and auto-acceptance."
  (interactive)
  (let ((minibuffer-local-must-match-map auto-completing-read-map))
    (completing-read prompt collection nil t)))

(defun auto-completing-read-self-insert (n)
  "Insert the character, then attempt to complete the current string,
automatically exiting when only one option remains, and displaying the
completion options otherwise."
  (interactive "p")
  (self-insert-command n)
  (auto-completing-read-complete)
  (let ((my-completions (completion-all-sorted-completions)))
    (if (and my-completions (eq 0 (cdr my-completions)))
        (exit-minibuffer)
      (minibuffer-completion-help))))

(defun auto-completing-read-complete ()
  "Copied from `minibuffer-complete-and-exit'."
  (interactive)
  (condition-case nil
      (completion--do-completion nil 'expect-exact)
    (error 1)))
 
;; `ido-completing-read' variant:

(defvar auto-ido-completing-read-map (make-sparse-keymap)
  "Keymap for `auto-ido-completing-read', remapping `self-insert-command'
to `auto-ido-completing-read-self-insert'.

Every time `ido-completing-read' is called it re-initializes
`ido-common-completion-map' and sets its parent to be `minibuffer-local-map'.

In `auto-ido-completing-read' we provide this as a replacement parent.")

(define-key auto-ido-completing-read-map
  [remap self-insert-command] 'auto-ido-completing-read-self-insert)

(set-keymap-parent auto-ido-completing-read-map minibuffer-local-map)

;;;###autoload
(defun auto-ido-completing-read (prompt collection)
  "Like `ido-completing-read' with auto-completion and auto-acceptance."
  (interactive)
  (let ((minibuffer-local-map auto-ido-completing-read-map)
        (ido-enable-prefix t))
    (ido-completing-read prompt collection)))

(defun auto-ido-completing-read-self-insert (n)
  "Insert the character, then attempt to complete the current string,
automatically exiting when only one option remains."
  (interactive "p")
  ;; ido uses buffer-local pre- and post-command hooks, so we need to take
  ;; those into account when we simulate the subsequent `ido-complete'.
  ;; The `ido-tidy' pre-command-hook has already executed at this point.
  (self-insert-command n)
  ;; Now we fake ido's post-command for this command and the pre-command
  ;; for a second command, so that we can call `ido-complete' as well.
  (ido-exhibit) ;; post-command
  (ido-tidy) ;; pre-command-hook
  ;; Now we perform the auto-complete, leaving the original `ido-exhibit'
  ;; post-command hook to finish up after us.
  (ido-complete))

(provide 'auto-completing-read)
