;;;----------------------------------------------------------------------
;; submerge.el
;; Copyright (C) 2009 Mike Mattie
;; License: LGPL-v3
;;;----------------------------------------------------------------------

;; This is currently a proof of concept.

(defconst submerge-version "0.0.1")

(require 'record-processor)
(require 'bzr-ui)

;; need an ignore which makes a empty commit that records the change as
;; merged.

;; hitting the m key repeatedly when conflicted moves to the next conflict.
;; r is used to mark the situation resolved.

(def-sparse-map subm-overlay-map
  "submerge merge overlay map"
  ("d" 'subm-diff-change)
  ("c" 'subm-commit-change)
  ("m" 'subm-merge-change)
  ("n" 'subm-next)
  ("p" 'subm-prev))

(defface subm-revision-face
  '((t :weight bold))
  "The submerge face for the commit revision identifier.")

(defface subm-selected-face
  '((t :foreground "khaki3"))
  "Face for Info node names.")

(defface subm-unselected-face
  '((t :foreground "grey70"))
  "The submberge face for a not selected face who's state has not changed.")

(defface subm-error-face
  '((t :foreground "firebrick1"))
  "The submberge face for a commit for a conflicted merge.")

(defface subm-merged-face
  '((t :foreground "sea green"))
  "The bzr-pick face for a commit already merged.")

(defface subm-conflicted-face
  '((t :foreground "orange1"))
  "The bzr-pick face for a commit that was conflicted.")

(defvar subm-before-merge-hook nil
  "Hooks run in the merge buffer after it has been setup but before the interface is presented.")

(defcustom bzr-pick-auto-advance t
  "When this option is t (the default) a successful merge and commit advances to the next change.")

(defcustom subm-auto-commit t
  "When this option is t each succesful merge is commited with a
   log message consisting of the revision id.  Turning this
   option off allows the user to coallesce several merges into a
   commit. Using auto commit is *Strongly Advised* as committing
   more than one merge is essentially a ham-handed rebase. When a
   task branch with multi-merge commits is merged there will be
   an large number of conflicts whereever this multi-merge commit
   propogates.
  ")

;;TODO:
;; auto-advance needs to recenter the window on the line. (recenter-top-bottom &optional ARG)

;;----------------------------------------------------------------------
;; struct ADT
;;----------------------------------------------------------------------

(defun subm-face-for-status ( status selected )
  (cond
    ((equal 'merged status)      'subm-merged-face)
    ((equal 'error status)       'subm-error-face)
    ((equal 'conflicted status)  'subm-conflicted-face)
    ((not status)
      (if selected
        'subm-selected-face
        'subm-unselected-face)) ))

(defun subm-make-overlay ( start end status)
  "subm-make-overlay START END

  Create a new overlay from START to END.
  "
  (let
    ((overlay (make-overlay start end)))

    (overlay-put overlay 'keymap   subm-overlay-map)
    (overlay-put overlay 'face     (subm-face-for-status status nil))
    (overlay-put overlay 'priority 0)

    overlay))

(defconst subm-struct-len        6 "the length of the commit structure")

(defconst subm-field-overlay     0 "the overlay for the commit")

(defconst subm-field-log-offset  1 "where the log starts relative to the beginning of the overlay")
(defconst subm-field-revid       2 "the revision identifier")
(defconst subm-field-dt          3 "the date and time string")
(defconst subm-field-files       4 "a list of files changed")
(defconst subm-field-status      5 "the status of the commit")


(defun subm-make-struct ( start
                          end
                          rev-id
                          date/time
                          files
                          &optional status )
  "subm-commit-struct  >see defun for parameters<

   Create a new commit structure starting with the revision id
   and the range of the commit in the log
  "
  (let
    ((struct (make-vector subm-struct-len nil)))

    (aset struct subm-field-overlay (subm-make-overlay start end status))

    (aset struct subm-field-log-offset 0)

    (aset struct subm-field-revid rev-id)
    (aset struct subm-field-dt    date/time)
    (aset struct subm-field-files files)

    (aset struct subm-field-status status)

    struct))

;;
;; overlay, start,end, and log
;;

(defun subm-struct-overlay ( struct )
  (aref struct subm-field-overlay))

(defun subm-struct-start ( struct )
  (overlay-start (aref struct subm-field-overlay)))

(defun subm-struct-end ( struct )
  (overlay-end (aref struct subm-field-overlay)))

(defun subm-struct-log ( struct )
  (+ (overlay-start (aref struct subm-field-overlay))
     (aref struct subm-field-log-offset)))

;;
;; get/set status
;;

(defun subm-struct-status ( struct )
  (aref struct subm-field-status))

(defun subm-struct-set-status ( struct status )
  (aset struct subm-field-status status))


;;
;; accessors for information extracted from the log entry.
;;

(defun subm-struct-revid ( struct )
  (aref struct subm-field-revid))

(defun subm-struct-date/time ( struct )
  (aref struct subm-field-dt))

(defun subm-struct-files ( struct )
  (aref struct subm-field-files))

;;----------------------------------------------------------------------
;; log analysis
;;----------------------------------------------------------------------

;; the record delimiter is a minimum 50 char repeat
(defconst subm-log-record-delimiter "^--------------------------------------------------+\n")

(defun subm-find-next-line ( &optional bound )
  (search-forward-regexp "\n" bound t))

(defun subm-record-extractor ( start end )
  "sub-find-next-record

   Find the beginning of the next record in the log. When no
   further records are found return nil.

   The data is the beginning of the commit.
  "
  (catch 'abort
    (let
      ((change-start (search-forward-regexp subm-log-record-delimiter end t)))

      (unless change-start (throw 'abort nil))

      (record-processor-output
        change-start
        (if (search-forward-regexp subm-log-record-delimiter end t)
          (progn
            (search-backward-regexp subm-log-record-delimiter start t)
            (point))
          end)) )))

(defun subm-revid-extractor ( start end )
  "subm-find-revid

   Find the revision ID marker in the current commit, and return
   the ID.
  "
  (when (search-forward-regexp "^revno:\\ *\\(.+\\)$" end t)
    (record-processor-output (point) (match-end 0) (match-string 1))))

(defun subm-date/time-extractor ( begin end )
  "subm-date/time-extractor

   Find the date and time of the current commit.
  "
  (when (search-forward-regexp "timestamp:\\ *\\(.+\\)$" end t)
    (record-processor-output (point) (match-end 0) (match-string 1)) ))

(defconst subm-log-changed-delimiter "^\\(removed\\|modified\\|added\\|renamed\\):")

(defun subm-logentry-extractor ( begin end )
  "subm-logentry-extractor

   Find the Log Entry of the current commit. a cons of the start
   and end is returned.
  "
  (when (search-forward-regexp "message:\\W*\n+" end t)
    (let
      ((start (point)))

      (while (not (or
                    (looking-at subm-log-changed-delimiter)
                    (looking-at subm-log-record-delimiter)))
        (subm-find-next-line end))

      (record-processor-output start (point)) )))

(defun subm-submerge-record-extractor ( begin end )
  "subm-submerge-record-extractor

   Find the Log Entry of the current commit. a cons of the start
   and end is returned.
  "

  (if (search-forward-regexp "bzr-pick\\ merge\\ revid\\ *\\(.+\\)$" end t)
    (record-processor-output begin (point) (match-string 1))
    'drop))

;; (defun subm-find-changed-files ( bound )
;;   (when (search-forward-regexp (concat subm-log-changed-delimiter "\n") bound t)
;;     (catch 'done
;;       (let
;;         ((modified-list nil))

;;         (while (not (looking-at subm-log-record-delimiter))
;;           (if (looking-at subm-log-changed-delimiter)
;;             (subm-find-next-line bound)
;;             (progn
;;               (if (search-forward-regexp "^\\W+\\(.*\\)[\\ \\t]*\n" bound t)
;;                 (setq modified-list (cons (match-string 1) modified-list))
;;                 (throw 'done modified-list) )) )) ))))

;;----------------------------------------------------------------------
;; log processing
;;----------------------------------------------------------------------

(defun subm-process-already-merged ()
  "subm-process-already-merged

   Return a list of all the revision id's that have already been merged.
  "
  (goto-char (buffer-end -1))

  (let
    ((end (buffer-end 1))
     (merged-list nil))

    (while (search-forward-regexp "\\(bzr-pick\\ merge\\ revid\\|submerge\\ revid\\)\\ +\\(\\w+\\)" end t)
      (push (match-string 2) merged-list))

    merged-list))

(defun subm-process-changelog ( previous-merges )
  "subm-process-changelog

   Process the log in the current buffer from the point to the end
   building data for commit structures out of the traversal.  a
   list of commit structure data is returned.
  "
  (let
    ((change-start  (record-processor-lookup "start"))
     (change-end    (record-processor-lookup "end"))
     (change-revid  (record-processor-lookup "revid->data")) )

    (mapcar (lambda ( change )
              (subm-make-struct
                (funcall change-start change)
                (funcall change-end   change)
                (funcall change-revid change)
                nil
                nil
                (when (member (funcall change-revid change) previous-merges)
                  'merged)) )
        (record-processor (buffer-end -1) (buffer-end 1)
          (record-processor-make-extractor "commit" 'subm-record-extractor
            (record-processor-make-extractor "revid" 'subm-revid-extractor))) ) ))

;;----------------------------------------------------------------------
;; merge buffer setup
;;----------------------------------------------------------------------

(defun subm-make-change-array ( change-list )
  "subm-make-change-array CHANGE-LIST

   Given a CHANGE-LIST of change structures create and return a vector of the
   changes The size of the vector must be known at creation time
   so this function constructs the array after the length is known.
  "
  (if change-list
    (let
      ((change-array  (make-vector (length change-list) nil))
        (i 0))

      (mapc
        (lambda ( change )
          (aset change-array i change)
          (setq i (+ i 1)))
        change-list)

      change-array)
    nil))

(defun subm-setup-merge-buffer ( previous-merges )
  "subm-setup-merge-buffer

   With the current buffer scan it and setup.
  "
  (remove-overlays)

  (set (make-local-variable 'change-array) (subm-make-change-array (subm-process-changelog previous-merges)))

  (when change-array
    (set (make-local-variable 'selected-change) nil)
    (subm-set-change-current 0)
    t))

;;----------------------------------------------------------------------
;; display.
;;----------------------------------------------------------------------


(defun subm-set-change-current ( current )
  "subm-set-change-current

   Set the change overlay as current by setting the previous
   selected overlay if any back to it's status computed face, and
   set the new current overlay's face by both status and
   selected.
  "
  (when selected-change
    (let
      ((old-change (aref change-array selected-change)))

      (overlay-put (subm-struct-overlay old-change)
                    'face
                    (subm-face-for-status (subm-struct-status old-change) nil))))

  (setq selected-change current)

  (let
    ((new-change (aref change-array current)))

    (overlay-put (subm-struct-overlay new-change)
                 'face
                 (subm-face-for-status (subm-struct-status new-change) t))

    (goto-char (subm-struct-log new-change))) )

;;----------------------------------------------------------------------
;; interface commands
;;----------------------------------------------------------------------

(defun subm-traverse ( wrap-bound wrap-start next in-bounds )
  (let
    ((previous selected-change)
     (i nil)
     (wrapped-around nil))

    (catch 'done
      ;; handle the selected-change unitialized case.
      (unless previous
        (setq i 0)
        (throw 'done t))

      ;; initialize i to current-commit. It is called previous
      ;; so we can detect motion.
      (setq i previous)

      ;; allow it to wrap around _once_
      (while (not wrapped-around)
        (when (funcall wrap-bound i)
          (setq wrapped-around t)
          (setq i wrap-start))

        ;; move it before the traversal iteration so that we don't end
        ;; up moving beyond the end of the array after the end of array
        ;; test.
        (setq i (funcall next i))

        ;; the traversal iteration.
        (while (funcall in-bounds i)
          (let
            ((change-status (subm-struct-status (aref change-array i))))

            ;; we are looking for nil, or error statuses which indicate that
            ;; there is something to do.
            (cond
              ((not change-status)          (throw 'done t))
              ((equal 'error change-status) (throw 'done t))) )

          (setq i (funcall next i)) )))

    (if (equal previous i)
      (progn
        (message "No unmerged or failed commits remaining")
        nil)
      (progn
        (subm-set-change-current i)
        t)) ))

(defun subm-traverse-forward-wrap-bound ( i )
  (when (>= i (length change-array)) t))

(defun subm-traverse-forward-in-bounds ( i )
  (when (< i (length change-array)) t))

(defun subm-traverse-forward-next ( i ) (+ i 1))

(defconst subm-traverse-forward-wrap -1)

(defun subm-next ()
  (interactive)
  (subm-traverse
    'subm-traverse-forward-wrap-bound
    subm-traverse-forward-wrap
    'subm-traverse-forward-next
    'subm-traverse-forward-in-bounds))

(defun subm-traverse-backward-wrap-bound ( i ) (when (< i 0) t))

(defun subm-traverse-backward-in-bounds ( i ) (when (>= i 0) t))

(defun subm-traverse-backward-next ( i ) (- i 1))

(defun subm-prev ()
  (interactive)
  (subm-traverse
    'subm-traverse-backward-wrap-bound
    (length change-array)
    'subm-traverse-backward-next
    'subm-traverse-backward-in-bounds))

(defun subm-diff-load ( revid )
  (let*
    ((buffer-name (format "%s:diff[%s]*" left-name revid))
     (diff-buffer (get-buffer-create buffer-name))

     ;; do the diff in the tip where we are merging from
     (default-directory right-path)
     (status (vc-bzr-command "diff" diff-buffer t nil "-c" revid)))

    ;; add diff buffers to a list so we can later kill'em all
    (setq diff-buffers (add-to-list 'diff-buffers buffer-name))

    ;; The exit status is atypical.
    ;; 1 = changes
    ;; 2 = unrepresentable changes
    ;; 3 = error
    ;; 0 = no changes.
    (with-current-buffer diff-buffer
      (if (equal 1 status)
        (progn
          (diff-mode)
          diff-buffer)

        (progn
          (set-buffer-modified-p nil)
          (kill-buffer diff-buffer)
          nil))) ))

(defun subm-diff-change ()
  (interactive)
  (let
    ((diff-buffer (subm-diff-load
                    (subm-struct-revid (aref change-array selected-change)))))

    (if diff-buffer
      (progn
        (display-buffer diff-buffer t)
        t)
      (progn
        (message "could not retrieve diff")
        nil)) ))

(defun subm-cleanup-diff-buffers ()
  (mapc
    (lambda (buffer-name)
      (let
        ((buffer-object (get-buffer buffer-name)))

        (when (bufferp buffer-object)
          (with-current-buffer buffer-object (set-buffer-modified-p nil))
          (kill-buffer buffer-object)) ))

    diff-buffers)
  (setq diff-buffers nil))

(defun subm-commit-change ( struct )
  (let
    ((change-status (vc-bzr-command "commit" (current-buffer) t
                      nil "-m" (format "submerge revid %s" (subm-struct-revid struct)))))

    (cond
      ((equal change-status 0) 'merged)
      (t 'error)) ))

(defun subm-merge-change ()
  (interactive)
  (let
    ((default-directory left-path)
     (bzr-status nil)
     (struct (aref change-array selected-change)))

    ;; make sure the point is positioned at the start
    ;; of the log info.
    (goto-char (subm-struct-log struct))

    ;; Attempt the merge.
    (setq bzr-status
      (apply 'vc-bzr-command "merge" (current-buffer) t
        (seq-filter-nil
          right-path
          (unless subm-auto-commit "--force")
          "-c"
          (subm-struct-revid struct)) ))

    ;; set the status based on exit value. The exit values are not well
    ;; documented apparently.

    (when (equal 'merge
            (subm-struct-set-status struct
              (cond
                ((equal 0 bzr-status)
                  ;; when auto-commit is turned on the status is set
                  ;; on the result of the commit as well.
                  (when subm-auto-commit (subm-commit-change struct)))
                ((equal 3 bzr-status)
                  (progn
                    ;; when we run into errors it is likely uncommitted changes.
                    ;; a good thing would be running a bzr status here.
                    (message "Merge Error!")
                    'error))
                (t (progn
                     (message "Merge Conflict!")
                     'conflicted)))))
      (when subm-auto-advance (subm-next-change)) )))

(defun submerge ()
  (interactive)
  (let*
    ((right-branch  (bzr-prompt-for-branch "Merge from Branch ?"))
     (left-branch   (bzr-prompt-for-branch "Merge to Branch ?"
                      (bzr-find-repository-top right-branch)))
     (merged-list   nil))

    (catch 'abort
      (unless (vc-bzr-root right-branch)
        (message "the branch to merge from (the tip) is not a bzr branch")
        (throw 'abort nil))

      (unless (vc-bzr-root left-branch)
        (message "the branch to merge to (left side) is not a bzr branch")
        (throw 'abort nil))


      (let
        ((left-relative  (make-branch-path-repository-relative left-branch))
         (right-relative (make-branch-path-repository-relative right-branch)))

        (message "scanning the target branch %s for previous commits" left-relative)

        ;; scan the target branch log for commits already merged.

        (let
          ((default-directory left-branch)
           (target-log-buffer (get-buffer-create (concat left-relative ":log"))))

          (message "loading the target branch log: %s" left-relative)

          (vc-bzr-command "log" target-log-buffer t "-v")

          (message "scanning the target branch log for previous merges: %s" left-relative)

          (setq merged-list (with-current-buffer target-log-buffer
                              (subm-process-already-merged)))
          (kill-buffer target-log-buffer))

;;        (message "already merged list %s" (princ merged-list))

        ;; scan the tip branch for changes outstanding

        (let
          ((default-directory right-branch)
           (merge-select-buffer (get-buffer-create (concat left-relative ":submerge"))))

          (with-current-buffer merge-select-buffer
            (set (make-local-variable 'left-path)  left-branch)
            (set (make-local-variable 'right-path) right-branch)

            (set (make-local-variable 'left-name)  left-relative)
            (set (make-local-variable 'right-name) right-relative)

            (set (make-local-variable 'diff-buffers) nil)

            ;; load the outstanding changes into the buffer
            (message "loading the log of changes between %s -> %s" right-relative left-relative)

            (erase-buffer)
            (vc-bzr-command "missing" merge-select-buffer t left-path "-v")

            ;; get rid of misc bzr output

            (message "setting up the buffer for a merge %s -> %s" right-relative left-relative)

            (goto-char (buffer-end -1))
            (when (search-forward-regexp "You\\W+are\\W+missing\\W+.*revision.*:" nil t)
              (search-backward-regexp "^" nil t)
              (delete-region (point) (buffer-end 1)))

            ;; setup the merge buffer
            (unless (subm-setup-merge-buffer merged-list)
              (message "No unmerged commits could be found !")
              (throw 'abort nil))

            ;; hooks
;;          (run-hooks 'subm-before-merge-hook)

            ;; add a buffer local hook to clean up the diff buffers
            (add-hook 'kill-buffer-hook 'subm-cleanup-diff-buffers t t))

          (pop-to-buffer merge-select-buffer t) ))
      t)))

(provide 'submerge)
