;;; markerpen.el
;; - allows you to colour and highlight arbitrary sections of buffers.
;;
;; This can be useful when looking through complex code etc. Also the invisible pen
;; is useful for clearing away bits of information which are uninteresting.
;;
;; Run 'M-x markerpen-show-all-pens' to get an idea of what effects you can get
;;
;; The main interface is via the 'M-x markerpen-mark-region' - see documentation on that.
;; The markerpens are all implemented using overlays.
;;
;; Let me know of any comments, suggestions, problems - but there are no
;; guarantees and use of this code is at your own risk.
;;
;; Ben Moseley 2005 - ben@moseley.name

(defvar markerpen-current-pen 1)

(global-set-key (kbd "M-C-,") 'markerpen-mark-region) ; Use this to apply a markerpen
(global-set-key (kbd "M-C-'") 'markerpen-clear-all-marks)

(defconst markerpens '())

;; Background colour marker pens - these can be used nicely together with font-lock
(defconst markerpen-initial-pen-colours '("dark red" ; markerpen1
					  "gray24"   ; markerpen2
					  "gold4"    ; markerpen3
					  "navy"     ; markerpen4
					  ))
;; More general face-adjusting pens
(defconst markerpen-initial-pen-attrs '((face . markerpen-face-a) ; markerpen5
					(face . markerpen-face-b) ; markerpen6
					(face . markerpen-face-c) ; markerpen7
					(face . markerpen-face-d) ; markerpen8
					(face . markerpen-face-e) ; markerpen9
					(face . markerpen-face-f) ; markerpen10
					))

(defface markerpen-face-a '((t :underline "red")) "MarkerPen")
(defface markerpen-face-b '((t :weight bold :slant italic)) "MarkerPen")
(defface markerpen-face-c '((t :weight bold :height 1.8 :inherit variable-pitch)) "MarkerPen")
(defface markerpen-face-d '((t :foreground "yellow")) "MarkerPen")
(defface markerpen-face-e '((t :foreground "red")) "MarkerPen")
(defface markerpen-face-f '((t :foreground "gray24")) "MarkerPen")

(defun markerpen-with-number (num)
  (intern (format "markerpen%d" num)))

(defun markerpen-new-pen (colour)
  (markerpen-new-pen-attr `(face . (:background ,colour))))

(defun markerpen-new-pen-attr (attr-value-pair)
  (let* ((new-pen-number (length markerpens))
	 (new-pen-sym (markerpen-with-number new-pen-number)))
       (setplist new-pen-sym (list (car attr-value-pair) (cdr attr-value-pair)))
       (set new-pen-sym new-pen-number)
       (add-to-list 'markerpens new-pen-sym t)
       ;; Now attach a convenience function to the symbol to switch straight to that pen
       ;; So you can do eg: M-x markerpen3 to switch the current pen to be markerpen3
       (fset new-pen-sym `(lambda () (interactive) (setq markerpen-current-pen ,new-pen-sym)))))

;; Create the default initial pens
(markerpen-new-pen-attr '(invisible . t)) ; The invisible pen should be first so that it's pen 0.
(mapcar 'markerpen-new-pen markerpen-initial-pen-colours)
(mapcar 'markerpen-new-pen-attr markerpen-initial-pen-attrs)

(make-variable-buffer-local 'markerpen-overlays)
(defun markerpen-mark-region (pen-number-raw)
  "Without an arg, applies the currently selected pen (markerpen-current-pen) to the region.
If -1 is supplied as a numeric arg then all marks are cleared from the region.
If any other numeric arg is supplied then the markerpen with that number is used.
Use 'M-x markerpen-show-all-pens' to see a list of available pens.
The pens are implelented using overlays, so they do not in anyway affect the contents
of the buffer - even if it uses text properties. Because of this you can quite happily
use markerpens together with a mode which uses fontlock.
markerpen0 is an invisible pen which can be used to temporarily remove text from display (it
still exists in the buffer and will be saved etc - it is just the display which is adjusted).
Use 'M-x markerpen-clear-all-marks' to remove all marks from a buffer."
  (interactive "P")			; raw prefix arg - so we can distinguish no-prefix-arg by 'nil'
  (let ((pen-number (prefix-numeric-value pen-number-raw)))
    (if (= -1 pen-number)
	(markerpen-clear-region)
	(let ((pen-to-use (if pen-number-raw pen-number markerpen-current-pen)))
	  (setq new-markerpen-overlay (make-overlay (region-beginning) (region-end)))
	  (deactivate-mark)		; for transient-mark-mode
	  (add-to-list 'markerpen-overlays new-markerpen-overlay)
	  (overlay-put new-markerpen-overlay 'category (markerpen-with-number pen-to-use))))))

(defun markerpen-show-all-pens ()
  "Display a buffer with samples of the markerpens in use"
  (interactive)
    (with-output-to-temp-buffer "*MarkerPens*"
      (save-excursion
	(set-buffer standard-output)
	(insert "MarkerPens - use M-x markerpen-mark-region to apply the pens\n\n")
	(insert "NB - markerpen0 in the invisible pen - it can be used to hide things temporarily\n\n")
	(push-mark (point) t t)		; Ensure mark is set in transient-mark-mode
	(mapcar 'markerpen-show-sample markerpens)
	(insert "\nNote that various markerpens can be combined (applied to the same ")
	(push-mark)
	(insert "region")
	(mapcar 'markerpen-mark-region '(2 7 5 6 8))
	(insert ")")
	)))

(defun markerpen-show-sample (pen)
  (insert (concat (symbol-name pen) "\t"))
  (push-mark)
  (insert (format "This is some Sample Text in Marker Pen %d." (symbol-value pen)))
  (markerpen-mark-region (symbol-value pen))
  (insert "\n")
  (push-mark))

(defun markerpen-clear-all-marks ()
  "Clears all markerpen 'marks' that have been made in the current buffer"
  (interactive)
  (mapcar 'markerpen-delete-overlay markerpen-overlays))

(defun markerpen-clear-region ()
  "Clears all markerpen 'marks' that have been made in the current region only"
  (interactive)
  (mapcar 'markerpen-delete-overlay (overlays-in (region-beginning) (region-end)))
  (deactivate-mark)		; for transient-mark-mode
  )

(defun markerpen-delete-overlay (overlay)
  "Deletes the supplied overlay if it's a markerpen overlay"
  (if (memq (overlay-get overlay 'category) markerpens)
      (progn
	(setq markerpen-overlays (delq overlay markerpen-overlays))
	(delete-overlay overlay))))

(provide 'markerpen)
