;;; inhibit-clash-detection.el --- suppress lockfiles
;;; Author: Daniel Colascione <dan.colascione@gmail.com>

;;; Commentary: Implement XEmacs' inhibit-clash-detection for GNU
;;              Emacs

;;; Code:

(defcustom inhibit-clash-detection nil
  "*Non-nil means don't detect others editing the same file,
though we'll still detect that situation when a file is saved"
  :group 'files
  :type 'boolean)

(defadvice lock-buffer (around inhibit-clash-detection
                               preactivate activate)
  "Make LOCK-BUFFER obey inhibit-clash-detection"
  (if inhibit-clash-detection
      nil
    ad-do-it))

(defadvice unlock-buffer (around inhibit-clash-detection
                                 preactivate activate)
  "Make UNLOCK-BUFFER obey inhibit-clash-detection"

  (if inhibit-clash-detection
      nil
    ad-do-it))

(defadvice file-locked-p (around inhibit-clash-detection
                                 preactivate activate)
  "Make FILE-LOCKED-P obey inhibit-clash-detection"

  (if inhibit-clash-detection
      nil
    ad-do-it))

(provide 'inhibit-clash-detection)

;;; inhibit-clash-detection.el ends here

