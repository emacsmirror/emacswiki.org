;;;

(defun xcode-compile (directory build-action)
  "Enhanced bh-compile function by Brett Hutley"
  (interactive
   (list (read-directory-name "Directory name: " nil default-directory nil)      
	 (read-from-minibuffer "Build action (default build): " nil nil t nil "build" nil)))
  (cd directory)
  (let ((df (directory-files "."))
        (has-proj-file nil))
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t))))
      (setq df (cdr df)))
    (if has-proj-file
	;; TODO configuration
        (compile (format "xcodebuild -configuration Debug %s" build-action))
      (compile "make"))))
