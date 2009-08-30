;;;


(defgroup adc nil
  "Browse Apple's documentation"
  :group 'unix
  :group 'tools)

(defcustom adc-path "/Developer/ADC Reference Library/"
  "ADC Reference Library path"
  :type 'string
  :group 'adc)

(defvar *adc-browse-command* 'w3m-goto-url)


(defun adc-browse (class method)
  "Look up METHOD of CLASS in the ADC Reference Library"
  (interactive "MClass name: 
MMethod of class %s: ")
  (let* ((foundation-path (concat adc-path "documentation/Cocoa/Reference/Foundation/Classes/"))
	 (filename (format "%s%s_Class/Reference/%s.html" foundation-path class class)))
    (cond ((file-exists-p filename) 
	   (message "Browse URL: %s" (format "file://%s#//apple_ref/occ/instm/%s/%s" filename class method))
	   (apply *adc-browse-command* (list (format "file://%s#//apple_ref/occ/instm/%s/%s" filename class method))))
	  (t

	   (let* ((appkit-filename (format "%s%s_Class/Reference/Reference.html" (concat adc-path "documentation/Cocoa/Reference/ApplicationKit/Classes/") class)))
	     (if (file-exists-p appkit-filename) 
		 (apply *adc-browse-command* (list (format "file://%s#//apple_ref/occ/instm/%s/%s" appkit-filename class method)))
	       
	       (apply *adc-browse-command* (list (format "file:////Developer/ADC Reference Library/documentation/Cocoa/Reference/Foundation/Classes/%s_Class/Reference/Reference.html#//apple_ref/occ/instm/%s/%s" class class method)))
))))))

