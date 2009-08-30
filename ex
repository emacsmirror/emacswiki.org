ex allows you to add functions not written in elisp. It accomplishes this by manually invoking the interpreter of the foreign language.

*Installing 

mkdir ~/.ex/

save this as a file and load it
<pre>
;===start of ex.el file=============

;this code written by Timothy Hobbs whon is tim.thelion at gmail.com and timthelion
(defun save-string (string file-name) "saves a string to a file"
  (with-temp-file file-name (insert-string string)))

(defun ex-defun (name args docstring executer code)
  "Makes a function written in an externally parsed language. ex,                                                                                  
  (ex-defun \"hello-world\" (name)                                                                
  \"print hi.\"                                                                                   
  \"python \"                                                                                     
  \"                                                                                              
   import sys                                                                                     
   print \\\"hi\\\"                                                                               
   for arg in sys.argv:                                                                           
         print arg\") ;returns hi\narg0\narg1..."
  (save-string code (concat "~/.ex/" name))
  (fset (intern name) (lambda (args) docstring (shell-command-to-string (concat executer "~/.ex/"\
 name " " args)))))
;==========end of ex.el file=============

*running
example code.

(ex-defun "hi" "" "print hi" "python " "print \"hi there\"")

(insert (hi "hi"))


</pre>
