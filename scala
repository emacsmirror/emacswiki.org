== Intro ==
In order to use scala in emacs, you must first use the mode distributed with scala under misc/scala-tool-support/emacs/. I like to place all my emacs file under one subdirectory and then add all directories under it to the load path. So paste those emacs file in a directory called scala-mode! I then use ensime
http://aemon.com/file_dump/ensime_manual.html. Follow the instructions there to download and test out a new project, i use below in my .emacs file.

 (progn
    (cd "~/.emacs.d/src")
    (normal-top-level-add-subdirs-to-load-path)) ;;add all subdirectories of ~/.emacs.d/src to load path
  
  (setq exec-path (append exec-path (list "/home/seth/.opt/scala/bin" ))) ;;change to location of scala bin!!! necessary for comint.
  (require 'scala-mode-auto)
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (eval-after-load "scala-mode" 
   '(progn
      (define-key scala-mode-map (kbd "<f9>") 'ensime-builder-build)
      (define-key scala-mode-map (kbd "<f10>") 'ensime-inf-switch)))

ENSIME provides a very nice experience - debugging support, incremental building, and lots of other goodies. 

== Save/Compile/Run ==

This code works that i know of for download ensime_2.8.1-0.3.8.tar.gz. Who knows, internals could change later on...
Basically, F9 is bound to run which will recompile the project if necessary, if no errors it will then run what is bound to scala-run-command. To set this
you can hit F10 which will prompt you for the new command. Press enter to keep old one. If there are errors it will open debug popup window. Alot of code just copied from the files. I use this to run a main application entry, so i hit F10, type in hello.HelloWorld, and it will run that classes main function.

  (eval-after-load "scala-mode" 
    '(progn
       (define-key scala-mode-map (kbd "<f8>") 'ensime-builder-build)
       (define-key scala-mode-map (kbd "<f10>") 'scala-run-prompt)
       (define-key scala-mode-map (kbd "<f9>") 'scala-run)
       ))
  
  (setq scala-run-command "")
  
  (defun scala-run-prompt ()
    (interactive)
    (let ((command (read-from-minibuffer (concat "run command (" scala-run-command "): "))))
       
       (setq scala-run-command (if (equal command "") scala-run-command command))
       (call-interactively 'scala-run)))
   
 (defun save-all ()
  (save-selected-window   
    (mapcar
     (lambda (b)   
       (if (buffer-file-name b)
	   (progn (set-buffer b)
		  (save-buffer))))
       (buffer-list))))

    (defun scala-run ()
      "Rebuild entire project, show debugger if necessary, otherwise run project"  
      (interactive) (save-all)
      (setf (ensime-builder-changed-files (ensime-connection)) nil)
  
      (let ((change-set (ensime-builder-changed-files
                         (ensime-connection))))
        
        (message "Building...")
        (ensime-rpc-async-builder-init 'scala-run-help)
        (setf (ensime-builder-changed-files (ensime-connection)) nil)))
     
  
 (defun scala-run-help (notes-in)
  (let ((root (or (plist-get (ensime-config (ensime-connection)) :root-dir) ".")))
    (if (null notes-in)
        (progn
          (message "Build Finished")
          (save-selected-window
	    (switch-to-buffer-other-window "*scala run*")
	    (comint-mode)
	    (cd root)
	    (erase-buffer)
	    (comint-exec (current-buffer) "*scala* "
			 "scala" nil
			 (append (list "-classpath" ".") 
				 (split-string scala-run-command)))))
      (ensime-show-compile-result-buffer notes-in))))


Note: you need to set the root-dir in your .ensime file - so add :root-dir = "/path/"
Note: Hit F10 to set the run command which will run when you hit F9. If you have jars (in your root directory), you need to enter something like this:
-classpath .:*.jar core.Test
where Test is your class you want to run (actually, it is usually an object which extends application). 


object Test extends Application {
  println("hello")
}
