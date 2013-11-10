[[http://scala-lang.org|Scala]] is a functional programming language emerging as a popular upgrade path for Java programmers.

== Packages ==
There are 2 ScalaMode packages, Scala Mode is available in [[Marmalade]] and [[https://github.com/hvesalai/scala-mode2|Scala2 mode]] is available only in [[Melpa]] right now.

Scala2 mode includes [[http://www.scala-sbt.org/|SBT]] support, syntax highlighting and indenting support.

== Older instructions == 
In order to use scala in emacs, you must first use the mode distributed with scala under misc/scala-tool-support/emacs/. I like to place all my emacs file under one subdirectory and then add all directories under it to the load path (see below emacs code for this!).  So paste those emacs file in a directory called scala-mode!

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

    (eval-after-load "scala-mode" 
      '(progn
         (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
         (define-key scala-mode-map (kbd "<f9>") 'scala-run)
         (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
         ))

   (defun scala-run () 
      (interactive)   
     (ensime-sbt-action "run")
     (ensime-sbt-action "~compile")
 (let ((c (current-buffer)))
      (switch-to-buffer-other-window
     (get-buffer-create (ensime-sbt-build-buffer-name)))
   (switch-to-buffer-other-window c))) 
   (setq exec-path
          (append exec-path (list "~/.opt/scala/bin"))) ;;REPLACE THIS with the directory of your scalac executable!


I then use ensime http://aemon.com/file_dump/ensime_manual.html and sbt http://code.google.com/p/simple-build-tool/wiki/Setup. 

Next step: make sure you can get sbt to setup your project (see sbt docs).

Ok, so create a random file in the source directory created by sbt that contains some random code.
Run ensime-config-project, follow instructioons, etc. It should suggest that the project is of type sbt.
Now, run the command 'ensime' - it should connect with ensime. Now, type C-c C-v s, this will start the scala sbt process. I suggest opening it in another frame by using 'C-x 5 2'. 

 
And you can now hit f9 to run your code - ok, so you have to hit it twice for some reason!

OK - so good libraries to use with emacs - 

ido.el - quickly switch buffers

ibuffer - replacement for C-x C-b - you can create a 'scala' group, and then switch between your project files easily, just as if you were using , say , a eclipse project directory. 
