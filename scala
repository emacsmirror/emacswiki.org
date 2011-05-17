== Intro ==
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

 I then use ensime http://aemon.com/file_dump/ensime_manual.html and sbt http://code.google.com/p/simple-build-tool/wiki/Setup. 

Next step: make sure you can get sbt to create a project folder! I personally took the linux script from the library scalala, which also comes with sbt ....

Now, we need to modify exec-path so that ensime can find its .sh script, and also so that it can find the scala executable. Something like this in .emacs should do it 

 (setq exec-path
          (append exec-path (list "~/.opt/scala/bin" "and_other_directories_needed")))

Ok, so create a random file in the source directory created by sbt that contains some random code.
Run ensime-config-project, follow instructioons, etc. It should suggest that the project is of type sbt.
Now, run ensime-sbt (or first ensime-connect) - and voila-you have an sbt prompt! Now place the following in it

 (eval-after-load "scala-mode" 
      '(progn
         (define-key scala-mode-map (kbd "<f9>") (lambda () (interactive) (ensime-sbt-action "run")))
         (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
         ))

And you can now hit f9 to automatically run your code - sbt will automatically compile if necessary, have jars on pathway, etc. Nice!

