= Introduction =
execute.el is a simple library that gives interactive commands for executing new asynchronous process. This means in simple words, gives commands for executing /more than one/ external programs without stopping the use of emacs.

This very basic behaviour is not possible using the `##async-shell-command##' or similars because you can execute only one process. As soon as you try to execute another one you will be asked to delete the last process or give up on executing the new one.

= Usage =
This library redefines the `##M-!##' keymap. So you have to type that keys in order to execute a command.

It has an autocompletion feature, so you can type a part of a command name and press TAB for autocomplete it. In fact, is the same autocompletion of the original `##shell-command##' function.

If you want to see all the commands executed using `##M-!##' you can call the interactive function `##execute-list##'. This function list the process ID, the command line and the status of the process. More details? Use the `##proced##' command that comes by default, its more complete and has more features.

= Downloading =
You can download it from the [[Lisp:execute.el]]. Just copy the code and paste it in a new file or clic on the "download" link.

= Instaling =
Go to your .emacs file and add the following code at the end:

    (setq load-path (cons "SCRIPT_PATH" load-path))
    (require 'execute)

Where `##SCRIPT_PATH##' is the path where you have the execute.el file.

