Although cdb-gud is nice it fails to show source and callstack when using with kd.exe
Also gud mangles syntax for connection to remote host.

Here is my hacked version of cdb-gud that resolves these issues with kd.exe

https://github.com/4DA/kd-gud

----

Running cdb-gud.el as currently in the wiki causes constant failures due to bugs in the auto-complete implementation.

Unless this bug is fixed, you will have to disable auto-complete-mode after triggering M-x cdb.

-- josteink 2015-10-01 09:04 UTC

