[[es:sqlite.el-es]]
sqlite.el is a simple SqliteInterface for connecting and retrieving information using the [[SQLite]] program through [[Elisp]] programming. It is not intended as a user interface, for that see SqlMode.

It has been created as a temporary solution because the JasonFruit's script is not available through the link given. As his script, this is based on the code founded at the third item in this page: http://mysite.verizon.net/mbcladwell/.

You can download it from [[Elisp:sqlite.el]].

== Features ==

This interface has the following features:

* Can connect to only one database. Maybe in near future can let you have more than one connection.
* No output buffer shown. The output buffer is hidden when using the interface.

== Problems ==
The library has some problems that it will be corrected in a near future: 

# The first query gives you the sqlite welcome and version. This is the first text that sqlite program prompts when you connect. 
# There is only one connection, you can't mannage more!
# The output buffer has not been erased, just hidden.
# Error manipulation is still under development.

''The first problem is the most important one'', beacuse whe you use this library you'll have to consider it!

== API Elements ==

You can find the following elements:

Variables:

* `sqlite-db' The complete path of the database file. 

Functions:

* <code>sqlite-init</code> For starting the program. It use `sqlite-db' for opening the database file.
* <code>sqlite-query</code> For sending a query or a sqlite ".something" command.
* <code>sqlite-bye</code>. For closing the sqlite program sending a <code>.quit</code> command.

== Usage ==

Just start the program using <code>sqlite-init</code>, and then query what you want calling <code>sqlite-query</code>. When you finish using your SQLite database, close the program with <code>sqlite-bye</code>

An example of usage:

    (require 'sqlite)
    
    (let ((sqlite-db "/path/to/db/file.sqlite"))
      (sqlite-init)) ;; Starting the sqlite program 

    (sqlite-query "") ;; For correcting the first problem... this will be unnecessary in near future.
    (setq results (sqlite-query "SELECT * FROM Persona;"))

    (sqlite-bye) ;; Closing the sqlite program

Take a look at the first query, this is because of the first problem explained before: it gives the first sqlite program prompt. We don't want that so we just call <code>sqlite-query</code> with nothing for ignoring it. In near future it will be unnecessary.



----
[[SQLite]] CategorySql
