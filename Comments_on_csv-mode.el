Byte-compiling csv-mode.el under GNU Emacs version 24.3.1 gave me these results:

<pre>Compiling file c:/cygwin/home/pemeeri/.emacs.d/elpa/csv-mode-1.50/csv-mode.el at Tue Jul  8 16:42:50 2014
Entering directory `c:/cygwin/home/pemeeri/.emacs.d/elpa/csv-mode-1.50/'
csv-mode.el:149:23:Warning: error called with 0 arguments, but requires 1+
csv-mode.el:172:23:Warning: error called with 0 arguments, but requires 1+
csv-mode.el:259:40:Warning: `default-mode-line-format' is an obsolete variable
    (as of 23.2); use `mode-line-format' instead.
csv-mode.el:264:20:Warning: `default-mode-line-format' is an obsolete variable
    (as of 23.2); use `mode-line-format' instead.
csv-mode.el:268:23:Warning: `default-mode-line-format' is an obsolete variable
    (as of 23.2); use `mode-line-format' instead.

In csv-align-fields:
csv-mode.el:1044:29:Warning: assignment to free variable `left-padding'
csv-mode.el:1058:31:Warning: assignment to free variable `right-padding'
csv-mode.el:1068:52:Warning: reference to free variable `left-padding'
csv-mode.el:1121:34:Warning: reference to free variable `right-padding'
csv-mode.el:1118:40:Warning: assignment to free variable `overlay'
csv-mode.el:1124:36:Warning: reference to free variable `overlay'
csv-mode.el:1134:15:Warning: reference to free variable `end'
csv-mode.el:1134:23:Error: Invalid read syntax: ")
</pre>

-- epement 2014-07-08 21:03 UTC


----

Trying to load "csv-mode" from Elpa returns an error message:

[code]Symbol's value as variable is void: end[/code]

-- epement 2014-07-22 20:43 UTC


----

The problem I reported above is now resolved. The version of csv-mode.el that was formerly posted here was over 9 years old. After obtaining the latest version 1.2 of csv-mode.el from http://elpa.gnu.org/packages/csv-mode.html and installing that, the compile problems were resolved.

-- epement 2014-08-12 19:47 UTC

