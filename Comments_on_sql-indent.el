Any idea how to turn _off_ indenting for closing semicolons?
E.g. if typing:
SELECT *
FROM foo
;

When on the lone semicolon line, the default behavior is to indent the semicolon.
This then affects text in the buffer below the semicolon, e.g. comments preceding the next SQL statement are then (incorrectly) indented to match the semicolon.

-- mmuurr 2015-02-03 18:24 UTC

