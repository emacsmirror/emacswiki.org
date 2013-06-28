The version 0.9 (or 20120505.1234 from MELPA) doesn't work for me. In some files (e.g. outline-magic.el itself), when the point is at the beginning of the buffer and I run `outline-cycle', the whole buffer gets hidden. Subsequent runs of `outline-cycle' fail and everything remains hidden. The failure is caused by (error "before first heading") called from `outline-back-to-heading`.

I run GNU Emacs 24.3.1 (x86_64-pc-linux-gnu, GTK+ Version 3.4.2) of 2013-04-13 on trouble, modified by Debian.

This patch fixes the problem.

<pre>
diff --git a/outline-magic.el b/outline-magic.el
index 52ca5cf..81331c5 100644
--- a/outline-magic.el
+++ b/outline-magic.el
@@ -266,7 +266,13 @@ them set by set, separated by a nil element.  See the example for
        (setq this-command 'outline-cycle-showall))
        (t
        ;; Default action: go to overview
-       (hide-sublevels 1)
+       (let ((toplevel (cond
+                        (current-prefix-arg (prefix-numeric-value current-prefix-arg))
+                        ((save-excursion (beginning-of-line)
+                                         (looking-at outline-regexp))
+                         (max 1 (funcall outline-level)))
+                        (t 1))))
+         (hide-sublevels toplevel))
        (message "OVERVIEW")
        (setq this-command 'outline-cycle-overview))))
</pre>

-- [http://https://rtime.felk.cvut.cz/~sojka/ Anonymous] 2013-06-28 16:02 UTC

