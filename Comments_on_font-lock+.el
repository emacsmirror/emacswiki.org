For me, using this library, the `font-lock-ignore' property protects every text property /except/ for `keymap'. Any ideas why this is?

Longer explanation: I’m trying to create clickable text in an Org dynamic block. My writer function propertizes the text with `font-lock-ignore' set to t, `keymap' set to a sparse keymap that binds mouse-2 and RET, and `follow-link' set to t, then inserts the text. If font-lock-mode is off this works fine. But if font-lock-mode is on, the text properties are all there (as confirmed by describe-text-properties) /except/ for `keymap', which is absent. Am I alone in this? Any ideas for how to do what I want? (I’m not using regular Org links because I want the text to have its original coloring, and to be able to follow the link text with RET.) Thanks!

-- TinaRussell 2019-01-29 22:34 UTC


----

@TinaRussell:

Dunno. There's no explicit occurrence of `keymap' in the code. What happens if you do the same or similar without the library (without `font-lock-ignore')? Maybe show some code. Also, try using `debug-on-entry': walk through the debugger for some function to see just what goes on. Try to simplify what you do, just for testing what's going on with the text properties. E.g., maybe take Org out of the loop, for now.

-- DrewAdams 2019-01-30 15:55 UTC


----

@DrewAdams:

Aha! I’ve found the culprit. The issue is that Org has its own unfontify-region function, `org-unfontify-region'…

<pre>
(defun org-unfontify-region (beg end &optional _maybe_loudly)
  "Remove fontification and activation overlays from links."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 deactivate-mark buffer-file-name buffer-file-truename)
    (decompose-region beg end)
    (remove-text-properties beg end
			    '(mouse-face t keymap t org-linked-text t
					 invisible t intangible t
					 org-emphasis t))
    (org-remove-font-lock-display-properties beg end)))
</pre>

…and it doesn’t have a redefinition in font-lock+.el. Do not fear, however! I have written one:

<pre>
(defun org-unfontify-region (beg end &optional _maybe_loudly)
  "Remove fontification and activation overlays from links.
Ignore text with property `font-lock-ignore'."
  (font-lock-default-unfontify-region beg end)
  (let* ((buffer-undo-list t)
	 (inhibit-read-only t) (inhibit-point-motion-hooks t)
	 (inhibit-modification-hooks t)
	 (here (min beg end))
	 (end1 (max beg end))
	 deactivate-mark buffer-file-name buffer-file-truename chg)
	(decompose-region here end1)
	(while (< here end1)
	  (setq chg  (next-single-property-change here 'font-lock-ignore nil end1))
	  (unless (get-text-property here 'font-lock-ignore)
		(remove-text-properties here chg
					'(mouse-face t keymap t org-linked-text t
						 invisible t intangible t
						 org-emphasis t)))
		(org-remove-font-lock-display-properties here chg))
	  (setq here  chg))))
</pre>

Now my link-text dynamic block thing works! Hooray!

-- TinaRussell 2019-02-20 07:13 UTC

