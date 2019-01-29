For me, using this library, the `font-lock-ignore' property protects every text property /except/ for `keymap'. Any ideas why this is?

Longer explanation: I’m trying to create clickable text in an Org dynamic block. My writer function propertizes the text with `font-lock-ignore' set to t, `keymap' set to a sparse keymap that binds mouse-2 and RET, and `follow-link' set to t, then inserts the text. If font-lock-mode is off this works fine. But if font-lock-mode is on, the text properties are all there (as confirmed by describe-text-properties) /except/ for `keymap', which is absent. Am I alone in this? Any ideas for how to do what I want? (I’m not using regular Org links because I want the text to have its original coloring, and to be able to follow the link text with RET.) Thanks!

-- TinaRussell 2019-01-29 22:34 UTC

