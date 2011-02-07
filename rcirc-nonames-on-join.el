;;;; Notice: 
;;;;        This file is copy & paste from http://www.emacswiki.org/emacs/rcircNoNamesOnJoin
;;;;        Original Author is XavierMaillard

(provide 'rcirc-nonames-on-join)

;;;; Don't show any JOIN message
(defvar rcirc-hide-names-on-join t
  "Non-nil if nick names list should be hidden when joining a channel.")

(defadvice rcirc-handler-353 (around my-aad-rcirc-handler-353 activate)
  "Do not render NICK list on join when `rcirc-hide-names-on-join' is non-nil.
 RPL_NAMREPLY."
  (when (not rcirc-hide-names-on-join)
    ad-do-it))

(defadvice rcirc-handler-366 (around my-aad-rcirc-handler-366 activate)
  "Do not render NICK list on join when `rcirc-hide-names-on-join' is non-nil.
 RPL_ENDOFNAMES."
  (when (not rcirc-hide-names-on-join)
    ad-do-it))

(defadvice rcirc-handler-JOIN (around my-before-ad-rcirc-handler-join-no-names activate)
  "Set `rcirc-hide-names-on-join' to `t'."
  ad-do-it
  (setq rcirc-hide-names-on-join t))

(defadvice rcirc-cmd-names (before my-ad-rcirc-cmd-names-no-list activate)
  "Reset rcirc-hide-names-on-join to nil after the JOIN step."
  (setq rcirc-hide-names-on-join nil))
