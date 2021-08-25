Could you please add autoload cookie for info-good-fixed-pitch-font-families?
Without it, folks are running into problems as detailed at 
https://github.com/syl20bnr/spacemacs/issues/15010

The issue is that defface forms that do have autoload cookie refer to info-good-fixed-pitch-font-families.  Thus within autoload file we end up with code that use info-good-fixed-pitch-font-families which is not defined.  Thanks.

-- emacs18 2021-08-25 00:07 UTC


----

Done - please try the latest. And thanks for your comment. I had no idea that Spacemacs, or some Spacemacs users, used `info+.el'.

That missing autoload cookie was an oversight.  It's needed because an autoloaded `defface` makes use of it.

-- DrewAdams 2021-08-25 02:03 UTC


----

This is FYI only.  Spacemacs uses info+ by default for all users.  Aside from loading info+.el, spacemacs has these two lines to configure it:

      (spacemacs/set-leader-keys "hj" 'info-display-manual)
      (setq Info-fontify-angle-bracketed-flag nil)

First line binds "SPC h j" and/or "M-m h j" key sequences to info-display-manual.

Following are the key sequences that follow "SPC h" or "M-m h" prefix keys (in case you are curious).  If you can suggest other commands in info+ that we can add key bindings for, then please do.  I can submit pull request to have the bindings added.  Whether my PR request is approved or not is not up to me to say.

  RET → helm-enable-minor-mode
  SPC → helm-spacemacs-help
  . → helm-spacemacs-help-dotspacemacs
  f → helm-spacemacs-help-faq
  i → helm-info-at-point
  j → info-display-manual
  k → which-key-show-top-level
  l → helm-spacemacs-help-layers
  m → helm-man-woman
  n → view-emacs-news
  p → helm-spacemacs-help-packages
  r → helm-spacemacs-help-docs
  t → helm-spacemacs-help-toggles
  I → report-issue
  M → helm-switch-major-mode
  b → +prefix
    d → browse-docs-online-at-point
  d → +help-describe
    a → helm-apropos
    b → describe-bindings
    c → describe-char
    f → describe-function
    k → describe-key
    l → describe-last-keys
    m → describe-mode
    p → describe-package
    s → describe-system-info
    t → describe-theme
    v → describe-variable
    x → describe-ex-command
    F → helm-faces
    K → describe-keymap
    P → configuration-layer/describe-pac..
    T → describe-theme
  P → +profiler
    k → profiler-stop
    r → profiler-report
    s → profiler-start
    w → profiler-report-write-profile
  T → +tutorials
    e → emacs-tutorial
    v → evil-tutor-start

-- emacs18 2021-08-25 06:51 UTC

