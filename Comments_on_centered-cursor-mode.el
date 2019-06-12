To make this work correctly with visual-line-mode when scrolling up/down, replace forward-line in ccm-scroll-up and ccm-scroll-down with next-line. As the code stands now, PgUp/PgDn does not scroll correctly when lines are soft-wrapped.

-- Andy 2019-02-01 06:03 UTC


----

As of Emacs 26.1, this can probably be replaced with

{{{
(setq maximum-scroll-margin 0.5
      scroll-margin 99999
      scroll-preserve-screen-position t
      scroll-conservatively 0)
}}}

-- npostavs 2019-06-12 12:57 UTC

