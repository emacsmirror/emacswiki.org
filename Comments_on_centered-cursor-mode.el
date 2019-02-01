To make this work correctly with visual-line-mode when scrolling up/down, replace forward-line in ccm-scroll-up and ccm-scroll-down with next-line. As the code stands now, PgUp/PgDn does not scroll correctly when lines are soft-wrapped.

-- Andy 2019-02-01 06:03 UTC

