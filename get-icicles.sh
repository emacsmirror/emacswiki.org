#!/usr/bin/env bash

base_url=http://www.emacswiki.org/emacs/download
required_libraries="icicles.el \
 icicles-cmd1.el icicles-cmd2.el \
 icicles-face.el icicles-fn.el icicles-mac.el \
 icicles-mcmd.el icicles-mode.el icicles-opt.el \
 icicles-var.el "
optional_libraries="col-highlight.el crosshairs.el \
 doremi.el ring+.el hexrgb.el hl-line+.el \
 icicles-chg.el icicles-doc1.el icicles-doc2.el \
 icomplete+.el lacarte.el \
 synonyms.el vline.el"
dir=icicles
libraries="$required_libraries $optional_libraries"

usage()
{
  cat << EOF
usage: `basename $0` <options>

This script downloads Icicles from Emacs Wiki.

  e.g. `basename $0` -r -d . -n

downloads only the required libraries (-r) into the current directory (-d .), without backup (-n).

OPTIONS:
   -h          Show this message
   -r          Get only required libraries.
   -d <dir>    Specify different directory name prefix, default is "icicles"
   -o          Get only optional libraries.
   -n          Don't attempt to create backups.
   -v          Be verbose.
EOF
}

while getopts “h?rd:onv” OPTION
do
  case $OPTION in
    h)
      usage
      exit
      ;;
    \?)
      usage
      exit
      ;;
    r)
      libraries=$required_libraries
      ;;
    d)
      dir=$OPTARG
      ;;
    o)
      libraries=$optional_libraries
      ;;
    n)
      no_backup=t
      ;;
    v)
      verbose=t
      ;;
  esac
done

if [ -z "$no_backup" -a -d "$dir" ]; then
  typeset -i i=0
  while [ -d "${dir}_OLD$i" ]; do
    i="$i + 1"
  done
  mv "$dir" "${dir}_OLD$i"
fi

for library in $libraries; do
  [ $verbose ] && echo downloading: $library ...
  wget -nd -P $dir ${base_url}/${library}
  # Sleep for 2 seconds so as not to overload www.emacswiki.org
  sleep 2
done

# Additional optional libraries that Icicles can take advantage of:
# apropos-fn+var.el
# bookmark+.el
# dired+.el
# doremi-frm.el
# ffap-.el
# fit-frame.el
# fuzzy-match.el
# info+.el
# linkd.el
# menu-bar+.el
# misc-cmds.el
# palette.el
# pp+.el
# thingatpt+.el
# wid-edit+.el
