#!/usr/bin/env bash

base_url=http://www.emacswiki.org/cgi-bin/wiki/download
required_libraries="icicles.el icicles-chg.el \
                    icicles-cmd1.el icicles-cmd2.el \
                    icicles-doc1.el icicles-doc2.el \
                    icicles-face.el icicles-fn.el icicles-mac.el \
                    icicles-mcmd.el icicles-mode.el icicles-opt.el \
                    icicles-var.el "
optional_libraries="col-highlight.el crosshairs.el hexrgb.el \
                    hl-line+.el icomplete+.el info+.el lacarte.el \
                    vline.el"
dir=icicles

if [ -d "$dir" ]; then
  typeset -i i=0
  while [ -d "${dir}_OLD$i" ]; do
    i="$i + 1"
  done
  mv "$dir" "${dir}_OLD$i"
fi

for library in $required_libraries $optional_libraries ; do
    wget -nd -P $dir ${base_url}/${library}
    # Sleep for 2 seconds so as not to overload www.emacswiki.org
    sleep 2
done

