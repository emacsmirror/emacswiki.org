#!/bin/bash

# Switch between emacs frame and its clients.  Also support piping to emacs.

EMACS_CLIENT=${EMACS_CLIENT:-emacsclient}
# The server to use.
EMACS_SERVER=
# Additional arguments for emacsclient.
EMACS_ARGS=

usage()
{
    cat <<EOF
usage: $(basename $0) [emacsclient options]

Focuses the emacs frame and switches bach to the client after dismissing its buffers.
Supports the following additional options :

    --do-switch      Do focus the emacs frame (default, unless one of -[ect] is present).
    --dont-switch    Don't focus the emacs frame.
    --do-return      Do return to the client, if necessary (default, unless -n is present).
    --dont-return    Don't return to the client.
    -                Read stdin and append it as a file to the argument list.

If you want to be able to not switch, you need to put this in your init file :

        (setq server-raise-frame nil)

emacslient --help output follows.
---

$($EMACS_CLIENT --help)
EOF
}

emacs_windowid()
{
    $EMACS_CLIENT --eval "\
(and (eq window-system 'x)                      \
     (string-to-number                          \
       (frame-parameter nil 'outer-window-id)))"
}

if which which &>/dev/null; then
    which xprop >/dev/null || { echo "You need to install xprop."; exit 0; }
    which wmctrl >/dev/null || { echo "You need to install wmctrl."; exit 0; }
fi

# This avoids a bug in gtk, when emacs treats this DISPLAYs
# differently.
if [[ "$DISPLAY" == :? ]];then
    EMACS_ARGS="$EMACS_ARGS -d ${DISPLAY}.0"
fi

if [ -n "$EMACS_SERVER" ];then
    EMACS_ARGS="$EMACS_ARGS -f $EMACS_SERVER"
fi

EMACS_CLIENT="$EMACS_CLIENT $EMACS_ARGS"

DO_SWITCH=true                  # Focus the frame ?
DO_RETURN=true                  # Return to the client ?
DO_PIPE=false                   # Read from stdin ?

for arg; do
    case $arg in
        --) break ;;
        -[cte]|--eval) DO_SWITCH=false ;;
        -n) DO_RETURN=false ;;
        --help) usage; exit 0 ;;
    esac
done

# Handle and remove extra options.
ARGS=()
EOA=false                       # Handle -- special argument.
for arg; do
    if [ "$EOA" = true ];then
        ARGS[${#ARGS[@]}]="$arg"
        continue
    fi
    case $arg in
        --do-switch) DO_SWITCH=true ;;
        --dont-switch) DO_SWITCH=false ;;
        --do-return) DO_RETURN=true ;;
        --dont-return) DO_RETURN=false ;;
        -) DO_PIPE=true ;;
        --) EOA=true ;&     # fall through
        *) ARGS[${#ARGS[@]}]="$arg" ;;
    esac
done
set -- "${ARGS[@]}"

# support piping to emacs
if [ $# -eq 0 ] || { [ $# -eq 1 ] && [ "$1" = -n ]; }; then
    DO_PIPE=true
fi

if [ "$DO_PIPE" = true ];then
    TEMPFILE=$(mktemp --tmpdir stdin.XXX)
    cat > $TEMPFILE
    set -- "$@" $TEMPFILE;
fi

# Get the window IDs.
CLIENTID=$(xprop -root | grep "^_NET_ACTIVE_WINDOW(WINDOW)" \
    | sed -e 's/.* \([^ ]\+\)$/\1/')
EMACSID=$(emacs_windowid 2>/dev/null | grep '^[0-9]\+$')

# Now focus the emacsframe
if [ "$DO_SWITCH" = "true" ] && \
    [ -n "$EMACSID" ];then
    wmctrl -i -a $EMACSID
fi

# Install a exit hook, which returns to the client and does some cleanup.
trap exit_hook EXIT
exit_hook()
{
    # give focus back to the client
    if [ "$DO_SWITCH" = "true" ] \
        && [ "$DO_RETURN" = "true" ] \
        && [ -n "$CLIENTID" ] ; then
        wmctrl -i -a $CLIENTID
    fi

    [ -n "$TEMPFILE" ] && rm -f "$TEMPFILE"
}

$EMACS_CLIENT "$@"
