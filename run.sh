#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"
cd "$DIR" || exit 1

if [ -z "$1" ]
then DAYN="$(date +%d)"
else DAYN="$1"; shift
fi
DAY="$(echo "$DAYN" | sed 's/^0*//')"
DAYP="$(printf "%02d" "$DAY")"

export AOC_INPUT
if [ "$1" = "--" ]
then AOC_INPUT="/dev/stdin"; shift
elif [ "$1" = "-i" ]
then AOC_INPUT="$2"; shift; shift
else
    AOC_INPUT="$DIR/input/day$DAYP.txt"
    if ! [ -f "$AOC_INPUT" ]
    then ./fetch.sh "$DAY" || exit 1
    fi
fi

export AOC_VIS="$DIR/vis/day$DAYP"

export AOC_RUN
if [ -z "$AOC_RUN" ] ; then AOC_RUN=1; fi

if [ -z "$GST" ] ; then GST=gst ; fi

exec $GST --emacs-mode -g "days/utils.st" $AOC_EXTRA_FILES "days/day$DAYP.st" < "$AOC_INPUT"
