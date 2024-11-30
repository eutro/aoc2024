#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"
cd "$DIR" || exit 1

if [ -z "$1" ]
then DAYN="$(date +%d)"
else DAYN="$1"
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

exec gst "days/utils.st" "days/day$DAYP.st" < "$AOC_INPUT"
