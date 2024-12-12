#!/usr/bin/env sh

DIR="$(readlink -f "$(dirname "$0")")"
cd "$DIR" || exit 1

export AOC_RUN='explore'
export AOC_EXTRA_FILES="$AOC_EXTRA_FILES $DIR/days/interaction.st"

exec ./run.sh "$@" --
