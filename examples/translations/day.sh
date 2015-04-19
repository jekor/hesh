#!/bin/sh

# https://github.com/pixelb/scripts/blob/f93a55beae3baf97af64f8c833e27cd6e04fb983/scripts/day

# Shows the date for "fri week" or "mon last" for e.g.

# License: LGPLv2

Usage () {
    echo "Usage: `basename $0` mon|tue|... [next]|week|last" >&2
    exit 1
}

[ "$#" -eq "0" ] && Usage

day=$1
which=$2
[ -z "$which" ] && which=next

case $which in
week)
    if [ `date +%D` = `date --date="next $day" +%D` ]; then
        weeks=+2 #assume we want the next week, not today
    else
        weeks=+1
    fi ;;
last)
    weeks=-1;;
next)
    if [ `date +%D` = `date --date="next $day" +%D` ]; then
        weeks=+1 #assume we want the next week, not today
    else
        weeks=+0
    fi ;;
*)
    Usage;;
esac

date --date="$day $weeks weeks" +%x
