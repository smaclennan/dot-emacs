#!/bin/sh

# This ispell script is just good enough to quiet the flyspell
# errors. It just says every word is spelled correctly. So it is good
# for your ego, but terrible as a spell checker.

# (setq ispell-program-name (concat user-emacs-directory "src/ispell.sh"))

echo "@(#) Poor Man's Ispell Version 3.2.06 08/01/01"

while read -p "word: " line; do
    case $line in (^*) echo "*";; esac
    echo
done
