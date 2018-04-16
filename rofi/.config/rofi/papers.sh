#!/bin/bash

# No arg, generate list of candidates
if [ -z $@ ]; then
    find -L ~/papers ~/books -name '*.pdf' -or -name '*.djvu'
else
    PAPER=$@

    if [ -f "${PAPER}" ]; then
        exec zathura "${PAPER}" 1>&- # Close stdout otherwise rofi hangs around
                                     # and steals input
    fi
fi
