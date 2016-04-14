#!/bin/bash

# No arg, generate list of candidates
if [ -z $@ ]; then
    find ~/papers -name '*.pdf'
else
    PAPER=$@

    if [ -f ${PAPER} ]; then
        exec zathura "${PAPER}" 1>&- # Close stdout otherwise rofi hangs around
        # and steals input
    fi
fi
