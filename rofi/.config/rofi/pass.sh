#!/usr/bin/env bash

# No arg, generate list of candidates
if [ -z $@ ]; then
    # Can't use pass to list since it uses tree(1).  But they are just files anyway.
    # First cut gets the file name and directory after .password-store.
    # Second cut removes the suffix.
    find ~/.password-store -name '*.gpg' | cut -d/ -f 5- | cut -d. -f 1
else
    ACCOUNT=$@

    # Copy password to primary X selection (middle-mouse paste)
    exec env PASSWORD_STORE_X_SELECTION=primary pass show --clip "$ACCOUNT" 1>&-
fi
