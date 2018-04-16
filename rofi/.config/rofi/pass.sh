#!/usr/bin/env bash

# No arg, generate list of candidates
if [ -z $@ ]; then
    # Can't use pass to list since it uses tree(1).  But they are just files anyway.
    # First cut gets the file name and directory after .password-store.
    # Second cut removes the suffix.
    find ~/.password-store -name '*.gpg' | cut -d/ -f 5- | cut -d. -f 1
else
    ACCOUNT=$@

    # Show extended info (but *not* the password which is on first line)
    # FIXME: this seems to hang in latest versions of rofi when the keypin
    # shows up.
    # pass show "$ACCOUNT" | sed -n '1!p'

    # Copy password to primary X selection (middle-mouse paste)
    exec env PASSWORD_STORE_X_SELECTION=primary PASSWORD_STORE_CLIP_TIME=10 \
         pass show --clip "$ACCOUNT" 1>&-
fi
