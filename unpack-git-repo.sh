#!/bin/env bash

# GitHub clone is a packed GIT repo which is not supported.
# The script does unpacking objects.

set -e
set -x

ls -la
find .git -name '*.pack' | \
    while read p ; do
        rm -rf unpacked
        git init unpacked
        git -C unpacked unpack-objects < $p
        git -C unpacked checkout -b master $(git log -1 | grep -oE '[a-f0-9]{40}')
        mv .git .git.bak
        mv unpacked/.git .git
        rm -rf unpacked
    done
