#!/bin/env bash

# GitHub clone is a packed GIT repo which is not supported.
# The script does unpacking objects.

set -e

git init unpacked
find .git -name '*.pack' | while read p ; do git -C unpacked unpack-objects < $p ; done
git -C unpacked checkout -b master $(git log -1 | grep -oE '[a-f0-9]{40}')
mv .git .git.bak
rm -rf unpacked
mv unpacked/.git .git
