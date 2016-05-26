#!/bin/bash

# This script refreshes the build from a particular branch
# Run this script where it is located.

while [[ $# -gt 0 ]]; do
  opt="$1"
  shift;
  case "$opt" in
    "-b"|"--branch"     ) BRANCH="$1"; shift;;
    *                   ) echo "ERROR: Invalid option: \""$opt"\"" >&2
                          exit 1;;
  esac
done

echo "Using branch: $BRANCH"

if [ -z "$BRANCH" ]; then
  echo "branch required. use -b BRANCH_NAME."
  exit 1;
fi


# update repo
cd ..
git fetch origin "$BRANCH"
git reset --hard FETCH_HEAD

# replace & reload frontend
mkdir /www/
rm -rf /www/alphasheets
cp -r frontend /www/alphasheets
nginx -s reload
