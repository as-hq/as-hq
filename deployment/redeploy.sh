#!/bin/bash

# This script redeploys the build from a particular branch
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

## redeploy backend services

# graph
cd graph
tmux kill-session -t "graphdb"
tmux new -s "graphdb" -d "./server"
cd ../

# pykernel
cd pylib
python setup.py develop
cd ../pykernel
tmux kill-session -t "pykernel"
tmux new -s "pykernel" -d "python server.py"
cd ../

# rkernel
cd rkernel
tmux kill-session -t "rkernel"
tmux new -s "rkernel" -d "./rkernel-exe"

#  wait for kernels to come up
sleep 10

# backend
cd ../server
tmux kill-session -t "backend"
tmux new -s "backend" -d "./alphasheets-exe"

# fileinput
cd static
tmux kill-session -t "fileinput"
tmux new -s "fileinput" -d "./file-input-handler"

# static
tmux kill-session -t "static"
tmux new -s "static" -d "python -m SimpleHTTPServer"
cd ../..

# keepup scripts
cd scripts
tmux kill-session -t "keepup"
tmux new -s "keepup" -d "./keepup.sh"
cd ..

# replace & reload frontend
mkdir /www/
rm -rf /www/alphasheets
cp -r frontend /www/alphasheets
nginx -s reload

