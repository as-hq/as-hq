#!/bin/bash

## DEPRECATING UNTIL ROUTER SETUP IS NEEDED (anand 3/13)
# # rebuild docker image
# cd container
# ./docker_build.sh

# # redeploy backend instances
# curl -H "Content-Type: application/json" -X POST \
#      -d '{"action":"redeploy_all"}' http://localhost:10000

# ASSUMES THIS SCRIPT IS RUN FROM deployment/
cd ..

# update repo
git fetch origin master
git reset --hard FETCH_HEAD
git clean -d -fx ""

# Not needed as long as we're not using pyinstaller on the python kernel.
# (anand 4/14)
# export PATH=\"/root/anaconda2/bin:$PATH\"; 

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

