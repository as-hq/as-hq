#!/bin/bash

## DEPRECATING UNTIL ROUTER SETUP IS NEEDED (anand 3/13)
# # rebuild docker image
# cd container
# ./docker_build.sh

# # redeploy backend instances
# curl -H "Content-Type: application/json" -X POST \
#      -d '{"action":"redeploy_all"}' http://localhost:10000

## redeploy backend services
# graph
cd graph
tmux new -s "graphdb" -d "./server"
cd ../

# pykernel
cd pykernel
tmux new -s "pykernel" -d "./server"
cd ../

# backend
cd server
tmux new -s "backend" -d "./alphasheets-exe"

# rkernel
tmux new -s "rkernel" -d "./rkernel-exe"

# fileinput
cd static
tmux new -s "fileinput" -d "./file-input-handler"

# static
tmux new -s "static" -d "python -m SimpleHTTPServer"
cd ../..

# replace & reload frontend
rm -rf /www/alphasheets
cp -r frontend /www/alphasheets
nginx -s reload

