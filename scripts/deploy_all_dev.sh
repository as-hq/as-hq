#!/usr/bin/env bash

tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | tmux kill-session 

cd backend/graph-database 
tmux new -s "graph_db" -d "./server"
cd ../pykernel
tmux new -s "python_kernel" -d "python server.py"
cd ../server
tmux new -s "backend" -d "stack exec alphasheets-exe"
tmux new -s "r_kernel" -d "stack exec rkernel-exe"
cd static
tmux new -s "static_server" -d "python -m SimpleHTTPServer"
tmux new -s "file_import" -d "python file-input-handler.py"
cd ~/codebase/frontend
tmux new -s "frontend" -d "npm start"
