#!/usr/bin/env bash

tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | tmux kill-session 

tmux new -s "redis" -d "redis-server"
tmux new -s "graph_db" -d "cd ../backend/graph-database; ./server"
tmux new -s "python_kernel" -d "cd ../backend/pykernel; python server.py"
tmux new -s "backend" -d "cd ../backend/server; stack exec alphasheets-exe"
tmux new -s "r_kernel" -d "cd ../backend/server; stack exec rkernel-exe"
tmux new -s "static_server" -d "cd ../backend/server/static; python -m SimpleHTTPServer"
tmux new -s "file_import" -d "cd ../backend/server/static; python file-input-handler.py"
tmux new -s "frontend" -d "cd ../frontend; npm start"
