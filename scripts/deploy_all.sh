#!/usr/bin/env bash
kill_existing()
{
  tmux kill-session -t "backend"
  tmux kill-session -t "graph_db"
  tmux kill-session -t "python_kernel"
  tmux kill-session -t "static_server"
  tmux kill-session -t "file_import"
  tmux kill-session -t "frontend"
}

kill_existing || true

tmux new-session -s "backend" -d "./start_backend.sh $1"
tmux new-session -s "graph_db" -d "cd ../backend/graph-database; ./server"
tmux new-session -s "python_kernel" -d "cd ../backend/pykernel; python server.py"
tmux new-session -s "static_server" -d "cd ../backend/server/static; python -m SimpleHTTPServer"
tmux new-session -s "file_import" -d "cd ../backend/server/static; python file-input-handler.py"
tmux new-session -s "frontend" -d "cd ../frontend; npm start"
