#!/bin/bash

# Start the redis-server on docker run
/etc/init.d/redis-server stop
/etc/init.d/redis-server start

# Kill running processes
fuser -k 80/tcp
fuser -k 8000/tcp
fuser -k 20000/tcp 
fuser -k 9000/tcp
fuser -k 5000/tcp
fuser -k 5555/tcp

# Run the Haskell, C++, and Python executables. There is one frontend instance on the demo server, 
# so we don't need to build the frontend inside the container
cd ../home/alphasheets
cd graph
./server &
cd ../pykernel/server
./server &
cd ../..
cd file-input-handler/file-input-handler
./file-input-handler &
cd ../..
cd backend
stack exec -- /home/alphasheets/backend/alphasheets-exe 



