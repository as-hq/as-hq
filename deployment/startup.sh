#!/bin/bash

# Start the redis-server on docker run
/etc/init.d/redis-server stop
/etc/init.d/redis-server start

# Run the Haskell, C++, and Python executables. There is one frontend instance on the demo server, 
# so we don't need to build the frontend inside the container
cd /home/alphasheets
cd graph
./server &
cd ../pykernel/server
./server &
cd ../..
cd file-input-handler/file-input-handler
./file-input-handler &
cd ../..
cd backend
/home/alphasheets/backend/alphasheets-exe 



