#!/bin/bash

# Run the Haskell, C++, and Python executables. There is one frontend instance on the demo server, 
# so we don't need to build the frontend inside the container
cd /home/alphasheets

# graph
cd graph
./server &

# pykernel
cd ../pykernel
python server.py &

# rkernel
cd ../rkernel
./rkernel-exe &

#  wait for kernels to come up
sleep 10

# backend
cd ../server
./alphasheets-exe &

# fileinput
cd static
./file-input-handler &

# static server
python -m SimpleHTTPServer
