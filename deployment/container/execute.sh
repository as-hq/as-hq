#!/bin/bash

# Run the Haskell, C++, and Python executables. There is one frontend instance on the demo server, 
# so we don't need to build the frontend inside the container
cd /home/alphasheets
cd graph
./server &
cd ../pylib
python setup.py develop
cd ../pykernel
python server.py &
cd ../
cd server/static
python -m SimpleHTTPServer &
./file-input-handler &
cd ..
./rkernel-exe &
./alphasheets-exe 
