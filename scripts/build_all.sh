#!/usr/bin/env bash
cd ../frontend
npm install
bower install

cd ../backend/server
stack install
cd ../graph-database
g++ -o server location.cpp graph.cpp server.cpp -lzmq -std=c++11 -lboost_regex
cd ../as-libs/py
sudo python setup.py develop

echo "done."