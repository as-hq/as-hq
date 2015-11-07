#!/usr/bin/env bash
cd ../server
stack install
cd ../graph-database
g++ -o as-graph-server server.cpp std=c++11 -lboost_regex -lzmq -lhiredis