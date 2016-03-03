#!/bin/bash

# This script is used for easily making executables from source code. 
# It makes frontend index.js files (uglified/minified and gzipped)
# It makes the Haskell, C++, and Python executables for backend servers
# It pushes to the alphasheets-builds repo if -p is passed (-s for using sudo for stack)

USE_SUDO=false

while [[ $# -gt 0 ]]; do
  opt="$1"
  shift;
  case "$opt" in
    "-p"|"--push"       ) BRANCH="$1"; shift;;
    "-s"|"--sudo"       ) USE_SUDO=true; shift;;
    *                   ) echo "ERROR: Invalid option: \""$opt"\"" >&2
                          exit 1;;
  esac
done

## check if we are in the project directory, and navigate to the root
CWD=${PWD##*/}

if [ $CWD = "scripts" ] || [ $CWD = "frontend" ] || [ $CWD = "backend" ]; then
  cd ../
elif [ -d ./frontend ]; then
  continue
else 
  echo "Please start from the project directory."
  exit 1
fi

## prepare destination
echo "preparing directories..."
rm -rf build
mkdir build
cd build
git init
git remote add origin git@github.com:ooblahman/alphasheets-builds.git
# preserve the branch history
git pull origin master
rm -rf *
mkdir frontend backend graph pykernel file-input-handler

# back to root directory
cd ..

## build and copy distributable files
echo "building..."
cd frontend
if $USE_SUDO; then
  sudo gulp prod-build
else 
  gulp prod-build
fi
cd ..
cp -r frontend/dist/* build/frontend/
echo "frontend build finished."

# Haskell executable
cd backend/server
if $USE_SUDO; then
  sudo stack build
else 
  stack build 
fi
cd ../..
cp backend/server/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin/alphasheets-exe build/backend
cp backend/Environment_docker.json build/Environment.json
echo "backend build finished."

# C++ executable
cd backend/graph-database
g++ -o server server.cpp location.cpp graph.cpp -lzmq -lboost_regex -std=c++11
cd ../../
cp backend/graph-database/server build/graph/

# Python executables using pyinstaller
pip install pyinstaller
cd backend/pykernel
pyinstaller server.py
cd ../..
cp -r backend/pykernel/dist/server build/pykernel
rm -rf backend/pykernel/dist
rm -rf backend/pykernel/build
rm -rf backend/pykernel/server.spec

cd backend/server/static
pyinstaller file-input-handler.py
cd ../../..
cp -r backend/server/static/dist/file-input-handler build/file-input-handler
rm -rf backend/server/static/dist
rm -rf backend/server/static/build
rm -rf backend/server/static/file-input-handler.spec

## push to remote
if [ -n "$BRANCH" ]; then
  echo "pushing to remote branch: $BRANCH"
  NOW=$(date +"%c")
  cd build
  git add -A .
  git commit -m "automated build: $NOW"
  git push origin "$BRANCH"
fi
