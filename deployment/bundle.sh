#!/bin/bash

# This script is used for easily making executables from source code. 
# It makes frontend index.js files (uglified/minified and gzipped)
# It makes the Haskell, C++, and Python executables for backend servers
# It pushes to the alphasheets-builds repo if -p is passed (-s for using sudo for stack)

# Directory structure:
# - this script creates a temporary build folder in the root directory, then copies it to the parent.
# - so you end up with a parent/build and parent/CODEBASE_DIR

# Requires:
# - pyinstaller
# - installed frontend libs
# - environment files in their right places

USE_SUDO=false
PUSH_REMOTE=false
ENV_FILE=false

while [[ $# -gt 0 ]]; do
  opt="$1"
  shift;
  case "$opt" in
    "-b"|"--branch"     ) BRANCH="$1"; ENV_FILE="$1"; shift;;
    "-p"|"--push"       ) PUSH_REMOTE=true; shift;;
    "-s"|"--sudo"       ) USE_SUDO=true; shift;;
    "-e"|"--environment") ENV_FILE="$1"; shift;;
    *                   ) echo "ERROR: Invalid option: \""$opt"\"" >&2
                          exit 1;;
  esac
done

echo "Using environment: $ENV_FILE"
echo "Committing to branch: $BRANCH"

if [ -z "$BRANCH" ]; then
  echo "branch required. use -b BRANCH_NAME."
  exit 1;
fi

## check if we are in the project directory, and navigate to the root
CWD=${PWD##*/}

if [ $CWD = "scripts" ] || [ $CWD = "frontend" ] || [ $CWD = "backend" ] || [ $CWD = "deployment" ]; then
  cd ../
elif [ -d ./frontend ]; then
  continue
else 
  echo "Please start from the project directory."
  exit 1
fi

## prepare destination, match the current directory structure of codebase
rm -rf build ../build
mkdir build
cd build
git init
git remote add origin git@github.com:ooblahman/alphasheets-builds.git
git checkout -b "$BRANCH"

# back to root directory
cd ..

## build and copy distributable files
echo "building..."
cd frontend
# Copy the correct environment file into the default one read by gulp while building
cp src/js/Environment_$ENV_FILE.js src/js/Environment.js
if $USE_SUDO; then
  sudo npm install 
  sudo bower install --allow-root
  sudo gulp prod-build
else 
  npm install 
  bower install --allow-root
  gulp prod-build
fi
cd ..
cp -r frontend/dist build/frontend
echo "frontend build finished."

# frontend environments
cp frontend/src/js/Environment*.js build/frontend/

# Install up-to-date Python libraries
cd backend/as-libs/py
python setup.py develop
cd ../../..
echo "built Python libraries"

# Haskell executable (use sudo if applicable, copy the executable to the build/server folder)
cd backend/server
if $USE_SUDO; then
  sudo stack build
else 
  stack build 
fi
cd ../..
mkdir build/server
mkdir build/rkernel
cp backend/server/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin/alphasheets-exe build/server/
cp backend/server/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin/rkernel-exe build/rkernel/

# backend environments are all the same, because they run on docker
cp backend/Environment_$ENV_FILE.json build/Environment.json
cp backend/email_whitelist.txt build/
echo "backend build finished."

# C++ executable (compile, move executable over to build/graph
cd backend/graph-database
g++ -o server server.cpp location.cpp graph.cpp -lzmq -lboost_regex -lpthread -std=c++11 
cd ../../
mkdir build/graph
cp backend/graph-database/server build/graph/

# pykernel
cp -r backend/as-libs/py build/pylib
mkdir build/pykernel
cp -r backend/pykernel/* build/pykernel/

# Make a static directory inside of the build/server for images + file input handler
mkdir build/server/static

# Python executables for file-input-handler
# Copy all executables needed into the build/server/static folder
cd backend/server/static
pyinstaller file-input-handler.py
cd ../../..
cp -r backend/server/static/dist/file-input-handler/* build/server/static/
cd backend/server/static
rm -rf dist build file-input-handler.spec
cd ../../..

# Copy the deployment folder into the builds folder
cp -r deployment build/

cd build
NOW=$(date +"%c")
git add --force -A .
git commit -m "automated build: $NOW"

## push to remote
if $PUSH_REMOTE; then
  echo "pushing to remote branch: $BRANCH"
  git push --force origin "$BRANCH"
fi

cd ..

# Remove the created builds folder from the root directory
mv build ../
