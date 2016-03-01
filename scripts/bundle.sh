#!/bin/bash

while getopts 'p:' arg
do
  case ${arg} in
    p) branch=${OPTARG};;
    *) echo "...exiting."; exit 1
  esac
done

## check if we are in the project directory, and navigate to the root
cwd=${PWD##*/}

if [ $cwd = "scripts" ] || [ $cwd = "frontend" ] || [ $cwd = "backend" ]; then
  cd ../
elif [ -d ./frontend ]; then
  continue
else 
  echo "Please start from the project directory."
  exit 1
fi

## prepare destination
echo "preparing directories..."
[ -d build ] || mkdir build
cd build
rm -rf *
mkdir frontend backend
cd ..

## build and copy distributable files
echo "building..."
cd frontend
gulp prod-build
cd ..
cp -r frontend/dist/* build/frontend/
echo "frontend build finished."

cd backend/server
stack build
cd ../..
cp backend/server/.stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin/alphasheets-exe build/backend
cp backend/Environment.json build/ 
echo "backend build finished."

## push to remote
if [ -n "$branch" ]; then
  echo "pushing to remote branch: $branch"
  now=$(date +"%c")
  cd build
  git add -A .
  git commit -m "automated build: $now"
  git push origin "$branch"
fi