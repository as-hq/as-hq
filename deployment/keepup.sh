#!/bin/bash

# Meant to be run from the root folder, with argument 1 = stable or master
type=$1

cd frontend
xvfb-run -a gulp keepup
killall Xvfb
if [ $? -eq 0 ]; then
  echo "BACKEND UP"
else
  echo "BACKEND DOWN"
  if [ $type == "stable" ]; then
    echo "stable" 
    curl -s http://stable.alphasheets.com:10000/job/stable-deploy/build
  else 
    if [ $type == "master" ]; then 
      echo "master"
      curl -s http://builds.alphasheets.com/job/master-deploy/build 
    fi
  fi
  cd ../scripts
  bash send-slack.sh "mayday mayday $1 down" "#general" "plumbus-bot"
fi
