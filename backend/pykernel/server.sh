#!/usr/bin/env bash

# This script auto-restarts the python kernel upon death.

until python server.py; do
  if [ $? -eq 137 ]
      then exit
  fi
  echo "Python kernel crashed with exit code $?. Respawning..." >&2
done