#!/usr/bin/env bash
until eval $1; do
  err=$?
  echo "Process crashed with exit code $err. Respawning..." >&2
  sleep 1
done