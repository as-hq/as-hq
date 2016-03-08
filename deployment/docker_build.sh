#!/bin/bash

# This script simply builds the docker image and tags it
# Needs to be run on each server that serves up Docker containers
# Run in the /deployment folder

docker build -t alphasheets/demo .
