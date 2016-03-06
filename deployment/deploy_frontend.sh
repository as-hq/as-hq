#!/bin/bash

# This script deploys frontend for nginx serving.

# prepare build dir
cd
rm -rf build
git clone https://github.com/ooblahman/alphasheets-builds build

# copy static files to /www/alphasheets
rm -rf /www/alphasheets
cp r build/frontend /www/alphasheets

# serve new files
nginx -s reload

# self-destruct build files
rm -rf build