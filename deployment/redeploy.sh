#!/bin/bash

# rebuild docker image
cd container
./docker_build.sh

# redeploy backend instances
curl -H "Content-Type: application/json" -X POST \
     -d '{"action":"redeploy_all"}' http://localhost:10000

# redeploy frontend
cd ../..
cp -r frontend /www/alphasheets
nginx -s reload

