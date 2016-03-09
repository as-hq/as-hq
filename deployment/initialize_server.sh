#!/bin/bash

# This script deploys AlphaSheets on a clean server. It:
# * installs dependencies
# * writes the various configuration files
# * deploys frontend via nginx
# * builds the backend container image
# * deploys an admin dashboard for the server.

# Requirements:
# * the alphasheets-builds folder (if you see this file, you have it.)
# * Ubuntu 14.04 (trusty)
# * the build server's public key is in ~/.ssh/authorized_keys

###### Install dependencies ######
apt-get update

# docker
apt-get install apt-transport-https ca-certificates
apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-trusty main" >> /etc/apt/sources.list.d/docker.list
apt-get update
apt-get install linux-image-extra-$(uname -r)
apt-get install apparmor
apt-get install docker-engine
service docker restart

# nginx
apt-get install nginx

# misc
apt-get install tmux

###### Write configurations ######

# ssh
cp config/sshd_config /etc/ssh/sshd_config
service ssh restart

# nginx
cp config/nginx.conf /etc/nginx/nginx.conf
rm -rf /etc/nginx/sites-enabled
cp -r config/sites-enabled /etc/nginx/

###### Build and deploy ######

# start router
cd deployment/router
tmux new -s "router" -d "./router"

# build image
cd container
./docker_build.sh

# deploy a backend instance
curl -H "Content-Type: application/json" -X POST \
     -d '{"action":"create"}' http://localhost:10000

# start sites
cd ../..
cp -r frontend /www/alphasheets
nginx -s reload
