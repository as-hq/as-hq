#!/bin/bash

# This script deploys AlphaSheets on a clean server. It:
# (a) installs dependencies
# (c) writes the various configuration files
# (d) deploys frontend via nginx
# (e) deploys a docker container running backend
# (f) deploys an admin dashboard for the server.

# It requires:
# the alphasheets-builds folder (the parent of this directory)
# Ubuntu 14.04 (trusty)

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

# docker
cd container
./docker_build.sh

# sites
cd ..
cp -r frontend /www/alphasheets
cp -r dashboard /www/dashboard
nginx -s reload

# router
cd deployment/router
tmux new -s "router" -d "./router"