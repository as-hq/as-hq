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

# pip
curl -O https://bootstrap.pypa.io/get-pip.py
python get-pip.py
pip install docker-py

# misc
apt-get install tmux

###### Write configurations ######

# ssh
cd deployment/config
cp sshd_config /etc/ssh/sshd_config
service ssh restart

###### Build and deploy ######

# start router
cd ../
tmux new -s "instance_router" -d "python instance_router.py"

# build image
cd container
./docker_build.sh