#!/bin/bash

# This script sets up of the beta users' servers.

BETA_USERS=(
 "defilippis"
 "kouvarispeter"
 "rouefully"
 "aryamanjalota"
 "amongthehills"
 "mmerchant"
 "michaelkatica"
 "cdvis"s
 "oliverpacut"
 "darrell"
 "philvanleeuwen"
 "josh"
 "workersgae"
 "eliross93"
 "catherio"
 "maniexx"
 "jayankovich"
 "rachela"
 "dan"
 "iannappier"
 "yizhou"
 "beta1"
 "beta2"
 "beta3"
 "beta4"
 "beta5"
 "martyboren"
 "vakaskar"
 "qiaochu"
 "brodiecarter"
 "akinwale"
 "jennybryan"
 "anicet"
 "gamzeortan"
 "akhil"
 "radoslav"
 "yonah"
 "aaron"
 "francisco"
 "linch"
 "cameron"
 "dpiemont"
 "nicholasfirth"
)

CMD="
apt-get update
apt-get -y install git
apt-get -y install tmux
mkdir build
cd build
git init
git remote add origin https://fa848255af68ed0e25ffe52a241be236781272bd@github.com/ooblahman/alphasheets-builds.git
git fetch origin beta
git checkout FETCH_HEAD
"

CMD_NEXT="
cd build/deployment
apt-get -y install nginx
./initialize_server.sh
"

CMD_DEPLOY="
TMUX=false
cd build/deployment
./redeploy.sh -b beta
"

CMD_DEPLOY_MIGRATE="
TMUX=false
cd build/migration
./alphasheets-migrate
./redeploy.sh -b beta
"

CMD_TEMP="
cd build/deployment/config/nginx
cp nginx.conf /etc/nginx/nginx.conf
rm -rf /etc/nginx/sites-enabled
cp -r sites-enabled /etc/nginx/
service nginx restart
"

CMD_FULL="
apt-get update
apt-get -y install git
apt-get -y install tmux
mkdir build
cd build
git init
git remote add origin https://fa848255af68ed0e25ffe52a241be236781272bd@github.com/ooblahman/alphasheets-builds.git
git fetch origin beta
git checkout FETCH_HEAD
cd ~/build/deployment
apt-get -y install nginx
./initialize_server.sh
TMUX=false
cd ~/build/deployment
./redeploy.sh -b beta
"

CMD_SSH="
rm ~/.ssh/authorized_hosts
rm ~/.ssh/authorized_heys
echo \"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDP4LZ6Fn9TAR0VnM43mkX1ZHf3WaMoOskVQGiKLOa2cB8p8M09MOYyTY2wDrIkDOnInpif6/yW8NZWtk9oM8ypgYHrh+4ihx4KFuVchNdAVPprqxNzhyeKgJ0RqbqlATrBs2eF3PFbEzEAOYdU5dShfpmPLNwpZrYFIW8htBUIdr362/PSSd2H8KH0JKzQ5lfZ0oBWqTw8WRKA9rj0W7qR/lN2DFyhyX3GztywufEvZIwKbNDCNt7cQAMNd4iaA1z1Hvjm6CJ7aE7k4R4oz2aDhD+zc7KYo4yYh9LO+AWND3v3vTfJNbvRhP9BGdsbF4aQzlTOm6uXfEOyJZ5x1KI45KZWo3263xFe7unF8mTJAhrePbW6G/15LG+KkV4+gKx537FA0V/p+4v3oK5lc46+KAQHPaqmStgLCiPEaPEm2/mzLIIKPhRAi6QMitnsE/xEjcb9TuaLYM4eGMZ6lLDeqJhoA8vP3tNm3xMYgLikHKhDgc2ruOdSJ0NE7wLGzSS5GEktWKWVTERk4J4yVpfHkguE8kDW0wvxg1BHS3vSvEmfUtI6gKhAcZ075GeaDadi2wkedtl7OOviYBepSCjhEmRBXhDfzvmFpt2wPnrHgnGOxJVtNUXle7ajSx+Bygj3FLi5E0OQjkOUYGq51JCuNlMGFtjsSqmomgPntVLPtw== zhukeepa@gmail.com\" >> ~/.ssh/authorized_keys
"

mkdir ~/beta-backups

for user in "${BETA_USERS[@]}"; do
  # echo "http://$user.alphasheets.com"
  # ssh "root@$user.alphasheets.com"
  # ssh -p 222 "root@$user.alphasheets.com" "$CMD_DEPLOY"
  scp -P 222 "root@$user.alphasheets.com:/var/lib/redis/6379/dump.rdb" ./
  sudo mv dump.rdb "/home/anand/beta-backups/$user.rdb"
  # tmux new -s "$user" -d "ssh -p 222 \"root@$user.alphasheets.com\" \"$CMD_DEPLOY_MIGRATE\""
done