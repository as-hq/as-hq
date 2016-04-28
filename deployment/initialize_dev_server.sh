#!/bin/bash

# This sets up an AlphaSheets dev environment on a clean server. It:
# * installs dependencies
# * deploys it

# Requirements:
# * Ubuntu 14.04 (trusty)

###### Install dependencies ######
mkdir ~/.alphasheets-dependencies
cd ~/.alphasheets-dependencies

apt-get update

apt-get -y install libtool 
apt-get -y install libboost-all-dev 
apt-get -y install libzmq3-dev 
apt-get -y install pkg-config 
apt-get -y install build-essential 
apt-get -y install autoconf 
apt-get -y install automake  
apt-get -y install python-all-dev 
apt-get -y install libpng-dev 
apt-get -y install zlib1g-dev 
apt-get -y install libfreetype6-dev 
apt-get -y install python-dev 
apt-get -y install python-setuptools 

apt-get -y install mysql-server
apt-get -y install mysql-client-core-5.5
apt-get -y install libmysqlclient-dev

apt-get -y install s3cmd
apt-get -y install nginx
apt-get -y install tmux

apt-get -y install binutils-gold

# redis
wget http://download.redis.io/releases/redis-stable.tar.gz
tar xzf redis-stable.tar.gz

cd redis-stable
make
make test
make install

cd utils
./install_server.sh
cd ../..

# pip
curl -O https://bootstrap.pypa.io/get-pip.py
python get-pip.py
pip install extern
pip install six
pip install websocket-client
pip install numpy

# libsodium
git clone git://github.com/jedisct1/libsodium.git
cd libsodium 
./autogen.sh
./configure
make check
make install
ldconfig
cd ..

# zmq
git clone https://github.com/zeromq/zeromq4-1
cd zeromq4-1
./autogen.sh
./configure
make check
make install
ldconfig
cd ..

# R 
echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
apt-get update
apt-get -y install r-base

# R packages
echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
Rscript -e "install.packages('rjson')"
Rscript -e "install.packages('ggplot2')"
Rscript -e "install.packages('party')"
Rscript -e "install.packages('jpeg')"

# stack
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
apt-get update 
apt-get -y install stack

# node
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | bash
source ~/.bashrc
nvm install 0.12.7
nvm alias default 0.12.7
nvm use default

# gold linker
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
update-alternatives --config ld

# anand's amazing git aliases
cd ~
rm .gitconfig
curl -o .gitconfig http://puu.sh/ojAFc/72c819717a.gitconfig

# S3 config for backups (uncomment for servers that need backup)
# curl -o .s3cfg http://puu.sh/ojPxI/2e874ee3a0.s3cfg

###### Codebase ######
cd ~

# repo
mkdir codebase
cd codebase
git init
git remote add origin https://github.com/ooblahman/alpha-sheets.git
git fetch -a
git checkout master

# frontend
cd frontend
npm install
npm install -g bower
npm install -g gulp-cli
bower install --allow-root
cd src/js/
cp Environment_local.js Environment.js
cd ../../../

# backend
cd backend
cp Environment_local.json Environment.json
cd server
stack setup
stack install --fast
cd ../

# graph db
cd graph-database
make
cd ../

# python libs
cd as-libs/py
python setup.py develop
cd ../../

###### Deploy ######
cd ~/codebase/

tmux ls | grep : | cut -d. -f1 | awk '{print substr($1, 0, length($1)-1)}' | tmux kill-session 

cd backend/graph-database 
tmux new -s "graph_db" -d "./server"
cd ../pykernel
tmux new -s "python_kernel" -d "python server.py"
cd ../server
tmux new -s "backend" -d "stack exec alphasheets-exe"
tmux new -s "r_kernel" -d "stack exec rkernel-exe"
cd static
tmux new -s "static_server" -d "python -m SimpleHTTPServer"
tmux new -s "file_import" -d "python file-input-handler.py"
cd ~/codebase/frontend
tmux new -s "frontend" -d "npm start"
