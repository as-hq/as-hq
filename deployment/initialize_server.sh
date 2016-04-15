#!/bin/bash

# This script deploys AlphaSheets on a clean server. It:
# * installs dependencies
# * writes the various configuration files
# * deploys frontend via nginx
# * builds the backend container image

# Requirements:
# * the alphasheets-builds folder (if you see this file, you have it.)
# * Ubuntu 14.04 (trusty)

# Run this script from the directory it lives in.

THIS_DIR=`pwd`

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
pip install six

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

###### Write configurations ######

cd $THIS_DIR

# ssh
cd config
cp sshd_config /etc/ssh/sshd_config
service ssh restart

# redis
cp redis/redis_6379 /etc/init.d/
cp redis/6379.conf /etc/redis/
update-rc.d redis_6379 defaults
service redis_6379 start

# nginx
cd nginx
cp nginx.conf /etc/nginx/nginx.conf
rm -rf /etc/nginx/sites-enabled
cp -r sites-enabled /etc/nginx/
service nginx restart

# authorize build server to login 
touch ~/.ssh/authorized_keys
echo "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1sPAVgGMHeOSGbmr8ehUkQiwQrbKmxUZtVhy8rTlq2hkLqCdC6uNcT2Z+eAbwJnmfLjXrXpvZlO2wLtlS3ITXdvs2cjITtn1BvQB/WcsIILwIJYmpmXEy8ZELBDXF5Crr6OAFObY8eo9mChM8fe7m46p5vww1pawqZuIWGVA5N2HJ5VX7tTTQdMywHSlvcMajuSLFEmhokAGcp+k3PE6YZqwTk9sxFSBJVyI2PKssP6rMbCPF4xco+YBB9CmV8pMCPSdIofGZkjAYHuUzwRw4JudD4cPyZ7S/WWOV1FqBInOfO5LUfagum6XdDxv79wscPkv+7nK/jQbsskmMXTSsTY3zGm+2kV1urxpaBz2HA1JGBPnWLM5oAOaauL7W7M9D0KA35s9egeimhn5Mm+DElVhfaXW3/sTDTpkGaWbYe3L7Tw+vkk8gxys82FMpu2YFwNglYSfsT8GReSFa/kiXNsxq2OI0PR0jShTbhxF5Af1ADCTKgQ76Vl7IEgkVGiaXGKAfRXmQ8PP37jWHqMxUbOz5MYwaY6KtuwzQ9sSZnp1Ynk7C+RjSPbl9y2JHTss4smNorUeopeiwd4HztcH/vN/2Yn1u56kz7jyqZd+jGg8naC+ufJn3MRhOmcEoUnqwQMsDRpMxloq7dGLXyCF3WEyJas31kmxdP5YLGuiFJQ== builds@alphasheets.com" >> ~/.ssh/authorized_keys
