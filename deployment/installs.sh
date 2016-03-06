#!/bin/bash

# Install libsodium and zeromq in installs folder
cd ../home
mkdir installs
cd installs
git clone git://github.com/jedisct1/libsodium.git
cd libsodium 
./autogen.sh
./configure
make check
make install
ldconfig
cd ..
mkdir zeromq
cd zeromq
git clone https://github.com/zeromq/zeromq4-1
cd zeromq4-1
./autogen.sh
./configure
make check
make install
ldconfig
cd ../..

