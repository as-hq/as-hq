#!/bin/bash
### Install Control Script

source install_core.sh

#### INSTALLATION # Ubuntu 14.04

function update_system {
    try apt_update apt_update apt-get update

    try install_libtool apt-get -y install libtool
    try install_boost apt-get -y install libboost-all-dev 
    try install_zmq3dev apt-get -y install libzmq3-dev 
	try install_pkgconfig apt-get -y install pkg-config 
    try install_build_essential apt-get -y install build-essential 
    try install_autoconf apt-get -y install autoconf 
    try install_automake apt-get -y install automake  
    try install_python apt-get -y install python-all-dev 
    try install_png apt-get -y install libpng-dev 
    try install_zlib apt-get -y install zlib1g-dev 
    try install_freetype apt-get -y install libfreetype6-dev 
    try install_python apt-get -y install python-dev 
    try install_python_tools apt-get -y install python-setuptools 

    try install_mysql_server apt-get -y install mysql-server
    try install_mysql_client apt-get -y install mysql-client-core-5.5
    try install_mysqlclient_dev apt-get -y install libmysqlclient-dev

    try install_s3 apt-get -y install s3cmd
    try install_nginx apt-get -y install nginx
    try install_tmux apt-get -y install tmux

    try install_binutils_gold apt-get -y install binutils-gold
}

function install_redis {
# redis
	try get_redis wget http://download.redis.io/releases/redis-stable.tar.gz
	tar xzf redis-stable.tar.gz

	cd redis-stable
	try compile_redis make
	try test_redis make test
	try install_redis make install
}
function install_redis_utils {
	try install_redis_utils ./install_server.sh
}

function install_pip {
# pip
	try get_pip curl -O https://bootstrap.pypa.io/get-pip.py
	try python_pip python get-pip.py
	try pip_extern pip install extern
	try pip_six pip install six
	try pip_websocket pip install websocket-client
	try pip_numpy pip install numpy
}
function install_libsodium {
# libsodium
	git clone git://github.com/jedisct1/libsodium.git
	cd libsodium 
	./autogen.sh
	try configure_libsodium ./configure
	try compile_libsodium make check
	try install_libsodium make install
	try register_libsodium ldconfig
}

function install_zmq {
# zmq
	try retrieve_git git clone https://github.com/zeromq/zeromq4-1
	cd zeromq4-1
	./autogen.sh
	try configure_git ./configure
	try compile_git make check
	try install_git make install
	try register_git ldconfig
}

function install_R {
# R 
	echo "deb http://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list
	gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
	gpg -a --export E084DAB9 | sudo apt-key add -
	try apt_get_R apt-get update
	try install_R apt-get -y install r-base
}

function install_R_packages {
# R packages
	try r_pack_configure echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.us.r-project.org'; options(repos = r);" > ~/.Rprofile
	try r_pack_json Rscript -e "install.packages('rjson')"
	try r_pack_ggplot2 Rscript -e "install.packages('ggplot2')"
	try r_pack_party Rscript -e "install.packages('party')"
	try r_pack_jpeg Rscript -e "install.packages('jpeg')"
}

function install_Stack {
# stack
	try get_ubuntu_keys apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
	echo 'deb http://download.fpcomplete.com/ubuntu trusty main'|sudo tee /etc/apt/sources.list.d/fpco.list
	try get_ubuntu_update apt-get update 
	try get_stack apt-get -y install stack
}

function install_Node {
	
# node
	try install_node curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.0/install.sh | bash
	source ~/.bashrc
	try install_nvm_0127 nvm install 0.12.7
	nvm alias default 0.12.7
	nvm use default
}

function install_Gold {
# gold linker
	try install_gold20 update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.gold" 20
	try install_gold10 update-alternatives --install "/usr/bin/ld" "ld" "/usr/bin/ld.bfd" 10
	try install_ld update-alternatives --config ld
}

function install_Aliases {
# anand's amazing git aliases
	cd ~
	rm .gitconfig
	try fetch_gitconfig curl -o .gitconfig http://puu.sh/ojAFc/72c819717a.gitconfig
}

# S3 config for backups (uncomment for servers that need backup)
# curl -o .s3cfg http://puu.sh/ojPxI/2e874ee3a0.s3cfg

function install_Codebase {
###### Codebase ######
# repo
	try init_git git init
	try install_alphasheets git remote add origin https://github.com/ooblahman/alpha-sheets.git
	try fetch_alphasheets git fetch -a
	try fetch_master git checkout master

# frontend
	cd frontend
	try install_npm npm install
	try install_bower npm install -g bower
	try install_gulp npm install -g gulp-cli
	try install_bowerer bower install --allow-root
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

}

function deploy_server {
###### Deploy ######
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
}


dependencies="~/.alphasheets-dependencies"

initialize_installer

stage /root/ update_system
stage $dependencies install_redis
stage $dependencies/utils install_redis_utils
stage $dependencies install_pip 
stage $dependencies install_libsodium 
stage $dependencies install_zmq
stage $dependencies install_R
stage $dependencies install_R_packages 
stage $dependencies install_Stack
stage $dependencies install_Node 
stage $dependencies install_Gold
stage $dependencies install_Aliases
stage $dependencies install_Codebase

stage ~/codebase/ deploy_server

run_install
