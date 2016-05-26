#!/bin/bash

source install_core.sh

## Test examples

function test_core {
	echo "Hello, world!" > testfile
}

function test_a_error {
	ls /root/
}
function test_b_error {
	cat /root/somefile
}
function test_c_error {
	echo 5 > /root/hello
}


homedir=~/sendao

initialize_installer $*

stage $homedir test_core
stage $homedir/abc test_core
stage ~ test_core
stage ~ test_a_error
stage ~ test_b_error
stage ~ test_c_error

run_install

