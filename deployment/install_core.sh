#!/bin/bash
### Install Control Script


ERRORS=()
function report_error {
	ERRORS+=($* stop)
	errored=1
}

function try {
	echo Trying $1

	exec 5>&1
	output=$(${@:2} 2>&1|tee /dev/fd/5)
	echo $output >> log_$1
	#echo $output
	code="$?"
	if [ ! "$code" = "0" ]; then
		report_error Process failed: "$1": "$output" exit code: $code
	fi
}

stage_set=0

function initialize_installer {
	if [ "$1" = "install" ]; then
		stage_set=2
	fi
	if [ "$stage_set" = "0" ] && [ -f '/root/.install_stages' ]; then
		STAGES=(`cat ~/.install_stages`)
		stage_set=2
		echo Repeating from where previous install left off.
	else
		stage_set=1
		STAGES=()
	fi
}

function report_errors {
	local i
	result=""
	for i in ${ERRORS[@]}; do
		if [ "$i" = "stop" ]; then
			echo $result
			result=""
		else
			result+="$i "
		fi
	done
	if [ ! "$result" = "" ]; then 
		echo $result
	fi
}


function stage {
	if [ $stage_set = 1 ]; then
		STAGES+=($1 $2 0)
	fi
}

function run_install {
	local i
    stage_count=${#STAGES[@]}
    
    echo Starting installation...
    
	for ((i=0; i<$stage_count; i+=3));do
		
		location=${STAGES[$i]}
		stage=${STAGES[$((i+1))]}
		status=${STAGES[$((i+2))]}
		
		if [ "$status" = "1" ]; then
			echo ===========================  Passing complete stage $stage
			echo 
		else
			if [ ! -e $location ]; then
				mkdir $location
			fi 
			echo ===========================  Running $stage
			cd $location
			try $stage $stage
			echo ===========================  Completed $stage
			if [ "$errored" = "1" ]; then
				report_errors
	    			report_errors >> ~/.install_status
				ERRORS=()
				STAGES[$((i+2))]=2
			else
				STAGES[$((i+2))]=1
    		fi
    													
    		echo ${STAGES[@]} > ~/.install_stages
		fi		
	done
	
	echo Done	
}


## Test examples

function test_core {
	echo "Hello, world!" > /tmp/testfile
}

function test_a_error {
	ls /root/
}


