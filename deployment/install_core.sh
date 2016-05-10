#!/bin/bash
### Install Control Script


ERRORS=()
function report_error {
	ERRORS+=($* stop)
	errored=1
}

function try {
	output=$(${@:2} 2>&1)
	code="$?"
	if [ ! "$code" = "0" ]; then
		report_error Process failed: "$1": "$output" exit code: $code
	fi
}

stage_set=0

function initialize_installer {
	if [ "$1" = "install" ]; then
		stage_set=1
	fi
	if [ "$1" = "clean" ]; then
		echo Cleaning install files.
		rm -f .install_stages install_status
		exit 0
	fi
	if [ $stage_set = 0 ] && [ -f ~/.install_stages ]; then
		STAGES=(`cat ~/.install_stages`)
		stage_set=2
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
		
		if [ $status = 1 ]; then
			echo ===========================  Passing complete stage $stage
		else
			echo ===========================  Running $stage at $location
			if [ ! -d $location ]; then
				echo ===========================  Creating $location
				mkdir -p $location
			fi 
			cd $location
			try $stage $stage
			echo ===========================  Completed $stage
			if [ "$errored" = "1" ]; then
				report_errors
    			report_errors >> ~/install_status
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


