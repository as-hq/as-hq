#!/usr/bin/env bash
cd ../server
until .stack-work/install/x86_64-linux/lts-3.7/7.10.2/bin/alphasheets-exe; do
    if [ $? -eq 137 ]
        then exit
    fi
    echo "Backend server crashed with exit code $?. Respawning..." >&2
    sleep 1
done