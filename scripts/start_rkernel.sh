#!/usr/bin/env bash
cd ../backend/server
until stack exec rkernel-exe; do
    if [ $? -eq 137 ]
        then exit
    fi
    echo "R KERNEL CRASHED with exit code $?. Respawning..." >&2
    sleep 1
done