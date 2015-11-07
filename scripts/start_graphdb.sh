#!/usr/bin/env bash
until ../graph-database/as-graph-server; do
    if [ $? -eq 137 ]
        then exit
    fi
	echo "Graph database crashed with exit code $?. Respawning..." >&2
	sleep 1
done
