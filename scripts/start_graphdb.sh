#!/usr/bin/env bash
until ../graph-database/as-graph-server; do
	echo "Graph database crashed with exit code $?. Respawning..." >&2
	sleep 1
done
