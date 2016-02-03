#!/bin/bash

# This script needs to be run as sudo, it will restore redis to a previous state
# Example: sudo restore-redis.sh 6379 Wednesday-January-27-at-07:00:07-AM.rdb

PORT=$1 # 6379
BACKUP_NAME=$2 # Wednesday-January-27-at-07:00:07-AM.rdb
REDIS_DUMP_FILE_PREFIX=/var/lib/redis
BACKUP_DIR=~/backups
BACKUP_FILE="$BACKUP_DIR/$PORT/$BACKUP_NAME"

# Backup the current redis state
bash redis-save.sh
# First stop the redis instance
/etc/init.d/redis_$i stop
# Delete the current dump file
REDIS_DUMP_FILE="$REDIS_DUMP_FILE_PREFIX/$PORT/dump.rdb"
# Copy the given file (as argument) back into dump.rdb and restart redis
# That way, redis has the backup dump file
cp $BACKUP_FILE $REDIS_DUMP_FILE
# Make sure the dump is owned by the same user as the redis-server user
chown redis:redis $REDIS_DUMP_FILE
# Restart redis
/etc/init.d/redis_$i start

# Note that after you do this command, if the cron save job is still running, 
# backup files will still be saved every N minutes. If you just want to play around,
# and not save anything for a while, you can disable future saving by stopping that cron job.
 
