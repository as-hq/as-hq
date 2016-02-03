#!/bin/bash

REDIS_DUMP_FILE_PREFIX=/var/lib/redis
BACKUP_DIR_PREFIX=~/backups

declare -a PORTS=("6380", "6381")
set -e
for i in "${PORTS[@]}"
do
   # Run a save operation in the background, using lastsave metrics to return only when the
   # saving is terminated and dump.rdb is written to disk
   CUR_LAST_SAVE=$(redis-cli -p $i lastsave)
   LAST_SAVE=$CUR_LAST_SAVE
   redis-cli -p $i bgsave
   while [ $CUR_LAST_SAVE = $LAST_SAVE ]; do LAST_SAVE=$(redis-cli -p $i lastsave); sleep 2; done;
   # Get the current time and dest file name
   TIME=`date '+%A-%B-%e-at-%I:%M:%S-%p'` 
   REDIS_DEST="$BACKUP_DIR_PREFIX/$i/$TIME.rdb"
   REDIS_DUMP_FILE="$REDIS_DUMP_FILE_PREFIX/$i/dump.rdb"
   # Copy the redis dump into a backup, and date the file
   cp $REDIS_DUMP_FILE $REDIS_DEST
done
