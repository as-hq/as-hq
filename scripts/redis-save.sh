#!/bin/bash

# This script backs up a copy of the Redis DB to Amazon S3 each hour
# It assumes a Redis file directory like the one on the server
# It also uses Amazon S3 configuration already set up on the server

REDIS_DUMP_FILE_PREFIX=/var/lib/redis
S3_BUCKET=s3://redis.alphasheets.com
RP=REDIS_PASSWORD_THAT_WONT_BE_PUSHED_TO_GIT

declare -a PORTS=("6379")
set -e

for i in "${PORTS[@]}"
do
  # Run a save operation in the background, using lastsave metrics to return
  # only when the saving is terminated and dump.rdb is modified
  CUR_LAST_SAVE=$(redis-cli -p $i -a $RP lastsave)
  LAST_SAVE=$CUR_LAST_SAVE
  redis-cli -p $i -a $RP bgsave
  while [ $CUR_LAST_SAVE == $LAST_SAVE ]; do LAST_SAVE=$(redis-cli -p $i -a $RP lastsave); sleep 2; done;
  # Get the current date and time, which gives the dest file name
  # This script is called each hour, so since the file name is a function of the day, 
  # the file will be modified 24 times/day, and a new one created each day
  TIME=`date '+%Y-%m-%d'`
  REDIS_DUMP_FILE="$REDIS_DUMP_FILE_PREFIX/$i/dump.rdb"
  S3_DUMP_FILE="$S3_BUCKET/$i/$TIME.rdb"
  s3cmd put $REDIS_DUMP_FILE  $S3_DUMP_FILE
done


