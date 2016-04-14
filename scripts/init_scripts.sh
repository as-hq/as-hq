#!/bin/bash

# Meant to be run from the root directory, as root

cd scripts
cp keepup.sh /usr/local/bin/keepup.sh
chmod +x /usr/local/bin/keepup.sh
cp redis-save.sh /usr/local/bin/redis-save.sh
chmod +x /usr/local/bin/redis-save.sh
cp send-slack.sh /usr/local/bin/send-slack.sh
chmod +x /usr/local/bin/send-slack.sh
cat cron.txt | crontab -
