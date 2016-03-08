#!/bin/bash

# Start the redis-server on docker run (as root)
/etc/init.d/redis-server stop
/etc/init.d/redis-server start

# Run the executables as a demo user
su -c "bash /usr/local/bin/execute.sh" demo



