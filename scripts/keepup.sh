#!/bin/bash

while :
    do
        cd /home/build/
        python deployment/keepup.py stable ws://localhost:5000
        sleep 600
done

