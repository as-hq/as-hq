#!/bin/bash

while :
    do
        cd /home/build/
        python keepup.py stable ws://localhost:5000
        sleep 600
done

