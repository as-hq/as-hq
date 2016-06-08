#!/bin/bash

while :
    do
        python keepup.py $(hostname) ws://localhost:81
        sleep 120
done

