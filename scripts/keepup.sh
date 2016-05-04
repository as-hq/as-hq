#!/bin/bash

while :
    do
        python keepup.py $(hostname) ws://localhost:5000
        sleep 120
done

