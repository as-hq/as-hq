#!/bin/bash
# Config
#segment-repeat=2
#line-repeat=0

# Setup
#line-repeat=3
#segment-repeat=0
echo Testing, testing, 1 2 3
apt-get install something_that_doesnt_exist

# Segment 2
echo Starting segment two...
apt-cache search gdbg

# Segment 3
## Test block on repeatable segment with a possible success
echo Segment 3
apt-get install nodejs
echo tryitagain

# Segment 4
## Test chain requirement
#segment-repeat=0
#chain
echo Segment 4.. Run, $x, run...
apt-get install npm
echo hello, world

