## Dependencies 

build-essential
libboost-all-dev
libzmq3-dev

# Building
## Graph db
```bash
$ cd graph-database
$ g++ -o server server.cpp -lzmq -std=c++11
$ ./server
```

# Testing
## Profiling
set ghc-options under the proper heading in alphasheets.cabal as 
'''
ghc-options:         -O2 -threaded -fprof-auto "-with-rtsopts=-N -p -s -h -i0.01"
'''
regular options are 
'''
ghc-options:         -O2 -threaded -with-rtsopts=-N
'''