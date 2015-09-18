## Dependencies 

build-essential
libboost-all-dev
libzmq3-dev

# Building
## Server 
```bash
$ cd server
$ stack install
```
## Graph db
```bash
$ cd graph-database
$ g++ -o server server.cpp -lzmq -std=c++11
```
# Running
(Redis must be started beforehand with '''redis-server''')
## Server 
```bash
$ stack exec alphasheets-exe
```
## Graph db
```bash
$ cd graph-database
$ ./server
```
## Static content server
```bash
$ cd server/static
$ python -m SimpleHTTPServer
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