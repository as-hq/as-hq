## Dependencies 

build-essential
libboost-all-dev
libzmq3-dev

## Building
# Graph db
```bash
$ cd graph-database
$ g++ -o server server.cpp -lzmq -std=c++11
$ ./server
```
