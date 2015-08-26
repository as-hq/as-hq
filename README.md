## min requirements: 

cabal 
ghc 7.8+
mongodb 3.0 (current)
python 2.7/3.x

## Building Redis files

```bash
$ cd src/AS/hiredis
$ sudo make && sudo make install
$ gcc -c as_db.c -fPIC
```

## Setup Mysql ODBC driver 
1) Install libmyodbc and unixodbc-dev with apt-get
2) sudo chmod a+w+r odbc.ini in etc folder, edit config to add database, test with isql -v AlphaSheets

[AlphaSheets]
Driver       = /usr/lib/x86_64-linux-gnu/odbc/libmyodbc.so
Description  = MyODBC 3.51 Driver DSN
SERVER       = 127.0.0.1
PORT         = 3306
USER         = root
Password     = *****
Database     = as-instance
Socket 	     = /var/run/mysqld/mysqld.sock
DefaultLongDataBuffLen = 1024
Pooling = 1
MaxPoolSize=100
MinPoolSize=0

3) In MySQL as-instance, create two tables: 
	a) a_s_relations with FromLoc and ToLoc as varchar(256) 
	b) a_s_cells with Location,Expression,Value all varchar(256) where Location is a primary key



## TODO: 
1) We shouldn't be sending cells to all connected users; there needs to be some permissions built in
