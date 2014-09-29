rockse
========

Tags: Rocksdb, Erlang

This is an Erlang binding for the Facebook Rocksdb. Currently relies on having Rocksdb 
downloaded, compiled and installed separately. 

This version of the binding only provides put, get and delete. 

Limitations v0.0.1
------------------

1. No tracking of keys for each table.
2. All tables are local to a node. 
3. No configuring options
4. Key and values must be strings

Roadmap v0.0.2
--------------

1. Key and values can be any Erlang term
2. Distributed tables

Installation
============

1. Download and install Rocksdb

https://github.com/facebook/rocksdb

2. Modify rockse/c_src/Makefile

What is relevant are the paths to various locations, in particular

ERL_BASE_PATH and ROCKSE_BASE_PATH

3. Build the bindings

$> make

Post Compile
============

You have to export LD_LIBRARY_PATH to point to the location of the rockse dynamic library.
Below is an example:

$> export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/rocksdb/lib

On MacOSX export DYLD_LIBRARY_PATH instead.

$> export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:/usr/local/rocksdb/lib

You have to export LD_LIBRARY_PATH before using the Erlang bindings. It is adviced to
place the export into your .profile

Run the tests

$> make tests

Usage
=====

Currently rockse uses error_logger which can be quite chatty. We can output all those
into a file

$> erl -pa ebin -sname rtest -kernel error_logger '{file,"/tmp/rockse.log"}'

Start the rockse application

1> application:start(rockse).

Check if there are any existing tables

2> rockse:tables().

Add a table if one currently doesn't exists

3> rockse:add_table(users).

Write an entry to a table

4> rockse:put(users, "tom", "[{password, foo}]").

5> rockse:put(users, "jill", "[{password, bar}]").

6> rockse:put(users, "peter", "[{password, rabbit}]").

Get an entry from a table

7> rockse:get(users, "jill").

Delete an entry from a table

8> rockse:delete(users, "peter").

9> rockse:get(users, "peter").
