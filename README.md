prime
=====

**make get-deps** - download dependencies

**make compile** - compile

**make run** - run in foreground

**make daemon** - run in background

**make stop** - stop

**make progress** - get current progress in %

**make save** - save results in text file



Input N-value and output filename are specified in **prime.config**

TODO
----
To make it work on several nodes (servers) the straightforward solution is:
* On startup we must take some time for other nodes to connect
* Connection from other (slave) nodes can be implemented similiarly with make target and escript, sending net_adm:ping
* Most code from prime.erl will go to a manager_server.erl which will manage each respective node
* Master should start supervised master_server on each slave node with help of erlang:nodes command
* Computing can be started after some timeout or manually
* Gathering of results can be implemented with gen_server:multi_call to managers

In order to make production system for similar task we should minimize usage of 
Erlang's internal message passing capabilities and use fault-tolerant 
message passing network service like RabbitMQ or other AMQP protocol.


