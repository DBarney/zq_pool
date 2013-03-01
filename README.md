## zq_pool

A worker pool implemented in erlang with erlamq2.

## Starting up a worker pool

So the idea behind the worker pool is that no communication happens to the worker in the pool except through the zmq_sockets.  jobs are dumped to the queue, and are given to the next available worker who is able to process them. Mulitple queues can be started on the same node, as there is no registering of process names.

I think in the fututre, the workers will also beable to be configured through the erlzmq_socet

## How to start a queue

Starting a queue is fairly straightforward, the queue and its workers fit nicely inside the process supervision tree so starting your own queue is as simple as starting a child under a supervisor:

```
% some where else
{ok, Context} = erlzmq:context(),


init([Context]) ->

    {ok, { {one_for_one, 5, 10}, [
        {zq_pool_sup, {zq_pool_sup, start_link, ['worker_handler',10,Context,["inproc://workers.jobs","inproc://workers.jobs#2"]]}, permanent, 2000, supervisor, [zq_pool_sup]}
    ]} }.
```

Parameters to the pool:
- 'worker_handler' is the callback module where all jobs are sent one they have been received. The only function that it needs to implement is the handle/1 function.
- 10 is the number of worker processes to spawn.
- Context is the erlzmq2 context that was created using `erlzmq:context()`.
- The ending list, is a list of Endpoints that the socket should connect to, these do not need to be the `inproc://` protocol, they can be any that zeromq supports.

# callback function definition:



Jobs submitted to the queue have a minimum of three messages. The first two messages should describe the work that needs to be done. For example, if our zmq workers are interacting with a key value store the three messages might be these:

```
ok = erlzmq:send(Socket,<<"insert">,[sndmore]),
ok = erlzmq:send(Socket,<<"foo">>,[sndmore]),
ok = erlzmq:send(Socket,<<"bar">>,[]),
```

or to wrap it up nicer

```
qz_pool:submit(Socket,[<<"insert">>,<<"foo">>,<<"bar">>]),
```

which will just turn around and call erlzmq:send for each member of the list.

The job would be sent to the callback module as:

```
handle({message,<<"insert">>,<<"foo">>,<<"bar">>}) -> ok.
```

Also jobs are not limited to sending only 3 messages, 3 is the minimun, but the maximun is as large as you would like. So taking the key value store idea again, image we are submitting to a list. Instead of sending :

```
qz_pool:submit(Socket,[<<"append">>,<<"foo">>,<<"bar">>]),
qz_pool:submit(Socket,[<<"append">>,<<"foo">>,<<"bar1">>]),
```

for each member of the list that we need to add we can just write:

```
qz_pool:submit(Socket,[<<"append">>,<<"foo">>,<<"bar">>,<<"bar">>,...]),
```

which will generate a large job, but will avoid the overhead of sening so many messages.

The zq_pool_worker will store off the first two messages and then reuse them for all of the rest of the messages that are part of the job. It will then feed the rest of the messages to the call back module one by one.

So:

```
[Cmd= <<"append">>,Key | Values] = AllMessages,

[CallbackModule:handle({message,Cmd,Key,Value}) || Value <- Rest],
% or
lists:foreach(fun(Value) -> CallbackModule:handle({message,Cmd,Key,Value}) end,Values),
```

## Warning

The only thing that needs to be taken care of by the developer is the zmq context, if this context is not shutdown correctly when the process that created it terminates. The whole erlang VM can hang as a secondary thread is created so not to block the erlang VM whiel performing zmq operations.