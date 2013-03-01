%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @author Daniel Barney <daniel@pagodabox.com>
%%% @copyright (C) 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  18 Feb 2013 by Andrew Bennett <andrew@pagodabox.com>
%%%-------------------------------------------------------------------
-module(zq_pool_worker).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {handler,
                channel,
                context,
                queue,
                socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

start_link(Handler,Context, Endpoints) ->
    gen_server:start_link(?MODULE, [Handler,Context, Endpoints], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

init([Handler, Context, Endpoints]) ->
    lager:info([{worker, zq}],
               "~s:init. ~p", [?MODULE, Endpoints]),

    %% active false and polling may work better, but i'm not sure we would have to run tests
    {ok, Socket} = erlzmq:socket(Context, [pull, {active, true}]),
    lists:foreach(fun(Endpoint) ->
      ok = erlzmq:connect(Socket, Endpoint)
    end, Endpoints),

    {ok, #state{handler= Handler,
                context = Context,
                socket = Socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({subscribe, Endpoint}, _From, State = #state{socket = Socket}) ->
    ok = erlzmq:connect(Socket, Endpoint),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info(timeout, State) ->
    {noreply, State};

handle_info({zmq, Socket, QueueName, [rcvmore]}, State = #state{socket = Socket,
                                                                queue = undefined}) ->
    {noreply, State#state{queue = QueueName}};

handle_info({zmq, Socket, Channel, [rcvmore]}, State = #state{socket = Socket,
                                                              channel = undefined,
                                                              queue = QueueName}) when QueueName /= undefined ->
    {noreply, State#state{channel = Channel}};



handle_info({zmq, Socket, Message, Opts}, State = #state{socket = Socket,
                                                        handler = Handler,
                                                        channel = Channel,
                                                        queue = QueueName}) 
                                                  when  Channel /= undefined; 
                                                        QueueName /= undefined-> 
    lager:info([{worker, zq}],
               "~s:handle_info. queue: ~s, channel: ~s, message: ~s", [?MODULE, QueueName, Channel, Message]),
    % TODO: send this message off somewhere!
    Handler:handle({message,QueueName,Channel,Message}),
    case Opts of
      [] ->
        %% we clear out the channel and queue because this was the last message sent in the list
        {noreply, State#state{channel = undefined, queue = undefined}};
      [rcvmore] ->
        %% we don't clear out the channel or the queueName because there are more messages associated
        %% that are being sent
        {noreply, State}
    end;
    

handle_info(Info, State) ->
    lager:info([{worker, zq}],
               "~s:handle_info. ignoring: ~p", [?MODULE, Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State = #state{socket = Socket}) ->
    ok = erlzmq:close(Socket).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
