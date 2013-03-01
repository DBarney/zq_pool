%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pagodabox.com>
%%% @author Daniel Barney <daniel@pagodabox.com>
%%% @copyright (C) 2013, Pagoda Box, Inc.
%%% @doc
%%%
%%% @end
%%% Created :  28 Feb 2013 by Daniel Barney <daniel@pagodabox.com>
%%%-------------------------------------------------------------------
-module(zq_pool).


%% API
-export([submit/2]).


%% wrapper for submitting jobs to the zq_pool
submit(_Socket,[]) -> ok;
submit(Socket,[Message]) -> 
	erlzmq:send(Socket,Message,[]);
submit(Socket,[Message | Tail]) -> 
	case erlzmq:send(Socket,Message,[sndmore]) of
		ok -> submit(Socket,Tail);
		Other -> Other
	end.