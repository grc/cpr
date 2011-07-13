%%% webclient 
%%%
%%% Stub implementation of webclient output module for testing
%%% purposes.


-module(webclient).
-export([reply/2]).

reply(UserName,Message) ->
    io:format("Shopping cart for ~p: ~p~n", [UserName, Message]).
