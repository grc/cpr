%% store.erl
%%
%% A supervisor for a resilient shopping cart, implemented in `cart2'.


-module(store).


% Internal implementation details
-export([init/1]).


% Implementation

init(Prices) ->
    io:format("store - initialising~p~n", [self()]),
    process_flag(trap_exit,true),
    loop(Prices, []).


%% Supervisor resilience: State is stored in a list so is lost if the
%% supervisor crashes.  An alternative would be to store it in a named
%% DETS table, allowing state to be recovered.  Should the supervisor
%% be lost through network partition we'd be no better off.  See
%% written part of assignment for discussion.

loop(Prices, State) ->
    io:format("store - looping with state: ~p~n", [State]),
    receive
	{TransId, Pid, stop} ->
	    io:format("store - received stop from ~p~n", [Pid]),
	    %% Stop all our carts:
	    lists:foreach(fun ({_Name, _Ref, CartPid}) ->
				      io:format("Stopping ~p~n", [CartPid]),
				      CartPid ! {stop, TransId, self() } end, State),
	    reply(Pid, TransId, ok);

	{TransId, Pid, {start_link, UserName, Node }} ->
	    io:format("store - received start link~n"),
	    Ref = list_to_atom(cart2:registered_name(make_ref())),
	    reply(Pid, TransId, {ok, Ref}),
	    NewPid = new_cart(UserName, Prices, Ref, Node),
	    loop(Prices, [{UserName, Ref, NewPid} |State]);

	{TransId, Pid, {start_backup, Ref, Node}} ->
	    io:format("store - received start_backup~n"),
	    {UserName, Ref, _Pid} = lists:keyfind(Ref, 2, State),
	    reply(Pid, TransId, {ok,Ref}),
	    NewPid = new_cart(UserName, Prices, Ref, Node),
	    loop(Prices, [{UserName, Ref, NewPid}|State]);

	{'EXIT', Pid, normal} ->
	    io:format("store: ~p exited normally~n", [Pid]),
	    loop(Prices, lists:keydelete(Pid, 3, State));

	{'EXIT', Pid, Reason} ->
	    io:format("store: ~p exited with reason ~p~n", [Pid, Reason]),
	    {UserName, Ref, Pid} = lists:keyfind(Pid, 3,State),
	    NewPid =  restart_cart(UserName, Prices, Ref),
	    NewState = lists:keyreplace(Pid, 3, State, {UserName, Ref, NewPid}),
	    loop(Prices,NewState)
	
    end.


reply(Pid, TransId, Message) ->
    Pid ! {reply, TransId, Message}.

    
new_cart(Name, Prices, Ref, Node) ->
    spawn_link(Node, cart2, init, [Name, Prices, Ref]).



%% restart_cart: we don't allow the caller to choose a node as they'd
%% probably specify the original and that may have gone away. Could
%% round robin amongst all connected nodes but for now just limit to
%% this node.

restart_cart(Name, Prices, Ref) ->
    spawn_link(cart2, init, [Name, Prices, Ref, restart]).




    







    

