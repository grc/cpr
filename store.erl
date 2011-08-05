%%% store
%%%


-module(store).


% Internal implementation details
-export([init/1]).


% Implementation

init(Prices) ->
    io:format("store - initialising~n"),
    process_flag(trap_exit,true),
    loop(Prices, []).


loop(Prices, State) ->
    io:format("store - looping with state: ~p~n", [State]),
    receive
	{Pid, stop} ->
	    io:format("store - received stop from ~p~n", [Pid]),
	    %% Stop all our carts:
	    lists:foreach(fun ({_Name, _Ref, CartPid}) ->
				      io:format("Stopping ~p~n", [CartPid]),
				      CartPid ! {stop, self() } end, State),
	    Pid ! {reply,ok};
	{Pid, {start_link, UserName, Node }} ->
	    Ref = list_to_atom(cart2:registered_name(make_ref())),
	    Pid ! {reply, {ok, Ref}},
	    NewPid = new_cart(UserName, Prices, Ref, Node),
	    loop(Prices, [{UserName, Ref, NewPid} |State]);

	{Pid, {start_backup, Ref, Node}} ->
	    io:format("State: ~p, Ref: ~p~n", [State, Ref]),
	    {UserName, Ref, _Pid} = lists:keyfind(Ref, 2, State),
	    Pid ! {reply, {ok, Ref}},
	    NewPid = new_cart(UserName, Prices, Ref, Node),
	    loop(Prices, [{UserName, Ref, NewPid}|State]);

	{'EXIT', Pid, normal} ->
	    io:format("store: ~p exited normally~n", [Pid]),
	    loop(Prices, lists:keydelete(Pid, 3, State));

	{'EXIT', Pid, Reason} ->
	    io:format("store: ~p exited with reason ~p~n", [Pid, Reason]),
	    {UserName, Ref, Pid} = lists:keyfind(Pid, 3,State),
	    NewPid =  restart_cart(UserName, Prices, Ref),
	    io:format("store: spawning new cart ~p~n", [NewPid]),
	    NewState = lists:keyreplace(Pid, 3, State, {UserName, Ref, NewPid}),
	    loop(Prices,NewState);

	Unknown ->
	    io:format("Store received unexpected message: ~p~n", [Unknown])
	
    end.

    
new_cart(Name, Prices, Ref, Node) ->
    spawn_link(Node, cart2, init, [Name, Prices, Ref]).



%% restart_cart: we don't allow the caller to choose a node as 
%% they'd probably specify the original and taht may have gone away.

restart_cart(Name, Prices, Ref) ->
    spawn_link(cart2, init, [Name, Prices, Ref, restart]).




    







    

