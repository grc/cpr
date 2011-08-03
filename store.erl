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
	{Pid, {start_link, UserName} } ->
	    Ref = cart2:registered_name(make_ref()),
	    Pid ! {reply, {ok, list_to_atom(Ref)}},
	    %% Spin off two carts, one to be master, one slave.
	    %% TODO, need to externalise this.
	    MasterPid = new_cart(UserName, Prices, Ref),
	    %% TODO - spawn slave off on a different node.
	    SlavePid = new_cart(UserName, Prices, Ref),
	    loop(Prices, [{UserName, Ref, MasterPid} |
			  [{UserName, Ref, SlavePid} |State]]);
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

    
new_cart(Name, Prices, Ref) ->
    spawn_link(cart2, init, [Name, Prices, Ref]).


restart_cart(Name, Prices, Ref) ->
    spawn_link(cart2, init, [Name, Prices, Ref, restart]).




    







    

