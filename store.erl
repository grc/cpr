%%% store
%%%
%%% Provides a singe point of contact for API calls, farming them out
%%% to the relevant cart instance.  Should a cart process terminate, a
%%% new one will be started in its place.

-module(store).

% Public interface
-export([start/0, stop/0]).

-export([start_link/1, donuts/2, macarons/2, danish/2, cupcakes/2, view_cart/1,
	billing_address/2, credit_card/3, buy/1]).

% Testing interface
-export([invalid_order/2]).

% Internal implementation details
-export([init/1]).


% Interface definitions


start() -> 
    Prices = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}],
    register(?MODULE, spawn(?MODULE, init, [Prices])).

stop() ->
    send(stop).

start_link(UserName) ->
    case whereis(?MODULE) of % Ensure we've started the supervisor
	undefined ->
	    start();
	_Else -> ok
    end,
    
    send({start_link, UserName}).


donuts(ReferenceId, N) -> 
    async_send(ReferenceId, {order, donuts, N}).

macarons(ReferenceId, N) -> 
    async_send(ReferenceId, {order, macarons, N}).

danish(ReferenceId, N) -> 
    async_send(ReferenceId, {order, danish, N}).

cupcakes(ReferenceId, N) -> 
    async_send(ReferenceId, {order, cupcakes, N}).

%% Order a non existent item for testing purposes
invalid_order(ReferenceId, N) ->
    async_send(ReferenceId, {order, green_cheese, N}).

view_cart(ReferenceId) ->
    sync_send(ReferenceId, view).
    
billing_address(ReferenceId, Address) ->
    sync_send(ReferenceId, {address, Address}).

credit_card(ReferenceId, Number, Date) ->
    sync_send(ReferenceId,{credit, Number, Date}).

buy(ReferenceId) ->
    sync_send(ReferenceId, buy).

%% API Impl

async_send(RefId, Message) ->
    send(request, RefId, Message).

sync_send(RefId, Message) ->
    send(sync_request, RefId, Message).


%%% Time outs on send: The API is intended to hide the fact that a
%%% server might die and be restarted.  A race condition exists where
%%% one message might crash the server and a second be in flight
%%% before the supervisor is notified of the death.  In that case the
%%% message will be lost, so a time out and retry mechanism is
%%% required.



send(Sync, RefId, Message) ->
    send(Sync, RefId, Message, 0).


%% TODO file static values

%% TODO need to add a unique marker to message as they may pass in flight

send(Sync, RefId,Message, RetryCount) when RetryCount < 3  ->
    try 
	RefId ! {Sync, self(), Message} of 
	_ ->
	    io:format("Sent ~p, ~p~n", [Sync, Message]),
	    receive
		{reply, Reply} -> Reply
	    after
		500 -> io:format("Retrying~n"),
		    send(Sync,RefId, Message, RetryCount +1)
	    end
    catch
	%% Process not yet registered, pause a bit and retry
	error: Error  ->
	    io:format("Send error: ~p, ~p, ~p~n ", [Error, RefId, Message])
		,
	    timer:sleep(100),
	    send(Sync, RefId, Message, RetryCount +1)
    end.


    
send(Message) ->
    ?MODULE ! {self(),Message},
    io:format("Sent ~p~n", [Message]),
    receive
	{reply, Reply} ->
	    Reply
    end.
    




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
	    %% Stop to all our carts:
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




    







    

