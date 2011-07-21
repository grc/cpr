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


% Internal implementation details
-export([init/1, test/0, test_start_stop/0]).


% Interface definitions


start() -> 
    Prices = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}],
    register(?MODULE, spawn(?MODULE, init, [Prices])).

stop() ->
    send(stop).

start_link(UserName) ->
    % TODO - needs to handle unstarted server
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

send(Sync, RefId,Message) ->
    ?MODULE ! {Sync, self(), RefId, Message},
     receive
	{reply, Reply} ->
	    Reply
     end.
    
send(Message) ->
    ?MODULE ! {self(),Message},
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
	{Pid, {start_link, UserName} } -> % duplicate username?
	    Ref = make_ref(),
	    Pid ! {reply, {ok, Ref}},
	    CartPid = new_cart(UserName, Prices, Ref),
	    loop(Prices, [{UserName, Ref, CartPid} |State]);
	{'EXIT', Pid, Reason} ->
	    io:format("store: ~p exited with reason ~p~n", [Pid, Reason]);
	    % {UserName, Ref, Pid} = lists:keyfind(Pid, 3,State),
	    % TODO need to store state of cart in persistent storage
	    %NewPid =  new_cart(UserName, Prices, Ref),
	    %NewState = lists:keyreplace(Pid, 3, State, {UserName, Ref, NewPid}),
	    %loop(Prices,NewState);

	{Sync, Pid, Ref, Message} ->
	    io:format("store received ~p for ~p~n", [Message, Ref]),
	    {_UserName, Ref, CartPid} = lists:keyfind(Ref, 2, State),
	    CartPid ! {Sync, Pid, Message},
	    loop(Prices,State)
	
    end.

close_cart({_Name, Ref, Pid}) ->
    Pid ! {self(), stop}.
    
new_cart(Name, Prices, Ref) ->
    spawn_link(cart2, init, [Name, Prices, tables_from_ref(Ref)]).

%% tables_from_ref - generates unique names for DETS tabled keyed off
%% a reference.  Returned as a list so that the cart can handle the
%% collection as a whole.z
tables_from_ref(Ref) ->
    [io_lib:format("User-~p", [Ref]),
     io_lib:format("Order-~p", [Ref])].

    


% Currently storing {User, Ref, Pid} in list.  Expect to have to
% deal with large numbers of users so use ETS?


%% Test harness

test_invalid_order() ->
    start(),
    {ok, Ref} = start_link(fred),
    donuts(Ref,2),
    invalid_order(Ref, 2),
    stop(), timer:sleep(250).
    

test_start_stop() ->
    io:format("~nTEST: start_stop~n"),
    start(),
    {ok, Ref} = start_link(benny),
    stop(), timer:sleep(250).


test_minimal_order() ->
    io:format("~nTEST: minimal_order~n"),
    start(),
    {ok, Ref} = start_link(fred),
    ok = donuts(Ref,2),
    Expected = {[{macarons,0},{cupcakes,0},{danish,0},{donuts,2}],100},
    Actual = view_cart(Ref),
    Expected = Actual,
    timer:sleep(300),
    stop(), timer:sleep(250).
    
test() ->
    test_start_stop(),
    test_empty_view(),
    test_minimal_order(),
    test_succeeded.

test_empty_view() ->
    io:format("~nTEST: empty_view~n"),
    start(),
    {ok, Ref} = start_link(fred),
    Expected = {[{macarons,0},{cupcakes,0},{danish,0},{donuts,0}],0},
    Actual = view_cart(Ref),
    Expected = Actual,
    timer:sleep(300),
    stop(), timer:sleep(250).
    
    


