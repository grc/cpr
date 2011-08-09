%%% cart2
%%% The resilient version of the shopping cart.

%%% Distributed communication is based on the `global' module.  The
%%% supervisor, `store' is registered in the global database so that it is
%%% accessible from all nodes. Individual master carts also register
%%% themselves their allowing clients to communicate directly with
%%% them and slave carts to monitor them for continued existence.
%%%
%%% The use of the global registry initialy raised performancd
%%% concerns.  However given that the global registry uses a
%%% gen_server instance backed by an ets table it will scale better
%%% than any home brew effort attempted for this assignment.
%%%
%%% State is shared between master and slave through shared knowledge
%%% of the name of the DETS tables in use. One of these tables will
%%% contain persisted credit card data.  This needs to be secured as
%%% otherwise another erlang process can simple recover that data.
%%% Use of the DETS table leaves us vulnerable to network partitions;
%%% this is discussed in the written portion of the submission.
%%%
%%% A supervisor process is responsible for spawning the carts and
%%% keeping them alive whilst necessary.
%%%


-module(cart2).




%% Private interfaces
-export([init/3, init/4]).

-export([registered_name/1, string_from_ref/2]).

% Public interface
-export([start/0, stop/0]).

-export([start_link/1, start_backup/1, 
	 donuts/2, macarons/2, danish/2, cupcakes/2, 
	 view_cart/1, billing_address/2, credit_card/3, buy/1]).

% Testing interface
-export([invalid_order/2]).


% Interface definitions


%% start/0 creates an instance of the supervisor on the current node.
start() -> 
    Prices = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}],
    global:register_name(?MODULE, spawn(store, init, [Prices])).

stop() ->
    send(stop).

start_link(UserName) ->
    case global:whereis_name(?MODULE) of % Ensure we've started the supervisor
	undefined ->
	    start();
	_Else -> ok
    end,
    
    send({start_link, UserName, node()}).

% start_backup: creates a hot standby cart for the transaction
% identified by `Ref'.  The cart will bre created on the node from
% which this call was made.
start_backup(Ref)->
    send({start_backup, Ref, node()}).

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



%% TODO need to add a unique marker to message as they may pass in flight

send(Sync, RefId,Message, RetryCount) when RetryCount < 3  ->
    TransId = make_ref(),
    try 
	io:format("sending to ~p at ~p~n", [RefId, global:whereis_name(RefId)]),

	global:send(RefId, {Sync, TransId, self(), Message}) of 
	_ ->
	    io:format("Sent ~p, ~p~n", [Sync, Message]),
	    receive
		{reply, TransId, Reply} -> Reply
	    after
		500 -> io:format("Retrying~n"),
		    send(Sync,RefId, Message, RetryCount +1)
	    end
    catch
	%% Process not yet registered, pause a bit and retry
	exit: Error  ->
	    io:format("Send error: ~p, ~p, ~p~n ", [Error, RefId, Message]),
	    timer:sleep(100),
	    send(Sync, RefId, Message, RetryCount +1)
    end.


    
send(Message) ->
    TransId = make_ref(),
    global:send(?MODULE , {TransId, self(),Message}),
    io:format("Sent ~p~n", [Message]),
    receive
	{reply, TransId, Reply} ->
	    Reply
    end.
    










%% Implementation

%% init/3 - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of
%% the cart application though the requisite API helper functions will
%% have to be added.  `Ref' is a unique ID for this transaction.


%% init variants:
%% 
%% init/3 is used to create a new cart.  An attempt to register the
%% process defines the master at startup and hence the process
%% responsible for initialising the order table.

init(UserName, Prices, Ref) ->
    ProcName = Ref,
    Customer = string_from_ref("Customer-",Ref),
    Order = string_from_ref("User-", Ref),
    Names = {ProcName, Customer, Order},
    case global:register_name(ProcName, self()) of
    	yes -> init(UserName, Prices, Names, master);

    	no  -> init(UserName, Prices, Names, slave)
    end.

%% init/4 - master variant sets up initial state in the DETS tables,
%% slave variant just opens them.  restart variant is called when
%% resurrecting a cart.  Slave variant will link to the master so it
%% can take over in the event of termination.


init(UserName, Prices, Names, master) ->
    {ProcName,Customer, Order} = Names,
    io:format("cart2 (~p) - initialising master for ~p~n", 
	      [self(),ProcName]),
    
    Tables = [Customer,Order],
    open(Tables),

    dets:insert(Customer, {name, UserName}),

    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    dets:insert(Order, InitialOrderLines),

    loop(ProcName, Tables,Prices);


init(UserName, Prices, Ref, restart) ->
    ProcName = Ref,
    Customer = string_from_ref("Customer-",Ref),
    Order = string_from_ref("User-", Ref),
    Names = {ProcName, Customer, Order},
    init(UserName, Prices, Names, slave);



init(_UserName, Prices, Names, slave) ->
    io:format("cart2 (~p): initialising slave~n", [self()]),
    {ProcName ,Customer, Order} = Names,
    Tables = [Customer,Order],
    open(Tables),
    
    % start monitoring the master.  If it has already died we'll still
    % get a DOWN message.  Use monitor instead of link as we want an
    % asymmetrical connection.
    monitor(process, global:whereis_name(ProcName)),
    
    loop(ProcName, [Customer, Order], Prices).



takeover(ProcName) ->
    %%	Race condition with supervisor starting another cart so be prepared
    %%	for register to fail:
    io:format("takeover attempt: ~p~n",[global:whereis_name(ProcName)]),
    case global:register_name(ProcName, self()) of
	yes -> 
	    io:format("cart2 (~p): successful take over of ~p~n", [self(), ProcName]);
	    % Don't need to demonitor as that happens automatically
	    % when the DOWN message is sent.

	no -> io:format("cart2 (~p): remaining slave~n", [self()]),
	      monitor(process,global:whereis_name(ProcName))
    end.
    


loop(ProcName, Tables, Prices) ->
    io:format("cart {~p) looping~n", [self()]),
    receive
	{'DOWN', _Ref, process, {_ProcName, _Node}, Info} ->
	    io:format("Cart ~p: Master down, ProcName ~p, Info: ~p~n",
		      
		      [self(), ProcName, Info]),
	    takeover(ProcName),
	    loop(ProcName,Tables, Prices);
	{stop, TransId, Pid} -> 
	    io:format("Cart ~p: stopping~n", [self()]),
	    close(Tables),
	    reply(Pid, TransId, ok);
	{request, TransId, Pid, Message} ->
	    reply(Pid, TransId, ok),
	    [Customer, Order] = Tables,
	    [Name] = dets:lookup(Customer, name),
	    ok = request(Name,Message, Order),
	    loop(ProcName, Tables, Prices);
	{sync_request, TransId, Pid, Message} ->
	    io:format("received sync_request: ~p~n",[Message]),
	    case sync_request(Message, Tables, Prices) of 
		{ok, Response} ->
		    reply(Pid, TransId, Response),
		    loop(ProcName,Tables, Prices);
		{stop, Response} ->
		    %% We're going to go away so need to tidy up our
		    %% persistent storage.
		    io:format("Cart ~p: stopping~n", [self()]),
		    close(Tables),
		    reply(Pid, TransId, Response)
	    end
    end.





request(Name, {order,Item,N},Table) ->
    order(Name, Item,N,Table).

sync_request(view, [_Customer, Order], Prices) ->
    invoice(Order,Prices);
sync_request({credit, Number,Date}, [Customer, _Order], _Prices) ->
    set_credit_card(Number,Date, Customer);
sync_request({address,Address}, [Customer, _Order], _Prices) ->
    set_address(Address,Customer);
sync_request(buy, Tables, Prices) ->
    buy(Tables, Prices).


    






reply(Pid, TransId, Message) ->
    Pid ! {reply, TransId,  Message}.



%% set_address
set_address(Customer,Address) ->
    io:format("setting address to ~p~n", [Address]),
    dets:insert(Customer, {address, Address}). 

address(Customer) ->
    [Address] = dets:lookup(address, Customer),
    Address.

set_credit_card(Customer,Number,Date) ->
    case cc:is_valid(address(Customer), Number, Date) of
	true ->  
	    dets:insert({number, Number}, Customer),
	    dets:insert({date, Date}, Customer),
	    {ok, ok};
	false -> {ok, {error, card_invalid}}
    end.
	



%% Valid buy requests have defined values for a user's address and
%% credit card info.  In that case the shopping basket is closed once
%% order confirmation is sent,


buy([Customer, Order], Prices) ->
    {Address, Number, Date} = credit_details(Customer),
    Total = order_total(Order,Prices),
    case cc:transaction(Address, Number, Date, Total) of
	{ok, _TrxId} ->
	    %% Transaction succesful, signal that we're done
	    {stop, {ok,invoice(Order,Prices)}};
	{error, _Reason} -> 
	    %% Transaction failed, retain state for another go.  
	    {ok, {error, credit_info}}
    end.



%% Given current set of orders and price list, return the orders and
%% overall value as per API spec.
invoice(Order, Prices) ->    
    io:format("calculating invoice~n"),
    OrderLines = dets:foldl(fun (Elem, Acc) -> [Elem | Acc] end, 
			    [],
			    Order),
    {ok, { OrderLines, order_total(Order,Prices)}}.

order_total(Table,Prices) ->
    %% Calculate price of each line item in `Table', looking up
    %% corresponding price in `Prices'.
    io:format("Calculating prices~n"),
    dets:foldl(fun({I,N}, Acc) ->
		       {I,P} = lists:keyfind(I, 1, Prices),
		       Acc + P*N end,
	       0,
	       Table).
		       
    
    
 %% order - > returns new order
order(Name,Item, N, Table) ->
    io:format("Processing an order for ~p ~p~n", [N, Item]),
    [{Item, Quantity}] = dets:lookup(Table, Item),
    io:format("Item found"),
    [{Action, Q1}, {total, Q2}] = modify_item( N, Quantity),
    Reply=io_lib:format("~p ~p ~p, Total number of ~p: ~p.~n", 
			[Action, Q1, Item, Item, Q2]),
    io:format(Reply),
    webclient:reply(Name,Reply),
    dets:insert(Table, {Item, Q2}).



modify_item( Delta, Current) when Delta + Current > 0 ->
    case Delta >= 0 of
	true -> [{added, Delta}, {total, Delta+Current}];
	false -> [{removed, Delta}, {total, Delta + Current}]
    end;
modify_item( _Delta, Current) ->
    [{removed, Current}, {total, 0}].




%%% credit_details - returns credit card details from the given table.
credit_details(Table) ->
    [Number] = dets:lookup(number, Table),
    [Date] = dets:lookup(date, Table),
    {address(Table), Number, Date}.



open(Tables) ->
    [{ok, T} = dets:open_file(T, []) || T <- Tables].

%%% close - close the DETS tables passed in, then delete the
%%% underlying file.
close(Tables) ->
    lists:map(fun (T) ->
		      ok = dets:close(T),
		      %% Don't check the return value of file:delete
		      %% as there is no guarantee the file is actually
		      %% created: empty tables may not be written to
		      %% disk.
		      file:delete(T) end, 
	      Tables).


			    
		 

	    
registered_name(Ref) ->
    string_from_ref("Cart-", Ref).

string_from_ref(Prefix, Ref) ->
    lists:flatten( io_lib:format("~s~w", [Prefix, Ref])).


