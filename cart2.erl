%%% cart2
%%% The resilient version of the shopping cart.

%%% On creation with a unique reference ID the cart attempts to
%%% register itself.  If that succeeds then it is the master,
%%% otherwise it's the slave.  Slave's link to their master and if
%%% they detect abnormal termination they register their own PID
%%% against the reference ID.
%%%
%%% State is shared between master and slave through shared knowledge
%%% of the name of the DETS tables in use. One of these tables will
%%% contain persisted credit card data.  This needs to be secured as
%%% otherwise another erlang process can simple recover that data.





%% TODO Use monitor to watch the primary cart.

-module(cart2).




%% Private interfaces
-export([init/3]).

-export([registered_name/1, string_from_ref/2]).


%% Implementation

%% init/3 - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of
%% the cart application though the requisite API helper functions will
%% have to be added.  `Ref' is a unique ID for this transaction.

init(UserName, Prices, Ref) ->
    ProcName = list_to_atom(Ref),
    Customer = string_from_ref("Customer-",Ref),
    Order = string_from_ref("User-", Ref),
    Names = {ProcName, Customer, Order},
    try register(ProcName, self()) of
    	true -> init(UserName, Prices, Names, master)
    catch
    	error:_Error ->
    	    init(UserName, Prices, Names, slave)
    end.


%% init/4 - master variant sets up initial state in the DETS tables,
%% slave variant just opens them.
%% Slave variant will link to the master so it can take over in the event 
%% of termination.


init(UserName, Prices, Names, master) ->
     io:format("cart2 (~p) - initialising master~n", 
	      [self()]),
    {_ProcName,Customer, Order} = Names,
    Tables = [Customer,Order],
    open(Tables),

    dets:insert(Customer, {name, UserName}),

    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    dets:insert(Order, InitialOrderLines),

    loop(Tables,Prices);

init(_UserName, Prices, Names, slave) ->
    io:format("cart2 (~p): initialising slave~n", [self()]),
    {ProcName ,Customer, Order} = Names,
    Tables = [Customer,Order],
    open(Tables),
    
    % start monitoring the master.  If it has already died we'll still
    % get a DOWN message.  Use monitor instead of link as we want an
    % asymmetrical connection.
    monitor(process, ProcName),
    
    loop([Customer, Order], Prices).



takeover(ProcName) ->
    %%	Race condition with supervisor starting another cart so be prepared
    %%	for register to fail:

    try register(ProcName, self()) of
	true -> 
	    io:format("cart2 (~p): successful take over~n", [self()])
	    % Don't need to demonitor as that happens automatically
	    % when the DOWN message is sent.

    catch	
	error:_Error -> io:format("cart2 (~p): remaining slave~n", [self()]),
			monitor(process,ProcName)
    end.
    


loop( Tables, Prices) ->
    io:format("cart {~p) looping~n", [self()]),
    receive
	{'DOWN', _Ref, process, {ProcName, _Node}, Info} ->
	    io:format("Master down, ~p~n", [Info]),
	    takeover(ProcName),
	    loop(Tables, Prices);
	{stop, Pid} -> 
	    io:format("Cart ~p: stopping~n", [self()]),
	    close(Tables),
	    reply(Pid, ok);
	{request, Pid, Message} ->
	    reply(Pid,ok),
	    [_Customer, Order] = Tables,
	    ok = request(Message, Order),
	    loop(Tables, Prices);
	{sync_request, Pid, Message} ->
	    io:format("received sync_request: ~p~n",[Message]),
	    case sync_request(Message, Tables, Prices) of 
		{ok, Response} ->
		    reply(Pid,Response),
		    loop(Tables, Prices);
		{stop, Response} ->
		    %% We're going to go away so need to tidy up our
		    %% persistent storage.
		    io:format("Cart ~p: stopping~n", [self()]),
		    close(Tables),
		    reply(Pid,Response)
		%Unexpected -> io:format("cart2:Unexpected case clause :~p~n", [Unexpected])
	    end;
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





request({order,Item,N},Table) ->
    order(Item,N,Table).

sync_request(view, [_Customer, Order], Prices) ->
    invoice(Order,Prices);
sync_request({credit, Number,Date}, [Customer, _Order], _Prices) ->
    set_credit_card(Number,Date, Customer);
sync_request({address,Address}, [Customer, _Order], _Prices) ->
    set_address(Address,Customer);
sync_request(buy, Tables, Prices) ->
    buy(Tables, Prices);
sync_request(Unknown, _A, _C) ->
    io:format("Unexpected sync_request: ~p~n", [Unknown]).


    






reply(Pid, Message) ->
    Pid ! {reply, Message}.



%% set_address
set_address(Customer,Address) ->
    io:format("setting address to ~p~n", [Address]),
    dets:insert(Customer, {address, Address}). % TODO error cases

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
order(Item, N, Table) ->
    io:format("Processing an order for ~p ~p~n", [N, Item]),
    [{Item, Quantity}] = dets:lookup(Table, Item),
    io:format("Item found"),
    [{Action, Q1}, {total, Q2}] = modify_item( N, Quantity),
    Reply=io_lib:format("~p ~p ~p, Total number of ~p: ~p.~n", 
			[Action, Q1, Item, Item, Q2]),
    io:format(Reply),
    %webclient:reply(User,Reply),
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
    lists:map(fun(T) ->
		      {ok, T} = dets:open_file(T, [])
	      end,
	      Tables).

%%% close - close the DETS tables passed in, then delete the
%%% underlying file.
close(Tables) ->
    lists:map(fun (T) ->
		      ok = dets:close(T),
		      %% Don't check the return value of delete as
		      %% there is no guarantee the file is actually
		      %% created: empty tables may not be written to
		      %% disk.
		      file:delete(T) end, 
	      Tables).


			    
		 

	    
registered_name(Ref) ->
    string_from_ref("Cart-", Ref).

string_from_ref(Prefix, Ref) ->
    lists:flatten( io_lib:format("~p~p", [Prefix, Ref])).


