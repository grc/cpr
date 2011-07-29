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




-module(cart2).




%% Private interfaces
-export([init/3]).


%% Implementation

%% init/3 - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of
%% the cart application though the requisite API helper functions will
%% have to be added.  `Ref' is a unique ID for this transaction.

init(UserName, Prices, Ref) ->
    ProcName = list_to_atom(registered_name(Ref)),
    Customer = string_from_ref("Customer-",Ref),
    Order = string_from_ref("User-", Ref),
    Names = {ProcName, Customer, Order}
	 
    case register(ProcName, self()) of
    	true -> 
    	    init(UserName, Prices, Names, master);
    	_Else ->
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

    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    {ok, Order} = dets:open_file(Order, []),
    dets:insert(Order, InitialOrderLines),


    {ok, Customer} = dets:open_file(Customer, []),
    dets:insert(Customer, {name, UserName}),
    loop([Customer,Order],Prices);

init(_UserName, Prices, Names, slave) ->
    io:format("cart2 (~p): initialising slave~n", [self()]),
    {ProcName ,Customer, Order} = Names,
    case whereis(ProcName) of
	undefined ->
	    %%	Master died before we got here, attempt a takeover.
	    %%	Race with supervisor starting another cart.
	    case register(ProcName, self()) of
		true -> io:format("cart2 (~p): successful take over~n", [self()]);
		_Else -> io:format("cart2 (~p): remaining slave~n", [self()])
	    end;
	Pid ->
	    %% Because we're trapping exits the following link is guaranteed
	    %% to succeed, but we may later receive a noproc EXIT reason.
	    true = link(Pid)
    end,

    {ok, Customer} = dets:open_file(Customer, []),
    {ok, Order} = dets:open_file(Order, []),
    loop([Customer, Order], Prices).






loop( Tables, Prices) ->
    io:format("cart {~p) looping~n", [self()]),
    receive
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



%%% close - close the DETS tables passed in, then delete the
%%% underlying file.
close(Tables) ->
    lists:map(fun (T) ->
		      io:format("Cart: closing table ~p~n", [T]),
		      ok = dets:close(T),
		      ok = file:delete(T) end, Tables).


			    
		 

	    
registered_name(Ref) ->
    string_from_ref("Cart-", Ref).

string_from_ref(Prefix, Ref) ->
    lists:flatten( io_lib:format("~p~p", [Prefix, Ref])).


