%%% cart2
%%% The resilient version of cart.
%%% A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart2).




%% Private interfaces
-export([init/4]).




%% Implementation

%% init - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of
%% the cart application though the requisite API helper functions will
%% have to be added.  `Customer' and `Order' contain file names
%% to be used for persistent staorage of user and order data
%% respectively.



%% init - start variant sets up initial state in the DETS tables,
%% restart variant just opens them.
%%
%% OrderLines format.  Now that we're using a DETS table, which
%% defaults to a set, the order lines format of {Item, Subtotal} is
%% wholly justified.

init(UserName, Prices, Tables, start) ->
     io:format("cart2 (~p) - initialising with price list ~p~n", 
	      [UserName, Prices]),
    [Customer, Order] = Tables,

    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    {ok, Order} = dets:open_file(Order, []),
    dets:insert(Order, InitialOrderLines),


    {ok, Customer} = dets:open_file(Customer, []),
    dets:insert(Customer, {name, UserName}),
    loop(Tables,Prices);

init(_UserName, Prices, Tables, restart) ->
    io:format("cart2: restarting ~p~n", [self()]),
    [Customer, Order] = Tables,
    {ok, Customer} = dets:open_file(Customer, []),
    {ok, Order} = dets:open_file(Order, []),
    loop(Tables, Prices).






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


			    
		 

	    
    


