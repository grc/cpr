%%% cart2
%%% The resilient version of cart.
%%% A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart2).




%% Private interfaces
-export([init/3]).




%% Implementation

%% init - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of
%% the cart application though the requisite API helper functions will
%% have to be added.  `UserTable' and `OrderTable' contain file names
%% to be used for persistent staorage of user and order data
%% respectively.

init(UserName, Prices, {UserTable, OrderTable}) ->
    io:format("cart2 (~p) - initialising with price list ~p~n", 
	      [UserName, Prices]),
    %% Cart is initialised with a zero count order list drawn from the
    %% set of valid items in the price list.
    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    dets:open_file(OrderTable, []),
    dets:insert(OrderTable, InitialOrderLines),

    dets:open_file(UserTable, []),
    dets:insert(UserTable, {name, UserName}),
    loop({UserTable,OrderTable}, Prices).


%% OrderLines format.  Now that we'reusing a DETS table, which
%% defaults to a set, the order lines format of {Item, Subtotal} is
%% wholly justified.



loop(Tables, Prices) ->
    io:format("cart {~p) looping~n", [self()]),
    receive
	{stop, Pid} -> 
	    {UserTable, OrderTable} = Tables,
	    io:format("Cart ~p: stopping~n", [self()]),
	    close([UserTable, OrderTable]),
	    reply(Pid, ok);
	{request, Pid, Message} ->
	    reply(Pid,ok),
	    {_UserTable, OrderTable} = Tables,
	    ok = request(Message, OrderTable),
	    loop(Tables, Prices);
	{sync_request, Pid, Message} ->
	    io:format("received sync_request: ~p~n",[Message]),
	    {UserTable, OrderTable} = Tables,
	    case sync_request(Message, UserTable, OrderTable, Prices) of 
		{ok, Response} ->
		    reply(Pid,Response),
		    loop(Tables, Prices);
		{stop, Response} ->
		    %% We're going to go away so need to tidy up our
		    %% persistent storage.
		    io:format("Cart ~p: stopping~n", [self()]),
		    close([UserTable, OrderTable]),
		    reply(Pid,Response)
		%Unexpected -> io:format("cart2:Unexpected case clause :~p~n", [Unexpected])
	    end;
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





request({order,Item,N},Table) ->
    order(Item,N,Table).

sync_request(view, _UserTable, OrderTable, Prices) ->
    invoice(OrderTable,Prices);
sync_request({credit, Number,Date}, UserTable, _OrderTable, _Prices) ->
    set_credit_card(Number,Date, UserTable);
sync_request({address,Address}, UserTable, _OrderTable, _Prices) ->
    set_address(Address,UserTable);
sync_request(buy, UserTable, OrderTable, Prices) ->
    buy(UserTable, OrderTable, Prices);
sync_request(Unknown, _A, _B, _C) ->
    io:format("Unexpected sync_request: ~p~n", [Unknown]).


    






reply(Pid, Message) ->
    Pid ! {reply, Message}.



%% set_address
set_address(UserTable,Address) ->
    io:format("setting address to ~p~n", [Address]),
    dets:insert(UserTable, {address, Address}). % TODO error cases

address(Table) ->
    [Address] = dets:lookup(address, Table),
    Address.

set_credit_card(UserTable,Number,Date) ->
    case cc:is_valid(address(UserTable), Number, Date) of
	true ->  
	    dets:insert({number, Number}, UserTable),
	    dets:insert({date, Date}, UserTable),
	    {ok, ok};
	false -> {ok, {error, card_invalid}}
    end.
	



%% Valid buy requests have defined values for a user's address and
%% credit card info.  In that case the shopping basket is closed once
%% order confirmation is sent,


buy(UserTable, OrderTable, Prices) ->
    {Address, Number, Date} = user_details(UserTable),
    Total = order_total(OrderTable,Prices),
    case cc:transaction(Address, Number, Date, Total) of
	{ok, _TrxId} ->
	    %% Transaction succesful, signal that we're done
	    {stop, {ok,invoice(OrderTable,Prices)}};
	{error, _Reason} -> 
	    %% Transaction failed, retain state for another go.  
	    {ok, {error, credit_info}}
    end.



%% Given current set of orders and price list, return the orders and
%% overall value as per API spec.
invoice(OrderTable, Prices) ->    
    io:format("calculating invoice~n"),
    OrderLines = dets:foldl(fun (Elem, Acc) -> [Elem | Acc] end, 
			    [],
			    OrderTable),
    {ok, { OrderLines, order_total(OrderTable,Prices)}}.

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




%%% user_details - returns credit card details from the given table.
user_details(Table) ->
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


			    
		 

	    
    


