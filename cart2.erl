%%% cart2
%%% The resilient version of cart.
%%% A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart2).




%% Private interfaces
-export([init/3]).



%% User record
%% We elect to use a record rather than tuple to represent the user
%% as it is anticipated that other user specific data will be added as
%% time goes by: discount rates, mail shot category etc.
-record(user, {name, address, card_number, card_date}).



%% API Definitions


%% Whilst reference ID uniquely identifies the user as required by the
%% spec, there is no constraint on the number of RefID's associated
%% with a given user.





%% Implementation

%% init - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of the cart
%% application though the requisite API helper functions will have to be added.


%% TODO, should store user info in separate table from order lines
init(UserName, Prices, Table) ->
    io:format("cart2 (~p) - initialising with price list ~p~n", 
	      [UserName, Prices]),
    %% Cart is initialised with a zero count order list drawn from the
    %% set of valid items in the price list.
    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    dets:insert(Table, InitialOrderLines),
    dets:insert(Table, {name, UserName}),
    loop(Table, Prices).


%% OrderLines format.  Now that we'reusing a DETS table, which
%% defaults to a set, the order lines format of {Item, Subtotal} is wholly justified.



loop(Table, Prices) ->
    io:format("cart {~p) looping~n", [self()]),
    receive
	{request, Pid, Message} ->
	    reply(Pid,ok),
	    NewOrderLines = request(Message, Table),
	    loop(Table, Prices);
	{sync_request, Pid, Message} ->
	    case sync_request(Message, Table, Prices) of 
		{ok ,Response} ->
		    reply(Pid,Response),
		    loop(Table, Prices);
		{stop, Response} ->
		    reply(Pid,Response)
	    end;
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





request({order,Item,N},Table) ->
    order(Item,N,Table).

sync_request(view, Table, Prices) ->
    invoice(Table,Prices);
sync_request({credit, Number,Date}, Table, _Prices) ->
    set_credit_card(Number,Date, Table);
sync_request({address,Address}, Table, _Prices) ->
    set_address(Address,Table);
sync_request(buy, Table, Prices) ->
    buy(Table,Prices).

    






reply(Pid, Message) ->
    Pid ! {reply, Message}.

set_address(User,Address) ->
    {User#user{address=Address}, ok}. % TODO error cases

set_credit_card(User,Number,Date) ->
    case cc:is_valid(User#user.address, Number, Date) of
	true ->  {User#user{card_number=Number, card_date=Date}, ok};
	false -> {User, {error, card_invalid}}
    end.
	



%% Valid buy requests have defined values for a user's address and
%% credit card info.  In that case the shopping basket is closed once
%% order confirmation is sent,


buy(Table, Prices) ->
    {Address, Number, Date} = user_details(Table),
    Total = order_total(Table,Prices),
    case cc:transaction(Address, Number, Date, Total) of
	{ok, _TrxId} ->
	    %% Transaction succesful, signal that we're done
	    {stop, {ok,invoice(Table,Prices)}};
	{error, _Reason} -> 
	    %% Transaction failed, retain state for another go.
	    {ok, {error, credit_info}}
    end.



%% Given current set of orders and price list, return the orders and
%% overall value as per API spec.
invoice(OrderLines, Prices) ->    
{ OrderLines, order_total(OrderLines,Prices)}.

order_total(Table,Prices) ->
    %% TODO, Must ensure that only order items in table
    %% Calculate price of each line item in `Table', looking up
    %% corresponding price in `Prices'.
    dets:foldl(fun({I,N}, Acc) ->
		       {I,P} = lists:keyfind(I, 1, Prices),
		       Acc + P*N end,
	       0,
	       Table).
		       
    
    
 %% order - > returns new order
order(Item, N, Table) ->
    {Item, Quantity} = dets:lookup(Table, Item),
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





user_details(Table) ->
    Address = dets:lookup(address, Table),
    Number = dets:lookup(number, Table),
    Date = dets:lookup(date, Table),
    {Address, Number, Date}.




			    
		 

	    
    


