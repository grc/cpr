%%% cart A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart).

%% API
-export([start_link/1, donuts/2, macarons/2, danish/2, cupcakes/2, view_cart/1,
	billing_address/2, credit_card/3, buy/1]).


%% Private interfaces
-export([init/2, test/0]).



%% User record
%% We elect to use a record rather than tuple to represent the user
%% as it is anticipated that other user specific data will be added as
%% time goes by: discount rates, mail shot category etc.
-record(user, {name, address, card_number, card_date}).



%% API Definitions


%% Whilst reference ID uniquely identifies the user as required by the
%% spec, there is no constraint on the number of RefID's associated
%% with a given user.

start_link(UserName) ->
    Prices = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}],
    ReferenceId = spawn(?MODULE, init, [UserName, Prices]),
    {ok, ReferenceId}.

donuts(ReferenceId, N) -> 
    send(ReferenceId, {order, donuts, N}).

macarons(ReferenceId, N) -> 
    send(ReferenceId, {order, macarons, N}).

danish(ReferenceId, N) -> 
    send(ReferenceId, {order, danish, N}).

cupcakes(ReferenceId, N) -> 
    send(ReferenceId, {order, cupcakes, N}).

view_cart(ReferenceId) ->
    sync_send(ReferenceId, view).
    
billing_address(ReferenceId, Address) ->
    sync_send(ReferenceId, {address, Address}).

credit_card(ReferenceId, Number, Date) ->
    sync_send(ReferenceId,{credit, Number, Date}).

buy(ReferenceId) ->
    sync_send(ReferenceId, buy).


%% API Impl
send(Pid, Message) ->
    Pid ! {request, self(), Message},
    receive
	{reply, Reply} ->
	    Reply
    end.

sync_send(Pid, Message) ->
    Pid ! {sync_request, self(), Message},
    receive
	{reply, Reply} ->
	    Reply
    end.


%% Implementation

%% init - The cart is initialised with a price list `Prices' a list
%% [{Item,Price}] defining all the items which may be purchased.  This
%% allows new items to be added without having to modify the core of the cart
%% application though the requisite API helper functions will have to be added.

init(UserName, Prices) ->
    io:format("cart (~p) - initialising with price list ~p~n", 
	      [UserName, Prices]),
    %% Cart is initialised with a zero count order list drawn from the
    %% set of valid items in the price list.
    InitialOrderLines = [ {Item, 0}||{Item, _Price} <- Prices],
    loop(#user{name=UserName}, InitialOrderLines, Prices).


%% OrderLines format.  This could be kept as a list of order items, to
%% be tallied when the client issues a `buy' command.  The behaviour
%% of item removal, where removing three macarons from a basket
%% containing 1 returns 0 macarons, suggests that a better model is to
%% maintain a running total for each order category.
%%
%% OrderLines and Prices are both stored as lists, though in each the
%% first member of the tuple, Item, should always be unique.  Given
%% the small size of the list in both cases, lists are prefered over
%% sets for simplicity.


loop(User, OrderLines, Prices) ->
    io:format("cart (~p) - looping with state ~p~n", [User, OrderLines]),
    receive
	{request, Pid, Message} ->
	    reply(Pid,ok),
	    NewOrderLines = request(Message, User,OrderLines),
	    loop(User, NewOrderLines, Prices);
	{sync_request, Pid, Message} ->
	    
	    case sync_request(Message, User, OrderLines,Prices) of 
		{NewUser,NewOrderLines,Response} ->
		    reply(Pid,Response),
		    loop(NewUser, NewOrderLines, Prices);
		{stop, Response} ->
		    reply(Pid,Response)
	    end;
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





request({order,Item,N},User, OrderLines) ->
    order(User, Item,N,OrderLines).

sync_request(view, User, OrderLines, Prices) ->
    {User,OrderLines,invoice(OrderLines,Prices)};
sync_request({credit, Number,Date}, User, OrderLines,_Prices) ->
    {NewUser, Response}=set_credit_card(User,Number,Date),
    {NewUser, OrderLines, Response};
sync_request({address,Address}, User, OrderLines,_Prices) ->
    {NewUser,Response} = set_address(User,Address),
    {NewUser,OrderLines,Response};
sync_request(buy, User, OrderLines,Prices) ->
    buy(User,OrderLines,Prices).

    






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

buy(#user{address=undefined} =U, OrderLines, _Prices) ->
    {U, OrderLines, {error, billing_info}}; 
buy(U, OrderLines,Prices) ->
    case cc:transaction(U#user.address, 
			U#user.card_number, 
			U#user.card_date, 
			order_total(OrderLines,Prices)) of
	{ok, _TrxId} ->
	    %% Transaction succesful, signal that we're done
	    {stop, {ok,invoice(OrderLines,Prices)}};
	{error, _Reason} -> 
	    %% Trandsaction failed, retain state for another go.
	    {U, OrderLines, {error, credit_info}}
    end.



%% Given current set of orders and price list, return the orders and
%% overall value as per API spec.
invoice(OrderLines, Prices) ->    
{ OrderLines, order_total(OrderLines,Prices)}.

order_total(Orders,Prices) ->
    %% Calculate price of each line item in `Orders', looking up
    %% corresponding price in `Prices'.
    SubTotals = lists:map(fun({I,N}) -> 
				   {I,P} = lists:keyfind(I,1,Prices), 
				   P*N end,
			  Orders),
    %% And the order total is the sum of the sub totals...
    lists:sum(SubTotals).
    
    
 %% order - > returns new order
order(User, Item, N, Order) ->
    {Item, Quantity} = lists:keyfind(Item, 1, Order),
    [{Action, Q1}, {total, Q2}] = modify_item( N, Quantity),
    Reply=io_lib:format("~p ~p ~p, Total number of ~p: ~p.~n", 
			[Action, Q1, Item, Item, Q2]),
    io:format(Reply),
    %webclient:reply(User,Reply),
    lists:keyreplace(Item, 1, Order, {Item, Q2}).



modify_item( Delta, Current) when Delta + Current > 0 ->
    case Delta >= 0 of
	true -> [{added, Delta}, {total, Delta+Current}];
	false -> [{removed, Delta}, {total, Delta + Current}]
    end;
modify_item( _Delta, Current) ->
    [{removed, Current}, {total, 0}].









test() ->
    {ok,TestPid} = start_link(my_user),
    ok = macarons(TestPid, 3),
    ok = macarons(TestPid, -2),
    ok = macarons(TestPid, -3),
    ok = donuts(TestPid,3),
    {[{donuts, 3}, {macarons,0}, {danish,0}, {cupcakes,0}], 150} = 
	view_cart(TestPid),
    {error, billing_info} = buy(TestPid),
    test_passed.

			    
		 

	    
    


