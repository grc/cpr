%%% cart A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart).

%% API
-export([start_link/1, donuts/2, macarons/2, danish/2, cupcakes/2, view_cart/1,
	billing_address/2, credit_card/3, buy/1]).


%% Private interfaces
-export([init/2, test/0]).



-record(user, {name, address, card_number, card_date}).

%% Cart is initialised with a price list representing all valid order items

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
init(UserName, Prices) ->
    io:format("cart (~p) - initialising with price list ~p~n", 
	      [UserName, Prices]),
    InitialState = [ {Item, 0, 0}||{Item, _Price} <- Prices],
    loop(#user{name=UserName}, InitialState, Prices).



loop(User, State, Prices) ->
    io:format("cart (~p) - looping with state ~p~n", [User, State]),
    receive
	{request, Pid, Message} ->
	    reply(Pid,ok),
	    {NewUser, NewState} = handle_request(Message, User,State,Prices),
	    loop(NewUser, NewState, Prices);
	{sync_request, Pid, Message} ->
	    
	    case sync_request(Message, User, State, Prices) of 
		{NewUser,NewState,Response} ->
		    reply(Pid,Response),
		    loop(NewUser, NewState, Prices);
		{stop, Response} ->
		    reply(Pid,Response)
	    end;
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





handle_request({order,Item,N}, User,State,Prices) ->
    {User,  order(Item,N,State,Prices)}.

sync_request(view, User, State, _Prices) ->
    {User,State,invoice(State)};
sync_request({credit, Number,Date}, User, State, _Prices) ->
    {NewUser, Response}=set_credit_card(User,Number,Date),
    {NewUser, State, Response};
sync_request({address,Address}, User, State, _Prices) ->
    {NewUser,Response} = set_address(User,Address),
    {NewUser,State,Response};
sync_request(buy, User, State,_Prices) ->
    {User, NewState, Response} = buy(User,State),
    {User, NewState,Response}.
    






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

buy(#user{address=undefined} =U, State) ->
    {U, State, {error, billing_info}}; 
buy(U, State) ->
    case cc:transaction(U#user.address, 
			U#user.card_number, 
			U#user.card_date, 
			order_total(State)) of
	{ok, _TrxId} -> {stop, {ok,invoice(State)}};
	{error, _Reason} -> {U, State, {error, credit_info}}
    end.



%% Given a list of order tuples of the form {Item, Count, SubTotal},
%% returns a tuple of {[Item,Count], Total} 
invoice(State) ->    
{[ {Item, N} || {Item, N, _SubTotal} <- State], order_total(State)}.

order_total(State) ->
    {_, _, SubTotals} = lists:unzip3(State),
    lists:sum(SubTotals).
    
    


%% Init state to have zeros for all price list

%% Current order format.  This could be kept as a list of order items,
%% to be tallied when the client issues a `buy' command.  The
%% behaviour of item removal, where removing three macarons from a
%% basket containing 1 returns 0 macarons, suggests that a better
%% model is to maintain a running total for each order category.

    %% We look up prices at order rather than buy time so as to fail
    %% early if a non-existent item is ordered.

%% order - > returns new order
order(Item, N, Order, Prices) ->
    io:format("Ordering ~p with current order of ~p~n", [Item, Order]),
    {Item, Price} = lists:keyfind(Item, 1, Prices),
    io:format("looking up current order~p~n", [Order]),
    {Item, Quantity, SubTotal} = lists:keyfind(Item, 1, Order),
    
    NewLineItem = modify_item(Item, N+Quantity, SubTotal+(N*Price)),
    lists:keyreplace(Item, 1, Order, NewLineItem).



modify_item(Item, N, T) when N > 0 ->
    {Item, N, T};
modify_item(Item, _N, _T) ->
    {Item, 0, 0}.









test() ->
    {ok,TestPid} = start_link(my_user),
    ok = macarons(TestPid, 3),
    ok = macarons(TestPid, -2),
    ok = macarons(TestPid, -3),
    ok = donuts(TestPid,3),
    view_cart(TestPid),
    {error, billing_info} = buy(TestPid).

			    
		 

	    
    


