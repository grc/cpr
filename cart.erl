%%% cart A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart).

%% API
-export([start_link/1, donuts/2, macarons/2, danish/2, cupcakes/2]).


%% Private interfaces
-export([init/2]).



%% Cart is initialised with a price list representing all valid order items

%% API Definitions

start_link(UserName) ->
    PriceList = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}],
    ReferenceId = spawn(?MODULE, init, [UserName, PriceList]),
    {ok, ReferenceId}.

donuts(ReferenceId, N) -> 
    send(ReferenceId, {order, donuts, N}).

macarons(ReferenceId, N) -> 
    send(ReferenceId, {order, macarons, N}).

danish(ReferenceId, N) -> 
    send(ReferenceId, {order, danish, N}).

cupcakes(ReferenceId, N) -> 
    send(ReferenceId, {order, cupcakes, N}).
    
%% API Impl
send(Pid, Message) ->
    Pid ! {request, self(), Message},
    receive
	{reply, Reply} ->
	    Reply
    end.



%% Implementation
init(UserName, PriceList) ->
    io:format("cart (~p) - initialising with price list ~p~n", [UserName, PriceList]),
    loop(UserName, PriceList, []).


loop(UserName, PriceList, State) ->
    io:format("cart (~p) - looping with state ~p~n", [UserName, State]),
    receive
	{stop, Pid} ->
	    io:format("cart (~p) - received stop~n", [UserName]),
	    Pid ! ok;
	{request, Pid, Message} ->
	    Pid! ok,
	    loop(UserName, PriceList, handle_request(Message, State, PriceList));
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.


%% Current order format.  This could be kept as a list of order items,
%% to be tallied when the client issues a `buy' command.  The
%% behaviour of item removal, where removing three macarons from a
%% basket containing 1 returns 0 macarons, suggests that a better
%% model is to maintain a running total for each order category.



handle_request({order, Item, N}, State, PriceList) ->
    order(Item, N, State, PriceList).

%% order - > returns new order
order(Item, N, Order, PriceList) when N >= 0 ->
    %% We look up prices at order rather than buy time so as to fail
    %% early if a non-existent item is ordered.
    {Item, Price} = lists:keyfind(Item, 1, PriceList),
    case (ExistingOrder = lists:keyfind(Item, 1, Order) ) of
	ExistingOrder = {Item, Quantity, SubTotal} ->
	    %% TODO should use a set here
	    %% Already ordered so increase quantity
	    NewLineItem = {Item, Quantity + N, SubTotal+ (N*Price)},
	    lists:keyreplace(Item, 1, Order, NewLineItem);
	 false ->
	    %% Not yet ordered so add it 
	    [{Item, N, N*Price}| Order]
    end;
order(Item, N, Order, PriceList) when N < 0 ->
    %% Reduce current order quantity down to a minimum of zero.
    {Item, Price} = lists:keyfind(Item, 1, PriceList),
    case (ExistingOrder = lists:keyfind(Item, 1, Order)) of 
	ExistingOrder=false ->
	    %% Not yet ordered so ignore
	    Order;
	ExistingOrder = {Item, Quantity, SubTotal} when Quantity > N ->
	    NewLineItem = {Item, Quantity -N, SubTotal - (N*Price)},
	    lists:keyreplace(Item, 1, Order, NewLineItem);
	ExistingOrder = {Item, Quantity, _SubTotal} when Quantity =< N ->
	    NewLineItem = {Item, 0, 0},
	    lists:keyreplace(Item, 1, Order, NewLineItem)
    end.





			    
		 

	    
    


