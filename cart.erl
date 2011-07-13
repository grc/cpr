%%% cart A collection of order items assocated with a given
%%% transaction id, terminated by a `buy' action.

-module(cart).

%% API
-export([start_link/1, donuts/2, macarons/2, danish/2, cupcakes/2, view_cart/1]).


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

view_cart(ReferenceId) ->
    send(ReferenceId, view).
    
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
    InitialState = [ {Item, 0, 0}||{Item, _Price} <- PriceList],
    loop(UserName, PriceList, InitialState).


loop(UserName, PriceList, State) ->
    io:format("cart (~p) - looping with state ~p~n", [UserName, State]),
    receive
	{stop, Pid} ->
	    io:format("cart (~p) - received stop~n", [UserName]),
	    Pid ! ok;
	{request, Pid, Message} ->
	    Pid! {reply,ok},
	    loop(UserName, PriceList, handle_request(Message, State, PriceList));
	Msg -> io:format("cart - Unexpected message: ~p~n", [Msg])
    end.





handle_request({order, Item, N}, State, PriceList) ->
    order(Item, N, State, PriceList);
handle_request(view, State, _PriceList) ->
    {_, _, SubTotals} = lists:unzip3(State),
    {[ {Item, N} || {Item, N, _SubTotal} <- State],
     lists:sum(SubTotals)}.
    



%% Init state to have zeros for all price list

%% Current order format.  This could be kept as a list of order items,
%% to be tallied when the client issues a `buy' command.  The
%% behaviour of item removal, where removing three macarons from a
%% basket containing 1 returns 0 macarons, suggests that a better
%% model is to maintain a running total for each order category.

    %% We look up prices at order rather than buy time so as to fail
    %% early if a non-existent item is ordered.

%% order - > returns new order
order(Item, N, Order, PriceList) ->
    io:format("Ordering ~p with current order of ~p~n", [Item, Order]),
    {Item, Price} = lists:keyfind(Item, 1, PriceList),
    io:format("looking up current order~p~n", [Order]),
    {Item, Quantity, SubTotal} = lists:keyfind(Item, 1, Order),
    
    NewLineItem = modify_item(Item, N+Quantity, SubTotal+(N*Price)),
    lists:keyreplace(Item, 1, Order, NewLineItem).



modify_item(Item, N, T) when N > 0 ->
    {Item, N, T};
modify_item(Item, _N, _T) ->
    {Item, 0, 0}.









			    
		 

	    
    


