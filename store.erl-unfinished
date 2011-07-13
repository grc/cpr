-module(store).

% Public interface
-export([start/0, stop/0, start_link/1, donuts/2]).

% Internal implementation details
-export([init/0, test/0]).


% Interface definitions


start() -> 
    PriceList = [{donuts,50}, {macarons,175},{danish,100},{cupcakes,75}], 
    register(?MODULE, spawn(?MODULE, init, [PriceList])).

    % No supervisor in use so register in line with spawn to remove 
    % registration race condition on start up.

stop() ->
    ?MODULE ! {stop, self()}.

start_link(UserName) ->
    % TODO - needs to handle unstarted server
    send({start_link, UserName}).

donuts(ReferenceId, N) ->
    order(donuts, ReferenceId, N). 


order(Item, ReferenceId, N) ->
    send({order, ReferenceId, {Item, N}}).

send(Message) ->
    ?MODULE ! {request, self(), Message},
    receive
	{reply, Reply} ->
	    Reply
    end.


% Implementation

init(PriceList) ->
    io:format("store - initialising~n"),
    loop([PriceList]).


loop(PriceList, State) ->
    io:format("store - looping with state: ~p~n", [State]),
    receive
	{stop, Pid} ->
	    io:format("store - received stop from ~p~n", [Pid]),
	    %TODO send a stop to all our carts
	    Pid ! ok;
	{request, Pid, {start_link, UserName} } -> % duplicate username?
	    Ref = make_ref(),
	    Pid ! {reply, {ok, Ref}},
	    UserCart = spawn(cart, init, [UserName, PriceList]), 
	    loop(PriceList, [{UserName, Ref, UserCart} |State]);
	{request, Pid, {order, Ref, Order}} ->
	    Pid ! ok,
	    io:format("store - received an order:~p,~p~n", [Ref, Order]),
	    {_UserName, Ref, UserCart} = lists:keyfind(Ref, 2, State),
	    UserCart ! {order, Order}
    end.



% Currently storing {User, Ref, Pid} in list.  Expect to have to
% deal with large numbers of users so use ETS?


%% Test harness

test() ->
    start(),
    {ok, Ref} = start_link(fred),
    donuts(Ref,2).
    
