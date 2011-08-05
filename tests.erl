-module(tests).
-export([reliable/0, unreliable/0]).
-export([start_stop/1, empty_view/1, minimal_order/1, invalid_order/1]).

reliable() ->
    generic(cart2),
    invalid_order(cart2).

unreliable() ->
    generic(cart).




generic(Module) ->
    start_stop(Module),
    empty_view(Module),
    minimal_order(Module),
    succeeded.




start_stop(M) ->
    io:format("~nTEST: start_stop~n"),
    %M:start(),
    {ok, _Ref} = M:start_link(benny),
    % TODO delay shouldn't be needed
    timer:sleep(500),
    M:stop(), timer:sleep(250).


minimal_order(M) ->
    io:format("~nTEST: minimal_order~n"),
    %M:start(),
    {ok, Ref} = M:start_link(fred),
    ok = M:donuts(Ref,2),
    Expected = {[{macarons,0},{cupcakes,0},{danish,0},{donuts,2}],100},
    Actual = M:view_cart(Ref),
    compare_cart(Expected, Actual),
    timer:sleep(300),
    M:stop(), timer:sleep(250).
    

empty_view(M) ->
    io:format("~nTEST: empty_view~n"),
    %M:start(),
    {ok, Ref} = M:start_link(fred),
    Expected = {[{macarons,0},{cupcakes,0},{danish,0},{donuts,0}],0},
    Actual = M:view_cart(Ref),
    compare_cart(Expected, Actual),
    timer:sleep(300),
    M:stop(), timer:sleep(250).


invalid_order(M) ->
    io:format("~nTEST: invalid_order~n"),

    {ok, Ref} = M:start_link(fred),
    {ok, Ref} = M:start_backup(Ref),
    M:donuts(Ref,2),
    M:invalid_order(Ref, 2),
    Expected = {[{macarons,0},{cupcakes,0},{danish,0},{donuts,2}],100},
    Actual = M:view_cart(Ref),
    io:format("View cart shows: ~p~n", [Actual]),
    compare_cart(Expected, Actual),
    timer:sleep(1000),
    M:stop(), timer:sleep(250).    
    
compare_cart({E1, E2}, {A1, A2}) ->
    A1Sorted = lists:sort(A1),
    E1Sorted = lists:sort(E1),
    A1Sorted = E1Sorted,
    E2 = A2.

