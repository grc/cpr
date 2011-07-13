-module(cc).
-export([is_valid/3, transaction/4]).

% stubbed out credit card module for test purposes. Sentinel values of
% CardNumber (invalid_card, insufficient_funds) will cause appropriate
% error returns, otherwise calls will succeed.


is_valid(BillingAddress, invalid_card, ExpirationDate) ->
    false;
is_valid(BillingAddress, CardNumber, ExpirationDate) ->
    true.




transaction(BillingAddress, invalid_card, ExpirationDate, Price) ->
    {error,invalid_card};
transaction(BillingAddress, insufficient_funds, ExpirationDate, Price) ->
    {insufficient_funds};
transaction(BillingAddress, CardNumber, ExpirationDate, Price) ->
    {ok, make_ref()}.


    
