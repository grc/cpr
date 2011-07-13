-module(cc).
-export([is_valid/3, transaction/4]).

% stubbed out credit card module for test purposes. Sentinel values of
% CardNumber (invalid_card, insufficient_funds) will cause appropriate
% error returns, otherwise calls will succeed.


is_valid(_BillingAddress, invalid_card, _ExpirationDate) ->
    false;
is_valid(_BillingAddress, _CardNumber, _ExpirationDate) ->
    true.




transaction(_BillingAddress, invalid_card, _ExpirationDate, _Price) ->
    {error,invalid_card};
transaction(_BillingAddress, insufficient_funds, _ExpirationDate, _Price) ->
    {insufficient_funds};
transaction(_BillingAddress, _CardNumber, _ExpirationDate, _Price) ->
    {ok, make_ref()}.


    
