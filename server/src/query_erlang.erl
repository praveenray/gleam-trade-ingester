-module(query_erlang).
-export([
         match_trades_for_ticker/2,
         all_transactions/1,
         all_transactions/2
]).
-include_lib("stdlib/include/ms_transform.hrl").

match_trades_for_ticker(Ticker, Ref) when is_binary(Ticker), is_reference(Ref) ->
  try
    Upper = string:uppercase(Ticker),
    List = ets:select(Ref,
               ets:fun2ms(
                 fun({Key, {trade, RunDate,Symbol,Qty,Price,Action}}) when Symbol == Upper ->
                         {trade, RunDate,Symbol,Qty,Price,Action} end)
    ),
    {ok, List}
  catch
    _:Error:StackTrace ->
          erlang:display(StackTrace),
          {error, list_to_binary(io_lib:format("Exception : ~p",[Error]))}
  end.

all_transactions(Ref, Limit) when is_reference(Ref), is_integer(Limit) -> match_ets(ets:match(Ref, {'$1','$2'}, Limit)).
all_transactions(Continue) -> match_ets(ets:match(Continue)).

match_ets(Result) ->
  Eot = <<"end_of_table">>,
  case Result of
    {List, '$end_of_table'} -> {ok, {flatten_match_list(List), Eot}};
    {List, Continue} ->
        {ok, {flatten_match_list(List), Continue}};
    '$end_of_table' -> {error, Eot}
  end.
flatten_match_list(List) -> [Rec || [_Key,Rec] <- List].
