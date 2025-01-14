-module(query_erlang).
-export([
         match_trades_for_ticker/2,
         all_transactions/1,
         all_transactions/3
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

all_transactions(Ref, Sort, Page) when is_reference(Ref) ->
    {sort, SortDirection, SortColumn} = Sort,
    io:format("~p~n",[Sort]),
    ResultsList = flatten_match_list(ets:match(Ref, {'$1','$2'})),
    Sorted = lists:sort(fun({trade, Dt1, Ticker1, Qty1, Price1, Action1}, {trade, Dt2, Ticker2, Qty2, Price2, Action2}) ->
       case {SortColumn,SortDirection} of
           {<<"action">>,asc} ->  Action1 < Action2;
           {<<"action">>,desc} ->  Action1 > Action2;
           {<<"symbol">>,asc} ->   Ticker1 < Ticker2;
           {<<"symbol">>,desc} ->  Ticker1 > Ticker2;
           {<<"quantity">>,asc} -> Qty1 < Qty2;
           {<<"quantity">>,desc} -> Qty1 > Qty2;
           {<<"price">>,asc} -> Price1 < Price2;
           {<<"price">>,desc} -> Price1 > Price2;
           {<<"run date">>, _} ->
               {date, Y1, M1, D1} = Dt1,
               {date, Y2, M2, D2} = Dt2,
               Date1 = calendar:date_to_gregorian_days(Y1,M1,D1),
               Date2 = calendar:date_to_gregorian_days(Y2,M2,D2),
               case SortDirection of
                   asc -> Date1 < Date2;
                   desc -> Date1 > Date2
               end;
            {<<"_none">>,_} -> true
       end
      end, ResultsList),
    SubList = case Page of
        {pagination, PageNumber, _} when PageNumber == -1 -> Sorted;
        {pagination, PageNumber, PageSize}  ->
            Index = PageNumber * PageSize,
            lists:sublist(Sorted, Index, PageSize)
    end,
    {SubList, length(Sorted)}.

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
