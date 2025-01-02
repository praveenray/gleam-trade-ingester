-module(erlang_utils).
-import(string, [concat/2]).
-export([
         replace_re_with/3,
         validate_date/3,
         join_paths/1
]).

validate_date(Y,M,D) when is_integer(Y), is_integer(M), is_integer(D) ->
    calendar:valid_date(Y,M,D).

join_paths([]) ->
    "";
join_paths(Paths) when is_list(Paths) ->
    filename:join(Paths).

replace_re_with(String, REString, Replacement) when is_binary(String), is_binary(Replacement), is_binary(REString) ->
    case re:compile(REString) of
      {ok, RE} -> 
        Result = re:replace(String, RE, Replacement,[{return, list}]),
        {ok, list_to_binary(Result)};
      _ -> {error, list_to_binary(io_lib:format("Invalid Regular expression: ~s", [REString]))}
    end.
