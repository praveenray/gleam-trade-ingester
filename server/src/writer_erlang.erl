-module(writer_erlang).
-export([create_md5/1,
    all_ets_tables/0,
    insert_into_table/2, lookup_table/2, delete_table/1,
    get_csv_files_in_dir/1
    ]).

get_csv_files_in_dir(DirName) ->
    {ok, R} = re:compile("\\.csv$"),
    case file:list_dir(DirName) of
        {ok, List}  -> {ok, [filename:join(DirName, Name) || Name <- List, name_ends_with(Name, R) ]};
        {error, Error} -> {error, list_to_binary(io_lib:format("Error listing directory [~s] ~p",[DirName, Error]))}
    end.

name_ends_with(Name, RE) ->
    case re:run(Name, RE) of
      {match, _} -> true;
      nomatch -> false
    end.

create_md5(Str) ->
    M = crypto:hash(md5, Str),
    B = base64:encode_to_string(M),
    unicode:characters_to_binary(B).

all_ets_tables() ->
    ets:all().

insert_into_table(Table_Ref, Term) when is_reference(Table_Ref), is_tuple(Term) ->
    ets:insert(Table_Ref, Term).

lookup_table(Table_Name, Key) ->
    ets:lookup(binary_to_existing_atom(Table_Name), Key).

delete_table(Table_Name) ->
    ets:delete(binary_to_existing_atom(Table_Name)).

