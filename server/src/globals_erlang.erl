-module(globals_erlang).
-import(string, [concat/2]).
-export([
         create_ets_table/1,
         insert/3,
         lookup_atom_key/2,
         lookup_string_key/2,
         delete_ets_key/2,
         try_ets_table_present/1,
         tab2file/2, file2tab/1,
         ets_table_from_file/2
]).

ets_table_from_file(FileName, TableName) when is_binary(FileName), is_binary(TableName) ->
    try
        FileNameList = binary_to_list(FileName),
        TableNameAtom = binary_to_atom(TableName),
        case ets:file2tab(FileNameList) of
            {ok, Tid} -> {ok, ets:whereis(Tid)};
            {error, {read_error, _}} ->
                NewTable = ets:new(TableNameAtom, [set, public, named_table, {read_concurrency, true}]),
                case ets:tab2file(NewTable, FileNameList) of
                    ok -> {ok, ets:whereis(NewTable)};
                    _ -> {error, <<"Error Creating ETS Table">>}
                end;
            {error, cannot_create_table} ->
                %% File exists and has been read into memory most likely
                case ets:whereis(TableNameAtom) of
                    undefined -> {error, <<"ETS File found but whereis still fails">>};
                    Ref -> {ok, Ref}
                end
        end
    catch
      _:Error:StackTrace ->
            erlang:display(StackTrace),
            {error, list_to_binary(io_lib:format("Exception : ~p",[Error]))}
    end.
                         
try_ets_table_present(TableName) ->
    case ets:whereis(binary_to_atom(TableName)) of
        undefined -> {error, "not found"};
        Ref -> {ok, Ref}
    end.

create_ets_table(TableName) when is_binary(TableName)  ->
    try
      TableNameAtom = binary_to_atom(TableName),
      case ets:whereis(TableNameAtom) of
        undefined ->
          Name = ets:new(TableNameAtom, [set, public, named_table, {read_concurrency, true}]),
          {ok, ets:whereis(Name)};
        Ref -> {ok, Ref}
      end
    catch
      _:Error -> {error, list_to_binary(io_lib:format("~p",[Error]))}
    end.

insert(Tid, Key, Value) when is_binary(Key), is_reference(Tid)  ->
    KeyAtom = binary_to_atom(Key),
    ets:insert(Tid, {KeyAtom, Value}).

lookup_atom_key(Tid, Key) when is_binary(Key), is_reference(Tid) ->
    ets:lookup(Tid, binary_to_atom(Key)).

lookup_string_key(Tid, Key) when is_binary(Key), is_reference(Tid) ->
    ets:lookup(Tid, Key).

delete_ets_key(Tid,Key) when is_binary(Key), is_reference(Tid) ->
    ets:delete(Tid, Key).

tab2file(Table_Ref, File_Name) when is_reference(Table_Ref) ->
    case ets:tab2file(Table_Ref, binary_to_list(File_Name)) of
        ok -> {ok, File_Name};
        {error, Error} -> {error, term_to_binary(Error)}
    end.
file2tab(FileName) when is_binary(FileName) ->
    case ets:file2tab(binary_to_list(FileName)) of
        {ok, Tid} -> {ok, ets:whereis(Tid)};
        {error, Reason} -> {error, term_to_binary(Reason)}
    end.
