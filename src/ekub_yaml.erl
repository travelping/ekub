-module(ekub_yaml).

-export([
    read_file/1, read_file/2,
    read/1, read/2
]).

-include_lib("yamerl/include/yamerl_errors.hrl").

read_file(FileName) -> read_file(FileName, []).
read_file(FileName, Options) ->
    case file:read_file(FileName) of
        {ok, Binary} -> read(Binary, Options);
        {error, Reason} -> {error, Reason}
    end.

read(Yaml) -> read(Yaml, []).
read(Yaml, Options) ->
    try yamerl:decode(Yaml) of
        Map -> {ok, maps_from_list(Map, Options)}
    catch
        throw:{yamerl_exception, [Error]} ->
            {error, Error#yamerl_parsing_error.text}
    end.

maps_from_list([List = [H|_]], Options) when not is_number(H) ->
    maps_from_list(List, Options);

maps_from_list(List, Options) ->
    lists:foldl(fun
        ({Key, Values = [[H|_]|_]}, Map) when not is_number(H) ->
            MapList = [maps_from_list(Value, Options) || Value <- Values],
            maps_put(Key, MapList, Map, Options);
        ({Key, Value = [H|_]}, Map) when is_tuple(H) ->
            maps_put(Key, maps_from_list(Value, Options), Map, Options);
        ({Key, Value}, Map) -> maps_put(Key, Value, Map, Options)
    end, #{}, List).

maps_put(Key, Value, Map, Options) -> 
    IsBinaryOption = lists:member(binary, Options),
    if IsBinaryOption -> maps_put_binary(Key, Value, Map);
    not IsBinaryOption -> maps:put(Key, Value, Map) end.

maps_put_binary(Key, Value, Map) ->
    maps:put(list_to_binary(Key), string_to_binary(Value), Map).

string_to_binary(String) -> case String of
    [] -> <<>>;
    [[]] -> [<<>>];
    String = [H|_] when is_number(H) -> list_to_binary(String);
    Strings = [[H|_]|_] when is_number(H) ->
        lists:map(fun list_to_binary/1, Strings);
    NotString -> NotString
end.
