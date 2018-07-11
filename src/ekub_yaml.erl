-module(ekub_yaml).

-export([
    read_file/1, read_file/2,
    read/1, read/2,

    decode/1,
    flatten/1,
    to_map/1, to_map/2
]).

-include_lib("yamerl/include/yamerl_errors.hrl").

read_file(FileName) -> read_file(FileName, []).
read_file(FileName, Options) ->
    case file:read_file(FileName) of
        {ok, Yaml} -> read(Yaml, Options);
        {error, Reason} -> {error, Reason}
    end.

read(Yaml) -> read(Yaml, []).

read(Yaml, Options) ->
    case decode(Yaml) of
        {ok, Proplists} -> {ok, to_map(Proplists, Options)};
        {error, Reason} -> {error, Reason}
    end.

decode(Yaml) ->
    try yamerl:decode(Yaml) of
        Proplists -> {ok, [Proplist || Proplist <- Proplists,
                           Proplist /= null]}
    catch
        throw:{yamerl_exception, [Error]} ->
            {error, Error#yamerl_parsing_error.text}
    end.

flatten(Proplists) ->
    [lists:reverse(flatten(Proplist, [])) || Proplist <- Proplists].

flatten(Proplist, Flattened) when not is_tuple(Proplist) ->
    lists:foldl(fun flatten/2, Flattened, Proplist);

flatten({Key, Value = [H|_]}, Flattened) ->
    case Flattened of
        [{Keys}|Rest] ->
            if is_number(H) -> [{lists:reverse([Key|Keys]), Value}|Rest];
            not is_number(H) -> flatten(Value, [{[Key|Keys]}|Rest]) end;
        Flattened ->
            if is_number(H) -> [{Key, Value}|Flattened];
            not is_number(H) -> flatten(Value, [{[Key]}|Flattened]) end
    end.

to_map(Proplists) -> to_map(Proplists, []).
to_map(Proplists = [[Prop|_]|_], Options) when is_tuple(Prop) ->
    [to_map(Proplist, Options) || Proplist <- Proplists];

to_map(Proplist = [Prop|_], Options) when is_tuple(Prop) ->
    lists:foldl(fun
        ({Key, Values = [[H|_]|_]}, Map) when is_tuple(H) ->
            Maps = [to_map(Value, Options) || Value <- Values],
            maps_put(Key, Maps, Map, Options);
        ({Key, Value = [H|_]}, Map) when is_tuple(H) ->
            maps_put(Key, to_map(Value, Options), Map, Options);
        ({Key, Value}, Map) -> maps_put(Key, Value, Map, Options)
    end, #{}, Proplist);

to_map(Strings = [[H|_]|_], Options) when is_number(H) ->
    [maps_put(String, #{}, Options) || String <- Strings].

maps_put(Key, Map, Options) ->
    maps_put(Key, null, Map, Options).

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
