-module(ekub_yaml).

-export([
    read/1, read/2,

    read_url/1, read_url/2,
    read_file/1, read_file/2,
    read_doc/1, read_doc/2,

    build/2,
    decode/1,

    to_map/1, to_map/2
]).

-include_lib("yamerl/include/yamerl_errors.hrl").

read(Target) -> read(Target, []).
read(Target, Options) ->
    case is_url(Target) of
        true -> read_url(Target, Options);
        false -> case is_file(Target) of
            true -> read_file(Target, Options);
            false -> read_doc(Target, Options)
        end
    end.

is_url(Target) -> {Result, _Details} = http_uri:parse(Target), Result == ok.
is_file(Target) -> filelib:is_file(Target).

read_url(Url) -> read_url(Url, []).
read_url(Url, Options) ->
    case httpc:request(Url) of
        {ok, {_StatusLine, _Headers, Body}} -> read_doc(Body, Options);
        {error, Reason} -> {error, Reason}
    end.

read_file(FileName) -> read_file(FileName, []).
read_file(FileName, Options) ->
    case file:read_file(FileName) of
        {ok, Yaml} -> read_doc(Yaml, Options);
        {error, Reason} -> {error, Reason}
    end.

read_doc(Yaml) -> read_doc(Yaml, []).

read_doc({YamlTemplate, Values}, Options) ->
    read_doc(build(YamlTemplate, Values), Options);

read_doc(Yaml, Options) ->
    case decode(Yaml) of
        {ok, Proplists} -> {ok, to_map(Proplists, Options)};
        {error, Reason} -> {error, Reason}
    end.

build(YamlTemplate, Values) ->
    lists:flatten(lists:foldl(fun({Key, Value}, Yaml) ->
        Name = "{{ " ++ Key ++ " }}",
        string:replace(Yaml, Name, Value, all)
    end, YamlTemplate, Values)).

decode(Yaml) ->
    try yamerl:decode(Yaml) of
        Proplists -> {ok, [Proplist || Proplist <- Proplists,
                           Proplist /= null]}
    catch
        throw:{yamerl_exception, [Error]} ->
            {error, Error#yamerl_parsing_error.text}
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
    [maps_put(String, #{}, Options) || String <- Strings];

to_map([], _Options) -> [].

maps_put(Key, Map, Options) ->
    maps_put(Key, null, Map, Options).

maps_put(Key, Value, Map, Options) -> 
    IsListsOption = lists:member(lists, Options),
    if IsListsOption -> maps:put(Key, Value, Map);
    not IsListsOption -> maps_put_binary(Key, Value, Map) end.

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
