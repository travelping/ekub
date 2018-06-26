-module(ekub_api).

-export([
    pods/1, pods/2, pods/3,
    version/1
]).

-define(Core, ekub_core).

-define(Api, "/api/v1").

pods(Access) ->
    get(pods, Access).

pods(Namespace, Access) ->
    get(path([namespaces, Namespace, pods]), Access).

pods(Namespace, Pod, Access) ->
    get(path([namespaces, Namespace, pods, Pod]), Access).

version(Access) ->
    ?Core:http_request("/version", Access).

get(Path, Access) ->
    ?Core:http_request(?Api ++ [$/|Path], Access).

path(Items) -> lists:flatten(lists:join($/, [to_list(Item) || Item <- Items])).

to_list(L) when is_list(L) -> L;
to_list(A) when is_atom(A) -> atom_to_list(A).
