-module(ekub_api).

-export([
    pods/1, pods/2, pods/3,
    exec/5,
    version/1
]).

-define(Core, ekub_core).

-define(ExecQuery, [
    {"stdout", "true"},
    {"stderr", "true"}
]).

pods(Access) ->
    get("/api/v1/pods", Access).

pods(Namespace, Access) ->
    get("/api/v1/namespace/~s/pods", [Namespace], Access).

pods(Namespace, Pod, Access) ->
    get("/api/v1/namespace/~s/pods/~s", [Namespace, Pod], Access).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    Resource = fmt("/api/v1/namespaces/~s/pods/~s/exec", [Namespace, PodName]),

    Query = [{"container", ContainerName}|?ExecQuery] ++
            [{"command", Arg} || Arg <- string:split(Command, " ", all)],

    ?Core:ws_request(Resource, Query, Access).

version(Access) -> get("/version", Access).

get(Resource, Access) -> get(Resource, [], Access).
get(Resource, Args, Access) -> ?Core:http_request(fmt(Resource, Args), Access).

fmt(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).
