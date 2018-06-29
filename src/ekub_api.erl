-module(ekub_api).

-export([
    pods/1, pods/2, pods/3,
    exec/5,
    watch/3, watch/1, watch_option/1,
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

watch(Workload, Options, Access) ->
    Resource = fmt("/api/v1/watch/~s", [Workload]),
    Query = lists:map(fun watch_option/1, Options),
    ?Core:http_stream_request(Resource, Query, Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

watch_option({Name, Value}) ->
    {underscore_atom_to_camel_case_string(Name), to_string(Value)}.

version(Access) -> get("/version", Access).

get(Resource, Access) -> get(Resource, [], Access).
get(Resource, Args, Access) -> ?Core:http_request(fmt(Resource, Args), Access).

fmt(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

underscore_atom_to_camel_case_string(Atom) ->
    case string:split(atom_to_list(Atom), "_", all) of
        [First] -> First;
        [First|Rest] -> lists:flatten([First|[[H-32|T] || [H|T] <- Rest]])
    end.

to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Number) when is_number(Number) -> integer_to_list(Number);
to_string(String) when is_list(String) -> String.
