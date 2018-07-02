-module(ekub_api_pod).

-export([
    read/3, read/4,
    list/2, list/3,
    list_all_namespaces/1, list_all_namespaces/2,

    watch/3, watch/4, watch/1,
    watch_list/2, watch_list/3,
    watch_list_all_namespaces/1, watch_list_all_namespaces/2,

    exec/5,

    read_log/3, read_log/4
]).

-define(Core, ekub_core).
-define(CoreTools, ekub_core_tools).

read(Namespace, PodName, Access) -> read(Namespace, PodName, [], Access).
read(Namespace, PodName, Options, Access) ->
    get("/api/v1/namespaces/~s/pods/~s", [Namespace, PodName], Options, Access).

list(Namespace, Access) -> list(Namespace, [], Access).
list(Namespace, Options, Access) ->
    get("/api/v1/namespaces/~s/pods", [Namespace], Options, Access).

list_all_namespaces(Access) -> list_all_namespaces([], Access).
list_all_namespaces(Options, Access) ->
    get("/api/v1/pods", [], Options, Access).

watch(Namespace, PodName, Access) -> watch(Namespace, PodName, [], Access).
watch(Namespace, PodName, Options, Access) ->
    ?Core:http_stream_request(
        ?CoreTools:resource("/api/v1/watch/namespaces/~s/pods/~s",
                            [Namespace, PodName]),
        ?CoreTools:query(Options),
        Access
    ).

watch_list(Namespace, Access) -> watch_list(Namespace, [], Access).
watch_list(Namespace, Options, Access) ->
    ?Core:http_stream_request(
        ?CoreTools:resource("/api/v1/watch/namespaces/~s/pods", [Namespace]),
        ?CoreTools:query(Options),
        Access
    ).

watch_list_all_namespaces(Access) -> watch_list_all_namespaces([], Access).
watch_list_all_namespaces(Options, Access) ->
    ?Core:http_stream_request(
        ?CoreTools:resource("/api/v1/watch/pods", []),
        ?CoreTools:query(Options),
        Access
    ).

watch(Ref) -> ?Core:http_stream_read(Ref).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    Resource = ?CoreTools:resource("/api/v1/namespaces/~s/pods/~s/exec",
                                   [Namespace, PodName]),

    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    ?Core:ws_request(Resource, Query, Access).

read_log(Namespace, PodName, Access) ->
    read_log(Namespace, PodName, [], Access).

read_log(Namespace, PodName, Options, Access) ->
    get("/api/v1/namespaces/~s/pods/~s/log",
        [Namespace, PodName], Options, Access).

get(Resource, Args, Options, Access) ->
    ?Core:http_request(?CoreTools:resource(Resource, Args),
                       ?CoreTools:query(Options), Access).
