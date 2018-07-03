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

read(Namespace, PodName, Access) -> read(Namespace, PodName, [], Access).
read(Namespace, PodName, Options, Access) ->
    Resource = {"/api/v1/namespaces/~s/pods/~s", [Namespace, PodName]},
    get(Resource, Options, Access).

list(Namespace, Access) -> list(Namespace, [], Access).
list(Namespace, Options, Access) ->
    get({"/api/v1/namespaces/~s/pods", [Namespace]}, Options, Access).

list_all_namespaces(Access) -> list_all_namespaces([], Access).
list_all_namespaces(Options, Access) ->
    get("/api/v1/pods", Options, Access).

watch(Namespace, PodName, Access) -> watch(Namespace, PodName, [], Access).
watch(Namespace, PodName, Options, Access) ->
    Resource = {"/api/v1/watch/namespaces/~s/pods/~s", [Namespace, PodName]},
    ?Core:http_stream_request(Resource, Options, Access).

watch_list(Namespace, Access) -> watch_list(Namespace, [], Access).
watch_list(Namespace, Options, Access) ->
    Resource = {"/api/v1/watch/namespaces/~s/pods", [Namespace]},
    ?Core:http_stream_request(Resource, Options, Access).

watch_list_all_namespaces(Access) -> watch_list_all_namespaces([], Access).
watch_list_all_namespaces(Options, Access) ->
    ?Core:http_stream_request("/api/v1/watch/pods", Options, Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    Resource = {"/api/v1/namespaces/~s/pods/~s/exec", [Namespace, PodName]},

    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    ?Core:ws_request(Resource, Query, Access).

read_log(Namespace, PodName, Access) ->
    read_log(Namespace, PodName, [], Access).

read_log(Namespace, PodName, Options, Access) ->
    Resource = {"/api/v1/namespaces/~s/pods/~s/log", [Namespace, PodName]},
    get(Resource, Options, Access).

get(Resource, Query, Access) ->
    ?Core:http_request(Resource, Query, Access).
