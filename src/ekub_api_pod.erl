-module(ekub_api_pod).

-export([
    create/3,
    patch/4,
    delete/3, delete/4,

    read/3, read/4,
    list/2, list/3,
    list_all_namespaces/1, list_all_namespaces/2,

    watch/3, watch/4, watch/1,
    watch_list/2, watch_list/3,
    watch_list_all_namespaces/1, watch_list_all_namespaces/2,

    exec/5, exec/6,

    read_log/3, read_log/4
]).

-define(Core, ekub_core).

-define(ApiPod, "/api/v1/namespaces/~s/pods/~s").
-define(ApiPods, "/api/v1/namespaces/~s/pods").
-define(ApiPodsAllNs, "/api/v1/pods").

-define(ApiWatchPod, "/api/v1/watch/namespaces/~s/pods/~s").
-define(ApiWatchPods, "/api/v1/watch/namespaces/~s/pods").
-define(ApiWatchPodsAllNs, "/api/v1/watch/pods").

-define(ApiPodExec, "/api/v1/namespaces/~s/pods/~s/exec").
-define(ApiPodLog, "/api/v1/namespaces/~s/pods/~s/log").

create(Namespace, Pod, Access) ->
    Resource = {?ApiPods, [Namespace]},
    ?Core:http_request(post, Resource, [], Pod, Access).

patch(Namespace, PodName, Patch, Access) ->
    Resource = {?ApiPod, [Namespace, PodName]},
    ?Core:http_request(patch, Resource, [], Patch, Access).

delete(Namespace, PodName, Access) -> delete(Namespace, PodName, [], Access).
delete(Namespace, PodName, Query, Access) ->
    Resource = {?ApiPod, [Namespace, PodName]},
    ?Core:http_request(delete, Resource, Query, Access).

read(Namespace, PodName, Access) -> read(Namespace, PodName, [], Access).
read(Namespace, PodName, Query, Access) ->
    Resource = {?ApiPod, [Namespace, PodName]},
    ?Core:http_request(Resource, Query, Access).

list(Namespace, Access) -> list(Namespace, [], Access).
list(Namespace, Query, Access) ->
    Resource = {?ApiPods, [Namespace]},
    ?Core:http_request(Resource, Query, Access).

list_all_namespaces(Access) -> list_all_namespaces([], Access).
list_all_namespaces(Query, Access) ->
    Resource = ?ApiPodsAllNs,
    ?Core:http_request(Resource, Query, Access).

watch(Namespace, PodName, Access) -> watch(Namespace, PodName, [], Access).
watch(Namespace, PodName, Query, Access) ->
    Resource = {?ApiWatchPod, [Namespace, PodName]},
    ?Core:http_stream_request(Resource, Query, Access).

watch_list(Namespace, Access) -> watch_list(Namespace, [], Access).
watch_list(Namespace, Query, Access) ->
    Resource = {?ApiWatchPods, [Namespace]},
    ?Core:http_stream_request(Resource, Query, Access).

watch_list_all_namespaces(Access) -> watch_list_all_namespaces([], Access).
watch_list_all_namespaces(Query, Access) ->
    Resource = ?ApiWatchPodsAllNs,
    ?Core:http_stream_request(Resource, Query, Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    exec(Namespace, PodName, ContainerName, Command, infinity, Access).

exec(Namespace, PodName, ContainerName, Command, Timeout, Access) ->
    Resource = {?ApiPodExec, [Namespace, PodName]},

    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    Options = [{recv_timeout, Timeout}],

    ?Core:ws_request(Resource, Query, Options, Access).

read_log(Namespace, PodName, Access) ->
    read_log(Namespace, PodName, [], Access).

read_log(Namespace, PodName, Query, Access) ->
    Resource = {?ApiPodLog, [Namespace, PodName]},
    ?Core:http_request(Resource, Query, Access).
