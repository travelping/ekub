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

    exec/5,

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
    ?Core:http_request(post, {?ApiPods, [Namespace]}, [], Pod, Access).

patch(Namespace, PodName, Patch, Access) ->
    Resource = {?ApiPod, [Namespace, PodName]},
    ?Core:http_request(patch, Resource, [], Patch, Access).

delete(Namespace, PodName, Access) -> delete(Namespace, PodName, [], Access).
delete(Namespace, PodName, Options, Access) ->
    Resource = {?ApiPod, [Namespace, PodName]},
    ?Core:http_request(delete, Resource, Options, Access).

read(Namespace, PodName, Access) -> read(Namespace, PodName, [], Access).
read(Namespace, PodName, Options, Access) ->
    ?Core:http_request({?ApiPod, [Namespace, PodName]}, Options, Access).

list(Namespace, Access) -> list(Namespace, [], Access).
list(Namespace, Options, Access) ->
    ?Core:http_request({?ApiPods, [Namespace]}, Options, Access).

list_all_namespaces(Access) -> list_all_namespaces([], Access).
list_all_namespaces(Options, Access) ->
    ?Core:http_request(?ApiPodsAllNs, Options, Access).

watch(Namespace, PodName, Access) -> watch(Namespace, PodName, [], Access).
watch(Namespace, PodName, Options, Access) ->
    Resource = {?ApiWatchPod, [Namespace, PodName]},
    ?Core:http_stream_request(Resource, Options, Access).

watch_list(Namespace, Access) -> watch_list(Namespace, [], Access).
watch_list(Namespace, Options, Access) ->
    ?Core:http_stream_request({?ApiWatchPods, [Namespace]}, Options, Access).

watch_list_all_namespaces(Access) -> watch_list_all_namespaces([], Access).
watch_list_all_namespaces(Options, Access) ->
    ?Core:http_stream_request(?ApiWatchPodsAllNs, Options, Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    ?Core:ws_request({?ApiPodExec, [Namespace, PodName]}, Query, Access).

read_log(Namespace, PodName, Access) ->
    read_log(Namespace, PodName, [], Access).

read_log(Namespace, PodName, Options, Access) ->
    ?Core:http_request({?ApiPodLog, [Namespace, PodName]}, Options, Access).
