-module(ekub).

-export([
    create/4,
    create_eviction/5,
    patch/5,
    replace/5,
    delete/4, delete/5,
    delete_collection/3, delete_collection/4,

    read/4, read/5,
    list/3, list/4,
    list_all_ns/2, list_all_ns/3,

    watch/4, watch/5, watch/1,
    watch_list/3, watch_list/4,
    watch_list_all_ns/2, watch_list_all_ns/3,

    patch_status/5,
    read_status/4,
    replace_status/5,

    % see deployments
    %Read Scale
    %Replace Scale
    %Patch Scale

    read_log/3, read_log/4,

    exec/5, exec/6
]).

-define(Api, ekub_api).
-define(Core, ekub_core).

create(Object, Namespace, Body, Access) ->
    Resource = {?Api:endpoint(create, Object), [Namespace]},
    ?Core:http_request(post, Resource, [], Body, Access).

create_eviction(Object, Namespace, Name, Body, Access) ->
    Resource = {?Api:endpoint(create_eviction, Object), [Namespace, Name]},
    ?Core:http_request(post, Resource, [], Body, Access).

patch(Object, Namespace, Name, Body, Access) ->
    Resource = {?Api:endpoint(patch, Object), [Namespace, Name]},
    ?Core:http_request(patch, Resource, [], Body, Access).

replace(Object, Namespace, Name, Body, Access) ->
    Resource = {?Api:endpoint(replace, Object), [Namespace, Name]},
    ?Core:http_request(put, Resource, [], Body, Access).

delete(Object, Namespace, Name, Access) ->
    delete(Object, Namespace, Name, [], Access).

delete(Object, Namespace, Name, Query, Access) ->
    Resource = {?Api:endpoint(delete, Object), [Namespace, Name]},
    ?Core:http_request(delete, Resource, Query, Access).

delete_collection(Object, Namespace, Access) ->
    delete_collection(Object, Namespace, [], Access).

delete_collection(Object, Namespace, Query, Access) ->
    Resource = {?Api:endpoint(delete_collection, Object), [Namespace]},
    ?Core:http_request(delete, Resource, Query, Access).

read(Object, Namespace, Name, Access) ->
    read(Object, Namespace, Name, [], Access).

read(Object, Namespace, Name, Query, Access) ->
    Resource = {?Api:endpoint(read, Object), [Namespace, Name]},
    ?Core:http_request(Resource, Query, Access).

list(Object, Namespace, Access) -> list(Object, Namespace, [], Access).
list(Object, Namespace, Query, Access) ->
    Resource = {?Api:endpoint(list, Object), [Namespace]},
    ?Core:http_request(Resource, Query, Access).

list_all_ns(Object, Access) ->
    list_all_ns(Object, [], Access).

list_all_ns(Object, Query, Access) ->
    Resource = ?Api:endpoint({list_all_ns, Object}),
    ?Core:http_request(Resource, Query, Access).

watch(Object, Namespace, Name, Access) ->
    watch(Object, Namespace, Name, [], Access).

watch(Object, Namespace, Name, Query, Access) ->
    Resource = {?Api:endpoint(watch, Object), [Namespace, Name]},
    ?Core:http_stream_request(Resource, Query, Access).

watch_list(Object, Namespace, Access) ->
    watch_list(Object, Namespace, [], Access).

watch_list(Object, Namespace, Query, Access) ->
    Resource = {?Api:endpoint(watch_list, Object), [Namespace]},
    ?Core:http_stream_request(Resource, Query, Access).

watch_list_all_ns(Object, Access) ->
    watch_list_all_ns(Object, [], Access).

watch_list_all_ns(Object, Query, Access) ->
    Resource = {?Api:endpoint(watch_list_all_ns, Object)},
    ?Core:http_stream_request(Resource, Query, Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

patch_status(Object, Namespace, Name, Body, Access) ->
    Resource = {?Api:endpoint(patch_status, Object), [Namespace, Name]},
    ?Core:http_request(patch, Resource, [], Body, Access).

read_status(Object, Namespace, Name, Access) ->
    Resource = {?Api:endpoint(read_status, Object), [Namespace, Name]},
    ?Core:http_request(Resource, [], Access).

replace_status(Object, Namespace, Name, Body, Access) ->
    Resource = {?Api:endpoint(replace_status, Object), [Namespace, Name]},
    ?Core:http_request(put, Resource, [], Body, Access).

read_log(Namespace, PodName, Access) ->
    read_log(Namespace, PodName, [], Access).

read_log(Namespace, PodName, Query, Access) ->
    Resource = {?Api:endpoint(read_log, pod), [Namespace, PodName]},
    ?Core:http_request(Resource, Query, Access).

exec(Namespace, PodName, ContainerName, Command, Access) ->
    exec(Namespace, PodName, ContainerName, Command, infinity, Access).

exec(Namespace, PodName, ContainerName, Command, Timeout, Access) ->
    Resource = {?Api:endpoint(exec, pod), [Namespace, PodName]},

    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    Options = [{recv_timeout, Timeout}],

    ?Core:ws_request(Resource, Query, [], Options, Access).
