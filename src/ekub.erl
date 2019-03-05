-module(ekub).

-export([
    create/2, create/3, create/4, create/5,
    delete/2, delete/3, delete/4, delete/5,

    patch/3, patch/4, patch/5, patch/6,
    replace/6,

    read/2, read/3, read/4, read/5, read/6,
    watch/2, watch/3, watch/4, watch/5,

    watch/1,
    watch_close/1,

    logs/3, logs/4, logs/5,
    exec/4, exec/5, exec/6,

    endpoint/5,
    metadata/3
]).

-define(Api, ekub_api).
-define(Core, ekub_core).

-define(ExecTimeout, 60 * 1000). % 1 minute

create(Resource, {Api, Access}) ->
    create(Resource, "", "", [], {Api, Access}).

create(Resource, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    create(Resource, "", "", Query, {Api, Access});

create(Resource, Namespace, {Api, Access}) ->
    create(Resource, Namespace, "", [], {Api, Access}).

create(Resource, Namespace, Query, {Api, Access}) ->
    create(Resource, Namespace, "", Query, {Api, Access}).

create(Resource, Namespace, SubResource, Query, {Api, Access}) ->
    {Kind, FinalNamespace, _Name} = metadata(Resource, Namespace, Access),
    Endpoint = endpoint(Kind, FinalNamespace, "", SubResource, {Api, Access}),
    ?Core:http_request(post, Endpoint, Query, Resource, Access).

delete(Resource, {Api, Access}) ->
    delete(Resource, "", [], {Api, Access}).

delete(Resource, Query, {Api, Access})
    when is_map(Resource), is_tuple(hd(Query))
->
    delete(Resource, "", Query, {Api, Access});

delete(Resource, Namespace, {Api, Access}) when is_map(Resource) ->
    delete(Resource, Namespace, [], {Api, Access});

delete(ResourceType, Namespace, {Api, Access}) ->
    delete(ResourceType, Namespace, "", [], {Api, Access}).

delete(Resource, Namespace, Query, {Api, Access}) when is_map(Resource) ->
    {Kind, FinalNamespace, Name} = metadata(Resource, Namespace, Access),
    delete(Kind, FinalNamespace, Name, Query, {Api, Access});

delete(ResourceType, Namespace, Query, {Api, Access})
    when is_tuple(hd(Query))
->
    delete(ResourceType, Namespace, "", Query, {Api, Access});

delete(ResourceType, Namespace, Name, {Api, Access}) ->
    delete(ResourceType, Namespace, Name, [], {Api, Access}).

delete(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", {Api, Access}),
    ?Core:http_request(delete, Endpoint, Query, Access).

patch(ResourceType, Patch, {Api, Access}) ->
    patch(ResourceType, "", "", [], Patch, {Api, Access}).

patch(ResourceType, Query, Patch, {Api, Access}) when is_tuple(hd(Query)) ->
    patch(ResourceType, "", "", Query, Patch, {Api, Access});

patch(ResourceType, Namespace, Patch, {Api, Access}) ->
    patch(ResourceType, Namespace, "", [], Patch, {Api, Access}).

patch(ResourceType, Namespace, Query, Patch, {Api, Access})
    when is_tuple(hd(Query))
->
    patch(ResourceType, Namespace, "", Query, Patch, {Api, Access});

patch(ResourceType, Namespace, Name, Patch, {Api, Access}) ->
    patch(ResourceType, Namespace, Name, [], Patch, {Api, Access}).

patch(ResourceType, Namespace, Name, Query, Patch, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", {Api, Access}),
    ?Core:http_request(patch, Endpoint, Query, Patch, Access).

replace(ResourceType, Namespace, Name, Query, ResourceTo, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", {Api, Access}),
    ?Core:http_request(put, Endpoint, Query, ResourceTo, Access).

read(ResourceType, {Api, Access}) ->
    read(ResourceType, "", "", "", [], {Api, Access}).

read(ResourceType, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    read(ResourceType, "", "", "", Query, {Api, Access});

read(ResourceType, Namespace, {Api, Access}) ->
    read(ResourceType, Namespace, "", "", [], {Api, Access}).

read(ResourceType, Namespace, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    read(ResourceType, Namespace, "", "", Query, {Api, Access});

read(ResourceType, Namespace, Name, {Api, Access}) ->
    read(ResourceType, Namespace, Name, "", [], {Api, Access}).

read(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    read(ResourceType, Namespace, Name, "", Query, {Api, Access}).

read(ResourceType, Namespace, Name, SubResource, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name,
                        SubResource, {Api, Access}),
    ?Core:http_request(Endpoint, Query, Access).

watch(ResourceType, {Api, Access}) ->
    watch(ResourceType, "", "", [], {Api, Access}).

watch(ResourceType, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    watch(ResourceType, "", "", Query, {Api, Access});

watch(ResourceType, Namespace, {Api, Access}) ->
    watch(ResourceType, Namespace, "", [], {Api, Access}).

watch(ResourceType, Namespace, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    watch(ResourceType, Namespace, "", Query, {Api, Access});

watch(ResourceType, Namespace, Name, {Api, Access}) ->
    watch(ResourceType, Namespace, Name, [], {Api, Access}).

watch(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", {Api, Access}),
    ?Core:http_stream_request(Endpoint, [{watch, true}|Query], Access).

watch(Ref) -> ?Core:http_stream_read(Ref).
watch_close(Ref) -> ?Core:http_close(Ref).

logs(Namespace, PodName, {Api, Access}) ->
    logs(Namespace, PodName, "", [], {Api, Access}).

logs(Namespace, PodName, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    logs(Namespace, PodName, "", Query, {Api, Access});

logs(Namespace, PodName, ContainerName, {Api, Access}) ->
    logs(Namespace, PodName, ContainerName, [], {Api, Access}).

logs(Namespace, PodName, ContainerName, Query, {Api, Access}) ->
    Endpoint = endpoint(pods, Namespace, PodName, log, {Api, Access}),
    FinalQuery = ensure_option(container, ContainerName, Query),
    ?Core:http_request(Endpoint, FinalQuery, Access).

exec(Namespace, PodName, Command, {Api, Access}) ->
    exec(Namespace, PodName, "", Command, [], {Api, Access}).

exec(Namespace, PodName, Command, Options, {Api, Access})
    when is_tuple(hd(Options))
->
    exec(Namespace, PodName, "", Command, Options, {Api, Access});

exec(Namespace, PodName, ContainerName, Command, {Api, Access}) ->
    exec(Namespace, PodName, ContainerName, Command, [], {Api, Access}).

exec(Namespace, PodName, ContainerName, Command, Options, {Api, Access}) ->
    Endpoint = endpoint(pods, Namespace, PodName, exec, {Api, Access}),

    Query = [{stdout, true},
             {stderr, true},
             {container, ContainerName}|
             [{command, Arg} || Arg <- string:split(Command, " ", all)]],

    FinalOptions = ensure_option(recv_timeout, ?ExecTimeout, Options),

    ?Core:ws_request(Endpoint, Query, [], FinalOptions, Access).

endpoint(ResourceRef, Namespace, Name, SubResource, {Api, Access}) ->
    {ok, Group} = ?Api:group(ResourceRef, SubResource, Api),
    {ok, ResourceType} = ?Api:resource_type(ResourceRef, SubResource, Api),

    ?Api:endpoint(Group, ResourceType,
                  namespace(Namespace, Access), Name, SubResource).

metadata(Resource, Namespace, Access) ->
    Metadata = maps:get(<<"metadata">>, Resource),

    {maps:get(<<"kind">>, Resource),
     maps:get(<<"namespace">>, Metadata, namespace(Namespace, Access)),
     maps:get(<<"name">>, Metadata, "")}.

namespace(Namespace, Access) ->
    NamespaceIsEmpty = string:is_empty(Namespace),
    if  NamespaceIsEmpty -> maps:get(namespace, Access, "");
    not NamespaceIsEmpty -> Namespace end.

ensure_option(Name, Value, Options) ->
    HasOption = lists:keymember(Name, 1, Options),
    if  HasOption -> Options;
    not HasOption -> [{Name, Value}|Options] end.
