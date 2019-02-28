-module(ekub).

-export([
    create/2, create/3, create/4, create/5,
    delete/2, delete/3, delete/4, delete/5,

    patch/4, patch/5,
    replace/4,

    read/4, read/6,
    watch/3, watch/4, watch/5, watch/1,

    logs/3, logs/4,
    exec/5, exec/6,

    endpoint/5
]).

-define(Api, ekub_api).
-define(Core, ekub_core).

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
    Endpoint = endpoint(Kind, FinalNamespace, "", SubResource, Api),
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
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_request(delete, Endpoint, Query, Access).

patch(ResourceType, Namespace, Name, Patch, {Api, Access}) ->
    patch(ResourceType, Namespace, Name, Query, Patch, {Api, Access}).

patch(ResourceType, Namespace, Name, Query, Patch, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_request(patch, Endpoint, Query, Patch, Access).

replace(Resource, Namespace, Name, {Api, Access}) ->
    Endpoint = endpoint(Resource, Namespace, Name, "", Api),
    ?Core:http_request(put, Endpoint, [], Resource, Access).

read(ResourceType, Namespace, Name, {Api, Access}) ->
    read(ResourceType, Namespace, Name, [], "", {Api, Access}).

read(ResourceType, Namespace, Name, Query, SubResource, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, SubResource, Api),
    ?Core:http_request(Endpoint, Query, Access).

watch(ResourceType, Namespace, {Api, Access}) ->
    watch(ResourceType, Namespace, "", [], {Api, Access}).

watch(ResourceType, Namespace, Name, {Api, Access}) ->
    watch(ResourceType, Namespace, Name, [], {Api, Access}).

watch(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_stream_request(Endpoint, [{watch, true}|Query], Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

logs(Namespace, PodName, {Api, Access}) ->
    Endpoint = endpoint(pods, Namespace, PodName, log, Api),
    ?Core:http_request(Endpoint, [], Access).

logs(Namespace, PodName, Query, {Api, Access}) ->
    Endpoint = endpoint(pods, Namespace, PodName, log, Api),
    ?Core:http_request(Endpoint, Query, Access).

exec(Namespace, PodName, ContainerName, Command, {Api, Access}) ->
    exec(Namespace, PodName, ContainerName, Command, infinity, {Api, Access}).

exec(Namespace, PodName, ContainerName, Command, Timeout, {Api, Access}) ->
    {ok, Group} = ?Api:group(pods, exec, Api),
    Endpoint = ?Api:endpoint(Group, pods, Namespace, PodName, exec),

    Query = [{stdout, true},
             {stderr, true},
             {container, ContainerName}|
             [{command, Arg} || Arg <- string:split(Command, " ", all)]],

    Options = [{recv_timeout, Timeout}],

    ?Core:ws_request(Endpoint, Query, [], Options, Access).

endpoint(ResourceRef, Namespace, Name, SubResource, Api) ->
    {ok, Group} = ?Api:group(ResourceRef, SubResource, Api),
    {ok, ResourceType} = ?Api:resource_type(ResourceRef, SubResource, Api),

    ?Api:endpoint(Group, ResourceType, Namespace, Name, SubResource).

metadata(Resource, Namespace, Access) ->
    Metadata = maps:get(<<"metadata">>, Resource),

    {maps:get(<<"kind">>, Resource),
     maps:get(<<"namespace">>, Metadata, namespace(Namespace, Access)),
     maps:get(<<"name">>, Metadata, "")}.

namespace(Namespace, Access) ->
    NamespaceIsEmpty = string:is_empty(Namespace),
    if  NamespaceIsEmpty -> maps:get(namespace, Access, "");
    not NamespaceIsEmpty -> Namespace end.
