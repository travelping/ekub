-module(ekub).

-export([
    create/2, create/3, create/4,

    delete/4, delete/5,
    delete_collection/3, delete_collection/4,

    patch/5,
    replace/3,

    read/4, read/6,
    watch/4, watch/5, watch/1,

    logs/4,
    exec/5, exec/6,

    endpoint/5
]).

-define(Api, ekub_api).
-define(Core, ekub_core).
-define(Yaml, ekub_yaml).

create(Resource, {Api, Access}) ->
    create(Resource, "", "", {Api, Access}).

create(Resource, Namespace, {Api, Access}) ->
    create(Resource, Namespace, "", {Api, Access}).

create(Resource, Namespace, SubResource, {Api, Access}) ->
    Endpoint = endpoint(Resource, Namespace, false, SubResource, Api),
    ?Core:http_request(post, Endpoint, [], Resource, Access).

delete(ResourceType, Namespace, Name, {Api, Access}) ->
    delete(ResourceType, Namespace, Name, [], {Api, Access}).

delete(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_request(delete, Endpoint, Query, Access).

delete_collection(ResourceType, Namespace, {Api, Access}) ->
    delete_collection(ResourceType, Namespace, [], {Api, Access}).

delete_collection(ResourceType, Namespace, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, "", "", Api),
    ?Core:http_request(delete, Endpoint, Query, Access).

patch(ResourceType, Namespace, Name, Patch, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_request(patch, Endpoint, [], Patch, Access).

replace(Resource, Namespace, {Api, Access}) ->
    Endpoint = endpoint(Resource, Namespace, "", "", Api),
    ?Core:http_request(put, Endpoint, [], Resource, Access).

read(ResourceType, Namespace, Name, {Api, Access}) ->
    read(ResourceType, Namespace, Name, [], "", {Api, Access}).

read(ResourceType, Namespace, Name, Query, SubResource, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, SubResource, Api),
    ?Core:http_request(Endpoint, Query, Access).

watch(ResourceType, Namespace, Name, {Api, Access}) ->
    watch(ResourceType, Namespace, Name, [], {Api, Access}).

watch(ResourceType, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = endpoint(ResourceType, Namespace, Name, "", Api),
    ?Core:http_stream_request(Endpoint, [{watch, true}|Query], Access).

watch(Ref) -> ?Core:http_stream_read(Ref).

logs(Namespace, PodName, Query, {Api, Access}) ->
    Endpoint = endpoint(pods, Namespace, PodName, log, Api),
    ?Core:http_request(Endpoint, Query, Access).

exec(Namespace, PodName, ContainerName, Command, {Api, Access}) ->
    exec(Namespace, PodName, ContainerName, Command, infinity, {Api, Access}).

exec(Namespace, PodName, ContainerName, Command, Timeout, {Api, Access}) ->
    {ok, {Group, Version}} = ?Api:group_version(pods, exec, Api),
    Endpoint = ?Api:endpoint(Group, Version, pods, Namespace, PodName, exec),

    Query = [{"stdout", "true"},
             {"stderr", "true"},
             {"container", ContainerName}|
             [{"command", Arg} || Arg <- string:split(Command, " ", all)]],

    Options = [{recv_timeout, Timeout}],

    ?Core:ws_request(Endpoint, Query, [], Options, Access).

endpoint(Resource, Namespace, Name, SubResource, Api) when is_map(Resource) ->
    Kind = maps:get(<<"kind">>, Resource),
    Metadata = maps:get(<<"metadata">>, Resource),

    FinalNamespace = if Namespace == false -> "";
                        Namespace /= false ->
                            maps:get(<<"namespace">>, Metadata, Namespace) end,

    FinalName = if Name == false -> "";
                   Name /= false -> maps:get(<<"name">>, Metadata, Name) end,

    {ok, {Group, Version}} = ?Api:group_version(Kind, SubResource, Api),
    {ok, ResourceType} = ?Api:resource_type(Kind, SubResource, Api),

    ?Api:endpoint(Group, Version, ResourceType,
                  FinalNamespace, FinalName, SubResource);

endpoint(ResourceAlias, Namespace, Name, SubResource, Api) ->
    {ok, ResourceType} = ?Api:resource_type(ResourceAlias, SubResource, Api),
    {ok, {Group, Version}} = ?Api:group_version(ResourceType, SubResource, Api),
    ?Api:endpoint(Group, Version, ResourceType, Namespace, Name, SubResource).
