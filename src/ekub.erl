-module(ekub).

-export([
    init/0, init/1,

    create/2, create/3, create/4,
    delete/2, delete/3, delete/4, delete/5,

    replace/2, replace/3, replace/4,
    patch/3, patch/4, patch/5, patch/6,

    read/2, read/3, read/4, read/5,
    watch/2, watch/3, watch/4, watch/5,

    watch/1,
    watch_close/1,

    logs/2, logs/3, logs/4, logs/5,
    exec/3, exec/4, exec/5, exec/6,

    metadata/2
]).

-define(Api, ekub_api).
-define(Core, ekub_core).
-define(Access, ekub_access).

-define(ExecTimeout, 60 * 1000). % 1 minute

init() ->
    case ?Access:read() of
        {ok, Access} -> init(Access);
        {error, Reason} -> {error, Reason}
    end.

init(Access) ->
    case ?Api:load(Access) of
        {ok, Api} -> {ok, {Api, Access}};
        {error, Reason} -> {error, Reason}
    end.

create(Resource, {Api, Access}) ->
    create(Resource, "", [], {Api, Access}).

create(Resource, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    create(Resource, "", Query, {Api, Access});

create(Resource, Namespace, {Api, Access}) ->
    create(Resource, Namespace, [], {Api, Access}).

create(Resource, Namespace, Query, {Api, Access}) ->
    {Group, ResourceAlias, FinalNamespace, _Name} =
        metadata(Resource, Namespace),
    Endpoint = ?Api:endpoint(
        Group, ResourceAlias, FinalNamespace, "", {Api, Access}),
    ?Core:http_request(post, Endpoint, Query, Resource, Access).

delete(Resource, {Api, Access}) ->
    delete(Resource, "", [], {Api, Access}).

delete(Resource, Query, {Api, Access})
    when is_map(Resource), is_tuple(hd(Query))
->
    delete(Resource, "", Query, {Api, Access});

delete(Resource, Namespace, {Api, Access}) when is_map(Resource) ->
    delete(Resource, Namespace, [], {Api, Access});

delete(ResourceAlias, Namespace, {Api, Access}) ->
    delete(ResourceAlias, Namespace, "", [], {Api, Access}).

delete(Resource, Namespace, Query, {Api, Access}) when is_map(Resource) ->
    {Group, ResourceAlias, FinalNamespace, Name} =
        metadata(Resource, Namespace),
    Endpoint = ?Api:endpoint(
        Group, ResourceAlias, FinalNamespace, Name, {Api, Access}),
    ?Core:http_request(delete, Endpoint, Query, Access);

delete(ResourceAlias, Namespace, Query, {Api, Access})
    when is_tuple(hd(Query))
->
    delete(ResourceAlias, Namespace, "", Query, {Api, Access});

delete(ResourceAlias, Namespace, Name, {Api, Access}) ->
    delete(ResourceAlias, Namespace, Name, [], {Api, Access}).

delete(ResourceAlias, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = ?Api:endpoint(ResourceAlias, Namespace, Name, {Api, Access}),
    ?Core:http_request(delete, Endpoint, Query, Access).

replace(Resource, {Api, Access}) ->
    replace(Resource, "", [], {Api, Access}).

replace(Resource, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    replace(Resource, "", Query, {Api, Access});

replace(Resource, Namespace, {Api, Access}) ->
    replace(Resource, Namespace, [], {Api, Access}).

replace(Resource, Namespace, Query, {Api, Access}) ->
    {Group, ResourceAlias, FinalNamespace, Name} =
        metadata(Resource, Namespace),
    Endpoint = ?Api:endpoint(
        Group, ResourceAlias, FinalNamespace, Name, {Api, Access}),
    ?Core:http_request(put, Endpoint, Query, Resource, Access).

patch(ResourceAlias, Patch, {Api, Access}) ->
    patch(ResourceAlias, "", "", [], Patch, {Api, Access}).

patch(ResourceAlias, Query, Patch, {Api, Access}) when is_tuple(hd(Query)) ->
    patch(ResourceAlias, "", "", Query, Patch, {Api, Access});

patch(ResourceAlias, Namespace, Patch, {Api, Access}) ->
    patch(ResourceAlias, Namespace, "", [], Patch, {Api, Access}).

patch(ResourceAlias, Namespace, Query, Patch, {Api, Access})
    when is_tuple(hd(Query))
->
    patch(ResourceAlias, Namespace, "", Query, Patch, {Api, Access});

patch(ResourceAlias, Namespace, Name, Patch, {Api, Access}) ->
    patch(ResourceAlias, Namespace, Name, [], Patch, {Api, Access}).

patch(ResourceAlias, Namespace, Name, Query, Patch, {Api, Access}) ->
    Endpoint = ?Api:endpoint(ResourceAlias, Namespace, Name, {Api, Access}),
    ?Core:http_request(patch, Endpoint, Query, Patch, Access).

read(ResourceAlias, {Api, Access}) ->
    read(ResourceAlias, "", "", [], {Api, Access}).

read(ResourceAlias, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    read(ResourceAlias, "", "", Query, {Api, Access});

read(ResourceAlias, Namespace, {Api, Access}) ->
    read(ResourceAlias, Namespace, "", [], {Api, Access}).

read(ResourceAlias, Namespace, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    read(ResourceAlias, Namespace, "", Query, {Api, Access});

read(ResourceAlias, Namespace, Name, {Api, Access}) ->
    read(ResourceAlias, Namespace, Name, [], {Api, Access}).

read(ResourceAlias, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = ?Api:endpoint(ResourceAlias, Namespace, Name, {Api, Access}),
    ?Core:http_request(Endpoint, Query, Access).

watch(ResourceAlias, {Api, Access}) ->
    watch(ResourceAlias, "", "", [], {Api, Access}).

watch(ResourceAlias, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    watch(ResourceAlias, "", "", Query, {Api, Access});

watch(ResourceAlias, Namespace, {Api, Access}) ->
    watch(ResourceAlias, Namespace, "", [], {Api, Access}).

watch(ResourceAlias, Namespace, Query, {Api, Access})
    when is_tuple(hd(Query))
->
    watch(ResourceAlias, Namespace, "", Query, {Api, Access});

watch(ResourceAlias, Namespace, Name, {Api, Access}) ->
    watch(ResourceAlias, Namespace, Name, [], {Api, Access}).

watch(ResourceAlias, Namespace, Name, Query, {Api, Access}) ->
    Endpoint = ?Api:endpoint(ResourceAlias, Namespace, Name, {Api, Access}),
    ?Core:http_stream_request(Endpoint, [{watch, true}|Query], Access).

watch(Ref) -> ?Core:http_stream_read(Ref).
watch_close(Ref) -> ?Core:http_close(Ref).

logs(PodName, {Api, Access}) ->
    logs("", PodName, "", [], {Api, Access}).

logs(Namespace, PodName, {Api, Access}) ->
    logs(Namespace, PodName, "", [], {Api, Access}).

logs(Namespace, PodName, Query, {Api, Access}) when is_tuple(hd(Query)) ->
    logs(Namespace, PodName, "", Query, {Api, Access});

logs(Namespace, PodName, ContainerName, {Api, Access}) ->
    logs(Namespace, PodName, ContainerName, [], {Api, Access}).

logs(Namespace, PodName, ContainerName, Query, {Api, Access}) ->
    Endpoint = ?Api:endpoint({pods, log}, Namespace, PodName, {Api, Access}),
    FinalQuery = ensure_option(container, ContainerName, Query),
    ?Core:http_request(Endpoint, FinalQuery, Access).

exec(PodName, Command, {Api, Access}) ->
    exec("", PodName, "", Command, [], {Api, Access}).

exec(Namespace, PodName, Command, {Api, Access}) ->
    exec(Namespace, PodName, "", Command, [], {Api, Access}).

exec(Namespace, PodName, Command, Options, {Api, Access})
    when is_tuple(hd(Options))
->
    exec(Namespace, PodName, "", Command, Options, {Api, Access});

exec(Namespace, PodName, ContainerName, Command, {Api, Access}) ->
    exec(Namespace, PodName, ContainerName, Command, [], {Api, Access}).

exec(Namespace, PodName, ContainerName, Command, Options, {Api, Access}) ->
    Endpoint = ?Api:endpoint({pods, exec}, Namespace, PodName, {Api, Access}),

    Query = [{stdout, true},
             {stderr, true},
             {container, ContainerName}|
             [{command, Arg} || Arg <- string:split(Command, " ", all)]],

    FinalOptions = ensure_option(recv_timeout, ?ExecTimeout, Options),

    ?Core:ws_request(Endpoint, Query, [], FinalOptions, Access).

metadata(Resource, Namespace) ->
    Metadata = maps:get(<<"metadata">>, Resource),
    Group = case string:split(maps:get(<<"apiVersion">>, Resource), "/") of
                [GroupName, GroupVersion] -> {GroupName, GroupVersion};
                [GroupVersion] -> {"", GroupVersion}
            end,
    Kind = maps:get(<<"kind">>, Resource),
    IsStatus = maps:is_key(<<"status">>, Resource),
    ResourceAlias = if IsStatus -> {Kind, <<"status">>};
                    not IsStatus -> Kind end,

    {Group, ResourceAlias,
     maps:get(<<"namespace">>, Metadata, Namespace),
     maps:get(<<"name">>, Metadata)}.

ensure_option(Name, Value, Options) ->
    HasOption = lists:keymember(Name, 1, Options),
    if HasOption -> Options;
    not HasOption -> [{Name, Value}|Options] end.
