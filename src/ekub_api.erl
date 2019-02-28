-module(ekub_api).

-export([
    endpoint/2, endpoint/3, endpoint/4, endpoint/5,

    group/2, group/3,
    resource_types/1, resource_type/2, resource_type/3,

    load/1
]).

-define(Core, ekub_core).

-define(ApiCoreEndpoint, <<"/api/v1">>).
-define(ApisEndpoint, <<"/apis">>).

endpoint(Group, ResourceType) ->
    endpoint(Group, ResourceType, <<"">>, <<"">>, <<"">>).

endpoint(Group, ResourceType, Namespace) ->
    endpoint(Group, ResourceType, Namespace, <<"">>, <<"">>).

endpoint(Group, ResourceType, Namespace, Name) ->
    endpoint(Group, ResourceType, Namespace, Name, <<"">>).

endpoint(Group, ResourceType, Namespace, Name, SubResource) ->
    endpoint_build(to_binary(Group), to_binary(ResourceType),
                   to_binary(Namespace), to_binary(Name),
                   to_binary(SubResource)).

endpoint_build(Group, ResourceType, Namespace, Name, SubResource) ->
    {GroupName, GroupVersion} = Group,
    iolist_to_binary([
        if GroupName == <<"">> -> <<"/api/", GroupVersion/binary>>;
           GroupName /= <<"">> -> <<"/apis/", GroupName/binary,
                                    $/, GroupVersion/binary>> end,
        if Namespace == <<"">> -> <<"">>;
           Namespace /= <<"">> -> <<"/namespaces/", Namespace/binary>> end,
        <<$/, ResourceType/binary>>,
        if Name == <<"">> -> <<"">>;
           Name /= <<"">> -> <<$/, Name/binary>> end,
        if SubResource == <<"">> -> <<"">>;
           SubResource /= <<"">> -> <<$/, SubResource/binary>> end
    ]).

group(ResourceRef, Api) ->
    group(ResourceRef, <<"">>, Api).

group(ResourceRef, SubResource, Api) ->
    case resource_type(ResourceRef, SubResource, Api) of
        {ok, ResourceType} ->
            Groups = maps:get(groups, Api, #{}),
            maps:find(ResourceType, Groups);
        error -> error
    end.

resource_types(Api) ->
    [binary_to_list(Type) || Type <- maps:keys(maps:get(groups, Api))].

resource_type(ResourceRef, Api) ->
    resource_type(ResourceRef, <<"">>, Api).

resource_type(ResourceRef, SubResource, Api) ->
    Alias = to_binary({ResourceRef, SubResource}),
    maps:find(Alias, maps:get(aliases, Api, #{})).

load(Access) ->
    case load_endpoints(Access) of
        {ok, Endpoints} -> load_api(Endpoints, Access);
        {error, Reason} -> {error, Reason}
    end.

load_endpoints(Access) ->
    case ?Core:http_request(?ApisEndpoint, Access) of
        {ok, Apis} -> {ok, [?ApiCoreEndpoint|[
            <<?ApisEndpoint/binary, $/, GroupVersion/binary>>
            || #{<<"preferredVersion">> :=
                   #{<<"groupVersion">> := GroupVersion}}
            <- maps:get(<<"groups">>, Apis)
        ]]};
        {error, Reason} -> {error, Reason}
    end.

load_api(Endpoints, Access) ->
    lists:foldl(fun
        (Endpoint, {ok, Api}) -> load_api(Endpoint, Api, Access);
        (_Endpoint, {error, Reason}) -> {error, Reason}
    end, {ok, #{}}, Endpoints).

load_api(Endpoint, InitialApi, Access) ->
    case ?Core:http_request(Endpoint, Access) of
        {ok, ApiResourcesObject} ->
            ApiResources = maps:get(<<"resources">>, ApiResourcesObject),
            BuildApiFun = build_api_fun(Endpoint),
            {ok, lists:foldl(BuildApiFun, InitialApi, ApiResources)};
        {error, Reason} -> {error, Reason}
    end.

build_api_fun(Endpoint) -> fun(ApiResource, Api) ->
    Kind = maps:get(<<"kind">>, ApiResource),
    Name = maps:get(<<"name">>, ApiResource),
    PluralName = maps:get(<<"pluralName">>, ApiResource, <<"">>),
    SingularName = maps:get(<<"singularName">>, ApiResource, <<"">>),
    {ResourceType, SubResource} = resource_type_sub_resource(Name),

    Names = lists:filter(fun({X, _}) -> X /= <<"">> end, [
        {Kind, SubResource},
        {string:lowercase(Kind), SubResource},
        {PluralName, SubResource},
        {SingularName, SubResource},
        {ResourceType, SubResource}
        |
        [{ShortName, SubResource} ||
         ShortName <- maps:get(<<"shortNames">>, ApiResource, [])]
    ]),

    Groups0 = maps:get(groups, Api, #{}),
    Groups = maps:put(ResourceType, group(Endpoint), Groups0),

    Aliases = lists:foldl(fun(Alias, Aliases0) ->
        maps:put(Alias, ResourceType, Aliases0)
    end, maps:get(aliases, Api, #{}), Names),

    maps:put(aliases, Aliases, maps:put(groups, Groups, Api))
end.

group(Endpoint) ->
    case string:split(Endpoint, "/", all) of
        [<<"">>, _Prefix, Version] -> {<<"">>, Version};
        [<<"">>, _Prefix, Group, Version] -> {Group, Version}
    end.

resource_type_sub_resource(Name) ->
    case string:split(Name, "/") of
        [ResourceType, SubResource] -> {ResourceType, SubResource};
        [ResourceType] -> {ResourceType, <<"">>}
    end.

to_binary({T1, T2}) -> {to_binary(T1), to_binary(T2)};
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(B) -> B.
