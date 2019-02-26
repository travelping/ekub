-module(ekub_api).

-export([
    endpoint/3, endpoint/4, endpoint/5, endpoint/6,

    group_version/2, group_version/3,
    resource_type/2, resource_type/3,

    load/1
]).

-define(Core, ekub_core).

-define(CoreEndpoint, "/api/v1").

endpoint(Group, Version, ResourceType) ->
    endpoint(Group, Version, ResourceType, "", "", "").

endpoint(Group, Version, ResourceType, Namespace) ->
    endpoint(Group, Version, ResourceType, Namespace, "", "").

endpoint(Group, Version, ResourceType, Namespace, Name) ->
    endpoint(Group, Version, ResourceType, Namespace, Name, "").

endpoint(Group, Version, ResourceType, Namespace, Name, SubResource) ->
    if Group == "" -> "/api/" ++ Version;
       Group /= "" -> "/apis/" ++ Group ++ [$/|Version] end ++
    if Namespace == "" -> "";
       Namespace /= "" -> "/namespaces/" ++ to_list(Namespace) end ++
    [$/|to_list(ResourceType)] ++
    if Name == "" -> "";
       Name /= "" -> [$/|to_list(Name)] end ++
    if SubResource == "" -> "";
       SubResource /= "" -> [$/|to_list(SubResource)] end.

group_version(ResourceRef, Api) -> group_version(ResourceRef, "", Api).
group_version(ResourceRef, SubResource, Api) ->
    Alias = {to_list(ResourceRef), to_list(SubResource)},
    case maps:find(Alias, maps:get(aliases, Api, #{})) of
        {ok, ResourceType} ->
            maps:find(ResourceType, maps:get(group_versions, Api, #{}));
        error -> error
    end.

resource_type(ResourceRef, Api) -> resource_type(ResourceRef, "", Api).
resource_type(ResourceRef, SubResource, Api) ->
    Alias = {to_list(ResourceRef), to_list(SubResource)},
    maps:find(Alias, maps:get(aliases, Api, #{})).

load(Access) ->
    case load_endpoints(Access) of
        {ok, Endpoints} -> load_api(Endpoints, Access);
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
            ApiResources = maps:get(resources, ApiResourcesObject),
            BuildApiFun = build_api_fun(Endpoint),
            {ok, lists:foldl(BuildApiFun, InitialApi, ApiResources)};
        {error, Reason} -> {error, Reason}
    end.

build_api_fun(Endpoint) -> fun(ApiResource, Api) ->
    Kind = binary_to_list(maps:get(kind, ApiResource)),
    Name = binary_to_list(maps:get(name, ApiResource)),
    PluralName = binary_to_list(maps:get(pluralName, ApiResource, <<"">>)),
    SingularName = binary_to_list(maps:get(singularName, ApiResource, <<"">>)),
    {ResourceType, SubResource} = resource_type_sub_resource(Name),

    GroupVersions = maps:get(group_versions, Api, #{}),
    GroupVersion = group_version(Endpoint),
    NewGroupVersions = maps:put(ResourceType, GroupVersion, GroupVersions),

    Names = lists:filter(fun({X, _}) -> X /= "" end, [
        {Kind, SubResource},
        {string:lowercase(Kind), SubResource},
        {PluralName, SubResource},
        {SingularName, SubResource},
        {ResourceType, SubResource}
        |
        [{binary_to_list(ShortName), SubResource} ||
         ShortName <- maps:get(shortNames, ApiResource, [])]
    ]),

    NewAliases = lists:foldl(fun(Alias, Aliases) ->
        maps:put(Alias, ResourceType, Aliases)
    end, maps:get(aliases, Api, #{}), Names),

    maps:put(aliases, NewAliases,
    maps:put(group_versions, NewGroupVersions, Api))
end.

group_version(Endpoint) ->
    case tl(string:split(tl(Endpoint), "/", all)) of
        [Version] -> {"", Version};
        [Group, Version] -> {Group, Version}
    end.

resource_type_sub_resource(Name) ->
    case string:split(Name, "/") of
        [ResourceType, SubResource] -> {ResourceType, SubResource};
        [ResourceType] -> {ResourceType, ""}
    end.

load_endpoints(Access) ->
    case ?Core:http_request("/apis", Access) of
        {ok, Apis} -> {ok, [?CoreEndpoint|[
            binary_to_list(<<"/apis/", GroupVersion/binary>>)
            || #{preferredVersion := #{groupVersion := GroupVersion}}
            <- maps:get(groups, Apis)
        ]]};
        {error, Reason} -> {error, Reason}
    end.

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(L) -> L.
