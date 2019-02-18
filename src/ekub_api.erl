-module(ekub_api).

-export([
    endpoint/3, endpoint/4, endpoint/5, endpoint/6,
    group_version/2, group_version/3,
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
       Namespace /= "" -> "/namespaces/" ++ Namespace end ++
    [$/|ResourceType] ++ "s" ++
    if Name == "" -> "";
       Name /= "" -> [$/|Name] end ++
    if SubResource == "" -> "";
       SubResource /= "" -> [$/|SubResource] end.

group_version(ResourceType, Api) -> group_version(ResourceType, "", Api).
group_version(ResourceType, SubResource, Api) ->
    Alias = if SubResource == "" -> ResourceType;
               SubResource /= "" -> ResourceType ++ [$/|SubResource] end,

    Aliases = maps:get(aliases, Api, #{}),
    GroupVersions = maps:get(group_versions, Api, #{}),

    case maps:find(Alias, Aliases) of
        {ok, Kind} -> maps:find(Kind, GroupVersions);
        error -> error
    end.

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
    GroupVersions = maps:get(group_versions, Api, #{}),
    NewGroupVersions = maps:put(Kind, group_version(Endpoint), GroupVersions),

    Names = lists:filter(fun(X) -> X /= "" end, [
        binary_to_list(maps:get(name, ApiResource)),
        binary_to_list(maps:get(pluralName, ApiResource, <<"">>)),
        binary_to_list(maps:get(singularName, ApiResource, <<"">>))
        |
        [binary_to_list(ShortName) ||
         ShortName <- maps:get(shortNames, ApiResource, [])]
    ]),

    AddAliasFun = fun(Alias, Aliases) -> maps:put(Alias, Kind, Aliases) end,
    NewAliases = lists:foldl(AddAliasFun, maps:get(aliases, Api, #{}), Names),

    maps:put(aliases, NewAliases,
    maps:put(group_versions, NewGroupVersions, Api))
end.

group_version(Endpoint) ->
    case tl(string:split(tl(Endpoint), "/", all)) of
        [Version] -> {"", Version};
        [Group, Version] -> {Group, Version}
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
