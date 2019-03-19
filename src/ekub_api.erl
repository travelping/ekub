-module(ekub_api).

-export([
    endpoint/2, endpoint/3, endpoint/4,
    endpoint_build/6,

    namespace/3,

    is_resource/2,
    is_namespaced/2,

    group/2,

    resource_types/1, resource_type/2,
    sub_resource/1,

    load/1
]).

-define(Core, ekub_core).

-define(ApiCoreEndpoint, <<"/api/v1">>).
-define(ApisEndpoint, <<"/apis">>).

%-type namespace_name() ::
%    {namespace(), name()} |
%    {namespace()} | % equal to {namespace(), ""}
%    name() | % equal to {"", name()}
%    {}.
%
%-type namespace() :: atom() | string() | binary().
%-type name() :: atom() | string() | binary().

endpoint(ResourceAlias, {Api, Access}) ->
    endpoint(false, ResourceAlias, {"", ""}, {Api, Access}).

endpoint(ResourceAlias, NamespaceName, {Api, Access}) ->
    endpoint(false, ResourceAlias, NamespaceName, {Api, Access}).

endpoint(Group, ResourceAlias, NamespaceName, {Api, Access}) ->
    IsResource = is_resource(ResourceAlias, Api),
    if IsResource ->
        {GroupName, GroupVersion} = if is_tuple(Group) -> Group;
                                    not Group -> group(ResourceAlias, Api) end,
        endpoint_build(
            to_binary(GroupName),
            to_binary(GroupVersion),
            to_binary(resource_type(ResourceAlias, Api)),
            to_binary(namespace(ResourceAlias, NamespaceName, {Api, Access})),
            to_binary(name(NamespaceName)),
            to_binary(sub_resource(ResourceAlias))
        );
    not IsResource -> "" end.

endpoint_build(
    GroupName, GroupVersion, ResourceType,
    Namespace, Name, SubResource
) ->
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

namespace(ResourceAlias, NamespaceName, {Api, Access}) ->
    Namespace = namespace(NamespaceName),
    IsNamespaced = is_namespaced(ResourceAlias, Api) andalso Namespace /= {},

    if IsNamespaced ->
        IsNamespaceEmpty = string_is_empty(Namespace),
        if IsNamespaceEmpty -> maps:get(namespace, Access, <<"">>);
        not IsNamespaceEmpty -> Namespace end;
    not IsNamespaced -> <<"">> end.

namespace({}) -> {};
namespace({Namespace}) -> Namespace;
namespace({Namespace, _Name}) -> Namespace;
namespace(_Name) -> <<"">>.

name({}) -> <<"">>;
name({_Namespace}) -> <<"">>;
name({_Namespace, Name}) -> Name;
name(Name) -> Name.

is_resource(ResourceAlias, Api) ->
    maps:is_key(alias(ResourceAlias), maps:get(aliases, Api, #{})).

is_namespaced(ResourceAlias, Api) ->
    ResourceType = resource_type(ResourceAlias, Api),
    sets:is_element(ResourceType, maps:get(namespaced, Api, sets:new())).

group(ResourceAlias, Api) ->
    ResourceType = resource_type(ResourceAlias, Api),
    maps:get(ResourceType, maps:get(groups, Api, #{})).

resource_types(Api) ->
    [binary_to_list(Type) || Type <- maps:keys(maps:get(groups, Api, #{}))].

resource_type(ResourceAlias, Api) ->
    maps:get(alias(ResourceAlias), maps:get(aliases, Api, #{})).

sub_resource({_ResourceRef, SubResource}) -> SubResource;
sub_resource(_ResourceAlias) -> "".

alias(Alias) when is_tuple(Alias) -> to_binary(Alias);
alias(Alias) -> to_binary({Alias, <<"">>}).

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
    IsNamespaced = maps:get(<<"namespaced">>, ApiResource, false),
    {ResourceType, SubResource} = resource_type_sub_resource(Name),

    ResourceAliases = lists:filter(fun({X, _}) -> X /= <<"">> end, [
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

    Aliases = lists:foldl(fun(ResourceAlias, Aliases0) ->
        maps:put(ResourceAlias, ResourceType, Aliases0)
    end, maps:get(aliases, Api, #{}), ResourceAliases),

    Namespaced0 = maps:get(namespaced, Api, sets:new()),
    Namespaced = if IsNamespaced -> sets:add_element(ResourceType, Namespaced0);
                 not IsNamespaced -> Namespaced0 end,

    maps:put(namespaced, Namespaced,
    maps:put(aliases, Aliases,
    maps:put(groups, Groups, Api)))
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

string_is_empty('') -> true;
string_is_empty(String) -> string:is_empty(String).

to_binary({T1, T2}) -> {to_binary(T1), to_binary(T2)};
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(B) -> B.
