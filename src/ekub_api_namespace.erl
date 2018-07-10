-module(ekub_api_namespace).

-export([
    create/2,
    patch/3,
    delete/2, delete/3,

    read/2,
    list/1, list/2
]).

-define(Core, ekub_core).

-define(ApiNamespace, "/api/v1/namespaces/~s").
-define(ApiNamespaces, "/api/v1/namespaces").

create(Body, Access) ->
    Resource = ?ApiNamespaces,
    ?Core:http_request(post, Resource, [], Body, Access).

patch(Namespace, Patch, Access) ->
    Resource = {?ApiNamespace, [Namespace]},
    ?Core:http_request(patch, Resource, [], Patch, Access).

delete(Namespace, Access) -> delete(Namespace, [], Access).
delete(Namespace, Options, Access) ->
    Resource = {?ApiNamespace, [Namespace]},
    ?Core:http_request(delete, Resource, Options, Access).

read(Namespace, Access) ->
    Resource = {?ApiNamespace, [Namespace]},
    ?Core:http_request(Resource, Access).

list(Access) -> list([], Access).
list(Options, Access) ->
    Resource = ?ApiNamespaces,
    ?Core:http_request(Resource, Options, Access).
