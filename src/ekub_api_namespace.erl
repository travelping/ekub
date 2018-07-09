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
    ?Core:http_request(post, ?ApiNamespaces, [], Body, Access).

patch(Namespace, Patch, Access) ->
    ?Core:http_request(patch, {?ApiNamespace, [Namespace]}, [], Patch, Access).

delete(Namespace, Access) -> delete(Namespace, [], Access).
delete(Namespace, Options, Access) ->
    ?Core:http_request(delete, {?ApiNamespace, [Namespace]}, Options, Access).

read(Namespace, Access) ->
    ?Core:http_request({?ApiNamespace, [Namespace]}, Access).

list(Access) -> list([], Access).
list(Options, Access) ->
    ?Core:http_request(?ApiNamespaces, Options, Access).
