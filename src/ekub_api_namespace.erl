-module(ekub_api_namespace).

-export([
    create/2,
    patch/3,
    delete/2, delete/3,

    read/2,
    list/1, list/2
]).

-define(Core, ekub_core).

create(Body, Access) ->
    ?Core:http_request(post, "/api/v1/namespaces", [], Body, Access).

patch(Namespace, Patch, Access) ->
    Resource = {"/api/v1/namespaces/~s", [Namespace]},
    ?Core:http_request(patch, Resource, [], Patch, Access).

delete(Namespace, Access) -> delete(Namespace, [], Access).
delete(Namespace, Options, Access) ->
    Resource = {"/api/v1/namespaces/~s", [Namespace]},
    ?Core:http_request(delete, Resource, Options, Access).

read(Namespace, Access) ->
    ?Core:http_request({"/api/v1/namespaces/~s", [Namespace]}, Access).

list(Access) -> list([], Access).
list(Options, Access) ->
    ?Core:http_request("/api/v1/namespaces", Options, Access).
