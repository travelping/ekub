-module(ekub_examples).

-export([
    pod_create/0,
    pod_patch/0,
    pod_delete/0
]).

-define(ApiPod, ekub_api_pod).

-define(Namespace, "default").
-define(PodName, "ekub-example").
-define(ContainerName1, "ekub-example-1").
-define(ContainerName2, "ekub-example-2").

-define(Pod,
    #{apiVersion => <<"v1">>,
      kind => <<"Pod">>,
      metadata =>
        #{name => list_to_binary(?PodName)},
      spec =>
        #{containers => [
          #{name => list_to_binary(?ContainerName1),
            image => <<"aialferov/pause:1.0.0">>}
        ]}}
).

-define(Patch,
    #{apiVersion => <<"v1">>,
      kind => <<"Patch">>,
      spec =>
        #{containers => [
          #{name => list_to_binary(?ContainerName1),
            image => <<"aialferov/pause:1.1.0">>}
        ]}}
).

pod_create() -> ?ApiPod:create(?Namespace, ?Pod, access()).
pod_patch() -> ?ApiPod:patch(?Namespace, ?PodName, ?Patch, access()).
pod_delete() -> ?ApiPod:delete(?Namespace, ?PodName, access()).

access() -> {ok, Access} = ekub_access:read(), Access.
