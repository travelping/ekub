-module(ekub_examples).

-export([
    namespace_create/0,
    namespace_delete/0,

    pod_yaml_create/0,
    pod_map_create/0,

    pod_yaml_patch/0,
    pod_map_patch/0,

    pod_list/0,

    pod_yaml_delete/0,
    pod_map_delete/0
]).

-define(ApiNs, ekub_api_namespace).
-define(ApiPod, ekub_api_pod).

namespace_create() -> ?ApiNs:create(
    "
     apiVersion: v1
     kind: Namespace
     metadata:
       name: ekub-example
    ",
    access()
).

namespace_delete() -> ?ApiNs:delete(
    "ekub-example", % namespace
    access()
).

pod_yaml_create() -> ?ApiPod:create(
    "ekub-example", % namespace
    "
     apiVersion: v1
     kind: Pod
     metadata:
       name: ekub-example-yaml
     spec:
       containers:
       - name: ekub-example-yaml
         image: aialferov/pause:1.0.0
    ",
    access()
).

pod_map_create() -> ?ApiPod:create(
    "ekub-example", % namespace

    #{apiVersion => <<"v1">>,
      kind => <<"Pod">>,
      metadata =>
        #{name => <<"ekub-example-map">>},
      spec =>
        #{containers => [
          #{name => <<"ekub-example-map">>,
            image => <<"aialferov/pause:1.0.0">>}
        ]}},

    access()
).

pod_yaml_patch() -> ?ApiPod:patch(
    "ekub-example", % namespace
    "ekub-example-yaml", % pod name
    "
     apiVersion: v1
     kind: Patch
     spec:
       containers:
       - name: ekub-example-yaml
         image: aialferov/pause:1.1.0
    ",
    access()
).

pod_map_patch() -> ?ApiPod:patch(
    "ekub-example", % namespace
    "ekub-example-map", % pod name

    #{apiVersion => <<"v1">>,
      kind => <<"Patch">>,
      spec =>
        #{containers => [
          #{name => <<"ekub-example-map">>,
            image => <<"aialferov/pause:1.1.0">>}
        ]}},

    access()
).

pod_list() -> ?ApiPod:list(
    "ekub-example", % namespace
    access()
).

pod_yaml_delete() -> ?ApiPod:delete(
    "ekub-example", % namespace
    "ekub-example-yaml", % pod name
    access()
).

pod_map_delete() -> ?ApiPod:delete(
    "ekub-example", % namespace
    "ekub-example-map", % pod name
    access()
).

access() -> {ok, Access} = ekub_access:read(), Access.
