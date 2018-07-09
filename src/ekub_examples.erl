-module(ekub_examples).

-export([
    namespace_create/0,
    namespace_delete/0,

    pod_create/0,
    pod_patch/0,

    pod_list/0,
    pod_exec/0,

    pod_delete/0,

    deployment_create/0,
    deployment_delete/0
]).

-define(ApiNs, ekub_api_namespace).
-define(ApiPod, ekub_api_pod).
-define(ApiDeploy, ekub_api_deployment).

% All manifests could be defined as either a Yaml document or an Erlang map

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

pod_create() -> ?ApiPod:create(
    "ekub-example", % namespace
    "
     apiVersion: v1
     kind: Pod
     metadata:
       name: ekub-example
     spec:
       containers:
       - name: ekub-example
         image: aialferov/pause:1.0.0
    ",
    %%% map based example
    %
    % #{apiVersion => <<"v1">>,
    %   kind => <<"Pod">>,
    %   metadata =>
    %     #{name => <<"ekub-example">>},
    %   spec =>
    %     #{containers => [
    %       #{name => <<"ekub-example">>,
    %         image => <<"aialferov/pause:1.0.0">>}
    %     ]}}
    %%%
    access()
).

pod_patch() -> ?ApiPod:patch(
    "ekub-example", % namespace
    "ekub-example", % pod name
    "
     apiVersion: v1
     kind: Patch
     spec:
       containers:
       - name: ekub-example
         image: aialferov/pause:1.1.0
    ",
    %%% map based example
    %
    % #{apiVersion => <<"v1">>,
    %   kind => <<"Patch">>,
    %   spec =>
    %     #{containers => [
    %       #{name => <<"ekub-example">>,
    %         image => <<"aialferov/pause:1.1.0">>}
    %     ]}},
    %%%
    access()
).

pod_list() -> ?ApiPod:list(
    "ekub-example", % namespace
    access()
).

pod_exec() -> ?ApiPod:exec(
    "ekub-example", % namespace
    "ekub-example", % pod name
    "ekub-example", % container name
    "pause version", % command
    % 10000, % optional timeout in ms
    access()
).

pod_delete() -> ?ApiPod:delete(
    "ekub-example",
    "ekub-example",
    access()
).

deployment_create() -> ?ApiDeploy:create(
    "ekub-example", % namespace
    "
     apiVersion: apps/v1beta1
     kind: Deployment
     metadata:
       name: ekub-example
       labels:
         app: ekub-example
     spec:
       replicas: 3
       template:
         metadata:
           labels:
             app: ekub-example
         spec:
           containers:
           - name: ekub-example
             image: aialferov/pause:1.0.0
    ",
    access()
).

deployment_delete() -> ?ApiDeploy:delete(
    "ekub-example", % namespace
    "ekub-example", % deployment name
    [{orphan_dependents, false}],
    access()
).

access() ->
    {ok, Access} = ekub_access:read(),
    Access.
