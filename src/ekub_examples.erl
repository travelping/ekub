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
       name: ekub-examples
    ",
    access()
).

namespace_delete() -> ?ApiNs:delete(
    "ekub-examples", % namespace
    access()
).

pod_create() -> ?ApiPod:create(
    "ekub-examples", % namespace
    "
     apiVersion: v1
     kind: Pod
     metadata:
       name: ekub-examples
     spec:
       containers:
       - name: ekub-examples
         image: aialferov/pause:1.0.0
    ",
    %%% map based example
    %
    % #{apiVersion => <<"v1">>,
    %   kind => <<"Pod">>,
    %   metadata =>
    %     #{name => <<"ekub-examples">>},
    %   spec =>
    %     #{containers => [
    %       #{name => <<"ekub-examples">>,
    %         image => <<"aialferov/pause:1.0.0">>}
    %     ]}}
    %%%
    access()
).

pod_patch() -> ?ApiPod:patch(
    "ekub-examples", % namespace
    "ekub-examples", % pod name
    "
     apiVersion: v1
     kind: Patch
     spec:
       containers:
       - name: ekub-examples
         image: aialferov/pause:1.1.0
    ",
    %%% map based example
    %
    % #{apiVersion => <<"v1">>,
    %   kind => <<"Patch">>,
    %   spec =>
    %     #{containers => [
    %       #{name => <<"ekub-examples">>,
    %         image => <<"aialferov/pause:1.1.0">>}
    %     ]}},
    %%%
    access()
).

pod_list() -> ?ApiPod:list(
    "ekub-examples", % namespace
    access()
).

pod_exec() -> ?ApiPod:exec(
    "ekub-examples", % namespace
    "ekub-examples", % pod name
    "ekub-examples", % container name
    "pause version", % command
    % 10000, % optional timeout in ms
    access()
).

pod_delete() -> ?ApiPod:delete(
    "ekub-examples",
    "ekub-examples",
    access()
).

deployment_create() -> ?ApiDeploy:create(
    "ekub-examples", % namespace
    "
     apiVersion: apps/v1beta1
     kind: Deployment
     metadata:
       name: ekub-examples
       labels:
         app: ekub-examples
     spec:
       replicas: 3
       template:
         metadata:
           labels:
             app: ekub-examples
         spec:
           containers:
           - name: ekub-examples
             image: aialferov/pause:1.0.0
    ",
    access()
).

deployment_delete() -> ?ApiDeploy:delete(
    "ekub-examples", % namespace
    "ekub-examples", % deployment name
    [{orphan_dependents, false}], % deprecated but still works
    % [{propagation_policy, "Background"}], % recommended but doesn't work
    access()
).

access() ->
    {ok, Access} = ekub_access:read(),
    Access.
