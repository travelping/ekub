-module(ekub_api_deployment).

-export([
    create/3,
    patch/4,
    delete/3, delete/4,

    read/3,
    read_scale/3
]).

-define(Core, ekub_core).

-define(ApiDeployment, "/apis/apps/v1beta1/namespaces/~s/deployments/~s").
-define(ApiDeployments, "/apis/apps/v1beta1/namespaces/~s/deployments").

-define(ApiDeploymentScale, ?ApiDeployment ++ "/scale").

create(Namespace, Deployment, Access) ->
    Resource = {?ApiDeployments, [Namespace]},
    ?Core:http_request(post, Resource, [], Deployment, Access).

patch(Namespace, DeploymentName, Patch, Access) ->
    Resource = {?ApiDeployment, [Namespace, DeploymentName]},
    ?Core:http_request(patch, Resource, [], Patch, Access).

delete(Namespace, DeploymentName, Access) ->
    delete(Namespace, DeploymentName, [], Access).

delete(Namespace, DeploymentName, Options, Access) ->
    Resource = {?ApiDeployment, [Namespace, DeploymentName]},
    ?Core:http_request(delete, Resource, Options, Access).

read(Namespace, DeploymentName, Access) ->
    Resource = {?ApiDeployment, [Namespace, DeploymentName]},
    ?Core:http_request(Resource, Access).

read_scale(Namespace, DeploymentName, Access) ->
    Resource = {?ApiDeploymentScale, [Namespace, DeploymentName]},
    ?Core:http_request(Resource, Access).
