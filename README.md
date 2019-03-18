# Ekub

[![License: Apache-2.0][Apache 2.0 Badge]][Apache 2.0]
[![GitHub Release Badge]][GitHub Releases]
[![Erlang Releases Badge]][Erlang Releases]

An [Erlang] client library to work with [Kubernetes] via [Kubernetes API].

## Usage

The "make shell" command will get into an Erlang console with everything needed
(Erlang should be installed before) downloaded, built and loaded.

Read access from the current kubeconfig or service account folder (in case of
running from a pod):

```
{ok, Access} = ekub_access:read().
```

Load current cluster API:

```
{ok, Api} = ekub_api:load(Access).
```

In one go:

```
{ok, {Api, Access}} = ekub:init().
```

Resource YAML file:

```
$ cat deployment.yaml
apiVersion: extensions/v1beta1
kind: Deployment
metadata:
  name: a
spec:
  selector:
    matchLabels:
      app: a
  template:
    metadata:
      labels:
        app: a
    spec:
      containers:
      - image: alpine
        name: a-sleep
        command:
        - sh
        - -c
        - |
          echo "Sleeping..."
          sleep 1000000
      terminationGracePeriodSeconds: 0
```

This example describes deployment, but can be whatever other resource supported
by the Kubernetes API. Or list of resources separated by "---".

Create deployment from the YAML file:

```
{ok, [Resource|_]} = ekub_yaml:read_file("deployment.yaml").
{ok, Object} = ekub:create(Resource, {Api, Access}).
```

Get all the pod names in the current namespace:

```
{ok, PodList} = ekub:read(pod, {Api, Access}).
PodNames = [Name || #{<<"metadata">> := #{<<"name">> := Name}}
                 <- maps:get(<<"items">>, PodList)].
```

Watch all the pods in the current namespace for changes:

```
start_watch({Api, Access}) ->
    case ekub:watch(pods, {Api, Access}) of
        {ok, Ref} -> continue_watch(Ref, {Api, Access});
        {error, Reason} -> {error, Reason}
    end.

continue_watch(Ref, {Api, Access}) ->
    case ekub:watch(Ref) of
        {ok, done} -> start_watch({Api, Access});
        {ok, Events} -> <Process Events>, continue_watch(Ref, {Api, Access});
        {error, timeout} -> continue_watch(Ref, {Api, Access});
        {error, req_not_found} -> start_watch({Api, Access});
        {error, Reason} -> {error, Reason}
    end.
```

Patch a resource (the deployment in this example). The patch YAML file:

```
$ cat patch.yaml
spec:
  template:
    metadata:
      labels:
        purpose: test
```

Read the patch and patch the deployment:

```
{ok, [Patch|_]} = ekub_yaml:read_file("patch.yaml").
ekub:patch(deployment, "", "a", Patch, {Api, Access}).
```

The second parameter is namespace name. When namespace is empty the current one
is used (is read from the "Access" variable if possible).

Execute a command within a pod:

```
ekub:exec(hd(PodNames), "ls -l", {Api, Access}).
```

Get a pod logs:

```
ekub:logs(hd(PodNames), {Api, Access}).
```

Delete the deployment:

```
ekub:delete(deployment, "", "a", [{propagation_policy, 'Foreground'}], {Api, Access}).
```

Or:

```
ekub:delete(Resource, [{propagation_policy, 'Foreground'}], {Api, Access}).
```

## License

Copyright 2018-2019 Travelping GmbH

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

<!-- Links -->

[Erlang]: http://www.erlang.org
[Kubernetes]: https://kubernetes.io
[Kubernetes API]: https://kubernetes.io/docs/reference

<!-- Badges -->

[Apache 2.0]: https://opensource.org/licenses/Apache-2.0
[Apache 2.0 Badge]: https://img.shields.io/badge/License-Apache%202.0-yellowgreen.svg?style=flat-square
[GitHub Releases]: https://github.com/travelping/ekub/releases
[GitHub Release Badge]: https://img.shields.io/github/release/travelping/ekub/all.svg?style=flat-square
[Erlang Releases]: http://www.erlang.org/news/tag/release
[Erlang Releases Badge]: https://img.shields.io/badge/Erlang-21.0%20to%2021.2-983936.svg?style=flat-square
