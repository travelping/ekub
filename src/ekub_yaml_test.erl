-module(ekub_yaml_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, ekub_yaml).

-define(Yaml, "
    apiVersion: v1
    kind: Pod
    metadata:
      name: pod-example
      annotations:
        annotation1: |
          value1
          value2
        annotation2: \"\"
        annotation3: ''
        annotation4: [\"\"] 
    spec:
      replicas: 3
      containers:
      - name: \"alpine\"
        image: alpine
        command: [\"echo\"]
        args: [\"Hello World\"]
").

-define(YamlMulti, "
---
    apiVersion: v1
    kind: Pod
    metadata:
      name: pod-example-1
---
    apiVersion: v1
    kind: Pod
    metadata:
      name: pod-example-2
---
").

yamerl_started_test() ->
    ?assertEqual({ok, [yamerl]}, application:ensure_all_started(yamerl)).

read_yaml_test() ->
    ?assertEqual({ok, [
        #{"apiVersion" => "v1",
          "kind" => "Pod",
          "metadata" =>
            #{"name" => "pod-example",
              "annotations" =>
                #{"annotation1" => "value1\nvalue2\n",
                  "annotation2" => "",
                  "annotation3" => "",
                  "annotation4" => [""]}},
          "spec" =>
            #{"replicas" => 3,
              "containers" => [
                #{"name" => "alpine",
                  "image" => "alpine",
                  "command" => ["echo"],
                  "args" => ["Hello World"]}
              ]}}
    ]}, ?M:read(?Yaml)).

read_yaml_binary_test() ->
    ?assertEqual({ok, [
        #{<<"apiVersion">> => <<"v1">>,
          <<"kind">> => <<"Pod">>,
          <<"metadata">> =>
            #{<<"name">> => <<"pod-example">>,
              <<"annotations">> =>
                #{<<"annotation1">> => <<"value1\nvalue2\n">>,
                  <<"annotation2">> => <<"">>,
                  <<"annotation3">> => <<"">>,
                  <<"annotation4">> => [<<"">>]}},
          <<"spec">> =>
            #{<<"replicas">> => 3,
              <<"containers">> => [
                #{<<"name">> => <<"alpine">>,
                  <<"image">> => <<"alpine">>,
                  <<"command">> => [<<"echo">>],
                  <<"args">> => [<<"Hello World">>]}
              ]}}
    ]}, ?M:read(?Yaml, [binary])).

read_yaml_multi_test() ->
    ?assertEqual({ok, [

        #{"apiVersion" => "v1",
          "kind" => "Pod",
          "metadata" =>
            #{"name" => "pod-example-1"}},

        #{"apiVersion" => "v1",
          "kind" => "Pod",
          "metadata" =>
            #{"name" => "pod-example-2"}}

    ]}, ?M:read(?YamlMulti)).

read_yaml_string_test() ->
    ?assertEqual({ok, [
        #{"string1 string2" => null}
    ]}, ?M:read("string1\n  string2")).

read_yaml_string_binary_test() ->
    ?assertEqual({ok, [#{<<"string">> => null}]}, ?M:read("string", [binary])).

read_yaml_multi_string_test() ->
    ?assertEqual({ok, [
        #{"string1" => null},
        #{"string2" => null}
    ]}, ?M:read("string1\n---\nstring2")).
