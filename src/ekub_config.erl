-module(ekub_config).

-export([
    filename/0,

    read/1, read/2,

    minify/1,
    flatten/1,

    context/2,
    cluster/2,
    user/2
]).

-define(Yaml, ekub_yaml).

-include_lib("yamerl/include/yamerl_errors.hrl").

-type kubeconfig() :: map().

-spec filename() -> FileName :: string().
filename() ->
    case os:getenv("KUBECONFIG") of
        false -> filename:join([os:getenv("HOME"), ".kube", "config"]);
        FileName -> FileName
    end.

-spec read(FileName :: string())  ->
    {ok, Config :: kubeconfig()} | {error, Reason :: term()}.

read(FileName) ->
    ?Yaml:read_file(FileName, [lists]).

-spec read(FileName :: string(), Options :: [minify | flatten])  ->
    {ok, Config :: kubeconfig()} | {error, Reason :: term()}.

read(FileName, Options) ->
    lists:foldl(fun
        (minify, {ok, Config}) -> minify(Config);
        (flatten, {ok, Config}) -> flatten(Config);
        (Option, {ok, _Config}) -> {error, {bad_option, Option}};
        (_Option, {error, Reason}) -> {error, Reason}
    end, read(FileName), Options).

-spec minify(Config :: kubeconfig()) ->
    {ok, MinifiedConfig :: kubeconfig()} | {error, Reason :: term()}.

minify(Config) ->
    case maps:find("current-context", Config) of
        {ok, ContextName} -> {ok, minify(ContextName, Config)};
        error -> {error, {minify, no_current_context}}
    end.

minify(ContextName, Config) ->
    #{"context" := #{
        "cluster" := ClusterName,
        "user" := UserName
    }} = Context = context(ContextName, Config),

    maps:put("contexts", [Context],
    maps:put("clusters", [cluster(ClusterName, Config)],
    maps:put("users", [user(UserName, Config)], Config))).

-spec flatten(Config :: kubeconfig()) ->
    {ok, FlattenedConfig :: kubeconfig()} | {error, Reason :: term()}.

flatten(Config) ->
    SubjectRefs = ["user", "cluster", "context"],
    try lists:foldl(fun flatten_subject/2, Config, SubjectRefs) of
        Result -> {ok, Result}
    catch
        throw:Reason -> {error, Reason}
    end.

flatten_subject(Ref, Config) ->
    Subjects = maps:get(Ref ++ "s", Config, []),
    Flattened = lists:map(flatten_subject_fun(Ref), Subjects),
    maps:put(Ref ++ "s", Flattened, Config).

flatten_subject_fun(Ref) ->
    fun(Subject = #{"name" := Name, Ref := Body}) ->
        try maps:fold(fun flatten_subject_body/3, #{}, Body) of
            FlattenedBody -> maps:put(Ref, FlattenedBody, Subject)
        catch
            throw:{Value, Reason} -> throw({Ref, Name, Value, Reason})
        end
    end.

flatten_subject_body(Name, Value, Body) when
    Name == "certificate-authority";
    Name == "client-certificate";
    Name == "client-key"
->
    case read_file_as_base64(Value) of
        {ok, Data} -> maps:put(Name ++ "-data", Data, Body);
        {error, Reason} -> throw({Value, Reason})
    end;

flatten_subject_body(Name, Value, Body) ->
    maps:put(Name, Value, Body).

-spec context(Name :: string(), Config :: kubeconfig()) -> Context :: map().
context(ContextName, Config) ->
    subject("context", ContextName, Config).

-spec cluster(Name :: string(), Config :: kubeconfig()) -> Cluster :: map().
cluster(ClusterName, Config) ->
    subject("cluster", ClusterName, Config).

-spec user(Name :: string(), Config :: kubeconfig()) -> User :: map().
user(UserName, Config) ->
    subject("user", UserName, Config).

subject(Ref, SubjectName, Config) ->
    case [Subject ||
          Subject = #{"name" := Name} <- maps:get(Ref ++ "s", Config),
          Name == SubjectName]
    of
        [] -> #{};
        [Subject|_] -> Subject
    end.

read_file_as_base64(FileName) ->
    case file:read_file(FileName) of
        {ok, Binary} -> {ok, binary_to_list(base64:encode(Binary))};
        {error, Reason} -> {error, Reason}
    end.
