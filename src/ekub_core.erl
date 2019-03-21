-module(ekub_core).

-export([
    http_request/2, http_request/3, http_request/4,
    http_request/5, http_request/6, http_request/7,

    http_stream_request/2, http_stream_request/3,
    http_stream_request/6, http_stream_request/7,

    http_close/1,

    http_body_read/2,
    http_stream_read/1,

    ws_request/2, ws_request/3, ws_request/4, ws_request/5,

    ws_connect/2, ws_connect/3, ws_connect/4, ws_connect/5,
    ws_close/1,

    ws_recv/1, ws_recv/2,
    ws_recv_close/1, ws_recv_close/2
]).

-export_types([
    method/0,
    endpoint/0,

    query/0,
    headers/0,

    http_options/0,
    ws_options/0,

    body/0,

    http_response/0,
    http_stream_ref/0, http_stream_response/0, http_stream_read_result/0,

    ws_connect_result/0, ws_response/0,
    socket/0
]).

-define(Access, ekub_access).

-type access() :: ?Access:access().

-type method() :: get | put | post | patch | delete.

-type endpoint() :: string().
-type url() :: string().

-type query() :: [{atom(), string()}].
-type headers() :: [{string(), string()}].

-type http_options() :: list().
-type ws_options() :: list().

-type body() :: map() | binary().

-type http_response() :: {ok, body()} |
                         {error, Reason :: term()}.

-type http_stream_ref() :: hackney:client_ref().

-type http_stream_response() :: {ok, http_stream_ref()} |
                                {error, Reason :: body()} |
                                {error, Reason :: term()}.

-type http_stream_read_result() :: {ok, done} |
                                   {ok, body()} |
                                   {error, timeout} |
                                   {error, req_not_found} |
                                   {error, Reason :: term()}.

-type ws_connect_result() :: {ok, socket()} |
                             {error, Reason :: body()}.

-type ws_response() :: {ok, Data :: binary()} |
                       {error, Reason :: term()}.

-type socket() :: gen_tcp:socket() |
                  ssl:socket().

-define(HeaderContentTypeJsonText, {"Content-Type", "application/json"}).
-define(HeaderContentTypeJson, {<<"Content-Type">>, <<"application/json">>}).
-define(HeaderContentTypeJsonPatch,
    {<<"Content-Type">>, <<"application/strategic-merge-patch+json">>}).

-define(JsonDecodeOptions, [return_maps]).

-define(RecvTimeout, 60 * 1000). % milliseconds

-define(SslOptions, [
    insecure_skip_tls_verify,
    ca_cert,
    client_cert,
    client_key
]).

-spec http_request(endpoint(), access()) -> http_response().
http_request(Endpoint, Access) ->
    http_request(Endpoint, [], Access).

-spec http_request(endpoint(), query(), access()) -> http_response().
http_request(Endpoint, Query, Access) ->
    http_request(get, Endpoint, Query, [], <<>>, [], Access).

-spec http_request(method(), endpoint(), query(), access()) -> http_response().
http_request(Method, Endpoint, Query, Access) ->
    http_request(Method, Endpoint, Query, [], <<>>, [], Access).

-spec http_request(method(), endpoint(), query(),
                   body(), access()) -> http_response().

http_request(Method, Endpoint, Query, Body, Access) ->
    http_request(Method, Endpoint, Query, [], Body, [], Access).

-spec http_request(method(), endpoint(), query(), headers(),
                   body(), http_options(), access()) -> http_response().

http_request(Method, Endpoint, Query, Headers, Body, Options, Access) ->
    Url = url(Endpoint, Query, Access),
    http_request(Method, Url, Headers, Body, Options, Access).

-spec http_request(method(), url(), headers(),
                   body(), http_options(), access()) -> http_response().

http_request(Method, Url, Headers, Body, Options, Access) ->
    case http_request_ref(Method, Url, Headers, Body, Options, Access) of
        {ok, {_Code, ResponseHeaders, Ref}} ->
            http_body_read(ResponseHeaders, Ref);
        {error, Reason} ->
            {error, Reason}
    end.

-spec http_stream_request(endpoint(), access()) -> http_stream_response().
http_stream_request(Endpoint, Access) ->
    http_stream_request(Endpoint, [], Access).

-spec http_stream_request(endpoint(), query(), access()) ->
    http_stream_response().

http_stream_request(Endpoint, Query, Access) ->
    http_stream_request(get, Endpoint, Query, [], <<>>, [], Access).

-spec http_stream_request(
    method(), endpoint(), query(), headers(),
    body(), http_options(), access()
) ->
    http_stream_response().

http_stream_request(Method, Endpoint, Query, Headers, Body, Options, Access) ->
    Url = url(Endpoint, Query, Access),
    http_stream_request(Method, Url, Headers, Body, Options, Access).

-spec http_stream_request(
    method(), url(), headers(),
    body(), http_options(), access()
) ->
    http_stream_response().

http_stream_request(Method, Url, Headers, Body, Options, Access) ->
    case http_request_ref(Method, Url, Headers, Body, Options, Access) of
        {ok, {_Code, _ResponseHeaders, Ref}} -> {ok, Ref};
        {error, Reason} -> {error, Reason}
    end.

-spec http_close(http_stream_ref()) -> ok.
http_close(Ref) ->
    hackney:close(Ref).

http_request_ref(Method, Url, Headers, Body, Options, Access) ->
    case hackney:request(
        Method, Url, headers(Method, Headers, Body, Access),
        body(Body), http_options(Options, Access)
    ) of
        {ok, Code, ResponseHeaders, Ref} when Code >= 200, Code =< 299 ->
            {ok, {Code, ResponseHeaders, Ref}};
        {ok, _Code, ResponseHeaders, Ref} ->
            case http_body_read(ResponseHeaders, Ref) of
                {ok, ResponseBody} -> {error, ResponseBody};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} -> {error, Reason}
    end.

-spec http_body_read(headers(), http_stream_ref()) -> http_response();
                    (headers(), body()) -> body().

http_body_read(Headers, Ref) when is_reference(Ref) ->
    case hackney:body(Ref) of
        {ok, Body} -> {ok, http_body_read(Headers, Body)};
        {error, Reason} -> {error, Reason}
    end;

http_body_read(Headers, Body) ->
    IsJson = ?HeaderContentTypeJson ==
                 lists:keyfind(<<"Content-Type">>, 1, Headers) orelse
             ?HeaderContentTypeJsonText ==
                 lists:keyfind("Content-Type", 1, Headers),

    if IsJson -> jsx:decode(Body, ?JsonDecodeOptions);
    not IsJson -> Body end.

-spec http_stream_read(http_stream_ref()) -> http_stream_read_result().
http_stream_read(Ref) ->
    http_stream_read(Ref, false).

http_stream_read(Ref, DecodeFun) ->
    case hackney:stream_body(Ref) of
        done -> {ok, done};
        {ok, Data} -> http_stream_to_json(Ref, Data, DecodeFun);
        {error, Reason} -> {error, Reason}
    end.

http_stream_to_json(_Ref, <<>>, false) -> {ok, []};
http_stream_to_json(Ref, Data, false) ->
    {incomplete, DecodeFun} = jsx:decode(<<"[">>, [stream|?JsonDecodeOptions]),
    http_stream_to_json(Ref, Data, DecodeFun);

http_stream_to_json(Ref, <<>>, DecodeFun) ->
    http_stream_read(Ref, DecodeFun);

http_stream_to_json(Ref, Data, DecodeFun) ->
    IsComplete = binary:last(Data) == $\n,
    DecodableData = binary:replace(Data, <<"\n">>, <<",">>, [global]),
    {incomplete, NewDecodeFun} = DecodeFun(DecodableData),

    case IsComplete of
        true ->
            {incomplete, F} = NewDecodeFun(<<"]">>),
            {ok, F(end_stream)};
        false ->
            http_stream_read(Ref, NewDecodeFun)
    end.

-spec ws_request(endpoint(), access()) -> ws_response().
ws_request(Endpoint, Access) ->
    ws_request(Endpoint, [], [], [], Access).

-spec ws_request(endpoint(), query(), access()) -> ws_response().
ws_request(Endpoint, Query, Access) ->
    ws_request(Endpoint, Query, [], [], Access).

-spec ws_request(endpoint(), query(), headers(), ws_options(), access()) ->
    ws_response().

ws_request(Endpoint, Query, Headers, Options, Access) ->
    ws_request(url(Endpoint, Query, Access), Headers, Options, Access).

-spec ws_request(url(), headers(), ws_options(), access()) -> ws_response().
ws_request(Url, Headers, Options, Access) ->
    {RecvTimeout, FinalOptions} = options_take(recv_timeout, Options, infinity),
    case ws_connect(Url, Headers, FinalOptions, Access) of
        {ok, Socket} -> ws_recv_close(Socket, RecvTimeout);
        {error, Reason} -> {error, Reason}
    end.

-spec ws_connect(endpoint(), access()) -> ws_connect_result().
ws_connect(Endpoint, Access) ->
    ws_connect(Endpoint, [], [], [], Access).

-spec ws_connect(endpoint(), query(), access()) -> ws_connect_result().
ws_connect(Endpoint, Query, Access) ->
    ws_connect(Endpoint, Query, [], [], Access).

-spec ws_connect(endpoint(), query(), headers(), ws_options(), access()) ->
    ws_connect_result().

ws_connect(Endpoint, Query, Headers, Options, Access) ->
    ws_connect(url(Endpoint, Query, Access), Headers, Options, Access).

-spec ws_connect(url(), headers(), ws_options(), access()) ->
    ws_connect_result().

ws_connect(Url, Headers, Options, Access) ->
    case ewsc:connect(binary_to_list(Url),
                      headers(Headers, Access),
                      ws_options(Options, Access))
    of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, ws_error_read(Reason)}
    end.

ws_error_read({http_message, response, _Status, ResponseHeaders, Body}) ->
    http_body_read(ResponseHeaders, Body);
ws_error_read(Reason) -> Reason.

-spec ws_close(socket()) -> ok.
ws_close(Socket) -> ewsc:close(Socket).

-spec ws_recv(socket()) -> ws_response().
ws_recv(Socket) -> ws_recv(Socket, infinity).

-spec ws_recv(socket(), timeout()) -> ws_response().
ws_recv(Socket, Timeout) -> ws_recv(Socket, Timeout, <<"">>).

ws_recv(Socket, Timeout, Acc) ->
    case ewsc:recv(Socket, Timeout) of
        {ok, [close|Messages]} ->
            {ok, ws_append_messages(Acc, Messages)};
        {ok, Messages} ->
            ws_recv(Socket, Timeout, ws_append_messages(Acc, Messages));
        {error, Reason} ->
            {error, Reason}
    end.

-spec ws_recv_close(socket()) -> ws_response().
ws_recv_close(Socket) -> ws_recv_close(Socket, infinity).

-spec ws_recv_close(socket(), timeout()) -> ws_response().
ws_recv_close(Socket, Timeout) ->
    Result = ws_recv(Socket, Timeout),
    ws_close(Socket),
    Result.

ws_append_messages(Messages, []) -> Messages;
ws_append_messages(Messages, NewMessages) ->
    NewMessagesStripped = [M || <<_, M/binary>> <- NewMessages],
    <<Messages/binary, (iolist_to_binary(NewMessagesStripped))/binary>>.

endpoint(Path, Args) ->
    lists:flatten(io_lib:format(Path, Args)).

url({Path, Args}, Query, Access) ->
    url(endpoint(Path, Args), Query, Access);

url(Endpoint, Query, Access) ->
    iolist_to_binary([maps:get(server, Access, ""),
                      Endpoint, url_query(Query)]).

url_query([]) -> "";
url_query(Query) ->
    QueryParams = lists:map(fun url_query_param/1, Query),
    iolist_to_binary([$?|lists:join($&, QueryParams)]).

url_query_param({Key, Value}) ->
    <<(http_uri:encode(to_camel_case(Key)))/binary, $=,
      (http_uri:encode(to_binary(Value)))/binary>>.

headers(patch, Headers, Body, Access) when is_map(Body) ->
    [?HeaderContentTypeJsonPatch|headers(Headers, Access)];

headers(_Method, Headers, Body, Access) when is_map(Body) ->
    [?HeaderContentTypeJson|headers(Headers, Access)];

headers(_Method, Headers, _Body, Access) -> headers(Headers, Access).

headers(Headers, Access) ->
    [{<<"Authorization">>, authorization(Access)}|Headers].

authorization(#{token := Token}) -> iolist_to_binary(["Bearer ", Token]);
authorization(#{username := UserName, password := Password}) ->
    base64:encode(iolist_to_binary([UserName, $:, Password]));
authorization(_Access) -> <<>>.

body(<<>>) -> <<>>;
body(Body) when is_map(Body) -> jsx:encode(Body).

http_options(Options, Access) -> [
    {ssl_options, ssl_options(Access)},
    {recv_timeout, proplists:get_value(recv_timeout, Options, ?RecvTimeout)}
    |Options
].

ws_options(Options, Access) ->
    ssl_options(Access) ++ Options.

ssl_options(Access) ->
    [{fail_if_no_peer_cert, true},
     {server_name_indication, undefined} % not specifying at all
                                         % is not the same as 'undefined'
     |lists:filtermap(ssl_option_fun(Access), ?SslOptions)].

ssl_option_fun(Access) -> fun
    (insecure_skip_tls_verify) ->
        Verify = case maps:get(insecure_skip_tls_verify, Access, false) of
            true -> verify_none;
            false -> verify_peer
        end,
        {true, {verify, Verify}};
    (ca_cert) ->
        CaCert = maps:get(ca_cert, Access, false),
        CaCert /= false andalso {true, {cacerts, [CaCert]}};
    (client_cert) ->
        ClientCert = maps:get(client_cert, Access, false),
        ClientCert /= false andalso {true, {cert, ClientCert}};
    (client_key) ->
        ClientKey = maps:get(client_key, Access, false),
        ClientKey /= false andalso {true, {key, ClientKey}}
end.

options_take(Key, Options1, Default) ->
    case lists:keytake(Key, 1, Options1) of
        {value, {Key, Value}, Options2} -> {Value, Options2};
        false -> {Default, Options1}
    end.

to_camel_case(Term) ->
    case string:split(to_binary(Term), "_", all) of
        [First] -> First;
        [First|Rest] ->
            CamelRest = [<<(H-32), T/binary>> || <<H, T/binary>> <- Rest],
            iolist_to_binary([First|CamelRest])
    end.

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(N) when is_number(N) -> integer_to_binary(N);
to_binary(B) -> B.
