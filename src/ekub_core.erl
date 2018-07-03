-module(ekub_core).

-export([
    http_request/2, http_request/3,
    http_request/6, http_request/7,

    http_stream_request/2, http_stream_request/3,
    http_stream_request/6, http_stream_request/7,

    http_body_read/2,
    http_stream_read/1,

    ws_request/2, ws_request/3, ws_request/4, ws_request/5,

    ws_connect/2, ws_connect/3, ws_connect/4, ws_connect/5,
    ws_close/1,

    ws_recv/1,
    ws_recv_close/1
]).

-define(JsonDecodeOptions, [return_maps, {labels, atom}]).

-define(RecvTimeout, 60 * 1000). % milliseconds

-define(SslOptions, [
    insecure_skip_tls_verify,
    ca_cert,
    client_cert,
    client_key
]).

http_request(Resource, Access) ->
    http_request(Resource, [], Access).

http_request(Resource, Query, Access) ->
    http_request(get, Resource, Query, [], <<>>, [], Access).

http_request(Method, Resource, Query, Headers, Body, Options, Access) ->
    Url = url(Resource, Query, Access),
    http_request(Method, Url, Headers, Body, Options, Access).

http_request(Method, Url, Headers, Body, Options, Access) ->
    case http_request_ref(Method, Url, Headers, Body, Options, Access) of
        {ok, {_Code, ResponseHeaders, Ref}} ->
            http_body_read(ResponseHeaders, Ref);
        {error, Reason} ->
            {error, Reason}
    end.

http_stream_request(Resource, Access) ->
    http_stream_request(Resource, [], Access).

http_stream_request(Resource, Query, Access) ->
    http_stream_request(get, Resource, Query, [], <<>>, [], Access).

http_stream_request(Method, Resource, Query, Headers, Body, Options, Access) ->
    Url = url(Resource, Query, Access),
    http_stream_request(Method, Url, Headers, Body, Options, Access).

http_stream_request(Method, Url, Headers, Body, Options, Access) ->
    case http_request_ref(Method, Url, Headers, Body, Options, Access) of
        {ok, {_Code, _ResponseHeaders, Ref}} -> {ok, Ref};
        {error, Reason} -> {error, Reason}
    end.

http_request_ref(Method, Url, Headers, Body, Options, Access) ->
    case hackney:request(
        Method, Url, headers(Headers, Access),
        Body, http_options(Options, Access)
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

http_body_read(Headers, Ref) when is_reference(Ref) ->
    case hackney:body(Ref) of
        {ok, Body} -> {ok, http_body_read(Headers, Body)};
        {error, Reason} -> {error, Reason}
    end;

http_body_read(Headers, Body) ->
    IsJson = {<<"Content-Type">>, <<"application/json">>} ==
             lists:keyfind(<<"Content-Type">>, 1, Headers) orelse
             {"Content-Type", "application/json"} ==
             lists:keyfind("Content-Type", 1, Headers),

    if IsJson -> jsx:decode(Body, ?JsonDecodeOptions);
    not IsJson -> Body end.

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

ws_request(Resource, Access) ->
    ws_request(Resource, [], [], [], Access).

ws_request(Resource, Query, Access) ->
    ws_request(Resource, Query, [], [], Access).

ws_request(Resource, Query, Headers, Options, Access) ->
    ws_request(url(Resource, Query, Access), Headers, Options, Access).

ws_request(Url, Headers, Options, Access) ->
    case ws_connect(Url, Headers, Options, Access) of
        {ok, Socket} -> ws_recv_close(Socket);
        {error, Reason} -> {error, Reason}
    end.

ws_connect(Resource, Access) ->
    ws_connect(Resource, [], [], [], Access).

ws_connect(Resource, Query, Access) ->
    ws_connect(Resource, Query, [], [], Access).

ws_connect(Resource, Query, Headers, Options, Access) ->
    ws_connect(url(Resource, Query, Access), Headers, Options, Access).

ws_connect(Url, Headers, Options, Access) ->
    case ewsc:connect(Url, headers(Headers, Access),
                           ws_options(Options, Access))
    of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, ws_error_read(Reason)}
    end.

ws_error_read({http_message, response, _Status, ResponseHeaders, Body}) ->
    http_body_read(ResponseHeaders, Body);
ws_error_read(Reason) -> Reason.

ws_close(Socket) -> ewsc:close(Socket).

ws_recv(Socket) -> ws_recv(Socket, <<"">>).

ws_recv(Socket, Acc) ->
    case ewsc:recv(Socket) of
        {ok, [close|Messages]} ->
            {ok, binary_to_list(ws_append_messages(Acc, Messages))};
        {ok, Messages} ->
            ws_recv(Socket, ws_append_messages(Acc, Messages));
        {error, Reason} ->
            {error, Reason}
    end.

ws_recv_close(Socket) ->
    Result = ws_recv(Socket),
    ws_close(Socket),
    Result.

ws_append_messages(Messages, []) -> Messages;
ws_append_messages(Messages, NewMessages) ->
    NewMessagesStripped = [M || <<_, M/binary>> <- NewMessages],
    <<Messages/binary, (iolist_to_binary(NewMessagesStripped))/binary>>.

resource(Path, Args) ->
    lists:flatten(io_lib:format(Path, Args)).

url({Path, Args}, Query, Access) ->
    url(resource(Path, Args), Query, Access);

url(Resource, Query, Access) ->
    maps:get(server, Access, "") ++ Resource ++ url_query(Query).

url_query([]) -> "";
url_query(Query) ->
    [$?|lists:flatten(lists:join($&, lists:map(fun url_query_param/1, Query)))].

url_query_param({Key, Value}) ->
    http_uri:encode(underscore_atom_to_camel_case_string(Key)) ++
    [$=|http_uri:encode(to_string(Value))].

headers(Headers, Access) ->
    [{"Authorization", authorization(Access)}|Headers].

authorization(#{token := Token}) -> "Bearer " ++ Token;
authorization(#{username := UserName, password := Password}) ->
    base64:encode(UserName ++ [$:|Password]);
authorization(_Access) -> "".

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

underscore_atom_to_camel_case_string(Atom) when is_atom(Atom) ->
    case string:split(atom_to_list(Atom), "_", all) of
        [First] -> First;
        [First|Rest] -> lists:flatten([First|[[H-32|T] || [H|T] <- Rest]])
    end;
underscore_atom_to_camel_case_string(Term) -> Term.

to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Number) when is_number(Number) -> integer_to_list(Number);
to_string(String) when is_list(String) -> String.
