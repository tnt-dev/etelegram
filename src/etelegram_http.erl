-module(etelegram_http).

-export([request/3]).

-define(BASE_URL, <<"https://api.telegram.org/bot">>).
-define(TIMEOUT, 5000).

request(Method, RecType, Params0) ->
    Params = jsonify_param(reply_markup, Params0),
    URL = make_url(Method),
    FileField = case method_send_data(Method) of
                    {ok, Field} -> Field;
                    false       -> undefined
                end,
    Result = case FileField /= undefined andalso
                 etelegram_utils:is_file_exists(
                   proplists:get_value(FileField, Params)) of
                 true  -> do_request(URL, Params, FileField);
                 false -> do_request(URL, Params)
             end,
    unwrap_result(Result, RecType).

% Internal

do_request(Url, Params) ->
    do_request1(Url, {form, prepare_form_data(Params)}).

do_request(Url, Params0, FileField) ->
    {_, {FileField, FilePath}, Params} = lists:keytake(FileField, 1, Params0),
    PreparedParams = prepare_form_data(Params),
    do_request1(Url, {multipart,
                      [{file, FilePath,
                        {<<"form-data">>,
                         [{<<"name">>, etelegram_utils:to_binary(FileField)},
                          {<<"filename">>, FilePath}]}, []}
                       | PreparedParams]}).

do_request1(Url, Data) ->
    ReqOpts = [{connect_timeout, ?TIMEOUT},
               {recv_timeout, ?TIMEOUT},
               {with_body, true}],
    hackney:request(post, Url, [], Data, ReqOpts).

token() ->
    {ok, Token} = application:get_env(etelegram, token),
    Token.

make_url(Method) ->
    lists:flatten(io_lib:format("~s~s/~s", [?BASE_URL, token(), Method])).

unwrap_result({ok, 200, _, Body}, RecType) ->
    case etelegram_json:decode(Body) of
        [{ok, false} | _]=Result ->
            {error, etelegram_records:parse(etelegram_error, Result)};
        [{ok, true}, {result, Result}] ->
            case RecType of
                undefined -> ok;
                _ ->
                    Records = [etelegram_records:parse(RecType, R)
                               || R <- Result],
                    {ok, Records}
            end;
        [{ok, true}, {result, true}] ->
            {ok, true}
    end;
unwrap_result({ok, _, _, Body}, _) ->
    try
        Error = etelegram_json:decode(Body),
        {error, etelegram_records:parse(etelegram_error, Error)}
    catch error:badarg -> {error, {bad_response, Body}}
    end;
unwrap_result({error, Reason}, _) ->
    {error, {connection_error, Reason}}.

prepare_form_data(Data) ->
    [{etelegram_utils:to_binary(K),
      etelegram_utils:to_binary(V)} || {K, V} <- Data].

method_send_data(<<"send", DataField/binary>>) ->
    case lists:member(DataField, [<<"Photo">>,
                                  <<"Audio">>,
                                  <<"Document">>,
                                  <<"Sticker">>,
                                  <<"Video">>,
                                  <<"Voice">>]) of
        true  -> {ok, list_to_atom(string:to_lower(binary_to_list(DataField)))};
        false -> false
    end;
method_send_data(_) -> false.

jsonify_param(K, Params) ->
    case lists:keysearch(K, 1, Params) of
        {value, {_, Value}} ->
            EncodedValue = etelegram_json:encode(
                             etelegram_records:to_json(Value)),
            lists:keyreplace(K, 1, Params, {K, EncodedValue});
        false -> Params
    end.
