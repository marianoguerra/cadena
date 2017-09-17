-module(cadena_h_keys).
-export([init/2]).

init(ReqIn=#{method := <<"POST">>}, _State) ->
    % get fields from the url
    {Ensemble, Key} = get_bindings(ReqIn),
    Timeout = 5000,
    {ok, Value, ReqOut} = parse_body(ReqIn),
    Res = riak_ensemble_client:kover(Ensemble, Key, Value, Timeout),
    reply(ReqOut, Res);

init(ReqIn=#{method := <<"GET">>}, _State) ->
    % get fields from the url
    {Ensemble, Key} = get_bindings(ReqIn),
    Timeout = 5000,
    Res = riak_ensemble_client:kget(Ensemble, Key, Timeout),
    reply(ReqIn, Res);

init(ReqIn=#{method := <<"DELETE">>}, _State) ->
    % get fields from the url
    {Ensemble, Key} = get_bindings(ReqIn),
    Timeout = 5000,
    Res = riak_ensemble_client:kdelete(Ensemble, Key, Timeout),
    reply(ReqIn, Res);

init(ReqIn, _State) ->
    json_reply(ReqIn, 405, #{error => true, code => invalid_method,
                             reason => <<"Invalid Method">>}).

%% private functions

get_bindings(ReqIn) ->
    EnsembleStr = cowboy_req:binding(ensemble, ReqIn),
	Ensemble = list_to_existing_atom(binary_to_list(EnsembleStr)),
	% for now we only allow the root ensemble
	root = Ensemble,
    Key = cowboy_req:binding(key, ReqIn),
    {Ensemble, Key}.

reply(ReqIn, {ok, {obj, Epoch, Seq, Key, notfound}}) ->
    Data = #{epoch => Epoch, seq => Seq, key => Key, value => null},
    json_reply(ReqIn, 200, #{ok => true, data => Data});

reply(ReqIn, {ok, {obj, Epoch, Seq, Key, Value}}) ->
    Data = #{epoch => Epoch, seq => Seq, key => Key, value => Value},
    json_reply(ReqIn, 200, #{ok => true, data => Data});


reply(ReqIn, {error, Reason}) ->
    json_reply(ReqIn, 500, #{error => true, code => Reason}).

json_reply(ReqIn, Status, Body) ->
    ReqOut = cowboy_req:reply(Status,
							  #{<<"content-type">> => <<"application/json">>},
							  jsone:encode(Body), ReqIn),
	{ok, ReqOut, no_state}.


parse_body(ReqIn) ->
    % NOTE: doesn't handle large bodies that return more instead of ok
    {ok, RawData, ReqOut} = cowboy_req:read_body(ReqIn),
    Data = jsone:decode(RawData),
    {ok, Data, ReqOut}.
