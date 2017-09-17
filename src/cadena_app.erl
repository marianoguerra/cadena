%%%-------------------------------------------------------------------
%% @doc cadena public API
%% @end
%%%-------------------------------------------------------------------

-module(cadena_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    setup_http_api(),
    cadena_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

setup_http_api() ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/keys/:ensemble/:key", cadena_h_keys, []}]}
  ]),

  HttpPort = application:get_env(cadena, http_port, 8080),
  HttpAcceptors = application:get_env(cadena, http_acceptors, 100),

  lager:info("Starting http listener on port ~p", [HttpPort]),

  cowboy:start_clear(cadena_http_listener,
    [{port, HttpPort}, {num_acceptors, HttpAcceptors}],
    #{env => #{dispatch => Dispatch}}).
