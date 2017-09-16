%%%-------------------------------------------------------------------
%% @doc cadena top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cadena_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    % get the configuration from sys.config
    DataRoot = application:get_env(riak_ensemble, data_root, "./data"),
    % create a unique path for each node to avoid clashes if running more
    % than one node in the same computer
    NodeDataDir = filename:join(DataRoot, atom_to_list(node())),

    Ensemble = {riak_ensemble_sup,
                {riak_ensemble_sup, start_link,
                 [NodeDataDir]},
                permanent, 20000, supervisor, [riak_ensemble_sup]},

    {ok, { {one_for_all, 0, 1}, [Ensemble]} }.

%%====================================================================
%% Internal functions
%%====================================================================
