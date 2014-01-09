-module(estatist_graphite).
-behaviour(application).
-behaviour(supervisor).

%% application callbacks
-export([start/2, stop/1]).

%% supervisor callbacks
-export([init/1]).


%% application callbacks
start(normal, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.


%% supervisor callbacks
%% Helper macro for declaring children of supervisor
-define(CHILD(Mod, Args), {Mod, {Mod, start_link, Args}, permanent, 5000, worker, [Mod]}).
init([]) ->
    Env = application:get_all_env(),
    {ok, {{one_for_one, 5, 10}, [
        ?CHILD(estatist_graphite_server, [Env])
    ]}}.
