-module(root_sup).
-behaviour(supervisor).

-export([start/0]).
-export([init/1, shutdown/0]).

start() ->
     supervisor:start_link({local,?MODULE}, ?MODULE, []).
     %lists:flatten(" ]] Supervisor initialized with PID: " ++ pid_to_list(ServerPid)).
     

init(_Args) ->
     RestartStrategy = {one_for_one, 1, 5},
     GenServer = {gen_server,
          {server_chat, startGenServer, []},
          permanent, 2000, worker, [server_chat]},

     ServerProcess = {server_process,
          {server_chat, startServerProcess, []},
          permanent, 2000, worker, [server_chat]},


     Children = [GenServer, ServerProcess],
     {ok, {RestartStrategy, Children}}.    

% supervisor can be shutdown by calling exit(SupPid,shutdown)
% or, if it's linked to its parent, by parent calling exit/1.
shutdown() ->
     exit(whereis(?MODULE), shutdown).
     % or
     % exit(normal).