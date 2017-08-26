-module(server_chat).
-behaviour(gen_server).

%% API.
-export([start/0]).
-export([discovery/0]).
%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, chatty).


%% API.
start() ->
	{ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
	io:format(" >> Server initialized with PID: ~p~n", [Pid]).

discovery() ->
	State = gen_server:call(?SERVER,discovery),
	Number = maps:size(State),
	io:format("There are ~p clients currently connected~n",[Number]),
	lists:foreach(fun({Key, Value}) -> io:format("(~p,~p)~n", [Key, Value]) end, maps:to_list(State)).

%% gen_server.
%% State ---> map of {Username, PID}

init([]) ->
	io:format(" >> Initializing server .... "),
	{ok, #{}}.


%% Handle the login request of the client
%% If the username is not already registered as a client, reply with {loginSuccess, Message}
%% then change the state to include the new {login => PID}
%% else, reply with {loginFail, Message}
%%
handle_call({login, UserName, Pid}, _From, State) ->
	%Check if username is already in state (i.e. logged in)
	Already = maps:is_key(UserName, State),
	case Already of
		false -> 
			NewState = maps:put(UserName,Pid,State),
			{reply, {loginSuccess, "You are now logged in"}, NewState};
		_ ->
			{reply, {loginFail, "Already logged in"}, State}
	end;

handle_call({logout, UserName}, _From, State) ->
	{reply, done, maps:remove(UserName, State)};

handle_call(discovery, _From, State) ->
	{reply, State, State}.


handle_cast(discovery, State) ->
	Number = maps:size(State),
	io:format("There are ~p clients currently connected~n",[Number]),
	lists:foreach(fun({Key, Value}) -> io:format("(~p,~p)~n", [Key, Value]) end, maps:to_list(State)),
	{noreply, State};

handle_cast({message, To, From, Message}, State) ->
	Pid = maps:get(To, State),		%Handle error when user has gone offline but message for it arrives
	gen_server:cast(Pid, {message, From, Message}),
	{noreply, State}.


handle_info(_Info, State) ->
	io:format(" >> Unexpected message: ~p~n",[_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format(" >> Server is terminating: ~p~n",[_Reason]),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
