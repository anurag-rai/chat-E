-module(server_chat).
-behaviour(gen_server).

%% API.
-export([startGenServer/0]).
-export([startServerProcess/0]).
-export([server/0]).

-export([login/2]).
-export([logout/1]).
-export([discovery/0]).
-export([showClients/0]).
-export([message/3]).

-export([showChatLog/0]).
-export([deleteChatLogs/0]).
-export([restartDatabase/0]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(GENSERVER, gen_chatty).
-define(SERVER, server).

startGenServer() ->
	welcomeMessage(),
	ServerPid = start_link(),
	startDatabase(),
	io:format(" ]] Gen-server initialized with PID: ~p~n",[ServerPid]),
	{ok, ServerPid}.

startServerProcess() -> 
	Pid = spawn_link(?MODULE, server, []),
	register(?SERVER, Pid),
	io:format(" ]] Server-process initialized with PID: ~p~n",[Pid]),
	{ok, Pid}.

server() ->
	receive
		{login, UserName, Pid} ->
			Pid ! login(UserName,Pid);
		{discovery, Pid} ->
			Pid ! discovery();
		{logout, UserName} ->
			logout(UserName);
		{message, To, From, Message} ->
			message(To, From, Message);
		{history, Number, To, From, Pid} ->
			Pid ! getPastChats(To, From, Number);
		{crash, X} ->
			A = 5/X,
			io:format("~p~n",[A]);
		_ ->
			io:format("Something else")
	end,
	server().

showClients() ->
	{ _ , Clients } = discovery(),
	Number = maps:size(Clients),
	io:format("~nThere are ~p clients currently connected~n",[Number]),
	lists:foreach(fun({Key, _}) -> io:format("--> ~p~n", [Key]) end, maps:to_list(Clients)).

showChatLog() ->
	databaseConnection:showConversations().

login(UserName,Address) ->
	gen_server:call(?GENSERVER, {login, UserName, Address}).

discovery() ->
	gen_server:call(?GENSERVER, discovery).

logout(UserName) ->
	gen_server:cast(?GENSERVER, {logout, UserName}).

message(To, From, Message) ->
	logMessage(To, From, Message),
	gen_server:cast(?GENSERVER, {message, To, From, Message}).

start_link() ->
	{ok, Pid} = gen_server:start_link({local, ?GENSERVER}, ?MODULE, [], []),
	Pid.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%	   GEN SERVER		%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% State ---> map of {Username, PID}

init([]) ->
	io:format(" ]] Initializing server .... ~n"),
	{ok, #{}}.

handle_call({login, UserName, Pid}, _From, State) ->
	%Check if username is already in state (i.e. logged in)
	Already = maps:is_key(UserName, State),
	case Already of
		false -> 
			NewState = maps:put(UserName,Pid,State),
			{reply, {loginSuccess, UserName, "You are now logged in"}, NewState};
		_ ->
			{reply, {loginFail, "Already logged in"}, State}
	end;

handle_call(discovery, _From, State) ->
	{reply, {discovery, State}, State};

handle_call({crash,X}, _From, State) ->
	{reply, 5/X, State}.

handle_cast({logout, UserName}, State) ->
	case maps:is_key(UserName, State) of
		true ->
			{noreply, maps:remove(UserName, State)};
		_ ->
			{noreply, State}
	end;
	
handle_cast({message, To, From, Message}, State) ->
	case maps:find(To, State) of
		{ok, PidTo} ->
			PidTo ! {messageFrom, From, Message};
		_ ->
			case maps:find(From, State) of
				{ok, PidFrom} ->
					PidFrom ! {messageError, To, Message}
			end
	end,
	{noreply, State}.


handle_info(_Info, State) ->
	io:format(" >> Unexpected message: ~p~n",[_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format(" >> Server is terminating: ~p~n",[_Reason]),
	ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%	   DATABASE FUNCTIONS		%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

startDatabase() ->
	case databaseConnection:init() of
		ok ->
			io:format("  >>  Database is up and running!~n");
		Reason ->
			io:format("  >>  Database had a problem: ~p~n",[Reason])
	end.

restartDatabase() -> startDatabase().

logMessage(To, From, Message) ->
	databaseConnection:insert(To, From, Message).

deleteChatLogs() ->
	io:format("  >>  DELETING ALL CHAT LOGS ...~n"),
	databaseConnection:delete(),
	startDatabase().

getPastChats(User1, User2, Number) ->
	PastChats = databaseConnection:getPastChats(User1, User2),
	case Number == all of 
		true ->
			{history, PastChats};
		_ ->
			{history, lists:sublist(PastChats,Number)}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%	   INTERNAL FUNCTIONS		%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

welcomeMessage() ->
	io:format("~n"),
	io:format("  __  _ _   _  ___   __  ___  ___ _ _  ___  ___ ~n"),
	io:format(" / _|| U | / \\|_ _| / _|| __|| o \\ | || __|| o \\~n"),
	io:format("( (_ |   || o || |  \\_ \\| _| |   / V || _| |   /~n"),
	io:format(" \\__||_n_||_n_||_|  |__/|___||_|\\\\\\_/ |___||_|\\\\~n"),
	io:format("~n").
                                                