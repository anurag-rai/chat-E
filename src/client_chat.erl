-module(client_chat).
-behaviour(gen_server).

%% API.
-export([start/0]).
-export([start_session/0]).
-export([start_link/0]).
-export([login/0]).
-export([logout/0]).
-export([discovery/0]).
-export([getNameFromState/0]).
-export([message/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(SERVER, chatty).

%internal function
server_node() ->
    chatServer@GGN000414.

client_pid() ->
	whereis(?MODULE).

prettyPrintMsg(Name, Msg) ->
	io:format(" ]] ~p ---> ~p~n",[Name,Msg]).

%% API.
start() ->
	io:format(" >> Starting client services ... ~n"),
	start_link(),
	io:format(" >> Successfully registered client process ...~n~n"),
	case login() of
		success ->
			start_session();
		fail ->
			login()
	end.

start_session() ->
	io:format("~n===============================~n"),
	io:format("   Options:~n"),
	io:format("   1. Show online people~n"),
	io:format("   2. Start IM with a person~n"),
	io:format("   3. Logout"),
	io:format("~n===============================~n"),
	loop().

loop() ->
	Option = getOption(),
	case Option of
		1 ->
			discovery();
		2 ->
			messageSession();
		3 ->
			logout();
		_ ->
			io:format("Invalid Option ... GGWP")
	end,
	loop().


getOption() ->
	Option = string:to_integer(string:strip(io:get_line("Enter option --> "), right, $\n)),
	case Option of
		{Number, _} ->
			%io:format("~p~n",[Term]);
			Number;
		_ ->
			io:format("Unable to parse ...~n"),
			getOption()
	end.

login() ->
	UserName = string:strip(io:get_line("Login As: "), right, $\n),						% Get input from client
	 case gen_server:call(?MODULE, {login, UserName, client_pid()}) of
	 	{success, Msg} ->
	 		io:format("~p~n",[Msg]),
	 		success;
	 	{fail, Msg} ->
	 		io:format("~p~n",[Msg]),
	 		fail;
	 	{ _, Msg} ->
	 		io:format("~p~n",[Msg]),
	 		fail
	 end.

logout() ->
	Status = gen_server:call(?MODULE, {logout, getNameFromState()}),
	case Status of
		done ->
			io:format("~n~n~n  Successfully logged out~n~n~n"),
			login();
		_ ->
			io:format("~n Some error occured while logging out")
	end.

discovery() ->
	Reply = gen_server:call(?MODULE, discovery),
	Number = maps:size(Reply),
	io:format("There are ~p clients currently connected apart from you~n",[Number]),
	lists:foreach(fun({Key, _}) -> io:format("--> ~p~n", [Key]) end, maps:to_list(Reply)).

getNameFromState() ->
	gen_server:call(?MODULE, getName).

message(To, Message) ->
	io:format(" ]] You ---> ~p~n",[Message]),
	gen_server:cast({?SERVER,server_node()}, {message, To, Message}).


start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server.

init([]) ->
	{ok, []}.



handle_call(getName, _From, State) ->
	{reply, element(2, hd(State)), State};

handle_call({login, UserName, Pid}, _From, State) ->
	Reply = gen_server:call({?SERVER,server_node()}, {login, UserName, Pid}),		% Send login request to server
	case Reply of
		{loginSuccess, Message} ->
			{reply, {success, Message}, [{loggedin, UserName} | State]};
		{loginFail, Message} ->
			{reply, {fail, Message}, State};
		_ ->
			{reply, {unknown, "Unknown login reply"}, State}
	end;

handle_call({logout, UserName}, _From, State) ->
	Reply = gen_server:call({?SERVER,server_node()}, {logout, UserName}),
	case Reply of
		done ->
			{reply, Reply, []};
		_ ->
			{reply, Reply, State}
	end;
	
handle_call(discovery, _From, State) ->
	Reply = gen_server:call({?SERVER,server_node()}, discovery),
	NewReply = maps:remove(element(2,hd(State)), Reply),
	{reply, NewReply, State}.


handle_cast({message, From, Message}, State) ->
%	io:format(" ]] ~p says: ~p~n",[From,Message]),
	prettyPrintMsg(From,Message),
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
