-module(client_chat).

%% API.
-export([start/0]).
-export([client/3]).

-define(SERVER, server).
-define(CLIENT, client).

server_node() ->
    chatServer@GGN000414.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%      API		%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start() ->
	io:format("~n  >>> Starting client services ... ~n"),
	Pid = spawn_link(?MODULE, client, [getServerPid(), self(), []]),
	register(?CLIENT, Pid),
	process_flag(trap_exit, true),
	login(),
	welcomeMessage(),
	displayOptions(),
	loop().

client(ServerPid, User, ClientState) ->
	receive
		% CLIENT MESSAGES
		{login, UserName} -> 
			ServerPid ! {login, UserName, self()},
			client(ServerPid, User, [UserName]);
		discovery ->
			ServerPid ! {discovery, self()};
		{messageTo, To, Message} ->
			ServerPid ! {message, To, hd(ClientState), Message};
		{history, all, To} ->
			ServerPid ! {history, all, To, hd(ClientState), self()};
		logout ->
			ServerPid ! {logout, hd(ClientState)},
			client(ServerPid, User, []);
		terminate ->
			exit(normal);
		%SERVER MESSAGES
		{loginSuccess, UserName, Msg} ->
			User ! {loginDone,Msg},
			client(ServerPid, User, [UserName]);
		{loginFail, Msg} ->
			User ! {loginFail, Msg};
		{discovery, Record} ->
			User ! {done, Record, hd(ClientState)};
		{messageFrom, From, Message} ->
			prettyPrintMsg(From, Message);
		{messageError, To, Message} ->
			io:format("User went offline ... Could not send ~p to ~p~n", [Message,To]);
		_ ->
			io:format("GOT IT!")
	end,
	client(ServerPid, User, ClientState).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%	   INTERNAL FUNCTIONS		%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login() ->
	io:format("~n[Enter /quit to stop all services]~n"),
	UserName = string:strip(io:get_line("Login As: "), right, $\n),
	case UserName of
		"/quit" ->
			terminate();
		_ ->
			?CLIENT ! {login, UserName},
			receive
				{loginDone, Msg} ->
					io:format("~p~n",[Msg]);
				{loginFail, Msg} ->
					io:format("~p~n",[Msg]),
					login();
				_ ->
					io:format("Unknown value")
			after 5000 ->
				io:format("Server is not responding ... "),
				login()
			end
	end.

loop() ->
	Option = getOption(),
	case Option of
		1 -> displayOptions();
		2 -> discovery();
		3 -> messageSession();
		4 -> logout();
		_ -> io:format("Invalid Option ... GGWP")
	end,
	loop().

getOption() ->
	Option = string:to_integer(string:strip(io:get_line("Enter option number --> "), right, $\n)),
	case Option of
		{Number, _} ->
			%io:format("~p~n",[Term]);
			Number;
		_ ->
			io:format("Unable to parse ...~n"),
			getOption()
	end.

logout() ->	
	?CLIENT ! logout,
	io:format("~n~n~n  Successfully logged out~n~n~n"),
	login().

messageSession() ->
	OtherUser = string:strip(io:get_line("Whom do you want to chat with? : "), right, $\n),
	?CLIENT ! discovery,
	receive
		{done, Record, UserName} ->
			Others = maps:remove(UserName, Record),
			case maps:find(OtherUser,Others) of
				{ok, _ } ->
					io:format("~n"),
					io:format("===============================~n"),
					io:format("   Starting chat with ~p~n",[OtherUser]),
					io:format("   Type /history to show previous chats~n"),
					io:format("   Type /quit to exit chat IM~n"),
					io:format("===============================~n"),
					message(OtherUser);
				_ ->
					io:format("  >>> Other user either offline or not available~n")
			end;
		_ ->
			io:format("Message: Cannot find list of online users ... ~n"),
			loop()
	after 5000	->
		io:format("Message: Server unresponsive ... ~n"),
		loop()
	end.

message(To) ->
	Message = string:strip(io:get_line(" ]] You ---> "), right, $\n),
	case Message of
		"/quit" ->
			io:format("===============================~n"),
			io:format(" Stopping IM services .....~n"),
			io:format("===============================~n");
		"/history" ->
			getPastChats(To);
		_ ->
			?CLIENT ! {messageTo, To, Message},
			message(To)
	end.

discovery() ->
	?CLIENT ! discovery,
	receive
		{done, Record, UserName} ->
			Others = maps:remove(UserName, Record),
			displayDiscovery(Others);
		_ ->
			io:format("Discovery: Don't know what this is ... ~n")
	after 5000	->
		io:format("Discovery failed~n")
	end.

getPastChats(To) ->
	io:format("===============================~n"),
	?CLIENT ! {history, all, To}.

terminate() ->
	quitMessage(),
	?CLIENT ! terminate,
	receive
		{'EXIT', _Pid, normal} ->
			exit(normal);
		{'EXIT', _Pid, shutdown} ->
			exit(normal);
		{'EXIT', _Pid, _} ->
			io:format("Not able to kill client service"),
			exit(normal)
	after 5000 ->
			io:format("Tired of waiting ... killing self"),
			exit(normal)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%	   HELPER FUNCTIONS	    	%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getServerPid() -> {?SERVER, server_node()}.

welcomeMessage() ->
	io:format("~n~n"),
	io:format("==============================~n"),
	io:format("=========== WELCOME ==========~n"),
	io:format("==============================~n~n").

quitMessage() ->
	io:format("~n~n"),
	io:format("==============================~n"),
	io:format("=========== BYE BYE ==========~n"),
	io:format("==============================~n~n").

displayOptions() ->
	io:format("===============================~n"),
	io:format("   Options:~n"),
	io:format("   1. Display server options~n"),
	io:format("   2. Show online people~n"),
	io:format("   3. Start IM with a person~n"),
	io:format("   4. Logout~n"),
	io:format("===============================~n").

prettyPrintMsg(Name, Msg) -> io:format(" ]] ~p ---> ~p~n",[Name,Msg]).

displayDiscovery(Record) ->
	Number = maps:size(Record),
	io:format("~nThere are ~p clients currently connected apart from you~n",[Number]),
	lists:foreach(fun({Key, _}) -> io:format("--> ~p~n", [Key]) end, maps:to_list(Record)).