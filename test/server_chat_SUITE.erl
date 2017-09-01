-module(server_chat_SUITE).

-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CALLBACKS %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [blackBox_testLogin, blackBox_testDiscovery, blackBox_testLogout, whiteBox_testLogin, whiteBox_testDiscovery, whiteBox_testLogout].

init_per_suite(Config) ->
	{ok, Pid} = server_chat:startGenServer(),	
	{ok, State} = server_chat:init([]),
    [{server, {Pid,State}} | Config].
    
end_per_suite(Config) ->	Config.

init_per_testcase(_TestCase, Config) ->	
	UserOne_name = "User One",
	UserOne_Pid = getRandomPid(),
	UserTwo_name = "User Two",
	UserTwo_Pid = getRandomPid(),
	[ {userOne, {UserOne_name, UserOne_Pid}} ] ++ [{userTwo, {UserTwo_name, UserTwo_Pid}}] ++ Config.

end_per_testcase(_TestCase, Config) ->	Config.

%%%%%%%%%%%%%%%%
%% TEST CASES %%
%%%%%%%%%%%%%%%%
blackBox_testLogin(Config) -> 

	{ok, _Pid} = server_chat:startGenServer(),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	%% Check login of first user
	{loginSuccess, UserOne_name, "You are now logged in"} = 
		server_chat:login(UserOne_name, UserOne_Pid),

	%% Check if same user can login again
	{loginFail, "Already logged in"} = 
		server_chat:login(UserOne_name, UserOne_Pid),

	%% Check login of second user
	{loginSuccess, UserTwo_name, "You are now logged in"} = 
		server_chat:login(UserTwo_name, UserTwo_Pid).

blackBox_testDiscovery(Config) -> 
	{ok, _Pid} = server_chat:startGenServer(),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	FinalMap = #{ UserOne_name => UserOne_Pid, UserTwo_name => UserTwo_Pid},

	{discovery, State1} = server_chat:discovery(),
	0 = maps:size(State1),

	%% Insert user and test discovery
	server_chat:login(UserOne_name, UserOne_Pid),
	{discovery, State2} = server_chat:discovery(),
	1 = maps:size(State2),
	UserOne_Pid = maps:get(UserOne_name, State2),

	%% Insert second user and test discovery
	server_chat:login(UserTwo_name, UserTwo_Pid),
	{discovery, State3} = server_chat:discovery(),
	2 = maps:size(State3),
	FinalMap = State3,

	%% logout A
	server_chat:logout(UserOne_name),
	{discovery, State4} = server_chat:discovery(),
	1 = maps:size(State4),
	UserTwo_Pid = maps:get(UserTwo_name, State4).

blackBox_testLogout(Config) ->
	{ok, _Pid} = server_chat:startGenServer(),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	server_chat:login(UserOne_name, UserOne_Pid),
	server_chat:login(UserTwo_name, UserTwo_Pid),

	{discovery, State1} = server_chat:discovery(),
	2 = maps:size(State1),

	%logout second user
	server_chat:logout(UserTwo_name),
	{discovery, State2} = server_chat:discovery(),
	1 = maps:size(State2),

	%logout first user
	server_chat:logout(UserOne_name),
	{discovery, State3} = server_chat:discovery(),
	0 = maps:size(State3),

	%% Try to logout first user again
	server_chat:logout(UserOne_name),
	{discovery, State3} = server_chat:discovery().

whiteBox_testLogin(Config) -> 
	{_Pid, State} = ?config(server,Config),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	%% Check number of entries in map
	0 = maps:size(State),
	
	%% Check login of first user
	{reply, {loginSuccess, UserOne_name, "You are now logged in"}, NewState1} = 
		server_chat:handle_call({login, UserOne_name, UserOne_Pid}, dummy_from, State),

	1 = maps:size(NewState1),
	UserOne_Pid = maps:get(UserOne_name, NewState1),

	%% Check if same user can login again
	{reply, {loginFail, "Already logged in"}, NewState1} = 
		server_chat:handle_call({login, UserOne_name, UserOne_Pid}, dummy_from, NewState1),

	1 = maps:size(NewState1),
	UserOne_Pid = maps:get(UserOne_name, NewState1),

	%% Check login of second user
	{reply, {loginSuccess, UserTwo_name, "You are now logged in"}, NewState2} = 
		server_chat:handle_call({login, UserTwo_name, UserTwo_Pid}, dummy_from, NewState1),

	2 = maps:size(NewState2),
	UserOne_Pid = maps:get(UserOne_name, NewState2),
	UserTwo_Pid = maps:get(UserTwo_name, NewState2).

whiteBox_testDiscovery(Config) -> 
	{_Pid, State} = ?config(server,Config),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	0 = maps:size(State),	

	FinalMap = #{ UserOne_name => UserOne_Pid, UserTwo_name => UserTwo_Pid},

	{reply, {discovery, State}, State} = server_chat:handle_call(discovery, dummy_from, State),

	%% Insert user and test discovery
	{reply, {loginSuccess, UserOne_name, "You are now logged in"}, NewState1} = 
		server_chat:handle_call({login, UserOne_name, UserOne_Pid}, dummy_from, State),
	{reply, {discovery, NewState1}, NewState1} = server_chat:handle_call(discovery, dummy_from, NewState1),

	%% Insert second user and test discovery
	{reply, {loginSuccess, UserTwo_name, "You are now logged in"}, NewState2} = 
		server_chat:handle_call({login, UserTwo_name, UserTwo_Pid}, dummy_from, NewState1),
	{reply, {discovery, FinalMap}, FinalMap} = server_chat:handle_call(discovery, dummy_from, NewState2),

	%% logout A
	{noreply, NewState3} = server_chat:handle_cast({logout, UserTwo_name}, NewState2),
	{reply, {discovery, NewState1}, NewState1} = server_chat:handle_call(discovery, dummy_from, NewState3).

whiteBox_testLogout(Config) ->
	{_Pid, State} = ?config(server,Config),
	{UserOne_name, UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, UserTwo_Pid} = ?config(userTwo,Config),

	0 = maps:size(State),

	%% Insert first user
	{reply, {loginSuccess, UserOne_name, "You are now logged in"}, NewState1} = 
		server_chat:handle_call({login, UserOne_name, UserOne_Pid}, dummy_from, State),

	%% Insert second user
	{reply, {loginSuccess, UserTwo_name, "You are now logged in"}, NewState2} = 
		server_chat:handle_call({login, UserTwo_name, UserTwo_Pid}, dummy_from, NewState1),


	%logout second user
	{noreply, NewState1} = server_chat:handle_cast({logout, UserTwo_name}, NewState2),
	%logout first user
	{noreply, State} = server_chat:handle_cast({logout, UserOne_name}, NewState1),
	%% Try to logout first user again
	{noreply, State} = server_chat:handle_cast({logout, UserOne_name}, NewState1).


getRandomPid() ->
	erlang:phash2({node(), erlang:timestamp()}).