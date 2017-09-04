-module(databaseConnection_SUITE).

-compile(export_all).

% required for common_test to work
-include_lib("common_test/include/ct.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% CALLBACKS %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Specify a list of all unit test functions
all() -> [	blackBox_testPastChats
			
			].

init_per_suite(Config) ->
	Config.
    
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
blackBox_testPastChats(Config) -> 
	databaseConnection:init(),
	databaseConnection:delete(),
	databaseConnection:init(),

	{UserOne_name, _UserOne_Pid} = ?config(userOne,Config),
	{UserTwo_name, _UserTwo_Pid} = ?config(userTwo,Config),

	[] = databaseConnection:getPastChats(UserOne_name, UserTwo_name),

	databaseConnection:insert(UserTwo_name, UserOne_name, "hi"),

	[{UserOne_name,"hi"}] = databaseConnection:getPastChats(UserOne_name, UserTwo_name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HELPER FUNCTIONS %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getRandomPid() ->
	erlang:phash2({node(), erlang:timestamp()}).