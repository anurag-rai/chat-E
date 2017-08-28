-module(databaseConnection).

-export([init/0]).
-export([insert/3]).
-export([showConversations/0]).
-export([showChatIDs/0]).
-export([delete/0]).
-export([getPastChats/2]).

-include_lib("stdlib/include/qlc.hrl"). 

-record(chatID, {users={userOne,userTwo},id}).
-record(conversation, {id, chat = []}).

init() ->
	io:format("  >>  Creating schema on current node ...~n"),
	mnesia:create_schema([node()]),
	io:format("  >>  Starting database ...~n"),
	mnesia:start(),
	io:format("  >>  Creating tables ...~n"),
	case mnesia:create_table(chatID, 
				[ 	{disc_copies, [node()] },
             		{attributes, record_info(fields,chatID)},
             		{type,set}
             	]) of
		{aborted, {already_exists, Name1}} ->
			io:format("  >>  Table ~p already exists~n",[Name1]);
		{atomic, ok} ->
			ok;
		{aborted, Reason1} ->
			Reason1
	end,
	case mnesia:create_table(conversation, 
				[ 	{disc_copies, [node()] },
             		{attributes, record_info(fields,conversation)},
             		{type,set}
             	]) of
		{aborted, {already_exists, Name2}} ->
			io:format("  >>  Table ~p already exists~n",[Name2]);
		{atomic, ok} ->
			ok;
		{aborted, Reason2} ->
			Reason2
	end.

insert(To, From, Message) ->
	Tuple = case To < From of
				true ->
					{To,From};
				false ->
					{From,To}
			end,
	ChatID = case getChatID(Tuple) of
				{true, Id} ->
					Id;
				false ->
					Id = getNextid(),
					insertNewChat(Tuple, Id),
					Id;
				_ -> 
					io:format("Program Crashing!!")
			 end,
	insertConversation(ChatID, From, Message).
	
getPastChats(User1, User2) ->
	Tuple = case User1 < User2 of
				true ->
					{User1,User2};
				false ->
					{User2,User1}
			end,
	case getChatID(Tuple) of
		{true, Id} ->
				fetchConversation(Id);
		false ->
				[]
	end.

showChatIDs() -> 
    mnesia:transaction( 
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(chatID) ] 
        )) 
    end ).

showConversations() ->
    mnesia:transaction( 
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(conversation) ] 
        )) 
    end ).

delete() ->
	mnesia:delete_table(chatID),
	mnesia:delete_table(conversation).

getChatID(Tuple) ->
	%check if chat ID exists
	Fun = 	fun() ->
				mnesia:read(chatID, Tuple)
			end,
	ChatID =  case mnesia:transaction(Fun) of
			%ID doesn't exist for tuple ... create and assign ID to conversation
			{atomic, []} ->
				false;
			{atomic, [{_,{_,_},Id}]} ->
				{true,Id};
			_ ->
				io:format("  >>  Failed to fetch ID to conversation between ~p ...~n",[Tuple])
		end,
	ChatID.

insertNewChat(Tuple, Id) ->
	Fun = 	fun() -> mnesia:write(
				#chatID{ 	users = Tuple,
							id = Id}
					)
			end,
	mnesia:transaction(Fun).

insertConversation(ChatID, From, Message) ->
	%check if previous conversation exists
	Fun = 	fun() ->
				mnesia:read(conversation,ChatID)
			end,
	case mnesia:transaction(Fun) of
		{atomic, []} ->
			startNewConversation(ChatID, From, Message);
		{atomic, [{ _ , _ ,History}]} ->
			NewHistory = [{From,Message} | History],
			appendToConversation(ChatID, NewHistory);
		Else ->
			io:format("  >>  Cannot find converstaion with ID ~p. Problem with DB~n",[ChatID]),
			Else
	end.

startNewConversation(ChatID, From, Message) ->
	Fun = 	fun() -> mnesia:write(
				#conversation{ 	id = ChatID,
								chat = [{From,Message}]}
					)
			end,
	mnesia:transaction(Fun).

appendToConversation(ChatID, NewHistory) ->
	Fun = 	fun() -> 
				[R] = mnesia:wread({conversation, ChatID}),
				mnesia:write(R#conversation{chat=NewHistory})
			end,
	mnesia:transaction(Fun).

fetchConversation(ChatID) ->
	Fun = 	fun() ->
				mnesia:read(conversation,ChatID)
			end,
	case mnesia:transaction(Fun) of
			{atomic, []} ->
				[];
			{atomic, [{ _ , _ ,History}]} ->
				History;
			Else ->
				io:format("  >>  Cannot fetch converstaion with ID ~p. Problem with DB~n",[ChatID]),
				Else
	end.

getNextid() ->
	erlang:phash2({node(), erlang:timestamp()}).
