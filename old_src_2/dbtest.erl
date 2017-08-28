-module(dbtest).

-export([init/0,insert/3,select_all/0,select/2,delete/0]).
-include_lib("stdlib/include/qlc.hrl"). 

-record(converstaion, {users={userOne,userTwo},past}).

init() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	case mnesia:create_table(converstaion, 
				[ 	{disc_copies, [node()] },
             		{attributes, record_info(fields,converstaion)},
             		{type,set}
             	]) of
		{aborted, {already_exists, Name}} ->
			io:format("Table ~p already exists~n",[Name]);
		{atomic, ok} ->
			done;
		{aborted, Reason} ->
			Reason
	end.

insert(UserOne, UserTwo, Past) ->
	Tuple = {UserOne,UserTwo},
	Fun = 	fun() ->
				mnesia:write(
					#converstaion{ 	users = Tuple,
									past = Past}
				)
			end,
	mnesia:transaction(Fun).

select(UserOne, UserTwo) ->
	Fun = fun() ->
			mnesia:read(converstaion, {UserOne,UserTwo})
			end,
	mnesia:transaction(Fun).

select_all() -> 
    mnesia:transaction( 
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(converstaion) ] 
        )) 
    end ).

delete() ->
	mnesia:delete_table(converstaion).