-module(test).

-export([doThis/1]).

doThis(A) ->
	L = [{"A", "hey"}],
	print([A|L]).

print(L) ->
	L.