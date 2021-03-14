
-module(mobius).

-export([
	is_prime/1,
	prime_factors/1,
	is_square_multiple/1,
	find_square_multiples/2,
	timer_exec/2
]).


is_prime(2) ->
	true;
is_prime(N) when N > 2 ->
	is_prime_help(N, round(math:sqrt(N)), false).

is_prime_help(_, 1, Res) ->
	Res;
is_prime_help(N, Threshold, _) ->
	if ((N rem Threshold) == 0) ->
		is_prime_help(N, 1, false);
	true ->
		is_prime_help(N, Threshold - 1, true)
end.

prime_factors(N, M, L) when N == M -> 
	[M|L];
prime_factors(N, M, L) when N < M -> 
	L;
prime_factors(N, M, L) when N rem M == 0 ->
	prime_factors(N div M, M, [M|L]);
prime_factors(N, M, L) -> 
	prime_factors(N, M+1, L).

prime_factors(N) -> lists:reverse(prime_factors(N, 2, [])).

is_square_multiple(N) ->
	Factors = prime_factors(N),
	UniqueFactors = sets:from_list(Factors),
	length(Factors) > sets:size(UniqueFactors).
  
find_sublist_of_length(_, _, []) -> [];
find_sublist_of_length(Pred, Len, [H | T]) ->
	Sub = lists:sublist([H | T], Len),
	LenSub = length(Sub),
	Passed = lists:all(Pred, Sub),
	if LenSub < Len -> fail;
	  Passed -> Sub;
	  true -> find_sublist_of_length(Pred, Len, T)
	end.
  
find_square_multiples(Count, MaxN) ->
	Checked = [
	  {X, is_square_multiple(X)} || X <- lists:seq(2, MaxN + 2)
	],
	Triplet = find_sublist_of_length(fun({_, Check}) -> Check end, Count, Checked),
	case Triplet of
	  fail -> fail;
	  [{E, _} | _] -> E
	end.

timer_exec(Count, MaxN) ->

    Start = os:timestamp(),

    Result = find_square_multiples(Count, MaxN),
    
    io:format("Exec time: ~p s~n", [timer:now_diff(os:timestamp(), Start) / 1000000]),
    io:format("Result: ~p~n", [Result]).