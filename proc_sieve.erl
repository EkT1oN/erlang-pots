-module(proc_sieve).

-export([
	generate/1,
	gen_print/1,
	sieve/0,
	timer_exec/1
]).

sieve() ->
  	receive
    	{send, N} ->
      	sieve_leaf_node(N)
  	end.

sieve_leaf_node(Divisor) ->
  	receive
    	{send, N} ->
      	if N rem Divisor == 0 ->
          	sieve_leaf_node(Divisor);
        true ->
          	ChildPID = spawn(proc_sieve, sieve, []),
          	ChildPID ! {send, N},
          	sieve_parent_node(Divisor, ChildPID)
      	end;
    	{done, ReqPID} ->
      		ReqPID ! [Divisor]
  		end.

sieve_parent_node(Divisor, ChildPID) ->
  	receive
    	{send, N} ->
      	if
        	N rem Divisor == 0 -> ok;
        	true -> ChildPID ! {send, N}
      	end,
      	sieve_parent_node(Divisor, ChildPID);
    	{done, ReqPID} ->
      	ChildPID ! {done, self()},
      	receive
        	Primes ->
          		ReqPID ! [Divisor | Primes]
      	end
  	end.

send_seq(Current, End, PID) when Current > End ->
  	PID ! {done, self()};
send_seq(Current, End, PID) ->
  	PID ! {send, Current},
  	send_seq(Current + 1, End, PID).

generate(MaxN) ->
  	ChildPID = spawn(proc_sieve, sieve, []),
  	send_seq(2, MaxN, ChildPID),
  	receive
    	Primes -> Primes
  	end.

gen_print(MaxN) ->
  	Primes = generate(MaxN),
  	lists:foreach(fun(E) -> io:format("~w~n", [E]) end, Primes).

timer_exec(MaxN) ->
    Start = os:timestamp(),
    Result = generate(MaxN),    
    io:format("Result: ~p~n", [Result]),
	io:format("Exec time: ~p s~n", [timer:now_diff(os:timestamp(), Start) / 1000000]).
