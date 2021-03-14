
-module(fib).

-export([
    fib/1, 
    fib_p/1, 
    fib_g/1,
    fib_tail/1,
    timer_exec/2
]).

fib(0) ->
    0;
fib(1) ->
    1;
fib(N) ->
    % io:format("fib exec ...~n");
    fib(N - 1) + fib(N - 2).

fib_p(X) ->
    if X == 0 ->
        0; 
    X == 1 -> 
        1; 
    true ->
        % io:format("fib_p exec ...~n"), 
        fib_p(X-1) + fib_p(X-2)
    end.

fib_g(X) when X == 0 ->
    0;
fib_g(X) when X == 1 -> 
    1;
fib_g(X) ->
    % io:format("fib_g exec ...~n"), 
    fib_g(X-1) + fib_g(X-2).

fib_tail(X) ->
    % io:format("fib_tail exec ...~n"), 
    fib_help(X, 0, 1).

fib_help(0, N1, _) ->
    N1;
fib_help(C, N1, N2) ->
    fib_help(C-1, N1+N2, N1).

timer_exec(Function, Argument) ->

    Start = os:timestamp(),

    io:format("Function: ~p, Argument:~p~n", [Function, Argument]),
    if Function == 'fib' ->
        Result = fib(Argument);
    Function == 'fib_p' ->
        Result = fib_p(Argument);
    Function == 'fib_g' ->
        Result = fib_g(Argument);
    true ->
        Result = fib_tail(Argument)
    end,
    
    io:format("Exec time: ~p s~n", [timer:now_diff(os:timestamp(), Start) / 1000000]),
    io:format("Result: ~p~n", [Result]).