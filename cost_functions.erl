-module(cost_functions).
-export([sum_of_squares/1, rastrigin_function/1]).

sum_of_squares(ListOfValues) ->
    SquareAll = [Var * Var || Var <- ListOfValues],
    SumOfSquares = lists:sum(SquareAll),
    SumOfSquares.

rastrigin_function(ListOfValues) ->
    A = 10,
    N = num(ListOfValues),
    A_times_N = A * N,
    RastriginList = [math:pow(X, 2) - (A * math:cos(2 * math:pi() * X))
		     || X <- ListOfValues],
    RastriginSum = lists:sum(RastriginList),
    A_times_N + RastriginSum.

    
num([]) ->
    0;
num(NUMS) ->
    num(NUMS, 0).

num([H|L], Count) -> 
    num(L, Count+1);

num([], Count) ->
    Count.
