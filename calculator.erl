%% calculator.erl
-module(calculator).
-export([calculate/1]).

%% Public function to evaluate a string expression
calculate(Str) ->
    {Result, _Rest} = eval_expr(strip_spaces(Str)),
    Result.

%% Remove all whitespace characters from the string
strip_spaces(Str) ->
    [C || C <- Str, not lists:member(C, " \t\n\r")].

%% Evaluate the top-level expression
eval_expr(Str) ->
    eval(Str, 0, 1).

%% Recursive expression evaluator
eval([], Acc, _Sign) ->
    {Acc, []};

eval([C | Rest], Acc, Sign) when C >= $0, C =< $9 ->
    {Num, Rem} = read_number([C | Rest]),
    eval(Rem, Acc + Sign * Num, 1);

eval([$+ | Rest], Acc, _Sign) ->
    eval(Rest, Acc, 1);

eval([$- | Rest], Acc, _Sign) ->
    eval(Rest, Acc, -1);

eval([$\( | Rest], Acc, Sign) ->
    {Val, Rem} = eval_expr(Rest),
    eval(Rem, Acc + Sign * Val, 1);

eval([$) | Rest], Acc, _Sign) ->
    {Acc, Rest};

%% Fallback for unknown characters (skipped)
eval([_ | Rest], Acc, Sign) ->
    eval(Rest, Acc, Sign).

%% Read a number (handles multi-digit integers)
read_number(Str) ->
    read_number(Str, 0).

read_number([], Acc) ->
    {Acc, []};

read_number([C | Rest], Acc) when C >= $0, C =< $9 ->
    Digit = C - $0,
    read_number(Rest, Acc * 10 + Digit);

read_number(Rest, Acc) ->
    {Acc, Rest}.
