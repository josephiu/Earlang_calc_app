-module(calc).
-export([start/0]).

start() ->
    io:format("~n---- Welcome to Joseph's Interactive Calculator from Erlang ----~n", []),
    loop().

%% Main loop: gets input, calculates, prints result, asks to continue
loop() ->
    loop([]).

loop(Results) ->
    First = get_number("Enter the first number: "),
    Op = get_operation("Enter operation (+, -, *, /): "),
    Second = get_number("Enter the second number: "),

    case safe_calculate(First, Op, Second) of
        {ok, Result} ->
            io:format("Result: ~p ~s ~p = ~p~n", [First, Op, Second, Result]),
            NewResults = [Result | Results];
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason]),
            NewResults = Results
    end,

    Continue = string:lowercase(get_input("Do you want to calculate again? (y/n): ")),
    case Continue of
        "y" -> loop(NewResults);
        "n" ->
            io:format("Goodbye!~n", []),
            print_results(NewResults);
        _ ->
            io:format("Invalid input. Exiting.~n", []),
            print_results(NewResults)
    end.


%% Safe arithmetic with error handling (using Guard)
safe_calculate(_, "/", Y) when Y =:= 0 ->
    {error, "Division by zero"};
safe_calculate(X, "+", Y) -> {ok, X + Y};
safe_calculate(X, "-", Y) -> {ok, X - Y};
safe_calculate(X, "*", Y) -> {ok, X * Y};
safe_calculate(X, "/", Y) when Y =/= 0 -> {ok, X / Y};
safe_calculate(_, Op, _) ->
    {error, io_lib:format("Invalid operation: ~s", [Op])}.

%% Get a valid number from user input
get_number(Prompt) ->
    Input = get_input(Prompt),
    case string:to_float(Input) of
        {Float, ""} -> Float;
        _ ->
            case string:to_integer(Input) of
                {Int, ""} -> float(Int);
                _ ->
                    io:format("Invalid number. Please Try again.~n", []),
                    get_number(Prompt)
            end
    end.


%% Get a valid arithmetic operator
get_operation(Prompt) ->
    Input = string:trim(get_input(Prompt)),
    case Input of
        "+" -> "+";
        "-" -> "-";
        "*" -> "*";
        "/" -> "/";
        _   ->
            io:format("Invalid operator. Please Try again.~n", []),
            get_operation(Prompt)
    end.

%% Prompt and read trimmed input
get_input(Prompt) ->
    io:format("~s", [Prompt]),
    case io:get_line("") of
        eof -> "n";
        {error, _} -> "n";
        Line -> string:trim(Line)
    end.

% Print all stored results using lists:map and lambda
print_results(Results) ->
    lists:map(
        fun(Result) ->
            io:format("results Stored : ~p~n", [Result])
        end,
        lists:reverse(Results) %restoring the original order of the results.
    ). 
