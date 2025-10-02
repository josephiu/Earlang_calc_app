-module(calc).
-export([start/0]).

start() ->
    io:format("~n---- Erlang Interactive Calculator ----~n", []),
    loop().

%% Main loop: gets input, calculates, prints result, asks to continue
loop() ->
    First = get_number("Enter the first number: "),
    Op = get_operation("Enter operation (+, -, *, /): "),
    Second = get_number("Enter the second number: "),

    case safe_calculate(First, Op, Second) of
        {ok, Result} ->
            io:format("Result: ~p ~s ~p = ~p~n", [First, Op, Second, Result]);
        {error, Reason} ->
            io:format("Error: ~s~n", [Reason])
    end,

    Continue = string:lowercase(get_input("Do you want to calculate again? (y/n): ")),
    case Continue of
        "y" -> loop();
        "n" -> io:format("Goodbye!~n", []);
        _   -> io:format("Invalid input. Exiting.~n", [])
    end.

%% Safe arithmetic with error handling
safe_calculate(_, "/", 0) ->
    {error, "Division by zero"};
safe_calculate(X, "+", Y) -> {ok, X + Y};
safe_calculate(X, "-", Y) -> {ok, X - Y};
safe_calculate(X, "*", Y) -> {ok, X * Y};
safe_calculate(X, "/", Y) -> {ok, X / Y};
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
                    io:format("Invalid number. Try again.~n", []),
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
            io:format("Invalid operator. Try again.~n", []),
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
