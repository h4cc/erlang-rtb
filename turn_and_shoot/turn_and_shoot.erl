-module(turn_and_shoot).

-export([run/0]).

% Start like this:
% $ erlc stdin.erl 
% $ erl -noshell -s stdin run

run() ->
  printRobotOptions(),
  loop(),
  init:stop().
  
loop() ->
  Line = read_stdin(),
  case gameEnded(Line) of
    true -> ok;
    false -> turnAndShoot(), loop()
  end.

read_stdin() ->
  % No prompt
  io:get_line("").

turnAndShoot() ->
  io:format("Accelerate 0.5~n"),
  io:format("Rotate 7 3~n"),
  lists:foreach(
    fun(_N) ->
      io:format("Shoot 10~n")
    end,
    lists:seq(1,10)
  ).

gameEnded(Line) ->
  case Line of
    "Dead\n" -> true;
    "GameFinishes\n" -> true;
    _ -> false
  end.

printRobotOptions() ->
  io:format("RobotOption 3 0~n"),
  io:format("RobotOption 1 1~n"),
  io:format("Name ETurnShoot~n"),
  io:format("Colour ff0000 0000ff~n").
