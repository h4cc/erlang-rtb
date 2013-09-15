
-module(protocol).

-export([get_next_message/0]).

-export([send_robot_option/2, send_name/1, send_name/2, send_colour/2, send_print/1, 
  send_shoot/1, send_sweep/4, send_rotate/2, send_rotate_amount/3, send_accelerate/1, send_brake/1]).

-include("records.hrl").

-define(send(Format, Values), io:format(Format, Values), io:format(standard_error, Format, Values)).



get_next_message() ->
  Message = io:get_line(""),
  CleanMessage = string:strip(Message, right, $\n),
  Tokens = string:tokens(CleanMessage, " "),
  tokens_to_record(Tokens).


send_robot_option(send_signal, Bool) when is_boolean(Bool) ->
  send_key_value("RobotOption", 0, bool_to_int(Bool));
  
send_robot_option(signal, SignalNumber) ->
  send_key_value("RobotOption", 2, SignalNumber);
  
send_robot_option(send_rotation_reached, Value) ->
  send_key_value("RobotOption", 1, Value);

send_robot_option(use_non_blocking, Bool) when is_boolean(Bool) ->
  send_key_value("RobotOption", 3, bool_to_int(Bool)).


send_name(Name) ->
  ?send("Name ~s~n", [Name]).

send_name(Name, Team) ->
  send_name(Name ++ " Team: "++Team).
  

send_colour(Home, Away) ->
  ?send("Colour ~s ~s~n", [Home, Away]).
    

send_print(String) ->
  ?send("Print ~s~n", [String]).
  
send_shoot(Energy) when is_float(Energy) ->
  send_key_value("Shoot", Energy).
  
send_sweep(RotateWhat, Speed, AngleLeft, AngleRight) when is_float(Speed), is_float(AngleLeft), is_float(AngleRight) ->
  What = object_to_number(RotateWhat),
  ?send("Sweep ~p ~f ~f ~f~n", [What, Speed, AngleLeft, AngleRight]).
  
send_rotate(RotateWhat, Angle) when is_float(Angle) ->
  What = object_to_number(RotateWhat),
  ?send("Rotate ~p ~f~n", [What, Angle]).

send_rotate_amount(RotateWhat, Velocity, Angle) when is_float(Velocity), is_float(Angle) ->
  What = object_to_number(RotateWhat),
  ?send("RotateAmount ~p ~f ~f~n", [What, Velocity, Angle]).

send_accelerate(Speed) when is_float(Speed) ->
  ?send("Accelerate ~f~n", [Speed]).

send_brake(Portion) when is_float(Portion) ->
  ?send("Brake ~f~n", [Portion]).

%----------------

object_to_number(ObjectType) when is_atom(ObjectType) ->
  case ObjectType of
    robot -> ?RTB_ROTATE_ROBOT;
    canon -> ?RTB_ROTATE_CANON;
    radar -> ?RTB_ROTATE_RADAR;
    robot_canon -> ?RTB_ROTATE_ROBOT + ?RTB_ROTATE_CANON;
    robot_radar -> ?RTB_ROTATE_ROBOT + ?RTB_ROTATE_RADAR;
    radar_canon -> ?RTB_ROTATE_RADAR + ?RTB_ROTATE_CANON;
    all         -> ?RTB_ROTATE_RADAR + ?RTB_ROTATE_CANON + ?RTB_ROTATE_ROBOT
  end.


tokens_to_record([Name | Values]) ->
  #message{name = Name, values = Values}.
  

%send_key_value(Key) -> 
%  ?send("~s~n", [Key]).
 
send_key_value(Key, Value) -> 
  ?send("~s ~p~n", [Key, Value]).

send_key_value(Key, Value1, Value2) -> 
  ?send("~s ~p ~p~n", [Key, Value1, Value2]).

bool_to_int(Bool) when is_boolean(Bool) ->
  case Bool of
    true -> 1;
    false -> 0
  end.


