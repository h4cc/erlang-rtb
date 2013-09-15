%%%
%%%
%%%

%--- Configuration
-define(PLAYER_NAME, "Julius").
-define(PLAYER_COLOUR_HOME, "ff0000").
-define(PLAYER_COLOUR_AWAY, "00ff00").
%----

-module(robot).
-export([run/0]).

-include("records.hrl").

-record(player, {
  name = ?PLAYER_NAME :: string()
}).
-record(control, {
  direction = left :: left | right
}).
-record(state, {
  player = #player{}, 
  control = #control{}, 
  iteration = 0         :: integer()
}).

% Access to current direction
-define(get_turn_direction(State), (State#state.control)#control.direction).

% Access to the current player name.
-define(get_player_name(State), (State#state.player)#player.name).

% Run the bot.
-spec run() -> ok.
run() ->
  appmon:start(),
  input_loop(#state{}),
  ok.

% Loop for each input from RTB.
input_loop(#state{iteration=Iteration} = State) ->
  Message = protocol:get_next_message(),
  %io:format(standard_error, "Message ~p~n", [Message]),
  
  Values = ?get_message_values(Message),
  case ?get_message_name(Message) of
    "Initialize" -> 
      handle_initialize(State, Values);
    "YourName" ->
      ok;
    "YourColour" -> 
      ok;
    "GameOption" ->
      handle_game_option(Values);
    "GameStarts" ->
      handle_game_starts();
    "Radar" ->
      handle_radar(State, Values);
    "Info" -> 
      handle_info(Values);
    "Coordinates" -> 
      handle_coordinates(Values);
    "RobotInfo" -> 
      handle_robot_info(Values);
    "RotationReached" -> 
      handle_rotation_reached(Values);
    "Energy" -> 
      handle_energy(Values);
    "RobotsLeft" -> 
      handle_robots_left(Values);
    "Collision" -> 
      handle_collision(Values);
    "Warning" -> 
      handle_warning(Values);
    "Dead" -> 
      handle_dead();
    "GameFinishes" -> 
      handle_game_finishes();
    "ExitRobot" -> 
      handle_exit_robot();
    UnknownName -> 
      protocol:send_print("Unknown Message: "++UnknownName)
  end,
  
  NewState = case Iteration rem 1000 of
    0 -> toggle_turn_direction(State);
    _ -> State
  end,
  
  case is_game_ended(Message) of
    true -> init:stop(), ok;
    false -> input_loop(NewState#state{iteration=Iteration+1})
  end.

% Toggles the turn direction in the given state.
toggle_turn_direction(#state{control= Control} = State) ->
  NewControl = case ?get_turn_direction(State) of
    left  -> Control#control{direction = right};
    right -> Control#control{direction = left}
  end,
  State#state{control = NewControl}.

handle_initialize(State, [_First]) ->
  protocol:send_print(?get_player_name(State) ++ " has joined the game."),
  send_init().
  
handle_game_starts() ->
  protocol:send_print("Lets get ready to rumble!"),
  protocol:send_sweep(radar_canon, 99.0, ?degree(-90), ?degree(90)),
  protocol:send_accelerate(0.7),
  ok.
  
handle_radar(State, [DistanceString, ObjectTypeString, RadarAngleString]) ->
  {Distance, _} = string:to_float(DistanceString),
  {ObjectType, _} = string:to_integer(ObjectTypeString),
  {RadarAngle, _} = string:to_float(RadarAngleString),
  io:format(standard_error, "RADAR Object Type: ~p Distance: ~p Angle: ~p~n", [ObjectType, Distance, RadarAngle]),
  
  TurnDirection = case ?get_turn_direction(State) of
    left -> 1;
    right -> -1
  end,
  
  case ObjectType of
    ?RTB_ROBOT ->
      % Shoot at enemy
      case Distance < 10 of
        false -> ok;
        true ->
          Power = 10 - Distance,
          protocol:send_shoot(Power)
      end,
      protocol:send_brake(0.0),
      protocol:send_rotate_amount(robot, 10.0, RadarAngle);
    ?RTB_SHOT -> ok;
    ?RTB_WALL ->
      % Try to avoid wall.
      case Distance < 4 of
        false -> ok;
        true ->
          protocol:send_brake(0.5),
          protocol:send_rotate_amount(robot, 10.0, ?degree(40)*TurnDirection)
      end,
      case random:uniform(100) of
        100 ->
          % Rotate once
          protocol:send_rotate_amount(robot, 10.0, ?degree(180)*TurnDirection);
        _ -> undefined
      end;
    ?RTB_COOKIE ->
      % Steer to cookie.
      protocol:send_brake(0.2),
      protocol:send_rotate_amount(robot, 10.0, RadarAngle);
    ?RTB_MINE ->
      % Destroy mine directly next to me
      protocol:send_brake(0.0),
      case Distance < 5 of
        false -> ok;
        true ->
          protocol:send_shoot(0.5),
          protocol:send_shoot(0.5),
          protocol:send_shoot(0.5)
      end
  end.
  
handle_game_option([_Type, _Value]) ->
  ok.
  
handle_info([_Time, _Speed, _CanonAngle]) ->
  ok.

handle_coordinates([_X, _Y, _Angle]) ->
  ok.

handle_robot_info([_EnergyLevel, _TeamMate]) ->
  ok.

handle_rotation_reached([_RotateType]) ->
  ok.

handle_energy([_EnergyLevel]) ->
  ok.

handle_robots_left([_NumberOfRobots]) ->
  ok.

handle_collision([ObjectTypeString, RelativeAngleString]) ->
  {ObjectType, _} = string:to_integer(ObjectTypeString),
  {_RelativeAngle, _} = string:to_float(RelativeAngleString),
  io:format(standard_error, "COLLISION Object Type: ~p~n", [ObjectType]),
  case ObjectType of
    ?RTB_ROBOT ->
      ok;
    ?RTB_SHOT ->
      ok;
      %protocol:send_print("I am hit, that hurts!");
    ?RTB_WALL ->
      protocol:send_print("That wall hit ME!");
    ?RTB_COOKIE ->
      protocol:send_print("I love Cookies!");
    ?RTB_MINE ->
      protocol:send_print("D'oh, a mine!")
  end.

handle_warning([_WarningType, _Message]) ->
  ok.

handle_dead() ->
  ok.

handle_game_finishes() ->
  ok.

%%% Handles the ExitRobot Message.
-spec handle_exit_robot() -> ok.
handle_exit_robot() ->
  ok.


%--------

send_init() ->
  protocol:send_name(?PLAYER_NAME),
  protocol:send_robot_option(use_non_blocking, false),
  protocol:send_colour(?PLAYER_COLOUR_HOME, ?PLAYER_COLOUR_AWAY),
  ok.

is_game_ended(#message{name=Name}) ->
  case Name of
    "ExitRobot" -> true;
    _ -> false
  end.
  
