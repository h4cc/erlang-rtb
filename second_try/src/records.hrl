%%%
%%% Records and Macros.
%%%

% Data for a incoming Message.
-record(message, {name, values=[]}).

% Object Types.
-define(RTB_ROBOT, 0).
-define(RTB_SHOT, 1).
-define(RTB_WALL, 2).
-define(RTB_COOKIE, 3).
-define(RTB_MINE, 4).

% Rotating
-define(RTB_ROTATE_ROBOT, 1).
-define(RTB_ROTATE_CANON, 2).
-define(RTB_ROTATE_RADAR, 4).
-define(RTB_ROTATE_ALL, 7).

% Macros
-define(degree(Degree), (Degree/360)*math:pi()).

-define(get_message_name(Message), Message#message.name).
-define(get_message_values(Message), Message#message.values).
