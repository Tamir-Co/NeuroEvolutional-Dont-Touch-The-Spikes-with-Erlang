%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Aug 2022 1:30
%%%-------------------------------------------------------------------
-module(bird_FSM).
-author("Nadav & Tamir").
-behaviour(gen_statem).
-include("constants.hrl").

%% API
-export([init/1, callback_mode/0]).

%% Creates a gen_statem process which calls bird_FSM:init/1
start_bird_FSM(Name,PC_PID) ->
	gen_statem:start({local,Name}, ?MODULE, [PC_PID], []).

init(PC_PID) ->
	{ok, idle, init_bird(PC_PID)}.

callback_mode() ->
	state_functions.


% Init bird location to center
init_bird(PC_PID) ->
	#bird{x=?BIRD_START_X, y=?BIRD_START_Y, velocityY=VelocityY, direction=right, pc_pid=PC_PID}.

simulate_bird(Bird = #bird{x=X, y=Y, velocityY=-?JUMP_VELOCITY, direction=Direction}) ->
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{right, _    , true } -> NewDirection = left     , NewX = X - ?X_VELOCITY;
		{right, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{left , true , _    } -> NewDirection = right    , NewX = X + ?X_VELOCITY;
		{left , false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,
	todo,
	Bird#bird{x=NewX, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+2, direction=NewDirection}.

% jump(Bird = #bird{x=X, y=Y, velocityY=_VelocityY, direction=Direction}) ->
jump(Bird=#bird{}) ->
	% Bird#bird{x=X, y=Y-?JUMP_VELOCITY*?TIME_UNIT, velocityY=-?JUMP_VELOCITY, direction=Direction}
	Bird#bird{velocityY=}
.