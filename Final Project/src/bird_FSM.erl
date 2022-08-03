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
-export([start_bird_FSM/2]).
-export([init/1, callback_mode/0]).
-export([idle/3, simulation/3]).

% =========================================
%% Creates a gen_statem process which calls bird_FSM:init/1
start_bird_FSM(Name, PC_PID) ->
	gen_statem:start({local,Name}, ?MODULE, [PC_PID], []).

% =========================================
init(PC_PID) ->
	{ok, idle, #bird{pc_pid=PC_PID}}.	% Init bird location to center

callback_mode() ->
	state_functions.

% =========================================
% idle(enter, _OldState, Bird=#bird{}) ->
	% {keep_state, #bird{}}.
idle(info, {start_simulation}, Bird=#bird{}) ->
	{next_state, simulation, Bird}.

% =========================================
simulation(cast, {jump}, Bird=#bird{}) ->
	{keep_state, jump(Bird)};
simulation(cast, {simulate_frame}, Bird=#bird{}) ->
	simulate_next_frame_bird(Bird),
	{keep_state, Bird};

simulation(info, {jump}, Bird=#bird{}) ->
	{keep_state, jump(Bird)};
simulation(info, {simulate_frame}, Bird=#bird{}) ->
	simulate_next_frame_bird(Bird),
	{keep_state, Bird}.

jump(Bird=#bird{}) ->
	Bird#bird{velocityY=-?JUMP_VELOCITY}.


simulate_next_frame_bird(Bird=#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}) ->
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{right, _    , true } -> NewDirection = left     , NewX = X - ?X_VELOCITY;
		{right, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{left , true , _    } -> NewDirection = right    , NewX = X + ?X_VELOCITY;
		{left , false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,
	% case  of
		 % -> ;
	% end,
	if 	Bird#bird.y >= ?SPIKES_BOTTOM_Y orelse Bird#bird.y =< ?SPIKES_TOP_Y ->	% bird touching top/bottom spikes
								io:format("~nGame Over!~n");
								% to idle
							true ->
								ok
						end,
	Bird#bird{x=NewX, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+2, direction=NewDirection}.

