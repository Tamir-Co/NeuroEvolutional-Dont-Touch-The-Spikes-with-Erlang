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
-export([start/2]).
-export([init/1, callback_mode/0, terminate/3, stop/0]).
-export([idle/3, simulation/3]).

% =========================================
%% Creates a gen_statem process which calls bird_FSM:init/1
start(Name, PC_PID) ->
	gen_statem:start({local,Name}, ?MODULE, [PC_PID], []).

% =========================================
init([PC_PID]) ->
	{ok, idle, #bird{pc_pid=PC_PID}}.	% Init bird location to center

callback_mode() ->
	state_functions.

% =========================================
% idle(enter, _OldState, Bird=#bird{}) ->
% {keep_state, #bird{}}.
idle(info, {start_simulation}, Bird=#bird{}) ->
	{next_state, simulation, Bird}.

% =========================================
simulation(cast, {jump}, Bird=#bird{pc_pid=PC_PID}) ->
	NextBird = simulate_next_frame_bird(jump(Bird)),
	#bird{x=X, y=Y, direction=Direction} = NextBird,
	gen_server:cast(PC_PID, {bird_location, X, Y, Direction}),
	{keep_state, NextBird};
simulation(cast, {simulate_frame}, Bird=#bird{pc_pid=PC_PID}) ->
	NextBird = simulate_next_frame_bird(Bird),
	#bird{x=X, y=Y, direction=Direction} = NextBird,
	gen_server:cast(PC_PID, {bird_location, X, Y, Direction}),
	{keep_state, NextBird}.
%%
%%simulation(info, {jump}, Bird=#bird{}) ->
%%	io:format("S3"),
%%	{keep_state, simulate_next_frame_bird(jump(Bird))};
%%simulation(info, {simulate_frame}, Bird=#bird{}) ->
%%	io:format("S4"),
%%	{keep_state, simulate_next_frame_bird(Bird)}.

jump(Bird=#bird{}) ->
	Bird#bird{velocityY=-?JUMP_VELOCITY}.


simulate_next_frame_bird(Bird=#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction, pc_pid = PC_PID}) ->
%%	io:format("~nGame simulate_next_frame_bird!, Bird=~p~n", [Bird]),
	%% update direction and X value
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{right, _    , true } -> NewDirection = left     , NewX = X - ?X_VELOCITY;
		{right, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{left , true , _    } -> NewDirection = right    , NewX = X + ?X_VELOCITY;
		{left , false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,

	%% check if the bird touching top/bottom spikes
	case Bird#bird.y >= ?SPIKES_BOTTOM_Y orelse Bird#bird.y =< ?SPIKES_TOP_Y of
		true  -> io:format("~nGame Over!~n"),
			gen_server:cast(PC_PID, {bird_disqualified, self()}),
			terminate(normal, undefined, undefined);
		false -> ok
	end,

	Bird#bird{x=NewX, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+2, direction=NewDirection}.


terminate(_Reason, _StateName, _State) ->
	ok.


stop() ->
	gen_statem:stop(bird_FSM, normal, infinity).