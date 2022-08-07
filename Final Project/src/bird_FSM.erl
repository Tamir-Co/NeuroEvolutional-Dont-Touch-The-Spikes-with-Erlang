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
-export([start/3]).
-export([init/1, callback_mode/0, terminate/3, stop/0]).
-export([idle/3, simulation/3]).

% =========================================
%% Creates a gen_statem process which calls bird_FSM:init/1
start(Name, PC_PID, SpikesList) ->
	gen_statem:start({local,Name}, ?MODULE, [PC_PID, SpikesList], []).

% =========================================
init([PC_PID, SpikesList]) ->
	{ok, idle, #bird{	pc_pid=PC_PID,
						spikesList = SpikesList
					}}.	% Init bird location to center

callback_mode() ->
	state_functions.

% =========================================
% idle(enter, _OldState, Bird=#bird{}) ->
% {keep_state, #bird{}}.
idle(info, {start_simulation}, Bird=#bird{}) ->
	{next_state, simulation, Bird}.

% =========================================
simulation(cast, {spikes_list, SpikesList}, Bird) ->
	{keep_state, Bird#bird{spikesList = SpikesList}};
simulation(cast, {jump}, Bird=#bird{pc_pid=PC_PID, spikesList=SpikesList}) ->
	NextBird = simulate_next_frame_bird(jump(Bird), SpikesList),
	#bird{x=X, y=Y, direction=Direction} = NextBird,
	gen_server:cast(PC_PID, {bird_location, X, Y, Direction}),
	{keep_state, NextBird};
simulation(cast, {simulate_frame}, Bird=#bird{pc_pid=PC_PID, spikesList=SpikesList}) ->
	NextBird = simulate_next_frame_bird(Bird, SpikesList),
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


simulate_next_frame_bird(Bird=#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction, pc_pid = PC_PID}, SpikesList) ->
%%	io:format("~nGame simulate_next_frame_bird!, Bird=~p~n", [Bird]),
	%% update direction and X value
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{r, _    , true } -> NewDirection = l        , NewX = X - ?X_VELOCITY;
		{r, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{l, true , _    } -> NewDirection = r        , NewX = X + ?X_VELOCITY;
		{l, false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,

	%% check if the bird touching top/bottom spikes
	case Y >= ?SPIKES_BOTTOM_Y orelse Y =< ?SPIKES_TOP_Y of
		true  -> game_over(PC_PID, top_bottom);
		false -> ok
	end,

	%% check if the bird touching wall spikes, only when heading to the wall and near it
%%	case (X =< ?SPIKE_HEIGHT_4 andalso NewDirection == l)  orelse (X >= ?RIGHT_WALL_X - ?SPIKE_HEIGHT_4 andalso NewDirection == r) of
%%		true ->
			case is_bird_touch_wall_spike(Bird, SpikesList, NewDirection) of
				true  -> game_over(PC_PID, wall);
				false -> ok
			end,
%%		false -> ok
%%	end,

	Bird#bird{x=NewX, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+2, direction=NewDirection}.


game_over(PC_PID, Reason) ->
	gen_server:cast(PC_PID, {bird_disqualified, self()}),
	io:format("~nGame Over! touch the ~p~n",[Reason]),
	terminate(normal, undefined, undefined).

%% Receive bird location and spikes.
%% Return true if bird disqualified and otherwise false
is_bird_touch_wall_spike(_Bird=#bird{x=X, y=Y}, SpikesList, Direction) ->
%%	io:format("\n\nX=~p, Y=~p", [X, Y]),
	case (X =< ?SPIKE_HEIGHT_4 andalso Direction == l)  orelse (X >= ?RIGHT_WALL_X - ?SPIKE_HEIGHT_4 andalso Direction == r) of	% bird is near the wall
		false -> false;				% bird still in the game
		true  -> case lists:nth(closest_spike(Y), SpikesList) of	% check closest spike
					 0 -> false;	% bird still in the game because there is no spike near
					 1 -> true		% bird disqualified
				 end
	end.

%% Gets a height Y and returns the closest spike's index
closest_spike(Y) ->
	SpikeSlotHeight = ?SPIKE_WIDTH + ?SPIKE_GAP,
	min(10, 1 + trunc((Y-?SPIKES_TOP_Y) / SpikeSlotHeight + 0.5)).


% =========================================
terminate(_Reason, _StateName, _State) ->
	ok.


stop() ->
	gen_statem:stop(bird_FSM, normal, infinity).