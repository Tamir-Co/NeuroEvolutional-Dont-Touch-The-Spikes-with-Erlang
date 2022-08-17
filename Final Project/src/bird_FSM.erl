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
-export([start/4]).
-export([init/1, callback_mode/0, terminate/3, stop/0]).
-export([idle/3, simulation/3]).

%% =================================================================
%% Creates a gen_statem process which calls bird_FSM:init/1
start(Name, PC_PID, SpikesList, GraphicState) ->
	gen_statem:start({local,Name}, ?MODULE, [PC_PID, SpikesList, GraphicState], []).

%% =================================================================
init([PC_PID, SpikesList, GraphicState]) ->
	case GraphicState of
		idle        -> NN_PID = undefined;
		play_NEAT   -> Self = self(), NN_PID = spawn_link(fun() -> neural_network:init(?NN_STRUCTURE, Self) end) % init NN
	end,
	{ok, idle, #bird{pcPID = PC_PID,
					 spikesList = SpikesList,
					 graphicState = GraphicState,
					 nnPID = NN_PID
					 }}.

callback_mode() ->
	state_functions.

%% =================================================================
idle(info, {start_simulation}, Bird=#bird{graphicState=GraphicState}) ->
	case GraphicState of
		idle ->
			{next_state, simulation, Bird#bird{graphicState=play_user, spikesList=?INIT_SPIKE_LIST}};    % graphics init this bird from the beginning
		
		play_NEAT ->
			{next_state, simulation, Bird#bird{frameCount=0, spikesList=?INIT_SPIKE_LIST}}
	end;
idle(info, {replace_genes, NewBrain}, Bird=#bird{nnPID = NN_PID}) ->    % Replace the genes of the bird with other better genes
%%	wx_object:cast(graphics, {brain, NewBrain}),
	rpc:call(?GRAPHICS_NODE, graphics, graphics_rpc, [{brain, NewBrain}]),
	NN_PID ! {set_weights, NewBrain},
%%	io:format("~ncast brain to graphics"),
	{keep_state, Bird};
idle(cast, {spikes_list, _SpikesList}, Bird) -> % ignore
	{keep_state, Bird};
idle(cast, {jump}, Bird) -> % ignore
	{keep_state, Bird};
idle(cast, {simulate_frame}, Bird) -> % ignore
	{keep_state, Bird}.


simulation(cast, {spikes_list, SpikesList}, Bird=#bird{nnPID=NN_PID, graphicState=GraphicState}) ->
	case GraphicState of
		play_user -> ok;
		play_NEAT -> NN_PID ! {spikes_list, SpikesList}   % send spike list to NN
	end,
	{keep_state, Bird#bird{spikesList = SpikesList}};
simulation(cast, {jump}, Bird) ->
	NextBird = jump(Bird),
	{keep_state, NextBird};
simulation(cast, {simulate_frame}, Bird=#bird{spikesList=SpikesList, graphicState=play_user}) ->	% play_user
	{IsDead, NextBird} = simulate_next_frame_bird(Bird, SpikesList),
	case IsDead of
		true ->
			wx_object:cast(graphics, {user_bird_disqualified}),  % notify graphics that its bird dead
			io:format("Game Over!~n"),
			{next_state, idle, #bird{}};
		
		false ->
			#bird{y=Y} = NextBird,
			wx_object:cast(graphics, {bird_location, Y}),
			{keep_state, NextBird}
	end;
simulation(cast, {simulate_frame}, Bird=#bird{spikesList=SpikesList, graphicState=play_NEAT,		% play_NEAT
											  pcPID=PC_PID, nnPID=NN_PID, frameCount=FrameCount}) ->
	?PRINT(),
	run_NN(Bird),       % ask NN whether to jump
	{IsDead, NextBird}  =
		receive
			{jump, FrameCount}      -> simulate_next_frame_bird(jump(Bird), SpikesList);
			{dont_jump, FrameCount} -> simulate_next_frame_bird(Bird, SpikesList)
		after ?TIMER     -> simulate_next_frame_bird(Bird, SpikesList)
		end,
	flush(),
%%	{IsDead, NextBird} = simulate_next_frame_bird(Bird, SpikesList),
	case IsDead of
		true ->     % bird is dead
			?PRINT(simulate_frame_play_NEAT_DEAD),
			NN_PID ! {get_weights, self()},    % get weights from the NN
			receive     % TODO after all
				{weights_list, WeightsList} ->
						case FrameCount > 280 of
							true  -> i;%o:format("WeightsList ~p~n", [WeightsList]);
							false -> ok
						end,
						gen_server:cast(PC_PID, {bird_disqualified, self(), FrameCount, WeightsList}),   % send bird_disqualified to PC
						NN_PID ! {spikes_list, ?INIT_SPIKE_LIST},
						{next_state, idle, #bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID, frameCount=0}}
				
			after ?TIMER ->
				gen_server:cast(PC_PID, {bird_disqualified, self(), -1, []}),   % send bird_disqualified to PC
				NN_PID ! {spikes_list, ?INIT_SPIKE_LIST},
				{next_state, idle, #bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID, frameCount=0}}
			end;
			
		false ->     % bird is alive
%%			run_NN(NextBird),
			#bird{y=Y} = NextBird,
			gen_server:cast(PC_PID, {bird_location, Y}),
			{keep_state, NextBird#bird{frameCount = FrameCount + 1}}
	end.


%% =================================================================
jump(Bird=#bird{}) ->
	Bird#bird{velocityY=-?JUMP_VELOCITY}.


simulate_next_frame_bird(Bird=#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}, SpikesList) ->
	%% update direction and X value
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{r, _    , true } -> NewDirection = l        , NewX = X - ?X_VELOCITY;
		{r, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{l, true , _    } -> NewDirection = r        , NewX = X + ?X_VELOCITY;
		{l, false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,

	%% check if the bird touching top/bottom spikes, or bird touching wall spikes (only when heading to the wall and near it)
	IsDead = (Y >= ?SPIKES_BOTTOM_Y-?BIRD_HEIGHT) orelse (Y =< ?SPIKES_TOP_Y) orelse is_bird_touch_wall_spike(Bird, SpikesList, NewDirection),
	
	{IsDead, Bird#bird{x=NewX, y=Y+VelocityY, velocityY=VelocityY+?GRAVITY, direction=NewDirection}}.


run_NN(_Bird = #bird{x=X, y=Y, direction=Direction, nnPID=NN_PID, frameCount=FrameCount}) ->
	case Direction of
		r -> NN_PID ! {decide_jump, Y, ?BG_WIDTH-X, FrameCount};
		l -> NN_PID ! {decide_jump, Y, X, FrameCount}
	end.


%% Receive bird location and spikes.
%% Return true if bird disqualified and otherwise false
is_bird_touch_wall_spike(_Bird=#bird{x=X, y=Y}, SpikesList, Direction) ->
	case (X =< ?SPIKE_HEIGHT_4 andalso Direction == l)  orelse (X >= ?BG_WIDTH - ?BIRD_WIDTH - ?SPIKE_HEIGHT_4 andalso Direction == r) of	% bird is near the wall
		false -> false;				% bird still in the game
		true  -> case lists:nth(closest_spike(Y), SpikesList) of	% check closest spike
					 0 -> false;	% bird still in the game because there is no spike near
					 1 -> true		% bird disqualified
				 end
	end.


%% Gets a height Y and returns the closest spike's index
closest_spike(Y) ->
	SpikeSlotHeight = ?SPIKE_WIDTH + ?SPIKE_GAP_Y,
	min(10, 1 + trunc((Y-?SPIKES_TOP_Y) / SpikeSlotHeight + 0.5)).


flush() ->
	receive
		_ -> flush()
	after
		0 -> ok
	end.

%% =================================================================
terminate(_Reason, _StateName, _State) ->
	ok.


stop() ->
	gen_statem:stop(bird_FSM, normal, infinity).
