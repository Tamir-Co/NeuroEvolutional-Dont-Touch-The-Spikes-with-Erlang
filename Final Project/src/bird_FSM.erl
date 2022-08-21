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

%% ================================================================= init:
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

%% ================================================================== idle:

%% A message from the PC in order to start the simulation state.
idle(info, {start_simulation}, Bird=#bird{graphicState=GraphicState}) ->
	case GraphicState of
		idle ->
			{next_state, simulation, Bird#bird{graphicState=play_user, spikesList=?INIT_SPIKE_LIST}};    % graphics init this bird from the beginning
		
		play_NEAT ->
			{next_state, simulation, Bird#bird{frameCount=0, spikesList=?INIT_SPIKE_LIST, framesToDecide=?FRAMES_BETWEEN_DECIDE_JUMP}}
	end;

%% A message from the PC with new brain/genes. Then, the neural network is set accordingly.
idle(info, {replace_genes, NewBrain}, Bird=#bird{nnPID = NN_PID}) ->
	NN_PID ! {set_weights, NewBrain},
	{keep_state, Bird};

%% Ignore this messages
idle(cast, {spikes_list, _SpikesList}, Bird) ->
	{keep_state, Bird};
idle(cast, {jump}, Bird) ->
	{keep_state, Bird};
idle(cast, {simulate_frame}, Bird) ->
	{keep_state, Bird}.

%% ================================================================== simulation:

%% A message from the PC with new spikes list. If the bird is in play_NEAT mode, the spikes list is sent to its neural network.
simulation(cast, {spikes_list, SpikesList}, Bird=#bird{nnPID=NN_PID, graphicState=GraphicState}) ->
	case GraphicState of
		play_user -> ok;
		play_NEAT -> NN_PID ! {spikes_list, SpikesList}   % send spike list to NN
	end,
	{keep_state, Bird#bird{spikesList = SpikesList}};

%% A message to tell the bird to jump.
simulation(cast, {jump}, Bird) ->
	NextBird = jump(Bird),
	{keep_state, NextBird};
	
%% A message from the graphics (the main node) to tell the bird to simulate the next frame (in play_user mode).
simulation(cast, {simulate_frame}, Bird=#bird{spikesList=SpikesList, graphicState=play_user}) ->	% play_user
	{IsDead, NextBird} = simulate_next_frame_bird(Bird, SpikesList),
	case IsDead of
		true ->
			wx_object:cast(graphics, {user_bird_disqualified}),  % notify graphics that its bird is dead
			io:format("Game Over!~n"),
			{next_state, idle, #bird{}};
		
		false ->
			#bird{y=Y} = NextBird,
			wx_object:cast(graphics, {user_bird_location, Y}),
			{keep_state, NextBird}
	end;

%% A message from the PC to tell the bird to simulate the next frame (in play_NEAT mode).
simulation(cast, {simulate_frame}, Bird=#bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID, frameCount=FrameCount, framesToDecide=FramesToDecide, spikesList=SpikesList}) ->     % play_NEAT
	{IsDead, NextBird} = simulate_next_frame_bird(Bird, SpikesList),
	case IsDead of		% check if the bird dies in the current frame.
		true ->     % bird is dead
			NN_PID ! {spikes_list, ?INIT_SPIKE_LIST},
			NN_PID ! {get_weights},    % get weights from the NN
			{keep_state, Bird};
		
		false ->     % bird is alive
			case FramesToDecide of	% don't ask the NN if the bird need to jump in each frame
				0 ->
					run_NN(NextBird),
					#bird{y=Y} = NextBird,
					gen_server:cast(PC_PID, {neat_bird_location, Y}),	% sends the new Y location to the PC.
					{keep_state, NextBird#bird{frameCount = FrameCount + 1, framesToDecide=?FRAMES_BETWEEN_DECIDE_JUMP}};
				
				_ ->
					#bird{y=Y} = NextBird,
					gen_server:cast(PC_PID, {neat_bird_location, Y}),	% sends the new Y location to the PC.
					{keep_state, NextBird#bird{frameCount = FrameCount + 1, framesToDecide=FramesToDecide-1}}
			end
	end;

%% A message from the neural_network with the current weight list (current brain).
%% This is the response to the message {get_weights} which is sent to the neural_network when this bird dies.
simulation(info, {weights_list, WeightsList}, _Bird=#bird{pcPID=PC_PID, nnPID=NN_PID, frameCount=FrameCount}) ->
	gen_server:cast(PC_PID, {bird_disqualified, self(), FrameCount, WeightsList}),   % send bird_disqualified to PC with the current frame count and weights list
	{next_state, idle, #bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID}}.

%%simulation(cast, {simulate_frame}, Bird=#bird{spikesList=SpikesList, graphicState=play_NEAT,		% play_NEAT
%%											  pcPID=PC_PID, nnPID=NN_PID, frameCount=FrameCount, framesToDecide=FramesToDecide}) ->
%%
%%	{{IsDead, NextBird}, NewFramesToDecide}  =
%%		case FramesToDecide of  % dont ask to jump each frame
%%			0 ->
%%				run_NN(Bird),       % ask NN whether to jump
%%				receive
%%					{jump     , FrameCount} -> {simulate_next_frame_bird(jump(Bird), SpikesList), ?FRAMES_BETWEEN_DECIDE_JUMP};
%%					{dont_jump, FrameCount} -> {simulate_next_frame_bird(Bird, SpikesList),       ?FRAMES_BETWEEN_DECIDE_JUMP}
%%				after ?TIMER                -> {simulate_next_frame_bird(Bird, SpikesList),       ?FRAMES_BETWEEN_DECIDE_JUMP}
%%				end;
%%
%%			_ ->
%%				{simulate_next_frame_bird(Bird, SpikesList), FramesToDecide - 1}
%%		end,
%%
%%%%	flush(),
%%%%	{IsDead, NextBird} = simulate_next_frame_bird(Bird, SpikesList),
%%	case IsDead of
%%		true ->     % bird is dead
%%			NN_PID ! {get_weights, self()},    % get weights from the NN
%%			receive
%%				{weights_list, WeightsList} ->
%%						case FrameCount > 280 of
%%							true  -> i;%o:format("WeightsList ~p~n", [WeightsList]);
%%							false -> ok
%%						end,
%%						gen_server:cast(PC_PID, {bird_disqualified, self(), FrameCount, WeightsList}),   % send bird_disqualified to PC
%%						NN_PID ! {spikes_list, ?INIT_SPIKE_LIST},
%%						{next_state, idle, #bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID, frameCount=0}}
%%
%%			after ?TIMER ->
%%				gen_server:cast(PC_PID, {bird_disqualified, self(), -1, []}),   % send bird_disqualified to PC
%%				NN_PID ! {spikes_list, ?INIT_SPIKE_LIST},
%%				{next_state, idle, #bird{graphicState=play_NEAT, pcPID=PC_PID, nnPID=NN_PID, frameCount=0}}
%%			end;
%%
%%		false ->     % bird is alive
%%%%			run_NN(NextBird),
%%			#bird{y=Y} = NextBird,
%%			gen_server:cast(PC_PID, {neat_bird_location, Y}),
%%			{keep_state, NextBird#bird{frameCount = FrameCount + 1, framesToDecide=NewFramesToDecide}}
%%	end.


%% ==================================================================

%% The bird performs a jump by setting its Y velocity.
jump(Bird=#bird{}) ->
	Bird#bird{velocityY=-?JUMP_VELOCITY}.


%% This function simulates the next frame of the bird. Returns: {IsDead, NewBird}
simulate_next_frame_bird(Bird=#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}, SpikesList) ->
	%% update direction and X value
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{r, _    , true } -> NewDirection = l        , NewX = X - ?X_VELOCITY;	% If the bird is moving to the right and     touching the right wall.
		{r, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;	% If the bird is moving to the right and not touching the right wall.
		{l, true , _    } -> NewDirection = r        , NewX = X + ?X_VELOCITY;	% If the bird is moving to the left  and     touching the left  wall.
		{l, false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY	% If the bird is moving to the left  and not touching the left  wall.
	end,

	%% check if the bird touching top/bottom spikes, or bird touching wall spikes (only when heading to the wall and near it)
	IsDead = (Y+?BIRD_HEIGHT >= ?BOTTOM_RECT_Y-?SPIKE_HEIGHT) orelse (Y =< ?TOP_RECT_HEIGHT+?SPIKE_HEIGHT) orelse is_bird_touch_wall_spike(Bird, SpikesList, NewDirection),
	
	{IsDead, Bird#bird{x=NewX, y=Y+VelocityY, velocityY=VelocityY+?GRAVITY, direction=NewDirection}}.


%% This function sends 2 new inputs (height & distance from wall) to the neural network and asks it to start the feed-forward.
run_NN(_Bird = #bird{x=X, y=Y, direction=Direction, nnPID=NN_PID}) ->
	case Direction of
		r -> NN_PID ! {decide_jump, Y, ?BG_WIDTH-X};
		l -> NN_PID ! {decide_jump, Y, X}
	end.


%% Receive bird location and spikes.
%% Return true if bird disqualified and otherwise false
is_bird_touch_wall_spike(#bird{x=X, y=Y}, SpikesList, Direction) ->
	case (Direction == r andalso X+?BIRD_WIDTH >= ?BG_WIDTH-?SPIKE_HEIGHT_4) orelse (Direction == l andalso X =< ?SPIKE_HEIGHT_4) of	% bird is near the wall
		false -> false;				% bird still in the game
		true  -> case lists:nth(closest_spike(Y), SpikesList) of	% check closest spike
					 0 -> false;	% bird still in the game because there is no spike near
					 1 -> true		% bird disqualified
				 end
	end.


%% Gets a height Y and returns the closest spike's index
closest_spike(Y) ->
	SpikeSlotHeight = ?SPIKE_WIDTH + ?SPIKE_GAP_Y,
	min(?MAX_SPIKES_AMOUNT, 1 + trunc((Y-?SPIKES_TOP_Y) / SpikeSlotHeight + 0.5)).


%% =================================================================
terminate(_Reason, _StateName, _State) ->
	ok.


stop() ->
	gen_statem:stop(bird_FSM, normal, infinity).

