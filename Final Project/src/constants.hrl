%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2022 13:17
%%%-------------------------------------------------------------------
-author("Nadav & Tamir").

-define(PRINT(Text, Arg), io:format(atom_to_list(Text) ++ " ~p~n", [Arg])).

%% Frame structure:
-define(RIGHT_WALL_X, 350).		% TODO check
-define(BG_WIDTH, 400).
-define(BG_HEIGHT, 700).


%% Spikes:
-define(MAX_SPIKES_AMOUNT, 10).
-define(MAX_RATIONAL_SPIKES_AMOUNT, (?MAX_SPIKES_AMOUNT-2)).
-define(ADD_SPIKES_WALL_TOUCH, 0.5).
-define(INIT_SPIKES_WALL_AMOUNT, 0.5).
-define(SPIKE_WIDTH, 36).
-define(SPIKE_HEIGHT, 25).
-define(SPIKE_HEIGHT_4, 6).
-define(SPIKE_HALF_WIDTH, 18).
-define(SPIKE_GAP_Y, 13).
-define(SPIKES_TOP_Y, 100).

-define(TOP_BOTTOM_SPIKES_AMOUNT, 8).
-define(SPIKES_LEFT_X, 20).%32 | 25		400 - (25 + (36 + 14) * 7).
-define(SPIKE_GAP_X, 9).   %12 | 14
-define(SPIKES_BOTTOM_Y, ?BG_HEIGHT-120). % TODO need this?
-define(TOP_RECT_HEIGHT, 70).
-define(BOTTOM_RECT_Y, 610). %?BG_HEIGHT-120
-define(BOTTOM_RECT_HEIGHT, 90).


%% Bird:
-define(BIRD_WIDTH, 50).
-define(BIRD_HEIGHT, 34).

-define(GRAVITY, 2).
-define(JUMP_VELOCITY, 15).
-define(X_VELOCITY, 7).
-define(BIRD_START_X, 180).
-define(BIRD_START_Y, 320).

-define(NUM_OF_BIRDS, 2).    % TODO 1000 or other number, and move to graphics
-define(PERCENT_SURVIVED_BIRDS, 0.1).   % how many birds are survived after each generation (in %)
-define(NUM_OF_SURVIVED_BIRDS, ceil(?NUM_OF_BIRDS*?PERCENT_SURVIVED_BIRDS)).   % how many birds are survived after each generation


%% Neural network:
-define(NN_STRUCTURE, [12, 1]).

-define(MUTATION_WEIGHT_FACTOR, 20).    % used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_BIAS_FACTOR, 1).       % used in division of the range [-0.5,0.5] to a smaller range

%% UI IDs:
-define(ButtonStartUserID, 10).
-define(ButtonStartNEATID, 11).
-define(ButtonJumpID, 12).

%% Time:
-define(TIMER, 60).		% Graphics update timer
-define(TIME_UNIT, 1).



%% RECORDS %%
-record(graphics_state, {
	frame,
	panel,
	brush,
	textScore,
	mainSizer,
	uiSizer,
	startSizer,
	jumpSizer,
	bitmapBG,
	bitmapBird_R,
	bitmapBird_L,
	birdUser,
	birdUserPID,
	birdList,
	bird_x = ?BIRD_START_X,
	bird_direction = r,
	bestCandBirds = [],
	score = 0,
	bestScore = 0,
	curr_state,
	spikesList,
	spikesAmount = ?INIT_SPIKES_WALL_AMOUNT,
	pcList,
	pcsInSimulation
}).
%graphics_state has: main window, panel, background and bird images, bird record, and atom curr_state (idle, play_user, play_NEAT)

-record(pc_bird_server_state, {
	pcName,
	graphicState = idle,
	numOfPcBirds,       % amount of PC birds ?250?
	numOfAliveBirds,    % amount of birds in simulation right now
	birdsMap            % #{PID => {frameCount, WeightsMap}}
}).

-record(bird, {
	x = ?BIRD_START_X,
	y = ?BIRD_START_Y,
	velocityY = 0,
	direction = r,
	pc_pid,
	spikesList = lists:map(fun(_) -> 0 end, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	nnPID,
	graphicState = idle,
	frameCount = 0
}).

-record(nn_data, {
	networkStructure,   % [L1, L2, ...]
	n_PIDsLayersMap,
	weightsMap,
	n_PIDsList
}).

-record(neuron_data, {
	acc = 0,			% accumulator (Z)
	weights = #{},		% input weights (W)
	bias = 0,			% bias (B)
	activation = tanh,	% activation function (A)
	remInPIDs = [],		% remaining input neuron PIDs. Change during the calculation (because of deleting)
	origInPIDs = [],	% original input neuron PIDs. Doesn't change
	outPIDs = []		% output neuron PIDs
}).
