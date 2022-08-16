%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2022 13:17
%%%-------------------------------------------------------------------
-author("Nadav & Tamir").


-define(GRAPHICS_NODE,  'pc0@Nadav-VirtualBox').
-define(PC1,            'pc1@Nadav-VirtualBox').
-define(PC2,            'pc2@Nadav-VirtualBox').
-define(PC3,            'pc3@Nadav-VirtualBox').
-define(PC4,            'pc4@Nadav-VirtualBox').
-define(PC_NAMES, [?PC1, ?PC2, ?PC3, ?PC4]).
-define(INIT_PC_AMOUNT, length(?PC_NAMES)).


-define(PRINT(Text, Arg), k).%io:format(atom_to_list(Text) ++ " ~p~n", [Arg])).%o
-define(PRINT(Text), ok).% io:format(atom_to_list(Text) ++ "~n")).%
-define(PRINT(),ok).% io:format("~n")).%


%% Frame structure:
-define(BG_WIDTH, 400).
-define(BG_HEIGHT, 700).


%% Spikes:
-define(INIT_SPIKE_LIST, [0 || _ <- lists:seq(1, ?MAX_SPIKES_AMOUNT)]).
-define(SPIKE_VALUE, 10).
-define(MAX_SPIKES_AMOUNT, 10).
-define(MAX_RATIONAL_SPIKES_AMOUNT, (?MAX_SPIKES_AMOUNT-2)).
-define(ADD_SPIKES_WALL_TOUCH, 1).
-define(INIT_SPIKES_WALL_AMOUNT, 1).
-define(SPIKE_WIDTH, 36).
-define(SPIKE_HEIGHT, 25).
-define(SPIKE_HEIGHT_4, 6).
-define(SPIKE_HALF_WIDTH, 18).
-define(SPIKE_GAP_Y, 14).
-define(SPIKES_TOP_Y, 100).

-define(TOP_BOTTOM_SPIKES_AMOUNT, 8).
-define(SPIKES_LEFT_X, 20).
-define(SPIKE_GAP_X, 9).
-define(SPIKES_BOTTOM_Y, (?BOTTOM_RECT_Y-?SPIKE_HEIGHT)).
-define(TOP_RECT_HEIGHT, 70).
-define(BOTTOM_RECT_Y, 630). %?BG_HEIGHT-120
-define(BOTTOM_RECT_HEIGHT, 70).


%% Bird:
-define(BIRD_WIDTH, 50).
-define(BIRD_HEIGHT, 34).

-define(GRAVITY, 2).
-define(JUMP_VELOCITY, 15).
-define(X_VELOCITY, 7).
-define(BIRD_START_X, 180).
-define(BIRD_START_Y, 320).

-define(NUM_OF_BIRDS, 100).    % TODO 1000 or other number, and move to graphics
-define(PERCENT_SURVIVED_BIRDS, 0.2).   % how many birds are survived after each generation (in %)
-define(NUM_OF_SURVIVED_BIRDS, ceil(?NUM_OF_BIRDS*?PERCENT_SURVIVED_BIRDS)).   % how many birds are survived after each generation, per PC and generation


%% Neural network:
-define(NN_STRUCTURE, [2+?MAX_SPIKES_AMOUNT, 6, 6, 1]).

-define(MUTATION_WEIGHT_FACTOR, 20).    % used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_BIAS_FACTOR, 1).       % used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_FACTOR, 10).            % used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_MAX_RAND_VAL, 20).     % used to define the probability of a edge deletion (W=0)

-define(ACTIVATION_FUNCTION, tanh).
-define(INPUT_ACTIVATION_FUNCTION, identity).


%% UI IDs:
-define(ButtonStartUserID, 10).
-define(ButtonStartNEATID, 11).
-define(ButtonJumpID, 12).


%% Time:
-define(TIMER, 70).		% Graphics update timer
-define(TIME_UNIT, 1).



%% Records:
-record(graphics_state, {
	frame,
	panel,
	brush,
	textScore,
	mainSizer,
	uiSizer,
	startSizer,
	jumpSizer,
	bitmapBird_R,
	bitmapBird_L,
	birdUser,
	birdUserPID,
	birdList = sets:new(),
	locatedBirdsAmount = 0,
	numOfAliveBirds = 0,
	bird_x = ?BIRD_START_X,
	bird_direction = r,
	bestCandBirds = [],
	score = 0,
	bestScore = 0,
	curr_state = idle,
	spikesList = ?INIT_SPIKE_LIST,
	spikesAmount = ?INIT_SPIKES_WALL_AMOUNT,
	pcList = [],
	waitForPCsAmount,
	genNum = 1,
	bestPreviousBrain,
	brainList = []
}).

-record(pc_bird_server_state, {
	pcName,
	graphicState = idle,
	numOfPcBirds,       % amount of PC birds ?250?
	numOfAliveBirds,    % amount of birds in simulation right now
	listOfAliveBirds,   % list of PIDs of current alive (in simulation) birds
	birdsMap            % #{PID => {frameCount, WeightsMap}}
}).

-record(bird, {
	x = ?BIRD_START_X,
	y = ?BIRD_START_Y,
	velocityY = 0,
	direction = r,
	pcPID,
	spikesList = ?INIT_SPIKE_LIST,
	nnPID,
	graphicState = idle,
	frameCount = 0
}).

-record(nn_data, {
	networkStructure,   % [L1, L2, ...]
	n_PIDsLayersMap,
	weightsMap,
	n_PIDsList,
	spikesList = ?INIT_SPIKE_LIST
}).

-record(neuron_data, {
	acc = 0,			% accumulator (Z)
	weights = #{},		% input weights (W)
	bias = 0,			% bias (B)
	activation = ?ACTIVATION_FUNCTION,	% activation function (A)
	remInPIDs = [],		% remaining input neuron PIDs. Change during the calculation (because of deleting)
	origInPIDs = [],	% original input neuron PIDs. Doesn't change
	outPIDs = []		% output neuron PIDs
}).
