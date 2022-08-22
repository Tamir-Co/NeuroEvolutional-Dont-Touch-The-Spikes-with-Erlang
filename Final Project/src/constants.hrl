%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2022 13:17
%%%-------------------------------------------------------------------
-author("Nadav & Tamir").

%% TODO change this!
%% 1 Terminal:
-define(GRAPHICS_NODE,  'pc0@Nadav-VirtualBox').
-define(PC1_NODE,       'pc0@Nadav-VirtualBox').
-define(PC2_NODE,       'pc0@Nadav-VirtualBox').
-define(PC3_NODE,       'pc0@Nadav-VirtualBox').
-define(PC4_NODE,       'pc0@Nadav-VirtualBox').

%% 5 Terminals:
%-define(GRAPHICS_NODE,  'pc0@Nadav-VirtualBox').
%-define(PC1_NODE,       'pc1@Nadav-VirtualBox').
%-define(PC2_NODE,       'pc2@Nadav-VirtualBox').
%-define(PC3_NODE,       'pc3@Nadav-VirtualBox').
%-define(PC4_NODE,       'pc4@Nadav-VirtualBox').

%% ====================================================================

%% PC names are always unique
-define(PC1_NAME, pc1).
-define(PC2_NAME, pc2).
-define(PC3_NAME, pc3).
-define(PC4_NAME, pc4).

-define(PC_NAMES, [?PC1_NAME, ?PC2_NAME, ?PC3_NAME, ?PC4_NAME]).
-define(PC_NODES, [?PC1_NODE, ?PC2_NODE, ?PC3_NODE, ?PC4_NODE]).
-define(PC_NAME_TO_NODE(PC_Name), maps:get(PC_Name, #{ ?PC1_NAME => ?PC1_NODE, ?PC2_NAME => ?PC2_NODE, ?PC3_NAME => ?PC3_NODE, ?PC4_NAME => ?PC4_NODE })).
-define(INIT_PC_AMOUNT, length(?PC_NODES)).

%% ====================================================================

%% Print define
-define(PRINT(Text, Arg), io:format(atom_to_list(Text) ++ " ~p~n", [Arg])).
-define(PRINT(Text), io:format(atom_to_list(Text) ++ "~n")).
-define(PRINT(),io:format("~n")).

%% ====================================================================

%% Frame structure:
-define(BG_WIDTH, 400).
-define(BG_HEIGHT, 600).

%% ====================================================================

%% Spikes:
-define(INIT_SPIKE_LIST, [0 || _ <- lists:seq(1, ?MAX_SPIKES_AMOUNT)]). % list of zeros
-define(MAX_SPIKES_AMOUNT, 7).                                          % size of the spikes list
-define(MAX_RATIONAL_SPIKES_AMOUNT, (?MAX_SPIKES_AMOUNT-2)).            % maximum playable spike amount on the wall

-define(ADD_SPIKES_WALL_TOUCH, 1).      % add spike every wall touch
-define(INIT_SPIKES_WALL_AMOUNT, 1).    % initial spike amount

-define(SPIKE_WIDTH, 42).
-define(SPIKE_HEIGHT, 25).
-define(SPIKE_HEIGHT_4, 8).
-define(SPIKE_HALF_WIDTH, 21).
-define(SPIKE_GAP_Y, 10).
-define(SPIKES_TOP_Y, 120).

-define(TOP_BOTTOM_SPIKES_AMOUNT, 7).
-define(SPIKES_LEFT_X, 20).
-define(SPIKE_GAP_X, 11).
-define(SPIKES_BOTTOM_Y, (?BOTTOM_RECT_Y-?SPIKE_HEIGHT)).
-define(TOP_RECT_HEIGHT, 70).
-define(BOTTOM_RECT_Y, 530).
-define(BOTTOM_RECT_HEIGHT, 70).

%% ====================================================================

%% Bird:
-define(BIRD_WIDTH, 50).
-define(BIRD_HEIGHT, 34).

-define(GRAVITY, 2).
-define(JUMP_VELOCITY, 15).
-define(X_VELOCITY, 7).
-define(BIRD_START_X, 180).
-define(BIRD_START_Y, 270).

%% TODO you may change to this values: {PCs, NUM_OF_BIRDS, PERCENT_SURVIVED_BIRDS} = {4, 100, 0.2}, {4, 400, 0.2}, {4, 1000, 0.2}
-define(NUM_OF_BIRDS, 400).
-define(PERCENT_SURVIVED_BIRDS, 0.2).                                           % how many birds are survived after each generation (in %)
-define(NUM_OF_SURVIVED_BIRDS, ceil(?NUM_OF_BIRDS*?PERCENT_SURVIVED_BIRDS)).    % how many birds are survived after each generation


-define(FRAMES_BETWEEN_DECIDE_JUMP, 4).     % the bird doesn't ask the NN whether to jump in each frame, to speed-up the game

%% ====================================================================

%% Neural network:
-define(NN_STRUCTURE, [2+?MAX_SPIKES_AMOUNT, 6, 6, 6, 1]).		% This is the structure of the network. The first and last number from the list are the in/outputs layers.

-define(BIAS_RANGE, 50).				% used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_FACTOR, 10).			% used in division of the range [-0.5,0.5] to a smaller range
-define(MUTATION_MAX_RAND_VAL, 20).		% used to define the probability of a edge deletion (W=0)

-define(INPUT_ACTIVATION_FUNCTION, identity).   % for the input neurons
-define(ACTIVATION_FUNCTION, relu).             % for the rest of the neurons. possible values are: [relu, tanh, sign, identity]

%% ====================================================================

%% UI buttons IDs:
-define(ButtonStartUserID, 10).
-define(ButtonStartNEATID, 11).
-define(ButtonJumpID, 12).

%% ====================================================================

%% Time:
-define(TIMER_NEAT, 80).    % Graphics update timer at NEAT mode
-define(TIMER_USER, 60).    % Graphics update timer at user mode
-define(TIMEOUT, 500).		% for im_alive messages
-define(TIME_UNIT, 1).      % for units to make sense

%% ====================================================================

%% Records:
-record(graphics_state, {
	frame,
	panel,
	brush,
	textScore,
	textBirdsPerPC,
	mainSizer,
	uiSizer,
	startSizer,
	jumpSizer,
	bitmapBird_R,
	bitmapBird_L,
	birdUser,
	birdUserPID,
	birdList = sets:new(),      % for drawing birds its enough to hold 1 instance of each different Y value
	locatedBirdsAmount = 0,     % how many birds have sent their location
	numOfAliveBirds = 0,        % how many birds are currently at simulation state
	bird_x = ?BIRD_START_X,     % synced between all birds. They just have to send their Y value
	bird_direction = r,         % synced between all birds. initial direction is right (possible values are l/r)
	bestCandBirds = [],         % sorted list by frame count. The list holds the best brains (NN weight list)
	score = 0,
	bestScore = 0,
	curr_state = idle,
	spikesList = ?INIT_SPIKE_LIST,
	spikesAmount = ?INIT_SPIKES_WALL_AMOUNT,
	recvACKsPCsNamesList = [],  % list of the PCs that sent im_alive
	alivePCsNamesList = ?PC_NAMES,
	waitForPCsAmount,           % how many PCs do the graphics wait for, e.g for ending simulation state
	birdsPerPcMap = maps:from_list([{PC_Name, trunc(?NUM_OF_BIRDS / ?INIT_PC_AMOUNT)} || PC_Name <- ?PC_NAMES]),    % alive birds amount per PC
	timeCount = 0,              % for alive messages
	genNum = 1                  % generation number
}).

-record(pc_bird_server_state, {
	pcName,
	numOfPcBirds,       % amount of PC birds ?250?
	numOfAliveBirds,    % amount of birds in simulation right now
	listOfAliveBirds,   % list of PIDs of current alive (in simulation) birds
	birdsMap,           % #{PID => {frameCount, WeightsMap}}
	locatedBirdsAmount = 0
}).

-record(bird, {
	x = ?BIRD_START_X,
	y = ?BIRD_START_Y,
	velocityY = 0,
	direction = r,      % initial direction is right (possible values are l/r)
	pcPID,
	spikesList = ?INIT_SPIKE_LIST,
	nnPID,
	graphicState = idle,
	frameCount = 0,     % how many frames the bird survives
	framesToDecide = 0  % amount of frames between request a jump
}).

-record(nn_data, {
	networkStructure,   % [L1, L2, ...]
	n_PIDsLayersMap,    % #{ {layer, 0} => [nnPID], {layer, 1} => [Neuron1PID, Neuron2PID, ...], ... }
	weightsMap,         % #{ {1, weight, PID1, PID2} => Weight, {2, bias, PID} => Bias, ... }
	spikesList = ?INIT_SPIKE_LIST,
	n_PIDsList,         % list of neuron's PIDs
	birdPID
}).

-record(neuron_data, {
	acc = 0,			% accumulator (Z)
	weights = #{},		% input weights (W)
	bias = 0,			% bias (B)
	activation = ?ACTIVATION_FUNCTION,	% activation function (A)
	remInPIDs = 0,		% remaining input neuron PIDs. Change during the calculation (because of deleting)
	origInPIDs = [],	% original input neuron PIDs. Doesn't change
	outPIDs = []		% output neuron PIDs
}).
