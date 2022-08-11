%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2022 13:17
%%%-------------------------------------------------------------------
-author("Nadav & Tamir").

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

-define(SPIKES_LEFT_X, 22).
-define(SPIKE_GAP_X, 4).
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

-define(NUM_OF_BIRDS, 1).    % TODO 1000 or other number

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
		bird,	% TODO change to birdList
		bird_x = ?BIRD_START_X,
		bird_direction = r,
		score = 0,
		bestScore = 0,
		curr_state,
		spikesList,
		spikesAmount = 1,
		pcList
}).
%graphics_state has: main window, panel, background and bird images, bird record, and atom curr_state (idle, play_user, play_NEAT)

-record(pc_bird_server_state, {
	pcName,
	graphicState = idle,
	birdList
}).

-record(bird, {
		x = ?BIRD_START_X,
		y = ?BIRD_START_Y,
		velocityY = 0,
		direction = r,
		pc_pid,
		spikesList,
		nnPID,
		graphicState
}).

-record(nn_data, {
		networkStructure,   % [L1, L2, ...]
		weightsList
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

