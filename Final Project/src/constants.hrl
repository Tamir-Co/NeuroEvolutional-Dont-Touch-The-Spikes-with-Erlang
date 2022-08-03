%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2022 13:17
%%%-------------------------------------------------------------------
-author("Nadav & Tamir").


-define(BG_WIDTH, 400).
-define(BG_HEIGHT, 700).

-define(MAX_SPIKES_AMOUNT, 10).
-define(SPIKE_WIDTH, 36).
-define(SPIKE_HALF_WIDTH, 18).
-define(SPIKE_LENGTH, 30).
-define(SPIKE_GAP, 13).
-define(SPIKES_TOP_Y, 100).
-define(SPIKES_BOTTOM_Y, ?BG_HEIGHT-100).

-define(BIRD_WIDTH, 50).
-define(BIRD_HEIGHT, 34).

-define(JUMP_VELOCITY, 15).
-define(X_VELOCITY, 5).
-define(BIRD_START_X, 180).
-define(BIRD_START_Y, 320).

-define(ButtonStartUserID, 10).
-define(ButtonStartNEATID, 11).
-define(ButtonJumpID, 12).

-define(Timer, 60).		% Graphics update timer
-define(TIME_UNIT, 1).

-define(NUM_OF_BIRDS, 1000).


%% RECORDS %%
-record(pc_bird_server_state, {
	name,
	birdList
}).

-record(graphics_state, {
		frame,
		panel,
		mainSizer,
		jumpSizer,
		bitmapBG,
		bitmapBird_R,
		bitmapBird_L,
		bird,	% TODO change to birdList
		curr_state,
		pcList
}).
%graphics_state has: main window, panel, background and bird images, bird record, and atom curr_state (idle, play_user, play_NEAT)

-record(bird, {
		x = ?BIRD_START_X,
		y = ?BIRD_START_Y,
		velocityY = 0,
		direction = right,
		pc_pid
}).
