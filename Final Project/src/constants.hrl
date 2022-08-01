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

-define(SPIKE_WIDTH, 30).
-define(SPIKE_LENGTH, 30).

-define(JUMP_VELOCITY, 10).
-define(BIRD_START_X, 180).
-define(BIRD_START_Y, 320).

-define(ButtonStartUserID, 10).
-define(ButtonStartNEATID, 11).
-define(ButtonJumpID, 12).

-define(Timer, 100).		% Graphics update timer
-define(TIME_UNIT, 1).

%% RECORDS %%
-record(graphics_state, {frame, panel, bitmapBG, bitmapBird, bird, curr_state}).
%graphics_state has: main window, panel, background and bird images, bird record, and atom curr_state (idle, play_user, play_NEAT)
-record(bird, {x, y, velocityY, direction}).
