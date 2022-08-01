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

-define(ButtonStartID, 10).
-define(ButtonJumpID, 11).

-define(Timer, 100).		% Graphics update timer
-define(TIME_UNIT, 1).

%% RECORDS %%
-record(graphics_state, {frame, panel, bitmapBG, bitmapBird, bird}).
-record(bird, {x, y, velocityY, direction}).
