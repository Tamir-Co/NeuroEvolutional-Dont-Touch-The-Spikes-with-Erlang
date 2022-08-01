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

%% API
-export([init/1, callback_mode/0]).



init(_Args) ->
	erlang:error(not_implemented).

callback_mode() ->
	erlang:error(not_implemented).