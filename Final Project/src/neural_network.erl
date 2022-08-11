%%%-------------------------------------------------------------------
%%% @author Tamir & Nadav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Aug 2022 21:21
%%%-------------------------------------------------------------------
-module(neural_network).
-author("Tamir & Nadav").
-include("constants.hrl").

%% API
-export([init/1]).

%% =================================================================
init(NetworkStructure) ->
%%	N_PID_LayerMap = construct_network(NetworkStructure),
	loop(#nn_data{networkStructure=NetworkStructure}).

loop(NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsMap=WeightsMap}) ->
	receive
		{set_weights, NewWeightsMap} ->
				todo,
				loop(NN_Data#nn_data{weightsMap=NewWeightsMap});

		{decide_jump, From, BirdHeight, BirdWallDistance, SpikesList} ->    % TODO send fewer SpikesList
				case decide_jump(NN_Data, BirdHeight, BirdWallDistance, SpikesList) of
					true  -> gen_statem:cast(From, {jump});
					false -> void   % don't jump
				end,
				loop(NN_Data);

		{get_weights, From} ->
				todo,
				From ! WeightsMap,
				loop(NN_Data)
	end.

%% Feed-forward the input game data, and using the nn_data to determine whether to jump right now
decide_jump(_NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsMap=_WeightsMap},
			_BirdHeight, BirdWallDistance, _SpikesList) ->
	Self = self(),
	Weights = #{Self=>0.1},
	N_PID = spawn_link(fun() -> neuron:init() end),   % TODO move to init
	N_PID ! {configure_neuron, Weights, -19.8, tanh, [Self], [Self]},
	N_PID ! {neuron, Self, BirdWallDistance},
	receive
		{neuron, N_PID, IsJump} ->
				io:format("BirdWallDistance: ~p,IsJump: ~p~n", [BirdWallDistance, IsJump]),
				case IsJump > 0.5 of
					true  -> true;    % jump
					false -> false
				end
	end.
