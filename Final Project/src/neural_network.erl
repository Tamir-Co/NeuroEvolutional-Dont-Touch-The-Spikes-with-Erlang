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

%% ======================================
init(NetworkStructure) ->
	loop(#nn_data{networkStructure=NetworkStructure}).

loop(NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsList=WeightsList}) ->
	receive
		{set_weights, NewWeightsList} ->
				todo,
				loop(NN_Data#nn_data{weightsList=NewWeightsList});

		{decide_jump, From, BirdHeight, BirdWallDistance, SpikesList} ->    % TODO send fewer SpikesList
				case decide_jump(NN_Data, BirdHeight, BirdWallDistance, SpikesList) of
					true  -> gen_statem:cast(From, {jump});
					false -> void   % don't jump
				end,
				loop(NN_Data);

		{get_weights, From} ->
				todo,
				From ! WeightsList,
				loop(NN_Data)
	end.

%% Feed-forward the input game data, and using the nn_data to determine whether to jump right now
decide_jump(_NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsList=_WeightsList},
			_BirdHeight, BirdWallDistance, _SpikesList) ->
	Self = self(),
	Weights = #{Self=>0.1},
	N_PID = spawn_link(fun() -> neuron:init(Weights, -19.5, tanh, [Self], [Self]) end),   % TODO move to init
	N_PID ! {neuron, Self, BirdWallDistance},
	receive
		{neuron, N_PID, IsJump} ->
				io:format("BirdWallDistance: ~p,IsJump: ~p~n", [BirdWallDistance, IsJump]),
				case IsJump > 0.5 of
					true  -> true;    % jump
					false -> false
				end
	end.
