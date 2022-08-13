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
	{N_PIDsList, N_PIDsLayersMap} = construct_NN(NetworkStructure),
	WeightsMap = rand_weights(N_PIDsLayersMap),
%%	io:format("WeightsMap ~p~n~n", [WeightsMap]),
%%	io:format("self ~p~n", [self()]),
	configure_NN(WeightsMap, N_PIDsList),
%%	io:format("N_PIDsLayersMap ~p~n", [N_PIDsLayersMap]),
	loop(#nn_data{networkStructure=NetworkStructure, weightsMap=WeightsMap, n_PIDsLayersMap=N_PIDsLayersMap, n_PIDsList=N_PIDsList}).

loop(NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsMap=WeightsMap, n_PIDsList=N_PIDsList}) ->
	receive
		{set_weights, NewWeightsMap} ->
				configure_NN(NewWeightsMap, N_PIDsList),
				loop(NN_Data#nn_data{weightsMap=NewWeightsMap});

		{decide_jump, From, BirdHeight, BirdWallDistance, SpikesList} ->    % TODO send fewer SpikesList
				case decide_jump(NN_Data, BirdHeight, BirdWallDistance, SpikesList) of
					true  -> gen_statem:cast(From, {jump});
					false -> void   % don't jump
				end,
				loop(NN_Data);

		{get_weights, From} ->
				From ! WeightsMap,
				loop(NN_Data)
	end.

%% Construct the neural network and return a map which holds all the neuron's PIDs by layers
construct_NN(NetworkStructure) ->
	construct_NN(NetworkStructure, #{}, [], 1).

construct_NN([], N_PIDsLayersMap, N_PIDsList, _LayerIdx) -> { N_PIDsList, N_PIDsLayersMap#{{layer, 0} => [self()]} };     % add the NN module to the 0 layer for later convenient
construct_NN([NeuronsAmount|NetworkStructureT], N_PIDsLayersMap, N_PIDsList, LayerIdx) ->
	PIDs = construct_neurons(NeuronsAmount, []),
	construct_NN(NetworkStructureT, N_PIDsLayersMap#{ {layer, LayerIdx} => PIDs }, N_PIDsList ++ PIDs, LayerIdx+1).

%% Create NeuronsAmount neurons and return list of their PIDs
construct_neurons(0, PIDs) -> PIDs;
construct_neurons(NeuronsAmount, PIDs) ->
	construct_neurons(NeuronsAmount-1, [spawn_link(fun() -> neuron:init() end)|PIDs]).

%% Create random weights and biases
rand_weights(N_PIDsLayersMap) ->
	rand_weights(N_PIDsLayersMap, #{}, 1, length(?NN_STRUCTURE)+1).

rand_weights(_N_PIDsLayersMap, WeightsMap, NumOfLayers_1, NumOfLayers_1) -> WeightsMap;
rand_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NumOfLayers_1) ->
	NewWeightsMap = rand_layer_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, lists:nth(LayerIdx+1, [1|?NN_STRUCTURE])),  % add (temporary) the NN module to the NN structure
	rand_weights(N_PIDsLayersMap, NewWeightsMap, LayerIdx+1, NumOfLayers_1).

rand_layer_weights(_N_PIDsLayersMap, WeightsMap, _LayerIdx, 0) -> WeightsMap;
rand_layer_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx) ->
	NewWeightsMap = rand_neuron_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, lists:nth(LayerIdx, [1|?NN_STRUCTURE])),
	NewNewWeightsMap = rand_neuron_bias(N_PIDsLayersMap, NewWeightsMap, LayerIdx, NeuronsIdx),
	rand_layer_weights(N_PIDsLayersMap, NewNewWeightsMap, LayerIdx, NeuronsIdx-1).

rand_neuron_weights(_N_PIDsLayersMap, WeightsMap, _LayerIdx, _NeuronsIdx, 0) -> WeightsMap;
rand_neuron_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, PrevLayerNeuronsIdx) ->
	Weight = rand:uniform(),    % [0,1)
	RightNeuronPID = lists:nth(NeuronsIdx, maps:get({layer, LayerIdx}, N_PIDsLayersMap)),
	LeftNeuronPID  = lists:nth(PrevLayerNeuronsIdx, maps:get({layer, LayerIdx-1}, N_PIDsLayersMap)),
	NewWeightsMap = WeightsMap#{ {weight, LeftNeuronPID, RightNeuronPID} => Weight },   % add weight to the map
	rand_neuron_weights(N_PIDsLayersMap, NewWeightsMap, LayerIdx, NeuronsIdx, PrevLayerNeuronsIdx-1).

rand_neuron_bias(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx) ->
	Bias = rand:uniform() - 0.5,    % [-0.5,0.5)
	NeuronPID = lists:nth(NeuronsIdx, maps:get({layer, LayerIdx}, N_PIDsLayersMap)),
	WeightsMap#{ {bias, NeuronPID} => Bias }.

%% Configure all the neurons within the neural network using the WeightsMap
configure_NN(_WeightsMap, []) -> void;
configure_NN(WeightsMap, [LastNeuronPID]) ->
	Weights = extract_weights(WeightsMap, LastNeuronPID),
	Bias    = maps:get({bias, LastNeuronPID}, WeightsMap),
	InPIDs  = [LeftNeuronPID  || {weight, LeftNeuronPID,  RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == LastNeuronPID],
	OutPIDs = [self()],
	LastNeuronPID ! {configure_neuron, Weights, Bias, tanh, InPIDs, OutPIDs};
configure_NN(WeightsMap, [NeuronPID|NeuronPID_T]) ->
	Weights = extract_weights(WeightsMap, NeuronPID),
	Bias    = maps:get({bias, NeuronPID}, WeightsMap),
	InPIDs  = [LeftNeuronPID  || {weight, LeftNeuronPID,  RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == NeuronPID],
	OutPIDs = [RightNeuronPID || {weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), LeftNeuronPID == NeuronPID],
	NeuronPID ! {configure_neuron, Weights, Bias, tanh, InPIDs, OutPIDs},
	configure_NN(WeightsMap, NeuronPID_T).

%% Extract from WeightsMap the relevant weights that inputs to NeuronPID, and return them in a map
extract_weights(WeightsMap, NeuronPID) ->   % WeightsMap = #{ {weight, LeftNeuronPID, RightNeuronPID} => Weight }
	WeightsMapNeuron = #{},
	maps:from_list([{ LeftNeuronPID, maps:get(Key, WeightsMap) } || Key={weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == NeuronPID])
	% #{ LeftNeuronPID => Weight }
	.

%% Feed-forward the input game data, and using the nn_data to determine whether to jump right now
decide_jump(_NN_Data = #nn_data{networkStructure=_NetworkStructure, n_PIDsLayersMap=N_PIDsLayersMap},
			BirdHeight, BirdWallDistance, SpikesList) ->
	feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList),
	OutputNeuronPID = hd(maps:get({layer, length(?NN_STRUCTURE)}, N_PIDsLayersMap)),
	receive
		{neuron, OutputNeuronPID, IsJump} ->
				io:format("BirdWallDistance: ~p,IsJump: ~p~n", [BirdWallDistance, IsJump]),
				case IsJump > 0 of
					true  -> true;    % jump
					false -> false
				end
	end.

%% Feed the neural network with its inputs
feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList) ->
	Layer1Len = length(maps:get({layer, 1}, N_PIDsLayersMap)),
	Self = self(),
	lists:nth(Layer1Len, maps:get({layer, 1}, N_PIDsLayersMap)) ! {neuron, Self, BirdHeight},           % insert BirdHeight
	lists:nth(Layer1Len-1, maps:get({layer, 1}, N_PIDsLayersMap)) ! {neuron, Self, BirdWallDistance},   % insert BirdWallDistance
	feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList, Layer1Len-2).
	
feed_inputs(_N_PIDsLayersMap, _BirdHeight, _BirdWallDistance, _SpikesList, 0) -> ok;
feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList, Amount) ->
	Self = self(),
	lists:nth(Amount, maps:get({layer, 1}, N_PIDsLayersMap)) ! {neuron, Self, lists:nth(Amount, SpikesList)},
	feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList, Amount-1).
