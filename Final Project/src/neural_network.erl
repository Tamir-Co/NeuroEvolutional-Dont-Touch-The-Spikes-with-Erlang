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
-export([init/2]).

%% =================================================================
init(NetworkStructure, BirdPID) ->
	{N_PIDsList, N_PIDsLayersMap} = construct_NN(NetworkStructure),
	WeightsMap = rand_weights(N_PIDsLayersMap),
	configure_NN(WeightsMap, N_PIDsList, 1),
	loop(#nn_data{networkStructure=NetworkStructure, weightsMap=WeightsMap, n_PIDsLayersMap=N_PIDsLayersMap, n_PIDsList=N_PIDsList, birdPID=BirdPID}).


%% This is the main loop. this module acts according to actions it gets as messages.
loop(NN_Data = #nn_data{networkStructure=_NetworkStructure, weightsMap=WeightsMap, spikesList=SpikesList, n_PIDsList=N_PIDsList, birdPID=BirdPID}) ->
	receive
		{decide_jump, BirdHeight, BirdWallDistance} ->
				case decide_jump(NN_Data, BirdHeight, BirdWallDistance, SpikesList) of
					true  -> gen_statem:cast(BirdPID, {jump}); % bird jump
%%					true  -> BirdPID ! {jump, FrameCount}; % bird jump TODO delete
					false -> ok                           % bird doesn't jump
				end,
				loop(NN_Data);
		
		{spikes_list, NewSpikesList}  ->
				loop(NN_Data#nn_data{spikesList=NewSpikesList});
		
		{set_weights, NewWeightsList} ->
				NewWeightsMap = set_weights(NewWeightsList, WeightsMap),
				configure_NN(NewWeightsMap, N_PIDsList, 1),
				loop(NN_Data#nn_data{weightsMap=NewWeightsMap});


		{get_weights} -> % TODO delete this:, From
				SortedWeightsByIdx = lists:sort(fun(Edge1, Edge2) -> ordering(Edge1, Edge2) end, maps:to_list(WeightsMap)),
				{_Edges, Weights} = lists:unzip(SortedWeightsByIdx),
				BirdPID ! {weights_list, Weights},  % send the bird its "brain" (weights) as a list
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


%% Create random weights and biases for the NN
rand_weights(N_PIDsLayersMap) ->
	rand_weights(N_PIDsLayersMap, #{}, 1, length(?NN_STRUCTURE)+1, 1).

rand_weights(_N_PIDsLayersMap, WeightsMap, NumOfLayers_1, NumOfLayers_1, _Idx) -> WeightsMap;
rand_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NumOfLayers_1, Idx) ->
	{NewWeightsMap, NewIdx} = rand_layer_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, lists:nth(LayerIdx+1, [1|?NN_STRUCTURE]), Idx),  % add (temporary) the NN module to the NN structure
	rand_weights(N_PIDsLayersMap, NewWeightsMap, LayerIdx+1, NumOfLayers_1, NewIdx).

%% Create random weights for a specific layer in the NN
rand_layer_weights(_N_PIDsLayersMap, WeightsMap, _LayerIdx, 0, Idx) -> {WeightsMap, Idx};
rand_layer_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, Idx) ->
	{NewWeightsMap, NewIdx} = rand_neuron_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, lists:nth(LayerIdx, [1|?NN_STRUCTURE]), Idx),
	NewNewWeightsMap = rand_neuron_bias(N_PIDsLayersMap, NewWeightsMap, LayerIdx, NeuronsIdx, NewIdx),
	rand_layer_weights(N_PIDsLayersMap, NewNewWeightsMap, LayerIdx, NeuronsIdx-1, NewIdx+1).

%% Create random weights for a specific neuron
rand_neuron_weights(_N_PIDsLayersMap, WeightsMap, _LayerIdx, _NeuronsIdx, 0, Idx) -> {WeightsMap, Idx};
rand_neuron_weights(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, PrevLayerNeuronsIdx, Idx) ->
	Weight = random_weight(Idx),
	RightNeuronPID = lists:nth(NeuronsIdx, maps:get({layer, LayerIdx}, N_PIDsLayersMap)),
	LeftNeuronPID  = lists:nth(PrevLayerNeuronsIdx, maps:get({layer, LayerIdx-1}, N_PIDsLayersMap)),
	NewWeightsMap = WeightsMap#{ {Idx, weight, LeftNeuronPID, RightNeuronPID} => Weight },   % add weight to the NN map
	rand_neuron_weights(N_PIDsLayersMap, NewWeightsMap, LayerIdx, NeuronsIdx, PrevLayerNeuronsIdx-1, Idx+1).


%% Gets an index of an edge and returning a random weight
random_weight(Idx) ->
	case Idx =< hd(?NN_STRUCTURE) * 2 of    % check if the Idx-th edge belongs to the first layer (input neuron)
		true -> 1;							% don't change the weight of an input
		false ->							% change the weight of an edge randomly
			case rand:uniform(?MUTATION_MAX_RAND_VAL) of
				1 -> 0;                     % resets the weight to 0
				_ -> rand:uniform() - 0.5   % random new weight
			end
	end.

%% Gets an index of an edge and returning a random bias
rand_neuron_bias(N_PIDsLayersMap, WeightsMap, LayerIdx, NeuronsIdx, Idx) ->
	case Idx =< hd(?NN_STRUCTURE) * 2 of    % check if the Idx-th edge belongs to the first layer (input neuron)
		true  -> Bias = 0;
		false -> Bias = (rand:uniform() - 0.5) * ?BIAS_RANGE    % [-0.5,0.5) * 50
	end,
	NeuronPID = lists:nth(NeuronsIdx, maps:get({layer, LayerIdx}, N_PIDsLayersMap)),
	WeightsMap#{ {Idx, bias, NeuronPID} => Bias }.   % add bias to the NN map


%% Configure all the neurons within the neural network using the WeightsMap
configure_NN(_WeightsMap, [], _N_Idx) -> void;
configure_NN(WeightsMap, [LastNeuronPID], _N_Idx) ->
	Weights = maps:from_list([{ LeftNeuronPID, maps:get(Key, WeightsMap) } || Key={_Idx, weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == LastNeuronPID]),
	Pred = fun({_Idx, bias, PID}, _V) when PID == LastNeuronPID -> true; (_,_) -> false end,
	Bias    = hd(maps:values(maps:filter(Pred,WeightsMap))),
	InPIDs  = [LeftNeuronPID  || {_Idx, weight, LeftNeuronPID,  RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == LastNeuronPID],
	OutPIDs = [self()],
	
	LastNeuronPID ! {configure_neuron, Weights, Bias, ?ACTIVATION_FUNCTION, InPIDs, OutPIDs};
configure_NN(WeightsMap, [NeuronPID|NeuronPID_T], N_Idx) ->
	Weights = maps:from_list([{ LeftNeuronPID, maps:get(Key, WeightsMap) } || Key={_Idx, weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == NeuronPID]),
	Pred = fun({_Idx, bias, PID}, _V) when PID == NeuronPID -> true; (_,_) -> false end,
	Bias    = hd(maps:values(maps:filter(Pred,WeightsMap))),
	InPIDs  = [LeftNeuronPID  || {_Idx, weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), RightNeuronPID == NeuronPID],
	OutPIDs = [RightNeuronPID || {_Idx, weight, LeftNeuronPID, RightNeuronPID} <- maps:keys(WeightsMap), LeftNeuronPID  == NeuronPID],
	
	case N_Idx =< hd(?NN_STRUCTURE) of    % ?12? first neurons (input neurons) has identity af
		true  -> NeuronPID ! {configure_neuron, Weights, Bias, ?INPUT_ACTIVATION_FUNCTION, InPIDs, OutPIDs};
		false -> NeuronPID ! {configure_neuron, Weights, Bias, ?ACTIVATION_FUNCTION, InPIDs, OutPIDs}
	end,
	
	configure_NN(WeightsMap, NeuronPID_T, N_Idx+1).


%% Feed-forward the input game data, and using the nn_data to determine whether to jump right now
decide_jump(_NN_Data = #nn_data{networkStructure=_NetworkStructure, n_PIDsLayersMap=N_PIDsLayersMap},
			BirdHeight, BirdWallDistance, SpikesList) ->
	feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList),
	OutputNeuronPID = hd(maps:get({layer, length(?NN_STRUCTURE)}, N_PIDsLayersMap)),
	receive_NN_output(OutputNeuronPID).
	

receive_NN_output(OutputNeuronPID) ->
	receive
		{neuron, OutputNeuronPID, IsJump} ->
%%				io:format("IsJump: ~p~n", [IsJump]),
				IsJump > 0;
		
		_Else ->    % delete other decide jump messages
				receive_NN_output(OutputNeuronPID)
	end.


%% Feed the neural network with its inputs
feed_inputs(N_PIDsLayersMap, BirdHeight, BirdWallDistance, SpikesList) ->
	Layer1Len = length(maps:get({layer, 1}, N_PIDsLayersMap)),
	Self = self(),
	lists:nth(Layer1Len, maps:get({layer, 1}, N_PIDsLayersMap)) ! {neuron, Self, BirdHeight},			% insert BirdHeight
	lists:nth(Layer1Len-1, maps:get({layer, 1}, N_PIDsLayersMap)) ! {neuron, Self, BirdWallDistance},	% insert BirdWallDistance
	feed_spike_list(maps:get({layer, 1}, N_PIDsLayersMap), SpikesList, ?MAX_SPIKES_AMOUNT).			% insert SpikesList

%% Insert spike list
feed_spike_list(_FirstLayerN_PIDs, _SpikesList, 0) -> ok;
feed_spike_list(FirstLayerN_PIDs, SpikesList, Idx) ->
	Self = self(),
	lists:nth(Idx, FirstLayerN_PIDs) ! {neuron, Self, lists:nth(Idx, SpikesList) * (?SPIKES_TOP_Y + Idx*(?SPIKE_WIDTH + ?SPIKE_GAP_Y))},
	feed_spike_list(FirstLayerN_PIDs, SpikesList, Idx-1).


%% returns a new (updated) WeightsMap
set_weights(NewWeightsList, WeightsMap) ->
	set_weights(NewWeightsList, lists:sort(fun(Edge1, Edge2) -> ordering(Edge1, Edge2) end, maps:to_list(WeightsMap)), #{}).

set_weights([], [], NewWeightsMap) -> NewWeightsMap;
set_weights([NewWeight|NewWeightsListT], [{{Idx, weight, L_PID, R_PID}, _OldWeight}|WeightsListT], NewWeightsMap) ->
	set_weights(NewWeightsListT, WeightsListT, NewWeightsMap#{ {Idx, weight, L_PID, R_PID} => NewWeight });
set_weights([NewWeight|NewWeightsListT], [{{Idx, bias, PID}, _OldWeight}|WeightsListT], NewWeightsMap) ->
	set_weights(NewWeightsListT, WeightsListT, NewWeightsMap#{ {Idx, bias, PID} => NewWeight }).


%% This function is used for sorting the weights map by edge index.
%% The inputs are {{Idx, 'bias', PID}, Weight} or {{Idx, 'weight', PID, PID}, Weight}
ordering({Edge1, _Val1}, {Edge2, _Val2}) ->
	element(1, Edge1) =< element(1, Edge2).

