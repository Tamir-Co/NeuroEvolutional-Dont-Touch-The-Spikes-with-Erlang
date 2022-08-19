%%%-------------------------------------------------------------------
%%% @author Tamir & Nadav
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Aug 2022 13:02
%%%-------------------------------------------------------------------
-module(neuron).
-author("Tamir & Nadav").
-include("constants.hrl").

%% API
-export([test/0, init/0]).

%% =================================================================
% A test function used to test the functionality of the neuron
test()->
	Self = self(),
	PID = spawn(fun()-> init() end),
	PID ! {configure_neuron, #{1=>0.1,2=>0.2,3=>0.3}, 7, tanh, [1,2,3], [Self]},
	PID ! {neuron, 3, 6},
	PID ! {neuron, 1, 7},
	PID ! {neuron, 2, 8},
	receive
		M -> io:format("M: ~p~n", [M])
	end.

init() ->
	loop(#neuron_data{}).

loop(_NeuronData = #neuron_data{acc=Acc, weights=Weights, bias=Bias, activation=Activation, origInPIDs=OrigInPIDs, remInPIDs=RemInPIDs, outPIDs=OutPIDs}) ->
	receive
		{configure_neuron, NewWeights, NewBias, NewActivation, NewInPIDs, NewOutPIDs} ->
				loop(#neuron_data{acc=0, weights=NewWeights, bias=NewBias, activation=NewActivation, origInPIDs=NewInPIDs, remInPIDs=length(NewInPIDs), outPIDs=NewOutPIDs});
		
		{neuron, From, A} ->
				W = maps:get(From, Weights),		% get the relevant input weight
				Z = Acc + W*A,
				case RemInPIDs > 1 of		% check if the neuron hasn't received all its inputs yet
					true  -> loop(#neuron_data{	acc=Z, weights=Weights, bias=Bias, activation=Activation,
												origInPIDs=OrigInPIDs, remInPIDs=RemInPIDs - 1, outPIDs = OutPIDs});
					false -> MyA = activation_func(Activation, Z + Bias),
							 [OutPID ! {neuron, self(), MyA} || OutPID <- OutPIDs],
							 loop(#neuron_data{	acc=0, weights=Weights, bias=Bias, activation=Activation,
								 				origInPIDs=OrigInPIDs, remInPIDs=length(OrigInPIDs), outPIDs = OutPIDs})
				end
		end.

%% =================================================================
activation_func(tanh, Z) -> math:tanh(Z);

activation_func(identity, Z) -> Z;

activation_func(relu, Z) -> case 0 < Z of
								true  -> Z;
								false -> 0
							end;

activation_func(sign, 0)-> 0;
activation_func(sign, Z)->
	case Z > 0 of
		true  ->  1;
		false -> -1
	end.

