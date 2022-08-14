%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2022 18:01
%%%-------------------------------------------------------------------
-module(pc_bird_server).
-author("Nadav & Tamir").
-behaviour(gen_server).
-include("constants.hrl").

%% API
-export([start/2]).
-export([init/1, handle_call/3, handle_cast/2]).

%% =================================================================
start(PC_Name, NumOfPcBirds) ->
	gen_server:start({local, PC_Name}, ?MODULE, [PC_Name, NumOfPcBirds], []).

%% =================================================================
init([PC_Name, NumOfPcBirds]) ->
	{ok, #pc_bird_server_state{
		pcName = PC_Name,
		graphicState = idle,
		numOfPcBirds = NumOfPcBirds,
		numOfAliveBirds = 0,
		birdsMap = #{}
	}}.


% build a new & unique bird FSM
create_bird_FSM_name(PC_Name) -> list_to_atom("bird_FSM_" ++ atom_to_list(PC_Name) ++ integer_to_list(erlang:unique_integer())).


handle_call(_Request, _From, _State) ->
	erlang:error(not_implemented).


handle_cast({start_bird_FSM, GraphicState, SpikesList}, State=#pc_bird_server_state{pcName=PC_Name, birdsMap=BirdsMap, numOfPcBirds=NumOfPcBirds}) ->
	io:format("NumOfPcBirds ~p ~n", [NumOfPcBirds]),
	NewBirdsMap = start_bird_FSM(NumOfPcBirds, PC_Name, SpikesList, BirdsMap, GraphicState),
	wx_object:cast(graphics, {finish_init_birds, self(), GraphicState}),		% tell graphics the PC finished to all start_bird_FSMs
	io:format("~n~n~nfinish_init_birds!~n~n~n"),
	{noreply, State#pc_bird_server_state{graphicState=GraphicState, birdsMap=NewBirdsMap}};

handle_cast({start_simulation}, State=#pc_bird_server_state{birdsMap=BirdsMap, numOfPcBirds=NumOfPcBirds}) ->
	msg_all_birds(maps:keys(BirdsMap), {start_simulation}, true),
	{noreply, State#pc_bird_server_state{numOfAliveBirds=NumOfPcBirds}};

handle_cast({spikes_list, SpikesList}, State=#pc_bird_server_state{birdsMap = BirdsMap}) ->
	msg_all_birds(maps:keys(BirdsMap), {spikes_list, SpikesList}, false),
	{noreply, State};

handle_cast({simulate_frame}, State=#pc_bird_server_state{birdsMap = BirdsMap}) ->  % TODO send only to alive birds
%%	gen_statem:cast(hd(BirdList), {simulate_frame}),
	msg_all_birds(maps:keys(BirdsMap), {simulate_frame}, false),
	{noreply, State};

handle_cast({bird_location, X, Y, Direction}, State=#pc_bird_server_state{}) ->
	wx_object:cast(graphics, {bird_location, X, Y, Direction}),
	{noreply, State};

handle_cast({bird_disqualified, BirdPID, FrameCount, WeightsMap}, State=#pc_bird_server_state{birdsMap=BirdsMap, numOfAliveBirds=NumOfAliveBirds}) ->
	NewBirdsMap = BirdsMap#{BirdPID := {FrameCount, WeightsMap}},
	case NumOfAliveBirds of
		1 ->
			SortedBirds = lists:keysort(1, maps:values(NewBirdsMap)),           % all birds are dead now, send them sorted (by frame count) to graphics
			{_, CandBirds} = lists:split(?NUM_OF_SURVIVED_BIRDS, SortedBirds),  % take only the ?100? best birds
			wx_object:cast(graphics, {pc_finished_simulation, CandBirds});
		_Else ->
			ok
	end,
	{noreply, State#pc_bird_server_state{birdsMap=NewBirdsMap, numOfAliveBirds=NumOfAliveBirds-1}};

handle_cast({populate_next_gen, WeightsMapList}, State=#pc_bird_server_state{birdsMap=BirdsMap}) ->
	create_mutations_and_send(maps:keys(BirdsMap), WeightsMapList),    % create mutations and send the new weights to the birds
	{noreply, State#pc_bird_server_state{}}.

%% =================================================================
%% Start all bird FSMs and return the new bird list
start_bird_FSM(0, _PC_Name, _SpikesList, BirdsMap, _GraphicState) -> BirdsMap;
start_bird_FSM(NumOfPcBirds, PC_Name, SpikesList, BirdsMap, GraphicState) ->
	{ok, BirdPID} = bird_FSM:start(create_bird_FSM_name(PC_Name), self(), SpikesList, GraphicState),
	start_bird_FSM(NumOfPcBirds-1, PC_Name, SpikesList, BirdsMap#{BirdPID => {0, #{}}}, GraphicState).


%% Send message/cast to all birds
msg_all_birds([], _Msg, _IsMsg) -> done;
msg_all_birds([Bird|Bird_T], Msg, IsMsg) ->
	case IsMsg of
		true -> Bird ! Msg;
		false-> gen_statem:cast(Bird, Msg)
	end,
	msg_all_birds(Bird_T, Msg, IsMsg).


%% Receive Bird PID list and weights map list.
%% Mutate each weight map 9 times and keep 1 copy, then send it to the birds
create_mutations_and_send(_BirdsListPID, []) -> finish;
create_mutations_and_send([BirdPID|BirdsListPID_T], [WeightsMap|WeightsMapListT]) ->
	BirdPID ! {replace_genes, WeightsMap},     % keep each "brain" once
	RemainingPIDs = mutate_brain_and_send(BirdsListPID_T, WeightsMap, trunc(1/?PERCENT_SURVIVED_BIRDS) - 1),  % mutate the "brain" ?9? times
	create_mutations_and_send(RemainingPIDs, WeightsMapListT).  % mutate the remaining brains

%% Mutate each weight map ?9? times, then send it to the birds.
mutate_brain_and_send(BirdsListPID, _WeightsMap, 0) -> BirdsListPID;
mutate_brain_and_send([BirdPID|BirdsListPID_T], WeightsMap, NumOfMutations) ->
	MutatedWeightsMap = mutate_brain(WeightsMap),
	?PRINT('MutatedWeightsMap PC!!!!', MutatedWeightsMap),
	BirdPID ! {replace_genes, MutatedWeightsMap},
	mutate_brain_and_send(BirdsListPID_T, WeightsMap, NumOfMutations-1).

%% Mutate a weight map (brain) randomly.
%% WeightsMap = #{ {weight, LeftNeuronPID, RightNeuronPID} => Weight, {bias, NeuronPID} => Bias }.
mutate_brain(WeightsMap) ->
	?PRINT('FNJDONFDJOFN WeightsMap', WeightsMap),
	Fun = fun({bias, _}, W)		 -> W + (rand:uniform() - 0.5) / ?MUTATION_BIAS_FACTOR;   
			 ({weight, _, _}, W) -> W + (rand:uniform() - 0.5) / ?MUTATION_WEIGHT_FACTOR
		  end,
	maps:map(Fun, WeightsMap).
	% MutateBiasesWeightsMap = maps:from_list([ {Key, maps:get(Key, WeightsMap) + ((rand:uniform())-0.5)/?MUTATION_BIAS_FACTOR} || Key={bias, _NeuronPID} <- maps:keys(WeightsMap)]),
	% MutateWeightsWeightsMap = maps:from_list([ {Key, maps:get(Key, WeightsMap) + ((rand:uniform())-0.5)/?MUTATION_WEIGHT_FACTOR} || Key={weight, _LeftNeuronPID, _RightNeuronPID} <- maps:keys(WeightsMap)]),
	% maps:merge(MutateBiasesWeightsMap, MutateWeightsWeightsMap).
	
