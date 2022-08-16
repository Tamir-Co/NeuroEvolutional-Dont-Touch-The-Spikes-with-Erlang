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
		listOfAliveBirds = [],
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
	{noreply, State#pc_bird_server_state{graphicState=GraphicState, birdsMap=NewBirdsMap}};

handle_cast({start_simulation}, State=#pc_bird_server_state{birdsMap=BirdsMap, numOfPcBirds=NumOfPcBirds}) ->
	msg_to_birds(maps:keys(BirdsMap), {start_simulation}, true),
	{noreply, State#pc_bird_server_state{numOfAliveBirds=NumOfPcBirds, listOfAliveBirds=maps:keys(BirdsMap)}};

handle_cast({spikes_list, SpikesList}, State=#pc_bird_server_state{listOfAliveBirds=ListOfAliveBirds}) ->
	msg_to_birds(ListOfAliveBirds, {spikes_list, SpikesList}, false),
	{noreply, State};

handle_cast({simulate_frame}, State=#pc_bird_server_state{listOfAliveBirds=ListOfAliveBirds}) ->
	msg_to_birds(ListOfAliveBirds, {simulate_frame}, false),
	{noreply, State};

handle_cast({bird_location, Y}, State=#pc_bird_server_state{}) ->
	wx_object:cast(graphics, {bird_location, Y}),
	{noreply, State};

handle_cast({bird_disqualified, BirdPID, FrameCount, WeightsList}, State=#pc_bird_server_state{listOfAliveBirds=ListOfAliveBirds, birdsMap=BirdsMap, numOfAliveBirds=NumOfAliveBirds, numOfPcBirds=NumOfPcBirds}) ->
	wx_object:cast(graphics, {neat_bird_disqualified}),
	NewBirdsMap = BirdsMap#{BirdPID := {FrameCount, WeightsList}},  % update birds map
	NewListOfAliveBirds = ListOfAliveBirds -- [BirdPID],   % bird is dead, remove it from alive birds
	case NumOfAliveBirds of
		1 ->
			SortedBirds = lists:keysort(1, maps:values(NewBirdsMap)),           % all birds are dead now, send them sorted (by frame count) to graphics
%%			io:format("NumOfPcBirds-?NUM_OF_SURVIVED_BIRDS:   ~p\n", [NumOfPcBirds-?NUM_OF_SURVIVED_BIRDS]),
			{_, CandBirds} = lists:split(NumOfPcBirds-?NUM_OF_SURVIVED_BIRDS, SortedBirds),  % take only the ?100? best birds
			wx_object:cast(graphics, {pc_finished_simulation, self(), CandBirds});
		
		_Else ->
			ok
	end,
	{noreply, State#pc_bird_server_state{listOfAliveBirds=NewListOfAliveBirds, birdsMap=NewBirdsMap, numOfAliveBirds=NumOfAliveBirds-1}};

handle_cast({populate_next_gen, BestBrains}, State=#pc_bird_server_state{birdsMap=BirdsMap}) ->
	create_mutations_and_send(maps:keys(BirdsMap), BestBrains),    % create mutations and send the new weights to the birds
	wx_object:cast(graphics, {pc_finished_population, self()}),
	{noreply, State#pc_bird_server_state{}}.

%% =================================================================
%% Start all bird FSMs and return the new bird list
start_bird_FSM(0, _PC_Name, _SpikesList, BirdsMap, _GraphicState) -> BirdsMap;
start_bird_FSM(NumOfPcBirds, PC_Name, SpikesList, BirdsMap, GraphicState) ->
	{ok, BirdPID} = bird_FSM:start(create_bird_FSM_name(PC_Name), self(), SpikesList, GraphicState),
	start_bird_FSM(NumOfPcBirds-1, PC_Name, SpikesList, BirdsMap#{BirdPID => {0, []}}, GraphicState).


%% Send message/cast to specific birds
msg_to_birds([], _Msg, _IsMsg) -> done;
msg_to_birds([Bird|Bird_T], Msg, IsMsg) ->
	case IsMsg of
		true -> Bird ! Msg;
		false-> gen_statem:cast(Bird, Msg)%, ?PRINT(msg_to_birds, Msg)
	end,
	msg_to_birds(Bird_T, Msg, IsMsg).


%% Receive Bird PID list and weights map list.
%% Mutate each weight list 9 times and keep 1 copy, then send it to the birds
create_mutations_and_send([], []) -> finish;
create_mutations_and_send(BirdsListPID, [Brain|BrainT]) ->
%%	io:format("trunc(1/?PERCENT_SURVIVED_BIRDS)-1 :   ~p\n", [trunc(1/?PERCENT_SURVIVED_BIRDS) - 1]),
	RemainingPIDs = mutate_brain_and_send(BirdsListPID, Brain, trunc(1/?PERCENT_SURVIVED_BIRDS) - 1),  % mutate the "brain" ?9? times
	case RemainingPIDs of
		[] -> io:format("\nfinish\n"), finish;
		[BirdPID|BirdsListPID_T] ->
			BirdPID ! {replace_genes, Brain},     % keep each "brain" once
			create_mutations_and_send(BirdsListPID_T, BrainT)  % mutate the remaining brains
	end.

%% Mutate each weight list ?9? times, then send it to the birds.
mutate_brain_and_send(BirdsListPID, _Brain, 0) -> BirdsListPID;
mutate_brain_and_send([], _Brain, _NumOfMutations) -> [];
mutate_brain_and_send([BirdPID|BirdsListPID_T], Brain, NumOfMutations) ->
	{InputNeuronsWeights, NN_Weights} = lists:split(hd(?NN_STRUCTURE) * 2, Brain),
	MutatedBrain = InputNeuronsWeights ++ mutate_brain(NN_Weights),
	BirdPID ! {replace_genes, MutatedBrain},
	mutate_brain_and_send(BirdsListPID_T, Brain, NumOfMutations-1).


%% Mutate a weight list (brain) randomly.
%% WeightsMap = #{ {weight, LeftNeuronPID, RightNeuronPID} => Weight, {bias, NeuronPID} => Bias }.
mutate_brain(Brain) ->
	Fun =
		fun(W) ->
			case rand:uniform(?MUTATION_MAX_RAND_VAL) of
				1 -> 0;                                                 % disconnect edge (weight=0)
				2 -> rand:uniform()-0.5;                                % new weight
				_ -> W * (1 + (rand:uniform()-0.5) / ?MUTATION_FACTOR)  % change weight
			end
	    end,
	lists:map(Fun, Brain).
