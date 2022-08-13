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
	NewBirdsMap = start_bird_FSM(NumOfPcBirds, PC_Name, SpikesList, BirdsMap, GraphicState),
	wx_object:cast(graphics, {finish_init_birds, self(), GraphicState}),		% tell graphics the PC finished to all start_bird_FSMs
	io:format("finish_init_birds!~n"),
	{noreply, State#pc_bird_server_state{graphicState=GraphicState, birdsMap=NewBirdsMap}};

handle_cast({start_simulation}, State=#pc_bird_server_state{birdsMap=BirdsMap, numOfPcBirds=NumOfPcBirds}) ->
	msg_all_birds(maps:keys(BirdsMap), {start_simulation}, true),
	{noreply, State#pc_bird_server_state{numOfAliveBirds=NumOfPcBirds}};

handle_cast({spikes_list, SpikesList}, State=#pc_bird_server_state{birdsMap = BirdsMap}) ->
	msg_all_birds(maps:keys(BirdsMap), {spikes_list, SpikesList}, false),
	{noreply, State};

handle_cast({simulate_frame}, State=#pc_bird_server_state{birdsMap = BirdsMap}) ->
%%	gen_statem:cast(hd(BirdList), {simulate_frame}),
	msg_all_birds(maps:keys(BirdsMap), {simulate_frame}, true),
	{noreply, State};

handle_cast({bird_location, X, Y, Direction}, State=#pc_bird_server_state{}) ->
	wx_object:cast(graphics, {bird_location, X, Y, Direction}),
	{noreply, State};

handle_cast({bird_disqualified, BirdPID, FrameCount}, State=#pc_bird_server_state{birdsMap=BirdsMap, numOfAliveBirds=NumOfAliveBirds}) ->
	NewBirdsMap = BirdsMap#{BirdPID := FrameCount},
	case NumOfAliveBirds of
		1 ->
			SortedBirds = lists:keysort(2, maps:to_list(NewBirdsMap)),          % all birds are dead now, send them sorted (by frame count) to graphics
			{_, CandBirds} = lists:split(?NUM_OF_SURVIVED_BIRDS, SortedBirds),  % take only the ?100? best birds
			wx_object:cast(graphics, {pc_finished_simulation, CandBirds});
		_Else ->
			ok
	end,
	{noreply, State#pc_bird_server_state{birdsMap=NewBirdsMap, numOfAliveBirds=NumOfAliveBirds-1}}.

%% =================================================================
%% Start all bird FSMs and return the new bird list
start_bird_FSM(0, _PC_Name, _SpikesList, BirdsMap, _GraphicState) -> BirdsMap;
start_bird_FSM(NumOfPcBirds, PC_Name, SpikesList, BirdsMap, GraphicState) ->
	{ok, BirdPID} = bird_FSM:start(create_bird_FSM_name(PC_Name), self(), SpikesList, GraphicState),
	start_bird_FSM(NumOfPcBirds-1, PC_Name, SpikesList, BirdsMap#{BirdPID => 0}, GraphicState).


%% Send message/cast to all birds
msg_all_birds([], _Msg, _IsMsg) -> done;
msg_all_birds([Bird|Bird_T], Msg, IsMsg) ->
	case IsMsg of
		true -> Bird ! Msg, io:format("Msg: ~p to bird:~p~n", [Msg,Bird]);
		false-> gen_statem:cast(Bird, Msg)
	end,
	msg_all_birds(Bird_T, Msg, IsMsg).
