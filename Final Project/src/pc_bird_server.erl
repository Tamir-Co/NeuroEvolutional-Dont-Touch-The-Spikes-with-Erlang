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
-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2]).

%% ====================================
start(PC_Name) ->
	gen_server:start({local, PC_Name}, ?MODULE, [PC_Name], []).

%% ====================================
init([PC_Name]) ->
	{ok, #pc_bird_server_state{
		pcName = PC_Name,
		graphicState = idle,
		birdList = []
	}}.

% build a new & unique bird FSM
create_bird_FSM_name(PC_Name) -> list_to_atom("bird_FSM_" ++ atom_to_list(PC_Name) ++ integer_to_list(erlang:unique_integer())).

handle_call(_Request, _From, _State) ->
	erlang:error(not_implemented).

handle_cast({start_bird_FSM, NumOfBirds, GraphicState, SpikesList}, State=#pc_bird_server_state{pcName = PC_Name, birdList = BirdList}) ->
	NewBirdList = start_bird_FSM(NumOfBirds, PC_Name, SpikesList, BirdList, GraphicState),
	wx_object:cast(graphics, {finish_init_birds, self(), GraphicState}),		% tell graphics the PC finished to all start_bird_FSMs
	io:format("finish_init_birds!~n"),
	{noreply, State#pc_bird_server_state{graphicState=GraphicState, birdList = NewBirdList}};

handle_cast({start_simulation}, State=#pc_bird_server_state{birdList = BirdList}) ->
	msg_all_birds(BirdList, {start_simulation}, true),
	{noreply, State};

handle_cast({spikes_list, SpikesList}, State=#pc_bird_server_state{birdList = BirdList}) ->
	msg_all_birds(BirdList, {spikes_list, SpikesList}, false),
	{noreply, State};

handle_cast({jump}, State=#pc_bird_server_state{birdList = BirdList, graphicState = play_user}) ->
	gen_statem:cast(hd(BirdList), {jump}),
	{noreply, State};

handle_cast({simulate_frame}, State=#pc_bird_server_state{birdList = BirdList}) ->
	gen_statem:cast(hd(BirdList), {simulate_frame}),
	{noreply, State};

handle_cast({bird_location, X, Y, Direction}, State=#pc_bird_server_state{}) ->
	wx_object:cast(graphics, {bird_location, X, Y, Direction}),
	{noreply, State};

handle_cast({bird_disqualified, BirdPID}, State=#pc_bird_server_state{graphicState=GraphicState, birdList = BirdList}) ->
	wx_object:cast(graphics, {bird_disqualified, BirdPID}),
	{noreply, State#pc_bird_server_state{graphicState = check_state(BirdList, GraphicState), birdList = BirdList -- [BirdPID]}}.

%% ====================================
%% Start all bird FSMs and return the new bird list
start_bird_FSM(0, _PC_Name, _SpikesList, BirdList, _GraphicState) -> BirdList;
start_bird_FSM(NumOfBirds, PC_Name, SpikesList, BirdList, GraphicState) ->
	{ok, BirdPID} = bird_FSM:start(create_bird_FSM_name(PC_Name), self(), SpikesList, GraphicState),
	start_bird_FSM(NumOfBirds-1, PC_Name, SpikesList, BirdList ++ [BirdPID], GraphicState).

%% Send message/cast to all birds
msg_all_birds([], _Msg, _IsMsg) -> done;
msg_all_birds([Bird|Bird_T], Msg, IsMsg) ->
	case IsMsg of
		true -> Bird ! Msg, io:format("Msg: ~p to bird:~p~n", [Msg,Bird]);
		false-> gen_statem:cast(Bird, Msg)
	end,
	msg_all_birds(Bird_T, Msg, IsMsg).

%% Check system state by the amount of running birds
check_state(BirdList, GraphicState) ->
	case length(BirdList) of
		0 	  -> idle;		% no birds
		_Else -> GraphicState	% there are birds
	end.
