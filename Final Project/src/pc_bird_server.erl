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
			curr_state = idle,
			birdList = []
	}}.

% build a new & unique bird FSM
create_bird_FSM_name(PC_Name) -> list_to_atom("bird_FSM_" ++ atom_to_list(PC_Name) ++ integer_to_list(erlang:unique_integer())).

handle_call(_Request, _From, _State) ->
	erlang:error(not_implemented).

handle_cast({start_bird_FSM, 0}, State=#pc_bird_server_state{pcName = PC_Name}) ->
	wx_object:cast(graphics, {finish_init_birds, PC_Name}),		% tell graphics the PC finished to all start_bird_FSMs
	{noreply, State};
handle_cast({start_bird_FSM, NumOfBirds}, State=#pc_bird_server_state{pcName = PC_Name, birdList = BirdList}) ->
	{ok, BirdPID} = bird_FSM:start_bird_FSM(create_bird_FSM_name(PC_Name), self()),
	handle_cast({start_bird_FSM, NumOfBirds-1}, State#pc_bird_server_state{birdList = BirdList ++ [BirdPID]});

handle_cast({start_simulation}, State=#pc_bird_server_state{birdList = BirdList}) ->
	msg_all_birds(BirdList, {start_simulation}),
	{noreply, State};

handle_cast({jump}, State=#pc_bird_server_state{birdList = BirdList}) ->
	gen_statem:cast(hd(BirdList), {jump}),
	{noreply, State};

handle_cast({simulate_frame}, State=#pc_bird_server_state{birdList = BirdList}) ->
	gen_statem:cast(hd(BirdList), {simulate_frame}),
	{noreply, State};

handle_cast({bird_location, X, Y, Direction}, State=#pc_bird_server_state{}) ->
	wx_object:cast(graphics, {bird_location, X, Y, Direction}),
	{noreply, State}.
%% ====================================
msg_all_birds([], _Msg) -> done;
msg_all_birds([Bird|Bird_T], Msg) ->
	Bird ! Msg,
	msg_all_birds(Bird_T, Msg).

