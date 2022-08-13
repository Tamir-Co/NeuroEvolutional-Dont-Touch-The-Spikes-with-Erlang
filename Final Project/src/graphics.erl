%%%-------------------------------------------------------------------
%%% @author Nadav & Tamir
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2022 19:01
%%%-------------------------------------------------------------------
-module(graphics).
-author("Nadav & Tamir").

%% API
-include("constants.hrl").
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-export([start/0]).
-export([init/1, handle_event/2, handle_sync_event/3, handle_info/2, handle_cast/2]).
-export([sound/0]).

-define(SERVER, ?MODULE).


%% =================================================================
start() ->
	wx_object:start({local,?SERVER}, ?MODULE, [], []).

init(_Args) ->
	process_flag(trap_exit, true),
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel = wxPanel:new(Frame, [{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	wxPanel:setBackgroundColour(Panel, {235,235,235}),
	
	Brush = wxBrush:new({128,128,128}, [{style, 100}]),
	
	ButtonStartUser = wxButton:new(Frame, ?ButtonStartUserID, [{label, "Start (user)"}]),
	ButtonStartNEAT = wxButton:new(Frame, ?ButtonStartNEATID, [{label, "Start (NEAT)"}]),
	ButtonJump = wxButton:new(Frame, ?ButtonJumpID, [{label, "Jump"}]),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	UiSizer = wxBoxSizer:new(?wxHORIZONTAL),
	StartSizer = wxBoxSizer:new(?wxVERTICAL),
	JumpSizer = wxBoxSizer:new(?wxVERTICAL),
	
	wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
	wxSizer:add(StartSizer, ButtonStartUser,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
	wxSizer:add(StartSizer, ButtonStartNEAT,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
	wxSizer:add(JumpSizer, ButtonJump,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
	wxSizer:add(UiSizer, JumpSizer),
	wxSizer:add(UiSizer, StartSizer),
	wxSizer:add(MainSizer, UiSizer),
	
	wxSizer:hide(UiSizer, JumpSizer, []),
	wxSizer:layout(MainSizer),
	wxPanel:setSizer(Frame, MainSizer),
	wxSizer:setSizeHints(MainSizer, Frame),
	
	%TxtSizer = wxBoxSizer:new(?wxVERTICAL),
	TxtScore = wxStaticText:new(Panel, -1, "Score: 0\nBest score: 0", [{style, ?wxALIGN_CENTRE}]),	%{size, {100, 100}}, 
	FontScore = wxFont:new(16, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
	wxStaticText:setFont(TxtScore, FontScore),
	%wxStaticText:setBackgroundColour(TxtScore, {200,200,200}),
	%wxSizer:add(TxtSizer, TxtScore, [{flag, ?wxALIGN_CENTRE}, {border, 5}]),	%{flag, ?wxLEFT bor ?wxTOP bor ?wxRIGHT bor ?wxEXPAND}
	%wxPanel:setSizer(Panel, TxtSizer),
	%wxSizer:setSizeHints(TxtSizer, Panel),
	
	ImageBG = wxImage:new("images/background.png", []),
	BitmapBG = wxBitmap:new(wxImage:scale(ImageBG, ?BG_WIDTH, ?BG_HEIGHT, [{quality, ?wxIMAGE_QUALITY_HIGH}])),
%%	StaticBitmapBG = wxStaticBitmap:new(Panel, 1, BitmapBG),
	
	ImageBird_R = wxImage:new("images/bird_RIGHT.png", []),
	ImageBird_L = wxImage:mirror(ImageBird_R),	% ImageBird_L = wxImage:new("images/bird_LEFT.png", []),
	BitmapBird_R = wxBitmap:new(wxImage:scale(ImageBird_R, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
	BitmapBird_L = wxBitmap:new(wxImage:scale(ImageBird_L, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
%%	StaticBitmapBird = wxStaticBitmap:new(Panel, 1, BitmapBird),

	wxFrame:show(Frame),
	erlang:send_after(?TIMER, self(), timer),

	wxPanel:connect(Panel, paint, [callback]),
	wxFrame:connect(Frame, close_window),
	wxButton:connect(ButtonStartUser, command_button_clicked),
	wxButton:connect(ButtonStartNEAT, command_button_clicked),
	wxButton:connect(ButtonJump, command_button_clicked),

	%% spawn sound maker process
	register(sound_proc, spawn(?MODULE, sound, [])),

	% Init bird servers and split the work
	{BirdUserPID, BirdServerPID} = init_system(),

	{Frame, #graphics_state{
		frame = Frame,
		panel = Panel,
		brush = Brush,
		textScore = TxtScore,
		mainSizer = MainSizer,
		uiSizer = UiSizer,
		startSizer = StartSizer,
		jumpSizer = JumpSizer,
		bitmapBG = BitmapBG,
		bitmapBird_R = BitmapBird_R,
		bitmapBird_L = BitmapBird_L,
		birdUser = #bird{},
		birdUserPID = BirdUserPID,
		birdList = [],
		curr_state = idle,
		spikesList = init_spike_list(),
		pcList = [BirdServerPID],
		pcsInSimulation = 1     % TODO change to define - the length of PCs list
	}}.

%% =================================================================
handle_cast({finish_init_birds, _PC_PID, CurrState}, State=#graphics_state{pcList = PC_List})->
	case CurrState of
%%		play_user -> gen_server:cast(hd(PC_List), {start_simulation});		% we can now goto start simulation
		play_NEAT -> todo,
					 gen_server:cast(hd(PC_List), {start_simulation})		% we can now goto start simulation
	end,
	{noreply, State#graphics_state{curr_state=CurrState}};	% only after all birds had initialized, the graphics_state changes its state.

handle_cast({bird_location, X, Y, Direction}, State=#graphics_state{birdUser=Bird})->
	NewBird = Bird#bird{x=X, y=Y, direction=Direction},
	NewState = State#graphics_state{birdUser=NewBird},
	{noreply, NewState};

handle_cast({user_bird_disqualified}, State=#graphics_state{curr_state = CurrState})->
	NewState = case CurrState of
				   play_user -> sound_proc ! "lose_trim",
					   			State#graphics_state{curr_state=idle, birdUser=#bird{}, bird_x=?BIRD_START_X, bird_direction=r, spikesAmount=?INIT_SPIKES_WALL_AMOUNT}
			   end,
	{noreply, NewState};

handle_cast({pc_finished_simulation, CandBirds}, State=#graphics_state{curr_state=CurrState, pcList=PC_List, pcsInSimulation=PCsInSimulation, bestCandBirds=BestCandBirds})->
	NewState = case CurrState of
				   play_NEAT -> case PCsInSimulation of     % how many PCs are running (birds) simulation now
									1 ->
										FinalBestCandBirds = merge_birds(BestCandBirds, CandBirds),
										send_best_birds(FinalBestCandBirds, PC_List),
										State#graphics_state{pcsInSimulation=length(PC_List), bestCandBirds=[]};
									_Else ->
										NewBestCandBirds = merge_birds(BestCandBirds, CandBirds),      % merge the sorted birds received from PC with current birds
										State#graphics_state{pcsInSimulation=PCsInSimulation-1, bestCandBirds=NewBestCandBirds}
				                end
			   end,
	{noreply, NewState}.

%% =================================================================
%% We reach here each button press
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{mainSizer=MainSizer, uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, 
																							  pcList = PC_List, curr_state = CurrState, bird_x=_Bird_x, bird_direction=_Bird_dir,
																							  spikesList = SpikesList, score=Score, bestScore=BestScore, birdUserPID=BirdUserPID}) ->
	NewState = case ID of
		?ButtonStartUserID ->
			wxSizer:hide(UiSizer, StartSizer, []),
			wxSizer:show(UiSizer, JumpSizer, []),
			wxSizer:layout(MainSizer),
			BirdUserPID ! {start_simulation},
			State#graphics_state{score=0, bestScore=max(BestScore, Score), curr_state=play_user};
		
		?ButtonStartNEATID ->
			wxSizer:hide(UiSizer, StartSizer, []),
			wxSizer:layout(MainSizer),
			gen_server:cast(hd(PC_List), {start_bird_FSM, play_NEAT, SpikesList}),
			State#graphics_state{score=0};
		
		?ButtonJumpID ->
			case CurrState of
				play_user ->
					sound_proc ! "jump_trim",
				    gen_statem:cast(BirdUserPID, {jump}),
				    State;
				
				play_NEAT ->
					State;
				
				idle ->
					State
			end
	end,
	{noreply, NewState};

% closing window event
handle_event(#wx{event = #wxClose{}},State = #graphics_state {frame = Frame}) -> % close window event
	io:format("Exiting\n"),
	wxWindow:destroy(Frame),
	wx:destroy(),
	unregister(sound_proc),
	{stop,normal,State};

%% We reach here each key_down event
handle_event(#wx{id=_ID, event=#wxCommand{type=Type}}, State) ->
	io:format("~n~nevent key down: ~p~n", [Type]),
	NewState = State,
	{noreply, NewState}.

%% =================================================================
%% We reach here each timer event
handle_info(timer, State=#graphics_state{uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, mainSizer=MainSizer, frame=Frame, pcList=PC_List, birdUserPID=BirdUserPID,
										 bird_x=Bird_x, bird_direction=Bird_dir, spikesList=SpikesList, curr_state=CurrState, score=Score, spikesAmount=SpikesAmount}) ->  % refresh screen for graphics
	wxWindow:refresh(Frame), % refresh screen
	NewState = case CurrState of
				  idle		->  wxSizer:hide(UiSizer, JumpSizer, []),
								wxSizer:show(UiSizer, StartSizer, []),
								wxSizer:layout(MainSizer),
								State#graphics_state{birdUser=#bird{}, bird_x=?BIRD_START_X, bird_direction=r, spikesAmount=?INIT_SPIKES_WALL_AMOUNT, spikesList=init_spike_list()};	%, score=0

				  play_user -> 	gen_statem:cast(BirdUserPID, {simulate_frame}),
								{NewDirection, NewX, Has_changed_dir} = simulate_x_movement(Bird_x, Bird_dir),
								case Has_changed_dir of
									true  -> sound_proc ! "bonus_trim",
											 NewSpikesList = create_spikes_list(trunc(SpikesAmount)),
											 gen_statem:cast(BirdUserPID, {spikes_list, NewSpikesList}),	%SpikesList,
											 NewSpikesAmount = min(SpikesAmount + ?ADD_SPIKES_WALL_TOUCH, ?MAX_RATIONAL_SPIKES_AMOUNT),      % TODO
											 NewScore = Score + 1;
									false -> NewSpikesList = SpikesList,
											 NewSpikesAmount = SpikesAmount,
											 NewScore = Score
								end,
								State#graphics_state{bird_x=NewX, bird_direction=NewDirection, spikesList=NewSpikesList, score=NewScore, spikesAmount=NewSpikesAmount};
								
				  play_NEAT	->  todo,      % TODO hd(PC_List) all code!!
					            gen_server:cast(hd(PC_List), {simulate_frame}),
					            {NewDirection, NewX, Has_changed_dir} = simulate_x_movement(Bird_x, Bird_dir),
					            case Has_changed_dir of
									  true  ->
										  NewSpikesList = create_spikes_list(trunc(SpikesAmount)),
										  gen_server:cast(hd(PC_List), {spikes_list, NewSpikesList}),	%SpikesList,
										  NewSpikesAmount = min(SpikesAmount + ?ADD_SPIKES_WALL_TOUCH, ?MAX_RATIONAL_SPIKES_AMOUNT),      % TODO
										  NewScore = Score + 1;
									  false ->
										  NewSpikesList = SpikesList,
										  NewSpikesAmount = SpikesAmount,
										  NewScore = Score
					            end,
					            State#graphics_state{bird_x=NewX, bird_direction=NewDirection, spikesList=NewSpikesList, score=NewScore, spikesAmount=NewSpikesAmount}
			  end,

	erlang:send_after(?TIMER, self(), timer),	% set new timer
	{noreply, NewState};

handle_info(#wx{event=#wxClose{}}, State) ->
	{stop, normal, State}.

%% =================================================================
handle_sync_event(_Event, _, _State=#graphics_state{curr_state=CurrState, spikesList=SpikesList, panel=Panel, brush=Brush, bitmapBG=_BitmapBG, bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L,
													birdUser=#bird{y=Y}, bird_x=X, bird_direction=Direction, birdList=BirdList, score=Score, bestScore=BestScore, textScore=TxtScore}) ->
	
	DC = wxPaintDC:new(Panel),
	%wxDC:drawBitmap(DC, BitmapBG, {0, 0}),
	BitmapBird = case Direction of
		r -> BitmapBird_R;
		l -> BitmapBird_L
	end,
	
	case CurrState of
		idle	  -> wxDC:drawBitmap(DC, BitmapBird, {X, Y});
		play_user -> wxDC:drawBitmap(DC, BitmapBird, {X, Y});
		play_NEAT -> draw_birds(DC, BitmapBird, BirdList)
	end,
	
	wxStaticText:setLabel(TxtScore, "Score: " ++ integer_to_list(Score) ++ "\nBest score: " ++ integer_to_list(BestScore)),
	
	wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
	wxDC:setBrush(DC, Brush),
	wxDC:drawRectangle(DC, {0, 0, ?BG_WIDTH, ?TOP_RECT_HEIGHT}),
	wxDC:drawRectangle(DC, {0, ?BOTTOM_RECT_Y, ?BG_WIDTH, ?BOTTOM_RECT_HEIGHT}),
	draw_top_bottom_spikes(DC, ?SPIKES_LEFT_X, ?TOP_BOTTOM_SPIKES_AMOUNT),
	draw_wall_spikes(DC, SpikesList, ?SPIKES_TOP_Y, Direction),
	
	wxPaintDC:destroy(DC);
handle_sync_event(_Event, _, State) ->
	{noreply, State}.
%%
%%terminate(_Reason, State = #graphics_state{}) ->
%%	wxFrame:destroy(State#graphics_state.frame).

%% =================================================================
%% Simulates a x movement of a bird during one frame.
%% the output is: {NewDirection, NewX, Has_changed_direction}
simulate_x_movement(X, Direction) ->
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{r, _    , true } -> {l        , X - ?X_VELOCITY, true };
		{r, _    , false} -> {Direction, X + ?X_VELOCITY, false};
		{l, true , _    } -> {r        , X + ?X_VELOCITY, true };
		{l, false, _    } -> {Direction, X - ?X_VELOCITY, false}
	end.


init_spike_list() -> lists:map(fun(_) -> 0 end, lists:seq(1,?MAX_SPIKES_AMOUNT)).

%% Wrap function. Creates a random spikes list with exactly TotalSpikes spikes
create_spikes_list(TotalSpikes) ->
	create_spikes_list(init_spike_list(), 0, TotalSpikes).


%% Creates a random spikes list with exactly TotalSpikes spikes
create_spikes_list(SpikesList, TotalSpikes, TotalSpikes) -> SpikesList;
create_spikes_list(SpikesList, SpikesCreated, TotalSpikes) ->
	SpikeIdx = rand:uniform(?MAX_SPIKES_AMOUNT-SpikesCreated),	% [1,10] -> [1,1]
	create_spikes_list(insert_spike(SpikesList, SpikeIdx), SpikesCreated+1, TotalSpikes).


%% Insert the spike to the list at the given index.
%% Skip 1's at the inserting
insert_spike(SpikesList, 1) -> [1|SpikesList--[0]];	% insert the spike
insert_spike([IsSpike|Spikes_T], SpikeIdx) ->
	case IsSpike of
		1 -> [IsSpike|insert_spike(Spikes_T, SpikeIdx)];	% skip
		0 -> [IsSpike|insert_spike(Spikes_T, SpikeIdx-1)]	% don't skip
	end.


%%create_spikeList() ->
%%	SpikeProb = 50,
%%	% SpikesList = lists:filter(rand:uniform(100) < SpikeProb, lists:seq(1,?MAX_SPIKES_AMOUNT)),
%%	% SpikesList = [rand:uniform(100) < SpikeProb || _ <- lists:seq(1,?MAX_SPIKES_AMOUNT)],
%%%%	SpikesList = [1,0,1,0,1,0,1,0,1,0],
%%	SpikesList = lists:map(fun(_) -> case rand:uniform(100) < SpikeProb of true -> 1; false -> 0 end end, lists:seq(1,?MAX_SPIKES_AMOUNT)),
%%	SpikesList.


draw_birds(_, _, []) -> ok;
draw_birds(DC, BitmapBird, [#bird{x=X,y=Y}|BirdList]) ->
	wxDC:drawBitmap(DC, BitmapBird, {X, Y}),
	draw_birds(DC, BitmapBird, BirdList).


draw_wall_spikes(_, [], _, _) -> ok;
draw_wall_spikes(DC, [IsSpike|SpikesList_Tail], CurrSpike_Y, Direction) ->
	case IsSpike of
		1 -> 
			case Direction of
				r -> wxDC:drawPolygon(DC, [{?BG_WIDTH, CurrSpike_Y}, {?BG_WIDTH-?SPIKE_HEIGHT, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {?BG_WIDTH, CurrSpike_Y+?SPIKE_WIDTH}]);
				l -> wxDC:drawPolygon(DC, [{0, CurrSpike_Y}, {?SPIKE_HEIGHT, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {0, CurrSpike_Y+?SPIKE_WIDTH}])
			end;
		0 -> no_spike
	end,
	draw_wall_spikes(DC, SpikesList_Tail, CurrSpike_Y+?SPIKE_WIDTH+?SPIKE_GAP_Y, Direction).


draw_top_bottom_spikes(_, _, 0) -> ok;
draw_top_bottom_spikes(DC, CurrSpike_X, Spikes_amount) ->
	Center_of_spike = CurrSpike_X + ?SPIKE_HALF_WIDTH,
	End_of_spike_X = CurrSpike_X + ?SPIKE_WIDTH,
	wxDC:drawPolygon(DC, [{CurrSpike_X, ?TOP_RECT_HEIGHT}, {Center_of_spike, ?TOP_RECT_HEIGHT+?SPIKE_HEIGHT}, {End_of_spike_X, ?TOP_RECT_HEIGHT}]),
	wxDC:drawPolygon(DC, [{CurrSpike_X, ?BOTTOM_RECT_Y}, {Center_of_spike, ?BOTTOM_RECT_Y-?SPIKE_HEIGHT}, {End_of_spike_X, ?BOTTOM_RECT_Y}]),
	draw_top_bottom_spikes(DC, End_of_spike_X+?SPIKE_GAP_X, Spikes_amount-1).


%% Merge CandBirds with BestCandBirds and return ?100? best birds. A bird performs better when it stays alive for more frames.
merge_birds(BestCandBirds, CandBirds) ->
	merge_birds(BestCandBirds, CandBirds, [], ?NUM_OF_SURVIVED_BIRDS).	% we only choose ceil(0.1*N) of all birds

merge_birds(_, _, BestCandBirds, 0) -> BestCandBirds;
merge_birds([Bird1|CandBirds1], [Bird2|CandBirds2], BestCandBirds, BirdsToChoose) ->
	{_BirdPID1, {FrameCount1, _WeightsMap1}} = Bird1,
	{_BirdPID2, {FrameCount2, _WeightsMap2}} = Bird2,
	case FrameCount1 < FrameCount2 of
		true  -> merge_birds([Bird1|CandBirds1], CandBirds2, BestCandBirds ++ [Bird2], BirdsToChoose-1);
		false -> merge_birds(CandBirds1, [Bird2|CandBirds2], BestCandBirds ++ [Bird1], BirdsToChoose-1)
	end.

% merge_birds(_, _, BestCandBirds, 0) -> BestCandBirds;
% merge_birds([{BirdPID1, FrameCount1}|CandBirds1], [{BirdPID2, FrameCount2}|CandBirds2], BestCandBirds, BirdsToChoose) when FrameCount2 < FrameCount1 ->
	% merge_birds(CandBirds1, [{BirdPID2, FrameCount2}|CandBirds2], BestCandBirds ++ [{BirdPID1, FrameCount1}], BirdsToChoose-1);
% merge_birds([{BirdPID1, FrameCount1}|CandBirds1], [{BirdPID2, FrameCount2}|CandBirds2], BestCandBirds, BirdsToChoose) ->
	% merge_birds([{BirdPID1, FrameCount1}|CandBirds1], CandBirds2, BestCandBirds ++ [{BirdPID2, FrameCount2}], BirdsToChoose-1).


%% Notify all PCs about their best birds. A bird performs better when it stays alive for more frames.
send_best_birds([], _PC_List) -> finish;
%%send_best_birds([{BirdPID, {FrameCount, WeightsMap}}|CandBirdsT], PC_List) ->
send_best_birds(BestCandBirds, PC_List) ->
	hd(PC_List) ! BestCandBirds.

%% =================================================================
init_system() ->
	{ok, BirdUserPID} = bird_FSM:start(create_bird_FSM_name(graphics), self(), init_spike_list(), idle),    % the graphics owns the user bird
	
	PC_Name = list_to_atom("pc1_" ++ integer_to_list(erlang:unique_integer())),
	{ok, BirdServerPID} = pc_bird_server:start(PC_Name, ?NUM_OF_BIRDS),	% init pc bird server. TODO divide by ?4?
	{BirdUserPID, BirdServerPID}.

% build a new & unique bird FSM
create_bird_FSM_name(PC_Name) -> list_to_atom("bird_FSM_" ++ atom_to_list(PC_Name) ++ integer_to_list(erlang:unique_integer())).

%% os:cmd("aplay sounds/lose.wav")
sound() ->
	receive
		SoundName -> SoundName%os:cmd("aplay sounds/" ++ SoundName ++ ".wav")
	end,
	sound().

