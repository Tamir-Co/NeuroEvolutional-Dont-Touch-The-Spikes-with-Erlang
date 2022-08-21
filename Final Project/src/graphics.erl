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
-export([sound/0, graphics_rpc/1]).

-define(SERVER, ?MODULE).


%% =================================================================
start() ->
	wx_object:start({local,?SERVER}, ?MODULE, [], []).

init(_Args) ->
%%	process_flag(trap_exit, true),  % TODO
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
	
	TxtScore = wxStaticText:new(Panel, -1, "Score: 0\nBest score: 0", [{style, ?wxALIGN_CENTRE}]),
	FontScore = wxFont:new(13, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
	wxStaticText:setFont(TxtScore, FontScore),
	
	TxtBirdsPerPc = wxStaticText:new(Panel, -1, "", [{style, ?wxALIGN_LEFT}, {pos, {10, ?BOTTOM_RECT_Y+30}}]),
	FontBirdsPerPC = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
	wxStaticText:setFont(TxtBirdsPerPc, FontBirdsPerPC),
	
	ImageBird_R = wxImage:new("images/bird_RIGHT.png", []),
	ImageBird_L = wxImage:mirror(ImageBird_R),
	BitmapBird_R = wxBitmap:new(wxImage:scale(ImageBird_R, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
	BitmapBird_L = wxBitmap:new(wxImage:scale(ImageBird_L, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),

	wxFrame:show(Frame),
	erlang:send_after(?TIMER_NEAT, self(), timer),	% set new timer

	wxPanel:connect(Panel, paint, [callback]),
	wxFrame:connect(Frame, close_window),
	wxButton:connect(ButtonStartUser, command_button_clicked),
	wxButton:connect(ButtonStartNEAT, command_button_clicked),
	wxButton:connect(ButtonJump, command_button_clicked),

	%% spawn sound maker process
	register(sound_proc, spawn(?MODULE, sound, [])),
	
	% Init bird servers and split the work
	{BirdUserPID} = init_system(),
	cast_all_PCs(?PC_NAMES, {are_you_alive}),

	{Frame, #graphics_state{
		frame = Frame,
		panel = Panel,
		brush = Brush,
		textScore = TxtScore,
		textBirdsPerPC = TxtBirdsPerPc,
		mainSizer = MainSizer,
		uiSizer = UiSizer,
		startSizer = StartSizer,
		jumpSizer = JumpSizer,
		bitmapBird_R = BitmapBird_R,
		bitmapBird_L = BitmapBird_L,
		birdUser = #bird{},
		birdUserPID = BirdUserPID,
		curr_state = idle,
		spikesList = ?INIT_SPIKE_LIST,
		waitForPCsAmount = ?INIT_PC_AMOUNT
	}}.

%% =================================================================

%% A message from a PC that represents this PC is still alive and connected.
handle_cast({im_alive, PC_Name}, State=#graphics_state{recvACKsPCsNamesList=RecvACKsPCsNamesList})->
	io:format("~p is alive!~n", [PC_Name]),
	{noreply, State#graphics_state{recvACKsPCsNamesList = RecvACKsPCsNamesList ++ [PC_Name]}};

%% A message from a PC when it finished 'start_bird_FSM' (in play_NEAT mode)
handle_cast({finish_init_birds, _PC_PID, _CurrState}, State=#graphics_state{waitForPCsAmount=WaitForPCsAmount, alivePCsNamesList=AlivePCsNamesList})->
	case WaitForPCsAmount of
		1 ->
			NewBirdsPerPcMap = maps:from_list([{PC_Name, trunc(?NUM_OF_BIRDS / ?INIT_PC_AMOUNT)} || PC_Name <- AlivePCsNamesList]),
			?PRINT(finish_init, NewBirdsPerPcMap),
			cast_all_PCs(AlivePCsNamesList, {start_simulation}),
			cast_all_PCs(AlivePCsNamesList, {simulate_frame}),
			{NewDirection, NewX, _Has_changed_dir} = simulate_x_movement(?BIRD_START_X, r),
			{noreply, State#graphics_state{ curr_state=play_NEAT_simulation, waitForPCsAmount=length(AlivePCsNamesList),
											bird_x=NewX, bird_direction=NewDirection, numOfAliveBirds=?NUM_OF_BIRDS, birdsPerPcMap=NewBirdsPerPcMap}};	% only after all birds had initialized, the graphics_state changes its state.
		
		_Else ->
			{noreply, State#graphics_state{waitForPCsAmount=WaitForPCsAmount-1}}
	end;

%% A message from a PC of one of its birds with his new Y location when he finish simulate_frame (in play_NEAT mode)
handle_cast({neat_bird_location, Y}, State=#graphics_state{curr_state=play_NEAT_simulation, locatedBirdsAmount=LocatedBirdsAmount, birdList=BirdList})->
	NewState = State#graphics_state{birdList=sets:add_element(Y, BirdList), locatedBirdsAmount=LocatedBirdsAmount+1},
	{noreply, NewState};

%% A message from a bird with his new Y location when he finish simulate_frame (in play_user mode)
handle_cast({user_bird_location, Y}, State)->
	{noreply, State#graphics_state{birdUser=#bird{y=Y}}};

%% A message from a PC that represents that one of its bird died (in play_NEAT mode)
handle_cast({neat_bird_disqualified, PC_Name}, State=#graphics_state{curr_state=play_NEAT_simulation, numOfAliveBirds=NumOfAliveBirds, birdsPerPcMap=BirdsPerPcMap})->
	NewBirdsPerPcMap = BirdsPerPcMap#{ PC_Name := maps:get(PC_Name, BirdsPerPcMap) - 1 },
	{noreply, State#graphics_state{numOfAliveBirds=NumOfAliveBirds-1, birdsPerPcMap=NewBirdsPerPcMap}};

%% A message from a bird when it dies (in play_user mode)
handle_cast({user_bird_disqualified}, State=#graphics_state{curr_state = play_user})->
	sound_proc ! "lose_trim",
	NewState = State#graphics_state{curr_state=idle, birdUser=#bird{}, bird_x=?BIRD_START_X, bird_direction=r, spikesAmount=?INIT_SPIKES_WALL_AMOUNT},
	{noreply, NewState};
%% TODO delete
%%handle_cast({brain, Brain}, State=#graphics_state{brainList=BrainList, genNum=GN, bestPreviousBrain=BestPreviousBrain})->
%%	NewBrainList = BrainList ++ [Brain],
%%	case length(NewBrainList) of
%%		?NUM_OF_BIRDS ->
%%%%			io:format("~n~p member!!!!! ~p~n", [GN, lists:member(BestPreviousBrain, NewBrainList)]),
%%			NewNewBrainList = [];
%%		_   ->
%%			NewNewBrainList = NewBrainList
%%	end,
%%	{noreply, State#graphics_state{brainList=NewNewBrainList}};
%% CandBirds = [{FrameCount1, WeightsList1}, {FrameCount2, WeightsList2}]


%% A message from a PC that represents that he finished the simulation. If all PCs finished, the graphics changes to play_NEAT_population state.
handle_cast({pc_finished_simulation, _PC_PID, CandBirds}, State=#graphics_state{curr_state=play_NEAT_simulation, alivePCsNamesList=AlivePCsNamesList, waitForPCsAmount=WaitForPCsAmount, bestCandBirds=BestCandBirds})->
	SortedBirds = lists:keysort(1, BestCandBirds ++ CandBirds),             % all birds are dead now, send them sorted (by frame count) to graphics
	{_, NewBestCandBirds} = lists:split(length(SortedBirds) - ?NUM_OF_SURVIVED_BIRDS, SortedBirds),      % take only the ?100? best birds
	NewState =
		case WaitForPCsAmount of     % how many PCs are running (birds) simulation now
			1 ->
				FinalBestCandBirds = element(2, lists:unzip(NewBestCandBirds)),
				ReverseList = lists:reverse(FinalBestCandBirds),
				send_best_birds(ReverseList, AlivePCsNamesList),    % send the best birds to the PCs
				State#graphics_state{curr_state=play_NEAT_population, bestPreviousBrain=lists:last(FinalBestCandBirds), waitForPCsAmount=length(AlivePCsNamesList),
					bestCandBirds=[], bird_x=?BIRD_START_X, bird_direction=r, spikesList=?INIT_SPIKE_LIST, spikesAmount=?INIT_SPIKES_WALL_AMOUNT};
			
			_Else ->
				State#graphics_state{waitForPCsAmount=WaitForPCsAmount-1, bestCandBirds=NewBestCandBirds}
		end,
	{noreply, NewState};

%% A message from a PC that represents that he finished the population. If all PCs finished, the graphics changes to play_NEAT_simulation state.
handle_cast({pc_finished_population, _PC_PID}, State=#graphics_state{curr_state=play_NEAT_population, alivePCsNamesList=AlivePCsNamesList, waitForPCsAmount=WaitForPCsAmount, genNum=GenNum, score=Score, bestScore=BestScore})->
	case WaitForPCsAmount of
		1 ->
			NewBirdsPerPcMap = maps:from_list([{PC_Name, trunc(?NUM_OF_BIRDS / ?INIT_PC_AMOUNT)} || PC_Name <- AlivePCsNamesList]),
			cast_all_PCs(AlivePCsNamesList, {start_simulation}),
			cast_all_PCs(AlivePCsNamesList, {simulate_frame}),
			{NewDirection, NewX, _Has_changed_dir} = simulate_x_movement(?BIRD_START_X, r),
			{noreply, State#graphics_state{curr_state=play_NEAT_simulation, waitForPCsAmount=length(AlivePCsNamesList), numOfAliveBirds=trunc(length(AlivePCsNamesList)*(?NUM_OF_BIRDS/?INIT_PC_AMOUNT)),
										   bird_x=NewX, bird_direction=NewDirection, birdsPerPcMap=NewBirdsPerPcMap, genNum=GenNum+1, score=0, bestScore=max(BestScore, Score)}};	% only after all birds had initialized, the graphics_state changes its state
		
		_Else ->
			{noreply, State#graphics_state{waitForPCsAmount=WaitForPCsAmount-1}}
	end.

%% =================================================================
%% We reach here each button click
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{curr_state=CurrState, mainSizer=MainSizer, uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, 
																							  bird_x=_Bird_x, bird_direction=_Bird_dir, alivePCsNamesList=AlivePCsNamesList,
																							  spikesList=SpikesList, score=Score, bestScore=BestScore, birdUserPID=BirdUserPID}) ->
	NewState = case {CurrState, ID} of
		{idle, ?ButtonStartUserID} ->
			wxSizer:hide(UiSizer, StartSizer, []),
			wxSizer:show(UiSizer, JumpSizer, []),
			wxSizer:layout(MainSizer),
			BirdUserPID ! {start_simulation},
			State#graphics_state{score=0, bestScore=max(BestScore, Score), curr_state=play_user};
		
		{idle, ?ButtonStartNEATID} ->
			wxSizer:hide(UiSizer, StartSizer, []),
			wxSizer:layout(MainSizer),
			cast_all_PCs(AlivePCsNamesList, {start_bird_FSM, play_NEAT, SpikesList}),
			State#graphics_state{score=0, bestScore=0};
		
		{play_user, ?ButtonJumpID} ->
			sound_proc ! "jump_trim",
			gen_statem:cast(BirdUserPID, {jump}),
			State;
			
		_ ->
			State
	end,
	{noreply, NewState};

%% closing window event
handle_event(#wx{event = #wxClose{}}, State = #graphics_state{frame = Frame}) -> % close window event
	io:format("Exiting\n"),
	wxWindow:destroy(Frame),
	wx:destroy(),
	unregister(sound_proc),
	{stop, normal, State}.

%% =================================================================
%% We reach here each timer event. This function moves to the next frame.
handle_info(timer, State=#graphics_state{uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, mainSizer=MainSizer, frame=Frame,
										 locatedBirdsAmount=LocatedBirdsAmount, birdUserPID=BirdUserPID, bird_x=Bird_x, bird_direction=Bird_dir,
										 spikesList=SpikesList, curr_state=CurrState, score=Score, spikesAmount=SpikesAmount, birdsPerPcMap=BirdsPerPcMap,
										 bestCandBirds=BestCandBirds}) ->
	wxWindow:refresh(Frame), % refresh screen
	
	{NewTimeCount, NewAlivePCsNamesList, NewRecvACKsPCsNamesList, NewBirdsPerPcMap, NewWaitForPCsAmount} = check_timeout(State),
	
	NewState =
		case CurrState of
			idle ->
				wxSizer:hide(UiSizer, JumpSizer, []),
				wxSizer:show(UiSizer, StartSizer, []),
				wxSizer:layout(MainSizer),
				erlang:send_after(?TIMER_NEAT, self(), timer),	% set new timer
				State#graphics_state{birdUser=#bird{}, bird_x=?BIRD_START_X, bird_direction=r, spikesAmount=?INIT_SPIKES_WALL_AMOUNT, spikesList=?INIT_SPIKE_LIST};
		
			play_user ->
				gen_statem:cast(BirdUserPID, {simulate_frame}),
				{NewDirection, NewX, Has_changed_dir} = simulate_x_movement(Bird_x, Bird_dir),
				case Has_changed_dir of
					true  ->
						sound_proc ! "bonus_trim",
						NewSpikesList = create_spikes_list(trunc(SpikesAmount)),
						gen_statem:cast(BirdUserPID, {spikes_list, NewSpikesList}),
						NewSpikesAmount = min(SpikesAmount + ?ADD_SPIKES_WALL_TOUCH, ?MAX_RATIONAL_SPIKES_AMOUNT),
						NewScore = Score + 1;
					
					false ->
						NewSpikesList = SpikesList,
						NewSpikesAmount = SpikesAmount,
						NewScore = Score
				end,
				erlang:send_after(?TIMER_USER, self(), timer),	% set new timer
				State#graphics_state{bird_x=NewX, bird_direction=NewDirection, spikesList=NewSpikesList, score=NewScore, spikesAmount=NewSpikesAmount};
				
			play_NEAT_simulation ->
				erlang:send_after(?TIMER_NEAT, self(), timer),	% set new timer
				NumOfAliveBirds = maps:fold(fun(_, NumBirds, Acc) -> Acc + NumBirds end, 0, BirdsPerPcMap),
				case NumOfAliveBirds of
					0 ->    % go to population when last PC died
						FinalBestCandBirds = element(2, lists:unzip(BestCandBirds)),
						send_best_birds(lists:reverse(FinalBestCandBirds), NewAlivePCsNamesList),
						State#graphics_state{curr_state=play_NEAT_population, locatedBirdsAmount=0, bestPreviousBrain=lists:last(FinalBestCandBirds), waitForPCsAmount=length(NewAlivePCsNamesList),
						bestCandBirds=[], bird_x=?BIRD_START_X, bird_direction=r, spikesList=?INIT_SPIKE_LIST, spikesAmount=?INIT_SPIKES_WALL_AMOUNT};
					
					_ ->
						case LocatedBirdsAmount >= NumOfAliveBirds of   % all birds sent their location.
							true  ->
								cast_all_PCs(NewAlivePCsNamesList, {simulate_frame}),
								{NewDirection, NewX, Has_changed_dir} = simulate_x_movement(Bird_x, Bird_dir),
								case Has_changed_dir of
									true  ->
										NewSpikesList = create_spikes_list(trunc(SpikesAmount)),
										cast_all_PCs(NewAlivePCsNamesList, {spikes_list, NewSpikesList}),
										NewSpikesAmount = min(SpikesAmount + ?ADD_SPIKES_WALL_TOUCH, ?MAX_RATIONAL_SPIKES_AMOUNT),
										NewScore = Score + 1;
									
									false ->
										NewSpikesList = SpikesList,
										NewSpikesAmount = SpikesAmount,
										NewScore = Score
								end,
								State#graphics_state{birdList=sets:new(), locatedBirdsAmount=0, bird_x=NewX, bird_direction=NewDirection, spikesList=NewSpikesList, score=NewScore, spikesAmount=NewSpikesAmount};

							false ->   % wait for some birds to send their location.
								State
						end
				end;
			
			play_NEAT_population ->
				erlang:send_after(?TIMER_NEAT, self(), timer),	% set new timer
				State
		end,

	{noreply, NewState#graphics_state{timeCount=NewTimeCount, recvACKsPCsNamesList=NewRecvACKsPCsNamesList, alivePCsNamesList=NewAlivePCsNamesList, birdsPerPcMap=NewBirdsPerPcMap, waitForPCsAmount=NewWaitForPCsAmount}};

handle_info(#wx{event=#wxClose{}}, State) ->
	{stop, normal, State}.

%% =================================================================
%% This function draws on the screen every frame.
handle_sync_event(_Event, _, _State=#graphics_state{curr_state=CurrState, spikesList=SpikesList, panel=Panel, brush=Brush,
													bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L, birdUser=#bird{y=Y},
													bird_x=X, bird_direction=Direction, birdList=BirdList, score=Score, bestScore=BestScore,
													textScore=TxtScore, textBirdsPerPC=TxtBirdsPerPC, birdsPerPcMap=BirdsPerPcMap, genNum=GenNum}) ->
	
	DC = wxPaintDC:new(Panel),
	BitmapBird = case Direction of
		r -> BitmapBird_R;
		l -> BitmapBird_L
	end,
	
	ScoreLabel = "Score: " ++ integer_to_list(Score) ++ "\nBest score: " ++ integer_to_list(BestScore),
	case CurrState of
		idle ->
			wxStaticText:setLabel(TxtScore, ScoreLabel),
			wxStaticText:setLabel(TxtBirdsPerPC, ""),
			wxDC:drawBitmap(DC, BitmapBird, {X, Y});
		
		play_user ->
			wxStaticText:setLabel(TxtScore, ScoreLabel),
			wxStaticText:setLabel(TxtBirdsPerPC, ""),
			wxDC:drawBitmap(DC, BitmapBird, {X, Y});
		
		play_NEAT_simulation ->
			wxStaticText:setLabel(TxtScore, ScoreLabel ++ "\nGeneration: " ++ integer_to_list(GenNum)),
			wxStaticText:setLabel(TxtBirdsPerPC, "Birds:\n" ++ io_lib:format("~p", [BirdsPerPcMap])),
			List = sets:to_list(BirdList),
			draw_birds(DC, BitmapBird, X, List);
		
		play_NEAT_population ->
			wxStaticText:setLabel(TxtScore, ScoreLabel ++ "\nGeneration: " ++ integer_to_list(GenNum)),
			wxDC:drawBitmap(DC, BitmapBird, {X, Y})
	end,
	
	wxDC:setPen(DC, ?wxTRANSPARENT_PEN),
	wxDC:setBrush(DC, Brush),
	wxDC:drawRectangle(DC, {0, 0, ?BG_WIDTH, ?TOP_RECT_HEIGHT}),
	wxDC:drawRectangle(DC, {0, ?BOTTOM_RECT_Y, ?BG_WIDTH, ?BOTTOM_RECT_HEIGHT}),
	draw_top_bottom_spikes(DC, ?SPIKES_LEFT_X, ?TOP_BOTTOM_SPIKES_AMOUNT),
	draw_wall_spikes(DC, SpikesList, ?SPIKES_TOP_Y, Direction),
	
	wxPaintDC:destroy(DC);
	
handle_sync_event(_Event, _, State) ->
	{noreply, State}.

%% =================================================================
%% check if timeout occurred
check_timeout(_State=#graphics_state{timeCount=TimeCount, recvACKsPCsNamesList=RecvACKsPCsNamesList, alivePCsNamesList=AlivePCsNamesList, birdsPerPcMap=BirdsPerPcMap, waitForPCsAmount=WaitForPCsAmount}) ->
	case TimeCount >= ?TIMEOUT of
		true ->     % timeout passed
			?PRINT(),
			NewTimeCount = 0,
			NewAlivePCsNamesList = RecvACKsPCsNamesList,
			NewRecvACKsPCsNamesList = [],
			{NewBirdsPerPcMap, NewWaitForPCsAmount} = update_birdsPerPcMap(BirdsPerPcMap, AlivePCsNamesList, RecvACKsPCsNamesList, WaitForPCsAmount),
			cast_all_PCs(NewAlivePCsNamesList, {are_you_alive});
		
		false ->
			NewTimeCount = TimeCount + ?TIMER_NEAT,
			NewAlivePCsNamesList = AlivePCsNamesList,
			NewRecvACKsPCsNamesList = RecvACKsPCsNamesList,
			NewBirdsPerPcMap = BirdsPerPcMap,
			NewWaitForPCsAmount = WaitForPCsAmount
		end,
	{NewTimeCount, NewAlivePCsNamesList, NewRecvACKsPCsNamesList, NewBirdsPerPcMap, NewWaitForPCsAmount}.


%% Gets a lists of the PCs that just died and removes them from the map with the amount of alive birds per PC.
update_birdsPerPcMap(BirdsPerPcMap, AlivePCsNamesList, RecvACKsPCsNamesList, WaitForPCsAmount) ->
	case AlivePCsNamesList -- RecvACKsPCsNamesList of
		[] -> {BirdsPerPcMap, WaitForPCsAmount}; % no new dead PC
		DeadPCs -> {maps:map(fun(PC_Name, Num_Birds) -> case lists:member(PC_Name, DeadPCs) of true -> 0; false -> Num_Birds  end end, BirdsPerPcMap), WaitForPCsAmount - length(DeadPCs)}
	end.


%% Simulates a x movement of a bird during one frame.
%% the output is: {NewDirection, NewX, Has_changed_direction}
simulate_x_movement(X, Direction) ->
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{r, _    , true } -> {l        , X - ?X_VELOCITY, true };	% If the bird is moving to the right and     touching the right wall.
		{r, _    , false} -> {Direction, X + ?X_VELOCITY, false};	% If the bird is moving to the right and not touching the right wall.
		{l, true , _    } -> {r        , X + ?X_VELOCITY, true };	% If the bird is moving to the left  and     touching the left  wall.
		{l, false, _    } -> {Direction, X - ?X_VELOCITY, false} 	% If the bird is moving to the left  and not touching the left  wall.
	end.


%% Wrap function. Creates a random spikes list with exactly TotalSpikes spikes
create_spikes_list(TotalSpikes) ->
	create_spikes_list(?INIT_SPIKE_LIST, 0, TotalSpikes).


%% Creates a random spikes list with exactly TotalSpikes spikes
create_spikes_list(SpikesList, TotalSpikes, TotalSpikes) -> SpikesList;
create_spikes_list(SpikesList, SpikesCreated, TotalSpikes) ->
	SpikeIdx = rand:uniform(?MAX_SPIKES_AMOUNT-SpikesCreated),	% [1,10] -> [1,1]
	create_spikes_list(insert_spike(SpikesList, SpikeIdx), SpikesCreated+1, TotalSpikes).


%% Insert the spike to the list at the given index.
%% Skip 1's at the inserting
insert_spike(SpikesList, 1) -> [1|SpikesList--[0]];         % insert the spike
insert_spike([IsSpike|Spikes_T], SpikeIdx) ->
	case IsSpike of
		1 -> [IsSpike|insert_spike(Spikes_T, SpikeIdx)];	% skip
		0 -> [IsSpike|insert_spike(Spikes_T, SpikeIdx-1)]	% don't skip
	end.


draw_birds(_, _, _, []) -> ok;
draw_birds(DC, BitmapBird, X, [Y|BirdList]) ->
	wxDC:drawBitmap(DC, BitmapBird, {X, Y}),
	draw_birds(DC, BitmapBird, X, BirdList).


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
	draw_wall_spikes(DC, SpikesList_Tail, CurrSpike_Y + ?SPIKE_WIDTH + ?SPIKE_GAP_Y, Direction).


draw_top_bottom_spikes(_, _, 0) -> ok;
draw_top_bottom_spikes(DC, CurrSpike_X, Spikes_amount) ->
	Center_of_spike = CurrSpike_X + ?SPIKE_HALF_WIDTH,
	End_of_spike_X = CurrSpike_X + ?SPIKE_WIDTH,
	wxDC:drawPolygon(DC, [{CurrSpike_X, ?TOP_RECT_HEIGHT}, {Center_of_spike, ?TOP_RECT_HEIGHT+?SPIKE_HEIGHT}, {End_of_spike_X, ?TOP_RECT_HEIGHT}]),
	wxDC:drawPolygon(DC, [{CurrSpike_X, ?BOTTOM_RECT_Y}, {Center_of_spike, ?BOTTOM_RECT_Y-?SPIKE_HEIGHT}, {End_of_spike_X, ?BOTTOM_RECT_Y}]),
	draw_top_bottom_spikes(DC, End_of_spike_X+?SPIKE_GAP_X, Spikes_amount-1).


%% Notify all PCs about their best birds. A bird performs better when it stays alive for more frames.
send_best_birds(_BestCandBirds, []) ->
%%	gen_server:cast(PC_Name, {populate_next_gen, BestCandBirds}),
	finish;%rpc:call(?PC_NAME_TO_NODE(PC_Name), pc_bird_server, pc_rpc, [PC_Name, {populate_next_gen, BestCandBirds}]);
send_best_birds(BestCandBirds, [PC_Name|PC_NamesT]) ->
	{CurrPcWeights, NextPcsWeights} = lists:split(trunc(?NUM_OF_SURVIVED_BIRDS / ?INIT_PC_AMOUNT), BestCandBirds),
%%	gen_server:cast(PC_Name, {populate_next_gen, CurrPcWeights}),
	rpc:call(?PC_NAME_TO_NODE(PC_Name), pc_bird_server, pc_rpc, [PC_Name, {populate_next_gen, CurrPcWeights}]),
	send_best_birds(NextPcsWeights, PC_NamesT).

%% =================================================================
init_system() ->
	{ok, BirdUserPID} = bird_FSM:start(create_bird_FSM_name(graphics), self(), ?INIT_SPIKE_LIST, idle),    % the graphics owns the user bird
	init_PCs(?PC_NODES, ?PC_NAMES),
	{BirdUserPID}.

init_PCs([], []) -> finish;
init_PCs([PC_Node|PC_NodesT], [PC_Name|PC_NamesT]) ->
	rpc:call(PC_Node, pc_bird_server, start, [PC_Name, trunc(?NUM_OF_BIRDS / ?INIT_PC_AMOUNT)]),
	init_PCs(PC_NodesT, PC_NamesT).


%% build a new & unique bird FSM name
create_bird_FSM_name(PC_Name) -> list_to_atom("bird_FSM_" ++ atom_to_list(PC_Name) ++ integer_to_list(erlang:unique_integer())).


%% Casts the message Msg to all PCs
cast_all_PCs([], _Msg) -> finish;
cast_all_PCs([PC_Name|PC_NamesT], Msg) ->
	rpc:call(?PC_NAME_TO_NODE(PC_Name), pc_bird_server, pc_rpc, [PC_Name, Msg]),
	cast_all_PCs(PC_NamesT, Msg).


%% Receive rpc from PC
graphics_rpc(Msg)->
	wx_object:cast(graphics, Msg).


%% This function is done by the sound process
%% Example of playing a sound from a file: os:cmd("aplay sounds/lose.wav")
sound() ->
	receive
		SoundName -> os:cmd("aplay sounds/" ++ SoundName ++ ".wav")
	end,
	sound().

