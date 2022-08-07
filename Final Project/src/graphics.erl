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

-define(SERVER, ?MODULE).

start() ->
	wx_object:start({local,?SERVER}, ?MODULE, [], []).

init(_Args) ->
	process_flag(trap_exit, true),
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
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

	ImageBG = wxImage:new("images/background.png", []),
	BitmapBG = wxBitmap:new(wxImage:scale(ImageBG, ?BG_WIDTH, ?BG_HEIGHT, [{quality, ?wxIMAGE_QUALITY_HIGH}])),
%%	StaticBitmapBG = wxStaticBitmap:new(Panel, 1, BitmapBG),

	ImageBird_R = wxImage:new("images/bird_RIGHT.png", []),
	ImageBird_L = wxImage:mirror(ImageBird_R),	% ImageBird_L = wxImage:new("images/bird_LEFT.png", []),
	BitmapBird_R = wxBitmap:new(wxImage:scale(ImageBird_R, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
	BitmapBird_L = wxBitmap:new(wxImage:scale(ImageBird_L, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
%%	StaticBitmapBird = wxStaticBitmap:new(Panel, 1, BitmapBird),
	
	TxtScore = wxStaticText:new(Panel, -1, "Score: 0"),%, [{pos, {?BG_WIDTH/2, 50}}]),
	
	wxPanel:setSizer(Frame, MainSizer),
	wxSizer:setSizeHints(MainSizer, Frame),

	wxFrame:show(Frame),
	erlang:send_after(?Timer, self(), timer),

	wxPanel:connect(Panel, paint, [callback]),
	wxFrame:connect(Frame, close_window),
	wxButton:connect(ButtonStartUser, command_button_clicked),
	wxButton:connect(ButtonStartNEAT, command_button_clicked),
	wxButton:connect(ButtonJump, command_button_clicked),
	

	%timer:sleep(5000),
	BirdServerPID = init_system(),		% Init bird servers and split the work

	{Frame, #graphics_state{
		frame = Frame,
		panel = Panel,
		textScore = TxtScore,
		mainSizer = MainSizer,
		uiSizer = UiSizer,
		startSizer = StartSizer,
		jumpSizer = JumpSizer,
		bitmapBG = BitmapBG,
		bitmapBird_R = BitmapBird_R,
		bitmapBird_L = BitmapBird_L,
		bird = #bird{},
		curr_state = idle,
		spikesList = [1,0,1,0,0,1,1,0,1,0],
		pcList = [BirdServerPID]
	}}.

%% =================================================================

handle_cast({finish_init_birds, _PC_PID, CurrState}, State=#graphics_state{pcList = PC_List})->
	case CurrState of
		play_user -> gen_server:cast(hd(PC_List), {start_simulation});		% we can now goto start simulation
		play_NEAT -> todo
	end,
	{noreply, State#graphics_state{curr_state = CurrState}};

handle_cast({bird_location, X, Y, Direction}, State=#graphics_state{bird=Bird})->
%%	io:format("~p~p~n", [X, Direction]),
	NewBird = Bird#bird{x=X, y=Y, direction=Direction},
	NewState = State#graphics_state{bird=NewBird},
	{noreply, NewState};

handle_cast({bird_disqualified, _BirdPID}, State=#graphics_state{curr_state = CurrState})->
	NewState = case CurrState of
				   play_user -> State#graphics_state{curr_state=idle, bird=#bird{}, bird_x=?BIRD_START_X, bird_direction=r};		% we can now goto start simulation
				   play_NEAT -> todo, State
			   end,
	{noreply, NewState}.

%% =================================================================

%% We reach here each button press
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{mainSizer=MainSizer, uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, 
																							  pcList = PC_List, curr_state = CurrState, bird_x=Bird_x, bird_direction=Bird_dir,
																							  spikesList = SpikesList}) ->
%%	io:format("a "),
	NewState = case ID of
				   ?ButtonStartUserID -> wxSizer:show(UiSizer, JumpSizer, []),
					   					 wxSizer:hide(UiSizer, StartSizer, []),
					   					 wxSizer:layout(MainSizer),
					   					 % gen_statem:cast(BirdPID, {start_simulation}),%timer:sleep(50000),

					   					 % cast pc to init FSM
					   					 NumOfBirds = 1,
					   					 gen_server:cast(hd(PC_List), {start_bird_FSM, NumOfBirds, play_user, SpikesList}),
					   					 State#graphics_state{score=0};

				   ?ButtonStartNEATID -> NumOfBirds = 1,	% TODO change
					   					 gen_server:cast(hd(PC_List), {start_bird_FSM, NumOfBirds, play_NEAT, SpikesList}),
					   					 State#graphics_state{score=0};

				   ?ButtonJumpID	   -> case CurrState of
											  play_user -> %io:format("\007\n"), TODO if we want sound: erl -oldshell
												  		   gen_server:cast(hd(PC_List), {jump}),
														   {NewDirection, NewX, _} = simulate_x_movement(Bird_x, Bird_dir),
														   State#graphics_state{bird_x=NewX, bird_direction=NewDirection};
											  play_NEAT -> todo, State;
											  idle -> void, State
										  end
			   end,
	{noreply, NewState};

% closing window event
handle_event(#wx{event = #wxClose{}},State = #graphics_state {frame = Frame}) -> % close window event
	io:format("Exiting\n"),
	wxWindow:destroy(Frame),
	wx:destroy(),
	{stop,normal,State};

%% We reach here each key_down event
handle_event(#wx{id=_ID, event=#wxCommand{type=Type}}, State) ->
	io:format("~n~nevent key down: ~p~n", [Type]),
	NewState = State,
	{noreply, NewState}.

%% =================================================================

%% We reach here each timer event
handle_info(timer, State=#graphics_state{uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, mainSizer=MainSizer, frame=Frame, pcList=PC_List,
										 bird_x=Bird_x, bird_direction=Bird_dir, spikesList=SpikesList, curr_state=CurrState, score=Score}) ->  % refresh screen for graphics
%%	io:format("b "),
	wxWindow:refresh(Frame), % refresh screen
	%io:format("score: ~p", [Score]),
	NewState = case CurrState of
				  idle		->  wxSizer:hide(UiSizer, JumpSizer, []),
								wxSizer:show(UiSizer, StartSizer, []),
								wxSizer:layout(MainSizer),
								State#graphics_state{bird=#bird{}, bird_x=?BIRD_START_X, bird_direction=r};	%, score=0

				  play_user -> 	gen_server:cast(hd(PC_List), {simulate_frame}),
								{NewDirection, NewX, Has_changed_dir} = simulate_x_movement(Bird_x, Bird_dir),
%%								io:format("~p~p ", [NewX, NewDirection]),
								case Has_changed_dir of
									true  -> NewSpikesList = create_spikeList(),
											 gen_server:cast(hd(PC_List), {spikes_list, NewSpikesList}),	%SpikesList,
											 NewScore = Score + 1;
									false -> NewSpikesList = SpikesList,
											 NewScore = Score
								end,
								State#graphics_state{bird_x=NewX, bird_direction=NewDirection, spikesList=NewSpikesList, score=NewScore};
								
				  play_NEAT	->  todo,
								State
			  end,

	erlang:send_after(?Timer, self(), timer),	% set new timer
%%	io:format("NewState ~p", [NewState]),
	{noreply, NewState};

handle_info(#wx{event=#wxClose{}}, State) ->
	{stop, normal, State}.

%% =================================================================

handle_sync_event(_Event, _, _State=#graphics_state{spikesList=SpikesList, panel=Panel, bitmapBG=BitmapBG, bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L, 
													bird=#bird{y=Y}, bird_x=X, bird_direction=Direction, score=Score, textScore=TxtScore}) ->
%%	io:format("c "),											  ^, x=_X, direction=_Direction

	DC = wxPaintDC:new(Panel),
	wxDC:clear(DC),
	wxDC:drawBitmap(DC, BitmapBG, {0, 0}),
	case Direction of
		r -> wxDC:drawBitmap(DC, BitmapBird_R, {X, Y});
		l -> wxDC:drawBitmap(DC, BitmapBird_L, {X, Y})
	end,
	
	wxStaticText:setLabel(TxtScore, "score: " ++ integer_to_list(Score)),

	wxDC:setPen(DC, wxPen:new({128,128,128}, [{style, 100}])),
	wxDC:setBrush(DC, wxBrush:new({128,128,128}, [{style, 100}])),
	draw_spikes(DC, SpikesList, 100, Direction),

%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
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
		{r, _    , true } -> {l     , X - ?X_VELOCITY, true };
		{r, _    , false} -> {Direction, X + ?X_VELOCITY, false};
		{l, true , _    } -> {r    , X + ?X_VELOCITY, true };
		{l, false, _    } -> {Direction, X - ?X_VELOCITY, false}
	end.


create_spikeList() ->
	SpikeProb = 50,
	% SpikesList = lists:filter(rand:uniform(100) < SpikeProb, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	% SpikesList = [rand:uniform(100) < SpikeProb || _ <- lists:seq(1,?MAX_SPIKES_AMOUNT)],
%%	SpikesList = [1,0,1,0,1,0,1,0,1,0],
	SpikesList = lists:map(fun(_) -> case rand:uniform(100) < SpikeProb of true -> 1; false -> 0 end end, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	SpikesList.


draw_spikes(_, [], _, _) -> ok;
draw_spikes(DC, [IsSpike|SpikesList_Tail], CurrSpike_Y, Direction) ->
	case IsSpike of
		1 -> 
			case Direction of
				r -> wxDC:drawPolygon(DC, [{?BG_WIDTH, CurrSpike_Y}, {?BG_WIDTH-?SPIKE_HEIGHT, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {?BG_WIDTH, CurrSpike_Y+?SPIKE_WIDTH}]);
				l  -> wxDC:drawPolygon(DC, [{0, CurrSpike_Y}, {?SPIKE_HEIGHT, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {0, CurrSpike_Y+?SPIKE_WIDTH}])
			end;
		0 -> no_spike
	end,
	draw_spikes(DC, SpikesList_Tail, CurrSpike_Y+?SPIKE_WIDTH+?SPIKE_GAP, Direction).

%% ==============================
init_system() ->
	PC_Name = list_to_atom("pc1_" ++ integer_to_list(erlang:unique_integer())),
	{ok, BirdServerPID} = pc_bird_server:start(PC_Name),	% init pc bird server
	BirdServerPID.
