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
	wx_object:start({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
	process_flag(trap_exit, true),
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
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
	% ImageBird_L = wxImage:new("images/bird_LEFT.png", []),
	ImageBird_L = wxImage:mirror(ImageBird_R),
	BitmapBird_R = wxBitmap:new(wxImage:scale(ImageBird_R, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
	BitmapBird_L = wxBitmap:new(wxImage:scale(ImageBird_L, ?BIRD_WIDTH, ?BIRD_HEIGHT, [])),
%%	StaticBitmapBird = wxStaticBitmap:new(Panel, 1, BitmapBird),

	wxPanel:setSizer(Frame, MainSizer),
	wxSizer:setSizeHints(MainSizer, Frame),

	wxFrame:show(Frame),
	erlang:send_after(?Timer, self(), timer),

	wxPanel:connect(Panel, paint, [callback]),
	wxFrame:connect(Frame, close_window),
	wxButton:connect(ButtonStartUser, command_button_clicked),
	wxButton:connect(ButtonStartNEAT, command_button_clicked),
	wxButton:connect(ButtonJump, command_button_clicked),
	
	% wxButton:connect(ButtonJump, command_button_clicked),
	% wx_object:connect(),
	wxEvtHandler:connect(Frame, key_down),
	% wxKeyEvent:wxKeyEventType()

	%timer:sleep(5000),
	BirdServerPID = init_system(),		% Init bird servers and split the work

	{Frame, #graphics_state{
		frame = Frame,
		panel = Panel,
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


% the location of the bird
handle_cast({bird_location, X, Y, Direction}, State=#graphics_state{bird=Bird})->
	NewBird = Bird#bird{x=X, y=Y, direction=Direction},
	NewState = State#graphics_state{bird=NewBird},
	{noreply, NewState};

handle_cast({finish_init_birds, _PC_Name}, State=#graphics_state{curr_state = CurrState, pcList = PC_List})->
	case CurrState of
		play_user -> gen_server:cast(hd(PC_List), {start_simulation});		% we can now goto start simulation
		play_NEAT -> todo
	end,
	{noreply, State};

handle_cast({bird_disqualified, _BirdPID}, State=#graphics_state{curr_state = CurrState})->
	NewState = case CurrState of
				   play_user -> State#graphics_state{curr_state = idle, bird = #bird{}};		% we can now goto start simulation
				   play_NEAT -> todo, State
			   end,
	{noreply, NewState}.

%% We reach here each button press
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{mainSizer=MainSizer, uiSizer=UiSizer, startSizer=StartSizer,jumpSizer=JumpSizer, pcList = PC_List, curr_state = CurrState}) ->
%%	io:format("a "),
	NewState = case ID of
				   ?ButtonStartUserID -> wxSizer:show(UiSizer, JumpSizer, []),
					   					wxSizer:hide(UiSizer, StartSizer, []),
					   					wxSizer:layout(MainSizer),
					   					 % gen_statem:cast(BirdPID, {start_simulation}),%timer:sleep(50000),

					   					 % cast pc to init FSM
					   					 NumOfBirds = 1,
					   					 gen_server:cast(hd(PC_List), {start_bird_FSM, NumOfBirds}),
					   					 State#graphics_state{curr_state=play_user};

				   ?ButtonStartNEATID -> _BirdPID = init_system(),
					   					 State#graphics_state{curr_state=play_NEAT};	% TODO change PID!

				   ?ButtonJumpID	   -> case CurrState of
											  play_user -> %io:format("\007\n"), TODO if we want sound: erl -oldshell
												  		   gen_server:cast(hd(PC_List), {jump});
											  play_NEAT -> todo;
											  idle -> void
										  end,
					   					  State
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

%% We reach here each timer event
handle_info(timer, State=#graphics_state{uiSizer=UiSizer, startSizer=StartSizer, jumpSizer=JumpSizer, mainSizer=MainSizer, spikesList=SpikesList, frame=Frame, pcList=PC_List, curr_state=CurrState, bird=Bird}) ->  % refresh screen for graphics
%%	io:format("b "),
	wxWindow:refresh(Frame), % refresh screen

	NewState = case CurrState of
				  idle 		->  wxSizer:hide(UiSizer, JumpSizer, []),
					  wxSizer:show(UiSizer, StartSizer, []),
					  			wxSizer:layout(MainSizer),
					  			State#graphics_state{bird=#bird{}};
				  play_user -> 	case is_bird_touch_wall_spike(Bird, SpikesList) of
									true -> io:format("Game Over!"),
											State;
									false-> State
								end,
					  			gen_server:cast(hd(PC_List), {simulate_frame}),
					  			State;
				  play_NEAT	->  todo,
					  			State
			  end,

	erlang:send_after(?Timer, self(), timer),	% set new timer
%%	io:format("NewState ~p", [NewState]),
	{noreply, NewState};

handle_info(#wx{event=#wxClose{}}, State) ->
	{stop, normal, State}.

handle_sync_event(_Event, _, _State=#graphics_state{spikesList=SpikesList, panel=Panel, bitmapBG=BitmapBG, bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L, bird=#bird{x=X, y=Y, direction=Direction}}) ->
%%	io:format("c "),

	DC = wxPaintDC:new(Panel),
	wxDC:clear(DC),
	wxDC:drawBitmap(DC, BitmapBG, {0, 0}),
	case Direction of
		right -> wxDC:drawBitmap(DC, BitmapBird_R, {X, Y});
		left  -> wxDC:drawBitmap(DC, BitmapBird_L, {X, Y})
	end,

	wxDC:setPen(DC, wxPen:new({128,128,128}, [{style, 100}])),
	wxDC:setBrush(DC, wxBrush:new({128,128,128}, [{style, 100}])),
	% SpikeProb = 50,
	% SpikesList = lists:filter(rand:uniform(100) < SpikeProb, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	% SpikesList = [rand:uniform(100) < SpikeProb || _ <- lists:seq(1,?MAX_SPIKES_AMOUNT)],

	% SpikesList = lists:map(fun(_) -> case rand:uniform(100) < SpikeProb  of true -> 1; false -> 0 end end, lists:seq(1,?MAX_SPIKES_AMOUNT)),

%%	SpikesList = [1,0,1,0,1,0,1,0,1,0],
	draw_spikes(DC, SpikesList, 100),

%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
	wxPaintDC:destroy(DC);
handle_sync_event(_Event, _, State) ->
	{noreply, State}.
%%
%%terminate(_Reason, State = #graphics_state{}) ->
%%	wxFrame:destroy(State#graphics_state.frame).

draw_spikes(_, [], _) -> ok;
draw_spikes(DC, [IsSpike|SpikesList_Tail], CurrSpike_Y) ->
	case IsSpike of
		1 -> wxDC:drawPolygon(DC, [{0, CurrSpike_Y}, {25, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {0, CurrSpike_Y+?SPIKE_WIDTH}]);
		0 -> no_spike
	end,
	draw_spikes(DC, SpikesList_Tail, CurrSpike_Y+?SPIKE_WIDTH+?SPIKE_GAP).

%% ==============================
init_system() ->
	PC_Name = list_to_atom("pc1_" ++ integer_to_list(erlang:unique_integer())),
	{ok, BirdServerPID} = pc_bird_server:start(PC_Name),	% init pc bird server
	BirdServerPID.

%% Receive bird location and spikes.
%% Return true if bird disqualified and otherwise false
is_bird_touch_wall_spike(_Bird=#bird{x=X, y=Y}, SpikesList) ->
%%	io:format("\n\nX=~p, Y=~p", [X, Y]),
	case X =< ?SPIKE_HEIGHT/4 orelse X >= ?RIGHT_WALL_X - ?SPIKE_HEIGHT/4 of	% bird is near the wall
		false -> false;			% bird still in the game
		true  -> case lists:nth(closest_spike(Y), SpikesList) of	% check closest spike
				 	0 -> false;	% bird still in the game because there is no spike near
					1 -> true	% bird disqualified
				 end
	end.

%% Gets a height Y and returns the closest spike's index
closest_spike(Y) ->
	SpikeSlotHeight = ?SPIKE_WIDTH + ?SPIKE_GAP,
	min(10, 1 + trunc((Y-?SPIKES_TOP_Y) / SpikeSlotHeight + 0.5)).
