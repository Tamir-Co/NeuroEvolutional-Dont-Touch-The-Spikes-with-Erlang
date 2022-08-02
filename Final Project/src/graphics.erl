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
-export([init/1, handle_event/2, handle_sync_event/3, handle_info/2]).

-define(SERVER, ?MODULE).

start() ->
	wx_object:start({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	ButtonStartUser = wxButton:new(Frame, ?ButtonStartUserID, [{label, "Start (user)"}]),
	ButtonStartNEAT = wxButton:new(Frame, ?ButtonStartNEATID, [{label, "Start (NEAT)"}]),
	ButtonJump = wxButton:new(Frame, ?ButtonJumpID, [{label, "Jump"}]),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	UiSizer = wxBoxSizer:new(?wxVERTICAL),

	wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
	wxSizer:add(UiSizer, ButtonStartUser,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
	wxSizer:add(UiSizer, ButtonStartNEAT,[{flag,?wxALL bor ?wxEXPAND},{border, 5}]),
	wxSizer:add(UiSizer, ButtonJump,[{flag,?wxALL bor ?wxEXPAND},{border, 2}]),
	wxSizer:add(MainSizer, UiSizer),

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
	wxButton:connect(ButtonStartUser, command_button_clicked),
	wxButton:connect(ButtonStartNEAT, command_button_clicked),
	wxButton:connect(ButtonJump, command_button_clicked),

	%timer:sleep(5000),

	{Frame, #graphics_state{
				frame = Frame,
				panel = Panel,
				bitmapBG = BitmapBG,
				bitmapBird_R = BitmapBird_R,
				bitmapBird_L = BitmapBird_L,
				bird = #bird{},
				curr_state = idle}}.

%% We reach here each button press
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{curr_state=CurrState, bird=Bird}) ->
	NewState = case ID of
		?ButtonStartUserID -> BirdPID = init_system(),
							  gen_statem:cast(BirdPID, {start_simulation}),
							  State#graphics_state{curr_state=play_user, birdPID=BirdPID};
		
		?ButtonStartNEATID -> BirdPID = init_system(),
							  State#graphics_state{curr_state=play_NEAT, birdPID=BirdPID};	% TODO change PID
		
		?ButtonJumpID	   -> gen_statem:cast(BirdPID, {jump})
	end,
	{noreply, NewState}.

%% We reach here each timer event
handle_info(timer, State=#graphics_state{frame=Frame, bird=Bird, birdPID=BirdPID, curr_state=CurrState}) ->  % refresh screen for graphics
	wxWindow:refresh(Frame), % refresh screen

	NewState = if 	CurrState == play_user ->		% Bird is falling only when state is user
						gen_statem:cast(BirdPID, {simulate_frame});
					true ->
						State
				end,

	erlang:send_after(?Timer, self(), timer),	% set new timer
	{noreply, NewState}.

handle_sync_event(_Event, _, _State = #graphics_state{panel=Panel, bitmapBG=BitmapBG, bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L, bird=_Bird=#bird{x=X, y=Y, direction=Direction}}) ->
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
	SpikesList = [1,1,1,1,1,1,0,0,0,1],
	draw_spikes(DC, SpikesList, 100),

%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
	wxPaintDC:destroy(DC).

draw_spikes(_, [], _) -> ok;
draw_spikes(DC, [IsSpike|SpikesList_Tail], CurrSpike_Y) ->
	case IsSpike of
		1 -> wxDC:drawPolygon(DC, [{0, CurrSpike_Y}, {25, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {0, CurrSpike_Y+?SPIKE_WIDTH}]);
		0 -> no_spike
	end,
	draw_spikes(DC, SpikesList_Tail, CurrSpike_Y+?SPIKE_WIDTH+?SPIKE_GAP).

%% ==============================
init_system() ->
	{ok, BirdPID} = bird_FSM:start_bird_FSM(bird_FSM, self()),	% TODO pc_server_pid instead of self()
	BirdPID.

