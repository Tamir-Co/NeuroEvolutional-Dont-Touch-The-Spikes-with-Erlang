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
	ImageBird_L = wxImage:new("images/bird_LEFT.png", []),
	BitmapBird_R = wxBitmap:new(wxImage:scale(ImageBird_R, ?BIRD_WIDTH, 50, [])),
	BitmapBird_L = wxBitmap:new(wxImage:scale(ImageBird_L, ?BIRD_WIDTH, 50, [])),
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

	%% Bird init
	X = ?BIRD_START_X,
	Y = ?BIRD_START_Y,
	VelocityY = 0,
	Direction = right,

	{Frame, #graphics_state{
				frame = Frame,
				panel = Panel,
				bitmapBG = BitmapBG,
				bitmapBird_R = BitmapBird_R,
				bitmapBird_L = BitmapBird_L,
				bird = #bird{x=X, y=Y, velocityY=VelocityY, direction=Direction},
				curr_state = idle}}
.

%% We reach here each button press
handle_event(#wx{id=ID, event=#wxCommand{type=command_button_clicked}}, State=#graphics_state{bird=Bird}) ->
	NewState = case ID of
		?ButtonStartUserID -> init_system(), State#graphics_state{curr_state=play_user};
		?ButtonStartNEATID -> init_system(), State#graphics_state{curr_state=play_NEAT};
		?ButtonJumpID -> State#graphics_state{bird=jump(Bird)}
	end,
	{noreply, NewState}.

%% We reach here each timer event
handle_info(timer, State = #graphics_state{frame=Frame, bird=Bird, curr_state=CurrState}) ->  % refresh screen for graphics
	wxWindow:refresh(Frame), % refresh screen				^ =#bird{x=X, y=Y, velocityY=VelocityY}

	if CurrState == play_user ->		% Bird is falling only when state is user
			NewState = State#graphics_state{bird=simulate_bird(Bird)};
		true ->
			NewState = State
	end,

	erlang:send_after(?Timer, self(), timer),	% set new timer
	{noreply, NewState}.

handle_sync_event(_Event, _, _State = #graphics_state{panel=Panel, bitmapBG=BitmapBG, bitmapBird_R=BitmapBird_R, bitmapBird_L=BitmapBird_L, bird=_Bird=#bird{x=X, y=Y, direction=Direction}}) ->
	DC2 = wxPaintDC:new(Panel),
	wxDC:clear(DC2),
	wxDC:drawBitmap(DC2, BitmapBG, {0, 0}),
	case Direction of
		right -> wxDC:drawBitmap(DC2, BitmapBird_R, {X, Y});
		left  -> wxDC:drawBitmap(DC2, BitmapBird_L, {X, Y})
	end,
	
	% Pen = ,
	wxDC:setPen(DC2, wxPen:new({128,128,128}, [{style, 100}])),
	% Brush = ,
	wxDC:setBrush(DC2, wxBrush:new({128,128,128}, [{style, 100}])),
	% SpikeProb = 50,
	% SpikesList = lists:filter(rand:uniform(100) < SpikeProb, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	% SpikesList = [rand:uniform(100) < SpikeProb || _ <- lists:seq(1,?MAX_SPIKES_AMOUNT)],
	
	% SpikesList = lists:map(fun(_) -> case rand:uniform(100) < SpikeProb  of true -> 1; false -> 0 end end, lists:seq(1,?MAX_SPIKES_AMOUNT)),
	SpikesList = [1,1,1,1,1,1,0,0,0,1],
	% wxDC:drawPolygon(DC2, [{0, 100}, {0, 130}, {25, 115}]),
	draw_spikes(DC2, SpikesList, 100),

%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
	wxPaintDC:destroy(DC2).

draw_spikes(_, [], _) -> ok;
draw_spikes(DC, [IsSpike|SpikesList_Tail], CurrSpike_Y) ->
	case IsSpike of
		1 -> wxDC:drawPolygon(DC, [{0, CurrSpike_Y}, {25, CurrSpike_Y+?SPIKE_HALF_WIDTH}, {0, CurrSpike_Y+?SPIKE_WIDTH}]);
		0 -> no_spike
	end,
	draw_spikes(DC, SpikesList_Tail, CurrSpike_Y+?SPIKE_WIDTH+?SPIKE_GAP).

%% ==============================
init_system() ->
	X = ?BIRD_START_X,
	Y = ?BIRD_START_Y,
	VelocityY = -?JUMP_VELOCITY,	% initialize the bird to jump when the game starts.
	Direction = right,
	% Bird = 
	#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}
	% ,simulate_bird(Bird)
	.

simulate_bird(Bird = #bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}) ->
	case {Direction, X =< 0, ?BG_WIDTH =< X+?BIRD_WIDTH} of
		{right, _    , true } -> NewDirection = left     , NewX = X - ?X_VELOCITY;
		{right, _    , false} -> NewDirection = Direction, NewX = X + ?X_VELOCITY;
		{left , true , _    } -> NewDirection = right    , NewX = X + ?X_VELOCITY;
		{left , false, _    } -> NewDirection = Direction, NewX = X - ?X_VELOCITY
	end,
	todo,
	Bird#bird{x=NewX, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+2, direction=NewDirection}.

% jump(Bird = #bird{x=X, y=Y, velocityY=_VelocityY, direction=Direction}) ->
jump(Bird=#bird{}) ->
	% Bird#bird{x=X, y=Y-?JUMP_VELOCITY*?TIME_UNIT, velocityY=-?JUMP_VELOCITY, direction=Direction}
	Bird#bird{velocityY=-?JUMP_VELOCITY}
	.