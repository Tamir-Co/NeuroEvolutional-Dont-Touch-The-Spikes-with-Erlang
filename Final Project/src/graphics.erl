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

	ImageBird = wxImage:new("images/bird.png", []),
	BitmapBird = wxBitmap:new(wxImage:scale(ImageBird, 50, 50, [])),
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
				bitmapBird = BitmapBird,
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
handle_info(timer, State = #graphics_state{frame=Frame, bird=Bird=#bird{x=X, y=Y, velocityY=VelocityY}, curr_state=CurrState}) ->  % refresh screen for graphics
	wxWindow:refresh(Frame), % refresh screen

	if CurrState == play_user ->		% Bird is falling only when state is user
			NewState = State#graphics_state{bird=Bird#bird{x=X, y=Y+VelocityY*?TIME_UNIT, velocityY=VelocityY+1}};
		true ->
			NewState = State
	end,

	erlang:send_after(?Timer, self(), timer),	% set new timer
	{noreply, NewState}.

handle_sync_event(_Event, _, _State = #graphics_state{panel=Panel, bitmapBG=BitmapBG, bitmapBird=BitmapBird, bird=_Bird=#bird{x=X, y=Y}}) ->
	DC2 = wxPaintDC:new(Panel),
	wxDC:clear(DC2),
	wxDC:drawBitmap(DC2, BitmapBG, {0, 0}),
	wxDC:drawBitmap(DC2, BitmapBird, {X, Y}),
%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
	wxPaintDC:destroy(DC2).

%% ==============================
init_system() ->
	X = ?BIRD_START_X,
	Y = ?BIRD_START_Y,
	VelocityY = 0,
	Direction = right,
	simulate_bird(#bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}).

simulate_bird(Bird = #bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}) ->
	todo,
	Bird = #bird{x=X, y=Y, velocityY=VelocityY, direction=Direction}.

jump(Bird = #bird{x=X, y=Y, velocityY=_VelocityY, direction=Direction}) ->
	Bird#bird{x=X, y=Y-?JUMP_VELOCITY*?TIME_UNIT, velocityY=-?JUMP_VELOCITY, direction=Direction}
	.