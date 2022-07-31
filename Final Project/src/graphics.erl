%%%-------------------------------------------------------------------
%%% @author nadavhd
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2022 19:01
%%%-------------------------------------------------------------------
-module(graphics).
-author("nadavhd").

%% API
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).
-export([start/0]).
-export([init/1, handle_event/2, handle_sync_event/3, handle_info/2]).

-define(SERVER, ?MODULE).

-define(BG_WIDTH,500).
-define(BG_HEIGHT,1000).
-define(Timer,100).% Graphics Update Timer, default: 60

%% GRAPHICS RECORDS %%
-record(graphics_state, {frame, panel, bitmapBG, bitmapBird}).

start() ->
	wx_object:start({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	Button = wxButton:new(Frame, 10, [{label, "Start"}]),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	UiSizer = wxBoxSizer:new(?wxVERTICAL),

	wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
	wxSizer:add(UiSizer, Button,[{flag,?wxALL bor ?wxEXPAND},{border, 2}]),
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
	wxButton:connect(Button, command_button_clicked),

	%timer:sleep(5000),

	{Frame, #graphics_state{
				frame = Frame,
				panel = Panel,
				bitmapBG = BitmapBG,
				bitmapBird = BitmapBird}}
.

handle_event(Request, State) ->
	io:format("Started! request: ~p", [Request]),
	{noreply, State}.

handle_info(timer, State = #graphics_state{frame = Frame}) ->  % refresh screen for graphics
	wxWindow:refresh(Frame), % refresh screen
	erlang:send_after(?Timer, self(), timer),
	io:format("handle_info~n"),
	{noreply, State}.

handle_sync_event(Event, _, _State = #graphics_state{panel = Panel, bitmapBG = BitmapBG, bitmapBird = BitmapBird}) ->
	DC2 = wxPaintDC:new(Panel),
	wxDC:clear(DC2),
	wxDC:drawBitmap(DC2, BitmapBG, {0,0}),
	wxDC:drawBitmap(DC2, BitmapBird, {220,400}),
%%	wxBitmap:destroy(BitmapBird),
%%	wxBitmap:destroy(BitmapBG),
	io:format("handle_sync_event~n Event: ~p~n~n", [Event]),
	wxPaintDC:destroy(DC2).