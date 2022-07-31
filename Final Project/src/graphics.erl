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
-export([init/1, handle_event/2]).

-define(SERVER, ?MODULE).

-define(BG_WIDTH,500).
-define(BG_HEIGHT,1000).

start() ->
	wx_object:start({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	_Button = wxButton:new(Frame, 10, [{label, "Start"}]),

	MainSizer = wxBoxSizer:new(?wxVERTICAL),
	SizerBird = wxStaticBoxSizer:new(?wxHORIZONTAL, Panel, [{label, "SizerBird"}]),
	SizerBG = wxStaticBoxSizer:new(?wxVERTICAL, Panel, [{label, "SizerBG"}]),

	ImageBG = wxImage:new("images/background.png", []),
	BitmapBG = wxBitmap:new(wxImage:scale(ImageBG, ?BG_WIDTH, ?BG_HEIGHT, [{quality, ?wxIMAGE_QUALITY_HIGH}])),
	StaticBitmapBG = wxStaticBitmap:new(Panel, 1, BitmapBG),
	wxSizer:add(SizerBG, StaticBitmapBG, []),

	ImageBird = wxImage:new("images/bird.png", []),
	BitmapBird = wxBitmap:new(wxImage:scale(ImageBird, 20, 20, [])),
	StaticBitmapBird = wxStaticBitmap:new(Panel, 1, BitmapBird),
	wxSizer:add(SizerBird, StaticBitmapBird, []),

	wxSizer:add(MainSizer, SizerBG, []),
	wxSizer:add(MainSizer, SizerBird, []),
	wxPanel:setSizer(Panel, MainSizer),

	wxFrame:show(Frame),

	timer:sleep(10000)
.

handle_event(_Request, _State) ->
	todo.
