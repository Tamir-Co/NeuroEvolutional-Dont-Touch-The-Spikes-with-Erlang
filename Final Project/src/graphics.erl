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

-define(BG_WIDTH,564).
-define(BG_HEIGHT,1024).

start() ->
	wx_object:start({local,?SERVER},?MODULE,[],[]).

init(_Args) ->
	WxServer = wx:new(),
	Frame = wxFrame:new(WxServer, ?wxID_ANY, "Don't Touch The Spikes - Nadav & Tamir", [{size,{?BG_WIDTH, ?BG_HEIGHT}}]),
	Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	_Button = wxButton:new(Frame, 10, [{label, "Start"}]),

	MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
	wxSizer:add(MainSizer, Panel,[{flag,?wxEXPAND}]),
	add_images(Panel),

	wxFrame:show(Frame),
	wxPanel:connect(Panel, paint, [callback]),
	timer:sleep(5000)
	.

handle_event(_Request, _State) ->
	todo.

add_images(Panel) ->
	Rmap = wxImage:new("images/background.png"),
	Rmapc = wxImage:scale(Rmap,?BG_WIDTH,?BG_HEIGHT),
	BmpRMap = wxBitmap:new(Rmapc),
	wxImage:destroy(Rmap),
	wxImage:destroy(Rmapc),
	DC2 = wxPaintDC:new(Panel),
	wxDC:clear(DC2),
	wxDC:drawBitmap(DC2,BmpRMap,{10,10}),
	wxPaintDC:destroy(DC2)
	.