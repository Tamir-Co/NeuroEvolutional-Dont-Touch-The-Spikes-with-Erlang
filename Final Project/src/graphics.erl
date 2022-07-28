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
	_Panel  = wxPanel:new(Frame,[{size, {?BG_WIDTH, ?BG_HEIGHT}}]),
	_Button = wxButton:new(Frame, 10, [{label, "Start"}]),

	add_images(),

	wxFrame:show(Frame),
	timer:sleep(5000)
	.

handle_event(_Request, _State) ->
	todo.

add_images() ->

	.