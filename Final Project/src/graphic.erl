%%%-------------------------------------------------------------------
%%% @author nadavhd
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2022 19:01
%%%-------------------------------------------------------------------
-module(graphic).
-author("nadavhd").

%% API
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).
-export([start_link/0,fn/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	handle_event/2,
	terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {}).

fn()->
	wx:new(),
	F = wxFrame:new(wx:null(), -1, "Countdown"),
	T = wxStaticText:new(F, -1, "42"),
	wxFrame:show(F).


start_link() ->
	wx_object:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	wx:new(),
	%% Env = wx:get_env(),
	%% build and layout the GUI components
	Frame = wxFrame:new(wx:null(), 1, "Countdown"),
	%% Player1 = gen_server:call(player1, get_panel),
	%% Player2 = gen_server:call(player2, get_panel),
	wxWindow:setMinSize(Frame, wxWindow:getSize(Frame)),
	wxFrame:connect(Frame, close_window),
	wxFrame:show(Frame),
	{Frame, #state{}}.
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.
handle_cast(_Msg, State) ->
	{noreply, State}.
handle_info(Msg, State) ->
	io:format("frame got unexpected message ~p~n", [Msg]),
	{noreply, State}.
handle_event(#wx{event = #wxClose{}}, State) ->
	{stop, normal, State}.
terminate(_Reason, _State) ->
	%% sys:terminate(player1, Reason),
	%% sys:terminate(player2, Reason),
	wx:destroy(),
	ok.
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
