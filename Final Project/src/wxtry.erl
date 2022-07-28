%%%-------------------------------------------------------------------
%%% @author nadavhd
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2022 21:09
%%%-------------------------------------------------------------------
-module(wxtry).
-author("nadavhd").

-include_lib("wx/include/wx.hrl").
%% API
-export([fn/0]).


fn()->
	wx:new(),
	F = wxFrame:new(wx:null(), -1, "Countdown"),
	_T = wxStaticText:new(F, -1, "42"),
	wxFrame:show(F).
