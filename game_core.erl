-module(game_core).

-behaviour(wx_object).

-export([start_link/0, start_link/1, init/1, code_change/3, handle_info/2, handle_event/2, handle_call/3, terminate/2]).
-export([load/2, unload/1, shutdown/1]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-record(state, {
          win,
          object
         }).

start_link() ->
  start_link([]).

start_link(Config) ->
  wx_object:start_link(?MODULE, Config, []).

init(Config) ->
  wx:new(Config),
  process_flag(trap_exit, true),

  Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Game Core", [{size, {300, 300}}]),
  wxFrame:show(Frame),
  {Frame, #state{win = Frame}}.

load(Ref, Module) ->
  wx_object:call(Ref, {load, Module}).
unload(Ref) ->
  wx_object:call(Ref, unload).
shutdown(Ref) ->
  wx_object:call(Ref, stop).

handle_info({'EXIT', _, wx_deleted}, State) ->
  {noreply, State};
handle_info({'EXIT', _, normal}, State) ->
  {noreply, State};
handle_info(Msg, State) ->
  io:format("Info: ~p~n", [Msg]),
  {noreply, State}.

handle_call({load, Module}, _From, State) ->
  Ref = Module:start([{parent, State#state.win}, {size, wxWindow:getClientSize(State#state.win)}]),
  {reply, Ref, State#state{object=Ref}};
handle_call(unload, _From, State) ->
  wx_object:get_pid(State#state.object) ! stop,
  {reply, ok, State#state{object=undefined}};
handle_call(stop, _From, State) ->
  {stop, normal, State};
handle_call(Msg, _From, State) ->
  io:format("Call: ~p~n", [Msg]),
  {reply, ok, State}.


handle_event(#wx{event=#wxClose{}}, State = #state{win = Frame}) ->
  io:format("~p Closing window ~n", [self()]),
  ok = wxFrame:setStatusText(Frame, "Closing...", []),
  {stop, normal, State};
handle_event(Ev, State) ->
  io:format("~p Event: ~p~n", [?MODULE, Ev]),
  {noreply, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
  wx:destroy().


