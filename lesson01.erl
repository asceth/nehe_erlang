-module(lesson01).

-behaviour(wx_object).

-export([init/1, code_change/3, handle_info/2, handle_event/2,
	 handle_call/3, terminate/2,
	 start/1]).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-record(state, {
          parent,
          config,
          canvas,
          timer,
          time
          }).

start(Config) ->
  wx_object:start_link(?MODULE, Config, []).

init(Config) ->
  wx:batch(fun() -> do_init(Config) end).

do_init(Config) ->
  Parent = proplists:get_value(parent, Config),
  Size = proplists:get_value(size, Config),
  Opts = [{size, Size}, {style, ?wxSUNKEN_BORDER}],
  GLAttrib = [{attribList, [?WX_GL_RGBA,
                            ?WX_GL_DOUBLEBUFFER,
                            ?WX_GL_MIN_RED, 8,
                            ?WX_GL_MIN_GREEN, 8,
                            ?WX_GL_MIN_BLUE, 8,
                            ?WX_GL_DEPTH_SIZE, 24, 0]}],
  Canvas = wxGLCanvas:new(Parent, Opts ++ GLAttrib),
  wxGLCanvas:connect(Canvas, size),
  wxWindow:hide(Parent),
  wxWindow:reparent(Canvas, Parent),
  wxWindow:show(Parent),
  wxGLCanvas:setCurrent(Canvas),
  setup_gl(Canvas),
  Timer = timer:send_interval(20, self(), update),

  {Parent, #state{parent = Parent, config = Config, canvas = Canvas, timer = Timer}}.

handle_event(#wx{event = #wxSize{size = {W, H}}}, State) ->
  case W =:= 0 orelse H =:= 0 of
    true -> skip;
    _ ->
      gl:viewport(0, 0, W, H),
      gl:matrixMode(?GL_PROJECTION),
      gl:loadIdentity(),
      glu:perspective(45.0, W / H, 0.1, 100.0),
      gl:matrixMode(?GL_MODELVIEW),
      gl:loadIdentity()
  end,
  {noreply, State}.

handle_info(update, State) ->
  wx:batch(fun() -> render(State) end),
  {noreply, State};

handle_info(stop, State) ->
  timer:cancel(State#state.timer),
  catch wxGLCanvas:destroy(State#state.canvas),
  {stop, normal, State}.

handle_call(Msg, _From, State) ->
  io:format("Call: ~p~n", [Msg]),
  {reply, ok, State}.

code_change(_, _, State) ->
  {stop, not_yet_implemented, State}.

terminate(_Reason, State) ->
  catch wxGLCanvas:destroy(State#state.canvas),
  timer:cancel(State#state.timer),
  timer:sleep(300).


setup_gl(Win) ->
  {_W, _H} = wxWindow:getClientSize(Win),
  gl:shadeModel(?GL_SMOOTH),
  gl:clearColor(0.0, 0.0, 0.0, 0.0),
  gl:clearDepth(1.0),
  gl:enable(?GL_DEPTH_TEST),
  gl:depthFunc(?GL_LEQUAL),
  gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),
  ok.

render(#state{parent = _Window, canvas = Canvas} = _State) ->
  draw(),
  wxGLCanvas:swapBuffers(Canvas).

draw() ->
  gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
  gl:loadIdentity(),
  ok.





