-module(lesson12).

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
          time,
          texture,
          box,
          top,
          xrot = 0.0,
          yrot = 0.0,
          box_colors = [{1.0, 0.0, 0.0}, {1.0, 0.5, 0.0}, {1.0, 1.0, 0.0}, {0.0, 1.0, 0.0}, {0.0, 1.0, 1.0}],
          top_colors = [{0.5, 0.0, 0.0}, {0.5, 0.25, 0.0}, {0.5, 0.5, 0.0}, {0.0, 0.5, 0.0}, {0.0, 0.5, 0.5}]
          }).

-record(texture, {id, w, h, minx, miny, maxx, maxy}).

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
  wxGLCanvas:connect(Canvas, key_up),
  wxWindow:hide(Parent),
  wxWindow:reparent(Canvas, Parent),
  wxWindow:show(Parent),
  wxGLCanvas:setCurrent(Canvas),

  State = #state{parent = Parent, config = Config, canvas = Canvas},
  NewState = setup_gl(State),

  Timer = timer:send_interval(20, self(), update),

  {Parent, NewState#state{timer = Timer}}.

handle_event(#wx{event = #wxSize{size = {W, H}}}, State) ->
  case W =:= 0 orelse H =:= 0 of
    true -> skip;
    _ ->
      resize_gl_scene(W, H)
  end,
  {noreply, State};

handle_event(#wx{event = #wxKey{keyCode = KeyCode}}, State) ->
  NewState = case KeyCode of
               ?WXK_UP ->
                 State#state{xrot = State#state.xrot - 0.2};
               ?WXK_DOWN ->
                 State#state{xrot = State#state.xrot + 0.2};
               ?WXK_LEFT ->
                 State#state{yrot = State#state.yrot - 0.2};
               ?WXK_RIGHT ->
                 State#state{yrot = State#state.yrot + 0.2};
               Key ->
                 io:format("key: ~p~n", [Key]),
                 State
             end,
  {noreply, NewState};

handle_event(Msg, State) ->
  io:format("~p~n", [Msg]),
  {noreply, State}.

handle_info(update, State) ->
  NewState = wx:batch(fun() -> render(State) end),
  {noreply, NewState};

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

resize_gl_scene(Width, Height) ->
      gl:viewport(0, 0, Width, Height),
      gl:matrixMode(?GL_PROJECTION),
      gl:loadIdentity(),
      glu:perspective(45.0, Width / Height, 0.1, 100.0),
      gl:matrixMode(?GL_MODELVIEW),
      gl:loadIdentity().

setup_gl(#state{parent = Window} = State) ->
  {Width, Height} = wxWindow:getClientSize(Window),
  resize_gl_scene(Width, Height),
  NewState = setup_display_lists(State),

  gl:enable(?GL_TEXTURE_2D),
  gl:shadeModel(?GL_SMOOTH),
  gl:clearColor(0.0, 0.0, 0.0, 0.5),
  gl:clearDepth(1.0),
  gl:enable(?GL_DEPTH_TEST),
  gl:depthFunc(?GL_LEQUAL),
  gl:enable(?GL_LIGHT0),
  gl:enable(?GL_LIGHTING),
  gl:enable(?GL_COLOR_MATERIAL),
  gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),
  Texture = load_texture_by_image(wxImage:new("crate.jpg")),
  NewState#state{texture = Texture}.

setup_display_lists(State) ->
  Box = gl:genLists(2),
  gl:newList(Box, ?GL_COMPILE),
  gl:'begin'(?GL_QUADS),
  % front face
  gl:texCoord2f(0.0, 0.0), gl:vertex3f(-1.0, -1.0,  1.0),
  gl:texCoord2f(1.0, 0.0), gl:vertex3f( 1.0, -1.0,  1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f( 1.0,  1.0,  1.0),
  gl:texCoord2f(0.0, 1.0), gl:vertex3f(-1.0,  1.0,  1.0),

  % back face
  gl:texCoord2f(1.0, 0.0), gl:vertex3f(-1.0, -1.0, -1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f(-1.0,  1.0, -1.0),
  gl:texCoord2f(0.0, 1.0), gl:vertex3f( 1.0,  1.0, -1.0),
  gl:texCoord2f(0.0, 0.0), gl:vertex3f( 1.0, -1.0, -1.0),

  % bottom face
  gl:texCoord2f(1.0, 1.0), gl:vertex3f(-1.0, -1.0, -1.0),
  gl:texCoord2f(0.0, 1.0), gl:vertex3f( 1.0, -1.0, -1.0),
  gl:texCoord2f(0.0, 0.0), gl:vertex3f( 1.0, -1.0,  1.0),
  gl:texCoord2f(1.0, 0.0), gl:vertex3f(-1.0, -1.0,  1.0),

  % right face
  gl:texCoord2f(1.0, 0.0), gl:vertex3f( 1.0, -1.0, -1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f( 1.0,  1.0, -1.0),
  gl:texCoord2f(0.0, 1.0), gl:vertex3f( 1.0,  1.0,  1.0),
  gl:texCoord2f(0.0, 0.0), gl:vertex3f( 1.0, -1.0,  1.0),

  % left face
  gl:texCoord2f(0.0, 0.0), gl:vertex3f(-1.0, -1.0, -1.0),
  gl:texCoord2f(1.0, 0.0), gl:vertex3f(-1.0, -1.0,  1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f(-1.0,  1.0,  1.0),
  gl:texCoord2f(0.0, 1.0), gl:vertex3f(-1.0,  1.0, -1.0),

  gl:'end'(),
  gl:endList(),

  Top = Box + 1,
  gl:newList(Top, ?GL_COMPILE),

  gl:'begin'(?GL_QUADS),

  % top face
  gl:texCoord2f(0.0, 1.0), gl:vertex3f(-1.0,  1.0, -1.0),
  gl:texCoord2f(0.0, 0.0), gl:vertex3f(-1.0,  1.0,  1.0),
  gl:texCoord2f(1.0, 0.0), gl:vertex3f( 1.0,  1.0,  1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f( 1.0,  1.0, -1.0),

  gl:'end'(),
  gl:endList(),

  State#state{box = Box, top = Top}.

render(#state{parent = _Window, canvas = Canvas} = State) ->
  NewState = draw(State),
  wxGLCanvas:swapBuffers(Canvas),
  NewState.

draw(#state{texture = Texture} = State) ->
  gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

  gl:bindTexture(?GL_TEXTURE_2D, Texture#texture.id),

  draw_cubes(State),

  State.

draw_cubes(State) ->
  draw_cubes(0, 1, State).
draw_cubes(_XLoop, 6, State) ->
  State;
draw_cubes(Loop, Loop, State) ->
  draw_cubes(0, Loop+1, State);
draw_cubes(XLoop, YLoop, #state{box = Box, top = Top, xrot = XRot, yrot = YRot, box_colors = BoxColors, top_colors = TopColors} = State) ->
  gl:loadIdentity(),
  gl:translatef(1.4 + (XLoop * 2.8) - (YLoop * 1.4), ((6.0 - YLoop) * 2.4) - 7.0, -20.0),
  gl:rotatef(45.0 - (2.0 * YLoop) + XRot, 1.0, 0.0, 0.0),
  gl:rotatef(45.0 + YRot, 0.0, 1.0, 0.0),
  gl:color3fv(lists:nth(YLoop, BoxColors)),
  gl:callList(Box),
  gl:color3fv(lists:nth(YLoop, TopColors)),
  gl:callList(Top),
  draw_cubes(XLoop + 1, YLoop, State).


load_texture_by_image(Image) ->
  ImageWidth = wxImage:getWidth(Image),
  ImageHeight = wxImage:getHeight(Image),
  Width = get_power_of_two_roof(ImageWidth),
  Height = get_power_of_two_roof(ImageHeight),
  Data = get_image_data(Image),

  % Create opengl texture for the image
  [TextureID] = gl:genTextures(1),
  gl:bindTexture(?GL_TEXTURE_2D, TextureID),
  gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_LINEAR),
  gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
  Format = case wxImage:hasAlpha(Image) of
             true -> ?GL_RGBA;
             false -> ?GL_RGB
           end,
  gl:texImage2D(?GL_TEXTURE_2D, 0, Format, Width, Height, 0, Format, ?GL_UNSIGNED_BYTE, Data),
  #texture{id = TextureID, w = ImageWidth, h = ImageHeight,
           minx = 0, miny = 0, maxx = ImageWidth / Width, maxy = ImageHeight / Height}.

get_image_data(Image) ->
  RGB = wxImage:getData(Image),
  case wxImage:hasAlpha(Image) of
    true ->
      Alpha = wxImage:getAlpha(Image),
      interleave_rgb_and_alpha(RGB, Alpha);
    false ->
      RGB
  end.

interleave_rgb_and_alpha(RGB, Alpha) ->
  list_to_binary(
    lists:zipwith(fun({R, G, B}, A) ->
                      <<R, G, B, A>>
                        end,
                  [{R,G,B} || <<R, G, B>> <= RGB],
                  [A || <<A>> <= Alpha])).


get_power_of_two_roof(X) ->
    get_power_of_two_roof_2(1, X).

get_power_of_two_roof_2(N, X) when N >= X -> N;
get_power_of_two_roof_2(N, X) -> get_power_of_two_roof_2(N*2, X).






