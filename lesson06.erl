-module(lesson06).

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
          xrot = 0.0,
          yrot = 0.0,
          zrot = 0.0
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
  gl:enable(?GL_TEXTURE_2D),
  gl:shadeModel(?GL_SMOOTH),
  gl:clearColor(0.0, 0.0, 0.0, 0.0),
  gl:clearDepth(1.0),
  gl:enable(?GL_DEPTH_TEST),
  gl:depthFunc(?GL_LEQUAL),
  gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),
  Texture = load_texture_by_image(wxImage:new("crate.jpg")),
  State#state{texture = Texture}.

render(#state{parent = _Window, canvas = Canvas} = State) ->
  NewState = draw(State),
  wxGLCanvas:swapBuffers(Canvas),
  NewState.

draw(#state{xrot = XRot, yrot = YRot, zrot = ZRot, texture = Texture} = State) ->
  gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
  gl:loadIdentity(),
  gl:translatef(0.0, 0.0, -5.0),

  gl:rotatef(XRot, 1.0, 0.0, 0.0),
  gl:rotatef(YRot, 0.0, 1.0, 0.0),
  gl:rotatef(ZRot, 0.0, 0.0, 1.0),

  gl:bindTexture(?GL_TEXTURE_2D, Texture#texture.id),
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

  % top face
  gl:texCoord2f(0.0, 1.0), gl:vertex3f(-1.0,  1.0, -1.0),
  gl:texCoord2f(0.0, 0.0), gl:vertex3f(-1.0,  1.0,  1.0),
  gl:texCoord2f(1.0, 0.0), gl:vertex3f( 1.0,  1.0,  1.0),
  gl:texCoord2f(1.0, 1.0), gl:vertex3f( 1.0,  1.0, -1.0),

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

  State#state{xrot = XRot + 0.3, yrot = YRot + 0.2, zrot = ZRot + 0.4}.

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






