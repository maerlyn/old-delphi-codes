program JediSDLBlockOut;
{******************************************************************}
{                                                                  }
{       Borland Delphi/Kylix SDL 3D TETRIS ( Block Out )           }
{                                                                  }
{ Portions copyright by Christian Hackbart <chackbart@SQC.de>      }
{ All Rights Reserved.                                             }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, in your path .                     }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   It should be easy compileable under Kylix, but I did not found }
{   any time to test this. If you have suggestions, questions or   }
{   something else, you want to tell me, feel free to drop a mail. }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{  October  05 2001 - CH : The first public version v 0.1.         }
{                                                                  }
{  January  27 2002 - DL : Slight change to add SDL Key values.    }
{                                                                  }
{******************************************************************}

{ $DEFINE DEBUG}

uses
  OpenGL12,
  SysUtils,
  Logger,
  SDL;

const
  FSX: Word = 640; // Fullscreen X
  FSY: Word = 480; // Fullscreen Y
  BlendMode: Boolean = True;
  Title = '3D Tetris ( BlockOut ) - ';

var
  Done: Boolean;
  event: TSDL_Event;
  keystate: PKeyStateArr;
  flags: Uint32;
  OldTicks: UInt32; // Timer

  // Game-Variables
  Scene: record
    Field: array[0..7, 0..10, 0..7] of byte;
    Fill: array[0..10] of byte;
    X, Y, Z: Byte;
    Speed, Rand: byte;
    StepX, StepY, StepZ: real;
    angle, mx, my, sx, sy,
      ax, ay, az: integer;
  end;
  Item: record
    Cube: array[0..3, 0..2] of byte;
    Rotate: array[0..2] of byte;
    IColor: byte;
  end;
  Score: LongInt;
  Pause: Boolean;
  MouseStatus: Byte;
  Count: Word;

  {$IFDEF LINUX}
  Colors: array[1..12, 0..3] of TGLfloat =
  ((1, 0, 0, 0.5), (0, 1, 0, 0.5), (0, 0, 1, 0.5), (1, 0, 1, 0.5),
    (1, 1, 0, 0.5), (0, 1, 1, 0.5), (1, 1, 1, 0.5), (1, 0.5, 0.5, 0.5),
    (0.5, 1, 0.5, 0.5), (0.5, 0.5, 1, 0.5), (1, 0.5, 1, 0.5),
    (1, 0.5, 0.1, 0.5));

  NewParts: array[1..7, 0..3, 0..2] of byte =
  (((2, 10, 3), (3, 10, 3), (3, 10, 4), (4, 10, 3)),
    ((2, 10, 3), (3, 10, 3), (4, 10, 3), (5, 10, 3)),
    ((3, 10, 3), (3, 10, 4), (4, 10, 3), (4, 10, 4)),
    ((3, 10, 3), (4, 10, 3), (4, 10, 4), (5, 10, 4)),
    ((3, 10, 3), (4, 10, 3), (5, 10, 3), (3, 10, 4)),
    ((3, 9, 3), (3, 10, 3), (3, 10, 4), (4, 10, 4)),
    ((3, 9, 4), (3, 10, 3), (3, 10, 4), (4, 10, 4)));
  LPos: array[0..2, 0..3] of TGLfloat = ((2, 0, 0, 1), (0, 2, 0, 1), (0, 0, 2,
    1));
  LColor: array[0..2, 0..3] of TGLfloat = ((1, 0, 0, 0.5), (0, 0, 1, 0.5), (0,
    1, 0, 0.5));
{$ENDIF}
{$IFDEF WIN32}
  Colors: array[1..12, 0..3] of TGLfloat =
  ((1, 0, 0, 0.5), (0, 1, 0, 0.5), (0, 0, 1, 0.5), (1, 0, 1, 0.5),
    (1, 1, 0, 0.5), (0, 1, 1, 0.5), (1, 1, 1, 0.5), (1, 0.5, 0.5, 0.5),
    (0.5, 1, 0.5, 0.5), (0.5, 0.5, 1, 0.5), (1, 0.5, 1, 0.5),
    (1, 0.5, 0.1, 0.5));

  NewParts: array[1..7, 0..3, 0..2] of byte =
  (((2, 10, 3), (3, 10, 3), (3, 10, 4), (4, 10, 3)),
    ((2, 10, 3), (3, 10, 3), (4, 10, 3), (5, 10, 3)),
    ((3, 10, 3), (3, 10, 4), (4, 10, 3), (4, 10, 4)),
    ((3, 10, 3), (4, 10, 3), (4, 10, 4), (5, 10, 4)),
    ((3, 10, 3), (4, 10, 3), (5, 10, 3), (3, 10, 4)),
    ((3, 9, 3), (3, 10, 3), (3, 10, 4), (4, 10, 4)),
    ((3, 9, 4), (3, 10, 3), (3, 10, 4), (4, 10, 4)));
  LPos: array[0..2, 0..3] of TGLfloat = ((2, 0, 0, 1), (0, 2, 0, 1), (0, 0, 2,
    1));
  LColor: array[0..2, 0..3] of TGLfloat = ((1, 0, 0, 0.5), (0, 0, 1, 0.5), (0,
    1, 0, 0.5));
{$ENDIF}
procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;

// A general OpenGL initialization function.  Sets all of the initial parameters.

procedure InitGL(Width: integer; Height: integer; Blend: Boolean);
  // We call this right after our OpenGL window is created.
var
  gldAspect: TGLdouble;
begin
  glViewport(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gldAspect := width / height;
  gluPerspective(60, gldAspect, 0.01, 20);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glEnable(GL_DEPTH_TEST);
  if Blend then
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

//
// Main Game Tetris- Routines
//

procedure MakeList;
var
  i: Integer;
  r: Real;
begin
  with Scene do
  begin
    glNewList(1, GL_COMPILE);
    glColor4f(1, 1, 1, 0.5);
    glBegin(GL_LINES);
    for i := 0 to X + 1 do
    begin
      r := -0.8 + StepX * i;
      glVertex3f(r, -0.7, -0.8);
      glVertex3f(r, -0.7, 0.8);
      glVertex3f(r, -0.7, -0.8);
      glVertex3f(r, 1.5, -0.8);
      glVertex3f(r, -0.7, 0.8);
      glVertex3f(r, 1.5, 0.8);
    end;
    for i := 0 to Z + 1 do
    begin
      r := -0.8 + StepZ * i;
      glVertex3f(-0.8, -0.7, r);
      glVertex3f(0.8, -0.7, r);
      glVertex3f(-0.8, -0.7, r);
      glVertex3f(-0.8, 1.5, r);
      glVertex3f(0.8, -0.7, r);
      glVertex3f(0.8, 1.5, r);
    end;
    for i := 0 to Y do
    begin
      r := -0.7 + StepY * (i + 1);
      glVertex3f(-0.8, r, -0.8);
      glVertex3f(-0.8, r, 0.8);
      glVertex3f(-0.8, r, 0.8);
      glVertex3f(0.8, r, 0.8);
      glVertex3f(0.8, r, 0.8);
      glVertex3f(0.8, r, -0.8);
      glVertex3f(0.8, r, -0.8);
      glVertex3f(-0.8, r, -0.8);
    end;
    glEnd;
    glEndList;
  end;
end;

procedure DrawBlock(xp, yp, zp: real; c: byte);
begin
  with scene do
  begin
    if c = 0 then
      glColor4fv(@colors[trunc(yp) + 1])
    else
      glColor4fv(@colors[c]);
    xp := -0.8 + StepX * xp;
    yp := -0.7 + StepY * yp;
    zp := -0.8 + StepZ * zp;
    glBegin(GL_QUADS);
    glVertex3f(xp, yp, zp);
    glVertex3f(xp + StepX, yp, zp);
    glVertex3f(xp + StepX, yp + StepY, zp);
    glVertex3f(xp, yp + StepY, zp);
    glVertex3f(xp, yp, zp);
    glVertex3f(xp + StepX, yp, zp);
    glVertex3f(xp + StepX, yp, zp + StepZ);
    glVertex3f(xp, yp, zp + StepZ);
    glVertex3f(xp, yp, zp);
    glVertex3f(xp, yp + StepY, zp);
    glVertex3f(xp, yp + StepY, zp + StepZ);
    glVertex3f(xp, yp, zp + StepZ);
    glVertex3f(xp + StepX, yp + StepY, zp + StepZ);
    glVertex3f(xp, yp + StepY, zp + StepZ);
    glVertex3f(xp, yp, zp + StepZ);
    glVertex3f(xp + StepX, yp, zp + StepZ);
    glVertex3f(xp + StepX, yp + StepY, zp + StepZ);
    glVertex3f(xp, yp + StepY, zp + StepZ);
    glVertex3f(xp, yp + StepY, zp);
    glVertex3f(xp + StepX, yp + StepY, zp);
    glVertex3f(xp + StepX, yp + StepY, zp + StepZ);
    glVertex3f(xp + StepX, yp, zp + StepZ);
    glVertex3f(xp + StepX, yp, zp);
    glVertex3f(xp + StepX, yp + StepY, zp);
    glEnd;
  end;
end;

procedure DrawGLScene;
var
  a, b, c: byte;
begin
  with scene do
  begin
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    glPushMatrix;
    glTranslatef(0, sx / 10, sy / 10);
    glRotatef(ax, 1, 0, 0);
    glRotatef(ay, 0, 1, 0);
    glRotatef(az, 0, 0, 1);
    glEnable(GL_BLEND);
    glCallList(1);

    for a := 0 to X do
      for b := 0 to Y do
        for c := 0 to Z do
          if Field[a, b, c] <> 0 then
            DrawBlock(a, b, c, 0);

    for a := 0 to 3 do
      with item do
        DrawBlock(Cube[a, 0], Cube[a, 1], Cube[a, 2], IColor);

    glDisable(GL_BLEND);
    glPopMatrix;
    glFlush;
    SDL_GL_SwapBuffers;
  end;
end;

procedure Destruct;
// Removes all filled lines
var
  i, j, l, n: integer;
  k: real;
begin
  with Scene do
  begin
    j := 0;
    for i := 0 to Y do
      if Fill[i] = 0 then
        Inc(j);

    case j of
      2: j := 3;
      3: j := 5;
      4: j := 7;
    end;

    Inc(Score, X * Z * Speed * j);
    SDL_WM_SetCaption(PChar(Title + 'Score: ' + intToStr(Score) + #0), nil);
    k := 0.9;

    while k > 0 do
    begin
      for i := 0 to Y do
        if Fill[i] = 0 then
          Colors[i + 1, 3] := k;
      k := k - 0.05;
      DrawGLScene;
    end;

    for n := Y downto 0 do // Badaboom
      if Fill[n] = 0 then
      begin
        Colors[n + 1, 3] := 0.5;
        for i := 0 to X do
          for j := 0 to Z do
            for l := n to Z - 1 do
              Field[i, l, j] := Field[i, l + 1, j];
        for i := 0 to X do
          for j := 0 to Z do
            Field[i, 10, j] := 0;

        for i := n to Y - 1 do
          Fill[i] := Fill[i + 1];

        Fill[Z] := (X + 1) * (Z + 1);
      end;
  end;
end;

procedure NewPart;
var
  i: integer;
begin
  Inc(Count);
  with Scene do
  begin
    if (Count mod 50 = 0) and (Speed < Count div 50) and (Speed < 10) then
      Speed := Count div 50;

    Inc(Score, 4 * Speed);
    SDL_WM_SetCaption(PChar( Title + 'Score: ' + intToStr(Score) + #0), nil);

    Item.IColor := Random(7) + 1;
    for i := 0 to 3 do
      with Item do
      begin
        Cube[i, 0] := NewParts[IColor, i, 0] + X - 5;
        Cube[i, 1] := NewParts[IColor, i, 1] + Y - 10;
        Cube[i, 2] := NewParts[IColor, i, 2] + Z - 5;
        if Field[Cube[i, 0], Cube[i, 1], Cube[i, 2]] > 0 then
        begin
          // Game Over
          SDL_WM_SetCaption(PChar( Title + 'Score: ' + intToStr(Score) +
            ' Game Over !'#0), nil);
          pause := true;
        end;
      end;

    with item do
    begin
      Rotate[0] := X - 4;
      Rotate[1] := Y - 3;
      Rotate[2] := Z - 4;
    end;
  end;
end;

function MoveDown: Boolean;
var
  i, j: integer;
begin
  MoveDown := false;
  for i := 0 to 3 do
    with Scene, item do
      if (Cube[i, 1] = 0) or
        (Field[Cube[i, 0], Cube[i, 1] - 1, Cube[i, 2]] <> 0) then
      begin
        for j := 0 to 3 do
        begin
          Field[Cube[j, 0], Cube[j, 1], Cube[j, 2]] := IColor;
          Dec(Fill[Cube[j, 1]]);
        end;
        Destruct; // remove the filled lines
        NewPart;
        Exit;
      end;
  for i := 0 to 3 do
    Dec(item.Cube[i, 1]);

  Dec(item.Rotate[1]);
  MoveDown := true;
end;

procedure NewGame;
var
  i, j, k: byte;
begin
  Score := 0;
  Count := 0;
  with Scene do
  begin
    for i := 0 to Y do
      Fill[i] := (X + 1) * (Z + 1);
    if Rand > 0 then
      for i := 0 to X do
        for j := 0 to Rand - 1 do
          for k := 0 to Z do
          begin
            Field[i, j, k] := Random(3);
            if Field[i, j, k] <> 0 then
            begin
              Dec(Fill[j]);
              Inc(Score, Speed * Y);
            end;
          end;
    for i := 0 to X do
      for j := Rand to Y do
        for k := 0 to Z do
          Field[i, j, k] := 0;
  end;
end;

procedure InitGame;
begin
  Randomize;
  with Scene do
  begin
    sx := -5;
    sy := -30;
    ax := 70;
    ay := -120;
    az := 0;
    angle := 10;

    X := 7;
    Y := 10;
    Z := 7;
    Speed := 1;
    Rand := 0;

    StepX := 1.6 / (X + 1);
    StepY := 2.2 / (Y + 1);
    StepZ := 1.6 / (Z + 1);
  end;

  MouseStatus := 0;
  Pause := false;

  MakeList;
  NewGame;
  NewPart;
end;

//
// Window-Handling functions
//

procedure WindowProc;
var //gldAspect : tglDouble;
  i: integer;
  ItemTmp: array[0..3, 0..2] of byte;

  function CheckItem: boolean;
  var
    i: integer;
  begin
    CheckItem := false;
    for i := 0 to 3 do
      if (ItemTmp[i, 0] < 0) or (ItemTmp[i, 0] > Scene.X) or
        (ItemTmp[i, 1] < 0) or (ItemTmp[i, 1] > Scene.Y) or
        (ItemTmp[i, 2] < 0) or (ItemTmp[i, 2] > Scene.Z) or
        (Scene.Field[ItemTmp[i, 0], ItemTmp[i, 1], ItemTmp[i, 2]] > 0) then
        Exit;
    CheckItem := true;
  end;

  procedure MoveItem(Direction: boolean);
  var
    i, j: integer;
  begin
    if Direction then
      for i := 0 to 3 do
        for j := 0 to 2 do
          Item.Cube[i, j] := ItemTmp[i, j]
    else
      for i := 0 to 3 do
        for j := 0 to 2 do
          ItemTmp[i, j] := Item.Cube[i, j];
  end;

begin
  while (SDL_PollEvent(@event) = 1) do
  begin
    case event.type_ of
      SDL_QUITEV: Done := true;
      SDL_VIDEORESIZE: with event.resize do
        begin
          if h = 0 then
            h := 1;
          SDL_SetVideoMode(w, h, 0, flags);
          InitGL(w, h, BlendMode);
          Makelist;
        end;
      SDL_MOUSEBUTTONDOWN:
        begin
          if event.button.state = 1 then
            Inc(MouseStatus);
          if event.button.state = 3 then
            Inc(MouseStatus, 2);
          scene.mx := event.button.x;
          scene.my := event.button.y;
        end;
      SDL_MOUSEBUTTONUP:
        begin
          if event.button.state = 1 then
            MouseStatus := MouseStatus and 2;
          if event.button.state = 3 then
            MouseStatus := MouseStatus and 1;
        end;
      SDL_MOUSEMOTION:
        begin
          if MouseStatus = 0 then
            Exit;

          if MouseStatus and 1 <> 0 then
          begin
            Inc(scene.ay, scene.mx - event.button.x);
            Inc(scene.ax, scene.my - event.button.y);
          end;

          if MouseStatus and 2 <> 0 then
          begin
            Inc(scene.az, scene.mx - event.button.x);
            Inc(scene.sy, scene.my - event.button.y);
            if scene.sy < -150 then
              scene.sy := -150;
            if scene.sy > 0 then
              scene.sy := 0;
          end;

          scene.mx := event.button.x;
          scene.my := event.button.y;
          DrawGLScene;
        end;

      // Keys
      SDL_KEYDOWN: if not pause then //<< ToDO: Remove this line
          case event.key.keysym.sym of
            SDLK_UP:
              begin // UP
                MoveItem(false);
                for i := 0 to 3 do
                  Dec(ItemTmp[i, 0]);
                if CheckItem then
                begin
                  MoveItem(true);
                  Dec(Item.Rotate[0]);
                end;
              end;
            SDLK_DOWN:
              begin // DOWN
                MoveItem(false);
                for i := 0 to 3 do
                  Inc(ItemTmp[i, 0]);
                if CheckItem then
                begin
                  MoveItem(true);
                  Inc(item.Rotate[0]);
                end;
              end;
            SDLK_RIGHT:
              begin //RIGHT
                MoveItem(false);
                for i := 0 to 3 do
                  Dec(ItemTmp[i, 2]);
                if CheckItem then
                begin
                  MoveItem(true);
                  Dec(item.Rotate[2]);
                end;
              end;
            SDLK_LEFT:
              begin //LEFT
                MoveItem(false);
                for i := 0 to 3 do
                  Inc(ItemTmp[i, 2]);
                if CheckItem then
                begin
                  MoveItem(true);
                  Inc(item.Rotate[2]);
                end;
              end;
            SDLK_SPACE: while MoveDown do
              begin
                Inc(Score, Scene.Speed);
                SDL_WM_SetCaption( PChar( Title + 'Score: ' + intToStr(Score) + #0), nil);
                DrawGLScene;
              end;
            SDLK_a : with item do
              begin // A - RotY+
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := 5 - Cube[i, 2] + Rotate[2] + Rotate[0];
                  ItemTmp[i, 1] := Cube[i, 1];
                  ItemTmp[i, 2] := Cube[i, 0] - Rotate[0] + Rotate[2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
            SDLK_y, SDLK_z: with item do
              begin // X,Z - RotY-
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := Cube[i, 2] - Rotate[2] + Rotate[0];
                  ItemTmp[i, 1] := Cube[i, 1];
                  ItemTmp[i, 2] := 5 - Cube[i, 0] + Rotate[0] + Rotate[2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
            SDLK_s: with item do
              begin // S - RotX+
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := Cube[i, 0];
                  ItemTmp[i, 1] := 5 - Cube[i, 2] + Rotate[2] + Rotate[1];
                  ItemTmp[i, 2] := Cube[i, 1] - Rotate[1] + Rotate[2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
            SDLK_x: with item do
              begin // X - RotX-
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := Cube[i, 0];
                  ItemTmp[i, 1] := Cube[i, 2] - Rotate[2] + Rotate[1];
                  ItemTmp[i, 2] := 5 - Cube[i, 1] + Rotate[1] + Rotate[2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
            SDLK_d: with item do
              begin // D - RotZ+
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := Cube[i, 1] - Rotate[1] + Rotate[0];
                  ItemTmp[i, 1] := 5 - Cube[i, 0] + Rotate[0] + Rotate[1];
                  ItemTmp[i, 2] := Cube[i, 2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
            SDLK_c: with item do
              begin // C - RotZ-
                for i := 0 to 3 do
                begin
                  ItemTmp[i, 0] := 5 - Cube[i, 1] + Rotate[1] + Rotate[0];
                  ItemTmp[i, 1] := Cube[i, 0] - Rotate[0] + Rotate[1];
                  ItemTmp[i, 2] := Cube[i, 2];
                end;
                if CheckItem then
                  MoveItem(true);
              end;
          end;
    end;
    keystate := PKeyStateArr(SDL_GetKeyState(nil));
    if keystate[SDLK_ESCAPE] <> 0 then
      Done := true;
    // for i:=0 to 300 do if keystate[i]<>0 then SDL_WM_SetCaption(PChar('Score: '+intToStr(i)+#0), nil);
  end;

end;

begin
  // Load the appropriate .DLL or .SO
  LoadOpenGL;

  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Could not initialize SDL: %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
  end;

  if (ParamStr(1) = '-fullscreen') or (ParamStr(1) = '-fs') then
    flags := SDL_OPENGL or SDL_FULLSCREEN
  else
    flags := SDL_OPENGL or SDL_RESIZABLE;

  SDL_WM_SetCaption( TITLE, nil);

  if (SDL_SetVideoMode(FSX, FSY, 0, flags) = nil) then
  begin
    Log.LogError(Format('Could not set video mode: %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
  end;

  // Loop, drawing and checking events

  InitGL(FSX, FSY, BlendMode);
  Done := False;
  initGame;

  oldticks := sdl_getticks;

  while (not Done) do
  begin
    DrawGLScene;
    WindowProc;

    // Drop
    if (sdl_getticks - oldticks) > (1000 div Scene.Speed) then
    begin
      if (not Pause) and (MoveDown) then
        DrawGLScene;
      oldticks := sdl_getticks;
    end;

  end;
  TerminateApplication;
end.

