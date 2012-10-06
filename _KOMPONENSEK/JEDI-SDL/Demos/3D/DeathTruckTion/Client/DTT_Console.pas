unit DTT_Console;

interface

uses
  DTT_Sound;

const
  NB_LINES = 32;

procedure ConsoleReset;
procedure ConsoleInput;
procedure ConsoleDraw;

var
  Beep: TSample;
  ExitFlag: Boolean;

implementation

uses
  OpenGL12, SysUtils, DTT_GUI, TNT_Font, TNT_3D, DTT_Input, SDL, DTT_Game;

var
  Size: Integer;
  Active: Boolean;
  Alpha: Single;

procedure ConsoleReset;
begin
  Active := False;
  Alpha := 0;
  Size := 5;
end;

procedure ConsoleDraw;
var
  i, Pos: Integer;
begin
  if Active and (Alpha < 0.4) then
    Alpha := Alpha + 0.01;
  if not Active and (Alpha > 0) then
    Alpha := Alpha - 0.01;

  if Alpha <> 0 then
  begin
    Pos := Console.Cursor;
    glDisable(GL_TEXTURE_2D);

      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glColor4f(0, 0, 0, Alpha);
    glRectf(0, Size*18, 640, 0);

      glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    glColor4f(1, 1, 1, Alpha/0.4);
    glBegin(GL_LINES);
    glVertex2f(0, Size*18);
    glVertex2f(640, Size*18);
    glEnd;
    glEnable(GL_TEXTURE_2D);
    for i := Pos downto Pos-Size+1 do
      Font.Print(10, (Size+i-Pos-1)*18, 1, 0, Console.Lines[i and (NB_LINES-1)]);
  end;
end;

procedure IncSize;
begin
  if Size < 20 then Inc(Size);
end;

procedure DecSize;
begin
  if Size > 1 then Dec(Size);
end;

procedure Exec;
var
  Cmd: String;
begin
  Cmd := Console.Execute;
  if Cmd = 'STAT' then HUD.ShowStat := not HUD.ShowStat;
  if Cmd = 'INFO' then Console.Log('DTT BUILD #1653 - Using the TNT-3D Engine by NitroGen');
  if Cmd = 'QUIT' then ExitFlag := True;
end;

procedure ConsoleInput;
begin
{$IFDEF WIN32}
  if Input.Keyboard.NewKey = SDLK_BACKQUOTE then
{$ENDIF}
{$IFDEF LINUX}
  if Input.Keyboard.NewKey = SDLK_WORLD_18 then
{$ENDIF}
  begin
    Beep.Play;
    Active := not Active;
  end;

  if Active then
  begin
    with Input.Keyboard do
    begin
      case NewKey of
        SDLK_BACKSPACE: Console.DelChar;
        SDLK_PAGEUP: DecSize;
        SDLK_PAGEDOWN: IncSize;
        SDLK_RETURN: Exec;
      end;
      if Character <> #0 then
      begin
        if (Character = ' ') or (Character = '.') or
        (Character >= 'a') and (Character <= 'z') or
        (Character >= 'A') and (Character <= 'Z') or
        (Character >= '0') and (Character <= '9') then
          Console.SendChar(Character);
        Character := #0;
      end;
    end;
  end;
  if ExitFlag then GUI.ExitGame;
end;

end.

