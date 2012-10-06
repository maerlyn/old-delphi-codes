unit TNT_Console;

interface

const
  NB_LINES = 32;
  PROMPT = 'TNT> ';
  LOG_NAME = 'TNT.log';

type
  LogProc = procedure;

  TConsole = class
  private
    LogFlag: Boolean;
    LogFile: Text;
    Prompt: String;
    CurrLine: String;
  public
    Lines: array of String;
    Cursor: Integer;
    constructor Create(NumLines: Integer; PromptStr, LogName: String);
    procedure Log(Str: String);
    procedure SendChar(Ch: Char);
    procedure DelChar;
    function GetParam(n: Integer): String;
    function Execute: String;
    procedure NewLine;
    destructor Destroy; override;
  end;

implementation

uses
  OpenGL12, SysUtils, TNT_Skybox, TNT_Landscape, TNT_3D, TNT_Object, TNT_Vector,
  SDL, TNT_Font;

constructor TConsole.Create(NumLines: Integer; PromptStr, LogName: String);
begin
  inherited Create;
  SetLength(Lines, NumLines);
  Prompt := PromptStr;
  Lines[Cursor] := Prompt;
  LogFlag := LogName <> '';
  if LogFlag then
  begin
    AssignFile(LogFile, LogName);
    ReWrite(LogFile);
    CloseFile(LogFile);
  end;
  Log('TNT Console running...');
  if LogFlag then
    Log('Log enabled -> ' + LogName);
end;

procedure TConsole.SendChar(Ch: Char);
begin
  Lines[Cursor] := Lines[Cursor] + Ch;
end;

procedure TConsole.DelChar;
begin
  if Length(Lines[Cursor]) > Length(Prompt) then
    SetLength(Lines[Cursor], Length(Lines[Cursor])-1);
end;

procedure TConsole.Log(Str: String);
var
  t: String;
begin
  if LogFlag then
  begin
    Append(LogFile);
    WriteLn(LogFile, Str);
    CloseFile(LogFile);
  end;
  t := Lines[Cursor];
  Lines[Cursor] := Str;
  NewLine;
  Lines[Cursor] := t;
end;

procedure TConsole.NewLine;
begin
  Cursor := (Cursor+1) and (Length(Lines)-1);
  Lines[Cursor] := Prompt;
end;

function TConsole.GetParam(n: Integer): String;
var
  i, j, k: Integer;
begin
  if n = 0 then
  begin
    CurrLine := Copy(Lines[Cursor], 6, 255) + ' ';
    NewLine;
    i := Pos(' ', CurrLine);
    if i = 0 then
      Result := CurrLine
    else
      Result := Copy(CurrLine, 1, i-1);
  end
  else
  begin
    j := 0;
    k := 0;
    for i := 0 to n do
    begin
      j := j+k+1;
      k := 1;
      while CurrLine[j+k] <> ' ' do
        Inc(k);
    end;
    Result := Copy(CurrLine, j, k);
  end;
  Result := UpperCase(Result);
end;

function TConsole.Execute: String;
var
  Str: String;
begin
  Str := GetParam(0);
  if Str = 'OPENGL' then
  begin
    Log('OpenGL implementation informations:');
    Log('Version: ' + glGetString(GL_VERSION));
    Log('Renderer: ' + glGetString(GL_RENDERER));
    Log('Vendor: ' + glGetString(GL_VENDOR));
    Log('Available extensions:');
    Str := glGetString(GL_EXTENSIONS);
    while Str <> '' do
    begin
      Log(Copy(Str, 1, Pos(' ', Str)-1));
      Delete(Str, 1, Pos(' ', Str));
    end;
  end;
  if Str = 'BSPHERE' then
  begin
    Str := GetParam(1);
    if Str = 'ON' then
    begin
      ShowBSpheres := True;
      Str := '';
    end;
    if Str = 'OFF' then
    begin
      ShowBSpheres := False;
      Str := '';
    end;
  end;
  if Str = 'LOAD' then
  begin
    Str := GetParam(1);
    if Str = 'SKYBOX' then
    begin
      TSkybox.Create(GetParam(2));
      Str := '';
    end;
    if Str = 'LANDSCAPE' then
    begin
      TLandscape.Create(GetParam(2), GetParam(3));
      Str := '';
    end;
    if Str = 'MODEL' then
    begin
      Scene.LoadModel(GetParam(2));
      Str := '';
    end;
    if Str = 'OBJECT' then
    begin
      TObj.Create(StrToInt(GetParam(2)), Vector(StrToFloat(GetParam(3)),
        StrToFloat(GetParam(4)), StrToFloat(GetParam(5))));
      Str := '';
    end;
  end;
  Result := Str;
end;

destructor TConsole.Destroy;
begin
  Log('TNT Console closed');
  Lines := nil;
  inherited Destroy;
end;

end.

