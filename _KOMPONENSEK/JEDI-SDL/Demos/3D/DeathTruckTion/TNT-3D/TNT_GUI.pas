unit TNT_GUI;

// Gestion du GUI
// --------------
//  Moteur de GUI (Graphical User Interface)

interface

uses
  SDL, TNT_Texture, TNT_HUD, TNT_Font;

type
  TMethod = procedure of object;

  TColor = record
    r, g, b, a: Single;
  end;

  TPic = record
    FileName: String;
    Texture: TTexture;
  end;

  PTextField = ^TTextField;
  TTextField = record
    Text: String;
    Position: Cardinal;
  end;

  TGUI = class(THUD)
  private
    StartTime: Uint32;
    OnGUIRender: TMethod;
    PicTable: array of TPic;
    CursorPic: String;
    CursSizeX, CursSizeY: Integer;
    CursorX, CursorY: Integer;
    SrcColor, DstColor, BorderSrcColor, BorderDstColor: TColor;
    BorderSize: Single;
    Focus: PTextField;
  protected
    procedure SetCursor(FileName: String; sX, sY, X, Y: Integer);
    procedure MoveCursor(X, Y: Integer);
    procedure OnRender; override;
    procedure OnBeforeGUI; virtual;
    procedure OnAfterGUI; virtual;
    function TimeElapsed: Single;
    procedure NextScreen(Addr: TMethod);
    procedure HideGUI;
    procedure ShowGUI;
    procedure ClearScreen(r, g, b: Single);
    procedure SetSrcColor(r, g, b, a: Single);
    procedure SetDstColor(r, g, b, a: Single);
    procedure SetBorderSize(n: Single);
    procedure SetBorderSrcColor(r, g, b, a: Single);
    procedure SetBorderDstColor(r, g, b, a: Single);
    procedure SetInterpolateColor(Src, Dst: TColor; Coeff: Single);
    procedure DrawBorder(Left, Up, Right, Down, Coeff: Single);
    procedure DrawRect(Left, Up, Right, Down, Coeff: Single);
    procedure Draw(X1, Y1, X2, Y2, dstX1, dstY1, dstX2, dstY2, Time: Single;
      PicName, PicMouseOn: String);
    procedure TextField(Left, Up, Right, Down, Time: Single; TextField: PTextField);
    procedure Text(Left, Up, Size, Time: Single; FontSet: Cardinal; Text: String);
    function MouseOnRect(Left, Up, Right, Down: Single): Boolean;
    procedure LoadTex(PicName: String);
    procedure SetFocus(TextField: PTextField);
    procedure SendChar(Ch: Char);
    procedure SendLeft;
    procedure SendRight;
    procedure SendBack;
    procedure SendDel;
  end;

implementation

uses
  OpenGL12;

function Color(r, g, b, a: Single): TColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

procedure TGUI.SetSrcColor(r, g, b, a: Single);
begin
  SrcColor := Color(r, g, b, a);
end;

procedure TGUI.SetDstColor(r, g, b, a: Single);
begin
  DstColor := Color(r, g, b, a);
end;

procedure TGUI.SetBorderSrcColor(r, g, b, a: Single);
begin
  BorderSrcColor := Color(r, g, b, a);
end;

procedure TGUI.SetBorderDstColor(r, g, b, a: Single);
begin
  BorderDstColor := Color(r, g, b, a);
end;

procedure TGUI.SetBorderSize(n: Single);
begin
  BorderSize := n;
end;

function TGUI.TimeElapsed: Single;
begin
  Result := (SDL_GetTicks - StartTime) / 1000;
end;

procedure TGUI.NextScreen(Addr: TMethod);
begin
  OnGUIRender := Addr;
  StartTime := SDL_GetTicks;
end;

procedure TGUI.SetCursor(FileName: String; sX, sY, X, Y: Integer);
begin
  CursorPic := FileName;
  CursSizeX := sX;
  CursSizeY := sY;
  CursorX := X;
  CursorY := Y;
end;

procedure TGUI.MoveCursor(X, Y: Integer);
begin
  CursorX := X;
  CursorY := Y;
end;

procedure TGUI.OnRender;
begin
  OnBeforeGUI;
  OnGUIRender;
  OnAfterGUI;
  if CursorPic <> '' then
  begin
    SetSrcColor(1,1,1,1);
    SetBorderSize(0);
    Draw(CursorX, CursorY, CursorX+CursSizeX, CursorY+CursSizeY,
         CursorX, CursorY, CursorX+CursSizeX, CursorY+CursSizeY,
         0, CursorPic, '');
  end;
end;

procedure TGUI.OnBeforeGUI;
begin
end;

procedure TGUI.OnAfterGUI;
begin
end;

procedure TGUI.ClearScreen(r, g, b: GLfloat);
begin
  glClearColor(r, g, b, 1);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TGUI.SetInterpolateColor(Src, Dst: TColor; Coeff: Single);
var
  Color: TColor;
begin
  Color.r := Src.r + (Dst.r - Src.r) * Coeff;
  Color.g := Src.g + (Dst.g - Src.g) * Coeff;
  Color.b := Src.b + (Dst.b - Src.b) * Coeff;
  Color.a := Src.a + (Dst.a - Src.a) * Coeff;
  with Color do
    glColor4f(r, g, b, a);
end;

procedure TGUI.DrawBorder(Left, Up, Right, Down, Coeff: Single);
begin
  if BorderSize > 0 then
  begin
    SetInterpolateColor(BorderSrcColor, BorderDstColor, Coeff);
    glDisable(GL_TEXTURE_2D);
    glLineWidth(BorderSize);
    glBegin(GL_LINE_LOOP);
      glVertex2f(Left, Up);
      glVertex2f(Left, Down);
      glVertex2f(Right, Down);
      glVertex2f(Right, Up);
    glEnd;
    glLineWidth(1);
    glEnable(GL_TEXTURE_2D);
  end;
end;

procedure TGUI.DrawRect(Left, Up, Right, Down, Coeff: Single);
begin
  SetInterpolateColor(SrcColor, DstColor, Coeff);
  glDisable(GL_TEXTURE_2D);
  glRectf(Left, Down, Right, Up);
  glEnable(GL_TEXTURE_2D);
end;

procedure TGUI.Text(Left, Up, Size, Time: Single; FontSet: Cardinal; Text: String);
var
  Coeff: Single;
begin
  Coeff := 0;
  if Time > 0 then
    Coeff := TimeElapsed/Time;
  if Coeff > 1 then
    Coeff := 1;
  SetInterpolateColor(SrcColor, DstColor, Coeff);
  Font.Print(Left, Up, Size, FontSet, Text);
end;

procedure TGUI.TextField(Left, Up, Right, Down, Time: Single; TextField: PTextField);
var
  Coeff: Single;
begin
  Coeff := 0;
  if Time > 0 then
    Coeff := TimeElapsed/Time;
  if Coeff > 1 then
    Coeff := 1;
  DrawRect(Left, Up, Right, Down, Coeff);
  if Focus = TextField then
    DrawBorder(Left, Up, Right, Down, Coeff);
  glColor4f(1,1,1,1);
  with TextField^ do
  begin
    Font.Print(Left+BorderSize, Up+BorderSize, 1, 0, Text);
    if (Focus = TextField) and ((SDL_GetTicks mod 1000) < 500) then
      Font.Print(Left+BorderSize, Up+BorderSize, 1, 0, Copy(Text, 1, Position) + '|');
  end;
end;

procedure TGUI.LoadTex(PicName: String);
begin
  SetLength(PicTable, Length(PicTable)+1);
  with PicTable[Length(PicTable)-1] do
  begin
    FileName := PicName;
    Texture := TTexture.Create('gui/'+PicName, GL_CLAMP, GL_LINEAR, True);
  end;
end;

procedure TGUI.Draw(X1, Y1, X2, Y2, dstX1, dstY1, dstX2, dstY2,
  Time: Single; PicName, PicMouseOn: String);
var
  i: Integer;
  Coeff, Left, Up, Right, Down: Single;
begin
  Coeff := 0;
  if Time > 0 then
    Coeff := TimeElapsed/Time;
  if Coeff > 1 then
    Coeff := 1;

  Left := X1 + (dstX1-X1)*Coeff;
  Up := Y1 + (dstY1-Y1)*Coeff;
  Right := X2 + (dstX2-X2)*Coeff;
  Down := Y2 + (dstY2-Y2)*Coeff;

  if PicName <> '' then
  begin
    if (PicMouseOn <> '') and MouseOnRect(Left, Up, Right, Down) then
      PicName := PicMouseOn;
    i := 0;
    while (i<Length(PicTable)) and (PicTable[i].FileName<>PicName) do
      Inc(i);
    if i = Length(PicTable) then
      LoadTex(PicName);
    if PicTable[i].Texture.Bpp = 4 then
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
    else
    	glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    PicTable[i].Texture.Use(GL_MODULATE);
    SetInterpolateColor(SrcColor, DstColor, Coeff);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex2f(Left, Up);
      glTexCoord2f(0, 1); glVertex2f(Left, Down);
      glTexCoord2f(1, 1); glVertex2f(Right, Down);
      glTexCoord2f(1, 0); glVertex2f(Right, Up);
    glEnd;
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  end
  else
    DrawRect(Left, Up, Right, Down, Coeff);
  DrawBorder(Left, Up, Right, Down, Coeff);
end;

function TGUI.MouseOnRect(Left, Up, Right, Down: Single): Boolean;
begin
  Result := (CursorX >= Left) and (CursorX <= Right) and
            (CursorY >= Up) and (CursorY <= Down);
end;

procedure TGUI.HideGUI;
var
  i: Integer;
begin
  for i := 0 to Length(PicTable)-1 do
    PicTable[i].Texture.Free;
  PicTable := nil;
  Visible := False;
end;

procedure TGUI.ShowGUI;
begin
  Visible := True;
end;

procedure TGUI.SetFocus(TextField: PTextField);
begin
  Focus := TextField;
end;

procedure TGUI.SendChar(Ch: Char);
begin
  with Focus^ do
  begin
    Inc(Position);
    Insert(Ch, Text, Position);
  end;
end;

procedure TGUI.SendLeft;
begin
  with Focus^ do
    if Position > 0 then
      Dec(Position);
end;

procedure TGUI.SendRight;
begin
  with Focus^ do
    if Position < Cardinal(Length(Text)) then
      Inc(Position);
end;

procedure TGUI.SendBack;
begin
  with Focus^ do
    if Position > 0 then
    begin
      Delete(Text, Position, 1);
      Dec(Position);
    end;
end;

procedure TGUI.SendDel;
begin
  with Focus^ do
    if Position < Cardinal(Length(Text)) then
      Delete(Text, Position+1, 1);
end;

end.

