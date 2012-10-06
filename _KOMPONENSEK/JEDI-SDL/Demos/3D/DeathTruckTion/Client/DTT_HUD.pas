unit DTT_HUD;

interface

uses
  TNT_HUD, TNT_Texture;

type
  DTTHUD = class(THUD)
  private
    LifePic, SpeedPic: TTexture;
  public
    ShowStat: Boolean;
    constructor Create; override;
    procedure OnRender; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, OpenGL12, TNT_3D, TNT_Font, DTT_Console, DTT_Client, TNT_Vector,
  DTT_Truck;

constructor DTTHUD.Create;
begin
  inherited Create;
  LifePic := TTexture.Create('life.tex', GL_CLAMP, GL_LINEAR, True);
  SpeedPic := TTexture.Create('speed.tex', GL_CLAMP, GL_LINEAR, True);
end;

procedure DTTHUD.OnRender;
var
  P: TVector;
  i, po: Integer;
  n: Single;
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_BLEND);

  if ShowStat then
  begin
    glColor4f(1,1,1,1);
    Font.Print(10, 100, 1, 0, 'FPS: ' + IntToStr(Round(TNT.FPS)));
    Font.Print(10, 120, 1, 0, 'Tris: ' + IntToStr(TNT.Tris));
  end;

  glColor4f(1,1,1,0.7);

  glDisable(GL_TEXTURE_2D);
  glBegin(GL_TRIANGLES);
    n := 429 - (Client.PlayerTruck.Life/100) * (429-327);
    glColor3f(0,0,1); glVertex2f(75,n);
    glColor3f(0,0,0.5); glVertex2f(53,429);
    glColor3f(0,0,0.5); glVertex2f(99,429);
    n := 428 - Abs(Client.PlayerTruck.V) * (428-327);
    if n < 327 then n := 327;
    glColor3f(1,0,0); glVertex2f(566,n);
    glColor3f(0.5,0,0); glVertex2f(542,428);
    glColor3f(0.5,0,0); glVertex2f(588,428);
  glEnd;
  glEnable(GL_TEXTURE_2D);

  LifePic.Use(GL_REPLACE);
 // glColor4i(1, 1, 1, 1);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(30, 290);
    glTexCoord2f(0, 1); glVertex2f(30, 475);
    glTexCoord2f(1, 1); glVertex2f(120, 475);
    glTexCoord2f(1, 0); glVertex2f(120, 290);
  glEnd;
  SpeedPic.Use(GL_REPLACE);
 // glColor4i(1, 1, 1, 1);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0); glVertex2f(520, 290);
    glTexCoord2f(0, 1); glVertex2f(520, 475);
    glTexCoord2f(1, 1); glVertex2f(610, 475);
    glTexCoord2f(1, 0); glVertex2f(610, 290);
  glEnd;

  glColor4f(1, 1, 1, 1);

  for i:=1 to MAX_TRUCK do
  begin
    if (TruckTable[i] <> nil) and (TruckTable[i].Name <> Client.PlayerTruck.Name) then
    begin
      Client.PlayerTruck.Transform(TruckTable[i].Position, P);
      if P.z > 0 then
      begin
        po := Round(320*P.x/P.z+320);
        if po < 0 then
          Font.Print(0, 0, 1, 0, '<<' + TruckTable[i].Name)
        else if po > 560 then
          Font.Print(560, 0, 1, 0, TruckTable[i].Name + '>>')
        else
          Font.Print(po, 0, 1, 0, TruckTable[i].Name);
      end;
    end;
  end;

//  Font.Print(10, 460, 1, 1, 'Rockets: ' + IntToStr(Client.PlayerTruck.RocketsLeft));

  ConsoleDraw;

  glDisable(GL_BLEND);
end;

destructor DTTHUD.Destroy;
begin
  LifePic.Free;
  SpeedPic.Free;
  inherited Destroy;
end;

end.

