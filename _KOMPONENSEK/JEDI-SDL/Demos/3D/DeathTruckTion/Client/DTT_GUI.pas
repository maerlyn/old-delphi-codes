unit DTT_GUI;

interface

uses
  TNT_GUI, TNT_Texture;

type
  DTTGUI = class(TGUI)
  private
    BackgroundTex: TTexture;
    procedure Plasma;
    procedure Intro;
    procedure Menu;
    procedure Loading;
    procedure Options;
    procedure Maps;
    procedure Credits1;
    procedure Credits2;
    procedure Credits3;
    procedure Credits4;
    procedure Credits5;
    procedure Beep;
    procedure Quit;
  published
    procedure OnBeforeGUI; override;
//    procedure OnAfterGUI; override;
  public
    constructor Create; override;
    procedure guiInput;
    procedure ExitGame;
  end;

var
  GUI: DTTGUI;
  Done: Boolean;

implementation

uses
  OpenGL12, SDL, DTT_Game, DTT_Input, DTT_Sound, TNT_3D, TNT_Console;

const
  SCR_NULL = 0;
  SCR_INTRO = 1;
  SCR_MENU = 2;
  SCR_LOADING = 3;
  SCR_OPTIONS = 4;
  SCR_MAPS = 5;
  SCR_CREDITS1 = 6;
  SCR_CREDITS2 = 7;
  SCR_CREDITS3 = 8;
  SCR_CREDITS4 = 9;
  SCR_CREDITS5 = 10;

var
  point: array [0..36, 0..36, 1..3] of Single;
  Music: TMusic;
  Click: TSample;
  BeepPlaying: Boolean = false;
  CurrentScreen: Integer;
  PreviousScreen: Integer;
  TextFieldServer, TextFieldName: TTextField;
  MapFilename: string;

constructor DTTGUI.Create;
begin
  inherited Create;

  MapFilename := 'bynight.txt';

  LoadTex('back.tex');
  LoadTex('credits.tex');
  LoadTex('creditsact.tex');
  LoadTex('cursor.tex');
  LoadTex('dtt.tex');

  LoadTex('loading.tex');
  LoadTex('options.tex');
  LoadTex('optionsact.tex');
  LoadTex('optionstre.tex');
  LoadTex('play.tex');
  LoadTex('playact.tex');
  LoadTex('quit.tex');
  LoadTex('quitact.tex');
  LoadTex('tnt.tex');
  LoadTex('mainmenu.tex');
  LoadTex('mainmenuact.tex');

  LoadTex('credits/actarus.tex');
  LoadTex('credits/and.tex');
  LoadTex('credits/beloved.tex');
  LoadTex('credits/broughtby.tex');
  LoadTex('credits/episupe2.tex');
  LoadTex('credits/including.tex');
  LoadTex('credits/nathkris.tex');
  LoadTex('credits/nitrogen.tex');
  LoadTex('credits/nobodii.tex');
  LoadTex('credits/spiritus.tex');
  LoadTex('credits/thanksto.tex');
  LoadTex('credits/vga.tex');

  LoadTex('maps/maps.tex');
  LoadTex('maps/mapsact.tex');
  LoadTex('maps/mapstre.tex');
  LoadTex('maps/choice.tex');
  LoadTex('maps/bynight.tex');
  LoadTex('maps/bynightact.tex');
  LoadTex('maps/delirious.tex');
  LoadTex('maps/deliriousact.tex');

  Music := TMusic.Create('menutune.s3m');
  Click := TSample.Create('beep.wav');

  glEnable(GL_BLEND);
  BackgroundTex := TTexture.Create('gui/back.tex', GL_REPEAT, GL_LINEAR, False);

  with TextFieldServer do
  begin
    Text := 'localhost';
    Position := Length(Text);
  end;
  with TextFieldName do
  begin
    Text := 'NoName';
    Position := Length(Text);
  end;
  SetFocus(@TextFieldServer);

  NextScreen(Intro);
end;

procedure DTTGUI.Intro;
begin

  CurrentScreen := SCR_INTRO;

  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);

  // tnt.tex, 718*226 600*189 old 600*210
  Draw(320, 240, 320, 240, 20, 135, 600+20, 135+189, 2, 'tnt.tex', '');

  if TimeElapsed > 5 then
  begin
    SetCursor('cursor.tex', 23, 32, Input.Mouse.X, Input.Mouse.Y);
    Music.Play;
    NextScreen(Menu);
  end;

end;

procedure DTTGUI.Menu;
begin

  CurrentScreen := SCR_MENU;

  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);

  case PreviousScreen of
    SCR_OPTIONS:
    begin
      SetSrcColor(1,1,1,0.2);
      Draw(114, 142, 114+413, 142+256, 160, 140, 480, 400, 0.2, '', '');
    end;
    SCR_CREDITS5:
    begin
      SetSrcColor(1,1,1,0.2);
      Draw(20, 80, 620, 460, 160, 140, 480, 400, 0.2, '', '');
    end;
  else
    Draw(320, 240, 320, 240, 160, 140, 480, 400, 0.2, '', '');
  end;


  // build/version text
  SetSrcColor(1,1,1,1);
  Text(380, 460, 1, 0, 0, 'v1.0 Thu Jun 14 16:30:00 CEST 2001');

  // dtt.tex, 1141*88 (571*44)
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);

  case PreviousScreen of
    SCR_OPTIONS:
      Draw(35, 49, 35+571, 49+44, 35, 48, 35+571, 48+44, 0.2, 'dtt.tex', '');
    SCR_CREDITS5:
      Draw(35, 18, 35+571, 18+44, 35, 48, 35+571, 48+44, 0.2, 'dtt.tex', '');
  else
    Draw(35, 48, 35+571, 48+44, 300, 300, 444, 344, 0, 'dtt.tex', '');
  end;

  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);

  // play.tex, playact.tex, 103*48
  Draw(268, 160, 268+103, 160+48, 268, 160, 268+103, 160+48, 0.5, 'play.tex', 'playact.tex');
  // options.tex, optionsact.tex, 172*48
  Draw(234, 220, 234+172, 220+48, 234, 220, 234+172, 220+48, 0.5, 'options.tex', 'optionsact.tex');
  // credits.tex, creditsact.tex, 158*40
  Draw(241, 280, 241+158, 280+40, 241, 280, 241+158, 280+40, 0.5, 'credits.tex', 'creditsact.tex');
  // quit.tex, quitact.tex, 99*43
  Draw(270, 340, 270+99, 340+43, 270, 340, 270+99, 340+43, 0.5, 'quit.tex', 'quitact.tex');

end;

procedure DTTGUI.Loading;
begin

  CurrentScreen := SCR_LOADING;

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(160, 140, 480, 400, 160, 200, 480, 340, 0.2, '', '');

  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 48, 35+571, 48+44, 300, 300, 444, 344, 0, 'dtt.tex', '');

  // loading.tex, 216*48
  Draw(212, 250, 212+216, 250+48, 212, 250, 212+216, 250+48, 0.4, 'loading.tex', '');

  if TimeElapsed > 1 then
  begin
    Music.Stop;
    BackgroundTex.Free;
    HideGUI;
    DTT_Init(TextFieldName.Text, TextFieldServer.Text, MapFilename);
  end;

end;

procedure DTTGUI.Options;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);

  if PreviousScreen = SCR_MAPS then
    Draw(165, 114, 165+310, 187+266-30, 114, 142, 114+413, 142+256, 0.2, '', '')
  else
    Draw(160, 140, 480, 400, 114, 142, 114+413, 142+256, 0.2, '', '');

  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  if PreviousScreen = SCR_MAPS then
    Draw(35, 35, 35+571, 35+44, 35, 49, 35+571, 49+44, 0.2, 'dtt.tex', '')
  else
    Draw(35, 48, 35+571, 48+44, 35, 49, 35+571, 49+44, 0.2, 'dtt.tex', '');

  // optionstre.tex, 172*51
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(234, 160, 234+172, 160+51, 234, 160, 234+172, 160+51, 0.4, 'optionstre.tex', '');

  // maps.tex, 123*48
  Draw(140, 323, 140+123, 323+48, 140, 323, 140+123, 323+48, 0.4, 'maps/maps.tex', 'maps/mapsact.tex');

  // mainmenu.tex, 118*61
  Draw(380, 315, 380+118, 315+61, 380, 315, 380+118, 315+61, 0.4, 'mainmenu.tex', 'mainmenuact.tex');

  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(2);
  TextField(150+94, 180+55, 400+94, 200+55, 0.4, @TextFieldServer);
  TextField(225+94, 220+55, 400+94, 240+55, 0.4, @TextFieldName);

  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Text(50+94, 177+55, 1.5, 0.4, 1, 'Server:');
  Text(50+94, 217+55, 1.5, 0.4, 1, 'Player Name:');

  if TimeElapsed > 0.4 then
    CurrentScreen := SCR_OPTIONS
  else
    CurrentScreen := SCR_NULL;

end;

procedure DTTGUI.Maps;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(114, 142, 114+413, 142+256, 165, 114, 165+310, 187+266-30, 0.2, '', '');

  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 48, 35+571, 48+44, 35, 35, 35+571, 35+44, 0.2, 'dtt.tex', '');

  // mapstre.tex, 123*51
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(259, 130, 259+123, 130+51, 259, 130, 259+123, 130+51, 0.4, 'maps/mapstre.tex', '');

  // choice.tex, 174*64
  Draw(233, 195, 233+174, 195+64, 233, 195, 233+174, 195+64, 0.4, 'maps/choice.tex', '');

  // bynight.tex, 179*48
  Draw(231, 290, 231+179, 290+48, 231, 290, 231+179, 290+48, 0.4, 'maps/bynight.tex', 'maps/bynightact.tex');

  // delirious.tex, 194*40
  Draw(223, 350, 223+194, 350+40, 223, 350, 223+194, 350+40, 0.4, 'maps/delirious.tex', 'maps/deliriousact.tex');


  if TimeElapsed > 0.4 then
    CurrentScreen := SCR_MAPS
  else
    CurrentScreen := SCR_NULL;

end;


procedure DTTGUI.Credits1;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(160, 140, 480, 400, 20, 80, 620, 460, 0.2, '', '');
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 48, 35+571, 48+44, 35, 18, 35+571, 18+44, 0.2, 'dtt.tex', '');

  // broughtby.tex, 458*96
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(-458, 125, 0, 125+96, 91, 125, 91+458, 125+96, 1, 'credits/broughtby.tex', '');

  // nitrogen.tex, 195*40
  Draw(20, 125+96, 20, 125+96, 80, 260, 80+195, 260+40, 1, 'credits/nitrogen.tex', '');

  // spiritus.tex, 174*48
  Draw(620, 125+96, 620, 125+96, 386, 260, 386+174, 260+48, 1, 'credits/spiritus.tex', '');

  // actarus.tex, 177*40
  Draw(20, 440, 20, 440, 80+9, 350, 80+9+177, 350+40, 1, 'credits/actarus.tex', '');

  // vga.tex, 241*40
  Draw(620, 440, 620, 440, 386-34, 350, 386+241-34, 350+40, 1, 'credits/vga.tex', '');

  if TimeElapsed > 1 then
    CurrentScreen := SCR_CREDITS1
  else
    CurrentScreen := SCR_NULL;

  if TimeElapsed > 5 then
  begin
    PreviousScreen := CurrentScreen;
    NextScreen(Credits2);
  end;

end;

procedure DTTGUI.Credits2;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(20, 80, 620, 460, 20, 80, 620, 460, 0, '', '');
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 18, 35+571, 18+44, 35, 18, 35+571, 18+44, 0, 'dtt.tex', '');

  // broughtby.tex, 458*96
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,0);
  Draw(91, 125, 91+458, 125+96, 91, 480, 91+458, 480+96, 0.5, 'credits/broughtby.tex', '');

  // nitrogen.tex, 195*40
  Draw(80, 260, 80+195, 260+40, -195, 260, 0, 260+40, 0.5, 'credits/nitrogen.tex', '');

  // spiritus.tex, 174*48
  Draw(386, 260, 386+174, 260+48, 640, 260, 640+174, 260+48, 0.5, 'credits/spiritus.tex', '');

  // actarus.tex, 177*40
  Draw(80+9, 350, 80+9+177, 350+40, -177, 350, 0, 350+40, 0.5, 'credits/actarus.tex', '');

  // vga.tex, 241*40
  Draw(386-34, 350, 386+241-34, 350+40, 640, 350, 640+241, 350+40, 0.5, 'credits/vga.tex', '');


  if TimeElapsed > 0.5 then
    begin
    CurrentScreen := SCR_CREDITS2;
    PreviousScreen := CurrentScreen;
    NextScreen(Credits3);
    end
  else
    CurrentScreen := SCR_NULL;

end;


procedure DTTGUI.Credits3;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(20, 80, 620, 460, 20, 80, 620, 460, 0, '', '');
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 18, 35+571, 18+44, 35, 18, 35+571, 18+44, 0, 'dtt.tex', '');

  // thanksto.tex, 378*48
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(640, 125, 640+378, 125+48, 131, 125, 131+378, 125+48, 1, 'credits/thanksto.tex', '');

  // nobodii.tex, 357*184
  Draw(142, 480, 142+378, 480+184, 142, 200, 142+378, 200+184, 1, 'credits/nobodii.tex', '');


  if TimeElapsed > 1 then
    CurrentScreen := SCR_CREDITS3
  else
    CurrentScreen := SCR_NULL;

  if TimeElapsed > 5 then
  begin
    PreviousScreen := CurrentScreen;
    NextScreen(Credits4);
  end;

end;


procedure DTTGUI.Credits4;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(20, 80, 620, 460, 20, 80, 620, 460, 0, '', '');
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 18, 35+571, 18+44, 35, 18, 35+571, 18+44, 0, 'dtt.tex', '');

  // thanksto.tex, 378*48  (744*96)
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,0);
  Draw(131, 125, 131+378, 125+48, -52, 125-24, 640+52, 125+48+24, 0.2, 'credits/thanksto.tex', '');

  // nobodii.tex, 357*184   (714*368)
  Draw(142, 200, 142+378, 200+184, -37, 200-92, 640+37, 200+184+92, 0.2, 'credits/nobodii.tex', '');


  if TimeElapsed > 0.5 then
    begin
    CurrentScreen := SCR_CREDITS4;
    PreviousScreen := CurrentScreen;
    NextScreen(Credits5);
    end
  else
    CurrentScreen := SCR_NULL;

end;

procedure DTTGUI.Credits5;
begin

  SetSrcColor(1,1,1,0.2);
  SetDstColor(1,1,1,0.2);
  SetBorderSrcColor(1,0,0,1);
  SetBorderDstColor(1,0,0,1);
  SetBorderSize(3);
  Draw(20, 80, 620, 460, 20, 80, 620, 460, 0, '', '');
  SetSrcColor(1,1,1,1);
  SetDstColor(1,1,1,1);
  SetBorderSize(0);
  Draw(35, 18, 35+571, 18+44, 35, 18, 35+571, 18+44, 0, 'dtt.tex', '');

  // and.tex, 130*40
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(255, 100, 255+130, 100+40, 255, 100, 255+130, 100+40, 0.5, 'credits/and.tex', '');

  // episupe2.tex, 489*48
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(76, 160, 76+489, 160+48, 76, 160, 76+489, 160+48, 0.5, 'credits/episupe2.tex', '');

  // including.tex, 197*48
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(222, 230, 222+197, 230+48, 222, 230, 222+197, 230+48, 1, 'credits/including.tex', '');

  // nathkris.tex, 413*40
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(114, 290, 114+413, 290+40, 114, 290, 114+413, 290+40, 1, 'credits/nathkris.tex', '');

  // beloved.tex, 457*40
  SetSrcColor(1,1,1,0);
  SetDstColor(1,1,1,1);
  Draw(92, 350, 92+457, 350+40, 92, 350, 92+457, 350+40, 1.5, 'credits/beloved.tex', '');

  if TimeElapsed > 2 then
    CurrentScreen := SCR_CREDITS5
  else
    CurrentScreen := SCR_NULL;

  if TimeElapsed > 6 then
  begin
    PreviousScreen := CurrentScreen;
    NextScreen(Menu);
  end;

end;



procedure DTTGUI.ExitGame;
begin
  DTT_Clean;
  ShowGUI;
  glEnable(GL_BLEND);
  BackgroundTex := TTexture.Create('gui/back.tex', GL_REPEAT, GL_LINEAR, False);
  SetCursor('cursor.tex', 23, 32, Input.Mouse.X, Input.Mouse.Y);
  Music.Play;
  NextScreen(Menu);
end;

procedure DTTGUI.Quit;
begin
  SetCursor('', 0, 0, 0, 0);
  HideGUI;
  BackgroundTex.Free;
  SDL_Delay(500);
  Music.Free;
  Click.Free;
  Done := True;
end;

{procedure DTTGUI.OnAfterGUI;
begin // code executé apres le gui et avant le curseur
end;}

procedure DTTGUI.guiInput;
begin

  with Input.Keyboard do
  begin
    case NewKey of
      SDLK_LEFT: SendLeft;
      SDLK_RIGHT: SendRight;
      SDLK_BACKSPACE: SendBack;
      SDLK_DELETE: SendDel;
    end;
    if (Character >= 'a') and (Character <= 'z') or
       (Character >= 'A') and (Character <= 'Z') or
       (Character >= '0') and (Character <= '9') or
       (Character = ' ') or (Character = '.') then
      SendChar(Character);
    Character := #0;
    NewKey := 0;
  end;

  with Input.Mouse do
  begin
    MoveCursor(X, Y);
    if not Left then
      BeepPlaying := false;
    if Left then
    begin
      if CurrentScreen = SCR_MENU then
      begin
        if MouseOnRect(268, 160, 268+103, 160+48) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          SetCursor('', 0, 0, 0, 0);
          NextScreen(Loading);
        end;
        if MouseOnRect(234, 220, 234+172, 220+48) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          NextScreen(Options);
        end;
        if MouseOnRect(241, 280, 241+158, 280+40) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          NextScreen(Credits1);
        end;
        if MouseOnRect(270, 340, 270+99, 340+43) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          NextScreen(Quit);
        end;
      end;
      if CurrentScreen = SCR_OPTIONS then
      begin
        if MouseOnRect(150+94, 180+55, 400+94, 200+55) then
          SetFocus(@TextFieldServer);
        if MouseOnRect(225+94, 220+55, 400+94, 240+55) then
          SetFocus(@TextFieldName);
        if MouseOnRect(140, 323, 140+123, 323+48) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          NextScreen(Maps);
        end;
        if MouseOnRect(380, 315, 380+118, 315+61) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          NextScreen(Menu);
        end;
      end;
    if CurrentScreen = SCR_MAPS then
      begin
        if MouseOnRect(231, 300, 231+179, 300+48) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          MapFilename := 'bynight.txt';
          NextScreen(Options);
        end;
        if MouseOnRect(223, 360, 223+194, 360+40) then
        begin
          Beep;
          PreviousScreen := CurrentScreen;
          MapFilename := 'delirious.txt';
          NextScreen(Options);
        end;
      end;
    end;
  end;
end;

procedure DTTGUI.Beep;
begin
  if not BeepPlaying then
  begin
    Click.Play;
    BeepPlaying := true;
  end;
end;

procedure DTTGUI.OnBeforeGUI;
begin
  Plasma;
end;

procedure DTTGUI.Plasma;
var
  x, y, Frames: Integer;
  xf, xf2, yf, yf2: Single;
begin
  Begin3D;
  glDisable(GL_BLEND);
  Frames := Round((SDL_GetTicks/1000)*30);
  glTranslatef(0, 0, -12);
  glRotatef(-Frames/12, 0, 0, 1);
  BackgroundTex.Use(GL_REPLACE);
  glBegin(GL_QUADS);
    for x := 0 to 15 do
    begin
      xf := x/16;
      xf2 := (x+1)/16;
      for y := 0 to 15 do
      begin
        yf := y/16;
        yf2 := (y+1)/16;
        glTexCoord2f(xf, yf); glVertex3f(point[x,y,1], point[x,y,2], point[x,y,3]);
        glTexCoord2f(xf2, yf); glVertex3f(point[x+1,y,1], point[x+1,y,2], point[x+1,y,3]);
        glTexCoord2f(xf2, yf2); glVertex3f(point[x+1,y+1,1], point[x+1,y+1,2], point[x+1,y+1,3]);
        glTexCoord2f(xf, yf2); glVertex3f(point[x,y+1,1], point[x,y+1,2], point[x,y+1,3]);
      end;
    end;
  glEnd;
  for x := 0 to 16 do
    for y := 0 to 16 do
    begin
      point[x, y, 1] := x*2 - 16 + sin(Frames/60 + x*10*PI/360)*2;
      point[x, y, 2] := y*2 - 16 + cos(Frames/95 + x*10*2*PI/360)*8;
      point[x, y, 3] := sin(Frames/90 + x*10*2*PI/360)*2;
    end;
  glEnable(GL_BLEND);
  End3D;
end;

end.

