unit Main;

interface

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  Classes,
  SysUtils,
{$IFDEF VCL}
  Controls,
  Forms,
  Dialogs,
  Graphics,
  StdCtrls,
  ComCtrls,
  Menus,
  ExtCtrls,
{$ENDIF}
{$IFDEF CLX}
  QControls,
  QForms,
  QDialogs,
  QGraphics,
  QExtCtrls,
  QMenus,
  QTypes,
  QStdCtrls,
{$ENDIF}
  SDL,
  SDL_Net;

type
  TForm1 = class( TForm )
    MainMenu : TMainMenu;
    miFile : TMenuItem;
    miConnect : TMenuItem;
    miDisconnect : TMenuItem;
    N1 : TMenuItem;
    Exit1 : TMenuItem;
    Timer : TTimer;
    mmoConversation : TMemo;
    pnlBottom : TPanel;
    edtMessage : TEdit;
    btnSend : TButton;
    procedure btnSendClick( Sender : TObject );
    procedure FormCreate( Sender : TObject );
    procedure Exit1Click( Sender : TObject );
    procedure Connect1Click( Sender : TObject );
    procedure FormDestroy( Sender : TObject );
    procedure TimerTimer( Sender : TObject );
    procedure miDisconnectClick( Sender : TObject );
    procedure FormClose( Sender : TObject; var Action : TCloseAction );
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddMessage( aFrom, aText : string; Col : TColor; Style : TFontStyles );
  end;

var
  Form1 : TForm1;
  UserName, From, Data, mText : string;
  Port : Word;
  Host : string;
  IP : TIPAddress;
  SocketSet : PSDLNet_SocketSet;
  Server : PTCPSocket;
  NumReady : Integer;
  Connected : Boolean = False;


implementation

uses
  TCPUtils;

{$IFDEF VCL}
{$R *.DFM}
{$ENDIF}
{$IFDEF CLX}
{$R *.xfm}
{$ENDIF}

{ TForm1 }

procedure TForm1.AddMessage( aFrom, aText : string; Col : TColor; Style : TFontStyles );
begin
  with mmoConversation do
  begin
    Lines.Add( ' < ' + aFrom + ' > ' + aText );
  end;
end;

procedure TForm1.btnSendClick( Sender : TObject );
begin
  if Connected then
  begin
    SendMessage( Server, edtMessage.Text );
    edtMessage.Text := EmptyStr;
  end;
end;

procedure TForm1.FormCreate( Sender : TObject );
begin
  // init SDL - for error reporting
  if SDL_Init( 0 ) <> 0 then
    Exit;
  // init SDL networking
  if SDLNet_Init <> 0 then
    Exit;
  // create a socket set
  SocketSet := SDLNet_AllocSocketSet( 1 );
end;

procedure TForm1.Exit1Click( Sender : TObject );
begin
  Close;
end;

procedure TForm1.Connect1Click( Sender : TObject );
begin
  // connect to host
  UserName := InputBox( 'UserName', 'Enter Username', '' );
  Host := InputBox( 'Host', 'Enter Host Name', 'localhost' );
  Port := StrToInt( InputBox( 'Port', 'Enter Port', '7777' ) );
  // resolve the host name into an IP address
  if SDLNet_ResolveHost( IP, PChar( Host ), Port ) = 0 then
  begin
    // open the Server Port
    Server := SDLNet_TCP_Open( IP );
    if Server <> nil then
    begin
      // add the server socket to the socket set
      if SDLNet_TCP_AddSocket( SocketSet, Server ) <> -1 then
      begin
        // send a message with name
        Connected := SendMessage( Server, UserName );
        miDisconnect.Enabled := Connected;
        miConnect.Enabled := not miDisconnect.Enabled;
      end;
    end;
  end;
end;

procedure TForm1.FormDestroy( Sender : TObject );
begin
  SDLNet_FreeSocketSet( SocketSet );
  SDLNet_Quit;
  SDL_Quit;
end;

procedure TForm1.TimerTimer( Sender : TObject );
begin
  if Connected then
  begin
    // if connected check to see if any sockets are ready
    NumReady := SDLNet_CheckSockets( SocketSet, 100 );
    // if sockets are ready and its the server socket
    if ( NumReady > 0 ) and SDLNet_SocketReady( PSDLNet_GenericSocket( Server ) ) then
    begin
      // get the message from the server
      if ReadMessage( Server, Data ) then
      begin
        // get the name and data
        if Data <> '#Full' then
        begin
          From := Copy( Data, 0, LastDelimiter( '>', Data ) );
          From := StringReplace( From, '<', '', [ rfReplaceAll ] );
          From := StringReplace( From, '>', '', [ rfReplaceAll ] );
          mText := Copy( Data, LastDelimiter( '>', Data ) + 1, Length( Data ) );
          if From = UserName then
            AddMessage( From, mText, clBlue, [ ] )
          else
            AddMessage( From, mText, clRed, [ ] );
        end
        else
        begin
          AddMessage( '', 'Server Full', clRed, [ ] );
          miDisconnectClick( Sender );
        end;
      end;
    end;
  end;
end;

procedure TForm1.miDisconnectClick( Sender : TObject );
begin
  if Connected then
  begin
    Connected := False;
    // send the /Q message to quit
    SendMessage( Server, '/Q' );
    // remove the socket from the socket set
    SDLNet_TCP_DelSocket( SocketSet, server );
    // close the socket
    SDLNet_TCP_Close( Server );
    miDisconnect.Enabled := Connected;
    miConnect.Enabled := not miDisconnect.Enabled;
  end;
end;

procedure TForm1.FormClose( Sender : TObject; var Action : TCloseAction );
begin
  // if connected on exit disconnect
  if Connected then miDisconnect.Click;
end;

end.

