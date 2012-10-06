unit Main;

interface

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  SysUtils,
  Classes,
{$IFDEF CLX}
  Types,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QExtCtrls,
  QStdCtrls,
{$ENDIF}
{$IFDEF VCL}
  Forms,
  Graphics,
  Dialogs,
  StdCtrls,
  Controls,
  ExtCtrls,
{$ENDIF}
  SDLMPEGPanel;

type
  TForm1 = class( TForm )
    Panel1 : TPanel;
    Panel2 : TPanel;
    Button1 : TButton;
    Button2 : TButton;
    Button3 : TButton;
    Panel3 : TPanel;
    CheckBox1 : TCheckBox;
    OpenDialog: TOpenDialog;
    SDLMPEGPanel: TSDLMPEGPanel;
    procedure Button1Click( Sender : TObject );
    procedure Button2Click( Sender : TObject );
    procedure Button3Click( Sender : TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1 : TForm1;

implementation

{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}

{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TForm1.Button1Click( Sender : TObject );
begin
  with OpenDialog do
  begin
    if Execute then
    begin
      SDLMPEGPanel.MPEGFile := FileName;
      SDLMPEGPanel.Sound := CheckBox1.Checked;
      SDLMPEGPanel.Play;
    end;
  end;
end;

procedure TForm1.Button2Click( Sender : TObject );
begin
  SDLMPEGPanel.Pause;
end;

procedure TForm1.Button3Click( Sender : TObject );
begin
  SDLMPEGPanel.Stop;
end;

end.

 