unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI;

const wm_IconMessage = wm_User;

type
  TForm2 = class(TForm)
    procedure Talcaraules;
    procedure Tunesatalcarol;
  private
    { Private declarations }
  public
    procedure IconTray(var Msg: TMessage);message wm_IconMessage;
  end;

var
  Form2: TForm2;
  nid: TNotifyIconData;

implementation

uses Unit1;

{$R *.DFM}

{ TForm2 }

procedure TForm2.IconTray(var Msg: TMessage);
begin
 if Msg.lParam = wm_lButtonDblClk then
 begin
  Form1.Show;
 end
 else if Msg.lParam = wm_rButtonDblClk then
  Application.Terminate
 else if Msg.lParam = wm_lButtonDown then
  Form1.ReReadData;
end;

procedure TForm2.Talcaraules;
begin
 nid.cbSize := SizeOf(nid);
 nid.Wnd := Handle;
 nid.uID := 1;
 nid.uCallbackMessage := wm_IconMessage;
 nid.hIcon := Application.Icon.Handle;
 nid.szTip := 'Noah LastMeet';
 nid.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
 Shell_NotifyIcon(NIM_ADD,@nid);
end;

procedure TForm2.Tunesatalcarol;
begin
 nid.uFlags := 0;
 Shell_NotifyIcon(NIM_DELETE,@nid);
end;

end.
 