unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, OleCtrls, SHDocVw;

type
  TfrmWebBrowser = class(TForm)
    WebBrowser1: TWebBrowser;
    txtLocation: TEdit;
    Label1: TLabel;
    Button1: TButton;
    StatusBar1: TStatusBar;
    procedure FormResize(Sender: TObject);
    procedure txtLocationKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure WebBrowser1DownloadBegin(Sender: TObject);
    procedure WebBrowser1DownloadComplete(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure GotoPage(ReqUrl: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWebBrowser: TfrmWebBrowser;

implementation

{$R *.DFM}

procedure TfrmWebBrowser.FormResize(Sender: TObject);
begin
 WebBrowser1.Top := 28;
 WebBrowser1.Left := 0;
 WebBrowser1.Width := frmWebBrowser.ClientWidth;
 WebBrowser1.Height := frmWebBrowser.ClientHeight - WebBrowser1.Top - StatusBar1.Height;
end;

procedure TfrmWebBrowser.txtLocationKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
  GotoPage(txtLocation.Text);
end;

procedure TfrmWebBrowser.FormShow(Sender: TObject);
begin
 txtLocation.Text := ExtractFilePath(ParamStr(0)) + 'default.html';
 GotoPage(txtLocation.Text);
end;

procedure TfrmWebBrowser.WebBrowser1DownloadBegin(Sender: TObject);
begin
 StatusBar1.Panels[0].Text := 'Oldal letöltése: ' + WebBrowser1.LocationURL;
end;

procedure TfrmWebBrowser.WebBrowser1DownloadComplete(Sender: TObject);
begin
 StatusBar1.Panels[0].Text := 'Kész.';
end;

procedure TfrmWebBrowser.Button1Click(Sender: TObject);
begin
 WebBrowser1.Stop;
end;

procedure TfrmWebBrowser.GotoPage(ReqUrl: string);
begin
 WebBrowser1.Navigate(ReqUrl,EmptyParam,EmptyParam,EmptyParam,EmptyParam);
end;

end.
