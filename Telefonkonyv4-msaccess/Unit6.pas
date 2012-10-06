unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Unit1;

type
  TfrmSplash = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.DFM}

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
 frmSplash.BorderStyle := bsNone;
 frmSplash.FormStyle := fsStayOnTop;
 Screen.Cursor := crHourGlass;
end;

procedure TfrmSplash.Timer1Timer(Sender: TObject);
begin
 if frmMainForm.MostmarBezarhatsz then
 begin
  Timer1.Enabled := false;
  frmMainForm.Show;
  frmSplash.Hide;
  Screen.Cursor := crDefault;
 end;
end;

end.
