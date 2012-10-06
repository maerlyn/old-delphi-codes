unit Eredmeny;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmEredmeny = class(TForm)
    Label1: TLabel;
    lblPont: TLabel;
    Label3: TLabel;
    lblOsszes: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblSzazalek: TLabel;
    Label8: TLabel;
    lblJegy: TLabel;
    Button1: TButton;
    procedure SzazalekEsJegyKalkulacio;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEredmeny: TfrmEredmeny;

implementation

{$R *.DFM}

{ TfrmEredmeny }

procedure TfrmEredmeny.SzazalekEsJegyKalkulacio;
var Pontok, Osszes: integer;
begin
 Pontok := StrToInt(lblPont.Caption);
 Osszes := StrToInt(lblOsszes.Caption);
 lblSzazalek.Caption := IntToStr(trunc(Pontok / Osszes * 100));
 lblJegy.Caption := '1';
 if StrToInt(lblSzazalek.Caption) > 60 then lblJegy.Caption := '2';
 if StrToInt(lblSzazalek.Caption) > 70 then lblJegy.Caption := '3';
 if StrToInt(lblSzazalek.Caption) > 80 then lblJegy.Caption := '4';
 if StrToInt(lblSzazalek.Caption) > 90 then lblJegy.Caption := '5';
 lblSzazalek.Caption := lblSzazalek.Caption + '%';
end;

procedure TfrmEredmeny.Button1Click(Sender: TObject);
begin
 frmEredmeny.Hide;
end;

end.
