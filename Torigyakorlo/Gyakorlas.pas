unit Gyakorlas;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Kerdes, MainForm, IniFiles;

type
  TfrmGyakorlas = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtEvszam: TEdit;
    txtEsemeny: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Delay(Seconds, MilliSec: word);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure UjKerdes;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

  TKerdes = record
    Letezik: longbool;
    Esemeny: string;
    Evszam: string;
  end;

var
  frmGyakorlas: TfrmGyakorlas;
  DBFile: TIniFile;
  KerdesSzama: integer;
  Kerdesek: array[1..65535] of TKerdes;
  KerdesekSzama: integer;
  Mode: integer;

implementation

{$R *.DFM}

procedure TfrmGyakorlas.Delay(Seconds, MilliSec: word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

procedure TfrmGyakorlas.Button2Click(Sender: TObject);
var Eljarasmutato: TNotifyEvent;
begin
 if Mode = 1 then
 begin                                 //én:évsz.;te:esemény
  if LowerCase(txtEsemeny.Text) = LowerCase(Kerdesek[KerdesSzama].Esemeny) then
  begin
   Button2.Caption := 'Helyes!';
   Button1.Enabled := false;
   Eljarasmutato := Button2.OnClick;
   Button2.OnClick := nil;
   Delay(2,500);
   Button1.Enabled := true;
   Button2.OnClick := Eljarasmutato;
   UjKerdes;
  end
  else
  begin
   Button2.Caption := 'Nem jó! Helyesen: ' + Kerdesek[KerdesSzama].Esemeny;
   Button1.Enabled := false;
   Eljarasmutato := Button2.OnClick;
   Button2.OnClick := nil;
   Delay(2,500);
   Button1.Enabled := true;
   Button2.OnClick := Eljarasmutato;
   UjKerdes;
  end;
 end
 else
 begin                                 //én:esemény;te:évsz.
  if LowerCase(txtEvszam.Text) = LowerCase(Kerdesek[KerdesSzama].Evszam) then
  begin
   Button2.Caption := 'Helyes!';
   Button1.Enabled := false;
   Eljarasmutato := Button2.OnClick;
   Button2.OnClick := nil;
   Delay(2,500);
   Button1.Enabled := true;
   Button2.OnClick := Eljarasmutato;
   UjKerdes;
  end
  else
  begin
   Button2.Caption := 'Nem jó! Helyesen: ' + Kerdesek[KerdesSzama].Evszam;
   Button1.Enabled := false;
   Eljarasmutato := Button2.OnClick;
   Button2.OnClick := nil;
   Delay(2,500);
   Button1.Enabled := true;
   Button2.OnClick := Eljarasmutato;
   UjKerdes;
  end;
 end;
end;

procedure TfrmGyakorlas.Button1Click(Sender: TObject);
begin
 frmGyakorlas.Hide;
end;

procedure TfrmGyakorlas.UjKerdes;
begin
 Button2.Caption := '&OK';
 repeat
  KerdesSzama := trunc(Random(KerdesekSzama) + 1);
 until Kerdesek[KerdesSzama].Letezik = true;
 if Mode = 1 then
 begin
  txtEvszam.Text := Kerdesek[KerdesSzama].Evszam;
  try
   txtEsemeny.Clear;
   txtEsemeny.SetFocus;
  except
  end;
 end
 else
 begin
  txtEsemeny.Text := Kerdesek[KerdesSzama].Esemeny;
  try
   txtEvszam.Clear;
   txtEvszam.SetFocus;
  except
  end; 
 end;
end;

procedure TfrmGyakorlas.FormCreate(Sender: TObject);
var i: integer;
begin
 Mode := 1;
 if not FileExists(ExtractFilePath(Application.ExeName) + 'Events.dat') then
 begin
  Application.MessageBox('Nem találom az ''Events.dat'' filet!','Törigyakorló',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;
 DBFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Events.dat');
 KerdesekSzama := DBFile.ReadInteger('General','NumberOfEvents',0);
 if KerdesekSzama = 0 then
 begin
  Application.MessageBox('Hibás/sérült ''Events.dat'' file!','Törigyakorló',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;
 for i := 1 to KerdesekSzama do
 begin
  if DBFile.SectionExists('Event' + IntToStr(i)) then
  begin
   Kerdesek[i].Letezik := true;
   Kerdesek[i].Esemeny := DBFile.ReadString('Event' + IntToStr(i),'Esemeny','');
   Kerdesek[i].Evszam := DBFile.ReadString('Event' + IntToStr(i),'Evszam','');
  end
  else
  begin
   Kerdesek[i].Letezik := false;
   Kerdesek[i].Esemeny := '';
   Kerdesek[i].Evszam := '';
  end;
 end;
 DBFile.Free;
 Randomize;
// UjKerdes;
end;

procedure TfrmGyakorlas.FormShow(Sender: TObject);
begin
 Mode := frmKerdes.RadioGroup1.ItemIndex + 1;
 UjKerdes;
end;

end.
