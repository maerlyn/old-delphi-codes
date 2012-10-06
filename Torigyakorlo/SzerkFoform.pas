unit SzerkFoform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ComCtrls, IniFiles;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    sedAktualisEsemeny: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblFileNeve: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    txtEsemeny: TEdit;
    txtEvszam: TEdit;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sedAktualisEsemenyClick(Sender: TObject);
    procedure txtEsemenyExit(Sender: TObject);
    procedure txtEvszamExit(Sender: TObject);
    procedure sedAktualisEsemenyChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DBFile: TIniFile;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
 begin
  DBFile.Free;
  DBFile := TIniFile.Create(OpenDialog1.FileName);
  lblFileNeve.Caption := OpenDialog1.FileName;
  sedAktualisEsemeny.Value := 1;
  StatusBar1.Panels[0].Text := 'File megnyitva';
  if DBFile.SectionExists('Event' + IntToStr(sedAktualisEsemeny.Value)) then
  begin
   txtEvszam.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Evszam','');
   txtEsemeny.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Esemeny','');
   StatusBar1.Panels[0].Text := 'Esemény szerkesztése';
  end
  else
  begin
   StatusBar1.Panels[0].Text := 'Új esemény hozzáadása';
   txtEsemeny.Text := '';
   txtEvszam.Text := '';
  end;
  sedAktualisEsemeny.Enabled := true;
 end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 DBFile.Free;
end;

procedure TForm1.sedAktualisEsemenyClick(Sender: TObject);
begin
 if DBFile.SectionExists('Event' + IntToStr(sedAktualisEsemeny.Value)) then
 begin
  StatusBar1.Panels[0].Text := 'Esemény szerkesztése';
  txtEvszam.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Evszam','');
  txtEsemeny.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Esemeny','');
 end
 else
 begin
  txtEvszam.Text := '';
  txtEsemeny.Text := '';
  StatusBar1.Panels[0].Text := 'Új esemény hozzáadása';
 end;
end;

procedure TForm1.txtEsemenyExit(Sender: TObject);
var ElozoSzoveg: string;
begin
 ElozoSzoveg := StatusBar1.Panels[0].Text;
 StatusBar1.Panels[0].Text := 'Mentés';
 DBFile.WriteString('Event' + IntToStr(sedAktualisEsemeny.Value),'Esemeny',txtEsemeny.Text);
 StatusBar1.Panels[0].Text := ElozoSzoveg;
end;

procedure TForm1.txtEvszamExit(Sender: TObject);
var ElozoSzoveg: string;
begin
 ElozoSzoveg := StatusBar1.Panels[0].Text;
 StatusBar1.Panels[0].Text := 'Mentés';
 DBFile.WriteString('Event' + IntToStr(sedAktualisEsemeny.Value),'Evszam',txtEvszam.Text);
 StatusBar1.Panels[0].Text := ElozoSzoveg;
end;

procedure TForm1.sedAktualisEsemenyChange(Sender: TObject);
begin
 if DBFile.SectionExists('Event' + IntToStr(sedAktualisEsemeny.Value)) then
 begin
  StatusBar1.Panels[0].Text := 'Esemény szerkesztése';
  txtEvszam.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Evszam','');
  txtEsemeny.Text := DBFile.ReadString('Event' + IntToStr(sedAktualisEsemeny.Value),'Esemeny','');
 end
 else
 begin
  txtEvszam.Text := '';
  txtEsemeny.Text := '';
  StatusBar1.Panels[0].Text := 'Új esemény hozzáadása';
 end;
end;

end.

