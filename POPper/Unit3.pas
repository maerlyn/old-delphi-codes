unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, POPperData;

type
  TfrmEmailkuldes = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtCimzett: TEdit;
    txtRejtettmasolat: TEdit;
    txtTargy: TEdit;
    lstCsatoltfileok: TListBox;
    txtSzoveg: TMemo;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    OpenDialog1: TOpenDialog;
    POPperData21: TPOPperData2;
    txtMasolat: TEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ReadLetter(Index: integer);
    procedure FormCreate(Sender: TObject);
  private
    IsReading: boolean;
    CurrentIndex: integer;
  public
    { Public declarations }
  end;

var
  frmEmailkuldes: TfrmEmailkuldes;

implementation

{$R *.DFM}

procedure TfrmEmailkuldes.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TfrmEmailkuldes.SpeedButton3Click(Sender: TObject);
begin
 if not OpenDialog1.Execute then
  Abort;

 lstCsatoltfileok.Items.Add(OpenDialog1.FileName);
end;

procedure TfrmEmailkuldes.SpeedButton4Click(Sender: TObject);
var i: integer;
begin
 if lstCsatoltfileok.Items.Count = 0 then
  Abort;

 if lstCsatoltfileok.ItemIndex = -1 then
  Abort;

 i := Application.MessageBox(PChar('Tényleg törlöd a következõ filet:'+#13#10+lstCsatoltFileok.Items[lstCsatoltFileok.ItemIndex]),
      'Putra POPper',mb_YesNo + mb_IconQuestion);

 if i = id_No then
  Abort;

 lstCsatoltFileok.Items.Delete(lstCsatoltFileok.ItemIndex);
 lstCsatoltFileok.ItemIndex := -1;
end;

procedure TfrmEmailkuldes.SpeedButton2Click(Sender: TObject);
begin
 if not IsReading then
 begin
  if Application.MessageBox('Tényleg törlöd az épp írt levelet?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
   Abort;
  Self.Close;
 end else

 if IsReading then
 begin
  if Application.MessageBox('Tényleg törlöd az épp olvasott levelet?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_Yes then
  begin
   POPperData21.Delete(CurrentIndex);
   POPperData21.SaveToFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
  end;
  Self.Close;
 end;
end;

procedure TfrmEmailkuldes.SpeedButton1Click(Sender: TObject);
var i: integer;
begin
 i := id_No;

 if IsReading then
 begin
  if (txtCimzett.Text <> POPperData21.GetIndex(CurrentIndex,sit2To)) or
     (txtMasolat.Text <> POPperData21.GetIndex(CurrentIndex,sit2CC)) or
     (txtRejtettmasolat.Text <> POPperData21.GetIndex(CurrentIndex,sit2BCC)) or
     (txtTargy.Text <> POPperData21.GetIndex(CurrentIndex,sit2Subject)) or
     (lstCsatoltfileok.Items.Text <> POPperData21.GetIndex(CurrentIndex,sit2Attach)) or
     (txtSzoveg.Lines.Text <> POPperData21.GetIndex(CurrentIndex,sit2Body)) then
  i := Application.MessageBox('A levél megváltozott. Mented?','Putra POPper',mb_YesNoCancel + mb_IconQuestion);
  if i = id_Cancel then
   Abort
  else
  if i = id_No then
   Self.Close
  else
  if i = id_Yes then
  begin
   POPperData21.SetIndex(CurrentIndex,sit2To,txtCimzett.Text);
   POPperData21.SetIndex(CurrentIndex,sit2CC,txtMasolat.Text);
   POPperData21.SetIndex(CurrentIndex,sit2BCC,txtRejtettmasolat.Text);
   POPperData21.SetIndex(CurrentIndex,sit2Subject,txtTargy.Text);
   POPperData21.SetIndex(CurrentIndex,sit2Attach,lstCsatoltfileok.Items.Text);
   POPperData21.SetIndex(CurrentIndex,sit2Body,txtSzoveg.Lines.Text);
   POPperData21.SaveToFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
   Self.Close;
  end;
  Self.Close;
 end
 else
 begin
  if FileExists(ExtractFilePath(ParamStr(0)) + 'tobesent.pop') then
   POPperData21.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');

  POPperData21.AddNew(txtCimzett.Text,
                      txtMasolat.Text,
                      txtRejtettmasolat.Text,
                      txtTargy.Text,
                      lstCsatoltfileok.Items.Text,
                      txtSzoveg.Lines.Text);

  POPperData21.SaveToFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');

  Self.Close;
 end;

 Self.Close; 
end;

procedure TfrmEmailkuldes.FormCreate(Sender: TObject);
begin
 IsReading := false;
end;

procedure TfrmEmailkuldes.ReadLetter(Index: integer);
begin
 POPperData21.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
 IsReading := true;

 Self.Caption := 'E-mail olvasás';
 SpeedButton1.Caption := 'OK';
 SpeedButton1.Glyph.Handle := LoadBitmap(hInstance,'BBOK');
 SpeedButton1.NumGlyphs := 2;

 CurrentIndex := Index;

 txtCimzett.Text := POPperData21.GetIndex(Index,sit2To);
 txtMasolat.Text := POPperData21.GetIndex(Index,sit2CC);
 txtRejtettmasolat.Text := POPperData21.GetIndex(Index,sit2BCC);
 txtTargy.Text := POPperData21.GetIndex(Index,sit2Subject);
 lstCsatoltfileok.Items.Text := POPperData21.GetIndex(Index,sit2Attach);
 txtSzoveg.Lines.Text := POPperData21.GetIndex(Index,sit2Body);
end;

end.
