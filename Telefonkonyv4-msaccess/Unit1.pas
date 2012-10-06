unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, Buttons, ToolWin, ComCtrls, ImgList, StdCtrls, Db,
  DBTables, ADODB, Excel97, OleServer, Word97, Unit5;

type
  TfrmMainForm = class(TForm)
    ToolBar1: TToolBar;
    cmdSave: TSpeedButton;
    Bevel1: TBevel;
    cmdPrint: TSpeedButton;
    cmdAbout: TSpeedButton;
    Bevel2: TBevel;
    cmdExit: TSpeedButton;
    Bevel3: TBevel;
    cmdFirst: TSpeedButton;
    cmdLast: TSpeedButton;
    Bevel4: TBevel;
    cmdPrevious: TSpeedButton;
    cmdNext: TSpeedButton;
    MainMenu2: TMainMenu;
    mnuFile2: TMenuItem;
    mnuFileSave2: TMenuItem;
    N2: TMenuItem;
    mnuFilePrint2: TMenuItem;
    N3: TMenuItem;
    mnuFileQuit2: TMenuItem;
    mnuEdit2: TMenuItem;
    N4: TMenuItem;
    mnuEditFirst2: TMenuItem;
    mnuEditLast2: TMenuItem;
    mnuEditPrevious2: TMenuItem;
    mnuEditNext2: TMenuItem;
    mnuHelp2: TMenuItem;
    mnuHelpAbout2: TMenuItem;
    ImageList1: TImageList;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    lblVaros: TLabel;
    txtVaros: TEdit;
    lblIrsz: TLabel;
    txtIrsz: TEdit;
    lblUtca: TLabel;
    txtUtca: TEdit;
    lblHazsz: TLabel;
    txtHazsz: TEdit;
    Bevel8: TBevel;
    lblTelsz: TLabel;
    txtTelsz: TEdit;
    lblEmail: TLabel;
    txtEmail: TEdit;
    lblHonlap: TLabel;
    txtHonlap: TEdit;
    cmdEmail: TBitBtn;
    cmdHonlap: TBitBtn;
    Timer2: TTimer;
    lblNev: TLabel;
    cmbNev: TComboBox;
    Table1: TADOTable;
    Bevel9: TBevel;
    cmdList: TSpeedButton;
    ADOCommand1: TADOCommand;
    ADOConnection1: TADOConnection;
    cmdDeleteRecord: TSpeedButton;
    WordDocument1: TWordDocument;
    ExcelApplication1: TExcelApplication;
    N1: TMenuItem;
    mnuFileExport: TMenuItem;
    mnuFileExportWord: TMenuItem;
    mnuFileExportExcel: TMenuItem;
    WordApplication1: TWordApplication;
    procedure Timer1Timer(Sender: TObject);
    procedure mnuFileQuit1Click(Sender: TObject);
    procedure mnuHelpAbout1Click(Sender: TObject);
    procedure mnuHelpAbout2Click(Sender: TObject);
    procedure mnuFileQuit2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdExitClick(Sender: TObject);
    procedure cmdAboutClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure SzovegmezoModositas(Sender: TObject);
    procedure cmbNevChange(Sender: TObject);
    procedure cmbNevExit(Sender: TObject);
    procedure GetData;
    procedure SaveData;
    procedure cmbNevClick(Sender: TObject);
    procedure cmbNevKeyPress(Sender: TObject; var Key: Char);
    procedure cmdSaveClick(Sender: TObject);
    procedure mnuFileSave2Click(Sender: TObject);
    procedure mnuEditPrevious2Click(Sender: TObject);
    procedure mnuEditNext2Click(Sender: TObject);
    procedure mnuEditFirst2Click(Sender: TObject);
    procedure mnuEditLast2Click(Sender: TObject);
    procedure cmdFirstClick(Sender: TObject);
    procedure cmdLastClick(Sender: TObject);
    procedure cmdPreviousClick(Sender: TObject);
    procedure cmdNextClick(Sender: TObject);
    procedure cmdListClick(Sender: TObject);
    procedure txtEmailChange(Sender: TObject);
    procedure cmdEmailClick(Sender: TObject);
    procedure cmdDeleteRecordClick(Sender: TObject);
    procedure MezobolValoKilepes(Sender: TObject);
    procedure cmdHonlapClick(Sender: TObject);
    procedure OnePercentSnipper(var Miben: string; const Mit: string);
    procedure Table1AfterPost(DataSet: TDataSet);
    procedure mnuFileExportWordClick(Sender: TObject);
    procedure mnuFileExportExcelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    fMostmarBezarhatsz: boolean;
  public
    property MostmarBezarhatsz: boolean read fMostmarBezarhatsz write fMostmarBezarhatsz;
  end;

  TNevRec = record
   Vezeteknev,
   Keresztnev: string[40];
  end;

  TCimRec = record
   Varos: string[30];
   Utca: string[40];
   Hazsz: string[10];
   Irsz: string[4];
  end;

  TKapcsolat = record
   Telefon: string[25];
   eMail: string[30];
   Web: string[40];
  end;

  TTelefonkonyv = record
   Nev: TNevRec;
   Cim: TCimRec;
   Kapcsolat: TKapcsolat;
  end;

var
  frmMainForm: TfrmMainForm;
  Modositott: boolean;

implementation

uses Unit2, Unit3, Unit6;

{$R *.DFM}

procedure TfrmMainForm.Timer1Timer(Sender: TObject);
begin
 StatusBar1.Panels[1].Text := TimeToStr(Time);
end;

procedure TfrmMainForm.mnuFileQuit1Click(Sender: TObject);
var temp: integer;
begin
 if Modositott then
 begin
  temp := Application.MessageBox('Az adatbázis tartalma megváltozott. Mented?','Telefonkönyv v3.0',mb_YesNoCancel + mb_IconQuestion);
  case temp of
   id_Yes    : SaveData;
   id_Cancel : Exit;
  end;
 end;
 Application.Terminate;
end;

procedure TfrmMainForm.mnuHelpAbout1Click(Sender: TObject);
var s: string;
begin
 s := 'Telefonkönyv v4.0'#13#10#13#10'by Tele von Zsinór, Zsinór Ware, Hungary'#13#10;
 s := s +  'eMail: TeleVonZsinor@webmail.hu'#13#10' Homepage: http://TeleVonZsinor.ini.hu';
 Application.MessageBox(PChar(s),'Névjegy',mb_Ok + mb_IconInformation);
end;

procedure TfrmMainForm.mnuHelpAbout2Click(Sender: TObject);
begin
 mnuHelpAbout1Click(Sender);
end;

procedure TfrmMainForm.mnuFileQuit2Click(Sender: TObject);
begin
 mnuFileQuit1Click(Sender);
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
var TablesList: TStringList;
    temp: widestring;
begin
 MostmarBezarhatsz := false;
 if not FileExists(ExtractFilePath(ParamStr(0)) + 'System\System.mdb') then
 begin
  Application.MessageBox('Nem találom az adatbázis file-t!','Hiba',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;

 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'Data Source=' + ExtractFilePath(Application.ExeName) + 'System\System.mdb;';
 temp := temp + 'Mode=Read|Write';
 ADOConnection1.ConnectionString := temp;

 TablesList := TStringList.Create;
 try
 try
  ADOConnection1.GetTableNames(TablesList);
  if TablesList.IndexOf(Table1.TableName) < 0 then
   ADOCommand1.Execute;
  Table1.Open;
 finally
  TablesList.Free;
 end;
 except
  on E: Exception do
  begin
   Application.MessageBox(PChar('Hiba!'#13#10+E.Message),'Hiba!',mb_Ok + mb_IconHand);
   Application.Terminate;
  end;
 end;

 Table1.First;
 while not Table1.Eof do
 begin
  cmbNev.Items.Add(Table1.Fields[0].AsString);
  Table1.Next;
 end;

 frmMainForm.Menu := MainMenu2;
 Modositott := false;
 Timer1.Enabled := false;
 Timer2.Enabled := false;
 Table1.First;
 cmbNev.ItemIndex := 0;
 GetData;
 frmSplash.Hide;
 MostmarBezarhatsz := true;
end;

procedure TfrmMainForm.cmdExitClick(Sender: TObject);
begin
 mnuFileQuit1Click(Sender);
end;

procedure TfrmMainForm.cmdAboutClick(Sender: TObject);
begin
 mnuHelpAbout1Click(Sender);
end;

procedure TfrmMainForm.Timer2Timer(Sender: TObject);
var Ora, Perc, Masodperc, Ezredmasodp: Word;
    Netido: integer;
begin
 DecodeTime(Time,Ora,Perc,Masodperc,Ezredmasodp);
 Netido := trunc((Masodperc + 60*Perc + 60*60*Ora) / 86.4);
 StatusBar1.Panels[2].Text := '@' + IntToStr(Netido);
end;

procedure TfrmMainForm.SzovegmezoModositas(Sender: TObject);
begin
 Modositott := true;
end;

procedure TfrmMainForm.cmbNevChange(Sender: TObject);
begin
 Modositott := true;
end;

procedure TfrmMainForm.cmbNevExit(Sender: TObject);
begin
 SaveData;
end;

procedure TfrmMainForm.GetData;
begin

 Table1.Locate('nev',cmbNev.Text,[loCaseInsensitive]);

 cmbNev.Text := Table1.Fields[0].AsString;
 txtVaros.Text := Table1.Fields[1].AsString;
 txtIrsz.Text := Table1.Fields[2].AsString;
 txtUtca.Text := Table1.Fields[3].AsString;
 txtHazsz.Text := Table1.Fields[4].AsString;
 txtTelsz.Text := Table1.Fields[5].AsString;
 txtEmail.Text := Table1.Fields[6].AsString;
 txtHonlap.Text := Table1.Fields[7].AsString;
end;

procedure TfrmMainForm.cmbNevClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmMainForm.cmbNevKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
 begin
  if Table1.Locate('nev',cmbNev.Text,[loCaseInsensitive]) then
   GetData
  else
  begin
   txtVaros.Text := '';
   txtIrsz.Text := '';
   txtUtca.Text := '';
   txtHazsz.Text := '';
   txtTelsz.Text := '';
   txtEmail.Text := '';
   txtHonlap.Text := '';
   txtVaros.SetFocus;
  end;
 end;
end;

procedure TfrmMainForm.SaveData;
begin
 if cmbNev.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 if Table1.Locate('nev',cmbNev.Text,[loCaseInsensitive]) then
 begin
  Table1.Edit;
  Table1.Fields[0].AsString := cmbNev.Text;
  Table1.Fields[1].AsString := txtVaros.Text;
  Table1.Fields[2].AsString := txtIrsz.Text;
  Table1.Fields[3].AsString := txtUtca.Text;
  Table1.Fields[4].AsString := txtHazsz.Text;
  Table1.Fields[5].AsString := txtTelsz.Text;
  Table1.Fields[6].AsString := txtEmail.Text;
  Table1.Fields[7].AsString := txtHonlap.Text;
  Table1.Post;
 end
 else
 begin
  Table1.InsertRecord([cmbNev.Text,txtVaros.Text,txtIrsz.Text,txtUtca.Text,
                       txtHazsz.Text,txtTelsz.Text,txtEmail.Text,txtHonlap.Text]);
  cmbNev.Items.Add(cmbNev.Text);
 end;
 Modositott := false;
end;

procedure TfrmMainForm.cmdSaveClick(Sender: TObject);
begin
 SaveData;
end;

procedure TfrmMainForm.mnuFileSave2Click(Sender: TObject);
begin
 SaveData;
end;

procedure TfrmMainForm.mnuEditPrevious2Click(Sender: TObject);
begin
 if Table1.Bof then Exit;
 cmbNev.ItemIndex := cmbNev.ItemIndex + 1;
 GetData;
end;

procedure TfrmMainForm.mnuEditNext2Click(Sender: TObject);
begin
 if Table1.Eof then Exit;
 cmbNev.ItemIndex := cmbNev.ItemIndex + 1;
 GetData;
end;

procedure TfrmMainForm.mnuEditFirst2Click(Sender: TObject);
begin
 cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmMainForm.mnuEditLast2Click(Sender: TObject);
begin
 cmbNev.ItemIndex := cmbNev.Items.Count - 1;
 GetData;
end;

procedure TfrmMainForm.cmdFirstClick(Sender: TObject);
begin
 cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmMainForm.cmdLastClick(Sender: TObject);
begin
 cmbNev.ItemIndex := cmbNev.Items.Count - 1;
 GetData;
end;

procedure TfrmMainForm.cmdPreviousClick(Sender: TObject);
begin
 if Table1.Bof then Exit;
 cmbNev.ItemIndex := cmbNev.ItemIndex - 1;
 GetData;
end;

procedure TfrmMainForm.cmdNextClick(Sender: TObject);
begin
 if Table1.Eof then Exit;
 cmbNev.ItemIndex := cmbNev.ItemIndex + 1;
 GetData;
end;

procedure TfrmMainForm.cmdListClick(Sender: TObject);
begin
 frmList.Show;
end;

procedure TfrmMainForm.txtEmailChange(Sender: TObject);
begin
 if Pos('@',txtEmail.Text) <> 0 then
  if Pos('.',txtEmail.Text) <> 0 then
   cmdEmail.Enabled := true
 else
  cmdEmail.Enabled := false;  
end;

procedure TfrmMainForm.cmdEmailClick(Sender: TObject);
begin
 frmEmailForm.txtTo.Text := txtEmail.Text;
 frmEmailForm.Show;
end;

procedure TfrmMainForm.cmdDeleteRecordClick(Sender: TObject);
var temp: integer;
begin
 temp := Application.MessageBox(PChar('Tényleg törlöd: ''' + cmbNev.Text + '''?'),'Telefonkönyv',mb_YesNo + mb_IconQuestion);
 if temp = id_No then Exit;
 Table1.DeleteRecords(arCurrent);

 Table1.First;
 while not Table1.Eof do
 begin
  cmbNev.Items.Add(Table1.Fields[0].AsString);
  Table1.Next;
  cmbNev.ItemIndex := 0;
  GetData;
 end;

end;

procedure TfrmMainForm.MezobolValoKilepes(Sender: TObject);
begin
 if Length((Sender as TEdit).Text) = 0 then
 begin
  Application.MessageBox('Nem lehet érték nélküli a mezõ!','Telefonkönyv',mb_Ok + mb_IconError);
  (Sender as TEdit).SetFocus;
 end;

end;

procedure TfrmMainForm.cmdHonlapClick(Sender: TObject);
begin
 frmWebBrowser.Show;
 frmWebBrowser.txtLocation.Text := txtHonlap.Text;
 frmWebBrowser.GotoPage(frmWebBrowser.txtLocation.Text);
end;

procedure TfrmMainForm.OnePercentSnipper(var Miben: string; const Mit: string);
var temp: string;
begin
 if Pos('%1',Miben) <> 0 then
 begin
  temp := Copy(Miben,1,Pos('%1',Miben) - 1);
  temp := temp + Mit;
  temp := temp + Copy(Miben,Pos('%1',Miben) + Length('%1'),Length(Miben));
  Miben := temp;
 end
 else
 begin
  temp := Miben + ' ' + Mit;
  Miben := temp;
 end;
end;

procedure TfrmMainForm.Table1AfterPost(DataSet: TDataSet);
begin
 frmList.FormShow(frmMainForm);
end;

procedure TfrmMainForm.mnuFileExportWordClick(Sender: TObject);
var temp: integer;
    Bookmark: TBookmark;
    RangeW: Word97.Range;
    v1: variant;
    ov1: OleVariant;
    Row1: Word97.Row;
begin
 temp := Application.MessageBox('Figyelem'#13#10#13#10'Az exportáláshoz a Microsoft Word 97-es verziója kell!','Figyelmeztetés',mb_OkCancel + mb_IconWarning);
 if temp = id_Cancel then Exit;
 WordApplication1.Visible := true;
 WordDocument1.Activate;
 WordDocument1.PageSetup.Orientation := 1;
 WordDocument1.Range.Text := 'Telefonszámok';
 WordDocument1.Range.Font.Size := 12;
 Bookmark := Table1.GetBookmark;
 Table1.First;
 WordDocument1.Range.InsertParagraphAfter;
 WordDocument1.Paragraphs.Last.Range.Text := 'Név' + #9 + 'Cím' + #9 + 'Telefonszám' + #9 + 'E-Mail cím' + #9 + 'Honlap';
 while not Table1.Eof do
 begin
  WordDocument1.Range.InsertParagraphAfter;
  WordDocument1.Paragraphs.Last.Range.Text := Table1.FieldByName('nev').AsString + #9 +
   Table1.FieldByName('cim_irsz').AsString + #32 +
   Table1.FieldByName('cim_varos').AsString + ', ' +
   Table1.FieldByName('cim_utca').AsString + #32 +
   Table1.FieldByName('cim_hazsz').AsString + #9 +
   Table1.FieldByName('telszam').AsString + #9 +
   Table1.FieldByName('email').AsString + #9 +
   Table1.FieldByName('honlap').AsString;
  Table1.Next;
 end;
 Table1.GotoBookmark(Bookmark);

 RangeW := WordDocument1.Content;
 v1 := RangeW;
 v1.ConvertToTable(#9,19,5);
 Row1 := WordDocument1.Tables.Item(1).Rows.Get_First;
 Row1.Range.Bold := 1;
 Row1.Range.Font.Size := 30;
 Row1.Range.InsertParagraphAfter;
 ov1 := ' ';
 Row1.ConvertToText(ov1);
end;

procedure TfrmMainForm.mnuFileExportExcelClick(Sender: TObject);
var temp: integer;
    RangeE: Excel97.Range;
    Row: integer;
    Bookmark: TBookmark;
begin
 temp := Application.MessageBox('Figyelem!'#13#10#13#10'Az exportáláshoz a Microsoft Excel 97-es verziója kell!','Filegyelmeztetés',mb_OkCancel + mb_IconWarning);
 if temp = id_Cancel then Exit;
 ExcelApplication1.Visible[0] := true;
 ExcelApplication1.Workbooks.Add(null,0);
 RangeE := ExcelApplication1.ActiveCell;
 RangeE.Value := 'Név';
 RangeE := RangeE.Next;
 RangeE.Value := 'Cím';
 RangeE := RangeE.Next;
 RangeE.Value := 'Telefonszám';
 RangeE := RangeE.Next;
 RangeE.Value := 'E-Mail cím';
 RangeE := RangeE.Next;
 RangeE.Value := 'Honlap';
 Table1.DisableControls;
 try
  Bookmark := Table1.GetBookmark;
  try
   Table1.First;
   Row := 2;
   while not Table1.Eof do
   begin
    RangeE := ExcelApplication1.Range['A' + IntToStr(Row),'A' + IntToStr(Row)];
    RangeE.Value := Table1.FieldByName('nev').AsString;
    RangeE := RangeE.Next;
    RangeE.Value := Table1.FieldByName('cim_irsz').AsString + ' ' +
     Table1.FieldByName('cim_varos').AsString + ', ' +
     Table1.FieldByName('cim_utca').AsString + ' ' +
     Table1.FieldByName('cim_hazsz').AsString;
    RangeE := RangeE.Next;
    RangeE.Value := Table1.FieldByName('telszam').AsString;
    RangeE := RangeE.Next;
    RangeE.Value := Table1.FieldByName('email').AsString;
    RangeE := RangeE.Next;
    RangeE.Value := Table1.FieldByName('honlap').AsString;
    RangeE := RangeE.Next; 
    Table1.Next;
    Inc(Row);
   end;
  finally
   Table1.GotoBookmark(Bookmark);
  end;
 finally
  Table1.EnableControls;
 end;
 RangeE := ExcelApplication1.Range['A1','E' + IntToStr(Row - 1)];
 RangeE.AutoFormat(3,null,null,null,null,null,null);
end;

procedure TfrmMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Application.Terminate;
end;

end.
