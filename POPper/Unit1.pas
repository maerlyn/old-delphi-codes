unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, Buttons, Unit2, ExtCtrls, Unit3, Unit4, Unit5,
  NMsmtp, Psock, NMpop3, IniFiles, Egyenlo, About, POPperData;

type
  TfrmMainForm = class(TForm)
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    cmdBeallitasok: TSpeedButton;
    cmdEmailkuldese: TSpeedButton;
    Bevel1: TBevel;
    cmdKilepes: TSpeedButton;
    Bevel2: TBevel;
    cmdKuldendoleveleklistaja: TSpeedButton;
    cmdLevellista: TSpeedButton;
    Bevel3: TBevel;
    Bevel4: TBevel;
    cmdElkuldes: TSpeedButton;
    cmdLetoltes: TSpeedButton;
    cmdElkuldesesletoltes: TSpeedButton;
    Bevel5: TBevel;
    Bevel6: TBevel;
    POPperData1: TPOPperData;
    POPperData21: TPOPperData2;
    NMPOP31: TNMPOP3;
    NMSMTP1: TNMSMTP;
    POPperData22: TPOPperData2;
    cmdElkuldottleveleklistaja: TSpeedButton;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdBeallitasokClick(Sender: TObject);
    procedure cmdEmailkuldeseClick(Sender: TObject);
    procedure cmdKilepesClick(Sender: TObject);
    procedure cmdKuldendoleveleklistajaClick(Sender: TObject);
    procedure cmdLevellistaClick(Sender: TObject);
    procedure cmdElkuldesClick(Sender: TObject);
    procedure cmdLetoltesClick(Sender: TObject);
    procedure NMPOP31List(Msg, Size: Integer);
    procedure NMSMTP1AttachmentNotFound(Filename: String);
    procedure NMSMTP1AuthenticationFailed(var Handled: Boolean);
    procedure NMSMTP1Connect(Sender: TObject);
    procedure Status(s: string);
    procedure NMSMTP1ConnectionFailed(Sender: TObject);
    procedure NMSMTP1Disconnect(Sender: TObject);
    procedure NMSMTP1Failure(Sender: TObject);
    procedure NMSMTP1HeaderIncomplete(var handled: Boolean; hiType: Integer);
    procedure NMSMTP1InvalidHost(var Handled: Boolean);
    procedure NMSMTP1PacketSent(Sender: TObject);
    procedure NMSMTP1RecipientNotFound(Recipient: String);
    procedure NMSMTP1SendStart(Sender: TObject);
    procedure NMSMTP1Success(Sender: TObject);
    procedure NMPOP31AuthenticationFailed(var Handled: Boolean);
    procedure NMPOP31AuthenticationNeeded(var Handled: Boolean);
    procedure NMPOP31Connect(Sender: TObject);
    procedure NMPOP31ConnectionFailed(Sender: TObject);
    procedure NMPOP31InvalidHost(var Handled: Boolean);
    procedure NMPOP31PacketRecvd(Sender: TObject);
    procedure cmdElkuldesesletoltesClick(Sender: TObject);
    procedure cmdElkuldottleveleklistajaClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogDebugInfo(s: string);
   private
    OldWinProc, NewWinProc: pointer;
    OutCanvas: TCanvas;
    EmailCount, CurrentlyDownloaded: integer;
    SendCount, CurrentSent: integer;
    procedure NewWinProcedure(var Msg: TMessage);
  public
    { Public declarations }
  end;

  TPOP3Thread = class(TThread)
  protected
    procedure Execute;override;
    procedure LogDebugInfo(s: string);
  end;

var
  frmMainForm: TfrmMainForm;
  hSysMenu, nCnt: longint;
  MailSizes: array[1..1024] of integer;
  levelszam: integer;
  dinfo: TStringList;
  POP3Thread: TPOP3Thread;

const mf_ByPosition = $400;
      mf_Remove = $1000;

implementation

{$R *.DFM}
{$R kepek.res}

procedure TfrmMainForm.FormCreate(Sender: TObject);
var VInfoSize, DetSize: dword;
    pVInfo, pDetail: pointer;
    MajorVer, MinorVer: integer;
    FileDescription: string;
    pLangInfo: ^TLangInfoBuffer;
    strLangID: string;
begin
 StatusBar1.Panels.Items[0].Width := StatusBar1.Width;

 hSysMenu := GetSystemMenu(Handle,false);
 if hSysMenu <> 0 then
 begin
  nCnt := GetMenuItemCount(hSysMenu);
  if nCnt <> 0 then
  begin
   RemoveMenu(hSysMenu,nCnt-1,mf_ByPosition + mf_Remove);
   RemoveMenu(hSysMenu,nCnt-2,mf_ByPosition + mf_Remove);
   DrawMenuBar(Handle);
  end;
 end;

 NewWinProc := MakeObjectInstance(NewWinProcedure);
 OldWinProc := pointer(SetWindowLong(ClientHandle,gwl_WndProc,cardinal(NewWinProc)));
 OutCanvas := TCanvas.Create;

 MajorVer := 0;
 MinorVer := 0;

 VInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),DetSize);
 if VInfoSize > 0 then
 begin
  GetMem(pVInfo,VInfoSize);
  try
   GetFileVersionInfo(PChar(ParamStr(0)),0,VInfoSize,pVInfo);
   VerQueryValue(PVInfo,'\',pDetail,DetSize);
   with TVSFixedFileInfo(pDetail^) do
   begin
    MajorVer := HiWord(dwFileVersionMS);
    MinorVer := LoWord(dwFileVersionMS);
    VerQueryValue(pVInfo,'\VarFileInfo\Translation',pointer(pLangInfo),DetSize);
    strLangID := IntToHex(smallint(pLangInfo^[1]),4) + IntToHex(smallint(pLangInfo^[2]),4);
    strLangID := 'StringFileInfo\' + strLangID;
    VerQueryValue(pVInfo,PChar(strLangID + '\FileDescription'),pDetail,DetSize);
    FileDescription := PChar(pDetail);
   end;
  finally
   FreeMem(PVInfo);
  end;
 end;

 frmMainForm.Caption := 'Putra POPper v' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + #32 + FileDescription;

 dinfo := TStringList.Create;
 dinfo.Text := '';
 LogDebugInfo('Napló megnyitva - ' + frmMainForm.Caption);

 POP3Thread := TPOP3Thread.Create(true);
end;

procedure TfrmMainForm.cmdBeallitasokClick(Sender: TObject);
var bf: TfrmBeallitasok;
begin
 bf := TfrmBeallitasok.Create(frmMainForm);
 bf.Show;
end;

procedure TfrmMainForm.cmdEmailkuldeseClick(Sender: TObject);
var ekf: TfrmEmailkuldes;
begin
 ekf := TfrmEmailkuldes.Create(frmMainForm);
 ekf.Show;
end;

procedure TfrmMainForm.cmdKilepesClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TfrmMainForm.cmdKuldendoleveleklistajaClick(Sender: TObject);
var kll: TfrmKuldendoleveleklistaja;
begin
 kll := TfrmKuldendoleveleklistaja.Create(frmMainForm);;
 kll.Show;
end;

procedure TfrmMainForm.cmdLevellistaClick(Sender: TObject);
var ull: TfrmUjleveleklistaja;
begin
 ull := TfrmUjleveleklistaja.Create(frmMainForm);
 ull.Show;
end;

procedure TfrmMainForm.NewWinProcedure(var Msg: TMessage);
var BmpWidth, BmpHeight: integer;
    Bitmap: TBitmap;
    i,j: integer;
begin
 Msg.Result := CallWindowProc(OldWinProc,ClientHandle,Msg.Msg,Msg.wParam,Msg.lParam);
 if Msg.Msg = wm_EraseBkgnd then
 begin
  Bitmap := TBitmap.Create;
  Bitmap.Handle := LoadBitmap(hInstance,'pwlogo');
  BmpWidth := Bitmap.Width;
  BmpHeight := Bitmap.Height;

  if (BmpWidth <> 0) and (BmpHeight <> 0) then
  begin
   OutCanvas.Handle := Msg.wParam;
   for i := 0 to frmMainForm.ClientWidth div BmpWidth do
    for j := 0 to frmMainForm.ClientHeight div BmpHeight do
     OutCanvas.Draw(i*BmpWidth,j*BmpHeight,Bitmap);
  end;
 end;
end;

procedure TfrmMainForm.cmdElkuldesClick(Sender: TObject);
var cfg: TIniFile;
    i: integer;
begin
 POPperData21.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
 POPperData22.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'sent.pop');

 if POPperData21.Count = 0 then
  Abort;

 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');

 nmSMTP1.Host := cfg.ReadString('smtp','host','');
 nmSMTP1.UserID := cfg.ReadString('smtp','userid','');

 SendCount := POPperData21.Count;

 Status('Levelek elõkészítése a küldésre');

 for i := 1 to POPperData21.Count do
 begin
  try
   nmSMTP1.Connect;

   CurrentSent := i;

   if cfg.ReadInteger('smtp','format',0) = 0 then
    nmSMTP1.SubType := mtPlain
   else
    nmSMTP1.SubType := mtHtml;
    
   nmSMTP1.EncodeType := uuMime;

   nmSMTP1.PostMessage.FromAddress := cfg.ReadString('sender','email','');
   nmSMTP1.PostMessage.FromName := cfg.ReadString('sender','name','');
   nmSMTP1.PostMessage.ReplyTo := cfg.ReadString('sender','replyto','');
   nmSMTP1.PostMessage.LocalProgram := 'Putra POPper';
   nmSMTP1.PostMessage.Date := DateToStr(Now);

   nmSMTP1.PostMessage.ToAddress.Text := POPperData21.GetIndex(i,sit2To);
   nmSMTP1.PostMessage.ToCarbonCopy.Text := POPperData21.GetIndex(i,sit2CC);
   nmSMTP1.PostMessage.ToBlindCarbonCopy.Text := POPperData21.GetIndex(i,sit2BCC);
   nmSMTP1.PostMessage.Subject := POPperData21.GetIndex(i,sit2Subject);
   nmSMTP1.PostMessage.Attachments.Text := POPperData21.GetIndex(i,sit2Attach);
   nmSMTP1.PostMessage.Body.Text := POPperData21.GetIndex(i,sit2Body);

   nmSMTP1.SendMail;
   Status('');

   if cfg.ReadBool('smtp','save',true) = true then
    POPperData22.AddNew(POPperData21.GetIndex(i,sit2To),
                        POPperData21.GetIndex(i,sit2CC),
                        POPperData21.GetIndex(i,sit2BCC),
                        POPperData21.GetIndex(i,sit2Subject),
                        POPperData21.GetIndex(i,sit2Attach),
                        POPperData21.GetIndex(i,sit2Body));

  finally
   nmSMTP1.Disconnect;
  end;
 end;


 if cfg.ReadBool('smtp','delete',true) = true then
  while POPperData21.Count > 0 do
   POPperData21.Delete(1);

 POPperData21.SaveToFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
 POPperData22.SaveToFile(ExtractFilePath(ParamStr(0)) + 'sent.pop');

 cfg.Free;
end;

procedure TfrmMainForm.cmdLetoltesClick(Sender: TObject);
var cfg: TIniFile;
//    i: integer;
//    tempstr: string;
//    tsl: TStringList;
//    s: string;
begin
 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');
 POPperData1.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');
                                                                                LogDebugInfo('popper.ini és newmail.pop sikeresen megnyitva');
 nmPOP31.Host := cfg.ReadString('pop3','host','');
 nmPOP31.UserID := cfg.ReadString('pop3','userid','');
 nmPOP31.Password := cfg.ReadString('pop3','password','');                      LogDebugInfo('pop3 kapcsolat beállítva');

 POP3Thread.Execute;

 LogDebugInfo('POP3 letöltõ szál elindítva');

{ try
  nmPOP31.Connect;                                                              LogDebugInfo('pop3 csatlakozás megnyitva');
  levelszam := nmPOP31.MailCount;                                               LogDebugInfo('a szerveren ' + IntToStr(levelszam) + 'db levél van');
  if levelszam = 0 then
  begin
   Status('Nincs új levél!');
   nmPOP31.Disconnect;                                                          LogDebugInfo('nincs új levél - kapcsolat bontva');
   Abort;
  end;

  nmPOP31.AttachFilePath := ExtractFilePath(ParamStr(0)) + 'AttachFiles';
  EmailCount := levelszam;

  nmPOP31.List;                                                                 LogDebugInfo('pop3 - list sikeresen végrehajtva');

  for i := 1 to levelszam do
  begin
   CurrentlyDownloaded := i;
   nmPOP31.GetMailMessage(i);                                                   LogDebugInfo('levél #' + IntToStr(i) + ' lekérve a szerverrõl');
   POPperData1.AddNew(nmPOP31.MailMessage.From,
                      false,
                      MailSizes[i],
                      Now,
                      TargyAtalakitas(nmPOP31.MailMessage.Subject),
                      nmPOP31.MailMessage.Attachments.Text,
                      EgyenlosegTorles(nmPOP31.MailMessage.Body.Text));

   LogDebugInfo('levél #' + IntToStr(i) + ' hozzáadva az adatbázishoz');

   if cfg.ReadBool('pop3','delete',false) = true then
   begin
    nmPOP31.DeleteMailMessage(i);
    LogDebugInfo('levél #' + IntToStr(i) + ' törölve a szerverrõl');
   end;
  end;
 finally
  nmPOP31.Disconnect;                                                           LogDebugInfo('lecsatlakozva a szerverrõl');
 end;

 tempstr := StatusBar1.Panels[0].Text;
// Status('Utómunkálatok a letöltött leveleken');        }

{ tsl := TStringList.Create;
 for i := 1 to levelszam do
 begin
  tsl.Text := POPperData1.GetIndex(i,sitBody);
  s := tsl[tsl.Count-1];
  if (pos('File',s)=1) and (pos('extracted',s)>1) then
  begin
   delete(s,1,length('File "'));
   delete(s,length(s)-length('" extracted'),length(s));
   POPperData1.SetIndex(i,sitAttach,POPperData1.GetIndex(i,sitAttach) + #13#10 + s);
  end;
 end;
 tsl.Free;}

// Status(tempstr);

// POPperData1.SaveToFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');          LogDebugInfo('adatbázis elmentve');

 cfg.Free;
end;

procedure TfrmMainForm.NMPOP31List(Msg, Size: Integer);
begin
 MailSizes[Msg] := Size;                                                        LogDebugInfo('levél #' + IntToStr(Msg) + ' mérete ' + IntToStr(Size) + ' byte');
end;

procedure TfrmMainForm.NMSMTP1AttachmentNotFound(Filename: String);
begin
 Application.MessageBox(PChar('Nem található a következõ csatolt file: ' + Filename),'Putra POPper',mb_Ok + mb_IconError);
 LogDebugInfo('smtp: nem található csatolt file: ' + Filename);
end;

procedure TfrmMainForm.NMSMTP1AuthenticationFailed(var Handled: Boolean);
var s: string;
begin
 LogDebugInfo('smtp: AuthenticationFailed');
 if Application.MessageBox('Hibás felhasználói név. Javítod?','Putra POPper',mb_YesNo + mb_IconError) = id_No then
 begin
  Handled := false;
  Abort;
 end;

 s := nmSMTP1.UserID;
 if not InputQuery('Putra POPper','Új felhasználói név:',s) then
 begin
  Handled := false;
  Abort;
 end;

 nmSMTP1.UserID := s;
end;

procedure TfrmMainForm.NMSMTP1Connect(Sender: TObject);
begin
 Status('Csatlakoztatva az SMTP szerverre');
end;

procedure TfrmMainForm.Status(s: string);
begin
 StatusBar1.Panels[0].Text := s;
 LogDebugInfo('Status: ' + s);
end;

procedure TfrmMainForm.NMSMTP1ConnectionFailed(Sender: TObject);
begin
 LogDebugInfo('smtp: ConnectionFailed');
 Application.MessageBox('Nem sikerült csatlakozni az SMTP szerverre!','Putra POPper',mb_OK + mb_IconError);
end;

procedure TfrmMainForm.NMSMTP1Disconnect(Sender: TObject);
begin
 Status('Lecsatlakozva az SMTP szerverrõl');
end;

procedure TfrmMainForm.NMSMTP1Failure(Sender: TObject);
begin
 LogDebugInfo('smtp: nem lehetséges a levélküldés');
 Application.MessageBox('A levélküldés nem lehetséges!','Putra POPper',mb_OK + mb_IconError);
end;

procedure TfrmMainForm.NMSMTP1HeaderIncomplete(var handled: Boolean; hiType: Integer);
var s: string;
begin
 if hiType = hiFromAddress then
 begin
  LogDebugInfo('smtp: nincs kitöltve a feladó címe');
  if Application.MessageBox('Nincs kitöltve a feladó címe. Megadod most?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
  begin
   Handled := false;
   Abort;
  end;
  s := nmSMTP1.PostMessage.FromAddress;
  if not InputQuery('Putra POPper','Feladó címe:',s) then
  begin
   Handled := false;
   Abort;
  end;
  nmSMTP1.PostMessage.FromAddress := s;
  Handled := true;
 end else
 if hiType = hiToAddress then
 begin
  LogDebugInfo('smtp: nincs megadva a címzett e-mail címe');
  if Application.MessageBox('Nincs megadva a címzett e-mail címe! Megadod most?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
  begin
   Handled := false;
   Abort;
  end;
  s := nmSMTP1.PostMessage.ToAddress.Text;
  if not InputQuery('Putra POPper','Címzett:',s) then
  begin
   Handled := false;
   Abort;
  end;
  nmSMTP1.PostMessage.ToAddress.Text := s;
  Handled := true;
 end;
end;

procedure TfrmMainForm.NMSMTP1InvalidHost(var Handled: Boolean);
var s: string;
begin
 LogDebugInfo('smtp: nem lehet csatlakozni a szerverre');
 if Application.MessageBox('Nem lehet csatlakozni az SMTP szerverre. Javítod a hibás címet?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
 begin
  Handled := false;
  Abort;
 end;
 s := nmSMTP1.Host;
 if not InputQuery('Putra POPper','SMTP szerver címe:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmSMTP1.Host := s;
 Handled := true;
end;

procedure TfrmMainForm.NMSMTP1PacketSent(Sender: TObject);
var i: integer;
begin
 i := trunc((nmSMTP1.BytesSent / nmSMTP1.BytesTotal)*100);

 Status('Levél küldése: levél' + IntToStr(CurrentSent) + '/' + IntToStr(SendCount) + ': ' + IntToStr(i) + '%...');
end;

procedure TfrmMainForm.NMSMTP1RecipientNotFound(Recipient: String);
begin
 LogDebugInfo('nem létezõ e-mail cím: ' + Recipient);
 Application.MessageBox(PChar('Nem létezik ilyen e-mail cím: ' + Recipient),'Putra POPper',mb_OK + mb_IconError);
end;

procedure TfrmMainForm.NMSMTP1SendStart(Sender: TObject);
begin
 Status('Levél küldésének kezdése');
end;

procedure TfrmMainForm.NMSMTP1Success(Sender: TObject);
begin
 Status('Levél sikeresen elküldve');
end;

procedure TfrmMainForm.NMPOP31AuthenticationFailed(var Handled: Boolean);
var s: string;
begin
 LogDebugInfo('pop3: AuthenticationFalied');
 if Application.MessageBox('Nem megfelelõ felhasználónév/jelszó páros. Javítod?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
 begin
  Handled := false;
  Abort;
 end;
 s := nmPOP31.UserID;
 if not InputQuery('Putra POPper','Új felhasználónév:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmPOP31.UserID := s;
 s := nmPOP31.Password;
 if not InputQuery('Putra POPper','Új jelszó:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmPOP31.Password := s;
 Handled := true;
end;

procedure TfrmMainForm.NMPOP31AuthenticationNeeded(var Handled: Boolean);
var s: string;
begin
 LogDebugInfo('pop3: AuthenticationNeeded');
 s := nmPOP31.UserID;
 if not InputQuery('Putra POPper','Felhasználónév:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmPOP31.UserID := s;
 s := nmPOP31.Password;
 if not InputQuery('Putra POPper','Jelszó:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmPOP31.Password := s;
 Handled := true;
end;

procedure TfrmMainForm.NMPOP31Connect(Sender: TObject);
begin
 Status('Csatlakoztatva a POP3 szerverre');
end;

procedure TfrmMainForm.NMPOP31ConnectionFailed(Sender: TObject);
begin
 Application.MessageBox('Nem sikerült kapcsolatot teremteni a POP3 szerverrel!','Putra POPper',mb_OK + mb_IconError);
 LogDebugInfo('nem sikerült a pop3 kapcsolat');
end;

procedure TfrmMainForm.NMPOP31InvalidHost(var Handled: Boolean);
var s: string;
begin
 LogDebugInfo('pop3: nem megfelelõ szervercím');
 if Application.MessageBox('Nem megfelelõ szervercím. Javítod?','Putra POPer',mb_YesNo + mb_IconQuestion) = id_No then
 begin
  Handled := false;
  Abort;
 end;
 s := nmPOP31.Host;
 if not InputQuery('Putra POPper','Új szervercím:',s) then
 begin
  Handled := false;
  Abort;
 end;
 nmPOP31.Host := s;
end;

procedure TfrmMainForm.NMPOP31PacketRecvd(Sender: TObject);
var i: integer;
begin
 if nmPOP31.BytesTotal <> 0 then
  i := trunc((nmPOP31.BytesRecvd / MailSizes[CurrentlyDownloaded])*100)
 else
  i := 999; 

 Status('Levél letötése: levél' + IntToStr(CurrentlyDownloaded) + '/' + IntToStr(EmailCount) + ': ' + IntToStr(i) + '%...');
end;

procedure TfrmMainForm.cmdElkuldesesletoltesClick(Sender: TObject);
begin
 cmdElkuldesClick(Sender);
 cmdLetoltesClick(Sender);
end;

procedure TfrmMainForm.cmdElkuldottleveleklistajaClick(Sender: TObject);
var kll: TfrmKuldendoleveleklistaja;
begin
 kll := TfrmKuldendoleveleklistaja.Create(frmMainForm);
 kll.ShowSentEmails;
 kll.Show;
end;

procedure TfrmMainForm.SpeedButton1Click(Sender: TObject);
var about: TfrmAbout;
begin
 about := TfrmAbout.Create(frmMainForm);
 about.Show;
end;

procedure TfrmMainForm.LogDebugInfo(s: string);
begin
 if pos('Status: Levél letötése',s) <> 1 then
  if pos('Status: Levél küldése',s) <> 1 then
   dinfo.Add(DateTimeToStr(Now) + ': ' + s);
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);
var f: TextFile;
begin
 LogDebugInfo('Napló lezárva');
 dinfo.Add('--------------------------------------------------------------------------------');
 AssignFile(f,ExtractFilePath(ParamStr(0)) + 'debug.log');
 Append(f);
 write(f,dinfo.Text);
 CloseFile(f);
end;

{ TPOP3Thread }

procedure TPOP3Thread.Execute;
var cfg: TIniFile;
    i: integer;
    s: string;
begin
 frmMainForm.cmdLetoltes.Enabled := false;

 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');

 try
  frmMainForm.nmPOP31.Connect;                                                  LogDebugInfo('pop3 csatlakozás megnyitva');
  levelszam := frmMainForm.nmPOP31.MailCount;                                   LogDebugInfo('a szerveren ' + IntToStr(levelszam) + 'db levél van');
  if levelszam = 0 then
  begin
   frmMainForm.Status('Nincs új levél!');
   frmMainForm.nmPOP31.Disconnect;                                              LogDebugInfo('nincs új levél - kapcsolat bontva');
   Abort;
  end;

  frmMainForm.nmPOP31.AttachFilePath := ExtractFilePath(ParamStr(0)) + 'AttachFiles';
  frmMainForm.EmailCount := levelszam;

  frmMainForm.nmPOP31.List;                                                     LogDebugInfo('pop3 - list sikeresen végrehajtva');

  for i := 1 to levelszam do
  begin
   frmMainForm.CurrentlyDownloaded := i;
   frmMainForm.nmPOP31.GetMailMessage(i);                                       LogDebugInfo('levél #' + IntToStr(i) + ' lekérve a szerverrõl');
   frmMainForm.POPperData1.AddNew(frmMainForm.nmPOP31.MailMessage.From,
                                  false,
                                  MailSizes[i],
                                  Now,
                                  TargyAtalakitas(frmMainForm.nmPOP31.MailMessage.Subject),
                                  frmMainForm.nmPOP31.MailMessage.Attachments.Text,
                                  EgyenlosegTorles(trim(frmMainForm.nmPOP31.MailMessage.Body.Text)));

   LogDebugInfo('levél #' + IntToStr(i) + ' hozzáadva az adatbázishoz');

   if cfg.ReadBool('pop3','delete',false) = true then
   begin
    frmMainForm.nmPOP31.DeleteMailMessage(i);
    LogDebugInfo('levél #' + IntToStr(i) + ' törölve a szerverrõl');
   end;
  end;
 finally
  frmMainForm.nmPOP31.Disconnect;                                               LogDebugInfo('lecsatlakozva a szerverrõl');
  frmMainForm.cmdLetoltes.Enabled := true;
 end;

 frmMainForm.POPperData1.SaveToFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');          LogDebugInfo('adatbázis elmentve');

 s := 'Lecsatlakozva a POP3 szerverrõl';
 if levelszam = 0 then
  s := s +  ' - nincs új levél'
 else
  s := s + ' - ' + IntToStr(levelszam) + 'db új levél';

 frmMainForm.Status(s);       

 LogDebugInfo('POP3 letöltõ szál befejezve');
end;

procedure TPOP3Thread.LogDebugInfo(s: string);
begin
 frmMainForm.LogDebugInfo(s);
end;

end.
