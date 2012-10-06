unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, NMpop3, Psock, NMsmtp, POPperData, Egyenlo;

type
  TfrmInternet = class(TForm)
    ListBox1: TListBox;
    ProgressBar1: TProgressBar;
    NMSMTP1: TNMSMTP;
    NMPOP31: TNMPOP3;
    procedure ClearAll;
    procedure AddToList(s: string);
    procedure mailSend;
    procedure mailGet;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type TThreadSMTP = class(TThread)
     protected
       procedure Execute;override;
     end;

     TThreadPOP3 = class(TThread)
     protected
       procedure Execute;override;
     end;

var
  frmInternet: TfrmInternet;
  tSMTP: TThreadSMTP;
  tPOP3: TThreadPOP3;
  MailSizes: array[1..32767] of integer;

implementation

uses Unit1;

{$R *.DFM}

{ TfrmInternet }

procedure TfrmInternet.AddToList(s: string);
begin
 ListBox1.Items.Add(s);
 ListBox1.ItemIndex := ListBox1.Items.Count - 1;
 Application.ProcessMessages;
end;

procedure TfrmInternet.ClearAll;
begin
 ListBox1.Items.Clear;
 ProgressBar1.Min := 0;
 ProgressBar1.Position := 0;
end;

procedure TfrmInternet.mailSend;
begin
 tSMTP.Execute;
end;

procedure TfrmInternet.mailGet;
begin
 tPOP3.Execute;
end;

{ TThreadSMTP }

procedure TThreadSMTP.Execute;
var i: integer;
begin
 if frmMainForm.popKuldendo.Count = 0 then
  Abort;

 with frmInternet do
 begin
  AddToList('SMTP beállítások elvégzése');
  nmSMTP1.Host := frmMainForm.cfg.ReadString('smtp','host','');
  nmSMTP1.UserID := frmMainForm.cfg.ReadString('smt','userid','');

  if frmMainForm.cfg.ReadInteger('smtp','format',0) = 0 then
   nmSMTP1.SubType := mtPlain
  else
   nmSMTP1.SubType := mtHtml;

  nmSMTP1.EncodeType := uuMime;
  ProgressBar1.Max := frmMainForm.popKuldendo.Count;

  AddToList('Csatlakozás...');
  for i := 1 to frmMainForm.popKuldendo.Count do
  begin
   try
    nmSMTP1.Connect;

    nmSMTP1.PostMessage.FromAddress := frmMainForm.cfg.ReadString('smtp','email','');
    nmSMTP1.PostMessage.FromName := frmMainForm.cfg.ReadString('smtp','name','');
    nmSMTP1.PostMessage.ReplyTo := frmMainForm.cfg.ReadString('smtp','replyto','');
    nmSMTP1.PostMessage.LocalProgram := 'Putra POPper';
    nmSMTP1.PostMessage.Date := DateTimeToStr(Now);

    nmSMTP1.PostMessage.ToAddress.Text := frmMainForm.popKuldendo.GetIndex(i,sit2To);
    nmSMTP1.PostMessage.ToCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2CC);
    nmSMTP1.PostMessage.ToBlindCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2BCC);
    nmSMTP1.PostMessage.Subject := frmMainForm.popKuldendo.GetIndex(i,sit2Subject);
    nmSMTP1.PostMessage.Attachments.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Attach);
    nmSMTP1.PostMessage.Body.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Body);

    nmSMTP1.SendMail;
    AddToList('Levél #' + IntToStr(i) + '/' + IntToStr(frmMainForm.popKuldendo.Count) + ' elküldve');

    if frmMainForm.cfg.ReadBool('smtp','save',true) = true then
     frmMainForm.popElkuldott.AddNew(frmMainForm.popKuldendo.GetIndex(i,sit2To),
                                     frmMainForm.popKuldendo.GetIndex(i,sit2CC),
                                     frmMainForm.popKuldendo.GetIndex(i,sit2BCC),
                                     frmMainForm.popKuldendo.GetIndex(i,sit2Subject),
                                     frmMainForm.popKuldendo.GetIndex(i,sit2Attach),
                                     frmMainForm.popKuldendo.GetIndex(i,sit2Body));

   finally
    nmSMTP1.Disconnect;
   end;
  end;
  AddToList('Lecsatlakozva az SMTP szerverrõl');

  if frmMainForm.cfg.ReadBool('smtp','delete',true) = true then
   while frmMainForm.popKuldendo.Count > 0 do
    frmMainForm.popKuldendo.Delete(1);

  frmInternet.ModalResult := mrOK;
 end; 
end;

procedure TfrmInternet.FormCreate(Sender: TObject);
begin
 tSMTP := TThreadSMTP.Create(true);
 tPOP3 := TThreadPOP3.Create(true);
end;

{ TThreadPOP3 }

procedure TThreadPOP3.Execute;
var i: integer;
//    s: string;
begin
 frmInternet.nmPOP31.Host := frmMainForm.cfg.ReadString('pop3','host','');
 frmInternet.nmPOP31.UserID := frmMainForm.cfg.ReadString('pop3','userid','');
 frmInternet.nmPOP31.Password := frmMainForm.cfg.ReadString('pop3','password','');
 frmInternet.AddToList('POP3 beállítások elvégezve');
 frmInternet.AddToList('Csatlakozás a POP3 szerverre');

 try
  frmInternet.nmPOP31.Connect;

  if frmInternet.nmPOP31.MailCount = 0 then
  begin
   frmInternet.AddToList('Nincs új levél!');
   frmInternet.nmPOP31.Disconnect;
   frmInternet.AddToList('Lecsatlakozva a POP3 szerverrõl');
   Exit;
  end;

  frmInternet.nmPOP31.AttachFilePath := ExtractFilePath(ParamStr(0)) + 'AttachFiles';
  frmInternet.ProgressBar1.Max := frmInternet.nmPOP31.MailCount;

  for i := 1 to frmInternet.nmPOP31.MailCount do
  begin
   frmInternet.nmPOP31.GetMailMessage(i);
   frmMainForm.popBejovo.AddNew(TargyAtalakitas(frmInternet.nmPOP31.MailMessage.From,false),
                                false,
                                MailSizes[i],
                                Now,
                                TargyAtalakitas(frmInternet.nmPOP31.MailMessage.Subject,false),
                                frmInternet.nmPOP31.MailMessage.Attachments.Text,
                                EgyenlosegTorles(trim(frmInternet.nmPOP31.MailMessage.Body.Text)));

   frmInternet.AddToList('Levél #' + IntToStr(i) + '/' + IntToStr(frmInternet.nmPOP31.MailCount) + ' letöltve');
   frmInternet.ProgressBar1.Position := i;

   if frmMainForm.cfg.ReadBool('pop3','delete',false) = true then
   begin
    frmInternet.nmPOP31.DeleteMailMessage(i);
    frmInternet.AddToList('Levél #' + IntToStr(i) + ' törölve a szerverrõl');
   end;
  end;
 finally
  frmInternet.nmPOP31.Disconnect;
  frmInternet.AddToList('Lecsatlakozva a POP3 szerverrõl');
  frmInternet.ModalResult := mrOK;
 end;
end;

end.
