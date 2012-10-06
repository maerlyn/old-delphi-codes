unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, Psock, NMpop3, NMsmtp, Unit4;

type
  TfrmEmailForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtTo: TEdit;
    txtCc: TEdit;
    txtSubject: TEdit;
    cmdSend: TBitBtn;
    cmdDelete: TBitBtn;
    cbxAttach: TCheckBox;
    txtAttach: TEdit;
    cmdAttach: TButton;
    StatusBar1: TStatusBar;
    txtBody: TMemo;
    OpenDialog1: TOpenDialog;
    NMSMTP1: TNMSMTP;
    procedure cbxAttachClick(Sender: TObject);
    procedure cmdAttachClick(Sender: TObject);
    procedure txtAttachEnter(Sender: TObject);
    procedure cmdDeleteClick(Sender: TObject);
    procedure cmdSendClick(Sender: TObject);
    procedure Delay(Seconds, MilliSec: Word);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEmailForm: TfrmEmailForm;

implementation

{$R *.DFM}

procedure TfrmEmailForm.cbxAttachClick(Sender: TObject);
begin
 txtAttach.Enabled := cbxAttach.Checked;
 cmdAttach.Enabled := cbxAttach.Checked;
end;

procedure TfrmEmailForm.cmdAttachClick(Sender: TObject);
begin
 if (OpenDialog1.Execute) and (FileExists(OpenDialog1.FileName)) then
 begin
  txtAttach.ReadOnly := False;
  txtAttach.Text := OpenDialog1.FileName;
  txtAttach.ReadOnly := True;
 end;
end;

procedure TfrmEmailForm.txtAttachEnter(Sender: TObject);
begin
 cmdAttach.SetFocus;
end;

procedure TfrmEmailForm.cmdDeleteClick(Sender: TObject);
begin
 if Application.MessageBox('Tényleg törölni akarjod a levelet?','Kérdés',mb_IconQuestion+mb_YesNo) = ID_YES then
 begin
  txtTo.Clear;
  txtCc.Clear;
  txtSubject.Clear;
  cbxAttach.Checked := False;
  cmdAttach.Enabled := False;
  txtAttach.Enabled := False;
  txtBody.Clear;
  StatusBar1.Panels[0].Text := '';
  StatusBar1.Panels[1].Text := '';
 end;
end;

procedure TfrmEmailForm.cmdSendClick(Sender: TObject);
begin
 if frmEmailBForm.ShowModal = mrOk then
 begin
  StatusBar1.Panels[1].Text := 'Állapot: levél elõkészítése';
  NmSMTP1.Host := frmEmailBForm.txtServer.Text;
  NmSMTP1.UserID := frmEmailBForm.txtUserID.Text;
  NmSMTP1.PostMessage.ToAddress.Clear;
  NmSMTP1.PostMessage.ToAddress.Add(txtTo.Text);
  NmSMTP1.PostMessage.ToCarbonCopy.Clear;
  NmSMTP1.PostMessage.ToCarbonCopy.Add(txtCc.Text);
  NmSMTP1.PostMessage.Subject := txtSubject.Text;
  if (cbxAttach.Checked) and ((Length(txtAttach.Text)>0) and (FileExists(txtAttach.Text))) then
  begin
   NmSMTP1.PostMessage.Attachments.Clear;
   NmSMTP1.PostMessage.Attachments.Add(txtAttach.Text);
  end;
  NmSMTP1.PostMessage.Body.Clear;
  NmSMTP1.PostMessage.Body.Add(txtBody.Lines.Text);
  StatusBar1.Panels[1].Text := 'Állapot: csatlakozás a szerverhez';
  try
   try
    NmSMTP1.Connect;
    StatusBar1.Panels[1].Text := 'Csatlakozva a következõhöz: ' + frmEmailBForm.txtServer.Text;
    Delay(1,0);
    StatusBar1.Panels[1].Text := 'Állapot: levél küldése';
    NmSMTP1.SendMail;
    StatusBar1.Panels[1].Text := 'Levél elküldve';
    Application.MessageBox('A levél elküldése sikeresen megtörtént.','Üzenet',mb_Ok + mb_IconInformation);
   finally
    NmSMTP1.Disconnect;
   end;
  except
   StatusBar1.Panels[1].Text := 'A levél küldése közben hiba történt';
   Application.MessageBox('A levél eküldése nem sikerült!','Hiba',mb_Ok + mb_IconError);
  end;
 end;
end;

procedure TfrmEmailForm.Delay(Seconds, MilliSec: Word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

end.
