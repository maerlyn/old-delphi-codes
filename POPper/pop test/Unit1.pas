unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Psock, NMpop3;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    NMPOP31: TNMPOP3;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button5: TButton;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure NMPOP31Connect(Sender: TObject);
    procedure NMPOP31List(Msg, Size: Integer);
    procedure NMPOP31AuthenticationFailed(var Handled: Boolean);
    procedure NMPOP31AuthenticationNeeded(var Handled: Boolean);
    procedure NMPOP31Reset(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure NMPOP31RetrieveStart(Sender: TObject);
    procedure NMPOP31RetrieveEnd(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure NMPOP31Success(Sender: TObject);
    procedure NMPOP31Failure(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if NMPOP31.Connected then
    NMPOP31.Disconnect
  else
  begin
    NMPOP31.Host := Edit1.Text;
    NMPOP31.UserID := Edit2.Text;
    NMPOP31.Password := Edit3.Text;
    NMPOP31.Connect;
  end;
end;

procedure TForm1.NMPOP31Connect(Sender: TObject);
begin
  if NMPOP31.MailCount > 0 then
    NMPOP31.List
  else
    Memo1.Lines.Add('No Messages');
end;

procedure TForm1.NMPOP31List(Msg, Size: Integer);
begin
  Memo1.Lines.Add('Message '+IntToStr(Msg)+': '+IntToStr(Size)+' bytes');
end;

procedure TForm1.NMPOP31AuthenticationFailed(var Handled: Boolean);
var
  NewPass,
  NewID: String;

begin
  if MessageDlg('Authentication Failed. Retry?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    NewPass := NMPOP31.Password;
    NewID := NMPOP31.UserID;
    InputQuery('Authentication Failed','Input User ID', NewID);
    InputQuery('Authentication Failed','Input Password', NewPass);
    NMPOP31.Password := NewPass;
    NMPOP31.UserID := NewID;
    Handled := TRUE;
  end
  else
    Handled := FALSE;
end;

procedure TForm1.NMPOP31AuthenticationNeeded(var Handled: Boolean);
var
  NewPass,
  NewID: String;
begin
  NewPass := NMPOP31.Password;
  NewID := NMPOP31.UserID;
  InputQuery('Authorization Needed','Input User ID', NewID);
  InputQuery('Authorization Needed','Input Password', NewPass);
  NMPOP31.UserID := NewID;
  NMPOP31.Password := NewPass;
  Handled := TRUE;
end;

procedure TForm1.NMPOP31Reset(Sender: TObject);
begin
  Memo1.Lines.Add('Delete flags reset');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  NMPOP31.Reset;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  S: String;
  M: Integer;
begin
  if NMPOP31.MailCount > 0 then
  begin
    if InputQuery('Retrieve a Summary', 'Which message? (1-'+IntToStr(NMPOP31.MailCount)+')', S) then
    begin
      M := StrToIntDef(S, -1);
      If (M < 0) or (M > NMPOP31.MailCount) then

        ShowMessage('Invalid message index')
      else
      begin
        NMPOP31.GetSummary(M);
        Edit6.Text := NMPOP31.UniqueID(M);
      end;
    end;
  end
  else
    ShowMessage('No Messages to Summarize');
end;

procedure TForm1.NMPOP31RetrieveStart(Sender: TObject);
begin
  Memo1.Lines.Add('Retrieving Summary');
end;

procedure TForm1.NMPOP31RetrieveEnd(Sender: TObject);
begin
  Memo1.Lines.Add('Summary retrieved');
  Edit4.Text := NMPOP31.Summary.From;

  Edit5.Text := NMPOP31.Summary.Subject;
  Memo2.Text := NMPOP31.Summary.Header.Text;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  S: String;
  M: Integer;
begin
  if NMPOP31.MailCount > 0 then
  begin
    if InputQuery('Delete a Message', 'Which message? (1-'+IntToStr(NMPOP31.MailCount)+')', S) then
    begin
      M := StrToIntDef(S, -1);
      If (M < 0) or (M > NMPOP31.MailCount) then
        ShowMessage('Invalid message index')
      else
      begin
        NMPOP31.DeleteMailMessage(M);
      end;
    end;
  end
  else
    ShowMessage('No Messages to Delete');
end;

procedure TForm1.NMPOP31Success(Sender: TObject);
begin
  ShowMessage('Message deleted.');
end;

procedure TForm1.NMPOP31Failure(Sender: TObject);
begin
  ShowMessage('Operation Failed');
end;

procedure TForm1.Button5Click(Sender: TObject);
var mailnumber: string;
begin
 mailnumber := '1';
 InputQuery('Delete a Message', 'Which message? (1-'+IntToStr(NMPOP31.MailCount)+')', mailnumber);
 nmpop31.GetMailMessage(strtoint(mailnumber));
 memo3.lines.text := nmpop31.MailMessage.body.text;
 edit5.text := nmpop31.MailMessage.Subject;
end;

end.
