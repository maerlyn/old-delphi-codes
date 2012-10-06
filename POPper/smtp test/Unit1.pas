unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Psock, NMsmtp;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    Memo4: TMemo;
    Memo5: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    NMSMTP1: TNMSMTP;
    RadioGroup1: TRadioGroup;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure NMSMTP1AttachmentNotFound(Filename: String);
    procedure NMSMTP1AuthenticationFailed(var Handled: Boolean);
    procedure NMSMTP1Connect(Sender: TObject);
    procedure NMSMTP1SendStart(Sender: TObject);
    procedure NMSMTP1EncodeStart(Filename: String);
    procedure NMSMTP1EncodeEnd(Filename: String);
    procedure NMSMTP1Failure(Sender: TObject);
    procedure NMSMTP1Success(Sender: TObject);
    procedure NMSMTP1HeaderIncomplete(var handled: Boolean; hiType: Integer);
    procedure NMSMTP1RecipientNotFound(Recipient: String);
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
  if NMSMTP1.Connected then
    NMSMTP1.Disconnect
  else
  begin
    NMSMTP1.Host := Edit1.Text;
    NMSMTP1.UserID := Edit2.Text;
    NMSMTP1.Connect;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if NMSMTP1.Connected then
  begin
    NMSMTP1.ClearParams := CheckBox1.Checked;

    NMSMTP1.SubType := mtPlain;
    case RadioGroup1.ItemIndex of
      0: NMSMTP1.EncodeType := uuMime;
      1: NMSMTP1.EncodeType := uuCode;
    end;
    NMSMTP1.PostMessage.FromAddress := Edit4.Text;
    NMSMTP1.PostMessage.FromName := Edit5.Text;
    NMSMTP1.PostMessage.ToAddress.Text := Memo1.Text;
    NMSMTP1.PostMessage.ToCarbonCopy.Text := Memo3.Text;
    NMSMTP1.PostMessage.ToBlindCarbonCopy.Text := Memo2.Text;
    NMSMTP1.PostMessage.Body.Text := Memo4.Text;

    NMSMTP1.PostMessage.Attachments.Text := ListBox1.Items.Text;
    NMSMTP1.PostMessage.Subject := Edit8.Text;
    NMSMTP1.PostMessage.LocalProgram := Edit6.Text;
    NMSMTP1.PostMessage.Date := Edit3.Text;
    NMSMTP1.PostMessage.ReplyTo := Edit7.Text;
    NMSMTP1.SendMail;
  end
  else
    ShowMessage('You need to connect before you can send your message');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  NMSMTP1.ClearParameters;
  Edit3.Clear;
  Edit4.Clear;
  Edit5.Clear;
  Edit6.Clear;
  Edit7.Clear;
  Edit8.Clear;
  Memo1.Clear;
  Memo2.Clear;
  Memo3.Clear;
  Memo4.Clear;
  Memo5.Clear;
  ListBox1.Clear;
end;

procedure TForm1.ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_INSERT then
    if OpenDialog1.Execute then
      ListBox1.Items.Add(OpenDialog1.FileName);
  if Key = VK_DELETE then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TForm1.NMSMTP1AttachmentNotFound(Filename: String);
begin
  Memo5.Lines.Add('File attachment '+FileName+' not found');
end;

procedure TForm1.NMSMTP1AuthenticationFailed(var Handled: Boolean);
var
  S: String;
begin
  S := NMSMTP1.UserID;
  if InputQuery('Authentication Failed', 'Invalid User ID. New User ID: ', S) then
  begin
    NMSMTP1.UserID := S;
    Handled := TRUE;
  end;
end;

procedure TForm1.NMSMTP1Connect(Sender: TObject);
begin
  Memo5.Lines.Add('Connected');
end;

procedure TForm1.NMSMTP1SendStart(Sender: TObject);
begin
  Memo5.Lines.Add('Sending Message');
end;

procedure TForm1.NMSMTP1EncodeStart(Filename: String);
begin
  Memo5.Lines.Add('Encoding '+FileName);
end;

procedure TForm1.NMSMTP1EncodeEnd(Filename: String);
begin
  Memo5.Lines.Add(FileName+' encoded');
end;

procedure TForm1.NMSMTP1Failure(Sender: TObject);
begin
  Memo5.Lines.Add('Message delivery failure');
end;

procedure TForm1.NMSMTP1Success(Sender: TObject);
begin
  Memo5.Lines.Add('Message sent successfully');
end;

procedure TForm1.NMSMTP1HeaderIncomplete(var handled: Boolean; hiType: Integer);
var
  S: String;
begin
  case hiType of
    hiFromAddress:
      if InputQuery('Missing From Address', 'Enter From Address: ', S) then
      begin
        NMSMTP1.PostMessage.FromAddress := S;
        Handled := TRUE;
      end;

    hiToAddress:
      if InputQuery('Missing To Address', 'Enter To Address: ', S) then
      begin
        NMSMTP1.PostMessage.ToAddress.Text := S;
        Handled := TRUE;
      end;

  end; 

end;

procedure TForm1.NMSMTP1RecipientNotFound(Recipient: String);
begin
  Memo5.Lines.Add('Recipient '+Recipient+' not found');
end;

end.
