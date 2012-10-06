unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IniFiles, Buttons, ExtCtrls;

type
  TfrmBeallitasok = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    txtSenderName: TEdit;
    txtSenderEmail: TEdit;
    txtSenderReplyto: TEdit;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    txtSmtpHost: TEdit;
    txtSmtpUserid: TEdit;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    txtPop3Host: TEdit;
    txtPop3Userid: TEdit;
    txtPop3Password: TEdit;
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    RadioGroup1: TRadioGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBeallitasok: TfrmBeallitasok;

implementation

{$R *.DFM}

procedure TfrmBeallitasok.Button1Click(Sender: TObject);
var tif: TIniFile;
begin
 tif := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');

 tif.WriteString('sender','name',txtSenderName.Text);
 tif.WriteString('sender','email',txtSenderEmail.Text);
 tif.WriteString('sender','replyto',txtSenderReplyto.Text);

 tif.WriteString('smtp','host',txtSmtpHost.Text);
 tif.WriteString('smtp','userid',txtSmtpUserid.Text);
 tif.WriteBool('smtp','delete',CheckBox1.Checked);
 tif.WriteBool('smtp','save',CheckBox3.Checked);
 tif.WriteInteger('smtp','format',RadioGroup1.ItemIndex);

 tif.WriteString('pop3','host',txtPop3Host.Text);
 tif.WriteString('pop3','userid',txtPop3Userid.Text);
 tif.WriteString('pop3','password',txtPop3Password.Text);
 tif.WriteBool('pop3','delete',CheckBox2.Checked);

 tif.Free;

 Self.Close;
end;

procedure TfrmBeallitasok.Button2Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmBeallitasok.FormCreate(Sender: TObject);
var tif: TIniFile;
begin
 tif := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');

 txtSenderName.Text := tif.ReadString('sender','name','Nincs megadva!');
 txtSenderEmail.Text := tif.ReadString('sender','email','Nincs megadva!');
 txtSenderReplyto.Text := tif.ReadString('sender','replyto','Nincs megadva!');

 txtSmtpHost.Text := tif.ReadString('smtp','host','Nincs megadva!');
 txtSmtpUserid.Text := tif.ReadString('smtp','userid','Nincs megadva!');
 CheckBox1.Checked := tif.ReadBool('smtp','delete',true);
 CheckBox3.Checked := tif.ReadBool('smtp','save',true);
 RadioGroup1.ItemIndex := tif.ReadInteger('smtp','format',0);

 txtPop3Host.Text := tif.ReadString('pop3','host','Nincs megadva!');
 txtPop3Userid.Text := tif.ReadString('pop3','userid','Nincs megadva!');
 txtPop3Password.Text := tif.ReadString('pop3','password','');
 CheckBox2.Checked := tif.ReadBool('pop3','delete',false);

 tif.Free;
end;

procedure TfrmBeallitasok.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

end.
