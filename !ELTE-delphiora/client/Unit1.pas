unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ScktComp;

type
  TForm1 = class(TForm)
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    ClientSocket1: TClientSocket;
    Edit1: TEdit;
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
  private
    { Private declarations }
  public
    procedure AddItem(s: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.AddItem(s: string);
begin
 ListBox1.Items.Add(s);
 ListBox1.ItemIndex := ListBox1.Items.Count-1;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
 if CheckBox1.Checked = false then
 begin
  ClientSocket1.Close;
  AddItem('kapcsolat megszüntetve');
  Abort;
 end;

 if ((LabeledEdit1.Text = '') or (LabeledEdit2.Text = '') or (LabeledEdit3.Text = '')) then
 begin
  Application.MessageBox('Hiányzó adat!','',mb_OK);
  CheckBox1.Checked := false;
  Abort;
 end;

 try
  ClientSocket1.Port := StrToInt(LabeledEdit3.Text);
 except
  on e:exception do
  begin
   Application.MessageBox('Hiba: számot adj meg portnak!','',mb_ok);
   Abort;
  end;
 end;
 ClientSocket1.Host := LabeledEdit2.Text;
 try
  ClientSocket1.Open;
 except
  on e:exception do
  begin
   Application.MessageBox('Hiba a kapcsolódás során!','',mb_ok);
   Abort;
  end;
 end;

 AddItem('kapcsolódva: ' + ClientSocket1.Socket.RemoteHost + ' (' + ClientSocket1.Socket.RemoteAddress + ')');
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key <> 13 then
  Abort;
 if ClientSocket1.Active = false then
  Abort;

 ClientSocket1.Socket.SendText(LabeledEdit1.Text + ': ' + Edit1.Text);
 Edit1.Text := '';
end;

procedure TForm1.ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
var msg: string;
begin
 msg := Socket.ReceiveText;
 AddItem(msg);
end;

end.
