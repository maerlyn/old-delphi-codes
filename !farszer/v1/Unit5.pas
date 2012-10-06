unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm5 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Button1: TButton;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    Edit10: TEdit;
    Label1: TLabel;
    TabSheet3: TTabSheet;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    Edit11: TEdit;
    Edit12: TEdit;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker2: TDateTimePicker;
    CheckBox14: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox14Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.dfm}

procedure TForm5.Button1Click(Sender: TObject);
begin
 if PageControl1.ActivePage = TabSheet1 then
  if not ((CheckBox1.Checked) or
          (CheckBox2.Checked) or
          (CheckBox3.Checked) or
          (CheckBox4.Checked) or
          (CheckBox5.Checked) or
          (CheckBox6.Checked) or
          (CheckBox7.Checked) or
          (CheckBox8.Checked) or
          (CheckBox9.Checked))
  then
   Application.MessageBox('Hiba: legalább egy keresési feltételt ki kell választani!','Fárszer',mb_IconError + mb_Ok)
  else
   Self.ModalResult := mrOK
 else

 if PageControl1.ActivePage = TabSheet2 then
  Self.ModalResult := mrOK;

 if PageControl1.ActivePage = TabSheet3 then
  if not ((CheckBox10.Checked) or
          (CheckBox11.Checked) or
          (CheckBox12.Checked) or
          (CheckBox13.Checked))
  then
   Application.MessageBox('Hiba: legalább egy keresési feltételt ki kell választani!','Fárszer',mb_IconError + mb_Ok)
  else
   Self.ModalResult := mrOK;
end;

procedure TForm5.CheckBox14Click(Sender: TObject);
begin
 if CheckBox14.Checked then
  CheckBox14.Caption := 'igen'
 else
  CheckBox14.Caption := 'nem';
end;

procedure TForm5.Button3Click(Sender: TObject);
begin
 if Button3.Caption = 'újabb, mint' then
  Button3.Caption := 'régebbi, mint'
 else
  Button3.Caption := 'újabb, mint';
end;

end.
