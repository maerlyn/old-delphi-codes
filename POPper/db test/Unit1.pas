unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, POPperData;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    CheckBox1: TCheckBox;
    DateTimePicker1: TDateTimePicker;
    ListBox1: TListBox;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    POPperData1: TPOPperData;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2Click(Sender: TObject);
begin
 POPperData1.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'popper.pop');

 Edit1.Text := string(POPperData1.GetIndex(1,sitSender));
 CheckBox1.Checked := boolean(POPperData1.GetIndex(1,sitRead));
 Edit2.Text := IntToStr(integer(POPperData1.GetIndex(1,sitSize)));
 DateTimePicker1.DateTime := TDateTime(POPPerData1.GetIndex(1,sitDate));
 Edit3.Text := string(POPperData1.GetIndex(1,sitSubject));
 ListBox1.Items.Text := string(POPperData1.GetIndex(1,sitAttach));
 Memo1.Lines.Text := string(POPperData1.GetIndex(1,sitBody))
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 with POPperData1 do
 begin
  SetIndex(1,sitSender,Edit1.Text);
  SetIndex(1,sitRead,CheckBox1.Checked);
  SetIndex(1,sitSize,StrToInt(Edit2.Text));
  SetIndex(1,sitDate,DateTimePicker1.Date);
  SetIndex(1,sitSubject,Edit3.Text);
  SetIndex(1,sitAttach,ListBox1.Items.Text);
  SetIndex(1,sitBody,Memo1.Lines.Text);
 end;

 POPperData1.SaveToFile(ExtractFilePath(ParamStr(0)) + 'popper.pop');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
// Button2Click(Sender);
end;

end.
