unit CommForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TCommFrm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    Label6: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CommFrm: TCommFrm;

implementation

{$R *.DFM}

end.
