unit Filter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, ComCtrls,db;

type
  TFilterForm = class(TForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    cbITA: TCheckBox;
    cbUSA: TCheckBox;
    cbHOL: TCheckBox;
    cbUK: TCheckBox;
    cbGER: TCheckBox;
    cbSWE: TCheckBox;
    cbOTH: TCheckBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Year_From: TEdit;
    Year_To: TEdit;
    cbBLANK: TCheckBox;
    GroupBox3: TGroupBox;
    Filter_on: TRadioButton;
    Filter_off: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure FilterChange(Sender: TObject);
    procedure Year_FromChange(Sender: TObject);
    procedure Year_ToChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FilterForm: TFilterForm;

implementation

uses Main;

{$R *.DFM}

procedure TFilterForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFilterForm.FilterChange(Sender: TObject);
begin
  mainform.DbfDisco.filtered:=filter_on.checked;
  mainform.DbfDisco.refresh;
end;

procedure TFilterForm.Year_FromChange(Sender: TObject);
begin
  Year_From.Tag:=StrToIntDef(Year_From.Text,0);
end;

procedure TFilterForm.Year_ToChange(Sender: TObject);
begin
  Year_To.Tag:=StrToIntDef(Year_To.Text,99);
end;

procedure TFilterForm.FormShow(Sender: TObject);
begin
  Filter_on.Checked:=true;
end;

procedure TFilterForm.FormHide(Sender: TObject);
begin
  Filter_off.Checked:=true;
end;

end.

