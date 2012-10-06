unit Index;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, DBCtrls, Grids, DBGrids, ComCtrls;

type
  TIndexForm = class(TForm)
    Button1: TButton;
    DBGrid1: TDBGrid;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  IndexForm: TIndexForm;

implementation

uses Main;

{$R *.DFM}

procedure TIndexForm.Button2Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='AUTHOR.NDX';
end;

procedure TIndexForm.Button3Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='TITLE.NDX';
end;

procedure TIndexForm.Button4Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='';
end;

procedure TIndexForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TIndexForm.Button5Click(Sender: TObject);
begin
  MainForm.DbfDisco.IndexName:='PRICE.NDX';
end;

procedure TIndexForm.Edit1Change(Sender: TObject);
begin
  MainForm.DbfDisco.BracketLow:=Edit1.Text;
end;

procedure TIndexForm.Edit2Change(Sender: TObject);
begin
  MainForm.DbfDisco.BracketHigh:=Edit2.Text;
end;

end.

