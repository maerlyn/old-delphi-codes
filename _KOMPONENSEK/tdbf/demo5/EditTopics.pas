unit EditTopics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls, Grids, DBGrids, ExtCtrls, ComCtrls, Buttons, db;

type
  TEditTopicsForm = class(TForm)
    Grid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    Button1: TButton;
    DBRichEdit1: TDBRichEdit;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    Pack: TButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure DBRichEdit1Enter(Sender: TObject);
    procedure PackClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  EditTopicsForm: TEditTopicsForm;

implementation

uses Main;

{$R *.DFM}

procedure TEditTopicsForm.Button1Click(Sender: TObject);
begin
  Close;
end;




procedure TEditTopicsForm.SpeedButton1Click(Sender: TObject);
var
  att:TTextAttributes;
begin
  att:=DBRichedit1.SelAttributes;

  case TSpeedButton(Sender).Tag of
  0: if fsBold in att.Style then att.Style:=att.Style - [fsBold] else att.Style:=att.Style + [fsBold];
  1: if fsItalic in att.Style then att.Style:=att.Style - [fsItalic] else att.Style:=att.Style + [fsItalic];
  2: if fsUnderline in att.Style then att.Style:=att.Style - [fsUnderline] else att.Style:=att.Style + [fsUnderline];
  3: att.size:=8;
  4: att.size:=10;
  5: att.size:=12;
  6: att.Color:=clBlack;
  7: att.Color:=clMaroon;
  8: att.Color:=clNavy;
  9: att.Name:='Arial';
  10:att.Name:='Times New Roman';
  11:att.Name:='Courier New';
  12:att.Color:=clGreen;
  end;

end;

procedure TEditTopicsForm.DBRichEdit1Enter(Sender: TObject);
begin
  if Mainform.DbfDemo.State=dsBrowse then MainForm.DbfDemo.Edit;
end;


procedure TEditTopicsForm.PackClick(Sender: TObject);
begin
  MainForm.DbfDemo.PackTable;
end;





end.

