unit List;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, DBGrids, Db, ADODB;

type
  TfrmList = class(TForm)
    DBGrid1: TDBGrid;
    cmdClose: TButton;
    ADOTable1: TADOTable;
    DataSource1: TDataSource;
    procedure FormResize(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmList: TfrmList;

implementation

{$R *.DFM}

procedure TfrmList.FormResize(Sender: TObject);
begin
 DBGrid1.Left := 0;
 DBGrid1.Top := 0;
 DBGrid1.Width := frmList.ClientWidth;
 DBGrid1.Height := frmList.ClientHeight - cmdClose.Height;
 cmdClose.Top := DBGrid1.Height + 1;
 cmdClose.Left := 0;
 cmdClose.Width := frmList.ClientWidth;
end;

procedure TfrmList.cmdCloseClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmList.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

end.
