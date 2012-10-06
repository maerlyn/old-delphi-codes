unit Unit7;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Unit8, IniFiles;

type
  TfrmLevelszurok = class(TForm)
    frameSzuro1: TframeSzuro;
    frameSzuro2: TframeSzuro;
    frameSzuro3: TframeSzuro;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    frameSzuro4: TframeSzuro;
    Bevel4: TBevel;
    frameSzuro5: TframeSzuro;
    Bevel5: TBevel;
    frameSzuro6: TframeSzuro;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure frameSzuro1cmdMentesClick(Sender: TObject);
  private
    cfg: TIniFile;
  public
    { Public declarations }
  end;

var
  frmLevelszurok: TfrmLevelszurok;
  FilterCount: byte;

implementation

{$R *.DFM}

procedure TfrmLevelszurok.FormCreate(Sender: TObject);
begin
 Self.Width := 725;
 Self.Height := 531;

 frameSzuro1.Initialize;
 frameSzuro2.Initialize;
 frameSzuro3.Initialize;
 frameSzuro4.Initialize;
 frameSzuro5.Initialize;
 frameSzuro6.Initialize;

 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');
end;

procedure TfrmLevelszurok.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 cfg.Free;
 Action := caFree;
end;

procedure TfrmLevelszurok.FormShow(Sender: TObject);
begin
 Self.Width := 725;
 Self.Height := 531;
end;

procedure TfrmLevelszurok.frameSzuro1cmdMentesClick(Sender: TObject);
begin
 frameSzuro1.cmbM
end;

end.
