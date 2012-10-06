unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, POPperData;

type
  TfrmUjlevelolvasasa = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    txtSender: TEdit;
    txtSize: TEdit;
    txtDate: TEdit;
    txtSubject: TEdit;
    lstAttach: TListBox;
    txtBody: TMemo;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    POPperData1: TPOPperData;
    SaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Read(Index: integer);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    CurrentIndex: integer;
  public
    { Public declarations }
  end;

var
  frmUjlevelolvasasa: TfrmUjlevelolvasasa;

implementation

{$R *.DFM}

procedure TfrmUjlevelolvasasa.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TfrmUjlevelolvasasa.Read(Index: integer);
begin
 CurrentIndex := Index;

 POPperData1.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');

 txtSender.Text := POPperData1.GetIndex(Index,sitSender);
 txtSize.Text := IntToStr(integer(POPperData1.GetIndex(Index,sitSize)));
 txtDate.Text := DateTimeToStr(TDateTime(POPperData1.GetIndex(Index,sitDate)));
 txtSubject.Text := POPperData1.GetIndex(Index,sitSubject);
 lstAttach.Items.Text := POPperData1.GetIndex(Index,sitAttach);
 txtBody.Lines.Text := POPperData1.GetIndex(Index,sitBody);

 POPperData1.SetIndex(Index,sitRead,true);

 POPperData1.SaveToFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');
end;

procedure TfrmUjlevelolvasasa.SpeedButton2Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmUjlevelolvasasa.SpeedButton3Click(Sender: TObject);
begin
 if Application.MessageBox('Tényleg törlöd az épp olvasott levelet?','Putra POPper',mb_YesNo + mb_IconQuestion) = id_No then
  Abort;

 POPperData1.Delete(CurrentIndex);
 POPperData1.SaveToFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop'); 
end;

procedure TfrmUjlevelolvasasa.SpeedButton1Click(Sender: TObject);
var s,d: TStream;
begin
 if lstAttach.ItemIndex = -1 then
  Abort;

 SaveDialog1.InitialDir := 'C:\';
 SaveDialog1.FileName := 'C:\' + lstAttach.Items[lstAttach.ItemIndex];

 if not SaveDialog1.Execute then
  Abort;

 txtBody.Lines.SaveToFile(SaveDialog1.FileName);

 s := TFileStream.Create(ExtractFilePath(ParamStr(0)) + lstAttach.Items[lstAttach.ItemIndex],fmOpenRead);
 d := TFileStream.Create(SaveDialog1.FileName,fmShareDenyWrite + fmOpenWrite);

 d.CopyFrom(s,s.Size);
 
 d.Free;
 s.Free;
end;

end.
