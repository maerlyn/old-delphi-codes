unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, jpeg, ExtCtrls;

type
  TfrmSplash = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type TLangInfoBuffer = array[1..4] of smallint;

var
  frmSplash: TfrmSplash;

implementation

{$R *.DFM}

procedure TfrmSplash.FormCreate(Sender: TObject);
var VInfoSize, DetSize: dword;
    pVInfo, pDetail: pointer;
    MajorVer, MinorVer: integer;
    FileDescription: string;
    pLangInfo: ^TLangInfoBuffer;
    strLangID: string;
begin
 MajorVer := 0;
 MinorVer := 0;

 VInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),DetSize);
 if VInfoSize > 0 then
 begin
  GetMem(pVInfo,VInfoSize);
  try
   GetFileVersionInfo(PChar(ParamStr(0)),0,VInfoSize,pVInfo);
   VerQueryValue(PVInfo,'\',pDetail,DetSize);
   with TVSFixedFileInfo(pDetail^) do
   begin
    MajorVer := HiWord(dwFileVersionMS);
    MinorVer := LoWord(dwFileVersionMS);
    VerQueryValue(pVInfo,'\VarFileInfo\Translation',pointer(pLangInfo),DetSize);
    strLangID := IntToHex(smallint(pLangInfo^[1]),4) + IntToHex(smallint(pLangInfo^[2]),4);
    strLangID := 'StringFileInfo\' + strLangID;
    VerQueryValue(pVInfo,PChar(strLangID + '\FileDescription'),pDetail,DetSize);
    FileDescription := PChar(pDetail);
   end;
  finally
   FreeMem(PVInfo);
  end;
 end;

 Label1.Caption := 'Putra POPper v' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + #32 + FileDescription;
end;

procedure TfrmSplash.Timer1Timer(Sender: TObject);
begin
 Timer1.Enabled := false;
 Self.Close;
 Self.Release;
end;

end.
