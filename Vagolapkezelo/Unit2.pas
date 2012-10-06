unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Registry;

type
  TForm2 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    btnBovebbRendszerinfo: TButton;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Image17: TImage;
    Image18: TImage;
    Image19: TImage;
    Image20: TImage;
    Image21: TImage;
    Image22: TImage;
    Image23: TImage;
    Image24: TImage;
    Image25: TImage;
    Image26: TImage;
    Image27: TImage;
    Image28: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnBovebbRendszerinfoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

type
 TLangInfoBuffer = array[1..4] of SmallInt;

procedure TForm2.FormCreate(Sender: TObject);
var
  VInfoSize, DetSize: DWord;
  pVInfo, pDetail: Pointer;
  pLangInfo: ^TLangInfoBuffer;
  strLangId: string;
  Reg: TRegistry;
begin
  VInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), DetSize);
  if VInfoSize > 0 then
  begin
    GetMem(pVInfo, VInfoSize);
    try
       GetFileVersionInfo(PChar(ParamStr(0)), 0, VInfoSize, pVInfo);
       VerQueryValue (pVInfo, '\', pDetail, DetSize);
       VerQueryValue(pVInfo, '\VarFileInfo\Translation', Pointer(pLangInfo), DetSize);
       strLangId := IntToHex (SmallInt (pLangInfo^ [1]), 4) + IntToHex (SmallInt (pLangInfo^ [2]), 4);
       strLangId := '\StringFileInfo\' + strLangId;
       VerQueryValue(pVInfo, PChar(strLangId + '\ProductVersion'), pDetail, DetSize);
       Label3.Caption := PChar(pDetail);
       Reg := TRegistry.Create;
       Reg.RootKey := HKEY_LOCAL_MACHINE;
       Reg.OpenKey('SOFTWARE\MICROSOFT\WINDOWS\CURRENTVERSION',false);
       Label4.Caption := 'Felhasználó: ' + Reg.ReadString('RegisteredOwner');
       Label5.Caption := 'Oprendszer: ' + Reg.ReadString('ProductName');
       Reg.Free;
    finally
      FreeMem (pVInfo);
    end;
  end;
end;


procedure TForm2.btnBovebbRendszerinfoClick(Sender: TObject);
var Reg: TRegistry;
    temp: string;
begin
 Reg := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey('SOFTWARE\MICROSOFT\SHARED TOOLS\MSINFO',false);
 temp := Reg.ReadString('Path');
 WinExec(PChar(temp),SW_SHOWNORMAL);
 Reg.Free;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caNone;
 Form2.Visible := False;
 //ShowWindow(Handle, SW_HIDE);
end;

end.
