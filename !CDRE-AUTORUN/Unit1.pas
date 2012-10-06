unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  Betuk: array[1..26] of string = ('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
    megvan: boolean;
begin
 MessageDlg('Üdvözöllek!'#13#10'Másolj annyi file-t, amennyit csak akarsz, de 1 kérésem lenne:'#13#10'A programokat ne módosítsd, vagy add tovább a saját neveden!'#13#10'Kösz:'#13#10'     A CD készítõje',mtInformation,[mbOk],0);

 megvan := false;
 for i := 1 to 26 do
  if FileExists(Betuk[i] + ':\Autorun.exe') then
   if FileExists(Betuk[i] + ':\icon.ico') then
    if FileExists(Betuk[i] + ':\Autorun.inf') then
     begin
     WinExec(PChar('explorer ' + Betuk[i] + ':'),sw_ShowNormal);
     megvan := true;
     end;
 if not megvan then
   Application.MessageBox('Nem találom a CDt amire ezt a prg-t írták!','Hiba',mb_Ok + mb_IconHand);

 Application.Terminate;
end;

end.

