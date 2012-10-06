unit AmobaClientList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TModifyField = (zName,zIPAddress,zWins,zLoses);

  TRecordType = record
    Name: string[255];
    IPAddress: string[15];
    Wins: byte;
    Loses: byte;
  end;

  TAmobaClientList = class(TComponent)
  private
    fData: array[1..16] of TRecordType;
    fCount: integer;
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    function Find(cName: string): integer;
    function Find2(cIP: string): integer;
    function Get(Index: integer): TRecordType;
    procedure Set_(Index: integer; Which: TModifyField; Data: variant);
    procedure AddNew(cName, cIPAddress: string);
    procedure Delete(Index: integer);
  published
    property Count: integer read fCount;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Sajat', [TAmobaClientList]);
end;

{ TAmobaClientList }

procedure TAmobaClientList.AddNew(cName, cIPAddress: string);
begin
 if fCount = 16 then
 begin
  raise Exception.Create('Too many clients!');
  Abort;
 end;

 inc(fCount);
 fData[fCount].Name := cName;
 fData[fCount].IPAddress := cIPAddress;
 fData[fCOunt].Wins := 0;
 fData[fCount].Loses := 0;
end;

constructor TAmobaClientList.Create(AOwner: TComponent);
var i: integer;
begin
 inherited;
 for i := Low(fData) to High(fData) do
 begin
  fData[i].Name := '';
  fData[i].IPAddress := '';
  fData[i].Wins := 0;
  fData[i].Loses := 0;
 end;
 fCount := 0;
end;

procedure TAmobaClientList.Delete(Index: integer);
var i: integer;
begin
 for i := Index to 15 do
 begin
  fData[i].Name := fData[i+1].Name;
  fData[i].IPAddress := fData[i+1].IPAddress;
  fData[i].Wins := fData[i+1].Wins;
  fData[i].Loses := fData[i+1].Loses;
 end;

 fData[16].Name := '';
 fData[16].IPAddress := '';
 fData[16].Wins := 0;
 fData[16].Loses := 0;

 dec(fCount);
end;

function TAmobaClientList.Find(cName: string): integer;
var i: integer;
begin
 Result := -1;
 for i := 1 to 16 do
  if fData[i].Name = cName then
   Result := i;
end;

function TAmobaClientList.Find2(cIP: string): integer;
var i: integer;
begin
 Result := -1;
 for i := 1 to 16 do
  if fData[i].IPAddress = cIP then
   Result := i;
end;

function TAmobaClientList.Get(Index: integer): TRecordType;
begin
 Result := fData[Index];
end;

procedure TAmobaClientList.Set_(Index: integer; Which: TModifyField; Data: variant);
begin
 case Which of
  zName:      fData[Index].Name :=      Data;
  zIPAddress: fData[Index].IPAddress := Data;
  zWins:      fData[Index].Wins :=      Data;
  zLoses:     fData[Index].Loses :=     Data;
 end;
end;

end.
