unit POPperData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TSetIndexType = (sitSender,sitRead,sitSize,sitDate,sitSubject,sitAttach,sitBody);
  TSetIndexType2 =(sit2To,sit2CC,sit2BCC,sit2Subject,sit2Attach,sit2Body);
  TSetIndexType3 =(sit3Name,sit3Address);

  TDatabase = record
    Sender:       string;
    Read:         boolean;
    Size:         integer;
    Date:         TDateTime;
    Subject:      string;
    AttachFiles:  TStringList;
    Body:         TStringList;
  end;

  TDatabase2 = record
    To_: string;
    CC: string;
    BCC: string;
    Subject: string;
    Attach: TStringList;
    Body: TStringList;
  end;

  TDatabase3 = record
    Name: string[100];
    Address: string[100];
  end;

  TDatabase3Header = record
    Azonosito: array[1..17] of char;
    Mennyi: word;
  end;

  TPOPperData = class(TComponent)
  private
    fData: array[1..32767] of TDatabase;
    fCount: integer;
  protected
    procedure AddZeroIfNeeded(Variable: word; var Str: string);
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy;override;
    function    SaveToFile(FileName: string): boolean;
    function    LoadFromFile(FileName: string): boolean;
    procedure   SetIndex(Index: integer; Which: TSetIndexType; Value: variant);
    function    GetIndex(Index: integer; Which: TSetIndexType): variant;
    procedure   AddNew(nSender: string; nRead: boolean; nSize: integer; nDate: TDateTime; nSubject, nAttach, nBody: string);
    procedure   Delete(Index: integer);
    function    GetItems: string;
    function    IndexOfItem(Item: string; Date: TDateTime): integer;
  published
    property Count: integer read fCount write fCount;
  end;

  TPOPperData2 = class(TComponent)
  private
    fData: array[1..32767] of TDatabase2;
    fCount: integer;
  public
    constructor Create(AOwner: TComponent);override;
    destructor  Destroy;override;
    function    SaveToFile(FileName: string): boolean;
    function    LoadFromFile(FileName: string): boolean;
    procedure   SetIndex(Index: integer; Which: TSetIndexType2; Value: string);
    function    GetIndex(Index: integer; Which: TSetIndexType2): string;
    procedure   AddNew(nTo, nCC, nBCC, nSubject, nAttach, nBody: string);
    procedure   Delete(Index: integer);
    function    GetItems: string;
    function    IndexOfItem(Item: string): integer;
  published
    property Count: integer read fCount write fCount;
  end;

  TPOPperData3 = class(TComponent)
  private
    fData: array[1..32767] of TDatabase3;
    fCount: integer;
  public
    constructor Create(AOwner: TComponent);override;
    function  LoadFromFile(FileName: string): boolean;
    function  SaveToFile(FileName: string): boolean;
    procedure SetIndex(Index: integer; Which: TSetIndexType3; Value: string);
    function  GetIndex(Index: integer; Which: TSetIndexType3): string;
    procedure AddNew(nName, nAddress: string);
    procedure DeleteIndex(Index: integer);
    function  GetItems: string;
    function  IndexOfItem(Item: string): integer;
  published
    property Count: integer read fCount write fCount;
  end;

procedure Register;
procedure Delay(Seconds, MilliSec: Word);

implementation

procedure Delay(Seconds, MilliSec: Word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

procedure Register;
begin
  RegisterComponents('Sajat', [TPOPperData]);
  RegisterComponents('Sajat', [TPOPperData2]);
  RegisterComponents('Sajat', [TPOPperData3]);
end;

{ TPOPperData }

procedure TPOPperData.AddNew(nSender: string; nRead: boolean; nSize: integer; nDate: TDateTime; nSubject, nAttach, nBody: string);
begin
 inc(fCount);
// Delay(0,10);

 fData[fCount].Sender :=           nSender;
 fData[fCount].Read :=             nRead;
 fData[fCount].Size :=             nSize;
 fData[fCount].Date :=             nDate;
 fData[fCount].Subject :=          nSubject;
 fData[fCount].AttachFiles.Text := nAttach;
 fData[fCount].Body.Text :=        nBody;
end;

procedure TPOPperData.AddZeroIfNeeded(Variable: word; var Str: string);
begin
 if length(IntToStr(Variable)) = 1 then
  Str := Str + '0';
 Str := Str + IntToStr(Variable);
end;

constructor TPOPperData.Create(AOwner: TComponent);
var i: integer;
begin
 inherited;

 for i := low(fData) to high(fData) do
  with fData[i] do
   begin
    Sender := '';
    Read := false;
    Size := 0;
    Date := Now;
    Subject := '';
    AttachFiles := TStringList.Create;
    AttachFiles.Text := '';
    Body := TStringList.Create;
    Body.Text := '';
   end;

 fCount := 0;
end;

procedure TPOPperData.Delete(Index: integer);
var i: integer;
begin
 if Index < 1 then
  Abort;
 if Index > fCount then
  Abort;

 i := Index;
 while i < fCount-1 do
 begin
  fData[i].Sender :=           fData[i+1].Sender;
  fData[i].Read   :=           fData[i+1].Read;
  fData[i].Size   :=           fData[i+1].Size;
  fData[i].Date   :=           fData[i+1].Date;
  fData[i].Subject:=           fData[i+1].Subject;
  fData[i].AttachFiles.Text := fData[i+1].AttachFiles.Text;
  fData[i].Body.Text :=        fData[i+1].Body.Text;
  inc(i);
 end;

 fData[fCount].Sender := '';
 fData[fCount].Read := false;
 fData[fCount].Size := 0;
 fData[fCount].Date := Now;
 fData[fCount].Subject := '';
 fData[fCount].AttachFiles.Text := '';
 fData[fCount].Body.Text := '';

 dec(fCount);
end;

destructor TPOPperData.Destroy;
var i: integer;
begin
 inherited;

 for i := low(fData) to high(fData) do
  with fData[i] do
  begin
   AttachFiles.Free;
   Body.Free;
  end;
end;

function TPOPperData.GetIndex(Index: integer; Which: TSetIndexType): variant;
begin
 if Index < 1 then
  Abort;
 if Index > fCount then
  Abort;

 case Which of
  sitSender:  Result := fData[Index].Sender;
  sitRead:    Result := fData[Index].Read;
  sitSize:    Result := fData[Index].Size;
  sitDate:    Result := fData[Index].Date;
  sitSubject: Result := fData[Index].Subject;
  sitAttach:  Result := fData[Index].AttachFiles.Text;
  sitBody:    Result := fData[Index].Body.Text;
 end;
end;

function TPOPperData.GetItems: string;
var i: integer;
    tsl: TStringList;
begin
 tsl := TStringList.Create;
 for i := 1 to fCount do
  tsl.Add(fData[i].Sender);

 Result := tsl.Text;

 tsl.Free;
end;

function TPOPperData.IndexOfItem(Item: string; Date: TDateTime): integer;
var i: integer;
begin
 Result := 0;

 for i := 1 to fCount do
  if fData[i].Sender = Item then
   if fData[i].Date = Date then
    Result := i;
end;

function TPOPperData.LoadFromFile(FileName: string): boolean;
var tsl: TStringList;
    i: integer;
    ts: string;
    ss: string;
    Year,Month,Day,Hour,Minute: word;
begin
 Result := true;
 tsl := TStringList.Create;

 fCount := 0;
// Delay(0,10);

 try
  tsl.LoadFromFile(FileName);
  for i := 0 to tsl.Count-1 do
  begin
   ss := tsl[i];
   if trim(ss) <> '' then
   begin
    if ss[1] = '0' then
    begin
     inc(fCount);
     fData[fCount].Sender := copy(tsl[i],2,length(tsl[i]));
//     Delay(0,10);
    end else
    if ss[1] = '1' then
    begin
     ts := tsl[i];
     if ts[2] = 'R' then
      fData[fCount].Read := true
     else
      fData[fCount].Read := false;
     system.delete(ts,1,3);
     fData[fCount].Size := StrToInt(trim(copy(ts,1,pos(#32,ts))));
     system.delete(ts,1,pos(#32,ts));
     ts := trim(ts);
     Year := StrToInt(copy(ts,1,4));
     Month := StrToInt(copy(ts,5,2));
     Day := StrToInt(copy(ts,7,2));
     Hour := StrToInt(copy(ts,9,2));
     Minute := StrToInt(copy(ts,11,2));
     fData[fCount].Date := EncodeDate(Year,Month,Day) + EncodeTime(Hour,Minute,0,0);
//     Delay(0,10);
    end else
    if ss[1] = '2' then
    begin
     fData[fCount].Subject := copy(tsl[i],2,length(tsl[i]));
//     Delay(0,10);
    end else
    if ss[1] = '3' then
    begin
     fData[fCount].AttachFiles.Add(copy(tsl[i],2,length(tsl[i])));
//     Delay(0,10);
    end else
    if ss[1] = '4' then
    begin
     fData[fCount].Body.Add(copy(tsl[i],2,length(tsl[i])));
//     Delay(0,10);
    end;
   end;
  end; 

 except
  Result := false;
 end;

 tsl.Free;

end;

function TPOPperData.SaveToFile(FileName: string): boolean;
var i,k: integer;
    ToSave: TStringList;
    temp: string;
//    temp2: TStringList;
    Year,Month,Day,Hour,Minute,Sec,MSec:word;
begin
 ToSave := TStringList.Create;
// Delay(0,10);
// temp2 := TStringList.Create;

 for i := 1 to fCount do
 begin
//  Delay(0,10);
  ToSave.Add('0' + fData[i].Sender);
//  Delay(0,10);
  if fData[i].Read then
   temp := 'R '
  else
   temp := 'U ';
  DecodeDate(fData[i].Date,Year,Month,Day);
  DecodeTime(fData[i].Date,Hour,Minute,Sec,MSec);
  temp := temp + IntToStr(fData[i].Size) + #32;
  temp := temp + IntToStr(Year);
  AddZeroIfNeeded(Month,temp);
  AddZeroIfNeeded(Day,temp);
  AddZeroIfNeeded(Hour,temp);
  AddZeroIfNeeded(Minute,temp);
//  Delay(0,10);
  ToSave.Add('1' + temp);
//  Delay(0,10);
  ToSave.Add('2' + fData[i].Subject);
//  Delay(0,10);
//  temp2.Text := fData[i].AttachFiles.Text;
  for k := 0 to fData[i].AttachFiles.Count-1 do
  begin
   ToSave.Add('3' + fData[i].AttachFiles[k]);
//   Delay(0,10);
  end;
  for k := 0 to fData[i].Body.Count-1 do
  begin
   ToSave.Add('4' + fData[i].Body[k]);
//   Delay(0,10);
  end;
 end;

 Result := true;
 try
//  Delay(0,100);
  ToSave.SaveToFile(FileName);
//  Delay(0,100);
 except
  Result := false;
 end;

 ToSave.Free;
end;

procedure TPOPperData.SetIndex(Index: integer; Which: TSetIndexType; Value: variant);
begin
 if Index < 1 then
  Abort;
 if Index > fCount then
  Abort;

 case Which of
  sitSender:  fData[Index].Sender :=           string(Value);
  sitRead:    fData[Index].Read :=             boolean(Value);
  sitSize:    fData[Index].Size :=             integer(Value);
  sitDate:    fData[Index].Date :=             TDateTime(Value);
  sitSubject: fData[Index].Subject :=          string(Value);
  sitAttach:  fData[Index].AttachFiles.Text := string(Value);
  sitBody:    fData[Index].Body.Text :=        string(Value);
 end;
end;

{ TPOPperData2 }

procedure TPOPperData2.AddNew(nTo, nCC, nBCC, nSubject, nAttach, nBody: string);
begin
 inc(fCount);
// Delay(0,10);

 fData[fCount].To_ := nTo;
 fData[fCount].CC := nCC;
 fData[fCount].BCC := nBCC;
 fData[fCount].Subject := nSubject;
 fData[fCount].Attach.Text := nAttach;
 fData[fCount].Body.Text := nBody;
end;

constructor TPOPperData2.Create(AOwner: TComponent);
var i: integer;
begin
 inherited;

 fCount := 0;

 for i := low(fData) to high(fData) do
 begin
  fData[i].To_ := '';
  fData[i].CC := '';
  fData[i].BCC := '';
  fData[i].Subject := '';
  fData[i].Attach := TStringList.Create;
  fData[i].Body := TStringList.Create;

  fData[i].Attach.Text := '';
  fData[i].Body.Text := '';
 end;
end;

procedure TPOPperData2.Delete(Index: integer);
var i: integer;
begin
 if Index < 1 then
  Abort;

 if Index > fCount then
  Abort;

 i := Index;
 while i < fCount-1 do
 begin
  fData[i].To_ :=         fData[i+1].To_;
  fData[i].CC :=          fData[i+1].CC;
  fData[i].BCC :=         fData[i+1].BCC;
  fData[i].Subject :=     fData[i+1].Subject;
  fData[i].Attach.Text := fData[i+1].Attach.Text;
  fData[i].Body.Text :=   fData[i+1].Body.Text;
  inc(i);
 end;

 fData[fCount].To_ := '';
 fData[fCount].CC := '';
 fData[fCount].BCC := '';
 fData[fCount].Subject := '';
 fData[fCount].Attach.Text := '';
 fData[fCount].Body.Text := '';

 dec(fCount);
end;

destructor TPOPperData2.Destroy;
var i: integer;
begin
 inherited;

 for i := low(fData) to high(fData) do
 begin
  fData[i].Attach.Free;
  fData[i].Body.Free;
 end;
end;

function TPOPperData2.GetIndex(Index: integer; Which: TSetIndexType2): string;
begin
 if Index < 1 then
  Abort;

 if Index > fCount then
  Abort;

 case Which of
  sit2To:      Result := fData[Index].To_;
  sit2CC:      Result := fData[Index].CC;
  sit2BCC:     Result := fData[Index].BCC;
  sit2Subject: Result := fData[Index].Subject;
  sit2Attach:  Result := fData[Index].Attach.Text;
  sit2Body:    Result := fData[Index].Body.Text;
 end;
end;

function TPOPperData2.GetItems: string;
var tsl: TStringList;
    i: integer;
begin
 tsl := TStringList.Create;

 for i := 1 to fCount do
  tsl.Add(fData[i].To_);

 Result := tsl.Text; 

 tsl.Free;
end;

function TPOPperData2.IndexOfItem(Item: string): integer;
var i: integer;
begin
 Result := 0;

 for i := 1 to fCount do
  if fData[i].To_ = Item then
   Result := i;
end;

function TPOPperData2.LoadFromFile(FileName: string): boolean;
var tsl: TStringList;
    i: integer;
begin
 Result := true;
 tsl := TStringList.Create;

 fCount := 0;

 try
  tsl.LoadFromFile(FileName);
//  Delay(0,10);
  for i := 0 to tsl.Count-1 do
  begin
   if tsl[i][1] = '0' then
   begin
    inc(fCount);
//    Delay(0,10);
    fData[fCount].To_ := copy(tsl[i],2,length(tsl[i]));
   end else
   if tsl[i][1] = '1' then
    fData[fCount].CC := copy(tsl[i],2,length(tsl[i]))
   else
   if tsl[i][1] = '2' then
    fData[fCount].BCC := copy(tsl[i],2,length(tsl[i]))
   else
   if tsl[i][1] = '3' then
    fData[fCount].Subject := copy(tsl[i],2,length(tsl[i]))
   else
   if tsl[i][1] = '4' then
    fData[fCount].Attach.Add(copy(tsl[i],2,length(tsl[i])))
   else
   if tsl[i][1] = '5' then
    fData[fCount].Body.Add(copy(tsl[i],2,length(tsl[i])));
  end;  

 except
  Result := false;
 end;

 tsl.Free;
end;

function TPOPperData2.SaveToFile(FileName: string): boolean;
var i,k: integer;
    ToSave: TStringList;
begin
 ToSave := TStringList.Create;

 for i := 1 to fCount do
 begin
  ToSave.Add('0' + fData[i].To_);               //     Delay(0,10);
  ToSave.Add('1' + fData[i].CC);                //     Delay(0,10);
  ToSave.Add('2' + fData[i].BCC);               //     Delay(0,10);
  ToSave.Add('3' + fData[i].Subject);           //     Delay(0,10);
  for k := 0 to fData[i].Attach.Count-1 do
  begin
   ToSave.Add('4' + fData[i].Attach[k]);
//   Delay(0,10);
  end;
  for k := 0 to fData[i].Body.Count-1 do
  begin
   ToSave.ADd('5' + fData[i].Body[k]);
//   Delay(0,10);
  end;
 end;

 Result := true;
 try
//  Delay(0,100);
  ToSave.SaveToFile(FileName);
//  Delay(0,100);
 except
  Result := false;
 end;

 ToSave.Free;
end;

procedure TPOPperData2.SetIndex(Index: integer; Which: TSetIndexType2; Value: string);
begin
 if Index < 1 then
  Abort;

 if Index > fCount then
  Abort;

 case Which of
   sit2To:      fData[Index].To_ := Value;
   sit2CC:      fData[Index].CC := Value;
   sit2BCC:     fData[Index].BCC := Value;
   sit2Subject: fData[Index].Subject := Value;
   sit2Attach:  fData[Index].Attach.Text := Value;
   sit2Body:    fData[Index].Body.TExt := Value;
 end;
end;

{ TPOPperData3 }

procedure TPOPperData3.AddNew(nName, nAddress: string);
begin
 if fCount = high(fData) then
  Exit;

 inc(fCount);

 fData[fCount].Name := nName;
 fData[fCount].Address := nAddress;
end;

constructor TPOPperData3.Create(AOwner: TComponent);
var i: integer;
begin
 inherited;
 for i := low(fData) to high(fData) do
 begin
  fData[i].Name := '';
  fData[i].Address := '';
 end;
end;

procedure TPOPperData3.DeleteIndex(Index: integer);
var i: integer;
begin
 if Index < 1 then
  Exit;

 if Index > fCount then
  Exit;

 i := Index;
 while i < fCount do
 begin
  fData[i].Name := fData[i+1].Name;
  fData[i].Address := fData[i+1].Address;
  inc(i);
 end;

 fData[fCount].Name := '';
 fData[fCount].Address := '';

 dec(fCount);
end;

function TPOPperData3.GetIndex(Index: integer; Which: TSetIndexType3): string;
begin
 Result := '';

 if Index < 1 then
  Exit;
 if Index > fCount then
  Exit;

 case Which of
  sit3Name: Result := fData[Index].Name;
  sit3Address: Result := fData[Index].Address;
 end; 
end;

function TPOPperData3.GetItems: string;
var i: integer;
begin
 Result := '';

 for i := 1 to fCount do
  Result := Result + #13#10 + fData[i].Name;

 delete(Result,1,2);
end;

function TPOPperData3.IndexOfItem(Item: string): integer;
var i: integer;
begin
 Result := -1;

 for i := 1 to fCount do
  if fData[i].Name = Item then
   Result := i;
end;

function TPOPperData3.LoadFromFile(FileName: string): boolean;
var fejlec: TDatabase3Header;
    filein: file;
begin
 Result := true;
 AssignFile(filein,FileName);
 try
  Reset(filein,1);
  BlockRead(filein,fejlec,sizeof(fejlec));
  if fejlec.Azonosito <> 'POPPERADDRESSLIST' then
   Result := false;

  fCount := fejlec.Mennyi;
  BlockRead(filein,fData,fCount*sizeof(TDatabase3));
  CloseFile(filein);
 except
  Result := false;
 end;
end;

function TPOPperData3.SaveToFile(FileName: string): boolean;
var fejlec: TDatabase3Header;
    fileout: file;
begin
 Result := true;
 AssignFile(fileout,FileName);
 try
  Rewrite(fileout,1);

  fejlec.Azonosito := 'POPPERADDRESSLIST';
  fejlec.Mennyi := fCount;
  BlockWrite(fileout,fejlec,sizeof(fejlec));
  BlockWrite(fileout,fData,sizeof(TDatabase3)*fCount);
  CloseFile(fileout);
 except
  Result := false;
 end;
end;

procedure TPOPperData3.SetIndex(Index: integer; Which: TSetIndexType3; Value: string);
begin
 if Index < 1 then
  Exit;
 if Index > fCount then
  Exit;

 case Which of
  sit3Name: fData[Index].Name := Value;
  sit3Address: fData[Index].Address := Value;
 end;
end;

end.
