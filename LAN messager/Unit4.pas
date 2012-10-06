unit Unit4;

interface

uses SysUtils, Windows;

type TAddressData = record
                     Nev: string[30];
                     IP: string[15];
                    end;

     TAddressHeader = record
                       Azonosito: array[1..26] of char;
                       Mennyi: word;
                      end;

     TUzenetAdat = record
                    Uj: boolean;
                    Surgos: boolean;
                    Datum: TDateTime;
                    Kuldo: string[100];
                    Uzenet: array[1..2048] of char;
//                    Uzenet: string[255];
                   end;

     TUzenetFejlec = record
                      Azonosito: array[1..25] of char;
                      Mennyi: word;
                     end;


function  CountDots(s: string): integer;
function  GetIPOfName(s: string): string;
function  GetNameOfIP(s: string): string;
procedure LoadAddresses;
procedure SaveAddresses;
procedure LoadMessageData;
procedure SaveMessageData;

var Addresses: array[1..1024] of TAddressData;
    Uzenetek: array[1..65536] of TUzenetAdat;
    MennyiCim: word;
    MennyiUzenet: word;

implementation

function GetIPOfName(s: string): string;
var i: integer;
    ss: string;
begin
 if MennyiCim = 0 then
 begin
  Result := 'ERROR';
  Exit;
 end;

 if (pos('<',s)>0)and(pos('>',s)>0) then
 begin
  ss := copy(s,pos('<',s)+1,length(s));
  delete(ss,length(ss),1);
  Result := ss;
  Exit;
 end;

 for i := 1 to MennyiCim do
  if LowerCase(Addresses[i].Nev) = LowerCase(s) then
  begin
   Result := s + ' <' + Addresses[i].IP + '>';
   Exit;
  end;

 Result := 'ERROR';
end;

function GetNameOfIP(s: string): string;
var i: integer;
begin
 if MennyiCim = 0 then
 begin
  Result := s;
  Exit;
 end;

 for i := 1 to MennyiCim do
  if Addresses[i].IP = s then
  begin
   Result := Addresses[i].Nev + ' <' + s + '>';
   Exit;
  end;

 Result := s; 
end;

function CountDots(s: string): integer;
var i: integer;
begin
 Result := 0;
 for i := 1 to length(s) do
  if s[i] = '.' then
   inc(Result);
end;

procedure SaveMessageData;
var fejlec: TUzenetFejlec;
    fileout: file;
begin
 AssignFile(fileout,ExtractFilePath(ParamStr(0)) + 'messages.lan');
 try
  ReWrite(fileout,1);
 except
//  Application.MessageBox('Nem sikerült elmenteni az üzeneteket!','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem sikerült elmenteni az üzeneteket!','LAN messager',mb_Ok + mb_IconError);
  Abort;
 end;

 fejlec.Azonosito := 'PUTRALANMESSAGER-MESSAGES';
 fejlec.Mennyi := MennyiUzenet;
 BlockWrite(fileout,fejlec,sizeof(fejlec));
 BlockWrite(fileout,Uzenetek,sizeof(TUzenetAdat)*MennyiUzenet);
 CloseFile(fileout);
end;

procedure LoadMessageData;
var header: TUzenetFejlec;
    filein: file;
begin
 AssignFile(filein,ExtractFilePath(ParamStr(0)) + 'messages.lan');
 try
  Reset(filein,1);
 except
//  Application.MessageBox('Nem sikerült megnyitni az üzeneteket tartalmazó filet.','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem sikerült megnyitni az üzeneteket tartalmazó filet','LAN messager',mb_OK + mb_IconError);
  Exit;
 end;
 BlockRead(filein,header,sizeof(header));
 if header.Azonosito <> 'PUTRALANMESSAGER-MESSAGES' then
 begin
//  Application.MessageBox('Nem érvényes az üzeneteket tartalmazó file!','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem érvényes az üzeneteket tartalmazó file!','LAN messager',mb_OK + mb_IconError);
  Abort;
 end;
 MennyiUzenet := header.Mennyi;
 BlockRead(filein,Uzenetek,sizeof(TUzenetAdat)*MennyiUzenet);
 CloseFile(filein);
end;

procedure SaveAddresses;
var fejlec: TAddressHeader;
    fileout: file;
begin
 AssignFile(fileout,ExtractFilePath(ParamStr(0)) + 'addresses.lan');
 try
  ReWrite(fileout,1);
 except
//  Application.MessageBox('Nem sikerült elmenteni az üzeneteket!','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem sikerült elmenteni a címeket!','LAN messager',mb_Ok + mb_IconError);
  Abort;
 end;

 fejlec.Azonosito := 'PUTRALANMESSAGER-ADDRESSES';
 fejlec.Mennyi := MennyiCim;
 BlockWrite(fileout,fejlec,sizeof(fejlec));
 BlockWrite(fileout,Addresses,sizeof(TAddressData)*MennyiCim);
 CloseFile(fileout);
end;

procedure LoadAddresses;
var header: TAddressHeader;
    filein: file;
begin
 AssignFile(filein,ExtractFilePath(ParamStr(0)) + 'addresses.lan');
 try
  Reset(filein,1);
 except
//  Application.MessageBox('Nem sikerült megnyitni az üzeneteket tartalmazó filet.','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem sikerült megnyitni a címeket tartalmazó filet','LAN messager',mb_OK + mb_IconError);
  Exit;
 end;
 BlockRead(filein,header,sizeof(header));
 if header.Azonosito <> 'PUTRALANMESSAGER-ADDRESSES' then
 begin
//  Application.MessageBox('Nem érvényes az üzeneteket tartalmazó file!','LAN messager',mb_OK + mb_IconError);
  MessageBox(0,'Nem érvényes a címeket tartalmazó file!','LAN messager',mb_OK + mb_IconError);
  Abort;
 end;
 MennyiCim := header.Mennyi;
 BlockRead(filein,Addresses,sizeof(TAddressData)*MennyiCim);
 CloseFile(filein);
end;

end.
 