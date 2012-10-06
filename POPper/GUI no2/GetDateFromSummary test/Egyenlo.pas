unit Egyenlo;

interface

uses Windows, Classes, SysUtils;

function EgyenlosegTorles(Mibol: string): string;
function TargyAtalakitas(Mibol: string; SecondCall: boolean): string;
function GetDateFromSummary(summary: string): TDateTime;

implementation

const Replace: array[1..19,1..2] of string = (('E1','225'), ('E9','233'), ('ED','237'),
                                              ('F3','243'), ('F6','246'), ('F5','245'),
                                              ('FA','250'), ('FC','252'), ('FB','251'),
                                              ('C1','193'), ('C9','201'), ('CD','205'),
                                              ('D3','211'), ('D6','214'), ('D5','213'),
                                              ('DA','218'), ('DC','220'), ('DB','219'),
                                              ('20','32'));

//------------------------------------------------------------------------------
//function Hex2Dec(Hex: string): integer
// bemenet: 2 karakteres string formájú hexadecimális érték
// kimenet: integer formába átalakítva a bemenet
//------------------------------------------------------------------------------

function Hex2Dec(Hex: string): integer;
begin
 if length(Hex) <> 2 then
  Abort;

 try
  Hex := UpperCase(Hex);

  if Hex[1] = 'F' then
   Result := 16*15
  else if Hex[1] = 'E' then
   Result := 16*14
  else if Hex[1] = 'D' then
   Result := 16*13
  else if Hex[1] = 'C' then
   Result := 16*12
  else if Hex[1] = 'B' then
   Result := 16*11
  else if Hex[1] = 'A' then
   Result := 16*10
  else
   Result := 16*StrToInt(Hex[1]);

  if Hex[2] = 'F' then
   inc(Result,15)
  else if Hex[2] = 'E' then
   inc(Result,14)
  else if Hex[2] = 'D' then
   inc(Result,13)
  else if Hex[2] = 'C' then
   inc(Result,12)
  else if Hex[2] = 'B' then
   inc(Result,11)
  else if Hex[2] = 'A' then
   inc(Result,10)
  else
   inc(Result,StrToInt(Hex[2]));
 except
  Result := 35; // ord('#')
 end;
end;

//------------------------------------------------------------------------------
//function EgyenlosegTorles(Mibol: string): string;
// bemenet: string, benne ilyenek, mint '=E1'
// kimenet: az '=E1' alakú karakterek normál karakterré átalakítva
//------------------------------------------------------------------------------

function EgyenlosegTorles(Mibol: string): string;
var i: integer;
    s, ss: string;
    sl: TStringList;
//    k: integer;
//    h: string;
begin
 sl := TStringList.Create;
 sl.Text := Mibol;

 s := '';
 ss := '';

 for i := 0 to sl.Count-1 do
 begin
  s := sl[i];
  if length(s) <> 0 then
   if s[length(s)] = '=' then
   begin
    delete(s,length(s),1);
    ss := ss + s
   end
   else
    ss := ss + s + #13#10
  else
   ss := ss + #13#10;

  if length(trim(s)) = 0 then
   ss := ss + #13#10;
 end;

 for i := 1 to 18 do
  while pos('=' + Replace[i,1],ss)>0 do
   ss := copy(ss,1,pos('=' + Replace[i,1],ss)-1) +
         chr(StrToInt(Replace[i,2])) +
         copy(ss,pos('=' + Replace[i,1],ss)+3,length(ss));

 Result := ss;

 sl.Free;
end;

//------------------------------------------------------------------------------
//function TargyAtalakitas(Mibol: string): string;
// bemenet: string, pl. '=?iso-8859-2?Q?Lev=E3lke?='
// kimenet: string, pl. 'Levélke'
//------------------------------------------------------------------------------

function TargyAtalakitas(Mibol: string; Secondcall: boolean): string;
var s: string;
begin
 s := Mibol;

 if pos('=?iso',s) = 1 then
 begin
  delete(s,1,length('=?iso-8859-2?Q?'));
  delete(s,length(s)-1,2);
 end;

 if length(s) > 0 then
  if s[length(s)-1] = '?' then
  begin
   delete(s,length(s),1);
   while pos('?',s) > 0 do
    delete(s,1,pos('?',s));
  end;

 while pos('_',s) > 0 do
  s := copy(s,1,pos('_',s)-1) + #32 + copy(s,pos('_',s)+1,length(s));

 s := EgyenlosegTorles(s);

 if length(s) > 0 then
  if s[length(s)-1] = #10 then
   delete(s,length(s)-1,2);

 if not SecondCall then
  s := TargyAtalakitas(s,true);

 Result := s;
end;

function GetMonthIndex(s: string): byte;
const Months: array[1..12] of string = ('Jan','Feb','Mar','Apr','May','Jun',
                                        'Jul','Aug','Sep','Oct','Nov','Dec');
var i: integer;
begin
 for i := 1 to 12 do
  if s = Months[i] then
  begin
   Result := i;
   Exit;
  end;
end;

//function GetDateFromSummary(summary: string): TDateTime;
// nmPOP3 komponenssel lekért summaryból szedi ki a levél
// megérkezésének dátumát

function GetDateFromSummary(summary: string): TDateTime;
var tsl: TStringList;
    i: integer;
    s: string;
    Year,Month,Day: word;
    Hour,Minute: word;
begin
 Result := Now;
 tsl := TStringList.Create;
 tsl.Text := summary;
 i := 0;
 while pos('Received:',tsl[i]) <> 1 do
  inc(i);

 s := tsl[i];
 delete(s,1,pos(';',s)+1);
 Day := StrToInt(copy(s,1,pos(' ',s)-1));
 delete(s,1,pos(' ',s));
 Month := GetMonthIndex(copy(s,1,pos(' ',s)-1));
 delete(s,1,pos(' ',s));
 Year := StrToInt(copy(s,1,pos(' ',s)-1));
 delete(s,1,pos(' ',s));
 Hour := StrToInt(copy(s,1,pos(':',s)-1));
 delete(s,1,pos(':',s));
 Minute := StrToInt(copy(s,1,pos(':',s)-1));

 Result := EncodeDate(Year,Month,Day) + EncodeTime(Hour,Minute,0,0);
 tsl.Free;
end;

end.
