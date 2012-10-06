unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList, StdCtrls, Unit2;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ReReadData;
  end;

var
  Form1: TForm1;
  LastMeet: TDateTime;
  NextMeet: TDateTime;

implementation

{$R *.DFM}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var f: textfile;
    s1,s2:string;
    Year1,Month1,Day1,Hour1,Min1:word;
    Year2,Month2,Day2,Hour2,Min2:word;
begin
 LastMeet := EncodeDate(2003,9,21);            //elmeletileg LastMeet az utolso
 LastMeet := LastMeet + EncodeTime(15,40,0,0); //talalkozas datumat tartalmazza (persze idealizalva)

 NextMeet := EncodeDate(2003,9,26);
 NextMeet := NextMeet + EncodeTime(17,34,0,0);

 if FileExists(ExtractFilePath(ParamStr(0)) + '\data.txt') then
 begin
  AssignFile(f,ExtractFilePath(ParamStr(0)) + '\data.txt');
  Reset(f);
  readln(f,s1);
  readln(f,s2);
  Closefile(f);

  if length(s1)=12 then
  begin
   Year1 := strtoint(s1[1]+s1[2]+s1[3]+s1[4]);
   Month1 := strtoint(s1[5]+s1[6]);
   Day1 := strtoint(s1[7]+s1[8]);
   Hour1 := strtoint(s1[9]+s1[10]);
   Min1 := strtoint(s1[11]+s1[12]);
   LastMeet := EncodeDate(Year1,Month1,Day1);
   LastMeet := LastMeet + EncodeTime(Hour1,Min1,0,0);

   Label1.Caption := 'Eltelt idõ ' + IntToStr(Year1) + '.';
    if length(IntToStr(Month1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Month1) + '.'
    else
     Label1.Caption := Label1.Caption + IntToStr(Month1) + '.';
    if length(IntToStr(Day1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Day1) + '. '
    else
     Label1.Caption := Label1.Caption + IntToStr(Day1) + '. ';
    if length(IntToStr(Hour1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Hour1) + ':'
    else
     Label1.Caption := Label1.Caption + IntToStr(Hour1) + ':';
    if length(IntToStr(Min1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Min1) + ' óta:'
    else
     Label1.Caption := Label1.Caption + IntToStr(Min1) + ' óta:';
  end;

  if length(s2)=12 then
  begin
   Year2 := strtoint(s2[1]+s2[2]+s2[3]+s2[4]);
   Month2 := strtoint(s2[5]+s2[6]);
   Day2 := strtoint(s2[7]+s2[8]);
   Hour2 := strtoint(s2[9]+s2[10]);
   Min2 := strtoint(s2[11]+s2[12]);
   NextMeet := EncodeDate(Year2,Month2,Day2);
   NextMeet := NextMeet + EncodeTime(Hour2,Min2,0,0);

   Label3.Caption := 'Hátralevõ idõ ' + IntToStr(Year2) + '.';
   if length(IntToStr(Month2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Month2) + '.'
   else
    Label3.Caption := Label3.Caption + IntToStr(Month2) + '.';
   if length(IntToStr(Day2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Day2) + '. '
   else
    Label3.Caption := Label3.Caption + IntToStr(Day2) + '. ';
   if length(IntToStr(Hour2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Hour2) + ':'
   else
    Label3.Caption := Label3.Caption + IntToStr(Hour2) + ':';
   if length(IntToStr(Min2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Min2) + '-ig:'
   else
    Label3.Caption := Label3.Caption + IntToStr(Min2) + '-ig:';
  end;
 end;

 Form2.Talcaraules;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var TimeSince: TDateTime;
    TimeTill: TDateTime;
    SecsSince: longint;
    SecsTill: longint;
begin
 if not Form1.Visible then Abort;

 TimeSince := Now - LastMeet;
 TimeTill := NextMeet - Now;
 //TimeSince: LastMeet ota eltelt napok szama
 //szorozd meg ezzel (24*3600) es megkapod a masodperceket

 SecsSince := trunc(TimeSince*(24*3600));
 SecsTill := trunc(TimeTill*(24*3600));

 Label2.Caption := IntToStr(SecsSince) + ' másodperc';
 Label4.Caption := IntToStr(SecsTill) + ' másodperc';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 Form2.Tunesatalcarol;
end;

procedure TForm1.ReReadData;
var f: textfile;
    s1,s2:string;
    Year1,Month1,Day1,Hour1,Min1:word;
    Year2,Month2,Day2,Hour2,Min2:word;
begin
 LastMeet := EncodeDate(2003,9,21);            //elmeletileg LastMeet az utolso
 LastMeet := LastMeet + EncodeTime(15,40,0,0); //talalkozas datumat tartalmazza (persze idealizalva)

 NextMeet := EncodeDate(2003,9,26);
 NextMeet := NextMeet + EncodeTime(17,34,0,0);

 if FileExists(ExtractFilePath(ParamStr(0)) + '\data.txt') then
 begin
  AssignFile(f,ExtractFilePath(ParamStr(0)) + '\data.txt');
  Reset(f);
  readln(f,s1);
  readln(f,s2);
  Closefile(f);

  if length(s1)=12 then
  begin
   Year1 := strtoint(s1[1]+s1[2]+s1[3]+s1[4]);
   Month1 := strtoint(s1[5]+s1[6]);
   Day1 := strtoint(s1[7]+s1[8]);
   Hour1 := strtoint(s1[9]+s1[10]);
   Min1 := strtoint(s1[11]+s1[12]);
   LastMeet := EncodeDate(Year1,Month1,Day1);
   LastMeet := LastMeet + EncodeTime(Hour1,Min1,0,0);

   Label1.Caption := 'Eltelt idõ ' + IntToStr(Year1) + '.';
    if length(IntToStr(Month1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Month1) + '.'
    else
     Label1.Caption := Label1.Caption + IntToStr(Month1) + '.';
    if length(IntToStr(Day1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Day1) + '. '
    else
     Label1.Caption := Label1.Caption + IntToStr(Day1) + '. ';
    if length(IntToStr(Hour1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Hour1) + ':'
    else
     Label1.Caption := Label1.Caption + IntToStr(Hour1) + ':';
    if length(IntToStr(Min1))=1 then
     Label1.Caption := Label1.Caption + '0' + IntToStr(Min1) + ' óta:'
    else
     Label1.Caption := Label1.Caption + IntToStr(Min1) + ' óta:';
  end;

  if length(s2)=12 then
  begin
   Year2 := strtoint(s2[1]+s2[2]+s2[3]+s2[4]);
   Month2 := strtoint(s2[5]+s2[6]);
   Day2 := strtoint(s2[7]+s2[8]);
   Hour2 := strtoint(s2[9]+s2[10]);
   Min2 := strtoint(s2[11]+s2[12]);
   NextMeet := EncodeDate(Year2,Month2,Day2);
   NextMeet := NextMeet + EncodeTime(Hour2,Min2,0,0);

   Label3.Caption := 'Hátralevõ idõ ' + IntToStr(Year2) + '.';
   if length(IntToStr(Month2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Month2) + '.'
   else
    Label3.Caption := Label3.Caption + IntToStr(Month2) + '.';
   if length(IntToStr(Day2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Day2) + '. '
   else
    Label3.Caption := Label3.Caption + IntToStr(Day2) + '. ';
   if length(IntToStr(Hour2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Hour2) + ':'
   else
    Label3.Caption := Label3.Caption + IntToStr(Hour2) + ':';
   if length(IntToStr(Min2))=1 then
    Label3.Caption := Label3.Caption + '0' + IntToStr(Min2) + '-ig:'
   else
    Label3.Caption := Label3.Caption + IntToStr(Min2) + '-ig:';
  end;
 end;
end;

end.
