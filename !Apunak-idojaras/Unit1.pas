unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    procedure EgySorbaRendez(const Filename: string; var Output: string);
    procedure Button1Click(Sender: TObject);
    procedure Feldolgozas;
    procedure FormCreate(Sender: TObject);
    procedure EliminateCommas(var aaaa: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  szum: string;
  result: string;
  r: TStringList;

implementation

{$R *.DFM}

{ TForm1 }

procedure TForm1.EgySorbaRendez(const Filename: string; var Output: string);
var f: TextFile;
    s: string;
begin
 Output := '';

 assignfile(f,Filename);
 reset(f);
 while not eof(f) do
 begin
  Readln(f,s);
  Output := Output + trim(s);// + ' ';
 end;

 closefile(f);

 while pos(#13,Output) > 0 do
  delete(Output,pos(#13,Output),1);
 while pos(#10,Output) > 0 do
  delete(Output,pos(#10,Output),1);
end;

procedure TForm1.Button1Click(Sender: TObject);
var s: string;
begin
 if not OpenDialog1.Execute then
  Abort;

 EgySorbaRendez(OpenDialog1.FileName,szum);

 Feldolgozas;

 s := copy(OpenDialog1.FileName,1,length(OpenDialog1.FileName)-3) + 'csv';

 r.SaveToFile(s);
// if not SaveDialog1.Execute then
//  Abort;

// r.SaveToFile(SaveDialog1.FileName);

 Application.MessageBox('kész','',mb_OK);
end;

procedure TForm1.Feldolgozas;
var s, temp: string;
begin
 r := TStringList.Create;
 while pos('<td align=center class=txt1 width=14% ><span class=txt2b>',szum) > 0 do
 begin
//<td align=center class=txt1 width=14% > <span class=txt2b>
  delete(szum,1,pos('<td align=center class=txt1 width=14% ><span class=txt2b>',szum)+Length('<td align=center class=txt1 width=14% ><span class=txt2b>')-1);
  temp := copy(szum,1,pos('<',szum)-1);
  EliminateCommas(temp);
  s := temp + ' '; //hónap
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,pos('>',szum));
  temp := copy(szum,1,2);
  EliminateCommas(temp);
  s := s + temp + ' '; //nap
  delete(szum,1,pos('r>',szum)+1); //<br>
  temp := copy(szum,1,5);
  EliminateCommas(temp);
  s := s + temp + ';'; //óra:perc
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,pos('<td',szum)-1);
  delete(szum,1,pos('% >',szum)+2);
  if szum[1] = '-' then
  begin
//   delete(szum,1,pos('%">',szum)+2);
   delete(szum,1,pos('% >',szum)+2);
   s := s + '-' + ';'; //idõjárási esemény
  end
  else
  begin
   delete(szum,1,pos('alt=',szum)+4);
   temp := copy(szum,1,pos('''',szum)-1);
   EliminateCommas(temp);
   s := s + temp + ';';
  end;
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('<td',szum)-1);
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('1b>',szum)+2);
  temp := copy(szum,1,pos('<',szum)-1);
  EliminateCommas(temp);
  s := s + temp + ';'; //hõmérséklet
  delete(szum,1,pos('&nbsp;',szum)-1);
  delete(szum,1,pos('<td',szum)-1);
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  temp := copy(szum,1,pos('<',szum)-1);
  EliminateCommas(temp);
  s := s + temp + ';'; //szélerõ
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,pos('km/h',szum)+3);
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  temp := copy(szum,1,pos('<',szum)-1);
  EliminateCommas(temp);
  s := s + temp + ';'; //szélirány
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,pos('<td',szum)-1);
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('1b>',szum)+2);
  temp := copy(szum,1,pos('<',szum)-1);
  EliminateCommas(temp);
  s := s + temp + ';'; //légnyomás
  delete(szum,1,pos('hPa',szum)+2);
  delete(szum,1,pos('>',szum)); //</TD>
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  temp := copy(szum,1,pos('<',szum)-1);
  s := s + temp; //páratartalom
  delete(szum,1,pos('R>',szum)+1);

  r.add(s);
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
 SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
end;

procedure TForm1.EliminateCommas(var aaaa: string);
begin
 while pos('"',aaaa) > 0 do
  delete(aaaa,pos('"',aaaa),1);
 if pos(';',aaaa) > 0 then
  aaaa := '"' + aaaa + '"';
 if length(aaaa) > 0 then
  if (aaaa[1] in ['0','1','2','3','4','5','6','7','8','9']) then
   while pos(' ',aaaa) > 0 do
    delete(aaaa,pos(' ',aaaa),1);
end;

end.
