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
  Output := Output + trim(s) + ' ';
 end;

 closefile(f);

 while pos(#13,Output) > 0 do
  delete(Output,pos(#13,Output),1);
 while pos(#10,Output) > 0 do
  delete(Output,pos(#10,Output),1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if not OpenDialog1.Execute then
  Abort;

 EgySorbaRendez(OpenDialog1.FileName,szum);

 Feldolgozas;

 if not SaveDialog1.Execute then
  Abort;

 r.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.Feldolgozas;
var s: string;
begin
 r := TStringList.Create;
 while pos('<TD align=middle class=txt1 width="14%"><SPAN class=txt2b>',szum) > 0 do
 begin
  delete(szum,1,pos('<TD align=middle class=txt1 width="14%"><SPAN class=txt2b>',szum)+Length('<TD align=middle class=txt1 width="14%"><SPAN class=txt2b>')-1);
  s := '"' + Copy(szum,1,pos('<',szum)-1) + ' ';
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,4);
  s := s + copy(szum,1,2) + '",';
  delete(szum,1,length('00</SPAN><BR>'));
  s := s + '"' + Copy(szum,1,5) + '",';
  delete(szum,1,5);
  delete(szum,1,length(' </TD> <TD align=middle class=txt1 width="14%">'));
  delete(szum,1,length('<IMG alt='));
  s := s + '"' + copy(szum,1,pos('src=',szum)-2) + '",';
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR>&nbsp;</TD> <TD align=middle class=txt1 width="14%">'));
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR><SPAN class=txt1b>'));
  s := s + '"' + copy(szum,1,pos('<',szum)-1) + '",';
  delete(szum,1,pos('&nbsp;',szum)-1);
  delete(szum,1,length('&nbsp;°C</TD><TD align=middle class=txt1 noWrap width="14%"> '));
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR><SPAN class=txt1b>'));
  s := s + '"' + copy(szum,1,pos('<',szum)-1) + '",';
  delete(szum,1,2);
  delete(szum,1,length('</SPAN>&nbsp;km/h</TD> <TD align=middle class=txt1 width="14%">'));
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR><SPAN class=txt1b>'));
  s := s + '"' + copy(szum,1,pos('<',szum)-1) + '",';
  delete(szum,1,pos('<',szum)-1);
  delete(szum,1,length('</SPAN>&nbsp;°</TD> <TD align=middle class=txt1 noWral width="14%">'));
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR><SPAN class=txt1b>'));
  s := s + '"' + copy(szum,1,pos('<',szum)-1) + '",';
  delete(szum,1,length('</SPAN>&nbsp;hPa</TD><TD align=middle class=txt1 noWral width="14%">'));
  delete(szum,1,pos('>',szum));
  delete(szum,1,pos('>',szum));
  delete(szum,1,length('<BR><SPAN class=txt1b>'));
  s := s + '"' + copy(szum,1,pos('<',szum)-1) + '"';
  delete(szum,1,length('</SPAN>&nbsp;%</TD></TR>'));

  r.add(s);
 end;

{<TR class=tdzold>
  <TD align=middle class=txt1 width="14%">
   <SPAN class=txt2b>november<BR>15</SPAN>
   <BR>06:00
  </TD>
  <TD align=middle class=txt1 width="14%">
   <IMG alt=borult src="magyarorszag_mertadatok_elemei/tn_borult.gif" width=50>
   <BR>&nbsp;
  </TD>
  <TD align=middle class=txt1 width="14%">
   <IMG align=top alt=Homerseklet src="magyarorszag_mertadatok_elemei/ico_ho.gif" width=50>
   <BR><SPAN class=txt1b>13</SPAN>&nbsp;°C
  </TD>
  <TD align=middle class=txt1 noWrap width="14%">
   <IMG align=center alt=szélerõ src="magyarorszag_mertadatok_elemei/ico_szelero.gif" title=szélerõ width=50>
   <BR><SPAN class=txt1b>32</SPAN>&nbsp;km/h
  </TD>
  <TD align=middle class=txt1 width="14%">
   <IMG alt=déli border=0 src="magyarorszag_mertadatok_elemei/nyil_180.gif" width=50>
   <BR><SPAN class=txt1b>170</SPAN>&nbsp;°
  </TD>
  <TD align=middle class=txt1 noWrap width="14%"><IMG align=center alt=Legnyomas src="magyarorszag_mertadatok_elemei/ico_legnyomas.gif" title=Legnyomas width=50>
   <BR><SPAN class=txt1b>1008</SPAN>&nbsp;hPa
  </TD>
  <TD align=middle class=txt1 noWrap width="14%">
   <IMG align=center alt="Relativ nedvesseg" src="magyarorszag_mertadatok_elemei/ico_relnedv.jpg" title="Relativ nedvesseg" width=50>
   <BR><SPAN class=txt1b>76</SPAN>&nbsp;%
  </TD>
 </TR>}

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
 SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
end;

end.
