unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Grids;

type
  TForm1 = class(TForm)
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var tsl: TStringList;
    s: string;
    i,k: integer;
    width: integer;
    height: integer;
    tbmp: TBitmap;
begin
 OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
 if not OpenDialog1.Execute then
  Application.Terminate;

 tsl := TStringList.Create;
 tsl.LoadFromFile(OpenDialog1.FileName);

 width := 1;
 for i := 0 to tsl.Count-1 do
 begin
  width := 1;
  s := tsl[i];
  while pos(',',s) > 0 do
  begin
   StringGrid1.Cells[width,i+1] := copy(s,1,pos(',',s)-1);
   inc(width);
   delete(s,1,pos(',',s));
  end;
  StringGrid1.Cells[width,i+1] := s;
 end;

 width := width;
 height := tsl.Count;
// StringGrid1.ColCount := width;
// StringGrid1.RowCount := height;

 for i := 1 to StringGrid1.ColCount do
  if (StringGrid1.Cells[i,1] <> '')and(s <> '') then
  begin
   s := StringGrid1.Cells[i,1];
  end;

 tbmp := TBitmap.Create;
 tbmp.LoadFromFile(ExtractFilePath(OpenDialog1.FileName) + s + '.bmp');

 Image1.Width := (tbmp.Width * width) - 1;
 Image1.Height := (tbmp.Height * height) - 1;

 for i := 1 to width do
  for k := 1 to height do
   if StringGrid1.Cells[i,k] <> '' then
   begin
    tbmp.LoadFromFile(ExtractFilePath(OpenDialog1.FileName) + StringGrid1.Cells[i,k] + '.bmp');
    Image1.Canvas.Draw((i-1)*tbmp.Width,(k-1)*tbmp.Height-1,tbmp);
   end;

 Self.Show;
 if SaveDialog1.Execute then
 begin
  s := SaveDialog1.FileName;
  if ExtractFileExt(s) <> '.bmp' then
   s := s + '.bmp';
  Image1.Picture.Bitmap.SaveToFile(s);
 end;
end;

end.
