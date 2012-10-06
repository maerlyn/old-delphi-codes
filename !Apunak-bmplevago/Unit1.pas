unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  options : TSelectDirOpts;
  chosenDirectory : string;
  searchResult : TSearchRec;
begin
  chosenDirectory := Label1.Caption;
  if not SelectDirectory(chosenDirectory, options, 0) then
   Abort;

  Label1.Caption := chosenDirectory;
  ListBox1.Items.Clear;

  if FindFirst(Label1.Caption + '\*.bmp', faAnyFile, searchResult) = 0 then
  begin
    repeat
      ListBox1.Items.Add(searchResult.Name);
    until FindNext(searchResult) <> 0;
    FindClose(searchResult);
  end;
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
 try
  StrToInt((Sender as TEdit).Text)
 except
   Application.MessageBox('Egész számot kérek','',mb_ok);
   (Sender as TEdit).SetFocus;
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var a,b: TBitmap;
    i: integer;
    k,j: integer;
begin
 a := TBitmap.Create;
 b := TBitmap.Create;

 for i := 0 to Listbox1.Items.Count-1 do
 begin
  a.LoadFromFile(Label1.Caption + '\' + ListBox1.Items[i]);
  if (a.Width - StrToInt(Edit3.Text) - StrToInt(Edit4.Text)) <= 0 then
   Application.MessageBox(PChar(ListBox1.Items[i] + ' file új szélessége <= 0!'),'',mb_ok)
  else
  if (a.Height - StrToInt(Edit1.Text) - StrToInt(Edit2.Text)) <= 0 then
   Application.MessageBox(PChar(ListBox1.Items[i] + 'file új magassága <= 0!'),'',mb_ok)
  else
  begin
   b.Width := a.Width - StrToInt(Edit3.Text) - StrToInt(Edit4.Text);
   b.Height := a.Height - StrToInt(Edit1.Text) - StrToInt(Edit2.Text);

   for k := 0 to b.Width do
    for j := 0 to b.Height do
     b.Canvas.Pixels[k,j] := a.Canvas.Pixels[StrToInt(Edit1.Text)+k,StrToInt(Edit3.Text)+j];

//   b.Canvas.CopyRect(Rect(1,1,b.Width,b.Height),
//                     a.Canvas,
//                     Rect(StrToInt(Edit3.Text),StrToInt(Edit1.Text),a.Width-StrToInt(Edit3.Text)-StrToInt(Edit4.Text),a.Height-StrToInt(Edit1.Text)-StrToInt(Edit2.Text)));

   b.PixelFormat := a.PixelFormat;
   b.

   b.SaveToFile(Label1.Caption + '\' + ListBox1.Items[i]);
  end;
 end;

 Application.MessageBox('Kész','',mb_ok);

 a.Free;
 b.Free;
end;

end.
