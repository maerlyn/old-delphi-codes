unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure loadprg;
  public
    { Public declarations }
  end;

  adattip = record
    nev,
    tel: string[20];
    varos: string[15];
    irsz: string[4];
    cim,
    megj: string[40];
  end;
  telhdr = record
    azon:array[1..14] of char; //PWCDNyDataFile
    mennyi: byte;
    auto: boolean;
  end;
  indtip=array[1..100]of byte;

var
  Form1: TForm1;
  adat: array[1..100]of adattip;
  mennyi,
  rend:byte;
  auto,
  files:boolean;
  dosszie:string[8];

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var fejlec: telhdr;
    fileout:file;
begin
 if not savedialog1.execute then
  abort;
 system.assignfile(fileout,savedialog1.filename);
 {$I-}
 rewrite(fileout,1);
 {$I+}
 if ioresult <> 0 then
 begin
  showmessage('hiba a mentes soran');
  abort;
 end;
 fejlec.azon:='PWCDNyDataFile';
 fejlec.mennyi:=mennyi;
 fejlec.auto:=auto;
 blockwrite(fileout,fejlec,sizeof(fejlec));
 blockwrite(fileout,adat,sizeof(adattip)*mennyi);
 system.closefile(fileout);
end;

procedure TForm1.loadprg;
var header:telhdr;
    filein:file;
    dirtel:TSearchRec;
begin
 findfirst('*.tel',faanyfile,dirtel);
 if dirtel.Name <> '' then
 begin
  system.Assignfile(filein,dirtel.name);
  {$I-}
  reset(filein,1);
  {$I+}
  if ioresult=0 then
  begin
   blockread(filein,header,sizeof(header));
   if header.azon='PWCDNyDataFile' then
   begin
    dosszie := copy(dirtel.name,1,pos('.',dirtel.name)-1);
    mennyi := header.mennyi;
    auto := header.auto;
    blockread(filein,adat,sizeof(adattip)*mennyi);
   end;
   closefile(filein);
  end;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 loadprg;
end;

procedure TForm1.Button2Click(Sender: TObject);
var header:telhdr;
    filein:file;
begin
 if not opendialog1.execute then
  abort;
 assignfile(filein,opendialog1.filename);
 {$I-}
 reset(filein,1);
 {$I+}
 if ioresult=0 then
 begin
  blockread(filein,header,sizeof(header));
  if header.azon='PWCDNyDataFile' then
  begin
   dosszie:=copy(extractfilename(opendialog1.filename),1,pos('.',extractfilename(opendialog1.filename))-1);
   mennyi:=header.mennyi;
   auto:=header.auto;
   blockread(filein,adat,sizeof(adattip)*mennyi);
  end
  else
  begin
   showmessage('hiba a megnyitas soran');
   abort;
  end;
  closefile(filein);
 end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var i: integer;
const betuk:string='abcdefghijklmnopqrstuvwxyz';
begin
 for i := 1 to length(betuk) do
 begin
  adat[i].nev := 'nev-'+betuk[i];
  adat[i].varos := 'varos-'+betuk[i];
  adat[i].cim := 'cim-'+betuk[i];
  adat[i].irsz := 'ir-'+betuk[i];
  adat[i].tel := 'tel-'+betuk[i];
  adat[i].megj := 'megj-'+betuk[i];
  mennyi := i;
 end;
 showmessage('kesz');
end;

end.
