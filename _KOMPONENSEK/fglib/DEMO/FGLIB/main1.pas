unit main1;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Menus, ExtCtrls,
  Buttons, fglib;

type
  TForm1 = class(TForm)
    BmpMosaic2: TBmpMosaic;
    BmpTransparent5: TBmpTransparent;
    BmpTransparent6: TBmpTransparent;
    BmpTransparent7: TBmpTransparent;
    Panel4: TPanel;
    BmpAnime9: TBmpAnime;
    BmpAnime12: TBmpAnime;
    BmpAnime13: TBmpAnime;
    BmpAnime14: TBmpAnime;
    BmpAnime15: TBmpAnime;
    BmpAnime16: TBmpAnime;
    BmpAnime17: TBmpAnime;
    BmpAnime18: TBmpAnime;
    BmpAnime19: TBmpAnime;
    BmpAnime20: TBmpAnime;
    Pad3D2: TPad3D;
    NotePad1: TNotePad;
  private
    { Déclarations private }
    dir : string;
  public
    { Déclarations public }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
