//****************************************************//
//                                                    //
//             Delphi Software Online                 //
//         internetes hetilap példaprogram            //
//          http://www.SoftwareOnline.hu              //
//                                                    //
//             Animare Software © 2003                //
//              http://www.animare.hu                 //
//                                                    //
//****************************************************//

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ProgressImage, jpeg;

type
  TForm1 = class(TForm)
    ProgressImage1: TProgressImage;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  ProgressImage1.Position:=TrackBar1.Position;
end;

end.
