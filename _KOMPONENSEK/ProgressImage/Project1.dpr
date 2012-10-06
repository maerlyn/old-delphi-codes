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

program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
