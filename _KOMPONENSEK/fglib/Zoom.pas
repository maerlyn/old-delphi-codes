{------------------------------------------------------------------------------}
{ Copyright 2001 by EuroSoft Software Development                              }
{ designl@worldnet.net                                                         }
{                                                                              }
{ This software is provided 'as-is', without any express or implied warranty.  }
{ In no event will the author be held liable for any  damages arising from     }
{ the use of this software.                                                    }
{                                                                              }
{ No part of this Unit may be copied in any way without a written permission.  }
{------------------------------------------------------------------------------}

unit zoom;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

type
  TRealSize = class(TForm)
    Image: TImage;
    procedure FormCreate(Sender: TObject);
  public
    fichier : string;
    pointeur : integer;
  end;

var
  RealSize: TRealSize;

implementation

{$R *.DFM}

procedure TRealSize.FormCreate(Sender: TObject);
begin
  bordericons := [bisystemmenu,biMaximize];
end;

end.
