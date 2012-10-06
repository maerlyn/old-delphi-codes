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

unit Strdlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TStringDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    strings: TMemo;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCreate(Sender: TObject);
  end;

var
  StringDlg: TStringDlg;

implementation

{$R *.DFM}

procedure TStringDlg.FormCreate(Sender: TObject);
begin
  bordericons := [];
end;

end.


