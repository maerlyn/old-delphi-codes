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

unit Curdlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Typefglib;

type
  TCursorsDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox1: TGroupBox;
    ListBox1: TListBox;
    GroupBox2: TGroupBox;
    TestPad: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  public
    NewCursor : TCursor;
    Title: String;
    Language : TLanguage;
  end;

var
  CursorsDlg: TCursorsDlg;

implementation

{$R *.DFM}

procedure TCursorsDlg.FormCreate(Sender: TObject);
begin
  bordericons := [];
  NewCursor := crDefault;
  Title := '';
  Language := laDefault;
end;

procedure TCursorsDlg.FormShow(Sender: TObject);
var
  i,Current : integer;
begin
  listbox1.clear;
  for I := Low(Cursors) to High(Cursors) do
  begin
    listbox1.items.add(StrPas(Cursors[I].Name));
    if NewCursor=Cursors[I].Value then Current := I;
  end;
  listbox1.itemindex := Current;
  TestPad.cursor := NewCursor;
end;

procedure TCursorsDlg.ListBox1Click(Sender: TObject);
begin
  TestPad.cursor := Cursors[listbox1.itemindex].value;
end;

procedure TCursorsDlg.BitBtn1Click(Sender: TObject);
begin
  NewCursor := TestPad.cursor;
  ModalResult := mrOk;
end;

procedure TCursorsDlg.BitBtn2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
