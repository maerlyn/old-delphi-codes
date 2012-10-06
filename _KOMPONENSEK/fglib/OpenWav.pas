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

unit Openwav;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, FileCtrl, ExtCtrls,
  Typefglib;

type
  TOpenWavDlg = class(TForm)
    CheckBox2: TCheckBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Filter: TFilterComboBox;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    PlayBtn: TBitBtn;
    procedure FileListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PlayBtnClick(Sender: TObject);
  public
    FileName: TFileName;
    InitialDir: String;
    Title: String;
    Language : TLanguage;
  end;


var
  OpenWavDlg : TOpenWavDlg;

implementation

{$R *.DFM}

procedure TOpenWavDlg.FormCreate(Sender: TObject);
begin
  bordericons := [];
  FileName := '';
  InitialDir := '';
  Title := '';
  Language := laDefault;
end;

procedure TOpenWavDlg.FormShow(Sender: TObject);
begin
  caption := Title;
  if Language=laDefault then Language := GetCurrentLanguage;
  if caption='' then
  begin
    case Language of
      laFrench :
      begin
        caption := 'Ouverture (Wave Sound)';
        CancelBtn.Caption := '&Annuler';
      end;
      laEnglish :
      begin
         caption := 'Open (Wave Sound)';
         CancelBtn.Caption := '&Cancel';
      end;
      laGerman :
      begin
         caption := 'Öffnung (Wave Sound)';
         CancelBtn.Caption := '&Beende';
      end;
      laItalian :
      begin
         caption := 'Apertura (Wave Sound)';
         CancelBtn.Caption := '&Annullare';
      end;
      laSpanish :
      begin
         caption := 'Abertura (Wave Sound)';
         CancelBtn.Caption := '&Cancelar';
      end;
    end;
  end;
  DirectoryListBox1.Directory:=InitialDir;
  if FileExists(filename) then PlayBtn.enabled := true else PlayBtn.enabled := false;
end;

procedure TOpenWavDlg.FileListBox1Change(Sender: TObject);
begin
  if ((FileListBox1.items.count>0) and (FileListBox1.itemindex<0)) then FileListBox1.itemindex := 0;
  filename := FileListBox1.FileName;
  FileListBox1.setfocus;
  if FileExists(filename) then PlayBtn.enabled := true else PlayBtn.enabled := false;
end;

procedure TOpenWavDlg.PlayBtnClick(Sender: TObject);
begin
  PlayWaveFile(filename,1);
end;
end.
