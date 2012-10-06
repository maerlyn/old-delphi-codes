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

unit Openbmp;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FileCtrl, ExtCtrls,
  Typefglib, Zoom;

type
  TOpenBmpDlg = class(TForm)
    CheckBox2: TCheckBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Filter: TFilterComboBox;
    FileListBox1: TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    OKBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CheckBox1: TCheckBox;
    ImageRect: TGroupBox;
    Image: TImage;
    ZoomBtn: TBitBtn;
    procedure FileListBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomBtnClick(Sender: TObject);
    procedure ShowImage;
  private
    RealSize : TRealSize;
  public
    FileName: TFileName;
    InitialDir: String;
    Title: String;
    Language : TLanguage;
  end;


var
  OpenBmpDlg : TOpenBmpDlg;

implementation


{$R *.DFM}

procedure TOpenBmpDlg.FormCreate(Sender: TObject);
begin
  bordericons := [];
  FileName := '';
  InitialDir := '';
  Title := '';
  Language := laDefault;
  RealSize := TRealSize.create(application);
end;

procedure TOpenBmpDlg.FormShow(Sender: TObject);
begin
  caption := Title;
  if Language=laDefault then Language := GetCurrentLanguage;
  case Language of
    laFrench :
    begin
      if caption='' then caption := 'Ouverture (Bitmap)';
      CheckBox1.caption := ' Voir ';
      CheckBox1.width := 45;
      filter.filter := 'Toutes les images|*.bmp;*.ico;*.wmf'+'|bmp|*.bmp'+'|ico|*.ico'+'|wmf|*.wmf';
      CancelBtn.Caption := '&Annuler';
    end;
    laEnglish :
    begin
      if caption='' then caption := 'Open (Bitmap)';
      CheckBox1.caption := ' View ';
      CheckBox1.width := 55;
      filter.filter := 'All Images|*.bmp;*.ico;*.wmf'+'|bmp|*.bmp'+'|ico|*.ico'+'|wmf|*.wmf';
      CancelBtn.Caption := '&Cancel';
    end;
    laGerman :
    begin
      if caption='' then caption := 'Öffnung (Bitmap)';
      CheckBox1.caption := ' View ';
      CheckBox1.width := 55;
      filter.filter := 'Alle Blider|*.bmp;*.ico;*.wmf'+'|bmp|*.bmp'+'|ico|*.ico'+'|wmf|*.wmf';
      CancelBtn.Caption := '&Beende';
    end;
    laItalian :
    begin
      if caption='' then caption := 'Apertura (Bitmap)';
      CheckBox1.caption := ' Vedere ';
      CheckBox1.width := 70;
      filter.filter := 'immagine|*.bmp;*.ico;*.wmf'+'|bmp|*.bmp'+'|ico|*.ico'+'|wmf|*.wmf';
      CancelBtn.Caption := '&Annullare';
    end;
    laSpanish :
    begin
      if caption='' then caption := 'Abertura (Bitmap)';
      CheckBox1.caption := ' Ver ';
      CheckBox1.width := 45;
      filter.filter := 'imagen|*.bmp;*.ico;*.wmf'+'|bmp|*.bmp'+'|ico|*.ico'+'|wmf|*.wmf';
      CancelBtn.Caption := '&Cancelar';
    end;
  end;
  DirectoryListBox1.Directory:=InitialDir;
  CheckBox1.Checked := true;
  ShowImage;
end;

procedure TOpenBmpDlg.FileListBox1Change(Sender: TObject);
begin
  if ((FileListBox1.items.count>0) and (FileListBox1.itemindex<0)) then FileListBox1.itemindex := 0;
  filename := FileListBox1.FileName;
  FileListBox1.setfocus;
  ShowImage;
end;

procedure TOpenBmpDlg.CheckBox1Click(Sender: TObject);
begin
  ShowImage;
end;

procedure TOpenBmpDlg.ZoomBtnClick(Sender: TObject);
begin
  RealSize.Image.picture.loadfromfile(fileName);
  RealSize.caption := 'Zoom - '+filename;
  RealSize.show;
end;

procedure TOpenBmpDlg.ShowImage;
var
  Dest : Trect;
function StretchRatio(w,h,rw,rh:integer) : TRect;
var
  Width,Height : integer;
  Dest : TRect;
begin
  if ((w>rw) or (h>rh)) then
  begin
    if w>h then
    begin
      Width  := rw;
      repeat
        Width := Width-1;
        Height := trunc(Width*(h/w));
      until (Height<=rh);
    end else
    begin
      Height := rh;
      repeat
        Height := Height-1;
        Width := trunc(Height*(w/h));
      until (Width<=rw);
    end;
  end else
  begin
    Width := w;
    Height := h;
  end;
  result := Rect(trunc((rw-Width)/2),trunc((rh-Height)/2),Width,Height);
end;
begin
  if (CheckBox1.Checked and FileExists(filename)) then
  begin
    image.picture.loadfromfile(fileName);
    dest := StretchRatio(image.picture.width,image.picture.height,ImageRect.width-10,ImageRect.height-30);
    image.left   := dest.Left+5;
    image.top    := dest.Top+20;
    image.width  := dest.Right;
    image.height := dest.Bottom;
    if ((image.picture.width=dest.Right) and (image.picture.height=dest.Bottom)) then ZoomBtn.enabled := false else
    ZoomBtn.enabled := true;
  end else
  begin
    image.picture := nil;
    ZoomBtn.enabled := false;
  end;
end;

end.
