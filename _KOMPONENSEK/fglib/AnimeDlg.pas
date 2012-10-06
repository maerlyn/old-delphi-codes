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

unit Animedlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, Grids,
  Typefglib, FgLib;

type
  TImageListDialog = class(TForm)
    Panel2: TPanel;
    Panel5: TPanel;
    BitBtn14: TBitBtn;
    GroupBox5: TGroupBox;
    Grid: TDrawGrid;
    AddBtn: TBitBtn;
    DelBtn: TBitBtn;
    UpBtn: TBitBtn;
    DownBtn: TBitBtn;
    Panel1: TPanel;
    OpenDialog1: TOpenDialog;
    ImageRect: TGroupBox;
    Image: TImage;
    Timer: TTimer;
    animation: TCheckBox;
    procedure okClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; Col, Row: Longint; Rect: TRect;
      State: TGridDrawState);
    procedure Header1Sized(Sender: TObject; ASection, AWidth: Integer);
    procedure AddBtnClick(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure UpBtnClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure animationClick(Sender: TObject);
  private
    ImageList: TStrings;
    AnimeCount : Integer;
    procedure ShowImage(index : integer);
    procedure CheckBtn;
  public
    Modified: Boolean;  
    procedure SetPageList(Value: TStrings);
  end;

var
  ImageListDialog: TImageListDialog;

implementation

{$R *.DFM}

procedure TImageListDialog.SetPageList(Value: TStrings);
begin
  ImageList := Value;
  Grid.RowCount := ImageList.Count;
end;

procedure TImageListDialog.FormCreate(Sender: TObject);
begin
  bordericons := [];
  Grid.DefaultColWidth := Grid.ClientWidth;
  AnimeCount := 0;
  timer.enabled := true;
end;

procedure TImageListDialog.GridDrawCell(Sender: TObject; Col, Row: Longint;
  Rect: TRect; State: TGridDrawState);
var
  R: TRect;
  C: array[0..255] of Char;
  W: TImageCtrl;
begin
  if Row < ImageList.Count then
  begin
    R := Rect;
    W := ImageList.Objects[Row] as TImageCtrl;
    Rect.Right := Grid.width;
    ExtTextOut(Grid.Canvas.Handle, Rect.Left + 2, Rect.Top + 2, ETO_CLIPPED,
      @Rect, StrPCopy(C,w.caption),
      Length(w.caption), nil);
  end;
end;

procedure TImageListDialog.FormShow(Sender: TObject);
begin
  if ImageList.Count-1>0 then
  begin
    Grid.Row := 0;
    ShowImage(0);
  end;
end;

procedure TImageListDialog.okClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TImageListDialog.ShowImage(index : integer);
var  
  Dest : Trect;
  W: TImageCtrl;
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
  if ImageList.count>0 then
  begin
  W := ImageList.Objects[index] as TImageCtrl;
  image.Picture.bitmap.assign(w.Bmp);
  dest := StretchRatio(image.picture.width,image.picture.height,ImageRect.width-10,ImageRect.height-30);
  image.left   := dest.Left+5;
  image.top    := dest.Top+20;
  image.width  := dest.Right;
  image.height := dest.Bottom;
  if ((image.picture.width<>image.width) or (image.picture.height<>image.height)) then
    image.stretch := true else image.stretch := false;
  end;
end;

procedure TImageListDialog.Header1Sized(Sender: TObject; ASection,
  AWidth: Integer);
begin
  Grid.Repaint;
end;

procedure TImageListDialog.AddBtnClick(Sender: TObject);
var
  W: TImageCtrl;
begin
  if OpenDialog1.execute then
  begin
    ImageList.Add(ExtractFileName(OpenDialog1.filename));       
    Grid.RowCount := ImageList.Count;
    W := ImageList.Objects[Grid.RowCount-1] as TImageCtrl;
    w.Bmp.loadfromfile(OpenDialog1.filename);
    Grid.DefaultColWidth := Grid.ClientWidth;
    Grid.Invalidate;
    Modified := True;
    Grid.Row := Grid.RowCount-1;
    ShowImage(Grid.Row);
    CheckBtn;
  end;
end;

procedure TImageListDialog.DelBtnClick(Sender: TObject);
begin
  if MessageDlg('Delete this image', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    timer.enabled := false;
    ImageList.Delete(Grid.Row);
    Grid.RowCount := ImageList.Count;
    Grid.DefaultColWidth := Grid.ClientWidth;
    Grid.Invalidate;
    Modified := True;
    if ImageList.Count>0 then ShowImage(Grid.Row);
    CheckBtn;
    timer.enabled := animation.checked;
  end;
end;

procedure TImageListDialog.UpBtnClick(Sender: TObject);
var
  NewIndex: Integer;
begin
  NewIndex := Grid.Row;
  if Sender = UpBtn then Dec(NewIndex)
  else Inc(NewIndex);
  if (NewIndex >= 0) and (NewIndex < ImageList.Count) then
  begin
    ImageList.Move(Grid.Row, NewIndex);
    Grid.Row := NewIndex;
    Grid.Invalidate;
  end;
  Grid.SetFocus;
  Modified := True;
end;

procedure TImageListDialog.GridClick(Sender: TObject);
begin
  if ImageList.Count>0 then
  begin
    Grid.Invalidate;
    ShowImage(Grid.Row);
  end;
end;

procedure TImageListDialog.CheckBtn;
begin           
  if ImageList.Count>0 then
  begin
    DelBtn.enabled := true;
    UpBtn.enabled := true;
    DownBtn.enabled := true;
  end else
  begin
    DelBtn.enabled := false;
    UpBtn.enabled := false;
    DownBtn.enabled := false;
  end;
end;

procedure TImageListDialog.TimerTimer(Sender: TObject);
begin
  if AnimeCount<ImageList.Count-1 then
  begin 
    ShowImage(AnimeCount);
    inc(AnimeCount);
  end else AnimeCount := 0;
end;

procedure TImageListDialog.animationClick(Sender: TObject);
begin
  timer.enabled := animation.checked;
end;

end.
