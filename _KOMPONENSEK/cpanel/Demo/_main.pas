// **************************************************************
// *** Demo for TCirclePanel Version 3.1 for Delphi 5.0 (4.0) ***
// *** Copyright (c) 1999-2000                                ***
// *** coded by Mikhail Moshkevitch - All rights reserved.    ***
// *** Status: Freeware                                       ***
// **************************************************************
// Contact
//
// eMail: m_2013@hotmail.com
//
// **************************************************************
// I'm looking for a software designer/analyst job.
// **************************************************************

unit _main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DrawComboBox, StdCtrls, ExtCtrls, CirclePanel, ComCtrls, ExtEdit, Buttons,
  ImgList, Menus;

type
  TForm1 = class(TForm)
    CirclePanel1: TCirclePanel;
    CirclePanel100: TCirclePanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    CirclePanel5: TCirclePanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    CheckBox5: TCheckBox;
    ComboBox4: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label6: TLabel;
    Label7: TLabel;
    IntEdit1: TIntEdit;
    ColorDialog1: TColorDialog;
    CirclePanel2: TCirclePanel;
    CirclePanel3: TCirclePanel;
    CirclePanel4: TCirclePanel;
    Button1: TButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ImageList1: TImageList;
    Label11: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure IntEdit1Change(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CirclePanel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure CirclePanel3ImageMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CirclePanel4MouseEnter(Sender: TObject);
    procedure CirclePanel4MouseLeave(Sender: TObject);
  private
    FOnly:boolean;
  public
    procedure ShowOnlyCirclePanels;
  end;

var
  Form1: TForm1;


//=========================================================
implementation
{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin //Create
  FOnly:=false;
  ComboBox1.ItemIndex:=0;
  ComboBox2.ItemIndex:=1;
  ComboBox4.ItemIndex:=0;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  CirclePanel1.CornersType:=TCornersType(ComboBox2.ItemIndex);
  CirclePanel2.CornersType:=CirclePanel1.CornersType;
  CirclePanel3.CornersType:=CirclePanel1.CornersType;
  CirclePanel4.CornersType:=CirclePanel1.CornersType;
  if FOnly then ShowOnlyCirclePanels;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0:begin
        CirclePanel1.BevelOuter:=bvRaised;
        CirclePanel1.BevelInner:=bvNone;
      end;
    1:begin
        CirclePanel1.BevelOuter:=bvLowered;
        CirclePanel1.BevelInner:=bvNone;
      end;
    2:begin
        CirclePanel1.BevelOuter:=bvRaised;
        CirclePanel1.BevelInner:=bvLowered;
      end;
    3:begin
        CirclePanel1.BevelOuter:=bvLowered;
        CirclePanel1.BevelInner:=bvRaised;
      end;
  end;
  CirclePanel2.BevelOuter:=CirclePanel1.BevelOuter;
  CirclePanel2.BevelInner:=CirclePanel1.BevelInner;
  CirclePanel3.BevelOuter:=CirclePanel1.BevelOuter;
  CirclePanel3.BevelInner:=CirclePanel1.BevelInner;
  CirclePanel4.BevelOuter:=CirclePanel1.BevelOuter;
  CirclePanel4.BevelInner:=CirclePanel1.BevelInner;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  CirclePanel1.Corners.TopLeft.Enabled:=CheckBox1.Checked;
  CirclePanel2.Corners.TopLeft.Enabled:=CheckBox1.Checked;
  CirclePanel3.Corners.TopLeft.Enabled:=CheckBox1.Checked;
  CirclePanel4.Corners.TopLeft.Enabled:=CheckBox1.Checked;
  if FOnly then ShowOnlyCirclePanels;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  CirclePanel1.Corners.TopRight.Enabled:=CheckBox2.Checked;
  CirclePanel2.Corners.TopRight.Enabled:=CheckBox2.Checked;
  CirclePanel3.Corners.TopRight.Enabled:=CheckBox2.Checked;
  CirclePanel4.Corners.TopRight.Enabled:=CheckBox2.Checked;
  if FOnly then ShowOnlyCirclePanels;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  CirclePanel1.Corners.BottomLeft.Enabled:=CheckBox3.Checked;
  CirclePanel2.Corners.BottomLeft.Enabled:=CheckBox3.Checked;
  CirclePanel3.Corners.BottomLeft.Enabled:=CheckBox3.Checked;
  CirclePanel4.Corners.BottomLeft.Enabled:=CheckBox3.Checked;
  if FOnly then ShowOnlyCirclePanels;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  CirclePanel1.Corners.BottomRight.Enabled:=CheckBox4.Checked;
  CirclePanel2.Corners.BottomRight.Enabled:=CheckBox4.Checked;
  CirclePanel3.Corners.BottomRight.Enabled:=CheckBox4.Checked;
  CirclePanel4.Corners.BottomRight.Enabled:=CheckBox4.Checked;
  if FOnly then ShowOnlyCirclePanels;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  CirclePanel1.Gradient.Style:=TCircleGradientStyle(ComboBox4.ItemIndex);
  CirclePanel2.Gradient.Style:=CirclePanel1.Gradient.Style;
  CirclePanel3.Gradient.Style:=CirclePanel1.Gradient.Style;
  CirclePanel4.Gradient.Style:=CirclePanel1.Gradient.Style;
end;

procedure TForm1.IntEdit1Change(Sender: TObject);
begin
  CirclePanel1.Gradient.Size:=IntEdit1.Value;
  CirclePanel2.Gradient.Size:=CirclePanel1.Gradient.Size;
  CirclePanel3.Gradient.Size:=CirclePanel1.Gradient.Size;
  CirclePanel4.Gradient.Size:=CirclePanel1.Gradient.Size;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  CirclePanel1.Gradient.Enabled:=CheckBox5.Checked;
  CirclePanel2.Gradient.Enabled:=CirclePanel1.Gradient.Enabled;
  CirclePanel3.Gradient.Enabled:=CirclePanel1.Gradient.Enabled;
  CirclePanel4.Gradient.Enabled:=CirclePanel1.Gradient.Enabled;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  try
    ColorDialog1.Color:=CirclePanel1.Gradient.ColorStart;
    if ColorDialog1.Execute then
    begin
      CirclePanel1.Gradient.ColorStart:=ColorDialog1.Color;
      CirclePanel2.Gradient.ColorStart:=CirclePanel1.Gradient.ColorStart;
      CirclePanel3.Gradient.ColorStart:=CirclePanel1.Gradient.ColorStart;
      CirclePanel4.Gradient.ColorStart:=CirclePanel1.Gradient.ColorStart;
    end;
  except
    ShowMessage('Color selection dialog failed to load');
  end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  try
    ColorDialog1.Color:=CirclePanel1.Gradient.ColorEnd;
    if ColorDialog1.Execute then
    begin
      CirclePanel1.Gradient.ColorEnd:=ColorDialog1.Color;
      CirclePanel2.Gradient.ColorEnd:=CirclePanel1.Gradient.ColorEnd;
      CirclePanel3.Gradient.ColorEnd:=CirclePanel1.Gradient.ColorEnd;
      CirclePanel4.Gradient.ColorEnd:=CirclePanel1.Gradient.ColorEnd;
    end;
  except
    ShowMessage('Color selection dialog failed to load');
  end;
end;

procedure TForm1.ShowOnlyCirclePanels;
var
  i,x,y:integer;
  FRegion,h1:HRGN;
begin
  if GetWindowRgn(self.Handle,FRegion)<>ERROR then DeleteObject(FRegion);
  if FOnly then
  begin
    // if BorderStyle=bsSizeable then ...
    // x:=GetSystemMetrics(SM_CXSIZEFRAME);
    // y:=GetSystemMetrics(SM_CYSIZEFRAME)+GetSystemMetrics(SM_CYCAPTION);
    x:=GetSystemMetrics(SM_CXDLGFRAME); //if BorderStyle=bsDialog...
    y:=GetSystemMetrics(SM_CYDLGFRAME)+GetSystemMetrics(SM_CYCAPTION);
    FRegion:=CreateRectRgnIndirect(Bounds(0,0,0,0));
    for i:=0 to ComponentCount-1 do
    if Components[i] is TCirclePanel then
    with Components[i] as TCirclePanel do
    begin
      h1:=CreateRectRgnIndirect(Bounds(0,0,0,0));
      GetRegion(h1);  // outside region for TCirclePanel
      OffsetRgn(h1, x+Left, y+Top);
      CombineRgn(FRegion,FRegion,h1,RGN_OR);
      DeleteObject(h1);
    end;
  end
  else
    FRegion:=CreateRectRgnIndirect(Bounds(0,0,Width,Height));
  SetWindowRgn(self.Handle,FRegion,true);
end;

procedure TForm1.CirclePanel2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not FOnly then exit;
  if Button = mbLeft then
  begin
    ReleaseCapture;
    self.Perform(WM_SYSCOMMAND,$F012,1);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FOnly:=not FOnly;
  ShowOnlyCirclePanels;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  if not Active then exit;
  try
    ColorDialog1.Color:=CirclePanel1.BorderTopColor;
    if ColorDialog1.Execute then
    begin
      CirclePanel1.BorderTopColor:=ColorDialog1.Color;
      CirclePanel2.BorderTopColor:=CirclePanel1.BorderTopColor;
      CirclePanel3.BorderTopColor:=CirclePanel1.BorderTopColor;
      CirclePanel4.BorderTopColor:=CirclePanel1.BorderTopColor;
    end;
  except
    ShowMessage('Color selection dialog failed to load');
  end;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  if not Active then exit;
  try
    ColorDialog1.Color:=CirclePanel1.BorderBottomColor;
    if ColorDialog1.Execute then
    begin
      CirclePanel1.BorderBottomColor:=ColorDialog1.Color;
      CirclePanel2.BorderBottomColor:=CirclePanel1.BorderBottomColor;
      CirclePanel3.BorderBottomColor:=CirclePanel1.BorderBottomColor;
      CirclePanel4.BorderBottomColor:=CirclePanel1.BorderBottomColor;
    end;
  except
    ShowMessage('Color selection dialog failed to load');
  end;
end;

procedure TForm1.CirclePanel3ImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i:integer;
begin // or OnImageClick
  with Sender as TCirclePanel do
  begin
    i:=Image.ImageIndex;
    Inc(i);
    if i=ImageList1.Count then i:=0;
    Image.ImageIndex:=i;
  end;
end;

procedure TForm1.CirclePanel4MouseEnter(Sender: TObject);
begin
  CirclePanel4.Image.Enabled:=true;
  //or CirclePanel4.Gradient.Enabled:=true;
  //or any manipulations with colors
end;

procedure TForm1.CirclePanel4MouseLeave(Sender: TObject);
begin
  CirclePanel4.Image.Enabled:=false;
  //or CirclePanel4.Gradient.Enabled:=true;
  //or any manipulations with colors
end;

end.
