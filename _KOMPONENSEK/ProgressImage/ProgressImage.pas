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

unit ProgressImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TProgressImage = class(TCustomControl)
  private
    FMin: Integer;
    FStep: Integer;
    FPosition: Integer;
    FOwner: TComponent;
    FMax: Integer;
    FPicture: TPicture;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetStep(const Value: Integer);
    procedure SetPicture(const Value: TPicture);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StepIt;
  published
    { Published declarations }
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Picture: TPicture read FPicture write SetPicture; 
    property Position: Integer read FPosition write SetPosition;
    property Step: Integer read FStep write SetStep;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DSO', [TProgressImage]);
end;

{ TProgressImage }

constructor TProgressImage.Create(AOwner: TComponent);
begin
  inherited;
  FPicture:=TPicture.Create;
  FMin:=0;
  FMax:=100;
  FPosition:=0;
  FStep:=1;
  Width:=150;
  Height:=17;
  FOwner:=AOwner;
  DoubleBuffered:=true;
end;

destructor TProgressImage.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TProgressImage.Paint;
var
  Bitmap: TBitmap;
  IStep: Double;
  IPos: Integer;
begin
  inherited;
  with Canvas do begin
    Brush.Color:=(FOwner as TForm).Color;
    FillRect(Rect(0,0,Width-1,Height-1));
    Pen.Color:=clBtnShadow;
    MoveTo(0,0);
    LineTo(Width-1,0);
    MoveTo(0,0);
    LineTo(0,Height-1);
    Pen.Color:=clWhite;
    MoveTo(Width-1,0);
    LineTo(Width-1,Height-1);
    MoveTo(Width-1,Height-1);
    LineTo(0,Height-1);

    Bitmap:=TBitmap.Create;
    with Bitmap do begin
      Width:=Self.Width-2;
      Height:=Self.Height-2;
      Canvas.Brush.Color:=(FOwner as TForm).Color;
      Canvas.FillRect(Rect(0,0,Width,Height));
      Canvas.StretchDraw(Rect(0,0,Width,Height),FPicture.Graphic);
    end;

    IStep:=(Width-2)/(FMax-FMin);
    IPos:=Round(IStep*(FPosition-FMin));
    StretchBlt(Canvas.Handle,1,1,IPos,Height-1,Bitmap.Canvas.Handle,0,0,IPos,Height-1,SRCCOPY);

    Bitmap.Free;
  end;
end;

procedure TProgressImage.SetMax(const Value: Integer);
begin
  FMax:=Value;
  if FMin>FMax then
    FMin:=FMax;
  if Position>FMax then
    Position:=FMax;
  Invalidate;
end;

procedure TProgressImage.SetMin(const Value: Integer);
begin
  FMin:=Value;
  if FMin>FMax then
    FMax:=FMin;
  if Position>FMax then
    Position:=FMax;
  Invalidate;
end;

procedure TProgressImage.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  Invalidate;
end;

procedure TProgressImage.SetPosition(const Value: Integer);
begin
  FPosition:=Value;
  if FPosition<FMin then
    FPosition:=FMin;
  if fPosition>FMax then
    FPosition:=FMax;
  Invalidate;
end;

procedure TProgressImage.SetStep(const Value: Integer);
begin
  if (Value>=0) and (Value<=FMax) then
    FStep:=Value;
end;

procedure TProgressImage.StepIt;
begin
  if FPosition+FStep<=FMax then
    Inc(FPosition,FStep)
  else
    FPosition:=FMax;
  Invalidate;    
end;

end.
 