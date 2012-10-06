unit G32_RotLayer;

interface

uses
  SysUtils, Classes, Controls, Forms, Graphics, G32, G32_Layers, G32_Transforms;

type
  TCustomAffineLayer = class(TCustomLayer)
  private
    FAlphaHit: Boolean;
    FTransformation: TAffineTransformation;
    FBitmapCenter: TFloatPoint;
    FBitmap: TBitmap32;
    procedure BitmapChanged(Sender: TObject);
    procedure SetBitmap(Value: TBitmap32);
  protected
    procedure AdjustTransformation; virtual;
    function DoHitTest(X, Y: Integer): Boolean; override;
    procedure Paint(Buffer: TBitmap32); override;
    property Transformation: TAffineTransformation read FTransformation;
  public
    constructor Create(ALayerCollection: TLayerCollection); override;
    destructor Destroy; override;
    property AlphaHit: Boolean read FAlphaHit write FAlphaHit;
    property Bitmap: TBitmap32 read FBitmap write SetBitmap;
  end;

  TAffineLayer = class(TCustomAffineLayer)
  public
    property Transformation;
  end;

  TRotLayer = class(TCustomAffineLayer)
  private
    FPosition: TFloatPoint;
    FScaled: Boolean;
    FAngle: Single;
    procedure SetAngle(Value: Single);
    procedure SetPosition(const Value: TFloatPoint);
    procedure SetScaled(Value: Boolean);
    procedure SetBitmapCenter(const Value: TFloatPoint);
  protected
    procedure AdjustTransformation; override;
  public
    property Angle: Single read FAngle write SetAngle;
    property BitmapCenter: TFloatPoint read FBitmapCenter write SetBitmapCenter;
    property Scaled: Boolean read FScaled write SetScaled;
    property Position: TFloatPoint read FPosition write SetPosition;
  end;

implementation

{ TCustomAffineLayer }

type TATAccess = class(TAffineTransformation);

procedure TCustomAffineLayer.AdjustTransformation;
begin
  // do nothing here
end;

procedure TCustomAffineLayer.BitmapChanged(Sender: TObject);
begin
  Transformation.SrcRect := FloatRect(0, 0, Bitmap.Width - 1, Bitmap.Height - 1);
  Changed;
end;

constructor TCustomAffineLayer.Create(ALayerCollection: TLayerCollection);
begin
  inherited;
  FBitmap := TBitmap32.Create;
  FBitmap.OnChange := BitmapChanged;
  FTransformation := TAffineTransformation.Create;
end;

destructor TCustomAffineLayer.Destroy;
begin
  FTransformation.Free;
  FBitmap.Free;
  inherited;
end;

function TCustomAffineLayer.DoHitTest(X, Y: Integer): Boolean;
var
  BX, BY: Integer;
  Pt: TPoint;
begin
  with TATAccess(Transformation) do Transform(X, Y, BX, BY); // BX,BY - in 'FBitmap' coordinates
  Pt := Point(BX, BY);
  if PtInRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Pt) then Result := True;
  if Result and AlphaHit and (Bitmap.PixelS[BX, BY] and $FF000000 = 0) then
    Result := False;
end;

procedure TCustomAffineLayer.Paint(Buffer: TBitmap32);
begin
  AdjustTransformation;
  Transform(Buffer, FBitmap, Transformation);
end;

procedure TCustomAffineLayer.SetBitmap(Value: TBitmap32);
begin
  FBitmap.Assign(Value);
end;

{ TRotLayer }

procedure TRotLayer.AdjustTransformation;
begin
  Transformation.Clear;
  Transformation.Translate(-BitmapCenter.X, -BitmapCenter.Y);
  Transformation.Rotate(0, 0, Angle);
  Transformation.Translate(Position.X, Position.Y);
  if Scaled and Assigned(LayerCollection) and Assigned(LayerCollection.CoordXForm) then
    with LayerCollection.CoordXForm^ do
    begin
      Transformation.Scale(ScaleX / 65536, ScaleY / 65536);
      Transformation.Translate(ShiftX, ShiftY);
    end;
end;

procedure TRotLayer.SetAngle(Value: Single);
begin
  FAngle := Value;
  Changed;
end;

procedure TRotLayer.SetBitmapCenter(const Value: TFloatPoint);
begin
  FBitmapCenter := Value;
  Changed;
end;

procedure TRotLayer.SetPosition(const Value: TFloatPoint);
begin
  FPosition := Value;
  Changed;
end;

procedure TRotLayer.SetScaled(Value: Boolean);
begin
  FScaled := Value;
  Changed;
end;

end.
