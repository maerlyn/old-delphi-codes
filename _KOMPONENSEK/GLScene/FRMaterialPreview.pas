{: FRMaterialPreview<p>

   Material Preview frame.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRMaterialPreview;

interface

uses
  Windows, Forms, StdCtrls, GLScene, GLObjects, Classes, Controls, GLTexture,
  GLMisc;

type
  TRMaterialPreview = class(TFrame)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    CBObject: TComboBox;
    Camera: TGLCamera;
    Cube: TCube;
    Sphere: TSphere;
    LightSource: TGLLightSource;
    CBBackground: TComboBox;
    PlanePattern: TPlane;
    procedure CBObjectChange(Sender: TObject);
    procedure CBBackgroundChange(Sender: TObject);

  private
    { Déclarations privées }
    function GetGLMaterial : TGLMaterial;
    procedure SetGLMaterial(const val : TGLMaterial);

  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;

    property Material : TGLMaterial read GetGLMaterial write SetGLMaterial;

  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.DFM}

uses Graphics;

constructor TRMaterialPreview.Create(AOwner : TComponent);
begin
   inherited;
   CBObject.ItemIndex:=0;     CBObjectChange(Self);
   CBBackground.ItemIndex:=0; CBBackgroundChange(Self);
end;

// GetGLMaterial
//
function TRMaterialPreview.GetGLMaterial : TGLMaterial;
begin
   case CBObject.ItemIndex of
      1 : Result:=Sphere.Material;
   else
      Result:=Cube.Material;
   end;
end;

// SetGLMaterial
//
procedure TRMaterialPreview.SetGLMaterial(const val : TGLMaterial);
begin
   Cube.Material.Assign(val);
   Sphere.Material.Assign(val);
end;

procedure TRMaterialPreview.CBObjectChange(Sender: TObject);
var
   i : Integer;
begin
   i:=CBObject.ItemIndex;
   Sphere.Visible:=((i and 1)=1);
   Cube.Visible:=((i and 1)<>1);
   if Sphere.Visible then
      SetGLMaterial(Cube.Material)
   else SetGLMaterial(Sphere.Material);
end;

procedure TRMaterialPreview.CBBackgroundChange(Sender: TObject);
var
   bgColor : TColor;
begin
   case CBBackground.ItemIndex of
      1 : bgColor:=clWhite;
      2 : bgColor:=clBlack;
      3 : bgColor:=clBlue;
      4 : bgColor:=clRed;
      5 : bgColor:=clGreen;
   else
      bgColor:=clNone;
   end;
   with PlanePattern.Material do begin
      Texture.Disabled:=(bgColor<>clNone);
      FrontProperties.Diffuse.Color:=ConvertWinColor(bgColor);
   end;
end;

end.
