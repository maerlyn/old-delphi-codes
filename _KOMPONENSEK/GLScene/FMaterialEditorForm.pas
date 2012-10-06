{: FMaterialEditorForm<p>

   Editor window for a material (with preview).<p>

   <b>Historique : </b><font size=-1><ul>
      <li>24/03/00 - Egg - Added Blending
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FMaterialEditorForm;

interface

uses
  Windows, Forms, FRMaterialPreview, FRColorEditor, ComCtrls, FRFaceEditor, StdCtrls,
  Controls, Classes, GLTexture, Buttons, FRTextureEdit;

type
  TMaterialEditorForm = class(TForm)
	 PageControl1: TPageControl;
    TSFront: TTabSheet;
    TSBack: TTabSheet;
    TSTexture: TTabSheet;
    FEFront: TRFaceEditor;
    FEBack: TRFaceEditor;
    GroupBox1: TGroupBox;
    MPPreview: TRMaterialPreview;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    RTextureEdit: TRTextureEdit;
    CBBlending: TComboBox;
    Label1: TLabel;
    procedure OnMaterialChanged(Sender : TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;

	 function Execute(material : TGLMaterial) : Boolean;
  end;

function MaterialEditorForm : TMaterialEditorForm;
procedure ReleaseMaterialEditorForm;

implementation

{$R *.DFM}

var
	vMaterialEditorForm : TMaterialEditorForm;

function MaterialEditorForm : TMaterialEditorForm;
begin
	if not Assigned(vMaterialEditorForm) then
	   vMaterialEditorForm:=TMaterialEditorForm.Create(nil);
	Result:=vMaterialEditorForm;
end;

procedure ReleaseMaterialEditorForm;
begin
	if Assigned(vMaterialEditorForm) then begin
	   vMaterialEditorForm.Free; vMaterialEditorForm:=nil;
	end;
end;

// Create
//
constructor TMaterialEditorForm.Create(AOwner : TComponent);
begin
	inherited;
	FEFront.OnChange:=OnMaterialChanged;
	FEBack.OnChange:=OnMaterialChanged;
	RTextureEdit.OnChange:=OnMaterialChanged;
end;

// Execute
//
function TMaterialEditorForm.Execute(material : TGLMaterial) : Boolean;
begin
   with material do begin
      FEFront.FaceProperties:=FrontProperties;
		FEBack.FaceProperties:=BackProperties;
		RTextureEdit.Texture:=Texture;
      CBBlending.ItemIndex:=Integer(BlendingMode);
	end;
	MPPreview.Material:=material;
	Result:=(ShowModal=mrOk);
	if Result then with material do begin
		FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
end;

// OnMaterialChanged
//
procedure TMaterialEditorForm.OnMaterialChanged(Sender : TObject);
begin
   with MPPreview.Material do begin
      FrontProperties:=FEFront.FaceProperties;
		BackProperties:=FEBack.FaceProperties;
		Texture:=RTextureEdit.Texture;
      BlendingMode:=TBlendingMode(CBBlending.ItemIndex);
	end;
	MPPreview.SceneViewer.Invalidate;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   ReleaseMaterialEditorForm;

end.
