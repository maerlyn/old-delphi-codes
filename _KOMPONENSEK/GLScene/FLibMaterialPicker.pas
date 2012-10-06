// FLibMaterialPicker
{: Egg<p>

	Allows choosing a material in a material library<p>

	<b>Historique : </b><font size=-1><ul>
	   <li>14/02/00 - Egg - Creation
	</ul></font>
}
unit FLibMaterialPicker;

interface

uses
  Forms, StdCtrls, Buttons, FRMaterialPreview, Controls, Classes, GLTexture;

type
  TLibMaterialPicker = class(TForm)
	 LBMaterials: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    BBOk: TBitBtn;
    BBCancel: TBitBtn;
    MPPreview: TRMaterialPreview;
    procedure LBMaterialsClick(Sender: TObject);
    procedure LBMaterialsKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
	  function Execute(var materialName : TGLLibMaterialName;
                      materialLibrary : TGLMaterialLibrary) : Boolean;
  end;

function LibMaterialPicker : TLibMaterialPicker;
procedure ReleaseLibMaterialPicker;

implementation

{$R *.DFM}

var
	vLibMaterialPicker : TLibMaterialPicker;

function LibMaterialPicker : TLibMaterialPicker;
begin
	if not Assigned(vLibMaterialPicker) then
	   vLibMaterialPicker:=TLibMaterialPicker.Create(nil);
	Result:=vLibMaterialPicker;
end;

procedure ReleaseLibMaterialPicker;
begin
	if Assigned(vLibMaterialPicker) then begin
	   vLibMaterialPicker.Free; vLibMaterialPicker:=nil;
	end;
end;

// Execute
//
function TLibMaterialPicker.Execute(var materialName : TGLLibMaterialName;
                                    materialLibrary : TGLMaterialLibrary) : Boolean;
begin
   with LBMaterials do begin
      materialLibrary.Materials.SetNamesToTStrings(LBMaterials.Items);
		ItemIndex:=Items.IndexOf(materialName);
		if (ItemIndex<0) and (Items.Count>0) then ItemIndex:=0;
		BBOk.Enabled:=(Items.Count>0);
	end;
	LBMaterialsClick(Self);
	Result:=(ShowModal=mrOk);
	if Result then begin
      with LBMaterials do
         if ItemIndex>=0 then
            materialName:=Items[ItemIndex]
         else materialName:='';
	end;
end;

procedure TLibMaterialPicker.LBMaterialsClick(Sender: TObject);
begin
   with LBMaterials do if ItemIndex>=0 then
      MPPreview.Material:=TGLLibMaterial(Items.Objects[ItemIndex]).Material;
end;

procedure TLibMaterialPicker.LBMaterialsKeyPress(Sender: TObject;
  var Key: Char);
begin
   LBMaterialsClick(Sender);
end;

initialization

finalization

   ReleaseLibMaterialPicker;

end.
