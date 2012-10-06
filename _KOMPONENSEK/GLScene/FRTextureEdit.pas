// FRTextureEdit
{: Egg<p>

	Basic editing frame for TGLTexture<p>

	<b>Historique : </b><font size=-1><ul>
		<li>17/03/00 - Egg - Added ImageAlpha combo
		<li>13/03/00 - Egg - Creation
	</ul></font>
}
{ TODO : Replace STImageClass with a dropdown (polymorphism) }
unit FRTextureEdit;

interface

uses
  Forms, StdCtrls, Buttons, Controls, Classes, GLTexture;

type
  TRTextureEdit = class(TFrame)
    Label2: TLabel;
    SBEditImage: TSpeedButton;
    CBMagFilter: TComboBox;
	 Label3: TLabel;
	 Label4: TLabel;
	 CBMinFilter: TComboBox;
	 CBTextureMode: TComboBox;
	 Label1: TLabel;
	 Label5: TLabel;
	 CBTextureWrap: TComboBox;
	 CBDisabled: TCheckBox;
	 CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    Label6: TLabel;
    procedure CBMagFilterChange(Sender: TObject);
    procedure CBMinFilterChange(Sender: TObject);
    procedure CBTextureModeChange(Sender: TObject);
    procedure CBTextureWrapChange(Sender: TObject);
    procedure CBDisabledClick(Sender: TObject);
    procedure SBEditImageClick(Sender: TObject);
    procedure CBImageClassChange(Sender: TObject);
    procedure CBImageAlphaChange(Sender: TObject);
	 
  private
	 { Déclarations privées }
	 FTexture : TGLTexture;
	 FOnChange : TNotifyEvent;
	 changeing : Boolean;

  protected
	 { Déclarations protégées }
	 procedure SetTexture(const val : TGLTexture);
	 procedure DoOnChange; dynamic;

  public
	 { Déclarations publiques }
	 constructor Create(AOwner: TComponent); override;
	 destructor Destroy; override;

	 property Texture : TGLTexture read FTexture write SetTexture;
	 property OnChange : TNotifyEvent read FOnChange write FOnChange;

  end;

implementation

{$R *.DFM}

uses SysUtils, GLMisc;

// Create
//
constructor TRTextureEdit.Create(AOwner: TComponent);
begin
	inherited;
	FTexture:=TGLTexture.Create(Self);
	SetTexture(FTexture);
	SetGLTextureImageClassesToStrings(CBImageClass.Items);
end;

// Destroy
//
destructor TRTextureEdit.Destroy;
begin
	FTexture.Free;
	inherited;
end;

// SetTexture
//
procedure TRTextureEdit.SetTexture(const val : TGLTexture);
begin
	FTexture.Assign(val);
	changeing:=True;
	try
		with CBImageClass do ItemIndex:=Items.IndexOfObject(Pointer(FTexture.ClassType));
		CBImageAlpha.ItemIndex:=Integer(FTexture.ImageAlpha);
		CBMagFilter.ItemIndex:=Integer(FTexture.MagFilter);
		CBMinFilter.ItemIndex:=Integer(FTexture.MinFilter);
		CBTextureMode.ItemIndex:=Integer(FTexture.TextureMode);
		CBTextureWrap.ItemIndex:=Integer(FTexture.TextureWrap);
		CBDisabled.Checked:=FTexture.Disabled;
	finally
		changeing:=False;
		DoOnChange;
	end;
end;

// DoOnChange
//
procedure TRTextureEdit.DoOnChange;
begin
	if (not changeing) and Assigned(FOnChange) then
		OnChange(Self);
end;

// CBImageClassChange
//
procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
	tic : TGLTextureImageClass;
	ti : TGLTextureImage;
begin
	if not changeing then begin
		with CBImageClass do	tic:=TGLTextureImageClass(Items.Objects[ItemIndex]);
		if FTexture.Image.ClassType<>tic then begin
			ti:=TGLTextureImageClass(tic).Create(Self);
			FTexture.Image:=ti;
			ti.Free;
		end;
		DoOnChange;
	end;
end;

// CBImageAlphaChange
//
procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
	FTexture.ImageAlpha:=TGLTextureImageAlpha(CBImageAlpha.ItemIndex);
	DoOnChange;
end;

// CBMagFilterChange
//
procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
	FTexture.MagFilter:=TGLMagFilter(CBMagFilter.ItemIndex);
	DoOnChange;
end;

// CBMinFilterChange
//
procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
	FTexture.MinFilter:=TGLMinFilter(CBMinFilter.ItemIndex);
	DoOnChange;
end;

// CBTextureModeChange
//
procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
	FTexture.TextureMode:=TGLTextureMode(CBTextureMode.ItemIndex);
	DoOnChange;
end;

// CBTextureWrapChange
//
procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
	FTexture.TextureWrap:=TGLTextureWrap(CBTextureWrap.ItemIndex);
	DoOnChange;
end;

// CBDisabledClick
//
procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
	FTexture.Disabled:=CBDisabled.Checked;
	DoOnChange;
end;

// SBEditImageClick
//
procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
	FTexture.Image.Edit;
end;

end.
