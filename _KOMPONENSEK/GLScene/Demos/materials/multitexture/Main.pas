{: This sample mixes two textures by using multitexturing.<p>

   Multitexturing requires at least two materials in the material library:<ul>
   <li>a base material: determines the basic properties, incl. blending, colors
      and lighting-related properties
   <li>a second material: determines the second texture (color and other
      properties are ignored)
   </ul><p>

   This structure allows reuse of second textures among a variety of materials,
   this is particularly usefull for details maps, which are usually just "noise"
   to be applied on different base textures. You can also use it to reuse a basic
   standard lighting map throughout many objects, thus reducing texture memory
   needs (many shadows can be derived from a few deformed basic maps).<p>

   The texture matrix (scale, offset) are adjusted independantly for the two
   textures, in this sample, the TrackBar adjusts an isotropic scaling.<p>

   When multi-texturing, never forget that both texture modes (decal, modulate etc.)
   are honoured. For instance, if you "Decal" a non-transparent second texture,
   the base texture will be completely replaced! 
}
unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLMisc, GLObjects, GLTexture, ComCtrls, StdCtrls, ExtCtrls,
  ExtDlgs;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Plane1: TPlane;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Jpeg;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName));
   // prepare images to merge in the multitexture
   with GLMaterialLibrary1 do begin
      Image1.Picture.LoadFromFile('..\..\media\ashwood.jpg');
      Materials[0].Material.Texture.Image.Assign(Image1.Picture);
      Image2.Picture.LoadFromFile('..\..\media\Flare1.bmp');
      Materials[1].Material.Texture.Image.Assign(Image2.Picture);
   end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
   // load a new Image1
   if OpenPictureDialog1.Execute then begin
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      GLMaterialLibrary1.Materials[0].Material.Texture.Image.Assign(Image1.Picture);
   end;
end;

procedure TForm1.Image2Click(Sender: TObject);
begin
   // load a new Image2
   if OpenPictureDialog1.Execute then begin
      Image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      GLMaterialLibrary1.Materials[1].Material.Texture.Image.Assign(Image2.Picture);
   end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // adjust scale
   with GLMaterialLibrary1.Materials[1].TextureScale do begin
      X:=TrackBar1.Position/10;
      Y:=TrackBar1.Position/10;
   end;
end;

end.
