unit BspFile;

interface

type
  Vec2f = record
    x, y : Single;
  end;

  Vec3f = record
    x, y, z : Single;
  end;

  TVertex = record
    tv, lv : Vec2f;
    pos : Vec3f;
  end;

  TPlane = record
    a, b, c, d : Single;
  end;

  TFace = record
    start, num, id, lid, pad : LongWord;
    plane : TPlane;
  end;
  
  TTexture = record
    id : LongWord;
    name : array[ 0..63 ] of char;
  end;
  
  TLightmap = record
    id : LongWord;
    data : array[ 0..127, 0..127, 0..2 ] of byte;
  end;

  TVertices = array of TVertex;
  TFaces = array of TFace;
  TTextures = array of TTexture;
  TLightmaps = array of TLightmap;

  TBSP = class
  private
    fVertices : TVertices;
    fFaces : TFaces;
    fTextures : TTextures;
    fLightmaps : TLightmaps;

    fnumVerts : LongWord;
    fnumFaces : LongWord;
    fnumLightmaps : LongWord;
    fnumTextures : LongWord;

  public
    procedure LoadFromFile( const filename : string );

    property Vertices : TVertices read fVertices;
    property Faces : TFaces read fFaces;
    property Textures : TTextures read fTextures;
    property Lightmaps : TLightmaps read fLightmaps;

    property NumVerts : LongWord read fNumVerts write fNumVerts;
    property NumFaces : LongWord read fNumFaces write fNumFaces;
    property NumLightmaps : LongWord read fNumLightmaps write fNumLightmaps;
    property NumTextures : LongWord read fNumTextures write fNumTextures;

  end;

implementation

procedure TBSP.LoadFromFile( const filename : string );
var
  f : file;
  i : Integer;
  ver : TVertex;
  face : TFace;
  tex : TTexture;
  lmp : TLightmap;
begin
  assignfile( f, filename );
  reset( f, 1 );

  // read number of vertices
  blockread( f, fnumVerts, sizeof( fnumVerts ) );
  setlength( fVertices, fnumVerts );

  // read number of faces
  blockread( f, fnumFaces, sizeof( fnumFaces ) );
  setlength( fFaces, fnumFaces );

  // read number of textures
  blockread( f, fnumTextures, sizeof( fnumTextures ) );
  setlength( fTextures, fnumTextures );

  // read number of lightmaps
  blockread( f, fnumLightmaps, sizeof( fnumLightmaps ) );
  setlength( fLightmaps, fnumLightmaps );

  for i := 0 to fnumVerts - 1 do
  begin
    blockread( f, ver, sizeof( ver ) );
    fVertices[ i ] := ver;
  end;

  for i := 0 to fnumFaces - 1 do
  begin
    blockread( f, face, sizeof( face ) );
    fFaces[ i ] := face;
  end;

  for i := 0 to fnumTextures - 1 do
  begin
    blockread( f, tex, sizeof( tex ) );
    fTextures[ i ] := tex;
  end;

  for i := 0 to fnumLightmaps - 1 do
  begin
    blockread( f, lmp, sizeof( lmp ) );
    fLightmaps[ i ] := lmp;
  end;
  closefile( f );
end;

end.

