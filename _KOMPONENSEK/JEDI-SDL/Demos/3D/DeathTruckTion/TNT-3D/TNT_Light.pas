unit TNT_Light;

interface

uses
  OpenGL12, TNT_Entity;

type
  TLight = class(TEntity)
  private
    Name: GLenum;
    Position: TVector4f;
  public
    constructor Create(n: GLenum; pos, amb, dif, spe: TVector4f);
    procedure SetPosition;
    destructor Destroy; override;
  end;

implementation

constructor TLight.Create(n: GLenum; pos, amb, dif, spe: TVector4f);
begin
  inherited Create;
  Name := n;
  glEnable(Name);
  glLightfv(Name, GL_AMBIENT, @amb);
  glLightfv(Name, GL_DIFFUSE, @dif);
  glLightfv(Name, GL_SPECULAR, @spe);
  Position := pos;
end;

procedure TLight.SetPosition;
begin
  glLightfv(Name, GL_POSITION, @Position);
end;

destructor TLight.Destroy;
begin
  glDisable(Name);
  inherited Destroy;
end;

end.

