unit TNT_Scene;

// Gestion de la scene
// -------------------
//  La scene est constituée de modeles (tableau) et d'entitees (liste chainée)
//  Plusieurs obj peuvent partager le meme modele (d'ou la differenciation)

interface

uses
  OpenGL12, TNT_Entity, TNT_Model, TNT_Camera;

type
  TScene = class
  public
    Entities: TEntity;
    Models: array of TModel;
    constructor Create;
    procedure LoadFromFile(FileName: String);
    procedure Render(TimeElapsed: Single);
    function LoadModel(ModelName: String): Integer;
    procedure Add(Entity: TEntity);
    procedure Remove(Entity: TEntity);
    procedure FlushModels;
    procedure FlushEntities(EntityType: TEntitySet);
    destructor Destroy; override;
  end;

implementation

uses
  TNT_3D, TNT_File, TNT_Console;

constructor TScene.Create;
begin
  inherited Create;
  Console.Log('Creating scene...');
  glClearColor(0, 0, 0, 0);
  glClearDepth(1);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glEnable(GL_CULL_FACE);
  glEnable(GL_TEXTURE_2D);
end;

procedure TScene.LoadFromFile(FileName: String);
var
  F: TFile;
  Str: String;
  c: Char;
begin
  F := TFile.Open(FileName);
  repeat
    Str := '';
    repeat
      c := F.ReadChar;
      Str := Str + c;
    until c=#10;
    if Copy(Str,1,3) <> 'end' then
      with Console do
      begin
        Lines[Cursor] := PROMPT + Copy(Str, 1, Length(Str)-2);
        Execute;
      end;
  until Copy(Str,1,3) = 'end';
  F.Close;
end;

procedure TScene.Render(TimeElapsed: Single);
var
  Entity: TEntity;
begin
  TNT.Tris := 0;
  Entity := Entities;
  while Entity <> nil do
    with Entity do
    begin
      if not Dead then
      begin
        Animate(TimeElapsed);
        if Visible then
          Render;
      end
      else
        Destroy;
      Entity := Next;
    end;
end;

function TScene.LoadModel(ModelName: String): Integer;
begin
  Console.Log('Loading model ' + ModelName);
  Result := Length(Models);
  SetLength(Models, Result+1);
  Models[Result] := TModel.Create(ModelName);
end;

procedure TScene.Add(Entity: TEntity);
var
  Next: PEntity;
begin
  Console.Log('Add entity: ' + Entity.ClassName);
  Next := @Entities;
  while (Next^ <> nil) and (Next^.Typ < Entity.Typ) do
    Next := @Next^.Next;
  Entity.Next := Next^;
  Next^ := Entity;
end;

procedure TScene.Remove(Entity: TEntity);
var
  Next: PEntity;
begin
  Console.Log('Remove entity: ' + Entity.ClassName);
  Next := @Entities;
  while (Next^ <> nil) and (Next^ <> Entity) do
    Next := @Next^.Next;
  if Next <> nil then
    Next^ := Entity.Next;
end;

procedure TScene.FlushModels;
var
  i: Integer;
begin
  Console.Log('Flushing models');
  for i := 0 to Length(Models)-1 do
    Models[i].Free;
  Models := nil;
end;

procedure TScene.FlushEntities(EntityType: TEntitySet);
var
  Entity, Temp: TEntity;
begin
  Console.Log('Flushing entities');
  Entity := Entities;
  while Entity <> nil do
  begin
    Temp := Entity;
    Entity := Entity.Next;
    if Temp.Typ in EntityType then
      Temp.Free;
  end;
end;

destructor TScene.Destroy;
begin
  FlushModels;
  FlushEntities([TYPE_CAMERA, TYPE_LIGHT, TYPE_SKYBOX, TYPE_LANDSCAPE,
                 TYPE_OBJECT, TYPE_PARTICLES, TYPE_HUD]);
  inherited Destroy;
end;

end.

