unit ParticleEngine;
{****************************************************************************

 <!!!> ParticleEngine is one of the models of the OpenGL engine.
 <!!!> This is a *development* version of the ParticleEngine, NOT FINAL.

     The contents of this file are subject to the Mozilla Public License
     Version 1.1 (the "License"); you may not use this file except in
     compliance with the License. You may obtain a copy of the License at
     http://www.mozilla.org/MPL/

     Software distributed under the License is distributed on an "AS IS"
     basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
     License for the specific language governing rights and limitations
     under the License.

     The Initial Developer of the Original Code is Ariel Jacob.
     Portions created by Ariel Jacob are Copyright (C) 2002 Ariel Jacob.

     Contributor(s):
       Ariel Jacob;               email: ariel@global-rd.com
       Kisnémeth Róbert.
******************************************************************************}

{                  ***** Development dump area *****

}

interface

uses
  Classes,
  OpenGL12,
  Vectors,
  {BillboardUnit,}
  logger,
  SysUtils;

type
  PParticle = ^TParticle;
  TParticle = record
    Position : T3DVector; // my Position in 3D space
    Vector : T3DVector; // my speed in 3D space
    life : Single; // life = 0 => his dead
    fadeSpeed : Single; // Dec(life, fadeSpeed)
    r, g, b : Single; // Particle color
    Size : Single;
  end;

type
  TParticleSystem = class;
  TParticleGroup = class;
  TAdvanceParticleGroup = class;
  TRegenerativeParticleGroup = class;

  TOnGroupDone = procedure( ParticleEngin : TParticleSystem;
    ParticleGroup : TParticleGroup ) overload;

  TParticleGroup = class
    NumberOfParticles : cardinal; // Current particles number
    MaxNumberOfParticles : cardinal; // lenght of dynamic array (MAX)
    Particles : array of TParticle; // Stores the particles
    Dead : boolean; // Do we need to clear the group?
    ID : byte; // Group ID
    TextureID : TGLUInt; // Stores the ID of the texture
    OnGroupDone : TOnGroupDone; // Done/dead event
    ParentEngine : TParticleSystem;

    GroupPos : T3DVector;

    ActiveParticles,
      LastActiveParticles : GLuint;

    ParticleVertexList,
      ParticleColorList,
      ParticleCoordList : array of single;

    procedure Free;
    procedure Move; virtual;
    procedure Draw; virtual;
    procedure AddParticle( const _r, _g, _b : Single;
      _Position, _Vector : T3DVector;
      _life, _fadeSpeed : Single; _size : Single = 0.5 );
    constructor Create( _MaxNumberOfParticles : Cardinal; _TextureID : TGLUInt;
      _ID : byte = 0 );
    procedure SetParticlesColor( _r, _g, _b, _a : single );
  end;

  TAdvanceParticleGroup = class( TParticleGroup )
  private
    grow : Boolean; // dose the size canges?
    growScale : Single; // how much?
    growMaxSize : Single; // whats the max size?
    Force : T3DVector;

    function Getgrow : Single;
    procedure Putgrow( newval : Single );
  public
    property GrowthScale : Single read Getgrow write Putgrow;
    property Gravity : T3DVector read Force write Force;
    procedure SetGrowth( _grow : Boolean; growthScale, growthMaxSize : Single );
    procedure Move; override;
  end;

  TRegenerativeParticleGroup = class( TAdvanceParticleGroup )
  private
    intSize : Single; // 0 = random 1-3
    //intFade  : Single;
    intSpeed : T3DVector; // % of the GroupVector (0= non, 1= the same)

    //    GroupPos    : T3DVector;
    GroupVector : T3DVector;
    GroupForce : T3DVector;

    Duration,
      Reduce : Single;
  public
    procedure Move; override;
    procedure SetIntParm( size : Single );
    procedure SetGroupParm( _GroupPos, _GroupVector,
      _GroupForce : T3DVector;
      _ParticleForce, _intParticleSpeed : T3DVector;
      lifeDuration, lifeReduce : Single );
  end;

  TParticleGroupList = class( TList )
  protected
    function Get( Index : Integer ) : TParticleGroup;
    procedure Put( Index : Integer; Item : TParticleGroup );
  public
    property Items[ Index : Integer ] : TParticleGroup read Get write Put; default;
  end;

  TParticleSystem = class
    NumberOfParticleGroups : cardinal;
    ParticleGroupList : TParticleGroupList;

    //    indexParticleList: GLuint;

    function AddParticleGroup( ParticleGroup : TParticleGroup ) : TParticleGroup;
    procedure RemoveParticleGroup( ParticleGroup : TParticleGroup ); overload;
    procedure RemoveParticleGroup( ParticleGroupID : byte ); overload;
    function ParticlesCount : cardinal;
    procedure Move;
    procedure Draw;
    procedure Free;
    //    procedure CompileList;
    constructor Create;
  end;

implementation

{ -- TParticleGroupList ------------------------------------------------------- }

function TParticleGroupList.Get( Index : Integer ) : TParticleGroup;
begin
  Result := inherited Get( Index );
end;

procedure TParticleGroupList.Put( Index : Integer; Item : TParticleGroup );
begin
  inherited Put( Index, Item );
end;

{ -- TParticleGroupList ------------------------------------------------------- }

constructor TParticleGroup.Create( _MaxNumberOfParticles : Cardinal;
  _TextureID : TGLUInt; _ID : byte = 0 );
begin
  inherited Create;
  Dead := False;
  MaxNumberOfParticles := _MaxNumberOfParticles;
  NumberOfParticles := 0;
  ID := _ID;
  OnGroupDone := nil;
  TextureID := _TextureID;
  SetLength( Particles, MaxNumberOfParticles );

  ActiveParticles := 0;
  LastActiveParticles := 0;
  SetLength( ParticleVertexList, 0 );
  SetLength( ParticleColorList, 0 );
  SetLength( ParticleCoordList, 0 );
end;

procedure TParticleGroup.Free;
begin
  SetLength( ParticleVertexList, 0 ); ParticleVertexList := nil;
  SetLength( ParticleColorList, 0 ); ParticleColorList := nil;
  SetLength( ParticleCoordList, 0 ); ParticleCoordList := nil;
  SetLength( Particles, 0 ); Particles := nil;
  inherited free;
end;

procedure TParticleGroup.AddParticle( const _r, _g, _b : Single;
  _Position, _Vector : T3DVector; _life, _fadeSpeed : Single; _size : Single = 0.5 );
begin
  if NumberOfParticles = MaxNumberOfParticles then
    Exit;
  with Particles[ NumberOfParticles ] do
  begin
    r := _r;
    g := _g;
    b := _b;
    Position := _Position;
    Vector := _Vector;
    life := _life;
    fadeSpeed := _fadeSpeed;
    size := _size / 2;
  end;
  Inc( NumberOfParticles );
end;

procedure TParticleGroup.SetParticlesColor( _r, _g, _b, _a : single );
var
  I : Cardinal;
begin
  I := 0;
  if NumberOfParticles = 0 then Exit;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      r := _r;
      g := _g;
      b := _b;
      life := _a;
    end;
    Inc( I );
  end;
end;

procedure TParticleGroup.Move;
var
  I : Cardinal;
begin
  I := 0;
  ActiveParticles := 0;
  Dead := True;
  if NumberOfParticles = 0 then Exit;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      if life > 0 then
      begin
        Dead := False;
        Position := VectorAdd( Position, Vector );
        life := life - fadeSpeed;
        Inc( ActiveParticles );
      end;
    end;
    Inc( I );
  end;
  if Dead and Assigned( OnGroupDone ) then
    OnGroupDone( ParentEngine, self );
end;

procedure TParticleGroup.Draw;
// helper procedures to set the array
  procedure Buiild4Array( index, part : GLuint;
    var List : array of single; v1, v2, v3, v4 : single );
  var
    pos : GLuint;
  begin
    pos := ( index shl 4 ) + ( part shl 2 ); // (shl 2 = *4, 3=8, 4=16...)
    List[ pos + 0 ] := v1;
    List[ pos + 1 ] := v2;
    List[ pos + 2 ] := v3;
    List[ pos + 3 ] := v4;
  end;
  procedure Buiild3Array( index, part : GLuint;
    var List : array of single; v1, v2, v3 : single );
  var
    pos : GLuint;
  begin
    pos := index * 12 + ( part * 3 );
    List[ pos + 0 ] := v1;
    List[ pos + 1 ] := v2;
    List[ pos + 2 ] := v3;
  end;
  procedure Buiild3vArray( index, part : GLuint;
    var List : array of single; vec : T3DVector );
  var
    pos : GLuint;
  begin
    pos := index * 12 + ( part * 3 );
    List[ pos + 0 ] := vec.X;
    List[ pos + 1 ] := vec.Y;
    List[ pos + 2 ] := vec.Z;
  end;
  procedure Buiild2Array( index, part : GLuint;
    var List : array of single; v1, v2 : single );
  var
    pos : GLuint;
  begin
    pos := ( index shl 3 ) + ( part shl 1 );
    List[ pos + 0 ] := v1;
    List[ pos + 1 ] := v2;
  end;

var
  I, index : GLuint;
  vx, vy : T3DVector;
  mat : array[ 0..15 ] of single;
  vA, vB,
    vC, vD : T3DVector;
  tempv1,
    tempv2,
    tempv3 : T3DVector;
begin
  if ActiveParticles = 0 then Exit;
  if ActiveParticles <> LastActiveParticles then
  begin
    SetLength( ParticleVertexList, ( ActiveParticles + 0 ) * 4 * 3 );
    SetLength( ParticleColorList, ( ActiveParticles + 0 ) * 4 * 4 );
    SetLength( ParticleCoordList, ( ActiveParticles + 0 ) * 4 * 2 );
    LastActiveParticles := ActiveParticles;
  end;

  glGetFloatv( GL_MODELVIEW_MATRIX, @mat );

  vx := Vector( mat[ 0 ], mat[ 4 ], mat[ 8 ] );
  vy := Vector( mat[ 1 ], mat[ 5 ], mat[ 9 ] );

  I := 0;
  index := 0;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      if Life > 0 then
      begin

        // why 3 temps.. thats a good question.. :/
        tempv1 := VectorScale( vx, -1 );
        tempv2 := VectorSubtract( tempv1, vy );
        tempv3 := VectorScale( tempv2, size );
        vA := VectorAdd( Position, tempv3 );
        tempv1 := vx;
        tempv2 := VectorSubtract( tempv1, vy );
        tempv3 := VectorScale( tempv2, size );
        vB := VectorAdd( Position, tempv3 );
        tempv1 := vx;
        tempv2 := VectorAdd( tempv1, vy );
        tempv3 := VectorScale( tempv2, size );
        vC := VectorAdd( Position, tempv3 );
        tempv1 := VectorScale( vx, -1 );
        tempv2 := VectorAdd( tempv1, vy );
        tempv3 := VectorScale( tempv2 , size );
        vD := VectorAdd( Position, tempv3 );

        Buiild3vArray( index, 0, ParticleVertexList, vA );
        Buiild3vArray( index, 1, ParticleVertexList, vB );
        Buiild3vArray( index, 2, ParticleVertexList, vC );
        Buiild3vArray( index, 3, ParticleVertexList, vD );

        Buiild2Array( index, 0, ParticleCoordList, 0, 0 );
        Buiild2Array( index, 1, ParticleCoordList, 1, 0 );
        Buiild2Array( index, 2, ParticleCoordList, 1, 1 );
        Buiild2Array( index, 3, ParticleCoordList, 0, 1 );

        Buiild4Array( index, 0, ParticleColorList, r, g, b, life );
        Buiild4Array( index, 1, ParticleColorList, r, g, b, life );
        Buiild4Array( index, 2, ParticleColorList, r, g, b, life );
        Buiild4Array( index, 3, ParticleColorList, r, g, b, life );

        Inc( index );
      end;
    end;
    Inc( I );
  end;

  glEnableClientState( GL_VERTEX_ARRAY );
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );
  glEnableClientState( GL_COLOR_ARRAY );

  glVertexPointer( 3, GL_FLOAT, 0, @ParticleVertexList[ 0 ] );
  glTexCoordPointer( 2, GL_FLOAT, 0, @ParticleCoordList[ 0 ] );
  glColorPointer( 4, GL_FLOAT, 0, @ParticleColorList[ 0 ] );

  glBindTexture( GL_TEXTURE_2D, TextureID );
  glDrawArrays( GL_QUADS {GL_TRIANGLE_STRIP}, 0, ActiveParticles * 4 {4*3} );
end;

{ -- TAdvanceParticleGroup --------------------------------------------------- }

function TAdvanceParticleGroup.Getgrow : single;
begin
  result := growScale * 2;
end;

procedure TAdvanceParticleGroup.Putgrow( newval : Single );
begin
  growScale := newval / 2;
end;

procedure TAdvanceParticleGroup.SetGrowth( _grow : Boolean; growthScale,
  growthMaxSize : Single );
begin
  grow := _grow;
  Putgrow( growthScale );
  growMaxSize := growthMaxSize;
end;

procedure TAdvanceParticleGroup.Move;
var
  I : Cardinal;
begin
  I := 0;
  ActiveParticles := 0;
  Dead := True;
  if NumberOfParticles = 0 then Exit;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      if life > 0 then
      begin
        Dead := False;
        Position := VectorAdd( Position, Vector );
        life := life - fadeSpeed;
        Vector := VectorAdd( Vector, Force );
        Inc( ActiveParticles );

        if grow and ( Size <= growMaxSize ) then
          Size := Size + growScale;
      end;
    end;
    Inc( I );
  end;
  if Dead and Assigned( OnGroupDone ) then
    OnGroupDone( ParentEngine, self );
end;

{ -- TRegenerativeParticleGroup ----------------------------------------------- }

procedure TRegenerativeParticleGroup.Move;
var
  I : Cardinal;
begin
  I := 0;
  Dead := True;
  ActiveParticles := 0;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      if life > 0 then
      begin
        Dead := False;
        Position := VectorAdd( Position, Vector );
        life := life - fadeSpeed;
        Vector := VectorAdd( Vector, Force );

        if grow and ( Size <= growMaxSize ) then
          Size := Size + growScale;
        Inc( ActiveParticles );
      end
      else
      begin
        if Duration > 0 then
        begin
          Position := GroupPos;
          Vector := VectorMultiply( GroupVector, intSpeed );
          life := 1;
          //fadeSpeed:= intFade;
          size := intSize;
          Inc( ActiveParticles );
        end;
      end;
    end;
    Inc( I );
  end;
  GroupPos := VectorAdd( GroupPos, GroupVector );
  GroupVector := VectorAdd( GroupVector, GroupForce );
  Duration := Duration - Reduce;

  Dead := Dead and ( Duration < 0 );
  if Dead and Assigned( OnGroupDone ) then
    OnGroupDone( ParentEngine, self );
end;

procedure TRegenerativeParticleGroup.SetIntParm( size : Single );
begin
  intSize := size;
end;

procedure TRegenerativeParticleGroup.SetGroupParm( _GroupPos, _GroupVector,
  _GroupForce : T3DVector; _ParticleForce, _intParticleSpeed : T3DVector;
  lifeDuration, lifeReduce : Single );
begin
  GroupPos := _GroupPos;
  GroupVector := _GroupVector;
  GroupForce := _GroupForce;
  Force := _ParticleForce;
  intSpeed := _intParticleSpeed;
  Duration := lifeDuration;
  Reduce := lifeReduce;
end;

{ -- TParticleSystem --------------------------------------------------------- }

constructor TParticleSystem.Create;
begin
  inherited Create;
  ParticleGroupList := TParticleGroupList.Create;
  NumberOfParticleGroups := 0;
end;

function TParticleSystem.AddParticleGroup(
  ParticleGroup : TParticleGroup ) : TParticleGroup;
begin
  Inc( NumberOfParticleGroups );
  ParticleGroupList.Add( ParticleGroup );
  ParticleGroup.ParentEngine := self;
  result := ParticleGroup;
end;

procedure TParticleSystem.RemoveParticleGroup( ParticleGroup : TParticleGroup );
begin
  ParticleGroup.Free;
  Dec( NumberOfParticleGroups );
  ParticleGroupList.Remove( ParticleGroup );
end;

procedure TParticleSystem.RemoveParticleGroup( ParticleGroupID : byte );
var
  I : GLuint;
begin
  if NumberOfParticleGroups = 0 then Exit;
  I := 0;
  while I < NumberOfParticleGroups do
  begin
    if ParticleGroupID = ParticleGroupList[ I ].ID then
      ParticleGroupList[ I ].Dead := True;
    Inc( I );
  end;
end;

function TParticleSystem.ParticlesCount : cardinal;
var
  I : GLuint;
begin
  result := 0;
  if NumberOfParticleGroups = 0 then Exit;
  I := 0;
  while I < NumberOfParticleGroups do
  begin
    result := result + ParticleGroupList[ I ].ActiveParticles;
    Inc( I );
  end;
end;

procedure TParticleSystem.Move;
var
  I : GLuint;
begin
  if NumberOfParticleGroups = 0 then Exit;
  I := 0;
  while I < NumberOfParticleGroups do
  begin
    ParticleGroupList[ I ].Move;
    if ParticleGroupList[ I ].Dead then
      RemoveParticleGroup( ParticleGroupList[ I ] );
    Inc( I );
  end;
end;

procedure TParticleSystem.Draw;
var
  I : GLuint;
begin
  if NumberOfParticleGroups = 0 then Exit;
  I := 0;
  while I < NumberOfParticleGroups do
  begin
    ParticleGroupList[ I ].Draw;
    Inc( I );
  end;
end;

procedure TParticleSystem.Free;
var
  I : GLuint;
begin
  //  glDeleteLists(1, indexParticleList);
  if NumberOfParticleGroups > 0 then
  begin
    I := 0;
    while I < NumberOfParticleGroups do
    begin
      RemoveParticleGroup( ParticleGroupList[ I ] );
    end;
  end;
  inherited Free;
end;

end.

 