program JEDISDLOpenGLClothSim;
{******************************************************************}
{                                                                  }
{                    Cloth Simulation Demo                         }
{                                                                  }
{ Portions created by Jordan Isaak <jordanisaak@hotmail.com>, are  }
{ Copyright (C) 2002 Jordan Isaak.                                 }
{ http://www.members.shaw.ca/jdisaak                               }
{ All Rights Reserved.                                             }
{                                                                  }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2002 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{  Shows how to use OpenGL with the SDL libraries                  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Also Makes uses of Mike Lischke's Cross-Platform OpenGL header.}
{   You can pick it up from...                                     }
{   http://www.lischke-online.de/Graphics.html#OpenGL12            }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   A cloth simulation implemented through Verlet integration and  }
{   constraints on particles.                                      }
{  Controls:                                                       }
{  +/- Changes wind speed                                          }
{  x/z Zooms camera in and out                                     }
{  f   Sets cloth as if it is attached to a flagpole               }
{  c   Sets cloth as if it is hanging from a clothesline           }
{  t   Toggles through different textures for the cloth            }
{                                                                  }
{  Right clicking toggles through the various mouse modes          }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   May    14 2002 - DL : Initial translation.                     }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  Math,
  OpenGL12,
  SDL,
  Logger;

const
  APP_TITLE = 'Jordan Isaak''s Cloth Simulation using JEDI-SDL';
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 24;

  CLOTH_SIZE = 5.0;
  CONSTRAINT_UPDATE = 3;
  TEXTURE_SIZE = 8;
  CLOTH_RESOLUTION = 16;
  TIME_STEP = 0.01;
  TIME_FACTOR = TIME_STEP * TIME_STEP;
  {Number of particles in the cloth}
  numparticles = CLOTH_RESOLUTION * CLOTH_RESOLUTION;
  numparticlesby3 = numparticles * 3;
  numparticlesby4 = numparticles * 4;

  {Number of constraints on cloth particle system}
  numconstraints = 2 * CLOTH_RESOLUTION * (CLOTH_RESOLUTION - 1)
    + (CLOTH_RESOLUTION - 1) * (CLOTH_RESOLUTION - 1);

  // Mouse Mode descriptions
  Mode : array[ 0..2 ] of string = ( 'Camera Direction', 'Sphere Position', 'Wind Direction');

  {Constraint structure}
type
  PConstraint = ^TConstraint;
  TConstraint = record
    particle1: integer;
    particle2: integer;
    restlength: single;
  end;

  TMouseMode = (mmCameraDirection, mmSpherePosition, mmWindDirection);

  T3ParticleArray = array[0..(numparticles * 3) - 1] of single;
  T4ParticleArray = array[0..(numparticles * 4) - 1] of single;
var
  surface: PSDL_Surface;

  {Texture object}
  texName: TGLuint;
  {Texture data array}
  texture: array[0..(3 * TEXTURE_SIZE * TEXTURE_SIZE) - 1] of TGLubyte;
  {Array of forces on the particles in the cloth}
  forces: T3ParticleArray;
  {Array of normals for particles in the cloth}
  normals: T3ParticleArray;
  {First particle array for cloth}
  cloth1: T4ParticleArray;
  {Second particle array for cloth}
  cloth2: T4ParticleArray;
  {Pointer to current cloth array}
  currentcloth: T4ParticleArray; //^single;
  {Pointer to previous cloth array}
  previouscloth: T4ParticleArray; //^single;

  {Array of constraints on the cloth particle system}
  constraints: array[0..numconstraints - 1] of TConstraint;

  {Position and radius of the sphere}
  sphere: array[0..3] of single = (0.0, 0.0, 0.0, 1.0);

  {The gravity force}
  gravityforce: single = -9.8;

  {Variables for controlling the wind}
  winddirection: array[0..2] of single = (0.0, 0.0, -1.0);
  windspeed: single = 0.0;
  windvector: array[0..2] of single = (0.0, 0.0, 0.0);

  {Lighting variables}
  lightpos: array[0..3] of single = (1.0, 1.0, 1.0, 0.0);
  lightambient: array[0..3] of single = (0.2, 0.2, 0.2, 1.0);
  lightdiffuse: array[0..3] of single = (1.0, 1.0, 1.0, 1.0);
  material: array[0..3] of single = (1.0, 1.0, 1.0, 1.0);

  {The elapsed time of the program}
  currenttime: integer = 0;
  {How far into the simulation the program is}
  simulationtime: Integer = 0;

  {Keeps track of where the mouse was last}
  lastx: integer = 0;
  lasty: integer = 0;

  {Camera control variables}
  rotation: single = 0;
  elevation: single = 1.0;
  cameradistance: single = 8.0;
  camerapos: array[0..2] of single = (0.0, 0.0, 0.0);

  {Controls what mouse position changes update}
  controlmode: integer = 0;

  {Stores the current texture used for the flag}
  currenttexture: integer = 0;

  SphereQuadric: PGLUQuadricObj; // Sphere Quadric object

  MouseMode: TMouseMode; // Current MouseMode

procedure TerminateApplication;
begin
  gluDeleteQuadric(SphereQuadric);
  SDL_Quit;
  UnLoadOpenGL;
  Halt(0);
end;

procedure generatetexture;
var
  i, j: integer;
begin
  {Depending on which texture is desired, generate different
  coloured checkerboard textures}
  if (currenttexture = 0) then
  begin
    for i := 0 to TEXTURE_SIZE - 1 do
    begin
      for j := 0 to TEXTURE_SIZE - 1 do
      begin
        texture[3 * (i * TEXTURE_SIZE + j)] := ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 1] := 255 - ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 2] := 0;
      end;
    end;
  end
  else if (currenttexture = 1) then
  begin
    for i := 0 to TEXTURE_SIZE - 1 do
    begin
      for j := 0 to TEXTURE_SIZE - 1 do
      begin
        texture[3 * (i * TEXTURE_SIZE + j)] := 255 - ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 1] := 255 - ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 2] := ((i + j) mod 2) * 255;
      end;
    end;
  end
  else if (currenttexture = 2) then
  begin
    for i := 0 to TEXTURE_SIZE - 1 do
    begin
      for j := 0 to TEXTURE_SIZE - 1 do
      begin
        texture[3 * (i * TEXTURE_SIZE + j)] := 50 + ((i + j) mod 2) * 205;
        texture[3 * (i * TEXTURE_SIZE + j) + 1] := 50 + ((i + j) mod 2) * 205;
        texture[3 * (i * TEXTURE_SIZE + j) + 2] := 50 + ((i + j) mod 2) * 205;
      end;
    end;
  end
  else if (currenttexture = 3) then
  begin
    for i := 0 to TEXTURE_SIZE - 1 do
    begin
      for j := 0 to TEXTURE_SIZE - 1 do
      begin
        texture[3 * (i * TEXTURE_SIZE + j)] := 255 - ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 1] := 255 - ((i + j) mod 2) * 255;
        texture[3 * (i * TEXTURE_SIZE + j) + 2] := 255;
      end;
    end;
  end;

  {Load the texture into OpenGL}
  glBindTexture(GL_TEXTURE_2D, texName);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
    GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
    GL_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGB, TEXTURE_SIZE, TEXTURE_SIZE, GL_RGB,
    GL_UNSIGNED_BYTE, @texture);
end;

procedure accumulateforces;
var
  i: integer;
  windforce: single;
begin

  //Determine the wind force vector
  windvector[0] := winddirection[0] * windspeed;
  windvector[1] := winddirection[1] * windspeed;
  windvector[2] := winddirection[2] * windspeed;

  for i := 0 to numparticles - 1 do
  begin
    //Initialize forces
    forces[3 * i] := 0.0;
    forces[3 * i + 1] := 0.0;
    forces[3 * i + 2] := 0.0;

    //Gravity force
    forces[3 * i + 1] := forces[3 * i + 1] + gravityforce;

    //Wind force
    windforce := windvector[0] * normals[3 * i]
      + windvector[1] * normals[3 * i + 1]
      + windvector[2] * normals[3 * i + 2];
    forces[3 * i] := forces[3 * i] + normals[3 * i] * windforce;
    forces[3 * i + 1] := forces[3 * i + 1] + normals[3 * i + 1] * windforce;
    forces[3 * i + 2] := forces[3 * i + 2] + normals[3 * i + 2] * windforce;

    //Dampening force
    windforce := (previouscloth[4 * i] - currentcloth[4 * i]) * normals[3 * i]
      + (previouscloth[4 * i + 1] - currentcloth[4 * i + 1]) * normals[3 * i + 1]
      + (previouscloth[4 * i + 2] - currentcloth[4 * i + 2]) * normals[3 * i +
        2];
    forces[3 * i] := forces[3 * i] + 80.0 * normals[3 * i] * windforce;
    forces[3 * i + 1] := forces[3 * i + 1] + 80.0 * normals[3 * i + 1] * windforce;
    forces[3 * i + 2] := forces[3 * i + 2] + 80.0 * normals[3 * i + 2] * windforce;
  end;
end;

procedure verlet;
var
  i: integer;
  tempsingleptr: T4ParticleArray; //^single;
begin

  //Update each particle via verlet integration
  for i := 0 to numparticles - 1 do
  begin
    previouscloth[4 * i] := 2.0 * currentcloth[4 * i]
      - previouscloth[4 * i]
      + forces[3 * i] * currentcloth[4 * i + 3] * TIME_FACTOR;
    previouscloth[4 * i + 1] := 2.0 * currentcloth[4 * i + 1]
      - previouscloth[4 * i + 1]
      + forces[3 * i + 1] * currentcloth[4 * i + 3] * TIME_FACTOR;
    previouscloth[4 * i + 2] := 2.0 * currentcloth[4 * i + 2]
      - previouscloth[4 * i + 2]
      + forces[3 * i + 2] * currentcloth[4 * i + 3] * TIME_FACTOR;
  end;

  //Update the current and previous cloth pointers
  tempsingleptr := previouscloth;
  previouscloth := currentcloth;
  currentcloth := tempsingleptr;
end;

procedure satisfyconstraints;
var
  i: integer;
  sphereradius: single;
  deltalength: single;
  delta: array[0..2] of single;
  diff: single;
  invmass1, invmass2: single;
begin
  sphereradius := sphere[3] + 0.05;

  //Satisfy stick constraints within cloth
  for i := 0 to numconstraints - 1 do
  begin
    delta[0] := currentcloth[4 * constraints[i].particle1]
      - currentcloth[4 * constraints[i].particle2];
    delta[1] := currentcloth[4 * constraints[i].particle1 + 1]
      - currentcloth[4 * constraints[i].particle2 + 1];
    delta[2] := currentcloth[4 * constraints[i].particle1 + 2]
      - currentcloth[4 * constraints[i].particle2 + 2];

    invmass1 := currentcloth[4 * constraints[i].particle1 + 3];
    invmass2 := currentcloth[4 * constraints[i].particle2 + 3];

    deltalength := sqrt(delta[0] * delta[0] + delta[1] * delta[1] + delta[2] *
      delta[2]);

    diff := (deltalength - constraints[i].restlength) / (deltalength * (invmass1 + invmass2));

    currentcloth[4 * constraints[i].particle1] := currentcloth[4 *
      constraints[i].particle1] - invmass1 * delta[0] * diff;
    currentcloth[4 * constraints[i].particle1 + 1] := currentcloth[4 *
      constraints[i].particle1 + 1] - invmass1 * delta[1] * diff;
    currentcloth[4 * constraints[i].particle1 + 2] := currentcloth[4 *
      constraints[i].particle1 + 2] - invmass1 * delta[2] * diff;

    currentcloth[4 * constraints[i].particle2] := currentcloth[4 *
      constraints[i].particle2] + invmass2 * delta[0] * diff;
    currentcloth[4 * constraints[i].particle2 + 1] := currentcloth[4 *
      constraints[i].particle2 + 1] + invmass2 * delta[1] * diff;
    currentcloth[4 * constraints[i].particle2 + 2] := currentcloth[4 *
      constraints[i].particle2 + 2] + invmass2 * delta[2] * diff;
  end;

  //Satisfy constraint that cloth points can't enter the sphere
  for i := 0 to numparticles - 1 do
  begin
    delta[0] := currentcloth[4 * i] - sphere[0];
    delta[1] := currentcloth[4 * i + 1] - sphere[1];
    delta[2] := currentcloth[4 * i + 2] - sphere[2];

    deltalength := delta[0] * delta[0] + delta[1] * delta[1] + delta[2] *
      delta[2];

    //If the particle is in the sphere, move it to outside the sphere
    if (deltalength < sphereradius * sphereradius) then
    begin
      deltalength := sqrt(deltalength);
      currentcloth[4 * i] := currentcloth[4 * i] + delta[0] * (sphereradius -
        deltalength) / sphereradius;
      currentcloth[4 * i + 1] := currentcloth[4 * i + 1] + delta[1] *
        (sphereradius - deltalength) / sphereradius;
      currentcloth[4 * i + 2] := currentcloth[4 * i + 2] + delta[2] *
        (sphereradius - deltalength) / sphereradius;
    end;
  end;
end;

procedure calculatenormals;
var
  i, j: integer;
  vectorlength: single;
  trianglenormal: array[0..2] of single;
  v1: array[0..2] of single;
  v2: array[0..2] of single;
begin
  //Set all normals to 0
  for i := 0 to numparticles - 1 do
  begin
    normals[3 * i] := 0.0;
    normals[3 * i + 1] := 0.0;
    normals[3 * i + 2] := 0.0;
  end;

  //Calculate the normal of each triangle
  //in the mesh and factor it into all the vertices
  //that are a part of it
  for i := 0 to CLOTH_RESOLUTION - 2 do
  begin
    for j := 0 to CLOTH_RESOLUTION - 2 do
    begin
      v1[0] := currentcloth[4 * (i * CLOTH_RESOLUTION + j)]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j)];
      v1[1] := currentcloth[4 * (i * CLOTH_RESOLUTION + j) + 1]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j) + 1];
      v1[2] := currentcloth[4 * (i * CLOTH_RESOLUTION + j) + 2]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j) + 2];

      v2[0] := currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1)]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j)];
      v2[1] := currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1) + 1]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j) + 1];
      v2[2] := currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1) + 2]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j) + 2];

      trianglenormal[0] := v1[1] * v2[2] - v1[2] * v2[1];
      trianglenormal[1] := v1[2] * v2[0] - v1[0] * v2[2];
      trianglenormal[2] := v1[0] * v2[1] - v1[1] * v2[0];

      normals[3 * (i * CLOTH_RESOLUTION + j)] := normals[3 * (i *
        CLOTH_RESOLUTION + j)] + trianglenormal[0];
      normals[3 * (i * CLOTH_RESOLUTION + j) + 1] := normals[3 * (i *
        CLOTH_RESOLUTION + j) + 1] + trianglenormal[1];
      normals[3 * (i * CLOTH_RESOLUTION + j) + 2] := normals[3 * (i *
        CLOTH_RESOLUTION + j) + 2] + trianglenormal[2];

      normals[3 * (i * CLOTH_RESOLUTION + j + 1)] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1)] + trianglenormal[0];
      normals[3 * (i * CLOTH_RESOLUTION + j + 1) + 1] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1) + 1] + trianglenormal[1];
      normals[3 * (i * CLOTH_RESOLUTION + j + 1) + 2] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1) + 2] + trianglenormal[2];

      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j)] := normals[3 * ((i + 1) *
        CLOTH_RESOLUTION + j)] + trianglenormal[0];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j) + 1] := normals[3 * ((i + 1)
        * CLOTH_RESOLUTION + j) + 1] + trianglenormal[1];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j) + 2] := normals[3 * ((i + 1)
        * CLOTH_RESOLUTION + j) + 2] + trianglenormal[2];

      v1[0] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1)]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1)];
      v1[1] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 1]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1) + 1];
      v1[2] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 2]
        - currentcloth[4 * (i * CLOTH_RESOLUTION + j + 1) + 2];

      v2[0] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j)]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1)];
      v2[1] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j) + 1]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 1];
      v2[2] := currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j) + 2]
        - currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 2];

      trianglenormal[0] := v1[1] * v2[2] - v1[2] * v2[1];
      trianglenormal[1] := v1[2] * v2[0] - v1[0] * v2[2];
      trianglenormal[2] := v1[0] * v2[1] - v1[1] * v2[0];

      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j + 1)] := normals[3 * ((i + 1)
        * CLOTH_RESOLUTION + j + 1)] + trianglenormal[0];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 1] := normals[3 * ((i +
        1) * CLOTH_RESOLUTION + j + 1) + 1] + trianglenormal[1];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j + 1) + 2] := normals[3 * ((i +
        1) * CLOTH_RESOLUTION + j + 1) + 2] + trianglenormal[2];

      normals[3 * (i * CLOTH_RESOLUTION + j + 1)] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1)] + trianglenormal[0];
      normals[3 * (i * CLOTH_RESOLUTION + j + 1) + 1] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1) + 1] + trianglenormal[1];
      normals[3 * (i * CLOTH_RESOLUTION + j + 1) + 2] := normals[3 * (i *
        CLOTH_RESOLUTION + j + 1) + 2] + trianglenormal[2];

      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j)] := normals[3 * ((i + 1) *
        CLOTH_RESOLUTION + j)] + trianglenormal[0];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j) + 1] := normals[3 * ((i + 1)
        * CLOTH_RESOLUTION + j) + 1] + trianglenormal[1];
      normals[3 * ((i + 1) * CLOTH_RESOLUTION + j) + 2] := normals[3 * ((i + 1)
        * CLOTH_RESOLUTION + j) + 2] + trianglenormal[2];
    end;
  end;

  //Normalize vertex normals
  for i := 0 to numparticles - 1 do
  begin
    vectorlength := 1.0 / sqrt(normals[3 * i] * normals[3 * i]
      + normals[3 * i + 1] * normals[3 * i + 1]
      + normals[3 * i + 2] * normals[3 * i + 2]);
    normals[3 * i] := normals[3 * i] * vectorlength;
    normals[3 * i + 1] := normals[3 * i + 1] * vectorlength;
    normals[3 * i + 2] := normals[3 * i + 2] * vectorlength;
  end;
end;

procedure updatecloth;
var
  i: integer;
begin
  //Accumulate forces on the particles
  accumulateforces;
  //Do verlet integration
  verlet;
  //Attempt to satisfy all the constraints on the system
  for i := 0 to CONSTRAINT_UPDATE - 1 do
    satisfyconstraints;
  //Calculate the triangle normals
  calculatenormals;
end;

procedure initializecloth(setuptype: integer);
var
  i, j: integer;
  currentconstraint: integer;
  restlength: single;
  diagonallength: single;
begin
  currentconstraint := 0;
  //Initialize cloth position and point weights
  if (setuptype = 0) then
  begin
    for i := 0 to CLOTH_RESOLUTION - 1 do
    begin
      for j := 0 to CLOTH_RESOLUTION - 1 do
      begin
        cloth1[4 * (CLOTH_RESOLUTION * i + j)] := -CLOTH_SIZE / 2.0 + j *
          CLOTH_SIZE / CLOTH_RESOLUTION;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 1] := 2.0;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 2] := -i * CLOTH_SIZE /
          CLOTH_RESOLUTION;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 3] := 1.0;
      end;
    end;
  end
  else if (setuptype = 1) then
  begin
    for i := 0 to CLOTH_RESOLUTION - 1 do
    begin
      for j := 0 to CLOTH_RESOLUTION - 1 do
      begin
        cloth1[4 * (CLOTH_RESOLUTION * i + j)] := 0.0;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 1] := -CLOTH_SIZE / 2.0 + j *
          CLOTH_SIZE / CLOTH_RESOLUTION;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 2] := -i * CLOTH_SIZE /
          CLOTH_RESOLUTION;
        cloth1[4 * (CLOTH_RESOLUTION * i + j) + 3] := 1.0;
      end;
    end;
  end;

  //Initialize second cloth array to match first
  for i := 0 to (4 * numparticles) - 1 do
    cloth2[i] := cloth1[i];

  //Initialize 2 corner points to infinite weight
  cloth1[3] := 0.0;
  cloth1[4 * (CLOTH_RESOLUTION - 1) + 3] := 0.0;

  for i := 0 to (4 * CLOTH_RESOLUTION * CLOTH_RESOLUTION) - 1 do
    cloth2[i] := cloth1[i];

  //Set up pointers for the previous frame and the current frame for the cloth
  currentcloth := cloth1;
  previouscloth := cloth2;

  restlength := CLOTH_SIZE / CLOTH_RESOLUTION;

  //Initialize cloth constraints
  //Constraints in the horizontal direction along the cloth
  for i := 0 to CLOTH_RESOLUTION - 1 do
  begin
    for j := 0 to CLOTH_RESOLUTION - 2 do
    begin
      constraints[currentconstraint].particle1 := i * CLOTH_RESOLUTION + j;
      constraints[currentconstraint].particle2 := i * CLOTH_RESOLUTION + j + 1;
      constraints[currentconstraint].restlength := restlength;
      inc(currentconstraint);
    end;
  end;
  //Constraints in the vertical direction along the cloth
  for i := 0 to CLOTH_RESOLUTION - 2 do
  begin
    for j := 0 to CLOTH_RESOLUTION - 1 do
    begin
      constraints[currentconstraint].particle1 := i * CLOTH_RESOLUTION + j;
      constraints[currentconstraint].particle2 := (i + 1) * CLOTH_RESOLUTION +
        j;
      constraints[currentconstraint].restlength := restlength;
      inc(currentconstraint);
    end;
  end;

  //A single diagonal constraint across every square in the cloth grid
  diagonallength := sqrt(2.0 * restlength * restlength);
  for i := 0 to CLOTH_RESOLUTION - 2 do
  begin
    for j := 0 to CLOTH_RESOLUTION - 2 do
    begin
      constraints[currentconstraint].particle1 := i * CLOTH_RESOLUTION + j;
      constraints[currentconstraint].particle2 := (i + 1) * CLOTH_RESOLUTION + j
        + 1;
      constraints[currentconstraint].restlength := diagonallength;
      inc(currentconstraint);
    end;
  end;
end;

procedure drawcloth;
var
  i, j: integer;
begin
  //Draw a series of triangle strips for the cloth
  for i := 0 to CLOTH_RESOLUTION - 2 do
  begin
    glBegin(GL_TRIANGLE_STRIP);
    for j := 0 to CLOTH_RESOLUTION - 1 do
    begin
      glNormal3fv(@(normals[3 * ((i + 1) * CLOTH_RESOLUTION + j)]));
      glTexCoord2f((j) / (CLOTH_RESOLUTION - 1), ((i + 1)) / (CLOTH_RESOLUTION -
        1));
      glVertex3fv(@(currentcloth[4 * ((i + 1) * CLOTH_RESOLUTION + j)]));
      glNormal3fv(@(normals[3 * (i * CLOTH_RESOLUTION + j)]));
      glTexCoord2f((j) / (CLOTH_RESOLUTION - 1), (i) / (CLOTH_RESOLUTION - 1));
      glVertex3fv(@(currentcloth[4 * (i * CLOTH_RESOLUTION + j)]));
    end;
    glEnd;
  end;
end;

procedure InitialiseScene;
begin
  // Set Default Mouse Mode
  MouseMode := mmCameraDirection;
  SDL_WM_SetCaption( PChar( APP_TITLE + ' - Mode : Change ' + Mode[ Ord( MouseMode ) ] ), nil );

  // Create the quadric object
  SphereQuadric := gluNewQuadric;

  initializecloth(0);

  glShadeModel(GL_SMOOTH);
  glDisable(GL_CULL_FACE);
  glEnable(GL_DEPTH_TEST);

  //Setup texturing
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glGenTextures(1, @texName);
  generatetexture;
  glEnable(GL_TEXTURE_2D);

  //Setup lighting
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightdiffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, @lightpos);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 1);
  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @material);
end;

procedure DisplayScene;
begin
  //Set the camera position
  camerapos[0] := sin(rotation) * sin(elevation) * cameradistance;
  camerapos[1] := cos(elevation) * cameradistance;
  camerapos[2] := cos(rotation) * sin(elevation) * cameradistance;

  //Set up for rendering
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  gluLookAt(camerapos[0], camerapos[1], camerapos[2],
    0.0, 0.0, 0.0,
    0.0, 1.0, 0.0);

  //Draw the cloth
  glColor3f(1.0, 1.0, 1.0);
  drawcloth;

  glDisable(GL_TEXTURE_2D);

  //Draw the wind direction
  glColor3f(0.0, 1.0, 0.0);
  glDisable(GL_LIGHTING);
  glBegin(GL_LINES);
  glVertex3fv(@windvector);
  glVertex3f(0.0, 0.0, 0.0);
  glEnd;
  glEnable(GL_LIGHTING);
  glColor3f(1.0, 1.0, 1.0);

  //Draw the sphere
  glTranslatef(sphere[0], sphere[1], sphere[2]);
  gluSphere(SphereQuadric, sphere[3], 16, 16);

  glEnable(GL_TEXTURE_2D);

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

procedure idle;
begin
  currenttime := SDL_GetTicks;

  //Update the simulation to match the current time
  while (simulationtime < currenttime) do
  begin
    updatecloth;
    simulationtime := Round(simulationtime + TIME_STEP * 1000);
  end;

  DisplayScene;
end;

// function to reset our viewport after a window resize

function ResizeWindow(width: integer; height: integer): Boolean;
begin
  // Protect against a divide by zero
  if (height = 0) then
    height := 1;
  // Setup our viewport.
  glViewport(0, 0, width, height);
  // change to the projection matrix and set our viewing volume.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  // Set our perspective
  gluPerspective(60.0, width / height, 0.1, 100.0);
  // Make sure we're changing the model view and not the projection
  glMatrixMode(GL_MODELVIEW);
  // Reset The View
  //glLoadIdentity;
  result := true;
end;

// function to handle key press events

procedure HandleKeyPress(keysym: PSDL_keysym);
begin
  case keysym.sym of
    SDLK_ESCAPE:
      // ESC key was pressed
      TerminateApplication;
    SDLK_RETURN:
      begin
        if (keysym.Modifier and KMOD_ALT <> 0) then
        begin
          {* Alt+Enter key was pressed
           * this toggles fullscreen mode
           *}
          SDL_WM_ToggleFullScreen(surface);
        end;
      end;
    SDLK_PLUS, SDLK_EQUALS:
      begin
        windspeed := windspeed + 1.0;
      end;
    SDLK_MINUS, SDLK_UNDERSCORE:
      begin
        windspeed := windspeed - 1.0;
      end;
    SDLK_f:
      begin
        initializecloth(1);
      end;
    SDLK_c:
      begin
        initializecloth(0);
      end;
    SDLK_z:
      begin
        cameradistance := cameradistance + 0.5;
      end;
    SDLK_x:
      begin
        cameradistance := cameradistance - 0.5;
        if (cameradistance < 4.0) then
          cameradistance := 4.0;
      end;
    SDLK_t:
      begin
        inc(currenttexture);
        currenttexture := currenttexture mod 4;
        generatetexture;
      end;
  end;
end;

// function to handle key press events

procedure HandleMouseButtonDown(Mouse: TSDL_MouseButtonEvent);
begin
  case Mouse.Button of
    SDL_BUTTON_LEFT :
      begin
        lastx := Mouse.x;
        lasty := Mouse.y;
      end;
      
    SDL_BUTTON_RIGHT :
      begin
        case MouseMode of
          mmCameraDirection :
          begin
           MouseMode := mmSpherePosition;
          end;
          mmSpherePosition :
          begin
           MouseMode := mmWindDirection;
          end;
          mmWindDirection :
          begin
           MouseMode := mmCameraDirection;
          end;
        end;
        SDL_WM_SetCaption( PChar( APP_TITLE + ' - Mode : Change ' + Mode[ Ord( MouseMode ) ] ), nil );
      end;
  end;
end;

// function to handle MouseMotion events

procedure HandleMouseMotion(MouseMotion: TSDL_MouseMotionEvent);
var
  modelviewmatrix: TMatrix4d;
  projectionmatrix: TMatrix4d;
  viewport: TVector4i;
  projectedpoint: array[0..2] of double;
  u: single;
  veclength: single;
begin
  case MouseMotion.state of
    SDL_BUTTON_LMASK:
      begin
        case MouseMode of
          // Change Direction
          mmCameraDirection:
            begin
              rotation := rotation + (MouseMotion.x - lastx) / 100.0;
              elevation := elevation + (MouseMotion.y - lasty) / 100.0;

              if (elevation < 0.1) then
                elevation := 0.1;
              if (elevation > 3.0) then
                elevation := 3.0;

              camerapos[0] := sin(rotation) * sin(elevation) * cameradistance;
              camerapos[1] := cos(elevation) * cameradistance;
              camerapos[2] := cos(rotation) * sin(elevation) * cameradistance;
            end;

          //Move sphere
          mmSpherePosition:
            begin
              glLoadIdentity;
              gluLookAt(camerapos[0], camerapos[1], camerapos[2],
                0.0, -0.5, 0.0,
                0.0, 1.0, 0.0);
              glGetDoublev(GL_MODELVIEW_MATRIX, @modelviewmatrix);
              glGetDoublev(GL_PROJECTION_MATRIX, @projectionmatrix);
              glGetIntegerv(GL_VIEWPORT, @viewport);

              gluUnProject(MouseMotion.x, (viewport[3] - MouseMotion.y), 1.0,
                modelviewmatrix, projectionmatrix, viewport,
                @(projectedpoint[0]), @(projectedpoint[1]),
                  @(projectedpoint[2]));

              u := camerapos[1] / (camerapos[1] - projectedpoint[1]);

              sphere[0] := camerapos[0] + u * (projectedpoint[0] -
                camerapos[0]);
              sphere[2] := camerapos[2] + u * (projectedpoint[2] -
                camerapos[2]);
            end;

          // Change Wind Direction
          mmWindDirection:
            begin
              glLoadIdentity;
              gluLookAt(camerapos[0], camerapos[1], camerapos[2],
                0.0, -0.5, 0.0,
                0.0, 1.0, 0.0);
              glGetDoublev(GL_MODELVIEW_MATRIX, @modelviewmatrix);
              glGetDoublev(GL_PROJECTION_MATRIX, @projectionmatrix);
              glGetIntegerv(GL_VIEWPORT, @viewport);

              gluUnProject(MouseMotion.x, (viewport[3] - MouseMotion.y), 1.0,
                modelviewmatrix, projectionmatrix, viewport,
                @(projectedpoint[0]), @(projectedpoint[1]),
                  @(projectedpoint[2]));

              //Calculate wind direction
              u := camerapos[1] / (camerapos[1] - projectedpoint[1]);
              winddirection[0] := camerapos[0] + u * (projectedpoint[0] -
                camerapos[0]);
              winddirection[2] := camerapos[2] + u * (projectedpoint[2] -
                camerapos[2]);

              //Normalize wind direction
              veclength := sqrt(winddirection[0] * winddirection[0]
                + winddirection[2] * winddirection[2]);
              winddirection[0] := winddirection[0] / veclength;
              winddirection[2] := winddirection[2] / veclength;
            end;
        end;
      end;

    SDL_BUTTON_RMASK:
      begin
        //
      end;

  end;

  lastx := MouseMotion.x;
  lasty := MouseMotion.y;
  idle;
end;

var
  Done: Boolean;
  event: TSDL_Event;
  videoflags: Uint32;
  videoInfo: PSDL_VideoInfo;
begin
  // Make sure OpenGL is loaded
  LoadOpenGL;

  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Could not initialize SDL : %s', [SDL_GetError]),
      'Main');
    TerminateApplication;
  end;
  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;
  if (videoInfo = nil) then
  begin
    Log.LogError(Format('Video query failed : %s', [SDL_GetError]),
      'Main');
    TerminateApplication;
  end;
  // the flags to pass to SDL_SetVideoMode
  videoFlags := SDL_OPENGL; // Enable OpenGL in SDL
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware
  // This checks to see if surfaces can be stored in memory
  if (videoInfo.hw_available <> 0) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;
  // This checks if hardware blits can be done * /
  if (videoInfo.blit_hw <> 0) then
    videoFlags := videoFlags or SDL_HWACCEL;
  // Set the OpenGL Attributes
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  // Set the title bar in environments that support it
  SDL_WM_SetCaption(APP_TITLE, nil);
  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing
  surface := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags);
  if (surface = nil) then
  begin
    Log.LogError(Format('Unable to create OpenGL screen : %s', [SDL_GetError]
      ),
      'Main');
    TerminateApplication;
  end;
  // Loop, drawing and checking events
  InitialiseScene;
  ReSizeWindow(SCREEN_WIDTH, SCREEN_HEIGHT);
  Done := False;
  while (not Done) do
  begin
    // This could go in a separate function
    while (SDL_PollEvent(@event) = 1) do
    begin
      case event.type_ of
        SDL_QUITEV:
          begin
            Done := true;
          end;
        SDL_KEYDOWN:
          begin
            // handle key presses
            HandleKeyPress(@event.key.keysym);
          end;
        SDL_MOUSEMOTION:
          begin
            // handle Mouse Movement
            HandleMouseMotion(event.motion);
          end;
        SDL_MOUSEBUTTONDOWN:
          begin
            // handle Mouse presses
            HandleMouseButtonDown(event.button);
          end;
        SDL_VIDEORESIZE:
          begin
            surface := SDL_SetVideoMode(event.resize.w, event.resize.h,
              SCREEN_BPP, videoflags);
            if (surface = nil) then
            begin
              Log.LogError(Format('Could not get a surface after resize : %s',
                [SDL_GetError]),
                'Main');
              TerminateApplication;
            end;
            InitialiseScene;
            ResizeWindow(event.resize.w, event.resize.h);
          end;
      end;
    end;
    // draw the scene
    Idle;
  end;

  TerminateApplication;
end.

