unit SkyboxUnit;
{******************************************************************************}
{                                                                              }
{                               SkyBoxes                                       }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{                                                                              }
{ Portions created by Ariel Jacob are                                          }
{ Copyright (C) 2000 - 2001 Ariel Jacob.                                       }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   SkyBoxes (for now only one)                                                }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{   OpenGL12                                                                   }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{                                                                              }
{                                                                              }
{                                                                              }
{******************************************************************************}

interface
uses OpenGL12, Math3D, SDL_Image, SDL, GLInit;

type
  TSkybox = Class
  protected
    BACK_ID,
    FRONT_ID,
    BOTTOM_ID,
    TOP_ID,
    TOPFX_ID,
    LEFT_ID,
    RIGHT_ID : TGLUInt;

    size     : T3DVector;
    CamPos   : P3DVector;
    addHight : single;
    U, V,
    USpeed,
    VSpeed   : single;

    procedure SkyboxLoadTexture(var PTexture: TGLUInt; filename: String);
  Published
    procedure Draw; virtual;
    constructor Create(Name, ext: String; _size: T3DVector; _CamPos: P3DVector);
    procedure Free;

    property FloorLevel: single read addHight write addHight;
  end;

implementation

constructor TSkybox.Create(Name, ext: String; _size: T3DVector;
  _CamPos: P3DVector);
begin
  inherited Create;
  // Load the Textures
  SkyboxLoadTexture( BACK_ID, Name+'BK'+ext );
  SkyboxLoadTexture( FRONT_ID, Name+'FT'+ext );
  SkyboxLoadTexture( BOTTOM_ID, Name+'DN'+ext );
  SkyboxLoadTexture( TOP_ID, Name+'UP'+ext );
  LoadGLTextures( TOPFX_ID, Name+'FX'+ext );
  SkyboxLoadTexture( LEFT_ID, Name+'LF'+ext );
  SkyboxLoadTexture( RIGHT_ID, Name+'RT'+ext );

  // set the box size
  size:= _size;
  addHight:= size.Y / 2;
  // get the ^ to the CamPos value
  CamPos:= _CamPos;

  U:= 0;
  V:= 0;
  USpeed:= 0.00007;
  VSpeed:= -0.000055;
end;

procedure TSkybox.Free;
begin
  glDeleteTextures(1, @BACK_ID);
  glDeleteTextures(1, @FRONT_ID);
  glDeleteTextures(1, @BOTTOM_ID);
  glDeleteTextures(1, @TOP_ID);
  glDeleteTextures(1, @TOPFX_ID);
  glDeleteTextures(1, @LEFT_ID);
  glDeleteTextures(1, @RIGHT_ID);
  inherited;
end;

procedure TSkybox.SkyboxLoadTexture(var PTexture: TGLUInt; filename: String);
var
  TextureImage: PSDL_Surface;
begin
  TextureImage := IMG_Load(PChar(filename));
  if (TextureImage <> nil) then
  begin
    // Create Texture
    glGenTextures(1, @PTexture);

    glPixelStorei (GL_UNPACK_ALIGNMENT, 1);

    glBindTexture(GL_TEXTURE_2D, PTexture);

{
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3,
      TextureImage.w, TextureImage.h,
      GL_RGB, GL_UNSIGNED_BYTE, TextureImage.pixels);
//}
    if TextureImage^.Format^.BytesPerPixel = 3 then
      glTexImage2D(GL_TEXTURE_2D,
       0,
       TextureImage.Format.BytesPerPixel,
       TextureImage.w, TextureImage.h, 0,
       GL_RGB, GL_UNSIGNED_BYTE,
       TextureImage.pixels)
    else
      glTexImage2D(GL_TEXTURE_2D,
       0,
       TextureImage.Format.BytesPerPixel,
       TextureImage.w, TextureImage.h, 0,
       GL_RGBA, GL_UNSIGNED_BYTE,
       TextureImage.pixels);


    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  end;

  if TextureImage <> nil then
    SDL_FreeSurface(TextureImage);
end;


//    (0,1)     (1,1)
//       ________
//      |        |
//      |        |
//      |        |
//       --------
//    (0,0)     (1,0)

procedure TSkybox.Draw;
const
  I = 1;
  def = 0.8;
  test = 0;
var
  x,y,z :single;
begin
  glColor4f(1, 1, 1, 1);

  x:= CamPos^.X;
  y:= CamPos^.Y;
  z:= CamPos^.Z;

  x:= x - size.X / 2;
  y:= y - addHight;
  z:= z - size.Z / 2;

  glBindTexture(GL_TEXTURE_2D, BACK_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0); glVertex3f(x, y, z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x, y + size.Y, z);
    glTexCoord2f(0.0, 0.0); glVertex3f(x + size.X, y + size.Y, z);
    glTexCoord2f(0.0, 1.0); glVertex3f(x + size.X, y, z);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, FRONT_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0); glVertex3f(x, y, z + size.Z);
    glTexCoord2f(0.0, 0.0); glVertex3f(x, y + size.Y, z + size.Z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x + size.X, y + size.Y, z + size.Z);
    glTexCoord2f(1.0, 1.0); glVertex3f(x + size.X, y, z + size.Z);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, BOTTOM_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0); glVertex3f(x, y, z);
    glTexCoord2f(0.0, 0.0); glVertex3f(x, y, z + size.Z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x + size.X, y, z + size.Z);
    glTexCoord2f(1.0, 1.0); glVertex3f(x + size.X, y, z);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, LEFT_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0); glVertex3f(x, y, z);
    glTexCoord2f(1.0, 1.0); glVertex3f(x, y, z + size.Z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x, y + size.Y, z + size.Z);
    glTexCoord2f(0.0, 0.0); glVertex3f(x, y + size.Y, z);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, RIGHT_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0); glVertex3f(x + size.X, y, z);
    glTexCoord2f(0.0, 1.0); glVertex3f(x + size.X, y, z + size.Z);
    glTexCoord2f(0.0, 0.0); glVertex3f(x + size.X, y + size.Y, z + size.Z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x + size.X, y + size.Y, z);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, TOP_ID);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex3f(x, y + size.Y, z);
    glTexCoord2f(0.0, 1.0); glVertex3f(x, y + size.Y, z + size.Z);
    glTexCoord2f(1.0, 1.0); glVertex3f(x + size.X, y + size.Y, z + size.Z);
    glTexCoord2f(1.0, 0.0); glVertex3f(x + size.X, y + size.Y, z);
  glEnd();
//{
  // TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
  glBindTexture(GL_TEXTURE_2D, TOPFX_ID);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_COLOR);
//  glBlendFunc(GL_BLEND_DST, GL_ONE);
  glEnable( GL_BLEND );
  glBegin(GL_QUADS);
    glTexCoord2f(0.0 + U/I, 0.0 + V/I);
    glVertex3f(x, y + size.Y - (size.Y / 3)+def, z);
    glTexCoord2f(0.0 + U/I, 1.0 + V/I);
    glVertex3f(x, y + size.Y - (size.Y / 3)+def, z + size.Z);
    glTexCoord2f(1.0 + U/I, 1.0 + V/I);
    glVertex3f(x + size.X, y + size.Y - (size.Y / 3)+def, z + size.Z);
    glTexCoord2f(1.0 + U/I, 0.0 + V/I);
    glVertex3f(x + size.X, y + size.Y - (size.Y / 3)+def, z);
  glEnd();

  glColor4f(0.4, 0.4, 0.4, 1);
  glBlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_COLOR);
  glBegin(GL_QUADS);
    glTexCoord2f(0.0 + U + test, 0.0 + V + test);
    glVertex3f(x, y + size.Y - (size.Y / 3), z);
    glTexCoord2f(0.0 + U + test, 1.0 + V + test);
    glVertex3f(x, y + size.Y - (size.Y / 3), z + size.Z);
    glTexCoord2f(1.0 + U + test, 1.0 + V + test);
    glVertex3f(x + size.X, y + size.Y - (size.Y / 3), z + size.Z);
    glTexCoord2f(1.0 + U + test, 0.0 + V + test);
    glVertex3f(x + size.X, y + size.Y - (size.Y / 3), z);
  glEnd();
  glDisable( GL_BLEND );
  glColor4f(1, 1, 1, 1);
  // TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP TEMP
//}
 U:= U + USpeed;
 V:= V + VSpeed;
end;

end.
