program JEDISDLMixer;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Mixer Example                           }
{       Conversion of the SDL Mixer Demo                           }
{                                                                  }
{ Portions created by Sam Lantinga <slouken@devolution.com>,  are  }
{ Copyright (C) 1998 Sam Lantinga.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : MixerDemo.c                             }
{                                                                  }
{ The original Pascal code is : JEDISDLMixer.dpr                   }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Matthias Thomas <ma.thoma@gmx.de>                                }
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
{   SDLMixerDemo: Shows how to load and play a wav file.           }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, and SDL_Mixer somewhere            }
{   in your path .                                                 }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This demo shows how to load and play a wav file.               }
{   YOU MUST HAVE "WAV_MUSIC" conditionally defined to be able to  }
{   compile this demo                                              }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   April   02 2001 - DL : Initial translation.                    }
{   June    30 2001 - DL : Delphi & Kylix unification of Code      }
{   July    04 2001 - MT : Got this working on Kylix               }
{                                                                  }
{                                                                  }
{******************************************************************}

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  {$IFDEF VCL}
  Forms,
  {$ELSE}
  QForms,
  {$ENDIF}
  Main in 'Main.pas'{Form1},
  SDL_Mixer,
  smpeg,
  SDL;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
