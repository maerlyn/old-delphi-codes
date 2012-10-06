unit Logger;
{******************************************************************************}
{                                                                              }
{                Error Logging Unit                                            }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2000 - 2001 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
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
{   Logging functions...                                                       }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{               2001 - DL : Initial creation                                   }
{         25/10/2001 - DRE : Added $M+ directive to allow published            }
{                                in classes. Added a compile directive         }
{                                around fmShareExclusive as this does not      }
{                                exist in Free Pascal                          }
{                                                                              }
{******************************************************************************}
{$IFDEF FPC}
{$M+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;
  
type
  TLogger = class(TFileStream)
  private
    FApplicationName : string;
    FApplicationPath : string;
  protected

  public
    constructor Create;
    procedure LogError( ErrorMessage : string; Location : string );
    procedure LogWarning( WarningMessage : string; Location : string );
    procedure LogStatus( StatusMessage : string; Location : string );
  published
    property ApplicationName : string read FApplicationName;
    property ApplicationPath : string read FApplicationPath;
  end;

var
  Log : TLogger;

implementation

{ TLogger }
constructor TLogger.Create;
begin
  FApplicationName := ExtractFileName( ParamStr(0) );
  FApplicationPath := ExtractFilePath( ParamStr(0) );
  inherited Create( FApplicationPath + Copy( FApplicationName, 0, Length( FApplicationName ){$IFDEF WIN32} - 4{$ENDIF} ) + '.log',
                    fmCreate {$IFNDEF FPC}or fmShareExclusive{$ENDIF} );
end;

procedure TLogger.LogError(ErrorMessage, Location: string);
var
  S : string;
begin
  S := '*** ERROR *** : @ ' + TimeToStr(Time) + ' MSG : ' + ErrorMessage + ' IN : ' + Location + #13#10;
  Write(S[1], Length(S));
end;

procedure TLogger.LogStatus(StatusMessage, Location: string);
var
  S : string;
begin
  S := 'STATUS INFO : @ ' + TimeToStr(Time) + ' MSG : ' + StatusMessage + ' IN : ' + Location + #13#10;
  Write(S[1], Length(S));
end;

procedure TLogger.LogWarning(WarningMessage, Location: string);
var
  S : string;
begin
  S := '=== WARNING === : @ ' + TimeToStr(Time) + ' MSG : ' + WarningMessage + ' IN : ' + Location + #13#10;
  Write(S[1], Length(S));
end;

initialization
begin
  Log := TLogger.Create;
  Log.LogStatus( 'Starting Application', 'Initialization' );
end;

finalization
begin
  Log.LogStatus( 'Terminating Application', 'Finalization' );
  Log.Free;
  Log := nil;
end;

end.
 