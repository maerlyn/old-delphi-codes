//==========================================================================================
//  FMOD errors header file. Copyright (c), FireLight Multimedia 2000.
//==========================================================================================
//  History
//
//  2000/12/14 by Steve 'Sly' Williams
//  - Updated to version 3.30
//
//  2000/11/08 by Steve 'Sly' Williams
//  - Conversion of fmod_errors.h.  Slight name change was required because Pascal
//    is not case-sensitive and confuses the unit name with the type FMOD_ERRORS.
//
//  2002/01/30 by Steve 'Sly' Williams
//  FMOD Version 3.50
//  - Removed FMOD_ERR_NO_EAX2
//==========================================================================================

unit fmoderrors;

interface

uses
  fmod;

function FMOD_ErrorString(ErrorCode: TFModErrors): PChar;

implementation

function FMOD_ErrorString(ErrorCode: TFModErrors): PChar;
begin
  case ErrorCode of
    FMOD_ERR_NONE:              Result := 'No errors';
    FMOD_ERR_BUSY:              Result := 'Cannot call this command after FSOUND_Init.  Call FSOUND_Close first';
    FMOD_ERR_UNINITIALIZED:     Result := 'This command failed because FSOUND_Init was not called';
    FMOD_ERR_PLAY:              Result := 'Playing the sound failed';
    FMOD_ERR_INIT:              Result := 'Error initializing output device';
    FMOD_ERR_ALLOCATED:         Result := 'The output device is already in use and cannot be reused';
    FMOD_ERR_OUTPUT_FORMAT:     Result := 'Soundcard does not support the features needed for this soundsystem (16bit stereo output)';
    FMOD_ERR_COOPERATIVELEVEL:  Result := 'Error setting cooperative level for hardware';
    FMOD_ERR_CREATEBUFFER:      Result := 'Error creating hardware sound buffer';
    FMOD_ERR_FILE_NOTFOUND:     Result := 'File not found';
    FMOD_ERR_FILE_FORMAT:       Result := 'Unknown file format';
    FMOD_ERR_FILE_BAD:          Result := 'Error loading file';
    FMOD_ERR_MEMORY:            Result := 'Not enough memory or resources';
    FMOD_ERR_VERSION:           Result := 'The version number of this file format is not supported';
    FMOD_ERR_INVALID_PARAM:     Result := 'An invalid parameter was passed to this function';
    FMOD_ERR_NO_EAX:            Result := 'Tried to use an EAX command on a non EAX enabled channel or output';
    FMOD_ERR_CHANNEL_ALLOC:     Result := 'Failed to allocate a new channel';
    FMOD_ERR_RECORD:            Result := 'Recording is not supported on this machine';
    FMOD_ERR_MEDIAPLAYER:       Result := 'Required Mediaplayer codec is not installed';
  else
    Result := 'Unknown error';
  end;
end;

end.
