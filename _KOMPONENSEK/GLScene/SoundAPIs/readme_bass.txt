  BASS 0.8 Multimedia Library
  -----------------------------
  (c) 1999 Ian Luck.
  Please report bugs/suggestions/etc... to bass@un4seen.com

  Delphi API Version 0.8 by Titus Miloi
  -----------------------------------------
  Questions, suggestions, etc. regarding the Delphi API
  can be sent to titus.a.m@t-online.de

  NOTE: This unit will only work with BASS.DLL version 0.8
  Check http://www.un4seen.com/music/ for any later
  versions of BASS.PAS

  History
  ---------
  02/18/2000 BASS.PAS 0.8
             - adapted for BASS version 0.8
             - indexed dll function calls replaced with
               dll function calls by name
             - new procedure: BASS_EAXPreset
               which replaces the EAX_PRESET_xxx definitions
             - some new example programs added
  09/21/1999 BASS.PAS 0.7
             - adapted for BASS version 0.7
               (see BASS.TXT for further information)
  08/20/1999 BASS.PAS 0.6.2
             The following bugs were fixed:
             - The STREAMPROC and SYNCPROC types were declared
               incorrectly; now declared as stdcall
             - BASS_StreamCreate and BASS_ChannelSetSync
               have been declared incorrectly; the last
               parameter must not be declared with var
             The following features have been added:
             - A new demo application has been added
               which shows how to use the stream functions
  07/31/1999 BASS.PAS 0.6
             The first BASS API for Delphi
             Tested with Delphi 3, but should also work
             proberly under Delphi 2 and 4.

  How to install
  ----------------
  Copy BASS.PAS to the \LIB subdirectory of your Delphi path
  e.g. to C:\Program Files\Borland\Delphi 3\Lib
  Now you should be able to run the demo projects.

END OF FILE
