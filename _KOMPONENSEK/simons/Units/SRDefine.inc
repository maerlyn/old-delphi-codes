{ Include File für Simons Komponenten. Enthält Compiler-Direktiven für die
  verschiedenen Delphi-Versionen

  Include File for Simons components. Provides some compiler directives for
  the different Delphi  versions

  p.i.c.s.
  http://www.picsoft.de
  Simon Reinhardt
  S.Reinhardt@WTal.de                                                          }

{ A brief explanation of the defines in this unit

 SRDefines             : Always defined when this unit is included
 SR_Win16              : Compilation target is 16-bit Windows
 SR_Win32              : Compilation target is 32-bit Windows
 SR_Delphi1            : To be compiled with Delphi 1.0
 SR_Delphi2            : To be compiled with Delphi 2.0
 SR_Delphi3            : To be compiled with Delphi 3.0
 SR_Delphi4            : To be compiled with Delphi 4.0
 SR_Delphi5            : To be compiled with Delphi 5.0
 SR_Delphi2_Up         : To be compiled with Delphi 2.0 or higher
 SR_Delphi3_Up         : To be compiled with Delphi 3.0 or higher
 SR_Delphi4_Up         : To be compiled with Delphi 4.0 or higher
 SR_Delphi5_Up         : To be compiled with Delphi 5.0 or higher              }

{$B-}

{$DEFINE SRDefines}

{$IFDEF WIN32}
  {$DEFINE SR_Win32}
{$ELSE}
  {$DEFINE SR_Win16}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SR_Delphi5}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SR_Delphi4}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE SR_Delphi3}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE SR_Delphi2}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE SR_Delphi1}
{$ENDIF}

{$IFDEF SR_Delphi2}
  {$DEFINE SR_Delphi2_Up}
{$ENDIF}

{$IFDEF SR_Delphi3}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
{$ENDIF}

{$IFDEF SR_Delphi4}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
{$ENDIF}

{$IFDEF SR_Delphi5}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
{$ENDIF}
