{$IFDEF VER140}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI6}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI4}
  {$ELSE}
    {$DEFINE BCB5}
    {$DEFINE BCB4}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFNDEF COMPILER4}
/////////////////////////////////////////////////////////////////
'  Graphics32 may only be installed with Delphi 4, Delphi 5,    '
'  Delphi6, C++ Builder 4, or C++ Builder 5!                    '
/////////////////////////////////////////////////////////////////
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}