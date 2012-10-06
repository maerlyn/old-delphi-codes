// Define a few directive to ease the compatibility

{$ifdef VER80}          //	Delphi 1.0
  #ERROR tDbf needs delphi or C++ Builder 3 minimum.
{$endif}

{$ifdef VER90}          //	 Delphi 2.0
  #ERROR tDbf needs delphi or C++ Builder 3 minimum.
{$endif}

{$ifdef VER93}          //	 is BCB++ 1.0
  #ERROR tDbf needs delphi or C++ Builder 3 minimum.
{$endif}

{$ifdef VER100}         // Delphi 3
  {$define DELPHI_3}
{$endif}

{$ifdef VER110}      // CBuilder 3
  {$define DELPHI_3}
{$endif}

{$ifdef VER120}      // Delphi 4
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER125} // C++ BUILDER 4
  {$define DELPHI_4}
  {$define DELPHI_3}
{$endif}

{$ifdef VER130} // Delphi 5
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}

{$ifdef VER135} // C++ Builder 5 ??
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}

{$ifdef VER140} // Delphi 6
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}

{$ifdef VER145} // C++ Builder 6
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}

{$ifdef VER150} // Delphi 7 :-) For once I am not late (12/07/2001)
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}

{$ifdef VER155} // C++ Builder 7
  {$define DELPHI_7}
  {$define DELPHI_6}
  {$define DELPHI_5}
  {$define DELPHI_4}
  {$define DELPHI_3}
{$ENDIF}


