(******************************************************************)
(* Copyright 1997 - 1999                                          *)
(*                                                                *)
(* Translation by Almer S. Tigelaar  almer-t@usa.net              *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

unit SFXstrings;

interface

const
     N                 = #13#10;
     STR_CANNOTOPEN    = 'Kan bestand niet openen ..';
     STR_MAINDLG       = 'MainDialog';
     STR_CANNOTCLOSE   = 'Kan bestand niet sluiten';
     STR_E             = 'Fout..';
     STR_EXISTS        = '...bestaat al, overschrijven?';
     STR_SELDLG        = 'DLGSEL';
     STR_NEWDLG        = 'DLGNEW';
     BSL               = '\';
     STR_EARCHIVE      = 'Een fout is opgetreden bij het lezen van het archief.';
     STR_INVALIDNAME   = 'Ongeldige bestandsnaam.';
     STR_PDLG          = 'PASSWD';
     STR_FILEEXISTS    = 'FileExist';
     STR_EDIRECTORY    = 'Fout in directory ..';
     STR_PREXT         = 'Uitpakken: ';
     STR_ETYPE         = 'Onbekend compressie type';
     STR_ALLEXT        = 'Alle bestanden zijn uitgepakt';
     STR_OK            = 'Klaar.';
     STR_NOTSELEXT     = 'De geselecteerde bestanden konden niet worden uitgepakt.';
     STR_APP           = 'DelZip Self-Extractor';
     STR_EXPT          = 'Kies de bestemmings directory';
     STR_RUN_PRE       = '>< na het uitpakken : '; // belongs to the 2 lines below
     STR_RUN_RUN       = 'Start'; 						// after extraction, run : xxx.exe
     STR_RUN_INST      = 'Install'; 					// after extraction, install : xxx.inf
     STR_BROWSECAP     = 'Uitpakken naar : ';
     STR_BTNFOLD       = 'Nieuw...';
     STR_EINC_SIZE     = 'Ongeldige bestandslengte. Probeer dit bestand opnieuw te downloaden';
     STR_ABOUT_CAP     = 'Over '+STR_APP+'...';
     STR_ABOUT_MSG     = ' © 1997-1999 by C. Bunton, E. W. Engler, M. Stephany'+N+N+
                         'De auteurs : '+N+N+'- Carl Bunton (http://www.concentric.net/~twojags)'+N+
                         N+'- Eric W. Engler (http://www.geocities.com/SiliconValley/Network/2114)'+N+
                         N+'- Markus Stephany (http://home.t-online.de/home/MirBir.St)'+N+
                         N+'Met dank aan : '+N+
                         N+'- Thomas Hoelzer (thoelzer@cityweb.de)'+
                         N+'- Deepu Chandy Thomas (deepuct@hotmail.com)'+
                         N+'- Todd Fast (tfast@eden.com)'+
                         N+'- Didier Havelange (Didier.Havelange@ping.be)'+
                         N+'- Frank Reichert (F.Rei@gmx.de)'+
							N+'- Almer S. Tigelaar (almer-t@usa.net)';


implementation

end.
