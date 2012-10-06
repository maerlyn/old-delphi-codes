(******************************************************************)
(* Copyright 1997 - 1999                                          *)
(*                                                                *)
(* Translated to Catalan by Jordi March  march_ribot@teleline.es  *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

unit SFXstringsCAT;

interface

const
     N                 = #13#10;
     STR_CANNOTOPEN    = 'No s''ha pogut obrir l''arxiu...';
     STR_MAINDLG       = 'MainDialog';
     STR_CANNOTCLOSE   = 'No s''ha pogut tancar l''arxiu';
     STR_E             = 'Error...';
     STR_EXISTS        = '...ja existeix. Reescriure''l ?';
     STR_SELDLG        = 'DLGSEL';
     STR_NEWDLG        = 'DLGNEW';
     BSL               = '\';
     STR_EARCHIVE      = 'Error llegint l''arxiu.';
     STR_INVALIDNAME   = 'Nom d''arxiu invàlid';
     STR_PDLG          = 'PASSWD';
     STR_FILEEXISTS    = 'FileExist';
     STR_EDIRECTORY    = 'Error en el directori...';
     STR_PREXT         = 'Extraient: ';
     STR_ETYPE         = 'Tipus de compressió desconegut';
     STR_ALLEXT        = 'Tots els arxius han estat extrets';
     STR_OK            = 'Extracció finalitzada';
     STR_NOTSELEXT     = 'No he pogut extreure els fitxers seleccionats';
     STR_APP           = 'DelZip Auto-extraïble';
     STR_EXPT          = 'Seleccioni un directori on descomprimir';
     STR_RUN_PRE       = 'Després de l''extracció, >< : '; // belongs to the 2 lines below
     STR_RUN_RUN       = 'executar';							 // after extraction, run : xxx.exe
     STR_RUN_INST      = 'instal·lar'; 					 // after extraction, install : xxx.inf
     STR_BROWSECAP     = 'Extreure a: ';
     STR_BTNFOLD       = 'Nou...';
     STR_EINC_SIZE     = 'Mida de l''arxiu incorrecte. Provi de descarregar de nou l''arxiu';
     STR_ABOUT_CAP     = 'Referència de '+STR_APP+'...';
     STR_ABOUT_MSG     = ' © 1997-1999 por C. Bunton, E. W. Engler, M. Stephany'+N+N+
                         'Els autors : '+N+N+'- Carl Bunton (http://www.concentric.net/~twojags)'+N+
                         N+'- Eric W. Engler (http://www.geocities.com/SiliconValley/Network/2114)'+N+
                         N+'- Markus Stephany (http://home.t-online.de/home/MirBir.St)'+N+
                         N+'Agraiments a : '+N+
                         N+'- Thomas Hoelzer (thoelzer@cityweb.de)'+
                         N+'- Deepu Chandy Thomas (deepuct@hotmail.com)'+
                         N+'- Todd Fast (tfast@eden.com)'+
                         N+'- Didier Havelange (Didier.Havelange@ping.be)'+
                         N+'- Frank Reichert (F.Rei@gmx.de)';

implementation

end.
