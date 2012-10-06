(******************************************************************)
(* Copyright 1997 - 1999                                          *)
(*                                                                *)
(* Translation by Michel Buttet  buttet@wanadoo.fr                *)
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
     STR_CANNOTOPEN    = 'Impossible d''ouvrir le fichier ..';
     STR_MAINDLG       = 'MainDialog';
     STR_CANNOTCLOSE   = 'Impossible de fermer le fichier';
     STR_E             = 'Erreur..';
     STR_EXISTS        = '...existe déjà, remplacer ?';
     STR_SELDLG        = 'DLGSEL';
     STR_NEWDLG        = 'DLGNEW';
     BSL               = '\';
     STR_EARCHIVE      = 'Erreur à la lecture de l''archive.';
     STR_INVALIDNAME   = 'Nom de fichier invalide.';
     STR_PDLG          = 'PASSWD';
     STR_FILEEXISTS    = 'FileExist';
     STR_EDIRECTORY    = 'Erreur dans le répertoire ..';
     STR_PREXT         = 'Extraction: ';
     STR_ETYPE         = 'Type de compression inconnu';
     STR_ALLEXT        = 'Tous les fichiers ont été extraits.';
     STR_OK            = 'Terminé.';
     STR_NOTSELEXT     = 'Le(s) fichier(s) sélectionné(s) n''ont pu être extraits.';
     STR_APP           = 'DelZip Self-Extractor';
     STR_EXPT          = 'Veuillez choisir le répertoire d''extraction';
     STR_RUN_PRE       = 'Après extraction, >< : '; // belongs to the 2 lines below
     STR_RUN_RUN       = 'lancer'; 						 // after extraction, run : xxx.exe
     STR_RUN_INST      = 'installer';					 // after extraction, install : xxx.inf
     STR_BROWSECAP     = 'Extraire vers : ';
     STR_BTNFOLD       = 'Nouveau...';
     STR_EINC_SIZE     = 'Taille de fichier incorrect, essayez de le télécharger à nouveau.';
     STR_ABOUT_CAP     = 'A propos de '+STR_APP+'...';
     STR_ABOUT_MSG     = ' © 1997-1999 par C. Bunton, E. W. Engler, M. Stephany'+N+N+
                         'Les auteurs : '+N+N+'- Carl Bunton (http://www.concentric.net/~twojags)'+N+
                         N+'- Eric W. Engler (http://www.geocities.com/SiliconValley/Network/2114)'+N+
                         N+'- Markus Stephany (http://home.t-online.de/home/MirBir.St)'+N+
                         N+'Remerciements à : '+N+
                         N+'- Thomas Hoelzer (thoelzer@cityweb.de)'+
                         N+'- Deepu Chandy Thomas (deepuct@hotmail.com)'+
                         N+'- Todd Fast (tfast@eden.com)'+
                         N+'- Didier Havelange (Didier.Havelange@ping.be)'+
                         N+'- Frank Reichert (F.Rei@gmx.de)'+
							N+'- Michel Buttet (buttet@wanadoo.fr)';

implementation

end.
