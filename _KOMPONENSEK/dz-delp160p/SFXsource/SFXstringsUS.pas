(******************************************************************)
(* Copyright 1997 - 1999                                          *)
(*                                                                *)
(* English source by Markus Stephany  mirbir.st@t-online.de       *)
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
     STR_CANNOTOPEN    = 'Cannot open file ..';
     STR_MAINDLG       = 'MainDialog';
     STR_CANNOTCLOSE   = 'Cannot close the file';
     STR_E             = 'Error..';
     STR_EXISTS        = '...already exists, overwrite ?';
     STR_SELDLG        = 'DLGSEL';
     STR_NEWDLG        = 'DLGNEW';
     BSL               = '\';
     STR_EARCHIVE      = 'Error reading archive.';
     STR_INVALIDNAME   = 'Invalid filename.';
     STR_PDLG          = 'PASSWD';
     STR_FILEEXISTS    = 'FileExist';
     STR_EDIRECTORY    = 'Error in directory ..';
     STR_PREXT         = 'Extracting: ';
     STR_ETYPE         = 'Unknown compression type';
     STR_ALLEXT        = 'All files have been extracted.';
     STR_OK            = 'Finished.';
     STR_NOTSELEXT     = 'The selected file(s) couldn''t get extracted.';
     STR_APP           = 'DelZip Self-Extractor';
     STR_EXPT          = 'Please choose the destination directory';
     STR_RUN_PRE       = 'After extraction, >< : '; // belongs to the 2 lines below
     STR_RUN_RUN       = 'run';							 // after extraction, run : xxx.exe
     STR_RUN_INST      = 'install'; 					 // after extraction, install : xxx.inf
     STR_BROWSECAP     = 'Extract to : ';
     STR_BTNFOLD       = 'New...';
     STR_EINC_SIZE     = 'Incorrect file size, please try to download this file again.';
     STR_ABOUT_CAP     = 'About '+STR_APP+'...';
     STR_ABOUT_MSG     = ' © 1997-1999 by C. Bunton, E. W. Engler, M. Stephany'+N+N+
                         'The authors : '+N+N+'- Carl Bunton (http://www.concentric.net/~twojags)'+N+
                         N+'- Eric W. Engler (http://www.geocities.com/SiliconValley/Network/2114)'+N+
                         N+'- Markus Stephany (http://home.t-online.de/home/MirBir.St)'+N+
                         N+'Credits to : '+N+
                         N+'- Thomas Hoelzer (thoelzer@cityweb.de)'+
                         N+'- Deepu Chandy Thomas (deepuct@hotmail.com)'+
                         N+'- Todd Fast (tfast@eden.com)'+
                         N+'- Didier Havelange (Didier.Havelange@ping.be)'+
                         N+'- Frank Reichert (F.Rei@gmx.de)';


implementation

end.
